;Calculates the time (sec) it takes an electron at a certain pitch angle
;to go from one location to another. E.g., the time it takes an electron
;at edge of loss cone at 20 deg mlat to arrive at FIREBIRD at 500 km.
;NOTE: Values of -1 mean the ray doesn't reach mlat_fin!!!!


;Uses the equation for t1 in top right of pg 2 of Miyoshi et al., 2010

;lshell = dipole L-value for [x,nrays], say [10000,nrays] where each ray
; is allowed up to 10000 points
;mlat = magnetic lat (absolute value) for [x,nrays]
;alt = final altitude of electron (e.g. 500 km FIREBIRD)
;e_energy -> [x,nrays] array of total e- energy (keV)
;pa -> loss cone pitch angle
;opposite_hemisphere -> set if the scattering occurs in opposite hemisphere as
;   precipitation (e.g. counter-streaming cyclotron resonance). If set,
;   calculates time from scattering mlat (mlat) to equator, then adds this to time
;   from equator to mlat2

;******
;TESTING
;lshell = 4.79
;bmult = 1.5
;mlatt = -12.6
;alt = 500.
;pa = 5.
;e_energyt = 1.761 ;MeV
;EMeV = 0.088

;********



function electron_precipitation_time,$
  lshell,$
  mlat,$
  alt,$
  e_energy,$
  pa,$
  bmult=bmult,$
  opposite_hemisphere=oh



  if ~KEYWORD_SET(bmult) then bmult = 1.


  tmp = size(lshell)
  if tmp[0] ne 1 then nrays = tmp[2] else nrays = 1.

  timeprecip = lshell
  timeprecip[*] = 0d


  for qq=0,nrays-1 do begin

    ;remove possible NaN values
    goo = where(finite(lshell[*,qq]) ne 0.)
    if goo[0] ne -1 then lshellt = lshell[goo,qq] else lshellt = lshell[*,qq]
    if goo[0] ne -1 then mlatt = mlat[goo,qq] else mlatt = mlat[*,qq]
    ;if goo[0] ne -1 then e_energyt = e_energy[goo,qq] else e_energyt = e_energy[*,qq]


    for i=0,n_elements(lshellt)-1 do begin

      dp = dipole(lshellt[i],bmult)

      ;determine arc length along Bo field line
      z = dp.s


      ;find array location of scattering point
      goo = where(dp.lat ge abs(mlatt[i]))
      loc1 = goo[0]
      ;find array location of "alt"
      goo = where(dp.r le (alt + 6370d))
      loc2 = goo[0]


      ;Array of steps along the field line
      dz = z - shift(z,1)
      dz = dz[1:n_elements(dz)-1]


      ;start/stop points (km) along field line for integration
      s1 = dp.s[loc1]
      s2 = dp.s[loc2]


      ;magnetic field value at point of loss cone scattering (s1)
      Bo = dp.B[loc1]
      ;constant inside the integrand
      c = (1d/Bo)*sin(pa*!dtor)^2

      ;Precipitation in same sector as scattering (costream, Landau)
      if ~KEYWORD_SET(oh) then begin

        ;find elements to integrate over
        good = where((z ge s1) and (z lt s2))
        integrand = fltarr(n_elements(good))
        integrand2 = fltarr(n_elements(good))
        dist_total = s2-s1

        ;Every term in the sum from s1 to s2  (units if km)
        for bb=0,n_elements(good)-1 do integrand[bb] = dz[good[bb]]/sqrt(1-c*dp.B[good[bb]])


        ;Now sum the dz contributions from the interaction point to FIREBIRD
        tots = total(integrand)  ;NaN values mean the ray doesn't reach mlat_fin


        ;if the ray doesn't reach the desired altitude, change the total
        ;to -1. Leaving it as NaN can be confusing later for debugging
        if finite(tots) eq 0 then tots = -1

      endif else begin
        ;Precipitation in opposite sector as scattering (counterstream)

        ;First integrate from location of scattering to magnetic eq
        good = where((z ge 0) and (z lt s1)) ;find elements to integrate over
        good = reverse(good)
        dt = fltarr(n_elements(good))
        ;Total distance traveled by Electron
        dist_total = max(z[good]) - min(z[good])

        ;Every term in the sum from s1 to equator. dt represents the contribution
        ;from each integration step. Near the equator these will all be pretty
        ;similar
        for bb=0,n_elements(good)-2 do dt[bb] = dz[good[bb]]/sqrt(1-c*dp.B[good[bb]])
        ;Now sum the dt contributions from the interaction point to FIREBIRD
        tots1 = total(dt)

        ;now integrate from magnetic eq to location of FB
        good = where((z ge 0) and (z lt s2)) ;find elements to integrate over
        dt = fltarr(n_elements(good))
        dist_total += max(z[good])

        ;Every term in the sum from s1 to s2
        for bb=0,n_elements(good)-2 do dt[bb] = dz[good[bb]]/sqrt(1-c*dp.B[good[bb]])

        ;Now sum the dt contributions from the interaction point to FIREBIRD
        tots2 = total(dt)

        tots = tots1 + tots2 ;NaN values mean the ray doesn't reach mlat_fin


        ;if the ray doesn't reach the desired altitude, change the total
        ;to -1. Leaving it as NaN can be confusing later for debugging
        if finite(tots) eq 0 then tots = -1

      endelse



      ;total velocity (km/s)
      vtots = 3d5*sqrt(1 - (0.511/(e_energy[i,qq]/1000. + 0.511))^2)
      vpar = vtots*cos(pa*!dtor)

      if tots ne -1 then begin
        timeprecip[i,qq] = tots/vpar
;        print,timeprecip[i,qq]
;        print,'average field-aligned velocity/c = ',(dist_total/timeprecip[i,qq])/3d5
      endif else timeprecip[i,qq] = !values.f_nan

    endfor

    ;***Test output***
    ;NaN values mean the ray doesn't reach mlat_fin
    ;plot the following values:
    ;---magnetic latitude of ray point
    ;---cyclotron resonant energy (keV) (anomalous or normal)
    ;---time to precipitate at "alt" (msec)
    ;---average vpar/c  (will be higher for counterstreaming b/c particle spends more time away from mirror points.
    ;                     It's just a check to be sure that average velocity doesn't exceed c)
    ;NaN values mean that this particular particle doesn't reach "alt" before it is mirrored
;    for u=0,340 do print,mlat[u,qq],e_energy[u,qq],1000.*timeprecip[u,qq],(dist_total/timeprecip[u,qq])/3d5
;    stop
    ;***Test output***

  endfor


  ;change the bad values (negative ones) to a value of -1
  ;Corresponds to situations in which the scattered electron doesn't
  ;reach the desired final altitude
  goo = where(timeprecip lt 0.)
  if goo[0] ne -1 then timeprecip[goo] = -1

  goo = where(timeprecip eq 0.)
  if goo[0] ne -1 then timeprecip[goo] = !values.f_nan

  return,timeprecip

end

;v = sqrt(E^2 - mo^2*c^4)/(c*g*mo)

;c*g*mo = g*0.511

;mo = 0.511/c^2  ;MeV/c2
;E = sqrt()

;p = g*mo*v = sqrt(E^2 - mo^2*c^4)/c
