;Calculates the time it takes an electron at an angle at/within the loss-cone
;to go from one location to another. E.x., the time it takes an electron
;scattered into loss cone at 20 deg to arrive at FIREBIRD at 500 km.

;Uses the equation for t1 in top right of pg of Miyoshi et al., 2010

;lval = dipole L-value
;mlat1, mlat2 -> abs of magnetic lat of starting and ending points.
;e_energy -> Total e- energy (eV)
;pa_lc -> pitch angle inside or at edge of loss cone (deg)
;opposite_hemisphere -> set if the scattering occurs in opposite hemisphere as
;   precipitation (e.g. counter-streaming cyclotron resonance). If set,
;   calculates time from scattering mlat (mlat1) to equator, then adds this to time
;   from equator to mlat2

function electron_precipitation_time,lval,mlat1,mlat2,e_energy,pa_lc,opposite_hemisphere=oh

L = dipole(lval)

z = L.s


;find array location of scattering point
goo = where(L.lat ge mlat1)
loc1 = goo[0]
;find array location of FIREBIRD
goo = where(L.lat ge mlat2)
loc2 = goo[0]



;Array of steps along the field line
dz = z - shift(z,1)
dz = dz[1:n_elements(dz)-1]


;start/stop points along field line for integration
;s1 = 0.*6370.
s1 = L.s[loc1]
s2 = L.s[loc2]



;magnetic field value at point of loss cone scattering (s1)
;Bo = 367.
Bo = L.B[loc1]
;constant inside the integrand
c = (1/Bo)*sin(pa_lc*!dtor)^2


;Precipitation in same sector as scattering (costream, Landau)
if ~KEYWORD_SET(oh) then begin

  ;find elements to integrate over
  good = where((z ge s1) and (z le s2))
  dt = fltarr(n_elements(good))

  ;Every term in the sum from s1 to s2
  for i=0,n_elements(good)-2 do dt[i] = dz[good[i]]/sqrt(1-c*L.B[good[i]])

  ;Now sum the dt contributions from the interaction point to FIREBIRD
  tots = total(dt)*1000.

endif else begin
;Precipitation in opposite sector as scattering (counterstream)

  ;First integrate from location of scattering to magnetic eq
  good = where((z ge 0) and (z le s1)) ;find elements to integrate over
  good = reverse(good)
  dt = fltarr(n_elements(good))

  ;Every term in the sum from s1 to s2
  for i=0,n_elements(good)-2 do dt[i] = dz[good[i]]/sqrt(1-c*L.B[good[i]])
  ;Now sum the dt contributions from the interaction point to FIREBIRD
  tots1 = total(dt)*1000.

  ;now integrate from magnetic eq to location of FB
  good = where((z ge 0) and (z le s2)) ;find elements to integrate over
  dt = fltarr(n_elements(good))

  ;Every term in the sum from s1 to s2
  for i=0,n_elements(good)-2 do dt[i] = dz[good[i]]/sqrt(1-c*L.B[good[i]])

  ;Now sum the dt contributions from the interaction point to FIREBIRD
  tots2 = total(dt)*1000.

  tots = tots1 + tots2
endelse



me = 9.11d-31   ;kg
energy = e_energy*1.6d-19  ;Joules

const = sqrt(me/2./energy)

timeprecip = const*tots

;RBSP_EFW> print,timeprecip
;      0.19741298
;RBSP_EFW> print,timeprecip
;     0.055883683

;;Compare results to the result obtained by assuming that the Electron
;;does not slow down the entire flight

;;Initial velocity parallel (correct)
;c_ms = 2.99792458d8      ; -Speed of light in vacuum (m/s)
;fac1 = 0.511d6/(0.511d6 + 1000.*250000.)
;fac1 = 1. - fac1^2
;vpar0 = sqrt(c_ms^2*fac1)  ;m/s
;
;print,1000.*s2/vpar0


return,timeprecip

end
