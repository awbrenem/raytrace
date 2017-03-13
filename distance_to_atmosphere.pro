;Returns distance along current dipole field
;line a particle would need to travel to reach atmosphere. Input values
;are ray L and mlat.
;(see crib_raytrace_microburst_chorus.pro for example)
;
;
;lshell -> l values of ray
;mlat -> mlat values of ray
;offset_alt -> height of precipitation point or observing point (e.g. LEO satellite)
;              If not set uses the Earth's surface
;opposite_hemisphere -> set of you'd like to know the distance a loss-cone scattered
;     e- must travel to the opposite hemisphere. Use for counter-streaming cyclotron resonance.
;     Don't set for costreaming or Landau resonance where e- will be precipitating in
;     same hemisphere that it interacts in.
;
; Written by Aaron W Breneman, Dec, 2016


function distance_to_atmosphere,lshell,mlat,offset_alt=alt,opposite_hemisphere=oh



  tmp = size(lshell)
  if tmp[0] ne 1 then nrays = tmp[2] else nrays = 1.


  travellength_remaining = fltarr(10000.,nrays)


  for qq=0,nrays-1 do begin

    ;remove possible NaN values
    goo = where(finite(lshell[*,qq]) ne 0)
    if goo[0] ne -1 then lshellt = lshell[goo,qq] else lshellt = lshell[*,qq]
    if goo[0] ne -1 then mlatt = mlat[goo,qq] else mlatt = mlat[*,qq]


    for i=0,n_elements(lshellt)-1 do begin

      ;determine arc length along Bo field line
      dp = dipole(lshellt[i])

      ;field line length at Earth's surface
      bolendiff = reverse(max(dp.s) - dp.s)
      dpsr = reverse(dp.s)

      ;element that represents height of FIREBIRD
      goo = where(bolendiff ge alt)
      ;print,max(dp.s) - dpsr[goo[0]]  ;almost exactly the same as altitude

      ;length of field line from equator to alt (+ for both hemispheres).
      ;For ex, alt may be the location of the FIREBIRD sat at 500 km
      ;Will change only if field line changes as rays cross them
      fb_s = dpsr[goo[0]]

      tmp = where(dp.lat ge abs(mlatt[i]))
      ;length of field line from equator to current ray position (+ for both hemispheres)
      slen = dp.s[tmp[0]]

      ;precipitation in the same hemisphere (costreaming or Landau resonance)
      ;Distance from source (e.g. mag eq) to observation (e.g. FIREBIRD in northern hemisphere)
      ;minus the distance from eq to northern hemisphere where the e- is scattered
      if ~KEYWORD_SET(oh) then travellength_remaining[i,qq] = fb_s - slen
      ;precipitation in the opposite hemisphere (counter-streaming)
      ;Distance from source (e.g. mag eq) to observation (e.g. FIREBIRD in northern hemisphere)
      ;plus the extra distance from eq to southern hemisphere where the e- is scattered
      if KEYWORD_SET(oh) then  travellength_remaining[i,qq] = fb_s + slen

    endfor

  endfor ;nrays

  travellength_remaining/=6370.
  return,travellength_remaining

end
