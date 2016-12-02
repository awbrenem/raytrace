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
;     e- must travel to the opposite hemisphere. Use for normal cyclotron resonance.
;     Don't set for anomalous or Landau resonance where e- will be precipitating in
;     same hemisphere that it interacts in.
;
; Written by Aaron W Breneman, Dec, 2016


function distance_to_atmosphere,lshell,mlat,offset_alt=alt,opposite_hemisphere=oh


  travellength_remaining = fltarr(n_elements(lshell))

  for i=0,n_elements(lshell)-1 do begin

    ;determine arc length along Bo field line
    dp = dipole(lshell[i])

    ;field line length at Earth's surface
    bolendiff = reverse(max(dp.s) - dp.s)
    dpsr = reverse(dp.s)

    ;element that represents height of FIREBIRD
    goo = where(bolendiff ge alt)
    ;print,max(dp.s) - dpsr[goo[0]]  ;almost exactly the same as altitude

    ;length of field line from equator to alt
    fb_s = dpsr[goo[0]]

    tmp = where(dp.lat ge abs(mlat[i]))
    slen = dp.s[tmp[0]]

    ;precipitation in the same hemisphere (anomalous or Landau resonance)
    if ~KEYWORD_SET(oh) then travellength_remaining[i] = fb_s - slen
    if KEYWORD_SET(oh) then travellength_remaining[i] = fb_s + slen

  endfor

  travellength_remaining/=6370.
  return,travellength_remaining

end
