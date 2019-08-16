;Overplot the Earth, mlat, Lvalue lines, etc. onto the ray plots
;Called from plot_rays.pro
;
;latv -> magnetic latitude lines to oplot. Defaults to [0,10,20,30,40,50,60,70,80]
;Lv -> Lvalue lines to plot. Defaults to [2,4,6,8]
;eq_plane -> Set to plot equatorial plane. Defaults to meridional plane

pro oplot_earth_mlat_l_lines,latv=latv,Lv=Lv,eq_plane=eq_plane


  ;overplot the Earth
  earthx = COS((2*!PI/99.0)*FINDGEN(100))
  earthy = SIN((2*!PI/99.0)*FINDGEN(100))
  oplot,earthx,earthy,color=60
  oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80


  if ~keyword_set(eq_plane) then begin

    ;latitude lines
    if ~KEYWORD_SET(latv) then latv = [0,10,20,30,40,50,60,70,80]
    latv = [-1*reverse(latv[1:n_elements(latv)-1]),latv]
    for i=0,n_elements(latv)-1 do oplot,[1,50]*cos([latv[i]*!dtor,latv[i]*!dtor]),[1,50]*sin([latv[i]*!dtor,latv[i]*!dtor]),linestyle=3,color=100


    ;overplot the dipole field lines
    if ~KEYWORD_SET(Lv) then Lv = [2,4,6,8]
    for i=0,n_elements(Lv)-1 do begin
      Ltmp = dipole(Lv[i])
      oplot,Ltmp.R/6370.*cos(Ltmp.lat*!dtor),Ltmp.R/6370.*sin(Ltmp.lat*!dtor),color=120
      oplot,Ltmp.R/6370.*cos(Ltmp.lat*!dtor),-1*Ltmp.R/6370.*sin(Ltmp.lat*!dtor),color=120
    endfor

    ;.....NOT IMPLEMENTED YET.
    if keyword_set(oplotX) then begin
      for i=0,n_elements(oplotX[*,0])-1 do oplot,[oplotX[i,0]],[oplotX[i,2]],psym=7,color=colorsX[i]
    endif
    if keyword_set(kvecs) then begin
      for bb=0,n_elements(kx2)-1 do oplot,[xc2[bb],xc2[bb]+kx2[bb]*sizexM],[zc2[bb],zc2[bb]+kz2[bb]*sizezM];,color=100
    endif



  endif else begin

    ;Eq plane circles at constant L
    for i=0,n_elements(Lv)-1 do oplot,replicate(Lv[i],360.)*cos(indgen(360.)*!dtor),replicate(Lv[i],360.)*sin(indgen(360.)*!dtor),color=120

    ;if keyword_set(Lsc) then oplot,replicate(Lst.R[0]/6370.,360.),indgen(360.)*!dtor,/polar,color=120
    if keyword_set(oplotX) then begin
      for i=0,n_elements(oplotX[*,0])-1 do oplot,[oplotX[i,0]],[oplotX[i,1]],psym=7,color=colorsX[i]
    endif
    if keyword_set(kvecs) then begin
      for bb=0,n_elements(kx2)-1 do oplot,[xc2[bb],xc2[bb]+kx2[bb]*sizexE],[yc2[bb],yc2[bb]+ky2[bb]*sizeyE];,color=100
    endif


  endelse



  ;  if ~keyword_set(Lsc) then Lst = dipole(1.001) else begin
  ;		LstR = 0.
  ;		Lstlat = 0.
  ;		for i=0,n_elements(Lsc)-1 do begin
  ;			Lsttmp = dipole(Lsc[i])
  ;			LstR = [LstR,Lsttmp.R]
  ;			Lstlat = [Lstlat,Lsttmp.lat]
  ;		endfor
  ;		Lst = {R:LstR[1:n_elements(LstR)-1],lat:Lstlat[1:n_elements(LstR)-1]}
  ;	endelse





end
