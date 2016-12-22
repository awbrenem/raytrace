;+
; NAME: triangulate_rays
;
; SYNTAX:
; PURPOSE: Plot a bunch of ray paths and their values (e.g. energy) on a regularly
;          gridded plot using triangulation and trigrid
; INPUT: (see crib_raytrace_microburst_chorus.pro)
; OUTPUT:
;
; KEYWORDS: xcoord -> [n,3] array of ray xcoordinate values
;           ycoord -> [n,3] array of ray ycoordinate values
;           vals -> [n,3] array of ray values of desired quantity
;           nlines --> number of grid lines (defaults to 400-1)
;           limits --> [x0,y0,x1,y1] limits for gridding
;           xrange --> plot x range. Defaults to x "limits"
;           yrange --> plot y range. Defaults to y "limits"
;           minv --> min value of "vals" to plot.
;           maxv --> max value of "vals" to plot.
;
; REQUIRES:
; HISTORY: Written by AWB 2016-12-22
; VERSION:
;-


pro triangulate_rays,xcoord,ycoord,vals,$
    nlines=nlines,limits=limits,minv=minv,maxv=maxv,$
    nlvls=nlvls,xrange=xrange,yrange=yrange,lsc=lsc

!p.multi = [0,0,2]

if ~keyword_set(nlines) then nlines = 400 - 1
gridspacing = fltarr(2)
gridspacing = [1./nlines,1./nlines]
if ~KEYWORD_SET(limits) then limits = [0,-2,6,0]

if ~keyword_set(minv) then minv = min(vals)
if ~keyword_set(maxv) then maxv = max(vals)

if ~KEYWORD_SET(nlvls) then nlvls=10

;turn into 1D arrays
xv = reform(reform(xcoord,n_elements(xcoord),1))
yv = reform(reform(ycoord,n_elements(ycoord),1))
valsF = reform(reform(vals,n_elements(ycoord),1))

if ~KEYWORD_SET(xrange) then xrange=[limits[0],limits[2]]
if ~KEYWORD_SET(yrange) then yrange=[limits[1],limits[3]]



;get rid of NaNs
goo = where(finite(xv) eq 0.)
if goo[0] ne -1 then xv[goo] = 0.
goo = where(finite(yv) eq 0.)
if goo[0] ne -1 then yv[goo] = 0.


;get vertices of triangles and plot
triangulate,xv,yv,tr


;plot,xv,yv,psym=1,title='triangles'
;for i=0,n_elements(tr)/3 - 1 do begin ;$
;    t=[tr[*,i],tr[0,i]]  ;& $
;    plots,xv[t],yv[t]
;endfor
;stop


;Remove long, unphysical triangles
      u=0
      u=long(u)
      nelem=n_elements(tr[0,*])
      while(u LT nelem) do begin ;$
          xc=xv(tr[*,u]) ;& $  ;vertices of current triangle
          yc=yv(tr[*,u]) ;& $

          sideA=sqrt((xc[0]-xc[1])^2 + (yc[0]-yc[1])^2); & $
          sideB=sqrt((xc[2]-xc[0])^2 + (yc[2]-yc[0])^2); & $
          sideC=sqrt((xc[2]-xc[1])^2 + (yc[2]-yc[1])^2); & $
          fail=0

          if(sideA GT 0.1) then fail=1 ;& $
          if(sideB GT 0.1) then fail=1 ;& $  ;gets rid of the long triangles that mess up the
          if(sideC GT 0.1) then fail=1 ;& $  ;triangulation
          if(fail) then tr[*,u] = 0 ;& $

          u=u+1
      endwhile

      plot,xv,yv,title='triangles',xrange=[0,6],yrange=[-2,0]
      for i=0,n_elements(tr)/3 - 1 do begin ;$
          t=[tr[*,i],tr[0,i]] ;& $
          plots,xv[t],yv[t]
      endfor

    print,'LONG TRIANGLES REMOVED'


	  result = trigrid(xv,yv,valsF,tr,xgrid=xg,ygrid=yg,gridspacing,limits)

    contour,result,xg,yg,nlevels=nlvls,min_value=minv,max_value=maxv,$
        /cell_fill,xrange=xrange,yrange=yrange,xstyle=1,ystyle=1



;plot colorbar
      loadct,39  ;need the first element to be black
  		nticks = 7.
  		tn = (indgen(nticks)/(nticks-1))*(maxv-minv)  + minv
  		tn = strtrim(string(tn,format='(f8.2)'),2)
  		colorbar,POSITION=[0.15, 0.50, 0.95, 0.52],$
  		divisions=nticks-1,ticknames=tn,charsize = 0.8,range=[minv,maxv]



      ;overplot earth, L shells, etc.
      earthx = COS((2*!PI/99.0)*FINDGEN(100))
    	earthy = SIN((2*!PI/99.0)*FINDGEN(100))

    	;latitude lines
    	lats = [0,10,20,30,40,50,60,70,80]
    	lats = [-1*reverse(lats[1:n_elements(lats)-1]),lats]

    	L2 = dipole(2.)
    	L4 = dipole(4.)
    	L6 = dipole(6.)
    	L8 = dipole(8.)

      if ~keyword_set(Lsc) then Lst = dipole(1.001) else begin
    		LstR = 0.
    		Lstlat = 0.
    		for i=0,n_elements(Lsc)-1 do begin
    			Lsttmp = dipole(Lsc[i])
    			LstR = [LstR,Lsttmp.R]
    			Lstlat = [Lstlat,Lsttmp.lat]
    		endfor
    		Lst = {R:LstR[1:n_elements(LstR)-1],lat:Lstlat[1:n_elements(LstR)-1]}
    	endelse


      oplot,earthx,earthy,color=60
    	;oplot,replicate(1.078,360.),indgen(360.)*!dtor,/polar,color=80

    	oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80
    	oplot,L2.R/6370.*cos(L2.lat*!dtor),L2.R/6370.*sin(L2.lat*!dtor),color=120
    	oplot,L2.R/6370.*cos(L2.lat*!dtor),-1*L2.R/6370.*sin(L2.lat*!dtor),color=120

    	oplot,L4.R/6370.*cos(L4.lat*!dtor),L4.R/6370.*sin(L4.lat*!dtor),color=120
    	oplot,L4.R/6370.*cos(L4.lat*!dtor),-1*L4.R/6370.*sin(L4.lat*!dtor),color=120

    	oplot,L6.R/6370.*cos(L6.lat*!dtor),L6.R/6370.*sin(L6.lat*!dtor),color=120
    	oplot,L6.R/6370.*cos(L6.lat*!dtor),-1*L6.R/6370.*sin(L6.lat*!dtor),color=120

    	oplot,L8.R/6370.*cos(L8.lat*!dtor),L8.R/6370.*sin(L8.lat*!dtor),color=120
    	oplot,L8.R/6370.*cos(L8.lat*!dtor),-1*L8.R/6370.*sin(L8.lat*!dtor),color=120


      if keyword_set(Lsc) then oplot,Lst.R/6370.*cos(Lst.lat*!dtor),Lst.R/6370.*sin(Lst.lat*!dtor),color=160 & oplot,Lst.R/6370.*cos(Lst.lat*!dtor),-1*Lst.R/6370.*sin(Lst.lat*!dtor),color=160
    	for i=0,n_elements(lats)-1 do oplot,[1,50]*cos([lats[i]*!dtor,lats[i]*!dtor]),[1,50]*sin([lats[i]*!dtor,lats[i]*!dtor]),linestyle=3,color=100

    	;for i=0,n_elements(lats)-1 do oplot,[1,50],[lats[i]*!dtor,lats[i]*!dtor],/polar,linestyle=3,color=100


;    	if keyword_set(oplotX) then begin
;    		for i=0,n_elements(oplotX[*,0])-1 do oplot,[oplotX[i,0]],[oplotX[i,2]],psym=7,color=colorsX[i]
;    	endif
;    	if keyword_set(kvecs) then begin
;    		for bb=0,n_elements(kx2)-1 do oplot,[xc2[bb],xc2[bb]+kx2[bb]*sizexM],[zc2[bb],zc2[bb]+kz2[bb]*sizezM];,color=100
;    	endif


end