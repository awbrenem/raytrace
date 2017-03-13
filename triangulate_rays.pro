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
;				    longit -> ray longitude. Needed to extract the Meridional plane (xcoord/cos(longit))
;           vals -> [n,3] array of ray values of desired quantity
;           nlines --> number of grid lines (defaults to 400-1)
;           limits --> [x0,y0,x1,y1] limits for gridding
;           xrange --> plot x range. Defaults to x "limits"
;           yrange --> plot y range. Defaults to y "limits"
;           minv --> min value of "vals" to plot.
;           maxv --> max value of "vals" to plot.
;           psonly --> plot ps of file (to Desktop)
;           zbuffer --> small filesize png plot (to Desktop)
;           maxtrianglesize -> Triangles longer than this (RE) will be removed
;             defaults to 0.1
;           noplot --> don't do the contour plot, which takes time
;
; REQUIRES:
; HISTORY: Written by AWB 2016-12-22
; VERSION:
;-


pro triangulate_rays,xcoord,ycoord,zcoord,longit,vals,$
  nlines=nlines,limits=limits,minv=minv,maxv=maxv,$
  nlvls=nlvls,xrange=xrange,yrange=yrange,lsc=lsc,$
  xgrid=xg,ygrid=yg,result=result,$
  mlats=mlats,lvals=lvals,rads=rads,ilats=ilats,$
  psonly=psonly,zbuffer=zbuffer,$
  maxtrianglesize=mts,noplot=np



  if KEYWORD_SET(zbuffer) then begin
    thisDevice = !D.Name
    Set_Plot, 'Z'
    Device, Set_Resolution=[500,500], Decomposed=0;, Set_Pixel_Depth=24
  endif


  !p.multi = [0,0,1]

  if KEYWORD_SET(psonly) then popen,'~/Desktop/triangulaterays_output.ps'

  if ~KEYWORD_SET(mts) then mts = 0.1

  if ~keyword_set(nlines) then nlines = 400 - 1
  gridspacing = fltarr(2)
  gridspacing = [1./nlines,1./nlines]
  if ~KEYWORD_SET(limits) then limits = [0,-2,6,0]
  if ~keyword_set(minv) then minv = min(vals)
  if ~keyword_set(maxv) then maxv = max(vals)
  if ~KEYWORD_SET(nlvls) then nlvls=10


  ;create relative longitude variable. The initial longitude for each ray helps to define
  ;the meridional plane. As the longitude changes for each ray we must take this into account.
  longit_ref = longit[0,*]
  longit_relative = longit
  longit_relative[*] = 0.
  for bb=0,n_elements(longit[0,*])-1 do longit_relative[*,bb] = longit[*,bb] - longit_ref[bb]


  ;turn into 1D arrays
  xv = reform(reform(xcoord,n_elements(xcoord),1))
  yv = reform(reform(ycoord,n_elements(ycoord),1))
  zv = reform(reform(zcoord,n_elements(zcoord),1))
  valsF = reform(reform(vals,n_elements(ycoord),1))
  lg = reform(reform(longit,n_elements(longit),1))
  lg_relative = reform(reform(longit_relative,n_elements(longit_relative),1))



  if ~KEYWORD_SET(xrange) then xrange=[limits[0],limits[2]]
  if ~KEYWORD_SET(yrange) then yrange=[limits[1],limits[3]]



  ;get rid of NaNs
  goo = where(finite(xv) eq 0.)
  if goo[0] ne -1 then xv[goo] = 0.
  goo = where(finite(yv) eq 0.)
  if goo[0] ne -1 then yv[goo] = 0.
  goo = where(finite(zv) eq 0.)
  if goo[0] ne -1 then zv[goo] = 0.
  goo = where(finite(lg_relative) eq 0.)
  if goo[0] ne -1 then lg_relative[goo] = 0.

  ;get vertices of Meridional plane triangles and plot
  meridx = sqrt(xv^2 + yv^2)
  xv_merid = meridx*cos(lg_relative*!dtor)

  triangulate,xv_merid,zv,tr

  ;plot,xv_merid,zv,psym=1,title='triangles'
  ;for i=0,n_elements(tr)/3 - 1 do begin ;$
  ;    t=[tr[*,i],tr[0,i]]  ;& $
  ;    plots,xv_merid[t],zv[t]
  ;endfor
  ;stop


  ;Remove long, unphysical triangles
  u=0
  u=long(u)
  nelem=n_elements(tr[0,*])
  while(u LT nelem) do begin ;$
    xc=xv_merid(tr[*,u]) ;& $  ;vertices of current triangle
    zc=zv(tr[*,u]) ;& $

    sideA=sqrt((xc[0]-xc[1])^2 + (zc[0]-zc[1])^2); & $
    sideB=sqrt((xc[2]-xc[0])^2 + (zc[2]-zc[0])^2); & $
    sideC=sqrt((xc[2]-xc[1])^2 + (zc[2]-zc[1])^2); & $
    fail=0

    if(sideA GT mts) then fail=1 ;& $
    if(sideB GT mts) then fail=1 ;& $  ;gets rid of the long triangles that mess up the
    if(sideC GT mts) then fail=1 ;& $  ;triangulation
    if fail then tr[*,u] = 0 ;& $

    u=u+1
  endwhile


  ;  plot,xv_merid,zv,title='triangles',xrange=[0,6],yrange=[-2,0],background=255,/nodata
  ;  for i=0,n_elements(tr)/3 - 1 do begin ;$
  ;    t=[tr[*,i],tr[0,i]] ;& $
  ;    plots,xv_merid[t],zv[t]
  ;  endfor
  print,'LONG TRIANGLES REMOVED'



  result = trigrid(xv_merid,zv,valsF,tr,xgrid=xg,ygrid=yg,gridspacing,limits)


  if ~KEYWORD_SET(np) then begin

    contour,result,xg,yg,nlevels=nlvls,min_value=minv,max_value=maxv,$
    /cell_fill,xrange=xrange,yrange=yrange,xstyle=1,ystyle=1,$
    background=255,position=aspect(1),color=2

    oplot_earth_mlat_L_lines,Lv=[2,4,5,6,8]



    ;plot colorbar
    loadct,39  ;need the first element to be black
    nticks = 7.
    tn = (indgen(nticks)/(nticks-1))*(maxv-minv)  + minv
    tn = strtrim(string(tn,format='(f8.2)'),2)
    colorbar,POSITION=[0.15, 0.75, 0.85, 0.77],$
    divisions=nticks-1,ticknames=tn,charsize = 0.8,range=[minv,maxv],color=2



  endif ;plot condition


  ;Calculate position values for each point crossed by a ray path
  mlats = fltarr(n_elements(xg),n_elements(yg))
  lvals = fltarr(n_elements(xg),n_elements(yg))
  rads = fltarr(n_elements(xg),n_elements(yg))
  binvar = fltarr(n_elements(xg),n_elements(yg))

  ;  ;Calculate position values of origin of ray for each point crossed by a ray path
  ;  ;i.e. the value is a constant along a raypath and represents the value at ray inception.
  mlat0 = fltarr(n_elements(xg),n_elements(yg))
  lval0 = fltarr(n_elements(xg),n_elements(yg))
  rad0 = fltarr(n_elements(xg),n_elements(yg))
  binvar0 = fltarr(n_elements(xg),n_elements(yg))



  for i=0,n_elements(xg)-1 do begin
    for j=0,n_elements(yg)-1 do begin
      rads[i,j] = sqrt(xg[i]^2 + yg[j]^2)
      mlats[i,j] = atan(yg[j]/xg[i])/!dtor
    endfor
  endfor

  lvals = rads/(cos(!dtor*mlats)^2)  ;L-shell in centered dipole
  ilats = acos(sqrt(1/lvals))/!dtor  ;invariant latitude

  ;remove values where there is no ray_vals
  goo = where(result ne 0.)
  if goo[0] ne -1 then binvar[goo] = 1.

  mlats *= binvar
  lvals *= binvar
  rads *= binvar
  ilats *= binvar

  if KEYWORD_SET(psonly) then pclose

  if KEYWORD_SET(zbuffer) then begin
    void = cgSnapshot(File='~/Desktop/triangulate_rays_plot', /PNG, /NoDialog)
    Set_Plot, thisDevice
  endif

end
