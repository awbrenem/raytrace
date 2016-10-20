pro tritest2
  window
  seed = 11
  n=400
  x=randomu(seed,n)
  y=randomu(seed,n)
  z = fltarr(n)

  for i=0,n-2 do begin
          z(i)=exp(-3*((x(i)-0.5)^2+(y(i)-0.5)^2))
  endfor

  plot,x,y,psym=1,title='Random test'
  wdelete

  window
  triangulate,x,y,tr

  plot,x,y,psym=1,title='triangles'
  for i=0,n_elements(tr)/3 - 1 do begin
      t=[tr[*,i],tr[0,i]]
      plots,x[t],y[t]
  endfor
  nlines = 400 - 1
  gridspacing = fltarr(2)
  gridspacing = [1./nlines,1./nlines]
  limits = [0,0,1,1]
  result = trigrid(x,y,z,tr,xgrid=xg,ygrid=yg,gridspacing,limits)

  diff_work = fltarr(n,n)

  for i=0,n-2 do begin
      for j=0,n-2 do begin
          diff_work(i,j)=15*[exp(-3*((x(i)-0.5)^2+(y(j)-0.5)^2)),exp(-3*((x(i)-0.5)^2+(y(j)-0.5)^2))]
      endfor
  endfor

  loadct,2
  contour,diff_work,xg,yg,levels=lvls,/fill

  rx = randomu(5,400)
  rx = floor(400*rx)
  ry = randomu(8,400)
  ry = floor(400*ry)

;  for i=0,n-2 do begin
;      for j=0,n-2 do begin
          ;diff_work(rx(i),ry(j)) = 0
          ;diff_work(rx(j),ry(i)) = 0
          diff_work(100:120,*) = 0
          diff_work(*,100:120) = 0
          diff_work(200:399,*) = 0
          diff_work(*,200:399) = 0

;      endfor
;  endfor

  ;set_plot,'ps'
  ;device,filename = 'myfile.ps'

  contour,diff_work,xg,yg,levels=lvls,/fill

  ;device,/close
  ;set_plot,'x'


  xc = fltarr(1)
  yc = fltarr(1)
  vertices = fltarr(200000,2)
  vertices(*,*) = !values.f_nan
  ctr = 0
  for i=0,n-1 do begin
      xt = where(diff_work(*,i) ne 0.)
      if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),0) = xt
      if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),1) = i

      ctr = ctr + n_elements(xt)
  endfor
stop
  tmp = where(finite(vertices(*,0)) eq 1)
  vertices_tmp = fltarr(n_elements(tmp),2)
  vertices_tmp(*,0) = vertices(tmp,0)
  vertices_tmp(*,1) = vertices(tmp,1)
  vertices = vertices_tmp
;  maxline = 0.2
  maxgridspac = 4.
  maxline = sqrt(2)*maxgridspac

  for i=0L,n_elements(vertices(*,0))-1 do begin   ;for each vertex pair
      
      maxgx = vertices(i,0) + maxgridspac
      mingx = vertices(i,0) - maxgridspac
      maxgy = vertices(i,1) + maxgridspac
      mingy = vertices(i,1) - maxgridspac

      within_dist = where((vertices(*,0) ge mingx) and (vertices(*,0) le maxgx) and (vertices(*,1) ge mingy) and (vertices(*,1) le maxgy))
      vertices_red = fltarr(n_elements(within_dist),2)
      vertices_red(*,0) = vertices(within_dist,0)
      vertices_red(*,1) = vertices(within_dist,1)

      ;quadrant method

      for b=0L,n_elements(vertices_red(*,0))-1 do begin
          maxy = max(vertices_red(*,1))
          miny = min(vertices_red(*,1))
          maxx = max(vertices_red(*,0))
          minx = min(vertices_red(*,0))
          refx = vertices_red(b,0)
          refy = vertices_red(b,1)

          if maxx gt refx and maxy gt refy then diff_work(refx:maxx,refy:maxy) = 50
          if maxx gt refx and refy gt miny then diff_work(refx:maxx,miny:refy) = 50
          if refx gt minx and refy gt miny then diff_work(minx:refx,miny:refy) = 50
          if refx gt minx and maxy gt refy then diff_work(minx:refx,refy:maxy) = 50                    
      endfor

;      contour,diff_work,xg,yg,levels=lvls,/fill
  endfor

  contour,diff_work,xg,yg,levels=lvls,/fill

stop














      for b=0L,n_elements(vertices_red(*,0))-1 do begin
          for j=0L,n_elements(vertices_red(*,0))-1 do begin
              if b ne j then begin
                  ;line1 = abs((yg((vertices_red(j,1))) - yg(vertices_red(b,1)))/(xg((vertices_red(j,0))) - xg(vertices_red(b,0))))
                                ;print,line1                  
    
                  line1 = sqrt(((vertices_red(j,1)-vertices_red(b,1))^2 + ((vertices_red(j,0)-vertices_red(b,0))^2)))
              
                  if line1 gt 0 and line1 lt maxline and finite(line1) ne 0 then begin
                      for q=0L,n_elements(vertices_red(*,0))-1 do begin
                          if q ne i and q ne j then begin
;                              line2 = abs((yg((vertices_red(q,1)))-yg(vertices_red(b,1)))/(xg((vertices_red(q,0)))-xg(vertices_red(b,0))))
;                              line3 = abs((yg((vertices_red(q,1)))-yg(vertices_red(j,1)))/(xg((vertices_red(q,0)))-xg(vertices_red(j,0))))                                 
                              line2 = sqrt(((vertices_red(q,1)-vertices_red(b,1))^2 + ((vertices_red(q,0)-vertices_red(b,0))^2)))
                              line3 = sqrt(((vertices_red(q,1)-vertices_red(j,1))^2 + ((vertices_red(q,0)-vertices_red(j,0))^2)))

 ;stop                          
                              if line2 gt 0 and line2 lt maxline and finite(line2) ne 0 and line3 gt 0 and line3 lt maxline and finite(line3) ne 0 then begin
                                ;all the triangle sides are within the
;limits
                                  x=[vertices_red(b,0),vertices_red(j,0),vertices_red(q,0)]
                                  y=[vertices_red(b,1),vertices_red(j,1),vertices_red(q,1)]
;stop
                                  result = polyfillv(x,y,400,400)
                                  if result(0) ne -1 then diff_work(result) = 50.
                                contour,diff_work,xg,yg,levels=lvls,/fill
                                  
                              endif
                          endif
                      endfor
                  endif
                                ;result = polyfillv(vertices(0,*),vertices(1,*),32,32)
              endif
          endfor
      endfor
  ;endfor
      
  
    contour,diff_work,xg,yg,levels=lvls,/fill

stop




end
