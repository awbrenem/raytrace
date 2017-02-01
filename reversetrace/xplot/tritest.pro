pro tritest
  window
  seed = 11
  n=1000
  x=randomu(seed,n)
  y=randomu(seed,n)
  z = fltarr(n)

  plot,x,y,psym=1,title='Random test'
stop
  wdelete
	z = indgen(1000)


triangulate,x,y,tr

  plot,x,y,psym=1,title='triangles'
  for i=0,n_elements(tr)/3 - 1 do begin
      t=[tr[*,i],tr[0,i]]
      plots,x[t],y[t]
  endfor
PRINT,'ALL TRIANGLES'
stop
  u=0
  u=long(u)
  nelem=n_elements(tr(0,*))
  while(u LT nelem) do begin
      xc=x(tr(*,u)) ;vertices of current triangle
      zc=y(tr(*,u))

      sideA=sqrt((xc(0)-xc(1))^2 + (zc(0)-zc(1))^2)
      sideB=sqrt((xc(2)-xc(0))^2 + (zc(2)-zc(0))^2)
      sideC=sqrt((xc(2)-xc(1))^2 + (zc(2)-zc(1))^2)

      test='no'
;0.0157 is equivalent to 100 km resolution.
      if(sideA GT 0.04) then test='yes'
      if(sideB GT 0.04) then test='yes'  ;gets rid of the long triangles that mess up the
      if(sideC GT 0.04) then test='yes'  ;triangulation
      if(test EQ 'yes') then tr(*,u) = 0
      u=u+1
  endwhile

  plot,x,y,psym=1,title='triangles'
  for i=0,n_elements(tr)/3 - 1 do begin
      t=[tr[*,i],tr[0,i]]
      plots,x[t],y[t]
  endfor
print,'LONG TRIANGLES REMOVED'
stop

	  nlines = 400 - 1
	  gridspacing = fltarr(2)
	  gridspacing = [1./nlines,1./nlines]
	  limits = [0,0,1,1]

	  result = trigrid(x,y,z,tr,xgrid=xg,ygrid=yg,gridspacing,limits)

	diff_work = result

diff_work_tmp = fltarr(400,400)
for i=0L,399 do begin
	tmp = where(diff_work(*,i) ne 0)
	if tmp(0) ne -1 then diff_work_tmp(tmp,i) = 1
endfor

diff_work = diff_work_tmp

  loadct,2
  contour,diff_work,xg,yg,levels=lvls,/fill
print,'DIFF WORK INIT'
stop



;###################################

	vertices = fltarr(200000,2)
  	vertices(*,*) = !values.f_nan
  	ctr = 0
	  for i=0,399 do begin
	      xt = where(diff_work(*,i) ne 0.)
	      if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),0) = xt
	      if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),1) = i
	      ctr = ctr + n_elements(xt)
	  endfor
  tmp = where(finite(vertices(*,0)) eq 1)
  vertices_tmp = fltarr(n_elements(tmp),2)
  vertices_tmp(*,0) = vertices(tmp,0)
  vertices_tmp(*,1) = vertices(tmp,1)
  vertices = vertices_tmp

triangulate,vertices(*,0),vertices(*,1),tr2

  plot,vertices(*,0),vertices(*,1),psym=1,title='triangles'
  for i=0L,n_elements(tr2)/3 - 1 do begin
      t=[tr2[*,i],tr2[0,i]]
      plots,vertices(t,0),vertices(t,1)
  endfor
PRINT,'SECOND TRIANGULATION'
stop

  u=0
  u=long(u)
  nelem=n_elements(tr2(0,*))
  while(u LT nelem) do begin
      xc=vertices(tr2(*,u),0)/400. ;vertices of current triangle
      zc=vertices(tr2(*,u),1)/400.

      sideA=sqrt((xc(0)-xc(1))^2 + (zc(0)-zc(1))^2)
      sideB=sqrt((xc(2)-xc(0))^2 + (zc(2)-zc(0))^2)
      sideC=sqrt((xc(2)-xc(1))^2 + (zc(2)-zc(1))^2)

      test='no'
;0.0157 is equivalent to 100 km resolution.
      if(sideA GT 0.075) then test='yes'
      if(sideB GT 0.075) then test='yes'  ;gets rid of the long triangles that mess up the
      if(sideC GT 0.075) then test='yes'  ;triangulation
      if(test EQ 'yes') then tr2(*,u) = 0
      u=u+1
  endwhile

 plot,vertices(*,0),vertices(*,1),psym=1,title='triangles'
  for i=0L,n_elements(tr2)/3 - 1 do begin
      t=[tr2[*,i],tr2[0,i]]
      plots,vertices(t,0),vertices(t,1)
  endfor
print,'LONG TRIANGLES REMOVED'
stop

z = fltarr(n_elements(vertices(*,0)))
z(*)= 50
;z = indgen(n_elements(vertices(*,0)))

xs = vertices(*,0)/400.
ys = vertices(*,1)/400.
result=trigrid(xs,ys,z,tr2,xgrid=xg,ygrid=yg,gridspacing,limits)
diff_work2 = result
 contour,diff_work2,xg,yg,levels=lvls,/fill

print,'FINAL STOP'
stop
end
