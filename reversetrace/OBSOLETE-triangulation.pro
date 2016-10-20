;performs IDL's triangulation and trigrid function and returns
;result which is a modified array of xcoord and zcoord that have
;been extrapolated to the gridpoints. 


Pro triangulation,raydata,result=result,xg=xg,zg=zg,xcoord=xcoord,zcoord=zcoord
common stuff,timetype,magnetic_multiplier,density_ref,phi,r_sc1,r_sc2,lat_sc1,lat_sc2,thetaoffres1,thetaoffres2,final_lat,final_alt,region,alt_sc1,alt_sc2,event,infoarray,finalfreq,limits,include_details,ppdensitychange,ppL_halfwidth,ppL_centralpos,raydata_sc1,raydata_sc2,gridspac,abcx,zzones,sca_f1_lo,sca_f1_hi,sca_f2_lo,sca_f2_hi,scb_f1_lo,scb_f1_hi,scb_f2_lo,scb_f2_hi
  ;goodindices=where(raydata(2,*) NE 0.)

  goodindices = where(raydata NE 0.)

  ;xcoord = raydata(2,goodindices)
  ;zcoord = raydata(3,goodindices)
  ;timing = raydata(0,goodindices)
  
  z = FLTARR(n_elements(xcoord))
  i=0
  i = long(i)
  while(i LT n_elements(xcoord)) do begin
      z(i) = 1
      i = i+1
  endwhile

;##################
  triangulate,xcoord,zcoord,Triangles

;following code gets rid of the long triangles that are artificial and
;mess up the data.
  u=0
  u=long(u)
  nelem=n_elements(Triangles(0,*))
  while(u LT nelem) do begin
      xc=xcoord(Triangles(*,u)) ;vertices of current triangle
      zc=zcoord(Triangles(*,u))

      sideA=sqrt((xc(0)-xc(1))^2 + (zc(0)-zc(1))^2)
      sideB=sqrt((xc(2)-xc(0))^2 + (zc(2)-zc(0))^2)
      sideC=sqrt((xc(2)-xc(1))^2 + (zc(2)-zc(1))^2)

      test='no'
;0.0157 is equivalent to 100 km resolution. 
      if(sideA GT 0.0157) then test='yes'
      if(sideB GT 0.0157) then test='yes'  ;gets rid of the long triangles that mess up the 
      if(sideC GT 0.0157) then test='yes'  ;triangulation

      if(test EQ 'yes') then Triangles(*,u) = 0
      u=u+1
  endwhile

;#####################################################
;  the following code is for displaying the triangles
  ;i=0
  ;while(i LT  n_elements(Triangles)/3 - 1) do begin
  ;    t = [Triangles(*,i),Triangles(0,i)]
  ;    plots,xcoord(t),zcoord(t)
  ;    ;print,xcoord(t)
  ;    ;print,zcoord(t)
  ;    i=i+1
  ;endwhile

;stop
;#####################################################
  ;limits = [4.3,-0.5,5.0,0.5]
  nlinesx = (gridspac-1)
  nlinesy = (gridspac-1)
  gridspacing = fltarr(2)
  gridspacing(0) = (limits(2) - limits(0))/nlinesx
  gridspacing(1) = (limits(3) - limits(1))/nlinesy

  result = trigrid(xcoord,zcoord,raydata,Triangles,xgrid=xg,ygrid=zg,gridspacing,limits)
;#####################

  if n_elements(xg) eq (gridspac+1) then xg = xg(0:(gridspac-1))
  if n_elements(zg) eq (gridspac+1) then zg = zg(0:(gridspac-1)) ;occasionally one of these will end up with 402 elements from the trigrid procedure. The last element is the correct one to be eliminated. 

  resx = gridspacing(0)*6370.
  resz = gridspacing(1)*6370.
  resx = strtrim(resx,2)
  resz = strtrim(resz,2)

;#######fix to only print this once##############  
  print,'resolution in x-direction is (km): ' + resx
  print,'resolution in z-direction is (km): ' + resz
  infoarray(25) = 'max grid resolution in x-direction is (km): ' + resx
  infoarray(26) = 'max grid resolution in z-direction is (km): ' + resz

  ;set up to manually produce 401x401 grid
  ;xg and zg are the extrapolated xvalues and yvalues.
end