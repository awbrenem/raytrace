;+
;*****************************************************************************************
;
;  FUNCTION : triangulation  
;  PURPOSE  : performs IDL's triangulation and trigrid function and returns
;			  result which is a modified array of xcoord and ycoord that have
;			  been extrapolated to the gridpoints. 
;
;  CALLED BY:   
;               
;
;  CALLS:
;               
;
;  REQUIRES:    
;               
;
;  INPUT:
;               
;
;  EXAMPLES:    
;               
;
;  KEYWORDS:    xg, yg <- returned extrapolated x and y grid values from trigrid
;				limits -> limits (RE) for the gridding routine. Values outside of limits
;						  are cut off [x0,y0,x1,y1]
;				maxlength -> max triangle length in km
;				showtriangles -> keyword flag to plot the triangles
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:      
;               
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-

function triangulation,xcoord,ycoord,zvals,xg=xg,yg=yg,gridspac=gridspac,limits=limits,maxlength=maxlength,showtriangles=showtriangles


triangulate,xcoord,ycoord,Triangles

;Get rid of the long triangles that are artificial and mess up the data.
if keyword_set(maxlength) then begin
for u=0L,n_elements(Triangles[0,*])-1 do begin
      xc=xcoord(Triangles[*,u]) ;vertices of current triangle
      yc=ycoord(Triangles[*,u])
      sideA=sqrt((xc[0]-xc[1])^2 + (yc[0]-yc[1])^2)*6370.
      sideB=sqrt((xc[2]-xc[0])^2 + (yc[2]-yc[0])^2)*6370.
      sideC=sqrt((xc[2]-xc[1])^2 + (yc[2]-yc[1])^2)*6370.
      if (sideA GT maxlength) or (sideB gt maxlength) or (sideC gt maxlength) then Triangles[*,u] = 0
endfor
endif


if keyword_set(showtriangles) then begin
 i=0
  while(i LT n_elements(Triangles)/3 - 1) do begin
      t = [Triangles[*,i],Triangles[0,i]]
      plots,xcoord[t],ycoord[t]
      i=i+1
  endwhile
endif


if keyword_set(gridspac) and keyword_set(limits) then begin

  nlinesx = (gridspac-1)
  nlinesy = (gridspac-1)
  gridspacing = fltarr(2)
  gridspacing(0) = (limits(2) - limits(0))/nlinesx
  gridspacing(1) = (limits(3) - limits(1))/nlinesy

  result = trigrid(xcoord,ycoord,zvals,Triangles,xgrid=xg,ygrid=yg,gridspacing,limits)

  if n_elements(xg) eq (gridspac+1) then xg = xg(0:(gridspac-1))
  if n_elements(yg) eq (gridspac+1) then yg = yg(0:(gridspac-1)) ;occasionally one of these will end up with 402 elements from the trigrid procedure. The last element is the correct one to be eliminated. 

  resx = gridspacing(0)*6370.
  resz = gridspacing(1)*6370.
  resx = strtrim(resx,2)
  resz = strtrim(resz,2)

  print,'resolution in x-direction is (km): ' + resx
  print,'resolution in z-direction is (km): ' + resz


endif else result = trigrid(xcoord,ycoord,zvals,Triangles,xgrid=xg,ygrid=yg)
  
return,result
  
end