;creates the "mastertiming" array

;calls triangulation


pro overlap_rays,array1,array2

restore,file1

zvals = timeG

nrays = n_elements(zvals[0,*])


;remove all of the NaN values and turn into 1D array

goodvals = where(finite(xcoord[*,0]) ne 0.)

xc = xcoord[goodvals,0]
yc = ycoord[goodvals,0]
zc = zvals[goodvals,0]

for ii=1,nrays-1 do begin
	goodvals = where(finite(xcoord[*,ii]) ne 0.)
	xc = [xc,xcoord[goodvals,ii]]
	yc = [yc,ycoord[goodvals,ii]]
	zc = [zc,zvals[goodvals,ii]]
endfor

tri1 = triangulation(xc,yc,zc,xg=xg,yg=yg)




restore,file2

zvals = timeG

nrays = n_elements(zvals[0,*])


;remove all of the NaN values and turn into 1D array

goodvals = where(finite(xcoord[*,0]) ne 0.)

xc = xcoord[goodvals,0]
yc = ycoord[goodvals,0]
zc = zvals[goodvals,0]

for ii=1,nrays-1 do begin
	goodvals = where(finite(xcoord[*,ii]) ne 0.)
	xc = [xc,xcoord[goodvals,ii]]
	yc = [yc,ycoord[goodvals,ii]]
	zc = [zc,zvals[goodvals,ii]]
endfor


tri2 = triangulation(xc,yc,zc,xg=xg,yg=yg)

stop











mastertiming=fltarr(gridspac,gridspac)    ;array that will be sent to TRIANGULATE



end