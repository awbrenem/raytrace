;Take a 2D array of data from raytracing routines (Meridional plane) and
;extract a slice along a single Lshell or single Mlat
;
;Can either input a single Lshell and array of mlats (L-extract) or an array of Lshells and single mlat (mlat-extract)
;xgrid, zgrid --> grid values (typically SM coord) for ray-traced grid
;gridvalues --> [n,m] array where xgrid is size n and zgrid is size m. Represents
;						values of area covered by rays after a series of rays are inputted into the
;						triangulation routine "triangulate_rays.pro"
;gridpts --> return x,z array indices of the extracted slice. This can be used to reference
;						other arrays returned by triangulate_rays.pro (see crib_raytrace_scenario1.pro)

;See example crib_raytrace_scenario1.pro

;Written by Aaron W Breneman, Jan 31, 2017.

function extract_lshell_mlat_slice,lval,mlat,xgrid,zgrid,gridvalues,gridpts=gridpts


	;mlat-extract
	if size(lval,/n_elements) eq 1 then begin

		gridvals_final = fltarr(n_elements(mlat))
		gridx = fltarr(n_elements(mlat))
		gridz = gridx

		for i=0,n_elements(mlat)-1 do begin
			radt = lval*cos(mlat[i]*!dtor)^2
			x_sm = radt*cos(mlat[i]*!dtor)
			z_sm = radt*sin(mlat[i]*!dtor)
			goo = where(xgrid ge x_sm)
			gridx[i] = goo[0]
			goo = where(zgrid ge z_sm)
			gridz[i] = goo[0]
			gridvals_final[i] = gridvalues[gridx[i],gridz[i]]

		endfor
	endif



	;lshell-extract
	if size(mlat,/n_elements) eq 1 then begin

		gridvals_final = fltarr(n_elements(lval))
		gridx = fltarr(n_elements(lval))
		gridz = gridx

		for i=0,n_elements(lval)-1 do begin
			radt = lval*cos(lval[i]*!dtor)^2
			x_sm = radt*cos(lval[i]*!dtor)
			z_sm = radt*sin(lval[i]*!dtor)
			goo = where(xgrid ge x_sm)
			gridx[i] = goo[0]
			goo = where(zgrid ge z_sm)
			gridz[i] = goo[0]
			gridvals_final[i] = gridvalues[gridx[i],gridz[i]]
		endfor
	endif

	gridpts = [[gridx],[gridz]]

	return,gridvals_final

end
