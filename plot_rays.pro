;+
;*****************************************************************************************
;
;  PROCEDURE : plot_rays
;  PURPOSE  :  plots ray output from trace.f.
;							 Tested fairly extensively and seems to be working
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
;  EXAMPLES:    ;Input with structure option. Structure returned from read_trace_ta.pro
;					plot_rays,ray_struct=struct
;
;
;  KEYWORDS:    rayx -> (*,n_rays) - Meridional x values (SM coord) for ray (RE)
;				rayy -> Equatorial y
;				rayz -> Meridional z
;				longit -> ray longitude. Needed to extract the Meridional plane (xcoord/cos(longit))
;				ray_struct -> the structure returned from read_trace_ta()
;				xrangeM -> Meridional x range in RE
;				zrangeM -> Meridional z range in RE
;				xrangeE -> Equatorial x range in RE
;				zrangeE -> Equatorial y range in RE
;				colors -> color for each ray
;				colorsX -> color for each X overplotted
;				oplotX -> [n,3] array of xcoord,ycoord,zcoord to overplot the symbol 'X'
;				kvecs  -> overplot the k-unit-vectors at equally spaced intervals
;							[n,3] array of x,y,z values
;				Lsc    -> Overplots L-shell of sc
;				psonly -> only plot ps (to ~/Desktop/rayplot.ps)
;				k_spacing -> spacing of k-vector arrows (km). Defaults to 300 km
;				minv, maxv -> min and max color values to plot
;				raycolor -> pretty clear what this is...
;				geocoord -> The IGRF model in trace.for requires input in geographic coord
;						and outputs geographic coord. Set this keyword to transform this
;						output to SM coord before plotting. MUST ALSO SET GEOTIME HERE
;						SO COTRANS KNOWS HOW TO PROPERLY TRANSFORM
;				geotime -> Time that the ray tracing occurs. Necessary for coord transformation
;						from geographic coord to SM coord (e.g. '2016-01-20/19:44')

;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:
;
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  11/01/2011   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;			-Changed all polar plots to xyz plots. For some reason plotting with the polar
;				keyword in IDL is inaccurate.
;*****************************************************************************************
;-

pro plot_rays,rayx,rayy,rayz,longit,ray_vals=ray_vals,$
	xrangeM=xrangeM,zrangeM=zrangeM,xrangeE=xrangeE,yrangeE=yrangeE,$
	ray_struct=ray_struct,$
	colors=colors,colorsX=colorsX,$
	oplotX=oplotX,$
	kvecs=kvecs,$
	Lsc=Lsc,$
	psonly=psonly,$
	k_spacing=k_spacing,$
	minval=minv,maxval=maxv,$
	raycolor=raycolor,$
	geocoord=geocoord,geotime=geotime


	if ~KEYWORD_SET(raycolor) then raycolor = 254
	if ~KEYWORD_SET(minv) then minv=10.
	if ~KEYWORD_SET(maxv) then maxv=500.
	if ~KEYWORD_SET(alpha) then alpha = 0.
	if ~keyword_set(k_spacing) then k_spacing = 300.
	if KEYWORD_SET(geocoord) and ~KEYWORD_SET(geotime) then begin
		print,'*********************'
		print,'*********************'
		print,'*********************'
		print,'NEED TO INPUT A TIME (GEOTIME) FOR THE TRANSFORMATION FROM GEOGRAPHIC COORD TO SM COORD'
		print,'....returning'
		print,'*********************'
		print,'*********************'
		print,'*********************'
		return
	endif


	if ~keyword_set(psonly) then begin

		;modify color table so first element is white
		rbsp_efw_init
		loadct,39
		TVLCT, red, green, blue, /GET
		red[0] = 254.
		blue[0] = 254.
		green[0] = 254.
		file = '~/Desktop/code/Aaron/github.umn.edu/raytrace/myidlcolors1.tbl'
		MODIFYCT, 75, 'aaronct', red, green, blue, FILE=file
		loadct,75,file=file


		window,2,xsize=400,ysize=750
	endif else begin
		win = {xsize:4.1,ysize:8.,xoffset:0.425,yoffset:1.68,inches:1,portrait:1,landscape:0}
		!p.font=0
		popen,'~/Desktop/rayplot.ps'
	endelse

	if keyword_set(ray_struct) then begin
		xcoord = ray_struct.xcoord
		ycoord = ray_struct.ycoord
		zcoord = ray_struct.zcoord
		kvec = [[ray_struct.kx],[ray_struct.ky],[ray_struct.kz]]
		n_rays = 1.
	endif else begin
		xcoord = rayx
		ycoord = rayy
		zcoord = rayz
		n_rays = n_elements(rayx[0,*])
	endelse


	;-------------------------------------------------------
	;The IGRF model inputs and outputs in geographic coord.
	;Transform these here to SM coord
	;-------------------------------------------------------

	xcoordSM = xcoord
	ycoordSM = xcoord
	zcoordSM = xcoord
	xcoordSM[*] = 0.
	ycoordSM[*] = 0.
	zcoordSM[*] = 0.



	if KEYWORD_SET(geocoord) then begin

		for qq=0,n_rays-1 do begin

			tmin = time_double(geotime)
			tmax = tmin + 1.
			dt = tmax-tmin
			nelem = n_elements(xcoord[*,qq])
			times = dt*indgen(nelem)/(nelem-1) + tmin


			store_data,'geocoordtmp',times,[[xcoord[*,qq]],[ycoord[*,qq]],[zcoord[*,qq]]]
			cotrans,'geocoordtmp','geicoordtmp',/geo2gei
			cotrans,'geicoordtmp','gsecoordtmp',/gei2gse
			cotrans,'gsecoordtmp','gsmcoordtmp',/gse2gsm
			cotrans,'gsmcoordtmp','smcoordtmp',/gsm2sm

			get_data,'smcoordtmp',tt,vals
			xcoordSM[*,qq] = vals[*,0]
			ycoordSM[*,qq] = vals[*,1]
			zcoordSM[*,qq] = vals[*,2]

			;clean up...
			store_data,['geocoordtmp','geicoordtmp','gsecoordtmp','gsmcoordtmp','smcoordtmp'],/delete

		endfor

	endif


	if ~keyword_set(colors) then colors=replicate(raycolor,n_rays)
	if keyword_set(oplotX) and ~keyword_set(colorsX) then colorsX=replicate(raycolor,n_elements(oplotX[*,0]))
	if ~keyword_set(xrangeM) then xrangeM=[0.,6.]
	if ~keyword_set(zrangeM) then zrangeM=[-3.,3.]
	if ~keyword_set(xrangeE) then xrangeE=xrangeM
	if ~keyword_set(yrangeE) then yrangeE=[-3.,3.]



	;-----------------------

	!P.multi=[0,0,2]

	;DETERMINE WHICH K-VALUES TO OVERPLOT
	if keyword_set(kvecs) then begin

		;determine path distance traveled by ray
		b=0
		foo = 6370.*sqrt(xcoordSM[*,b]^2 + ycoordSM[*,b]^2 + zcoordSM[*,b]^2)
		pathseg = fltarr(n_elements(foo))
		pathD = pathseg
		for i=0,n_elements(foo)-2 do pathseg[i] = (abs(foo[i+1] - foo[i]))
		for i=1,n_elements(pathseg)-2 do pathd[i] = total(pathseg[0:i-1])

		n = 1.
		goodv = 0.
		test =''
		while test ne 'stop' do begin
			foo = where(pathD ge n*k_spacing)
			if foo[0] ne -1 then goodv = [goodv,foo[0]]
			if foo[0] eq -1 then test = 'stop'
			n=n+1
		endwhile

		kx2 = kvecs[goodv,0]
		ky2 = kvecs[goodv,1]
		kz2 = kvecs[goodv,2]
		xc2 = rayx[goodv]
		yc2 = rayy[goodv]
		zc2 = rayz[goodv]

		sizexM = (xrangeM[1] - xrangeM[0])/20.
		sizezM = (xrangeM[1] - zrangeM[0])/20.
		sizexE = (xrangeE[1] - xrangeE[0])/20.
		sizeyE = (xrangeE[1] - yrangeE[0])/20.

	endif


	;_________________Meridional Plane__________________


	if KEYWORD_SET(ray_vals) then begin

		cgPlot, xcoordSM, zcoordSM, /NoData,xrange=xrangeM,yrange=zrangeM,xstyle=1,ystyle=1,position=aspect(1)

		for qq=0,n_rays-1 do begin
			;get rid of NaN values
			goo = where(finite(rayx[*,qq]) ne 0)
			if goo[0] ne -1 then begin
				xcoordt = xcoordSM[goo,qq]
				ycoordt = ycoordSM[goo,qq]
				zcoordt = zcoordSM[goo,qq]
				longitt = longit[goo,qq]
				ray_valst = ray_vals[goo,qq]
				;deviation away from the initial (Meridional) longitude
				;...usually pretty insignificant for phi=0 or 180 deg rays
				longitt_relative = longitt[goo] - longitt[0]
			endif


			s = n_elements(xcoordt)
			colors = long(bytscl(ray_valst,min=minv,max=maxv))
			goober = where(colors eq 0)
			if goober[0] ne -1 then colors[goober] = 1

			meridx = sqrt(xcoordt^2 + ycoordt^2)

			for j=0,s-2 do cgPlotS,[meridx[j]*cos(longitt_relative[j]*!dtor), $
															meridx[j+1]*cos(longitt_relative[j+1]*!dtor)],$
														 [zcoordt[j], zcoordt[j+1]], Color=colors[j], Thick=2

		endfor

		;Plot colorbar
		loadct,39  ;need the first element to be black
		nticks = 7.
		tn = (indgen(nticks)/(nticks-1))*(maxv-minv) + minv
		tn = strtrim(string(tn,format='(f8.2)'),2)
		colorbar,POSITION=[0.15, 0.85, 0.95, 0.90],$
		divisions=nticks-1,ticknames=tn,charsize = 0.8,range=[minv,maxv],color=2

	endif else begin

		;plot the rays without any color fill value
		plot,[0,0],color=1,/nodata,xrange=xrangeM,yrange=zrangeM,ystyle=1,xstyle=1,title='Meridional Plane',xtitle='x (SM)',ytitle='z (SM)'
		for qq=0,n_rays-1 do begin

			goo = where(finite(rayx[*,qq]) ne 0)
			if goo[0] ne -1 then begin
				xcoordt = xcoordSM[goo,qq]
				ycoordt = ycoordSM[goo,qq]
				zcoordt = zcoordSM[goo,qq]
				longitt = longit[goo,qq]
				;deviation away from the initial (Meridional) longitude
				;...usually pretty insignificant for phi=0 or 180 deg rays
;				longitt_relative = longitt[*,qq] - longitt[0,qq]
				longitt_relative = longitt[goo] - longitt[0]
			endif

			meridx = sqrt(xcoordt^2 + ycoordt^2)
			oplot,meridx*cos(longitt_relative*!dtor),zcoordt,color=colors[qq]

		endfor
	endelse

	oplot_earth_mlat_L_lines



	;_________________Equatorial Plane__________________

	plot,[0,0],/nodata,xrange=xrangeE,yrange=yrangeE,ystyle=1,xstyle=1,title='Equatorial Plane',xtitle='x (SM)',ytitle='y (SM)'

	for qq=0,n_rays-1 do oplot,xcoordSM[*,qq],ycoordSM[*,qq],color=colors[qq]
	oplot_earth_mlat_L_lines,/eq_plane


	if keyword_set(psonly) then pclose

	;	if keyword_set(psonly) then begin
	;		device,/close
	;		set_plot,'x'
	;	endif

end
