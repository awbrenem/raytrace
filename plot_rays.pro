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
;
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

pro plot_rays,rayx,rayy,rayz,longit,ray_vals=ray_vals,xrangeM=xrangeM,zrangeM=zrangeM,xrangeE=xrangeE,$
	yrangeE=yrangeE,ray_struct=ray_struct,colors=colors,colorsX=colorsX,oplotX=oplotX,kvecs=kvecs,Lsc=Lsc,$
	psonly=psonly,k_spacing=k_spacing,minval=minv,maxval=maxv


	;rbsp_efw_init

	if ~KEYWORD_SET(minv) then minv=10.
	if ~KEYWORD_SET(maxv) then maxv=500.
	if ~KEYWORD_SET(alpha) then alpha = 0.
	if ~keyword_set(k_spacing) then k_spacing = 300.
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


	if ~keyword_set(colors) then colors=replicate(254.,n_rays)

	if keyword_set(oplotX) and ~keyword_set(colorsX) then colorsX=replicate(254.,n_elements(oplotX[*,0]))

	if ~keyword_set(xrangeM) then xrangeM=[0.,6.]
	if ~keyword_set(zrangeM) then zrangeM=[-3.,3.]
	if ~keyword_set(xrangeE) then xrangeE=xrangeM
	if ~keyword_set(yrangeE) then yrangeE=[-3.,3.]
	;-----------------------

	earthx = COS((2*!PI/99.0)*FINDGEN(100))
	earthy = SIN((2*!PI/99.0)*FINDGEN(100))

	;latitude lines
	lats = [0,10,20,30,40,50,60,70,80]
	lats = [-1*reverse(lats[1:n_elements(lats)-1]),lats]

	!P.multi=[0,0,2]

	L2 = dipole(2.)
	L4 = dipole(4.)
	L5 = dipole(5.)
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


	circ = 4*cos(2*!pi*indgen(n_elements(L2.R))/(n_elements(L2.R)-1))*(360.-0.)


	;DETERMINE WHICH K-VALUES TO OVERPLOT
	if keyword_set(kvecs) then begin

		;determine path distance traveled by ray
		b=0
		foo = 6370.*sqrt(xcoord[*,b]^2 + ycoord[*,b]^2 + zcoord[*,b]^2)
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

;		if minv lt 1 then minv = 1. ;can't have values < 1 due to log scale


		cgPlot, rayx, rayz, /NoData,xrange=xrangeM,yrange=zrangeM,xstyle=1,ystyle=1,position=aspect(1)

		oplot,earthx,earthy,color=60
		;oplot,replicate(1.078,360.),indgen(360.)*!dtor,/polar,color=80

		oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80
		oplot,L2.R/6370.*cos(L2.lat*!dtor),L2.R/6370.*sin(L2.lat*!dtor),color=120
		oplot,L2.R/6370.*cos(L2.lat*!dtor),-1*L2.R/6370.*sin(L2.lat*!dtor),color=120

		oplot,L4.R/6370.*cos(L4.lat*!dtor),L4.R/6370.*sin(L4.lat*!dtor),color=120
		oplot,L4.R/6370.*cos(L4.lat*!dtor),-1*L4.R/6370.*sin(L4.lat*!dtor),color=120

		oplot,L5.R/6370.*cos(L5.lat*!dtor),L5.R/6370.*sin(L5.lat*!dtor),color=120
		oplot,L5.R/6370.*cos(L5.lat*!dtor),-1*L5.R/6370.*sin(L5.lat*!dtor),color=120

		oplot,L6.R/6370.*cos(L6.lat*!dtor),L6.R/6370.*sin(L6.lat*!dtor),color=120
		oplot,L6.R/6370.*cos(L6.lat*!dtor),-1*L6.R/6370.*sin(L6.lat*!dtor),color=120

		oplot,L8.R/6370.*cos(L8.lat*!dtor),L8.R/6370.*sin(L8.lat*!dtor),color=120
		oplot,L8.R/6370.*cos(L8.lat*!dtor),-1*L8.R/6370.*sin(L8.lat*!dtor),color=120




		for qq=0,n_rays-1 do begin

			;get rid of NaN values
			goo = where(finite(rayx[*,qq]) ne 0)
			if goo[0] ne -1 then begin
				rayxt = rayx[goo,qq]
				rayzt = rayz[goo,qq]
				ray_valst = ray_vals[goo,qq]
			endif



			s = n_elements(rayxt)
			colors = long(bytscl(ray_valst,min=minv,max=maxv))
			goober = where(colors eq 0)
			if goober[0] ne -1 then colors[goober] = 1

PRINT,'***NEED TO MODIFY THIS FOR LONGIT...I.E. PLACE THE PLOT IN MERIDIONAL PLANE. '
PRINT,'***ONLY WORKS SO FAR WHEN LONGIT = 0. BUT, THE RAY TRACING MODEL HAS LONGITUDINAL DEPENDENCIES'
stop
			for j=0,s-2 do cgPlotS, [rayxt[j], rayxt[j+1]], [rayzt[j], rayzt[j+1]], Color=colors[j], Thick=2
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
				xcoordt = xcoord[goo,qq]
				zcoordt = zcoord[goo,qq]
			endif

			oplot,xcoordt/cos(longit*!dtor),zcoordt,color=colors[qq]
		endfor
	endelse




	if keyword_set(Lsc) then oplot,Lst.R/6370.*cos(Lst.lat*!dtor),Lst.R/6370.*sin(Lst.lat*!dtor),color=160 & oplot,Lst.R/6370.*cos(Lst.lat*!dtor),-1*Lst.R/6370.*sin(Lst.lat*!dtor),color=160

	for i=0,n_elements(lats)-1 do oplot,[1,50]*cos([lats[i]*!dtor,lats[i]*!dtor]),[1,50]*sin([lats[i]*!dtor,lats[i]*!dtor]),linestyle=3,color=100

	if keyword_set(oplotX) then begin
		for i=0,n_elements(oplotX[*,0])-1 do oplot,[oplotX[i,0]],[oplotX[i,2]],psym=7,color=colorsX[i]
	endif
	if keyword_set(kvecs) then begin
		for bb=0,n_elements(kx2)-1 do oplot,[xc2[bb],xc2[bb]+kx2[bb]*sizexM],[zc2[bb],zc2[bb]+kz2[bb]*sizezM];,color=100
	endif



	;_________________Equatorial Plane__________________

	skip = 'no'

	if skip ne 'yes' then begin

		plot,[0,0],/nodata,xrange=xrangeE,yrange=yrangeE,ystyle=1,xstyle=1,title='Equatorial Plane',xtitle='x (SM)',ytitle='y (SM)'

		for qq=0,n_rays-1 do oplot,xcoord[*,qq],ycoord[*,qq],color=colors[qq]
		oplot,earthx,earthy,color=60

		oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80
		oplot,replicate(2,360.)*cos(indgen(360.)*!dtor),replicate(2,360.)*sin(indgen(360.)*!dtor),color=120
		oplot,replicate(4,360.)*cos(indgen(360.)*!dtor),replicate(4,360.)*sin(indgen(360.)*!dtor),color=120
		oplot,replicate(6,360.)*cos(indgen(360.)*!dtor),replicate(6,360.)*sin(indgen(360.)*!dtor),color=120
		oplot,replicate(8,360.)*cos(indgen(360.)*!dtor),replicate(8,360.)*sin(indgen(360.)*!dtor),color=120


		;if keyword_set(Lsc) then oplot,replicate(Lst.R[0]/6370.,360.),indgen(360.)*!dtor,/polar,color=120
		if keyword_set(oplotX) then begin
			for i=0,n_elements(oplotX[*,0])-1 do oplot,[oplotX[i,0]],[oplotX[i,1]],psym=7,color=colorsX[i]
		endif
		if keyword_set(kvecs) then begin
			for bb=0,n_elements(kx2)-1 do oplot,[xc2[bb],xc2[bb]+kx2[bb]*sizexE],[yc2[bb],yc2[bb]+ky2[bb]*sizeyE];,color=100
		endif


	endif


	if keyword_set(psonly) then pclose

	;	if keyword_set(psonly) then begin
	;		device,/close
	;		set_plot,'x'
	;	endif



end
