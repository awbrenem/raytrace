;Test how much better having 10 AFIRE (FIREBIRD) channels in IMPAX will have vs 
;the 5 channels on FIREBIRD. 
;Microburst with dispersion observed on 2016-08-30/20:47 - 20:48 
;During this time RBSPa is in the same region and sees strong lower band chorus activity at ~ 1 kHz. 
;***CHECK TO SEE HOW CLOSE CONJUNCTION IS AND WHETHER THERE IS BURST DATA AVAILABLE


;Approach 
;1) Simulate a 1 kHz chorus source with a variety of wave normal angles. 
;2) Record all the locations along the Lshell of FIREBIRD where 220 keV electrons are scattered, 
;as well as their travel time from source to location and the time it would take that scattered 220 keV
;electron to arrive at FIREBIRD.  
;3) Do the same for 721 keV electrons. 
;4) From all of these combined locations (220, 721 in both hemispheres) calculate the time difference 
;of arrival at FIREBIRD.
;5) compare these time differences to the max and min slopes calculated for this uB event 
;based on energy uncertainty (see )
;6) Repeat the above for different source locations or frequencies


;FU3 location at 2016-08-30/20:47 

geolat = -51.026791
geolon = 84.924263
alt = 504.91843
mlt = 2.1601484
L = 6.0482759



;RBSPa Local observations at 01:45:30 UT:
	;mlat = 11.9 (SM)
	;mlong = 66 (SM)
	;L = 3.92
	;RE = 3.75
	;smcoord = [1.48,3.37,0.78]
	;density = 65 cm-3
	;Bo = 564 nT
	;fce = 15792 Hz
	;fce/2 = 7896 Hz
	;fce/10 = 1579 Hz
	;f = 6500 Hz
	;wave normal angle roughly 40-57 deg


;Arase local observations at 01:45:30 UT
	;mlat = 20.81 (SM)
	;L = 3.75
	;RE = 3.28
	;smcoord = [1.02,2.88,1.16]
	;density =
	;Bo =
	;fce =
	;fce/2 =
	;fce/10 =
	;f = 6500 Hz
	;wave normal angle roughly


;------------------------------------------------
;FIRST VERSION IS FOR REGULAR CYCLOTRON resonance
;------------------------------------------------


rba_l = 3.92
rba_re = 3.75

;--------------------------------------------
;Set up model for observed density, Bo.
;altv=(6370.*rbspa_l)-6370.
;create_rays_general,1800.,theta=0,alt=altv,lat=geolatlong.geolat,long=geolatlong.geolong,title='uB3',geotime='2016-01-20/19:44'
;dens_bo_profile,1800.,dens_sc=8,bo_sc=146,L_sc=chorus_leq;,/ps
;--------------------------------------------



freqv = 6500.
ti = read_write_trace_in(freq=freqv,$
	bmodel = 0,$
	mmult = 1,$
	lat=11.9,$
	longit=-117,$ ;66.,$
	theta=0.,$
	phi=0.,$
	alt=(6370.*rba_re)-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=60,$
	pplcp=3.,$
	pplhw=0.5,$
	drl=55.)


	;Test out the density model
	create_rays_thetakb_spread,0.,freqs=freqv,title='uB3',geotime=time
	rba_l2 = rba_l
	dens_bo_profile,freqv,dens_sc=65,bo_sc=564,L_sc=rba_l2,/ps


;	thetavals = 180+[-10.,-20,-30,-40.,-50.,-55,-60]
;thetavals = [10.,20,30,40.,50.,60.,70.]

	;Only + values reasonably reach Arase
;	thetavals = [40,50,60.]
;	thetavals = -1*[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
	thetavals = [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
	time = '2017-08-21/01:45'

	freqs = replicate(freqv,n_elements(thetavals))
	create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime=time

	x = read_trace_ta(geotime=time)

;restore multiple rays
	restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

;SM coord of Arase and RBSPa
	oplotcoord = transpose([[1.48,3.37,0.78],[1.02,2.88,1.16]])

	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
		xrangeM=[2,4],zrangeM=[0,2],$
		oplotX=oplotcoord


	;Plot the wave normal angle along the trajectory
	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=thk,$
		xrangeM=[2,4],zrangeM=[0,2],$
		oplotX=oplotcoord,minval=0,maxval=90

	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk,minv=0.,maxv=90.,Lsc=[rba_l],$
		limits=[0,-3,6,3],nlvls=100,xgrid=xg,ygrid=yg,result=tkb_gridv,xrange=[2,4],yrange=[0,2],$
		oplotX=oplotcoord;,/psonly

	;Extract slice along single L-value
	lval_extract = 3.75  ;Arase Lshell
	;lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)

	mlatrange = indgen(90.)
	thk_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,tkb_gridv,gridpts=pts)

	for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
	;I find the wave normal angle at Arase is ~72 deg



	;-----------------------------------------------------
	;Determine initial wave normal angle
	;Get ray initial theta_kb values
	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk0,minv=1.,maxv=60.,Lsc=[chorus_leq,FB_leq],$
	limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
	for i=0,89 do thk0_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
	print,'theta_kb initial value along slice = ',thk0_finL[*,bbq]



	;-----------------------------------------------------
	;Determine travel time (group velocity)

	;Plot the wave normal angle along the trajectory
	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=timeG,$
		xrangeM=[2,4],zrangeM=[0,2],$
		oplotX=oplotcoord,minval=0,maxval=0.5

	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,timeG,minv=0.,maxv=0.5,Lsc=[rba_l],$
		limits=[0,-3,6,3],nlvls=100,xgrid=xg,ygrid=yg,result=timeG_gridv,xrange=[2,4],yrange=[0,2],$
		oplotX=oplotcoord,/psonly

	;Extract slice along single L-value
	lval_extract = 3.75  ;Arase Lshell
	;lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)

	mlatrange = indgen(90.)
	timeG_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,timeG_gridv,gridpts=pts)

	for ii=0,89 do print,mlatrange[ii],timeG_slice[ii]




	;-----------------------------------------------------
	;Determine cyclotron resonance energy along ray path

	fce = freqv/f_fce
	kvec = 2*!pi/wavelength   ;1/km
	pa = replicate(5.,n_elements(thetavals))
;	function cycl_energies,freq,theta_kb,pitchangle,fce,kmag,nres
	freqtmp = fltarr(10000.,n_elements(thetavals))
	freqtmp[*,0] = freqs[0]
	freqtmp[*,1] = freqs[1]
	evals = cycl_energies(freqtmp,thk,pa,fce,kvec,dens,1)
;	functioncycl_energies(freq, thk,pa,fce,kmag,dens,nres


	eplot = evals.e_cycl_counterstream
	vz = evals.vz_cycl_counterstream
	;Get cycl energies along ray track
	;get e- precip time vs track location
	;combine with


	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=eplot,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[rba_l],minval=5,maxval=850

	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,eplot,minv=5.,maxv=850.,Lsc=[rba_l],$
		limits=[0,-3,6,3],nlvls=50,oplotX=oplotcoord





;------------------------------------------------------------------------------
;Reverse trace from Arase to equator. Then, use this equatorial point as a
;source location to launch many rays.

;Ray at ARASE will have
;theta_kb~72 deg.
;



;Set up for southward raytracing
freqv = 6500.
arase_re = 3.28
ti = read_write_trace_in(freq=freqv,$
	bmodel = 0,$
	mmult = 1,$
	lat=20.81,$
	longit=66.,$
	theta=0.,$
	phi=0.,$
	alt=(6370.*arase_re)-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=0.,$
	pplcp=3.,$
	pplhw=0.5,$
	drl=55.)



;	thetavals = 180.+[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
	thetavals = 180.+73.
	time = '2017-08-21/01:45'

	freqs = replicate(freqv,n_elements(thetavals))
	create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime=time

;	rba_l2 = rba_l
;	dens_bo_profile,freqv,dens_sc=65,bo_sc=564,L_sc=rba_l2;,/ps
	x = read_trace_ta(geotime=time)

;restore multiple rays
	restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

;SM coord of Arase and RBSPa
	oplotcoord = transpose([[1.48,3.37,0.78],[1.02,2.88,1.16]])

	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
		xrangeM=[2,4],zrangeM=[0,2],$
		oplotX=oplotcoord


	;Plot the wave normal angle along the trajectory
	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=thk,$
		xrangeM=[2,4],zrangeM=[0,2],$
		oplotX=oplotcoord,minval=0,maxval=90

	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk,minv=0.,maxv=90.,Lsc=[rba_l],$
		limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tkb_gridv,xrange=[2,4],yrange=[0,2],$
		oplotX=oplotcoord

	;Extract slice along single L-value
	lval_extract = 3.75  ;Arase Lshell
	;lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)

	mlatrange = indgen(90.)
	thk_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,tkb_gridv,gridpts=pts)

	for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
	;I find the wave normal angle at Arase is ~72 deg


	;Equatorial location that produces rays that go through RBSP and ARASE is
	;thk = 176.9  --> equivalent to -3.1 deg
	;SM = [1.62385,3.64722,0.]


	;-----------------------------------------------------------------------------
	;Finally, use the equatorial source to produce rays that go through both RBSP
	;and Arase


	freqv = 6500.
	eq_re = 3.9923
	ti = read_write_trace_in(freq=freqv,$
		bmodel = 0,$
		mmult = 1,$
		lat=0.,$
		longit=66.,$
		theta=0.,$
		phi=0.,$
		alt=(6370.*eq_re)-6370.,$
		final_alt=4000.,$
		model=0,$
		final_lat=25.,$
		pplcp=3.,$
		pplhw=0.5,$
		drl=55.)



			thetavals = -1*indgen(20)*0.5 ;[0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
			;thetavals = -3.1    ;equivalent to 180 + 176.9
			time = '2017-08-21/01:45'

			freqs = replicate(freqv,n_elements(thetavals))
			create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime=time

		;	rba_l2 = rba_l
		;	dens_bo_profile,freqv,dens_sc=65,bo_sc=564,L_sc=rba_l2;,/ps
			x = read_trace_ta(geotime=time)

		;restore multiple rays
			restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

		;SM coord of Arase and RBSPa
			oplotcoord = transpose([[1.48,3.37,0.78],[1.02,2.88,1.16]])

			plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
				xrangeM=[2,4],zrangeM=[0,2],$
				oplotX=oplotcoord


			;Plot the wave normal angle along the trajectory
			plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=thk,$
				xrangeM=[2,4],zrangeM=[0,2],$
				oplotX=oplotcoord,minval=0,maxval=90

			triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk,minv=0.,maxv=90.,Lsc=[rba_l],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tkb_gridv,xrange=[2,4],yrange=[0,2],$
				oplotX=oplotcoord,/psonly

			;Extract slice along single L-value
			lval_extract = 3.75  ;Arase Lshell
			mlatrange = indgen(90.)
			thk_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,tkb_gridv,gridpts=pts)
			for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
			;I find the wave normal angle at Arase is ~72.4 deg

			lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)
			thk_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,tkb_gridv,gridpts=pts)
			for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
			;RBSPa should see theta_kb = 57.4 deg.



			;-----------------------------------------------------
			;Determine travel time (group velocity)

			plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=timeG,$
				xrangeM=[2,4],zrangeM=[0,2],$
				oplotX=oplotcoord,minval=0,maxval=0.2

			triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,timeG,minv=0.,maxv=90.,Lsc=[rba_l],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=timeG_gridv,xrange=[2,4],yrange=[0,2],$
				oplotX=oplotcoord

			;Extract slice along single L-value
			mlatrange = indgen(90.)

			lval_extract = 3.75  ;Arase Lshell
			timeG_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,timeG_gridv,gridpts=pts)
			for ii=0,89 do print,mlatrange[ii],timeG_slice[ii]
			;Group travel time to ARASE from equator is 0.348127 sec

			lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)
			timeG_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,timeG_gridv,gridpts=pts)
			for ii=0,89 do print,mlatrange[ii],timeG_slice[ii]
			;Group travel time to RBSP from equator is 0.22705 sec


			;Means travel time from RBSP to ARASE is ~0.121 sec
