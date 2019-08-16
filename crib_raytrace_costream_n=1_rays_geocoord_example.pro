;This crib sheet demonstrates how to use many of the raytrace routines.
;...setting up ray tracing
;...getting input geographic coord from transform_sm_to_geographic_latlong_for_igrf_raytrace_input
;......GEO coord used for IGRF model. For SM dipole model, just input dipole coord.
;...test the density/Bo model with dens_bo_profile
;...create a series of rays from a single point spanning range of theta_kb (create_rays_general)
;...plot those rays with plot_rays
;...triangulate them to a regular grid with triangulate_rays
;......also have this routine return ray values (like cyclotron res energy along ray path)
;...extract ray values along a single L slice with extract_lshell_mlat_slice
;...calculate the total delta-t b/t chorus emission and arrival of scattered
;......electrons at a certain location with distance_to_atmosphere
;...Extract all sorts of useful ray quantities and plot them along the slice

;Results are saved in IDL save file. To plot them see crib_plot_raytrace_results_costream.pro


;------------------------------------------------
;CO-STREAMING CYCLOTRON (n=1) RESONANCE
;------------------------------------------------

FB_leq = 5.5         ;Position of FIREBIRD
chorus_leq = 3.92   ;value if you adjust L to make |B|-|Bmodel|=0
mlatSM = 11.9
mlongSM = 66.

;Only used for setting up model Bo magnitude
freqvt = 6500.
t0 = '2017-08-21/01:45'

geolatlong = transform_sm_to_geographic_latlong_for_igrf_raytrace_input($
	[chorus_leq,mlatSM,mlongSM],t0,/smlatlong)

;Determine geographic values for final lat
smlat_fin = 20.81
geolatlong_fin = transform_sm_to_geographic_latlong_for_igrf_raytrace_input($
	[geolatlong.radius,smlat_fin,geolatlong.smlong],t0,/smlatlong)



;Initial raytrace setup
ti = read_write_trace_in(freq=freqvt,$
bmodel = 1,$
mmult = 0.8,$
lat=geolatlong.geolat,$
theta=0.,$
phi=0.,$
longit=geolatlong.geolong,$
alt=6370.*geolatlong.radius-6370.,$
final_alt=4000.,$
model=0,$
final_lat=geolatlong_fin.geolat,$
pplcp=3.,$
pplhw=0.5,$
drl=10.)

;--------------------------------------------
;Set up model for observed density, Bo.
altv=(6370.*chorus_leq)-6370.
create_rays_general,freqvt,theta=0,alt=altv,lat=geolatlong.geolat,long=geolatlong.geolong,title='uB3',geotime='2017-08-21/01:45'
dens_bo_profile,freqvt,dens_sc=65,bo_sc=564,L_sc=chorus_leq,/ps
;--------------------------------------------



freqsALL = float([1000,1100,1200,1300,1400,1500,1600,1700,1800])
;freqsALL = float([1800])
nrayss = 80.  ;number of rays for each point source
nlvals = 40.  ;number of point sources
allLvals = (7-3.4)*indgen(nlvals)/(nlvals-1)+3.4
;allLvals = 5.5
lval_extract = 5.
thetav = reverse((-45+0)*indgen(nrayss)/(nrayss-1))-0
;thetav = ((45-10)*indgen(nrayss)/(nrayss-1))+10
latGEO_input = replicate(geolatlong.geolat,nrayss)
longGEO_input = replicate(geolatlong.geolong,nrayss)


filename = 'costream_'+string(freqsALL,format='(I4)')+'Hz_L=3.4-7_nrays='+string(nrayss,format='(I2)')+'.idl'


for qqq=0,n_elements(freqsALL)-1 do begin

	freqvt = freqsALL[qqq]
	freqv = replicate(freqvt,nrayss)

	;Subset of values with the correct resonant energy
	mlatrange = indgen(90.)

	;Value along L=5 cut at each mlat for each ray
	energiesL = replicate(!values.f_nan,90.,n_elements(allLvals))
	totaltimeL = energiesL
	lval0_finL = energiesL  ;0 denotes initial value
	thk_finL =  energiesL
	thk0_finL = energiesL
	lval_finL = energiesL
	mlat0_finL = energiesL
	;Same as above but only where the resonance energies falls b/t 220-985 keV
	energiesL2 = energiesL
	totaltimeL2 = energiesL
	lval0_finL2 = energiesL
	thk_finL2 =  energiesL
	thk0_finL2 = energiesL



	for bbq=0,n_elements(allLvals)-1 do begin

		chorus_leqv = replicate(allLvals[bbq],nrayss)
		altv=(6370.*chorus_leqv)-6370.

		create_rays_general,freqv,theta=thetav,alt=altv,lat=latGEO_input,long=longGEO_input,title='uB3',geotime='2016-01-20/19:44';,/geo2sm

		;restore multiple rays
		restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

		xcoordsm /= 6370.
		ycoordsm /= 6370.
		zcoordsm /= 6370.

		freqs = make_array(n_elements(thk[*,0]),n_elements(thk[0,*]),value=0)
		for i=0,n_elements(freqv)-1 do freqs[*,i] = freqv[i]


		fce = freqs/f_fce
		kvec = 2*!pi/wavelength   ;1/km
		pa = replicate(5.,n_elements(thk[*,0]),n_elements(freqv))
		evals = cycl_energies(freqs,thk,pa,fce,kvec,dens,1)
		eplot = evals.e_cycl_costream
		vz = evals.vz_cycl_costream

		;plot resonance energy
		plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=eplot,$
		xrangeM=[2,8],zrangeM=[-3,3],minv=220,maxv=985.;,/geocoord,geotime='2016-01-20/19:44'




		;fill in holes by triangulating
		triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,eplot,minv=220,maxv=985.,Lsc=[chorus_leq,FB_leq],$
		limits=[2,-3,8,3],nlvls=50,xgrid=xg,ygrid=yg,result=eres,mlats=mlats,$
		lvals=lvals,rads=rads,ilats=ilats;,/zbuffer;,/psonly

		;Extract slice along single L-value
		energiesL[*,bbq] = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,eres,gridpts=pts)



		;;Extract initial theta_kb values for ray slice
		;	triangulate_rays,xcoord,zcoord,longit,thk0,minv=1.,maxv=60.,Lsc=[chorus_leq,FB_leq],$
		;	limits=[2,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/geocoord,geotime='2016-01-20/19:44',;,/zbuffer
		;	ptsx = pts[*,0]
		;	ptsz = pts[*,1]
		;for i=0,89 do thk0_fin[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
		;print,thk0_fin[*,bbq]


		alt = 500.;rough altitude of FB4 (632 by 433 km orbit)
		dist_remaining = distance_to_atmosphere(lval,latSM,offset_alt=alt)


		;calculate time for scattered e- to arrive at FB
		tarr = 6370.*dist_remaining/vz
		tarr = 1000.*tarr ;msec for better plotting
		;	plot_rays,xcoord,ycoord,zcoord,ray_vals=tarr,$
		;		xrangeM=[0,6],zrangeM=[-3,3],$
		;		Lsc=[chorus_leq,FB_leq],minval=20,maxval=400,/geocoord,geotime='2016-01-20/19:44'

		;	triangulate_rays,xcoord,zcoord,longit,tarr,minv=20.,maxv=400.,Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],$
		;		limits=[0,-3,6,3],nlvls=50;,xgrid=xg,ygrid=yg,result=result1,/geocoord,geotime='2016-01-20/19:44'



		;Now add together the time it takes for chorus wave to propagate
		;to a particular point with time it would take electron scattered at
		;that point to reach FIREBIRD

		ttotal = tarr + timeg*1000.
		plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=ttotal,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[chorus_leq,FB_leq],minval=200,maxval=1000;,/geocoord,geotime='2016-01-20/19:44'


		triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ttotal,minv=220.,maxv=985.,Lsc=[chorus_leq,FB_leq],$
		limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=totaltime;,/zbuffer

		;Get values of grid points that correspond to a particular L
		totaltimeL[*,bbq] = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,totaltime,gridpts=pts)
		;Test to be sure that the Lvalues of extracted grid points is lval_extract
		;NOTE THAT THESE CORRESPOND TO DIPOLE COORD, WHEREAS THE MODEL'S RETURNING IGRF.
		ptsx = pts[*,0]
		ptsz = pts[*,1]
		print,xg[ptsx]
		print,yg[ptsz]
		radtst = sqrt(xg[ptsx]^2 + yg[ptsz]^2)
		mlattst = 90. - acos(yg[ptsz]/radtst)/!dtor
		ltest = radtst/cos(mlattst*!dtor)^2

		;LET'S GET THE DIPOLE L VALUES
		Ldip = radius/(cos(latsm*!dtor)^2)


		;Test to make sure that we have some rays on L=lval_extract
		tdifftest = max(totaltimeL[*,bbq],/nan) - min(totaltimeL[*,bbq],/nan)



		;Now let's extract a bunch of ray values. Some are for testing to see if
		;the software's working, others are more useful.
		;the values to extract are given by ptsx and ptsz

		if ~(finite(tdifftest) eq 0) then begin
			if ~(tdifftest eq 0.) then begin

				;Get ray L-values
				triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,Ldip,minv=1.,maxv=10.,Lsc=[chorus_leq,FB_leq],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
				;Extract L-values at the L slice. Note that these had better be equal
				;to the value of the L slice lval_extract
				for i=0,89 do lval_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
				print,'L value along slice = ',lval_finL[*,bbq]

				;Get ray initial L-values
				triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,lval0,minv=1.,maxv=10.,Lsc=[chorus_leq,FB_leq],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
				for i=0,89 do lval0_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
				print,'L value initial along slice = ',lval0_finL[*,bbq]

				;Get ray inital mlat values
				triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,latSM0,minv=0.,maxv=10.,Lsc=[chorus_leq,FB_leq],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
				for i=0,89 do mlat0_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
				print,'mlat initial value along slice = ',mlat0_finL[*,bbq]

				;Get ray initial theta_kb values
				triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk0,minv=1.,maxv=60.,Lsc=[chorus_leq,FB_leq],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
				for i=0,89 do thk0_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
				print,'theta_kb initial value along slice = ',thk0_finL[*,bbq]

				;Get in situ theta_kb values
				triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk,minv=1.,maxv=60.,Lsc=[chorus_leq,FB_leq],$
				limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,/zbuffer
				for i=0,89 do thk_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
				print,'theta_kb value along slice = ',thk_finL[*,bbq]



				;Subset of values with the correct resonant energy
				energiesL2[*,bbq] = energiesL[*,bbq]
				totaltimeL2[*,bbq] = totaltimeL[*,bbq]
				lval0_finL2[*,bbq] = lval0_finL[*,bbq]
				thk_finL2[*,bbq] = thk_finL[*,bbq]
				thk0_finL2[*,bbq] = thk0_finL[*,bbq]


				;;The L2 variables contain only the parts of the ray path that COUNTER-STREAMING
				;;the correct energies (220-985 keV) and correct delta-time (<=50 msec)

				;Remove energies outside of range
				bade = where((energiesL[*,bbq] lt 220.) or (energiesL[*,bbq] gt 985.))
				;bade = where((energiesL[*,bbq] lt 200.) or (energiesL[*,bbq] gt 900.))
				if bade[0] ne -1 then begin
					energiesL2[bade,bbq] = !values.f_nan
					totaltimeL2[bade,bbq] = !values.f_nan
					lval0_finL2[bade,bbq] = !values.f_nan
					thk_finL2[bade,bbq] = !values.f_nan
					thk0_finL2[bade,bbq] = !values.f_nan
				endif


				L0diff = max(lval0_finL2[*,bbq],/nan) - min(lval0_finL2[*,bbq],/nan)
				;Calculate max time diff b/t arrival of 985 keV e- and 220 keV e-
				tdiff = max(totaltimeL2[*,bbq],/nan) - min(totaltimeL2[*,bbq],/nan)
				print,'TIME OF ARRIVAL DIFFERENCE = '+strtrim(tdiff,2)+ ' (MSEC)'




				!p.multi = [0,2,3]
				rbsp_efw_init
				plot,mlatrange,energiesL[*,bbq],yrange=[0,1000],xrange=[0,50],$
				xtitle='mlat',ytitle='costream energy (keV)',$
				title='ray energy vs mlat for L='+strtrim(lval_extract,2)
				oplot,mlatrange,energiesL2[*,bbq],color=250,thick=3
				boomin = min(energiesL2[*,bbq],mintmp,/nan)
				boomax = max(energiesL2[*,bbq],maxtmp,/nan)
				oplot,[mlatrange[mintmp],mlatrange[mintmp]],[0,boomin],linestyle=2
				oplot,[mlatrange[maxtmp],mlatrange[maxtmp]],[0,boomax],linestyle=2
				mlatstr = 'mlat = '+strtrim(mlatrange[mintmp],2) + '-' + strtrim(mlatrange[maxtmp],2)+' deg'
				xyouts,0.2,0.8,mlatstr,/normal

				plot,mlatrange,totaltimeL[*,bbq],yrange=[0,1000],xrange=[0,50],$
				xtitle='mlat',ytitle='Precip time at FB (msec)',$
				title='Precip time after chorus onset!Cvs mlat for L='+strtrim(lval_extract,2)
				oplot,mlatrange,totaltimeL2[*,bbq],color=250,thick=3
				boomin = min(totaltimeL2[*,bbq],mintmp,/nan)
				boomax = max(totaltimeL2[*,bbq],maxtmp,/nan)
				oplot,[mlatrange[mintmp],mlatrange[mintmp]],[0,boomin],linestyle=2
				oplot,[mlatrange[maxtmp],mlatrange[maxtmp]],[0,boomax],linestyle=2
				tstr = string(tdiff,format='(f4.0)')+' msec'
				xyouts,0.7,0.8,'Delta time = '+tstr,/normal

				plot,mlatrange,lval0_finL[*,bbq],yrange=[2,8],xrange=[0,50],$
				xtitle='mlat',ytitle='Initial Lshell of ray',$
				title='ray L0 vs mlat for L='+strtrim(lval_extract,2)
				oplot,mlatrange,lval0_finL2[*,bbq],color=250,thick=3
				boomin = min(lval0_finL2[*,bbq],mintmp,/nan)
				boomax = max(lval0_finL2[*,bbq],maxtmp,/nan)
				oplot,[mlatrange[mintmp],mlatrange[mintmp]],[0,boomin],linestyle=2
				oplot,[mlatrange[maxtmp],mlatrange[maxtmp]],[0,boomax],linestyle=2
				lstr = string(min(lval0_finL2[*,bbq],/nan),format='(f4.1)')+'-'+$
				string(max(lval0_finL2[*,bbq],/nan),format='(f4.1)')
				xyouts,0.2,0.42,'L0 range='+lstr,/normal

				plot,mlatrange,thk_finL[*,bbq],yrange=[0,70],xrange=[0,50],$
				xtitle='mlat',ytitle='theta_kb of ray',$
				title='ray theta_kb vs mlat for L='+strtrim(lval_extract,2)
				oplot,mlatrange,thk_finL2[*,bbq],color=250,thick=3
				boomin = min(thk_finL2[*,bbq],mintmp,/nan)
				boomax = max(thk_finL2[*,bbq],maxtmp,/nan)
				oplot,[mlatrange[mintmp],mlatrange[mintmp]],[0,boomin],linestyle=2
				oplot,[mlatrange[maxtmp],mlatrange[maxtmp]],[0,boomax],linestyle=2

				plot,mlatrange,thk0_finL[*,bbq],yrange=[0,70],xrange=[0,50],$
				xtitle='mlat',ytitle='Inital theta_kb of ray',$
				title='ray initial theta_kb vs mlat for L='+strtrim(lval_extract,2)
				oplot,mlatrange,thk0_finL2[*,bbq],color=250,thick=3
				boomin = min(thk0_finL2[*,bbq],mintmp,/nan)
				boomax = max(thk0_finL2[*,bbq],maxtmp,/nan)
				oplot,[mlatrange[mintmp],mlatrange[mintmp]],[0,boomin],linestyle=2
				oplot,[mlatrange[maxtmp],mlatrange[maxtmp]],[0,boomax],linestyle=2
				t0str = string(min(thk0_finL2[*,bbq],/nan),format='(f4.0)')+'-'+$
				string(max(thk0_finL2[*,bbq],/nan),format='(f4.0)')
				xyouts,0.2,0.22,'theta_kb0 range='+t0str+' deg',/normal

			endif  ;tdiff ne 0
		endif else print,'****NO SOLUTION FOUND****' ;tdiff ne NaN


	endfor  ;all the rays in a single freq





	save,mlatrange,lval_extract,tdiff,$
	totaltimeL,energiesL,lval_finL,$
	lval0_finL,thk_finL,thk0_finL,$
	allLvals,$
	filename='~/Desktop/Research/RBSP_Firebird_microburst_conjunction_jan20/raytrace_files/'+filename[qqq]



endfor  ;for each freq

;xyouts,0.2,0.13,'L0 range = '+strtrim(L0diff,2)+' RE',/normal



;;*************************************
;;Test that I've calculated lshell, mlat, radial distance correctly
;;*************************************

;minv=3
;maxv=max(lvals)
;contour,lvals,xg,yg,nlevels=10,min_value=minv,max_value=maxv,$
;/cell_fill,xrange=[0,6],yrange=[-3,3],xstyle=1,ystyle=1,$
;background=255,position=aspect(1),color=2

;triangulate_rays,xcoord,zcoord,longit,lval0,minv=1.,maxv=10.,Lsc=[chorus_leq,FB_leq],$
;limits=[0,-3,6,3],nlvls=50,xgrid=xg,ygrid=yg,result=eres,mlats=mlats,lvals=lvals,rads=rads,ilats=ilats/geocoord,geotime='2016-01-20/19:44',


;************
;Test of ray initial values held constant throughout trajectory (raypath should be a single color...test with single ray)
;plot_rays,xcoord,ycoord,zcoord,ray_vals=lval0,$
;xrangeM=[0,6],zrangeM=[-3,3],minv=1,maxv=7.

;plot_rays,xcoord,ycoord,zcoord,ray_vals=radius0,$
;xrangeM=[0,6],zrangeM=[-3,3],minv=1,maxv=7.

;plot_rays,xcoord,ycoord,zcoord,ray_vals=lat,$
;xrangeM=[0,6],zrangeM=[-3,3],minv=1,maxv=10.
;************



;minv=3
;maxv=max(rads)
;contour,rads,xg,yg,nlevels=10,min_value=minv,max_value=maxv,$
;/cell_fill,xrange=[0,6],yrange=[-3,3],xstyle=1,ystyle=1,$
;background=255,position=aspect(1),color=2

;minv=0
;maxv=max(mlats)
;contour,mlats,xg,yg,nlevels=30,min_value=minv,max_value=maxv,$
;/cell_fill,xrange=[0,6],yrange=[-3,3],xstyle=1,ystyle=1,$
;background=255,position=aspect(1),color=2


;L2 = dipole(2.)
;L4 = dipole(4.)
;L6 = dipole(6.)
;L5 = dipole(5.)
;L8 = dipole(8.)
;earthx = COS((2*!PI/99.0)*FINDGEN(100))
;earthy = SIN((2*!PI/99.0)*FINDGEN(100))
;oplot,earthx,earthy,color=60
;;oplot,replicate(1.078,360.),indgen(360.)*!dtor,/polar,color=80

;oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80
;oplot,L2.R/6370.*cos(L2.lat*!dtor),L2.R/6370.*sin(L2.lat*!dtor),color=120
;oplot,L2.R/6370.*cos(L2.lat*!dtor),-1*L2.R/6370.*sin(L2.lat*!dtor),color=120

;oplot,L4.R/6370.*cos(L4.lat*!dtor),L4.R/6370.*sin(L4.lat*!dtor),color=120
;oplot,L4.R/6370.*cos(L4.lat*!dtor),-1*L4.R/6370.*sin(L4.lat*!dtor),color=120

;oplot,L6.R/6370.*cos(L6.lat*!dtor),L6.R/6370.*sin(L6.lat*!dtor),color=120
;oplot,L6.R/6370.*cos(L6.lat*!dtor),-1*L6.R/6370.*sin(L6.lat*!dtor),color=120

;oplot,L5.R/6370.*cos(L5.lat*!dtor),L5.R/6370.*sin(L5.lat*!dtor),color=120
;oplot,L5.R/6370.*cos(L5.lat*!dtor),-1*L5.R/6370.*sin(L5.lat*!dtor),color=120

;oplot,L8.R/6370.*cos(L8.lat*!dtor),L8.R/6370.*sin(L8.lat*!dtor),color=120
;oplot,L8.R/6370.*cos(L8.lat*!dtor),-1*L8.R/6370.*sin(L8.lat*!dtor),color=120

;;latitude lines
;latstmp = [0,10,20,30,40,50,60,70,80]
;latstmp = [-1*reverse(latstmp[1:n_elements(latstmp)-1]),latstmp]
;for i=0,n_elements(latstmp)-1 do oplot,[1,50]*cos([latstmp[i]*!dtor,latstmp[i]*!dtor]),[1,50]*sin([latstmp[i]*!dtor,latstmp[i]*!dtor]),linestyle=3,color=100


;loadct,39  ;need the first element to be black
;nticks = 10.
;tn = (indgen(nticks)/(nticks-1))*(maxv-minv)  + minv
;tn = strtrim(string(tn,format='(f8.2)'),2)
;colorbar,POSITION=[0.15, 0.75, 0.85, 0.77],$
;divisions=nticks-1,ticknames=tn,charsize = 0.8,range=[minv,maxv],color=2

;;*************************************

;stop

end
