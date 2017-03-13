;Test different northward/southward propagation scenarios.
;I'm using this to make sure I understand all the variables in trace_in.txt

;Test how rays from a single point source near mag eq can spread out
;as they propagate to 20 deg mlat and beyond.



rbsp_efw_init


;------------------------------------------------
;Northward tracing IGRF model
;------------------------------------------------


;IGRF model uses geographic coordinates.
;Determine geographic coord from geomagnetic coord
;It also outputs geographic coord.

;Start with RBSP SM coord for jan20, 2016 at 19:44 UT (RBSP science gateway)
t0 = time_double('2016-01-20/19:44')
times = t0 + dindgen(10)
xsm = 5.25
ysm = -2.25
zsm = -0.1


;Transform our SM coord to geographic coord to input into trace_in.txt
geolatlong = transform_sm_to_geographic_latlong_for_igrf_raytrace_input([xsm,ysm,zsm],t0)

;Determine geographic values for final lat
smlat_fin = 30.
geolatlong_fin = transform_sm_to_geographic_latlong_for_igrf_raytrace_input($
	[geolatlong.radius,smlat_fin,geolatlong.smlong],t0,/smlatlong)


freqv = 1800.
ti = read_write_trace_in(freq=freqv,$
	bmodel=1,$
	mmult = 1.,$
	lat=geolatlong.geolat,$
	theta=0.,$
	phi=0.,$
	longit=geolatlong.geolong,$
	alt=geolatlong.radius*6370.-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=geolatlong_fin.geolat,$
	pplcp=3.,$
	pplhw=0.5,$
	arl=27391.,$
	drl=167.)

;	thetavals = [0,10,20,30,40,50,60,70]
thetavals = [0]

freqs = replicate(freqv,n_elements(thetavals))
create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime='2016-01-20/19:44'

;geographic coord
restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

rtmp = sqrt(xcoordSM^2 + ycoordSM^2 + zcoordSM^2)
print,rtmp[0:50]

	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$;geolatlong.geolong,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[4,5,6];,/geocoord,geotime='2016-01-20/19:44'



		;------------------------------------------------
		;Southward tracing
		;------------------------------------------------
		smlat_fin = -30.
		geolatlong_fin = transform_sm_to_geographic_latlong_for_igrf_raytrace_input($
			[geolatlong.radius,smlat_fin,geolatlong.smlong],t0,/smlatlong)


		freqv =1800.
;		longit = 0.
		ti = read_write_trace_in(freq=freqv,$
			bmodel=1,$
			mmult = 1.,$
			lat=geolatlong.geolat,$
			theta=180.,$
			phi=0.,$
			longit=geolatlong.geolong,$
			alt=geolatlong.radius*6370.-6370.,$
			final_alt=4000.,$
			model=0,$
			final_lat=geolatlong_fin.geolat,$
			pplcp=3.,$
			pplhw=0.5,$
			arl=27391.,$
			drl=167.)

		;	thetavals = [180,170,160,150,140,130,120,110]
thetavals = -180
			;thetavals = [180,190,200,210]
			;same as -1*[180,170,160,150]


			freqs = replicate(freqv,n_elements(thetavals))
			create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime='2016-01-20/19:44'

		;restore multiple rays
		restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

rtmp = sqrt(xcoord^2 + ycoord^2 + zcoord^2)
print,rtmp[0:50]

		plot_rays,xcoord,ycoord,zcoord,longit,$  ;geolatlong.geolong,$
				xrangeM=[0,6],zrangeM=[-3,3],$
				Lsc=[4,5,6],raycolor=50,/geocoord,geotime='2016-01-20/19:44'






				;------------------------------------------------
				;Northward tracing dipole
				;------------------------------------------------


				;Start with RBSP SM coord for jan20, 2016 at 19:44 UT (RBSP science gateway)
				t0 = time_double('2016-01-20/19:44')
				times = t0 + dindgen(10)
				xsm = 5.25
				ysm = -2.25
				zsm = -0.1


				smlat_fin = 30.

				freqv = 1800.
				ti = read_write_trace_in(freq=freqv,$
					bmodel=0,$
					mmult = 2.,$
					lat=0.,$
					theta=0.,$
					phi=0.,$
					longit=0.,$
					alt=5*6370.-6370.,$
					final_alt=4000.,$
					model=0,$
					final_lat=40,$
					pplcp=3.,$
					pplhw=0.5,$
					arl=27391.,$
					drl=167.)

				;	thetavals = [0,10,20,30,40,50,60,70]
				thetavals = [0]

				freqs = replicate(freqv,n_elements(thetavals))
				create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime='2016-01-20/19:44'

				restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

				rtmp = sqrt(xcoordSM^2 + ycoordSM^2 + zcoordSM^2)
				print,rtmp[0:50]

					plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
						xrangeM=[0,6],zrangeM=[-3,3],$
						Lsc=[4,5,6]


		;------------------------------------------------
		;Southward tracing dipole
		;------------------------------------------------


						;Start with RBSP SM coord for jan20, 2016 at 19:44 UT (RBSP science gateway)
;						t0 = time_double('2016-01-20/19:44')
						times = t0 + dindgen(10)
						xsm = 5.25
						ysm = -2.25
						zsm = -0.1

						freqv = 1800.
						ti = read_write_trace_in(freq=freqv,$
							bmodel=0,$
							mmult = 1.,$
							lat=0.,$
							theta=180.,$
							phi=0.,$
							longit=0.,$
							alt=5*6370.-6370.,$
							final_alt=4000.,$
							model=0,$
							final_lat=-40,$
							pplcp=3.,$
							pplhw=0.5,$
							arl=27391.,$
							drl=167.)

						;	thetavals = [0,10,20,30,40,50,60,70]
						thetavals = [140]

						freqs = replicate(freqv,n_elements(thetavals))
						create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime='2016-01-20/19:44'

						restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

						rtmp = sqrt(xcoordSM^2 + ycoordSM^2 + zcoordSM^2)
						print,rtmp[0:50]

							plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
								xrangeM=[0,6],zrangeM=[-3,3],$
								Lsc=[4,5,6]
