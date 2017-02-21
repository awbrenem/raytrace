;Test different northward/southward propagation scenarios.
;I'm using this to make sure I understand all the variables in trace_in.txt

;Test how rays from a single point source near mag eq can spread out
;as they propagate to 20 deg mlat and beyond.


;N-S discrepency may be caused by higher order terms added to the dipole Bo.
;See Ondrej's email from Sept 4, 2010
;These should disppear at higher altitudes



;------------------------------------------------
;Northward tracing
;------------------------------------------------

freqv = 1000.
longit = 0.
ti = read_write_trace_in(freq=freqv,$
	bmodel=1,$
	mmult = 1.,$
	lat=0.,$
	theta=0.,$
	phi=0.,$
	longit=longit,$
	alt=(6370.*5)-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=35,$
	pplcp=3.,$
	pplhw=0.5,$
	arl=27391.,$
	drl=167.)

	thetavals = [0,10,20,30,40,50,60,70]

	freqs = replicate(freqv,n_elements(thetavals))
	create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3'

restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'
	plot_rays,xcoord,ycoord,zcoord,longit,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[4,5,6]



		;------------------------------------------------
		;Southward tracing
		;------------------------------------------------

		freqv =1800.
		longit = 0.
		ti = read_write_trace_in(freq=freqv,$
			bmodel=1,$
			mmult = 1.,$
			lat=0.,$
			theta=180.,$
			phi=0.,$
			longit=longit,$
			alt=(6370.*5.)-6370.,$
			final_alt=4000.,$
			model=0,$
			final_lat=-30,$
			pplcp=3.,$
			pplhw=0.5,$
			arl=27391.,$
			drl=167.)

			thetavals = [180,170,160,150,140,130,120,110]
			;thetavals = [180,190,200,210]
			;same as -1*[180,170,160,150]


			freqs = replicate(freqv,n_elements(thetavals))
			create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3'

		;restore multiple rays
		restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

		plot_rays,xcoord,ycoord,zcoord,longit,$
				xrangeM=[0,6],zrangeM=[-3,3],$
				Lsc=[4,5,6],raycolor=50
