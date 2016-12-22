;Test how rays from a single point source near mag eq can spread out
;as they propagate to 20 deg mlat and beyond.


;loc of PP = 3
;density < 10 @19:44:00
;Bo = 146 nT @19:44:00
;fce = 4088 Hz
;fce/2 = 2044 Hz
;f = 1700 Hz @19:44:00 UT
;wave normal angle limits (from EMFISIS) = -40 to 40
;RBSP-A location  L = 5.77


;------------------------------------------------
;FIRST VERSION IS FOR REGULAR CYCLOTRON resonance
;------------------------------------------------

;FB_leq1 = 5.0926843
;FB_leq2 = 5.248

timing_error_l = 0.1   ;uncertainty in L due to a ~5 sec timing
											 ;uncertainty in FB data. This is likely to be
											 ;high, but doesn't make much difference here.
FB_leq1 = 4.922 - timing_error_l
FB_leq2 = 5.072 + timing_error_l
;RBSPa_leq = 5.77   ;TSY04s model value
;TSY04 Bz at RBSPa is 10% too small
;Correcting this is equivalent to moving RBa from L=5.77 to L=5.39.
RBSPa_leq = 5.39   ;value if you adjust L to make |B|-|Bmodel|=0
;RBSPa_leq = 5.2
RBSPa_leq1 = RBSPa_leq - 0.15  ;adjustment for the size of a chorus wave packet (Agapitov)
RBSPa_leq2 = RBSPa_leq + 0.15

;Set up for southward raytracing
freqv = 1800.
ti = read_write_trace_in(freq=freqv,$
	mmult = .65,$
	lat=-0.93,$
	theta=0.,$
	phi=0.,$
	alt=(6370.*RBSPa_leq)-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=-50,$
	pplcp=3.,$
	pplhw=0.5,$
	drl=10000.)

	thetavals = 180+[-10.,-20,-30,-40.,-50.,-55,-60]


	freqs = replicate(freqv,n_elements(thetavals))
	create_rays,thetavals,freqs=freqs,title='uB3'

	dens_bo_profile,freqv,dens_sc=8,bo_sc=146,L_sc=RBSPa_leq;,/ps
;	x = read_trace_ta()

;restore multiple rays
restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'


	plot_rays,xcoord,ycoord,zcoord,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[FB_leq1,FB_leq2]


	fce = freqv/f_fce
	kvec = 2*!pi/wavelength   ;1/km
	pa = replicate(5.,n_elements(thetavals))
;	function cycl_energies,freq,theta_kb,pitchangle,fce,kmag,nres
	evals = cycl_energies(freqs,thk,pa,fce,kvec,1)


	eplot = evals.e_cycl_normal
	vz = evals.vz_cycl_normal
	;Get cycl energies along ray track
	;get e- precip time vs track location
	;combine with


		plot_rays,xcoord,ycoord,zcoord,ray_vals=eplot,$
			xrangeM=[0,6],zrangeM=[-3,3],$
			Lsc=[FB_leq1,FB_leq2],minval=5,maxval=850

			triangulate_rays,xcoord,zcoord,eplot,minv=5.,maxv=850.,Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],$
				limits=[0,-3,6,3],nlvls=50



			alt = 500.;rough altitude of FB4 (632 by 433 km orbit)

			dist_remaining = distance_to_atmosphere(lval,lat,offset_alt=alt,/opposite_hemisphere)
			;plot distance in RE to precipitation point of FIREBIRD
;			plot_rays,x.xcoord,x.ycoord,x.zcoord,ray_vals=dist_remaining,$
;				xrangeM=[0,6],zrangeM=[-3,3],$
;				Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=6.5,maxval=10


				;calculate time for scattered e- to arrive at FB
				;NOTE THAT I'M NOT ACCOUNTING FOR DECREASE(INCREASE) IN PARALLEL
				;VELOCITY AS E- GETS CLOSER TO EQUATOR(MIRROR POINT IN OPPOSITE HEMISPHERE)
				;TWO REASONS WHY THIS PROBABLY ISN'T IMPORTANT:
				;1) E- WILL SPEED UP NEAR EQ AND SLOW DOWN AWAY FROM IT. THESE EFFECTS
				;		WILL SOMEWHAT OFFSET (MORESO AT HIGHER LATS)
				;2) THE CHORUS TRAVEL TIME IS TYPICALLY THE DOMINANT EFFECT
				tarr = 6370.*dist_remaining/vz
				tarr = 1000.*tarr ;msec for better plotting

				plot_rays,xcoord,ycoord,zcoord,ray_vals=tarr,$
					xrangeM=[0,6],zrangeM=[-3,3],$
					Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=50,maxval=800

					triangulate_rays,xcoord,zcoord,tarr,minv=50.,maxv=800.,Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],$
						limits=[0,-3,6,3],nlvls=50



	;Now add together the time it takes for chorus wave to propagate
	;to a particular point with time it would take electron scattered at
	;that point to reach FIREBIRD
	;THIS IS THE TIME AFTER THE LAUNCH OF THE CHORUS WAVE THAT THE MICROBURST
	;SHOULD APPEAR
	ttotal = tarr + timeg*1000.
	plot_rays,xcoord,ycoord,zcoord,ray_vals=ttotal,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=1.,maxval=3000;,/ps

		triangulate_rays,xcoord,zcoord,ttotal,minv=1.,maxv=3000.,Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],$
			limits=[0,-3,6,3],nlvls=50

		;Filter the energy plot rays by total time.

		tmin = 0.
		tmax = 2500.

	goo = where((ttotal lt tmin) or (ttotal gt tmax))
	eplot2 = eplot
	eplot2[goo] = 0.

plot_rays,xcoord,ycoord,zcoord,ray_vals=eplot2,$
	xrangeM=[0,6],zrangeM=[-3,3],$
	Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=200,maxval=850,/ps








		;------------------------------------------------
		;SECOND VERSION IS FOR CO-STREAMING CYCLOTRON (n=1) RESONANCE
		;------------------------------------------------

		timing_error_l = 0.1   ;uncertainty in L due to a ~5 sec timing
													 ;uncertainty in FB data. This is likely to be
													 ;high, but doesn't make much difference here.
		FB_leq1 = 4.922 - timing_error_l
		FB_leq2 = 5.072 + timing_error_l
;		RBSPa_leq = 5.77   ;TSY04s model value
		;TSY04 Bz at RBSPa is 10% too small
		;Correcting this is equivalent to moving RBa from L=5.77 to L=5.39.
		RBSPa_leq = 5.39   ;value if you adjust L to make |B|-|Bmodel|=0
		;RBSPa_leq = 5.24   ;value if you adjust L to make |B|-|Bmodel|=0
		RBSPa_leq1 = RBSPa_leq - 0.15  ;adjustment for the size of a chorus wave packet (Agapitov)
		RBSPa_leq2 = RBSPa_leq + 0.15


	;Set up for northward raytracing
	freqv = 1700.
	ti = read_write_trace_in(freq=freqv,$
		mmult = .80,$
		lat=-0.93,$
		theta=0.,$
		phi=0.,$
		alt=(6370.*RBSPa_leq)-6370.,$
		final_alt=4000.,$
		model=0,$
		final_lat=42,$
		pplcp=3.,$
		pplhw=0.5,$
		drl=10000.)

thetavals = [-20.,-40.,-60.,-65]
;thetavals = 180+[-60,-50,-40,-30,-20, -10,0,10,20,30,40,50,60]
;thetavals = 0.
freqs = replicate(freqv,n_elements(thetavals))
create_rays,thetavals,freqs=freqs,title='uB3'

;dens_bo_profile,freqv,dens_sc=8,bo_sc=146,L_sc=RBSPa_leq;,/ps
;x = read_trace_ta()

;restore multiple rays
restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'


fce = freqv/f_fce
kvec = 2*!pi/wavelength   ;1/km
pa = replicate(5.,n_elements(thetavals))
evals = cycl_energies(freqs,thk,pa,fce,kvec,1)


eplot = evals.e_cycl_anom
vz = evals.vz_cycl_anom


	plot_rays,xcoord,ycoord,zcoord,ray_vals=eplot,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=0,maxval=850


		alt = 500.;rough altitude of FB4 (632 by 433 km orbit)
		dist_remaining = distance_to_atmosphere(lval,lat,offset_alt=alt)


			;calculate time for scattered e- to arrive at FB

			tarr = 6370.*dist_remaining/vz
			tarr = 1000.*tarr ;msec for better plotting
			plot_rays,xcoord,ycoord,zcoord,ray_vals=tarr,$
				xrangeM=[0,6],zrangeM=[-3,3],$
				Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=20,maxval=400;,/ps



;Now add together the time it takes for chorus wave to propagate
;to a particular point with time it would take electron scattered at
;that point to reach FIREBIRD

ttotal = tarr + timeg*1000.
plot_rays,xcoord,ycoord,zcoord,ray_vals=ttotal,$
	xrangeM=[0,6],zrangeM=[-3,3],$
	Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=400,maxval=600;,/ps


	;Filter the energy plot rays by total time.

	tmin = 0.
	tmax = 2500.

goo = where((ttotal lt tmin) or (ttotal gt tmax))
eplot2 = eplot
eplot2[goo] = 0.

plot_rays,xcoord,ycoord,zcoord,ray_vals=eplot2,$
xrangeM=[0,6],zrangeM=[-3,3],$
Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=200,maxval=800;,/psonly



;-------------------------------------------------------------------
;THIRD VERSION IS FOR ANOMALOUS CYCLOTRON (n>1) RESONANCE AT EQUATOR
;-------------------------------------------------------------------

;200 keV ray:  f=1700 Hz, theta_kb=-20
;800 keV ray:  f=1000 Hz, theta_kb = 60.


timing_error_l = 0.1   ;uncertainty in L due to a ~5 sec timing
											 ;uncertainty in FB data. This is likely to be
											 ;high, but doesn't make much difference here.
FB_leq1 = 4.922 - timing_error_l
FB_leq2 = 5.072 + timing_error_l
;		RBSPa_leq = 5.77   ;TSY04s model value
;TSY04 Bz at RBSPa is 10% too small
;Correcting this is equivalent to moving RBa from L=5.77 to L=5.39.
RBSPa_leq = 5.39   ;value if you adjust L to make |B|-|Bmodel|=0
;RBSPa_leq = 5.24   ;value if you adjust L to make |B|-|Bmodel|=0
RBSPa_leq1 = RBSPa_leq - 0.15  ;adjustment for the size of a chorus wave packet (Agapitov)
RBSPa_leq2 = RBSPa_leq + 0.15


;Set up for northward raytracing
freqv = 1000.
ti = read_write_trace_in(freq=freqv,$
mmult = .80,$
lat=-0.93,$
theta=0.,$
phi=0.,$
alt=(6370.*RBSPa_leq)-6370.,$
final_alt=4000.,$
model=0,$
final_lat=42,$
pplcp=3.,$
pplhw=0.5,$
drl=10000.)

thetavals = 60.
;thetavals = 180+[-60,-50,-40,-30,-20, -10,0,10,20,30,40,50,60]
;thetavals = 0.
freqs = replicate(freqv,n_elements(thetavals))
create_rays,thetavals,freqs=freqs,title='uB3'

;dens_bo_profile,freqv,dens_sc=8,bo_sc=146,L_sc=RBSPa_leq;,/ps
x = read_trace_ta()



fce = freqv/x.f_fce
kvec = 2*!pi/x.wl   ;1/km
evals = cycl_energies(freqv,x.thk,5.,fce,kvec,4)


eplot = evals.e_cycl_anom
vz = evals.vz_cycl_anom


;	plot_rays,x.xcoord,x.ycoord,x.zcoord,ray_vals=eplot,$
;		xrangeM=[0,6],zrangeM=[-3,3],$
;		Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=0,maxval=850


alt = 500.;rough altitude of FB4 (632 by 433 km orbit)
dist_remaining = distance_to_atmosphere(x.L,x.lat,offset_alt=alt)


	;calculate time for scattered e- to arrive at FB

	tarr = 6370.*dist_remaining/vz
	tarr = 1000.*tarr ;msec for better plotting
	plot_rays,x.xcoord,x.ycoord,x.zcoord,ray_vals=tarr,$
		xrangeM=[0,6],zrangeM=[-3,3],$
		Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=40,maxval=200,/ps



;Now add together the time it takes for chorus wave to propagate
;to a particular point with time it would take electron scattered at
;that point to reach FIREBIRD

ttotal = tarr + x.timeg*1000.
plot_rays,x.xcoord,x.ycoord,x.zcoord,ray_vals=ttotal,$
xrangeM=[0,6],zrangeM=[-3,3],$
Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=200,maxval=500,/ps


;Filter the energy plot rays by total time.

tmin = 0.
tmax = 2500.

goo = where((ttotal lt tmin) or (ttotal gt tmax))
eplot2 = eplot
eplot2[goo] = 0.

plot_rays,x.xcoord,x.ycoord,x.zcoord,ray_vals=eplot2,$
xrangeM=[0,6],zrangeM=[-3,3],$
Lsc=[RBSPa_leq1,RBSPa_leq2,FB_leq1,FB_leq2],minval=200,maxval=800,/psonly
