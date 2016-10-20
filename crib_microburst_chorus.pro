;Test how much rays from a single point source near mag eq can spread out
;as they propagate to 20 deg mlat and beyond.


function read_write_trace_in,freq=freq,lat=lat,alt=alt,longit=longit,theta=theta,phi=phi,$
	final_alt=final_alt,final_lat=final_lat,mmult=mmult,model=model,ppdc=ppdc,pplhw=pplhw,$
	pplcp=pplcp,drde=drde,dlhw=dlhw,dlcp=dlcp,arl=arl,drl=drl,nHp=nHp,nHe=nHe,nO=nO,temp=temp


;loc of PP = 3
;density < 10 @19:44:00
;Bo = 146 nT @19:44:00
;fce = 4088 Hz
;fce/2 = 2044 Hz
;f = 1700 Hz @19:44:00 UT
;wave normal angle limits (from EMFISIS) = -40 to 40
;RBSP-A location  L = 5.77

FB_leq = 5.0926843
FB_leq = 5.248
RBSPa_leq = 5.7704665



;OVERPLOT THE FB4 FIELD LINE!!!!

ti = read_write_trace_in(freq=1700.,$
	lat=-1.,$
	theta=0.,$
	alt=(6370.*5.77)-6370.,$
	model=2,$
	final_lat=40.,$
	pplcp=3.,$
	pplhw=0.5)


;	thetavals = [-40,-20, -10,0,10,20,40]
	thetavals = [-30,-20, -10,0,10,20,30]
freqs = replicate(1700.,n_elements(thetavals))

create_rays,thetavals,freqs=freqs,title='uB3'


restore,'uB3_rays.sav'
plot_rays,xcoord,ycoord,zcoord,xrangeM=[3,7],zrangeM=[-2,2],Lsc=[5.09,5.248]


;  KEYWORDS:    rayx -> (*,n_rays) - Meridional x values (SM coord) for ray (RE)
;				rayy -> Equatorial y
;				rayz -> Meridional z
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



v = read_trace_ta()
plot_rays,ray_struct=v,xrangeM=[2,6]



plot_rays,ray_struct='mb_rays'
