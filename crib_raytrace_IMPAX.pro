;NOTE - THERE'S A HUGE SIMPLIFICATION I CAN MAKE TO THIS PROGRAM. 
;RUN FOR A RANGE OF SOURCE L-VALUES AND CHORUS WAVE NORMAL ANGLES. FIND WHAT THE RANGE OF 
;RESONANT ELECTRONS IS ALONG EACH FIELD LINE AS WELL AS THEIR DT VALUES
;(this is what program currently does)
;THE SIMPLIFICATION IS THAT THE ABOVE IS AGNOSTIC TO THE BEST FIT LINE AND MIN/MAX ERROR SLOPES. 
;THESE CAN BE APPLIED LATER AFTER I HAVE SAVED ALL THE RAY TRACING RESULTS TO A FILE. 
;THIS MEANS THAT I CAN SIMULATE EACH SOURCE ONLY ONCE AND THEN LATER DECIDE WHAT THE RANGE OF SOURCES 
;WORKS FOR A PARTICULAR RANGE OF ERROR SLOPES. 

;CURRENTLY I'M RUNNIG THIS CODE (SIMULATING ALL SOURCES) FOR EVERY DIFFERENT FIT OF A MICROBURST MIN/MAX SLOPE. 
;VERY WASTEFUL.....

;HOWEVER, NEED TO MAKE SURE THAT ALL OF THE FILTERING CRITERIA ARE PRINTED. CURRENTLY THEY'RE NOT, AND 
;IT'S IMPOSSIBLE TO DISTINGUISH B/T AN ACTUAL SOURCE AND NONSOURCE BASED ON CURRENTLY OUTPUTTED DATA. 
;*******************************************************************************************

;******
;******
;****(6/15/2022) THIS CODE NEEDS SOME VERY CAREFUL DEBUGGING AND CHECKING, AND SHOULD BE REWRITTEN TO BE MORE MODULAR
;******
;******


;Test how much better having a better FIREBIRED instrument (e.g. IMPAX) will have vs 
;the 5 channels on FIREBIRD. 
;Microburst with dispersion observed on 2016-08-30/20:47:27.900 (FB is at L=6.1)
;During this time RBSPa is in the same region near equator and sees strong lower band chorus activity at ~ 1 kHz. 
;***CHECK TO SEE HOW CLOSE CONJUNCTION IS AND WHETHER THERE IS BURST DATA AVAILABLE

;USED IN CONJUNCTION WITH microburst_simulator.pro and microburst_fit_slope.pro

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


;The two constraints are:
;1) Find location extent along FB field line where the min and max FIREBIRD microburst energies are simulated. 
;2) narrow down that extent by requiring the arrival time difference of precipitated electrons to be within the 
;	max and min calculated microburst slope. 


;****TEST RESULTS OF SIMULATED MICROBURST SLOPE WITH NO NOISE (SEE microburst_fit_slope.pro)
;***ARRIVAL TIME DIFFERENCE OF UB B/T 220 AND 721 KEV
;5 channel --> [45.55,53.55,65.57]  (220-721 keV)
;10 channel--> [55.05,58.56,62.56]  (220-721 keV)
;1000 channel->[60.06] (CORRECT ANSWER FOR SIMULATED DATA)  (220-721 keV)


;N_S_rays = 'S'
;;;;;;EQUATORIAL SOURCE 
;source_L = 6.15
;nbins = '5'
;freqv = 1000.

;vals = crib_raytrace_IMPAX(N_S_rays,source_L,nbins,freqv,noplot=np)

function crib_raytrace_IMPAX,N_S_rays,source_L,nbins,freqv,noplot=np,finalplot=finalplot


tst = file_test('~/Desktop/raytrace_IMPAX.txt')

if tst then begin
	openw,lun,'~/Desktop/raytrace_IMPAX.txt',/get_lun,/append 
endif else begin 
	openw,lun,'~/Desktop/raytrace_IMPAX.txt',/get_lun
endelse 





;---------------------------------------------------
;END USER INPUT 
;---------------------------------------------------



;Manually set indices of slice array that correspond to the min and max precipitating energies
;corresponding to delta-times from best fit line and errors (bin0 from fblow and bin1 from fbhig)
;Because there is uncertaintly in the channel energy (e.g. 220-283 keV bin) I'll have a min and max low energy 
;and a min and max high energy
if nbins eq '5' then begin 
    fblow = [220.,283.,384.,520.,721.]
    fbhig = [283.,384.,520.,721.,985.]
	bin0 = 0.  ;lowest energy bin to consider (520-721)
	bin1 = 3.  ;highest energy bin to consider
endif else begin 

	emaxtmp = 1000. ;keV 
	emintmp = 125.  ;keV
	nch = float(nbins)
	fblow = (emaxtmp - emintmp) * indgen(nch)/(nch-1) + emintmp 
	fbhig = shift(fblow, -1) 
	;stop
	deltae = fbhig[0] - fblow[0]
	fbhig[n_elements(fbhig)-1] = emaxtmp + deltae
	bin0 = 0.
	if nbins eq '63' then bin1 = 41.  ;for 63 bins
	if nbins eq '16' then bin1 = 10.  ;for 16 bins
	if nbins eq '32' then bin1 = 20.
endelse



;The min/max ALLOWABLE arrival time differences are a subset of the above, and are determined 
;by the max/min errors to the best-fit slope from microburst_fit_slope.pro

if nbins eq '5' then begin 
	minallowt = 40.  ;msec
	maxallowt = 250.  ;msec
endif
if nbins eq '63' then begin
	minallowt = 63.  ;msec
	maxallowt = 77.  ;msec  
endif
if nbins eq '16' then begin
	minallowt = 49.  ;msec
	maxallowt = 87.  ;msec  
endif
if nbins eq '32' then begin
	minallowt = 60.  ;msec
	maxallowt = 78.  ;msec  
endif


;----------------
;Print header info
;----------------

printf,lun,'Freq = ' + strtrim(floor(freqv),2)+'Hz rays heading '+N_S_rays+' for source at L='+strtrim(source_L,2)+' for '+nbins+' energy bins'
printf,lun,'Instrument energy bin range = '+strtrim(floor(fblow[0]),2) + '-' + strtrim(floor(fbhig[n_elements(fbhig)-1]),2) +' keV'
printf,lun,'Required energy range for calculating dt = ' + strtrim(floor(fblow[bin0]),2) + '-' + strtrim(fbhig[bin1],2) + ' keV'
printf,lun,'min/max allowable dt = ' + strtrim(minallowt,2) + ' - ' + strtrim(maxallowt,2)



;FU3 location at 2016-08-30/20:47:27.900
geolat = -51.026791
geolon = 84.924263
alt = 504.91843 + 6370.
mlt = 2.1601484
L = 6.0482759

xgeo = alt*cos(!dtor*geolat)*cos(!dtor*geolon)
ygeo = alt*cos(!dtor*geolat)*sin(!dtor*geolon)
zgeo = alt*sin(!dtor*geolat)

t0 = time_double('2016-08-30/20:47:27.900')
times = dindgen(20)/1. + t0

store_data,'fb3_geo',data={x:times,y:[[replicate(xgeo,20)],[replicate(ygeo,20)],[replicate(zgeo,20)]]}
;tplot,'fb3_geo'
;tlimit,times[0],times[19]

;Calculate MLT
cotrans,'fb3_geo','fb3_gei',/geo2gei
cotrans,'fb3_gei','fb3_gse',/gei2gse
cotrans,'fb3_gse','fb3_gsm',/gse2gsm
cotrans,'fb3_gsm','fb3_sm',/gsm2sm    ;VERIFIED CORRECT FROM SSCWEB

;tplot,'fb3_sm'
;FU3 Local observations at 20:47 UT:
	;mlat = -61.29 (SM)
	;mlong = 212.89 (SM)
	;L = 4.7 
	;RE = 
	;smcoord = [-0.43,-0.28,-0.94]


;Plasma at equatorial location of ray source (USING RBSPA DATA)
	;density = 0.5 cm-3 (from HOPE - probably not very accurate) **NOTE THAT THE EMFISIS DENSITY DOESN'T EXIST AT THIS TIME
	;Bo = 95 nT
	;fce = 2660 Hz
	;fce/2 = 1330 Hz
	;fce/10 = 266 Hz
	;f = 1000 Hz


rbsp_efw_init


;------------------------------------------------
;FIRST VERSION IS FOR REGULAR CYCLOTRON resonance
;------------------------------------------------



;Final L=value to extract data from 
lval_extract = L

;--------------------------------------------
;Set up model for observed density, Bo.
altv=(6370.*lval_extract)-6370.
create_rays_general,1000.,theta=0,alt=altv,lat=0.,long=85.,title='uB3',geotime='2016-08-30/20:47'
if ~keyword_set(np) then dens_bo_profile,1000.,dens_sc=10,bo_sc=95,L_sc=lval_extract;,/ps
;--------------------------------------------
;geolat = -51.026791
;geolon = 84.924263

finlat = 40. 
if N_S_rays eq 'S' then finlat *= -1

ti = read_write_trace_in(freq=freqv,$
	bmodel = 0,$
	mmult = 1,$
	lat=0.,$
	longit=0.,$  ;213,$
	theta=0.,$
	phi=0.,$
	alt=(6370.*source_L)-6370.,$
	final_alt=4000.,$
	model=0,$
	final_lat=finlat,$
	pplcp=3.,$
	pplhw=0.5,$
	drl=20.)


	;Test out the density model
	create_rays_thetakb_spread,0.,freqs=freqv,title='uB3',geotime=time
;	rba_l2 = rba_l
	if ~keyword_set(np)then dens_bo_profile,freqv,dens_sc=10,bo_sc=95,L_sc=lval_extract;,/ps



;	;---Co-streaming resonance (Southward, anti-Earthward rays)
	if N_S_rays eq 'S' then begin 
		thetavals = 90 + [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
		opposite_hemisphere = 0
	endif 

	;---Counterstreaming resonance (Northward, anti-Earthward rays)
	if N_S_rays eq 'N' then begin 
		thetavals = [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85]
		opposite_hemisphere = 1
	endif

	time = '2016-08-30/20:47'

	freqs = replicate(freqv,n_elements(thetavals))
	create_rays_thetakb_spread,thetavals,freqs=freqs,title='uB3',geotime=time

	x = read_trace_ta(geotime=time_double(time))

;restore multiple rays
	restore,'/Users/aaronbreneman/Desktop/code/Aaron/github.umn.edu/raytrace/uB3_rays.sav'

;SM coord of FU3 (Meridional plane)
	oplotcoord = transpose([[-0.46,-0.29,-0.93]])

	if ~keyword_set(np) then plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,$
		xrangeM=[1,7],zrangeM=[-3,3],$
		oplotX=oplotcoord




	;-----------------------------------------------------
	;Determine cyclotron resonance energy along ray path
	;This will be used to find locations where the microbursts can be created. 

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

	;plot cyclotron resonance energies keV
	if ~keyword_set(np) then plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=eplot,$
		xrangeM=[1,7],zrangeM=[-3,3],$
		Lsc=[lval_extract],minval=5,maxval=1000

if ~keyword_set(np) then stop
	;cyclotron resonance energies (keV) triangulated
	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,eplot,minv=5.,maxv=1500.,Lsc=[lval_extract],$
		limits=[1,-3,7,3],nlvls=50,oplotX=oplotcoord,result=energy_gridv,xgrid=xg,ygrid=yg,noplot=np

if ~keyword_set(np) then stop
	;Extract slice along single L-value
	mlatrange = 90.*indgen(1000)/999.
	if opposite_hemisphere ne 1 then mlatrange *= -1
	energy_slice = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,energy_gridv,gridpts=pts)

	for ii=0,999 do print,ii,mlatrange[ii],energy_slice[ii]
	;plot,mlatrange,energy_slice,xrange=[10,30],xtitle='mlat (deg)',ytitle='energy (keV) counterstream cyclotron res'


	;---------------------------------------------------------------------
	;Calculate indices that are used to determine the range of delta-times from the min and max error slopes 
	;---------------------------------------------------------------------

	cond1 = where(energy_slice ge fblow[bin0]) 
	cond2 = where(energy_slice le fbhig[bin0]) 
	tmp = setintersection(cond1, cond2)
	ind0 = [tmp[0],tmp[n_elements(tmp)-1]]
	if ind0[0] eq ind0[1] then ind0[1] += 1 ;avoid infinitely thin mlat source

	cond1 = where(energy_slice ge fblow[bin1]) 
	cond2 = where(energy_slice le fbhig[bin1]) 
	tmp = setintersection(cond1, cond2)
	ind1 = [tmp[0],tmp[n_elements(tmp)-1]]
	if ind1[0] eq ind1[1] then ind1[1] += 1 ;avoid infinitely thin mlat source



;print,'MAKE SURE THAT THE FIELD LINE OF FIREBIRD SEES ELECTRONS IN THE LOW AND HIGH ENERGY RANGE CHANNELS'
print,'RANGE OF RESONANT ELECTRON ENERGIES SEEN ON FIELD LINE FALLING WITHIN INSTRUMENT RANGE'
print,energy_slice[ind0[0]], energy_slice[ind1[1]]   ;keV
;print,energy_slice[indA[0]], energy_slice[indA[1]]   ;keV
print,'COMPARISON OF ENERGY SLICES'
if ~keyword_set(np) then stop  ;MAKE SURE ENTIRE DESIRED ENERGY RANGE IS FOUND ON FIELD LINE OF FIREBIRD. 
;;IF NOT, THEN MOVE SOURCE

if total(ind0) eq -2. or total(ind1) eq -2. then begin ;printf,lun,'NO APPROPRIATE RAYS ON FIELD LINE'
;if total(indA) eq -2. then begin ;printf,lun,'NO APPROPRIATE RAYS ON FIELD LINE'
	printf,lun,'NO SOLUTION FOUND - no appropriate rays on field line'
	printf,lun,'Range of resonant e- energies seen on field line: ' + strtrim(floor(energy_slice[ind0[0]]),2)+'-'+strtrim(floor(energy_slice[ind1[1]]),2)+' keV'
	printf,lun,'dt range observed = ' + strtrim(floor(dt_msec_min_observed),2) + '-' + strtrim(floor(dt_msec_max_observed),2) + ' msec'
	printf,lun,'dt allowable range = ' + strtrim(floor(minallowt),2) + '-' + strtrim(floor(maxallowt),2)+' msec'
	printf,lun,'---------------------------------------------------'
	close,lun 
	free_lun,lun 

	vals = {N_S_rays:N_S_rays,source_L:source_L,nbins:nbins,freqv:freqv,$
		deltalat_fin_max:!values.f_nan,$
		min_lats_low_fin:!values.f_nan,max_lats_low_fin:!values.f_nan,$
		min_lats_hig_fin:!values.f_nan,max_lats_hig_fin:!values.f_nan}

	return,vals
endif


;****
;MLATS WHERE THE ENTIRE RANGE OF MICROBURSTS CAN BE OBSERVED 
;MLAT = 17 DEG (295 KEV) TO 22 DEG (711 KEV)




;	;-----------------------------------------------------
;	;Determine travel time (group velocity)
;
;	;Plot the travel time along the trajectory
;	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=timeG,$
;		xrangeM=[2,6],zrangeM=[-2,2],$
;		oplotX=oplotcoord,minval=0,maxval=0.5
;
;	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,timeG,minv=0.,maxv=0.5,Lsc=[lval_extract[0]],$
;		limits=[0,-3,6,3],nlvls=100,xgrid=xg,ygrid=yg,result=timeG_gridv,xrange=[2,6],yrange=[-2,2],$
;		oplotX=oplotcoord;,/psonly
;
;	;Extract slice along single L-value
;	timeG_slice = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,timeG_gridv,gridpts=pts)
;
;	for ii=0,89 do print,mlatrange[ii],timeG_slice[ii]






	;Now calculate the time it takes scattered electrons to reach FU3

	alt = 500.;rough altitude of FB3 (632 by 433 km orbit)
	dist_remaining = distance_to_atmosphere(lval,latSM,offset_alt=alt,opposite_hemisphere=opposite_hemisphere)
;opposite_hemisphere -> set of you'd like to know the distance a loss-cone scattered
;     e- must travel to the opposite hemisphere. Use for counter-streaming cyclotron resonance.
;     Don't set for costreaming or Landau resonance where e- will be precipitating in
;     same hemisphere that it interacts in.
;


	;calculate time for scattered e- to arrive at FB
	tarr = 6370.*dist_remaining/vz
	tarr = 1000.*tarr ;msec for better plotting

;	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=tarr,$
;		xrangeM=[2,6],zrangeM=[-2,2],$
;		Lsc=[lval_extract[0]],minval=5,maxval=200
;
;	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,tarr,minv=5.,maxv=200.,Lsc=[lval_extract[0]],$
;		limits=[0,-3,6,3],nlvls=50,oplotX=oplotcoord,result=tarr_gridv


;	;Extract slice along single L-value
;	tarr_slice = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,tarr_gridv,gridpts=pts)
;	for ii=0,89 do print,mlatrange[ii],tarr_slice[ii]
;


	;Now add together the time it takes for chorus wave to propagate
	;to a particular point with time it would take electron scattered at
	;that point to reach FIREBIRD

	ttotal = tarr + timeg*1000.

	if ~keyword_set(np) then plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=ttotal,$
	xrangeM=[1,7],zrangeM=[-3,3],$
	Lsc=[lval_extract[0]],minval=220.,maxval=2000.

if ~keyword_set(np) then stop


;	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ttotal,minv=220.,maxv=2000.,Lsc=[lval_extract[0]],$
;	limits=[1,-3,7,3],nlvls=50,xgrid=xg,ygrid=yg,result=totaltime_gridv;,/zbuffer
	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ttotal,Lsc=[lval_extract[0]],$
	limits=[1,-3,7,3],nlvls=50,xgrid=xg,ygrid=yg,result=totaltime_gridv,noplot=np;,/zbuffer


if ~keyword_set(np) then stop

	;Get values of grid points that correspond to a particular L
;		totaltimeL[*,bbq] = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,totaltime,gridpts=pts)
;		totaltimeL = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,totaltime,gridpts=pts)

	;Extract slice along single L-value
	totaltimeL_slice = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,totaltime_gridv,gridpts=pts)
	for ii=0,999 do print,ii,mlatrange[ii],totaltimeL_slice[ii]


;print,totaltimeL_slice[indA[1]] - totaltimeL_slice[indA[0]]
print,totaltimeL_slice[ind0]
print,totaltimeL_slice[ind1]    ;msec time difference of arrival b/t min and max energy at FIREBIRD
;stop

;---------------------------------------------------------------------------
;Calculate min/max range of final arrival time differences (msec) b/t lowest and highest energies 
;Positive values mean that lower energies arrive first (like positive slope). 

d1tmp = max(totaltimeL_slice[ind1]) - min(totaltimeL_slice[ind0])
d2tmp = min(totaltimeL_slice[ind1]) - max(totaltimeL_slice[ind0])
dt_msec_min_observed = d1tmp < d2tmp 
dt_msec_max_observed = d1tmp > d2tmp 

;dt_msec_min_observed = max(totaltimeL_slice[ind1]) - min(totaltimeL_slice[ind0])
;dt_msec_max_observed = min(totaltimeL_slice[ind1]) - max(totaltimeL_slice[ind0])


;dt_msec_test = min(totaltimeL_slice[indA[1]]) - max(totaltimeL_slice[indA[0]])

;*********************
;NOTE: dt_msec_min_observed and dt_msec_max_observed MAY NOT BE FOR THE ENTIRE 
;DESIRED ENERGY RANGE (E.G. 220-750 KEV) SINCE NOT ALL OF THOSE ENERGIES ARE NECESSARILY SEEN 
;ON THE FIELD LINE IN QUESTION. 
;MAKE SURE THAT WE ARE COMPARING THE DELTA-TIMES FOR THE CORRECT DELTA-ENERGIES. 
;*********************



;Min difference of arrival times observed along field line of FIREBIRD
;print,'delta-t min observed = ' + strtrim(dt_msec_min_observed,2) + ' msec'
;;Max difference of arrival times observed along field line of FIREBIRD
;print,'delta-t max observed = ' + strtrim(dt_msec_max_observed,2) + ' msec'
;print,'The above delta-times need to fall within range of ' + strtrim(minallowt,2) + ' - ' + strtrim(maxallowt,2) + '. If not then no uB source solutions exist'
;stop


if ~keyword_set(np) then stop


;Calculate 2D array with all possible arrival time differences for the entire range of acceptable 
;scattering locations for the low energy and high energy
nind0 = ind0[1]-ind0[0]
nind1 = ind1[1]-ind1[0]
final_deltat = fltarr(nind0,nind1)
final_deltat = reform(final_deltat,nind0,nind1)  ;make sure it's a 2D array if one of the dimensions is of size 1
for i=0,nind0-1 do begin $
	for j=0,nind1-1 do final_deltat[i,j] = totaltimeL_slice[ind1[0]+j] - totaltimeL_slice[ind0[0]+i]
endfor

;low energy lat values 
nelem = abs(ind0[1]-ind0[0])
valslow = indgen(nelem) + min(ind0)
latrange_low = mlatrange[valslow]
nelem = abs(ind1[1]-ind1[0])
valshig = indgen(nelem) + min(ind1)
latrange_hig = mlatrange[valshig]





;-----------------------------------------------------------------------------------------------------
;Master printout to make sure that the ray tracing satisfies the following requirement along the field line of interest. 
;-----------------------------------------------------------------------------------------------------

;1) entire energy range of microburst observed
;2) some subsection of delta-times falls within max and min deltatime estimates from best-fit slope + error slopes. 


print,'Energy range required along L=' + strtrim(source_L,2) + ' is ' + strtrim(fblow[bin0],2) + ' - ' + strtrim(fbhig[bin1],2) + ' keV'
;print,'Energy range observed: ' + strtrim(energy_slice[indA[0]],2) + ' - ' + strtrim(energy_slice[indA[1]],2) + ' keV'
print,'The above delta-times need to fall within range of ' + strtrim(minallowt,2) + ' - ' + strtrim(maxallowt,2) + '. If not then no uB source solutions exist'

;Min difference of arrival times observed along field line of FIREBIRD
print,'delta-t min observed = ' + strtrim(dt_msec_min_observed,2) + ' msec'
;Max difference of arrival times observed along field line of FIREBIRD
print,'delta-t max observed = ' + strtrim(dt_msec_max_observed,2) + ' msec'




;-----------------------------------------------------------------------------------------------------

;**************************
;BELOW WE TEST DIFFERENT MIN AND MAX SLOPES
;**************************


;***********************
;***********************
;***********************
;TESTING NOTES: THERE SEEMS TO BE A DISCREPANCY B/T THE FINAL_DELTAT VALUES AND 
;DT_MSEC_MIN_OBSERVED AND DT_MSEC_MAX_OBSERVED.
;FOR EXAMPLE: x = crib_raytrace_IMPAX('N',6.15,'5',300.,/noplot)
;THE DT_MSEC_???_OBSERVED VALUES ARE BOTH >> THAN THE ALLOWED DT, BUT 
;THERE IS A SINGLE VALUE IN FINAL_DELTAT THAT'S WITHIN THE DT RANGE. 
;***********************
;***********************
;***********************


;goo = where((final_deltat ge minallowt) and (final_deltat le maxallowt))
;if goo[0] eq -1 then begin 

goo = where((final_deltat ge minallowt) and (final_deltat le maxallowt))


c1 = dt_msec_max_observed gt minallowt 
c2 = dt_msec_min_observed lt maxallowt 
tst = c1 and c2 
if tst eq 0 or goo[0] eq -1 then begin 
	printf,lun,'NO SOLUTION FOUND - INCORRECT DELTA-T OF ARRIVAL'
	printf,lun,'Range of resonant e- energies seen on field line: ' + strtrim(floor(energy_slice[ind0[0]]),2)+'-'+strtrim(floor(energy_slice[ind1[1]]),2)+' keV'
	printf,lun,'dt range observed = ' + strtrim(floor(dt_msec_min_observed),2) + '-' + strtrim(floor(dt_msec_max_observed),2) + ' msec'
	printf,lun,'dt allowable range = ' + strtrim(floor(minallowt),2) + '-' + strtrim(floor(maxallowt),2)+' msec'
	printf,lun,'---------------------------------------------------'
	close,lun 
	free_lun,lun 

	vals = {N_S_rays:N_S_rays,source_L:source_L,nbins:nbins,freqv:freqv,$
		deltalat_fin_max:!values.f_nan,$
		min_lats_low_fin:!values.f_nan,max_lats_low_fin:!values.f_nan,$
		min_lats_hig_fin:!values.f_nan,max_lats_hig_fin:!values.f_nan,$
		Emin:fblow[bin0],Emax:fbhig[bin1], dtmin:minallowt, dtmax:maxallowt}
	print,'****NO CHORUS SOURCE FOUND****'
;	stop
	return,vals
endif 

indxy = ARRAY_INDICES(final_deltat, goo)

;mlatincrement = 90./1000.
;	mlatrange = 90.*indgen(1000)/999.

;Final values of latitudes where the min/max ALLOWABLE time differences are satisfied 
lats_low_fin = fltarr(n_elements(indxy[0,*]))
lats_hig_fin = fltarr(n_elements(indxy[0,*]))


for i=0,n_elements(indxy[0,*])-1 do lats_low_fin[i] = latrange_low[indxy[0,i]]
for i=0,n_elements(indxy[1,*])-1 do lats_hig_fin[i] = latrange_hig[indxy[1,i]]



;stop
;--------------------------------------------------------------------------
;Replot the rays with only the final acceptable locations for scattering 
;low energy lat values 

;Calculate x,y,z values of final lat ranges 
latrange_lowx = L*cos(!dtor*lats_low_fin)^2*cos(lats_low_fin*!dtor)
latrange_lowz = L*cos(!dtor*lats_low_fin)^2*sin(lats_low_fin*!dtor)
latrange_higx = L*cos(!dtor*lats_hig_fin)^2*cos(lats_hig_fin*!dtor)
latrange_higz = L*cos(!dtor*lats_hig_fin)^2*sin(lats_hig_fin*!dtor)

minx = min(latrange_lowx < latrange_higx) & maxx = max(latrange_lowx > latrange_higx)
minz = min(latrange_lowz < latrange_higz) & maxz = max(latrange_lowz > latrange_higz)

cond1 = where((xcoordSM ge minx) and (xcoordSM le maxx))
cond2 = where((zcoordSM ge minz) and (zcoordSM le maxz))

goo = setintersection(cond1,cond2)
xcoordSMtmp = xcoordSM & xcoordSMtmp[*] = 0. & xcoordSMtmp[goo] = xcoordSM[goo]
ycoordSMtmp = ycoordSM & ycoordSMtmp[*] = 0. & ycoordSMtmp[goo] = ycoordSM[goo]
zcoordSMtmp = zcoordSM & zcoordSMtmp[*] = 0. & zcoordSMtmp[goo] = zcoordSM[goo]
ttotaltmp = ttotal & ttotaltmp[*] = 0. & ttotaltmp[goo] = ttotal[goo]

;FINAL PLOT
if keyword_set(finalplot) then begin 
triangulate_rays,xcoordSMtmp,ycoordSMtmp,zcoordSMtmp,longitSM,ttotaltmp,Lsc=[lval_extract[0]],$
limits=[1,-3,7,3],nlvls=50,xgrid=xg,ygrid=yg,result=totaltime_gridvtmp,/psonly;,noplot=np
endif

;stop
;--------------------------------------------------------------------------

;;******TEST: IF ALL FINAL DELTA-TIMES ARE ALLOWED, THE ABOVE SHOULD COMPARE EXACTLY TO 
;mlatrange[ind0]
;mlatrange[ind1]
;;****NOTE THAT THEY'RE SLIGHTLY DIFFERENT




;Maximum possible source extent for acceptable arrival time differences b/t minallowt and maxallowt
deltalat_fin_max = abs(max(lats_hig_fin)) - abs(min(lats_low_fin))


;-------------
print,'*****SOLUTION FOUND*****'
print,'freq = ' + strtrim(fq,2)
print,'Rays heading '+N_S_rays+' for source at L=',source_L,' for min/max slope from '+nbins+' energy bin simulation'
print,'Max mlat source size =',deltalat_fin_max
print,'Range of lats that may be source for lowest energy bin = ',min(lats_low_fin),max(lats_low_fin)
print,'Range of lats that may be source for highest energy bin = ',min(lats_hig_fin),max(lats_hig_fin)
print,'RANGE OF RESONANT ELECTRON ENERGIES SEEN ON FIELD LINE FALLING WITHIN INSTRUMENT RANGE'
print,energy_slice[ind0[0]], energy_slice[ind1[1]]   ;keV
print,'delta-t min observed = ' + strtrim(dt_msec_min_observed,2) + ' msec'
print,'delta-t max observed = ' + strtrim(dt_msec_max_observed,2) + ' msec'
print,'The above delta-times need to fall within range of ' + strtrim(minallowt,2) + ' - ' + strtrim(maxallowt,2) + '. If not then no uB source solutions exist'
;-------------
;stop


printf,lun,'*****SOLUTION FOUND*****'
printf,lun,'Max mlat source size ='+strtrim(deltalat_fin_max,2)+' deg'
printf,lun,'Range of lats that may be source for lowest energy bin = ' + strtrim(min(lats_low_fin),2)+'-'+strtrim(max(lats_low_fin),2)+' deg'
printf,lun,'Range of lats that may be source for highest energy bin = '+ strtrim(min(lats_hig_fin),2)+'-'+strtrim(max(lats_hig_fin),2)+' deg'
printf,lun,'Range of resonant e- energies seen on field line: ' + strtrim(floor(energy_slice[ind0[0]]),2)+'-'+strtrim(floor(energy_slice[ind1[1]]),2)+' keV'
printf,lun,'dt range observed = ' + strtrim(floor(dt_msec_min_observed),2) + '-' + strtrim(floor(dt_msec_max_observed),2) + ' msec'
printf,lun,'dt allowable range = ' + strtrim(floor(minallowt),2) + '-' + strtrim(floor(maxallowt),2)+' msec'
printf,lun,'---------------------------------------------------'
close,lun 
free_lun,lun 


vals = {N_S_rays:N_S_rays,source_L:source_L,nbins:nbins,freqv:freqv,$
	deltalat_fin_max:deltalat_fin_max,$
	min_lats_low_fin:min(lats_low_fin),max_lats_low_fin:max(lats_low_fin),$
	min_lats_hig_fin:min(lats_hig_fin),max_lats_hig_fin:max(lats_hig_fin),$
	Emin:fblow[bin0],Emax:fbhig[bin1], dtmin:minallowt, dtmax:maxallowt,$
	dt_msec_min_observed:dt_msec_min_observed,dt_msec_max_observed:dt_msec_max_observed}


return,vals
;stop

;***********************************
;5 ENERGY BINS


;Northward: Counterstreaming 1 kHz point source tests (45.55-65.57 msec delta-t allowed):

	;L=5.9 - NO SOLUTION 
	;L=6.0
		;Max mlat source size =      5.8
		;Range of lats that may be source for lowest energy bin =       26.0360      27.2072
		;Range of lats that may be source for highest energy bin =       30.4505      31.8919
	;L=6.15
		;Max mlat source size =      5.76577
		;Range of lats that may be source for lowest energy bin =       27.0270      28.1982
		;Range of lats that may be source for highest energy bin =       31.4414      32.7928

;Southward: Co-streaming 1 kHz point source tests 
	;L=6.0 - NO SOLUTION
	;L=6.1 - NO SOLUTION (not full upper energy range, and no final solution)
	;L=6.15 
		;Max mlat source size =      3.51351
		;Range of lats that may be source for lowest energy bin =      -30.2703     -29.2793
		;Range of lats that may be source for highest energy bin =      -34.9550     -33.7838


;***********************************
;10 ENERGY BINS


;Northward: Counterstreaming 1 kHz point source tests (55.05-62.56 msec delta-t allowed):
	;L=6.0
		;Max mlat source size =      5.76577
		;Range of lats that may be source for lowest energy bin =       26.0360      26.2162
		;Range of lats that may be source for highest energy bin =       31.3514      31.8018
	;L=6.15
		;Max mlat source size =      5.76577
		;Range of lats that may be source for lowest energy bin =       27.0270      27.4775
		;Range of lats that may be source for highest energy bin =       32.2523      32.7928


;Southward: Co-streaming 1 kHz point source tests 

	;L=6.15 - NO SOLUTION


























stop










;*********************
;EXTRA STUFF
;*********************




	;Plot the wave normal angle along the trajectory
	plot_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,ray_vals=thk,$
		xrangeM=[1,7],zrangeM=[-3,3],$
		oplotX=oplotcoord,minval=0,maxval=90

	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk,minv=0.,maxv=90.,Lsc=[lval_extract[0]],$
		limits=[1,-3,7,3],nlvls=100,xgrid=xg,ygrid=yg,result=tkb_gridv,xrange=[2,6],yrange=[-2,2],$
		oplotX=oplotcoord,noplot=np;,/psonly

	;Extract slice along single L-value
	thk_slice = extract_lshell_mlat_slice(lval_extract[0],mlatrange,xg,yg,tkb_gridv,gridpts=pts)

	for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
	;I find the wave normal angle at Arase is ~72 deg



	;-----------------------------------------------------
	;Determine initial wave normal angle
	;Get ray initial theta_kb values
	triangulate_rays,xcoordSM,ycoordSM,zcoordSM,longitSM,thk0,minv=1.,maxv=60.,Lsc=[lval_extract[0]],$
	limits=[1,-3,7,3],nlvls=50,xgrid=xg,ygrid=yg,result=tmpvar,noplot=np,/zbuffer
	for i=0,89 do thk0_finL[i,bbq] = tmpvar[ptsx[i],ptsz[i]]
	print,'theta_kb initial value along slice = ',thk0_finL[*,bbq]




;***********************
;*************************









;*************************************************************************************
;NOW EXTRACT THE VALUES ONLY WHERE THE CORRECT ENERGIES ARE ON THE CORRECT FIELD LINE
;*************************************************************************************





;************HERE  
;************HERE  
;************HERE  
;************HERE  

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
		oplotX=oplotcoord,noplot=np

	;Extract slice along single L-value
	lval_extract = 3.75  ;Arase Lshell
	;lval_extract = 3.91  ;RBSPa Lshell - test (works - returns theta_kb at location or RBSPa)

	thk_slice = extract_lshell_mlat_slice(lval_extract,mlatrange,xg,yg,tkb_gridv,gridpts=pts)

	for ii=0,89 do print,mlatrange[ii],thk_slice[ii]
	;I find the wave normal angle at Arase is ~72 deg


	;Equatorial location that produces rays that go through RBSP and ARASE is
	;thk = 176.9  --> equivalent to -3.1 deg
	;SM = [1.62385,3.64722,0.]

stop
end