;+
;*****************************************************************************************
;
;  FUNCTION : read_write_trace_in
;  PURPOSE  : reads the file trace_in.txt (input file for trace.for) and returns its values
;			  and also changes these values in trace_in.txt
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
;  EXAMPLES:
;
;
;  KEYWORDS:  (see corresponding variable list below)
;				bmodel --> 0 for dipole, 1 for IGRF
;				interp_date --> IGRF OR DIPOLE INTERPOLATION DATE (IYEAR,IDOY): e.g. "2005    209"
;				mmult -> magnetic multiplier
;				model -> density model used
;					ppdc -> plasmapause relative density change
;					pplhw -> plasmapause L-half width
;					pplcp -> plasmapause L-central position
;					drde -> duct relative density enhancement
;					dlhw -> duct L-half width
;					dlcp -> duct L-central position
;       aib -> alt of ionospheric base (km)
;				arl -> reference alt (above Earth's surface) for input density (km)
;				drl -> density at reference altitude (cm-3)
;					nH -> nH+/ne at 1000 km
;					nHe -> nHe+/ne at 1000 km
;					nO -> nO+/ne at 1000 km
;					temp -> temperature at 1000 km (K)
;				lat  -> magnetic lat (deg)
;				final_lat -> final latitude (deg)
;				longit -> SM magnetic long (deg)
;				alt -> initial altitude above surface (km)
;				final_alt -> final altitude (km)
;				freq -> frequency (Hz)
;				theta -> wave normal angle theta_kb (deg)
;				phi  -> wave azimuthal angle (deg)
;
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:   Can use the online International Reference Ionosphere to set up model at:
;			 http://modelweb.gsfc.nasa.gov/models/iri.html
;
;
;   CREATED:  07/22/2010
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-

;---------------------
;EXAMPLE OF WHAT TRACE_IN.TXT SHOULD LOOK like
;---------------------

;TRACE (ray tracing) input values
;___________________________________________________:_______|______|
;GEOMAG MODEL-COORDINATES (0:DIPOLE-MAG, 1:IGRF-GEO):0.00000
;IGRF OR DIPOLE INTERPOLATION DATE (IYEAR,IDOY)-----:   2005    209
;MAGNETIC-FIELD multiplier (dipole model)-----------:0.650000
;DENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):0
;--PLASMAPAUSE relative density change--------------:  0.600000E-01
;--PLASMAPAUSE  L-HALF WIDTH------------------------:0.500000
;--PLASMAPAUSE L-CENTRAL POSITION-------------------:3.00000
;--DUCT relative density enhancement----------------: -0.600000
;--DUCT L-HALF WIDTH--------------------------------:  0.500000
;--DUCT L-CENTRAL POSITION--------------------------:   6.65000
;ALTITUDE of the IONOSPHERIC BASE (km)--------------:   89.
;ALTITUDE of REFERENCE LEVEL (km)-------------------: 22195.
;DENSITY AT the REFERENCE LEVEL (cm^-3)-------------: 120.0
;--nH+/ne at 1000km---------------------------------:   120.000
;--nHe+/ne at 1000km--------------------------------:   1.00000
;--n0+/ne at 1000km---------------------------------:        0.
;--temperature at 1000km (K)------------------------:        0.
;Initial latitude (deg)-----------------------------:-0.930000
;Final latitude (deg)-------------------------------:-50.0000
;Initial longitude (deg)----------------------------:   80.0000
;Initial altitude (km)------------------------------:27964.3
;Final altitude (km)--------------------------------:4000.00
;Frequency (Hz)-------------------------------------:1800.00
;INITIAL VALUE OF THETA (deg)-----------------------:0.00000
;INITIAL VALUE OF Phi (deg)-------------------------:0.00000


;Theta_kb (wave normal angle) diagram
;Showing all the 45 deg angles
;									Bo
;       			  	|
;          			  |
;         +45     |   -45 (215)
;                 |
;Earth  --------------------- (equator)
;       					|
;       	 				|
;        +135		  |   -135 (225)
;     					  |


;MIRROR IMAGES (earthward) theta_kb in dipole field
;thetav = reverse((30+0)*indgen(nrayss)/(nrayss-1))-0
;thetav = 180 + ((-30+0)*indgen(nrayss)/(nrayss-1))-0

;MIRROR IMAGES (anti earthward) theta_kb in dipole field
;thetav = reverse((-30+0)*indgen(nrayss)/(nrayss-1))-0
;thetav = 180 + ((30+0)*indgen(nrayss)/(nrayss-1))-0



function read_write_trace_in,$
	bmodel=bmodel,$
	interp_date=interp_date,$
	mmult=mmult,$
	model=model,$
	ppdc=ppdc,$
	pplhw=pplhw,$
	pplcp=pplcp,$
	drde=drde,$
	dlhw=dlhw,$
	dlcp=dlcp,$
	aib=aib,$
	arl=arl,$
	drl=drl,$
	nHp=nHp,$
	nHe=nHe,$
	nO=nO,$
	temp=temp,$
	lat=lat,$
	final_lat=final_lat,$
	longit=longit,$
	alt=alt,$
	final_alt=final_alt,$
	freq=freq,$
	theta=theta,$
	phi=phi


	openr,lun,'~/Desktop/code/Aaron/github.umn.edu/raytrace/trace_in.txt', /get_lun
	trace_in= strarr(27)
	tmp = ''

	;read in trace_in.txt
	for i=0,26 do begin
		readf,lun,tmp
		trace_in[i] = tmp
	endfor

	close,lun
	free_lun,lun


	;Define all of the text file except for the values, which will come after the ":"
	t_init = $
	['TRACE (ray tracing) input values',$
	'___________________________________________________:_______|______|',$
	'GEOMAG MODEL-COORDINATES (0:DIPOLE-MAG, 1:IGRF-GEO):',$
	'IGRF OR DIPOLE INTERPOLATION DATE (IYEAR,IDOY)-----:',$
	'MAGNETIC-FIELD multiplier (dipole model)-----------:',$
	'DENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):',$
	'--PLASMAPAUSE relative density change--------------:',$
	'--PLASMAPAUSE  L-HALF WIDTH------------------------:',$
	'--PLASMAPAUSE L-CENTRAL POSITION-------------------:',$
	'--DUCT relative density enhancement----------------:',$
	'--DUCT L-HALF WIDTH--------------------------------:',$
	'--DUCT L-CENTRAL POSITION--------------------------:',$
	'ALTITUDE of the IONOSPHERIC BASE (km)--------------:',$
	'ALTITUDE of REFERENCE LEVEL (km)-------------------:',$
	'DENSITY AT the REFERENCE LEVEL (cm^-3)-------------:',$
	'--nH+/ne at 1000km---------------------------------:',$
	'--nHe+/ne at 1000km--------------------------------:',$
	'--n0+/ne at 1000km---------------------------------:',$
	'--temperature at 1000km (K)------------------------:',$
	'Initial latitude (deg)-----------------------------:',$
	'Final latitude (deg)-------------------------------:',$
	'Initial longitude (deg)----------------------------:',$
	'Initial altitude (km)------------------------------:',$
	'Final altitude (km)--------------------------------:',$
	'Frequency (Hz)-------------------------------------:',$
	'INITIAL VALUE OF THETA (deg)-----------------------:',$
	'INITIAL VALUE OF Phi (deg)-------------------------:']



	;Extract original values
	vals = strarr(27)
	for i=2,26 do vals[i] = strmid(trace_in[i],52)

	;Now populate the new array that will be saved as the new trace_in.txt
	trace_in2 = strarr(27)
	trace_in2[0] = t_init[0]
	trace_in2[1] = t_init[1]
	if keyword_set(bmodel) or n_elements(bmodel) eq 1 then trace_in2[2] = t_init[2] + strtrim(floor(float(bmodel)),2) else trace_in2[2] = t_init[2] + vals[2]
	if keyword_set(interp_date) or n_elements(interp_date) eq 1 then trace_in2[3] = t_init[3] + strtrim(float(interp_date),2) else trace_in2[3] = t_init[3] + vals[3]
	if keyword_set(mmult) or n_elements(mmult) eq 1 then trace_in2[4] = t_init[4] + strtrim(float(mmult),2) else trace_in2[4] = t_init[4] + vals[4]
	if keyword_set(model) or n_elements(model) eq 1 then trace_in2[5] = t_init[5] + strtrim(floor(float(model)),2) else trace_in2[5] = t_init[5] + vals[5]
	if keyword_set(ppdc) or n_elements(ppdc) eq 1 then trace_in2[6] = t_init[6] + strtrim(float(ppdc),2) else trace_in2[6] = t_init[6] + vals[6]
	if keyword_set(pplhw) or n_elements(pplhw) eq 1 then trace_in2[7] = t_init[7] + strtrim(float(pplhw),2) else trace_in2[7] = t_init[7] + vals[7]
	if keyword_set(pplcp) or n_elements(pplcp) eq 1 then trace_in2[8] = t_init[8] + strtrim(float(pplcp),2) else trace_in2[8] = t_init[8] + vals[8]
	if keyword_set(drde) or n_elements(drde) eq 1 then trace_in2[9] = t_init[9] + strtrim(float(drde),2) else trace_in2[9] = t_init[9] + vals[9]
	if keyword_set(dlhw) or n_elements(dlhw) eq 1 then trace_in2[10] = t_init[10] + strtrim(float(dlhw),2) else trace_in2[10] = t_init[10] + vals[10]
	if keyword_set(dlcp) or n_elements(dlcp) eq 1 then trace_in2[11] = t_init[11] + strtrim(float(dlcp),2) else trace_in2[11] = t_init[11] + vals[11]
	if keyword_set(aib) or n_elements(aib) eq 1 then trace_in2[12] = t_init[12] + strtrim(float(aib),2) else trace_in2[12] = t_init[12] + vals[12]
	if keyword_set(arl) or n_elements(arl) eq 1 then trace_in2[13] = t_init[13] + strtrim(float(arl),2) else trace_in2[13] = t_init[13] + vals[13]
	if keyword_set(drl) or n_elements(drl) eq 1 then trace_in2[14] = t_init[14] + strtrim(float(drl),2) else trace_in2[14] = t_init[14] + vals[14]
	if keyword_set(nHp) or n_elements(nHp) eq 1 then trace_in2[15] = t_init[15] + strtrim(float(nHp),2) else trace_in2[15] = t_init[15] + vals[15]
	if keyword_set(nHe) or n_elements(nHe) eq 1 then trace_in2[16] = t_init[16] + strtrim(float(nHe),2) else trace_in2[16] = t_init[16] + vals[16]
	if keyword_set(nO) or n_elements(nO) eq 1 then trace_in2[17] = t_init[17] + strtrim(float(nO),2) else trace_in2[17] = t_init[17] + vals[17]
	if keyword_set(temp) or n_elements(temp) eq 1 then trace_in2[18] = t_init[18] + strtrim(float(temp),2) else trace_in2[18] = t_init[18] + vals[18]
	if keyword_set(lat) or n_elements(lat) eq 1 then trace_in2[19] = t_init[19] + strtrim(float(lat),2) else trace_in2[19] = t_init[19] + vals[19]
	if keyword_set(final_lat) or n_elements(final_lat) eq 1 then trace_in2[20] = t_init[20] + strtrim(float(final_lat),2) else trace_in2[20] = t_init[20] + vals[20]
	if keyword_set(longit) or n_elements(longit) eq 1 then trace_in2[21] = t_init[21] + strtrim(float(longit),2) else trace_in2[21] = t_init[21] + vals[21]
	if keyword_set(alt) or n_elements(alt) eq 1 then trace_in2[22] = t_init[22] + strtrim(float(alt),2) else trace_in2[22] = t_init[22] + vals[22]
	if keyword_set(final_alt) or n_elements(final_alt) eq 1 then trace_in2[23] = t_init[23] + strtrim(float(final_alt),2) else trace_in2[23] = t_init[23] + vals[23]
	if keyword_set(freq) or n_elements(freq) eq 1 then trace_in2[24] = t_init[24] + strtrim(float(freq),2) else trace_in2[24] = t_init[24] + vals[24]
	if keyword_set(theta) or n_elements(theta) eq 1 then trace_in2[25] = t_init[25] + strtrim(float(theta),2) else trace_in2[25] = t_init[25] + vals[25]
	if keyword_set(phi) or n_elements(phi) eq 1 then trace_in2[26] = t_init[26] + strtrim(float(phi),2) else trace_in2[26] = t_init[26] + vals[26]


	;Write the new values to trace_in.txt
	OPENW,lun2,'~/Desktop/code/Aaron/github.umn.edu/raytrace/trace_intemp.txt',/GET_LUN

	cd,'~/Desktop/code/Aaron/github.umn.edu/raytrace',current=currdir
	spawn,'chmod 777 trace_intemp.txt'
	PRINTF,lun2,trace_in2
	CLOSE,lun2
	FREE_LUN,lun2
	spawn,'mv trace_intemp.txt trace_in.txt'
	cd,currdir

	;dummy struct
	struct = {bmodel:0,interp_date:'',mmult:!values.f_nan,model:0,ppdc:!values.f_nan,pplhw:!values.f_nan,pplcp:!values.f_nan,drde:!values.f_nan,$
	dlhw:!values.f_nan,dlcp:!values.f_nan,aib:!values.f_nan,arl:!values.f_nan,drl:!values.f_nan,nHp:!values.f_nan,nHe:!values.f_nan,nO:!values.f_nan,$
	temp:!values.f_nan,lat:!values.f_nan,final_lat:!values.f_nan,longit:!values.f_nan,alt:!values.f_nan,final_alt:!values.f_nan,$
	freq:!values.f_nan,theta:!values.f_nan,phi:!values.f_nan}


	;Returned values for structure
	if keyword_set(bmodel) or n_elements(bmodel) eq 1 then struct.bmodel = bmodel else struct.bmodel = floor(float(strmid(trace_in[2],52,100)))
	if keyword_set(interp_date) or n_elements(interp_date) eq 1 then struct.interp_date = interp_date else struct.interp_date = strmid(trace_in[3],52,100)
	if keyword_set(mmult) or n_elements(mmult) eq 1 then struct.mmult = mmult else struct.mmult = float(strmid(trace_in[4],52,100))
	if keyword_set(model) or n_elements(model) eq 1 then struct.model = model else struct.model = floor(float(strmid(trace_in[5],52,100)))
	if keyword_set(ppdc) or n_elements(ppdc) eq 1 then struct.ppdc = ppdc else struct.ppdc = float(strmid(trace_in[6],52,100))
	if keyword_set(pplhw) or n_elements(pplhw) eq 1 then struct.pplhw = pplhw else struct.pplhw = float(strmid(trace_in[7],52,100))
	if keyword_set(pplcp) or n_elements(pplcp) eq 1 then struct.pplcp = pplcp else struct.pplcp = float(strmid(trace_in[8],52,100))
	if keyword_set(drde) or n_elements(drde) eq 1 then struct.drde = drde else struct.drde = float(strmid(trace_in[9],52,100))
	if keyword_set(dlhw) or n_elements(dlhw) eq 1 then struct.dlhw = dlhw else struct.dlhw = float(strmid(trace_in[10],52,100))
	if keyword_set(dlcp) or n_elements(dlcp) eq 1 then struct.dlcp = dlcp else struct.dlcp = float(strmid(trace_in[11],52,100))
	if keyword_set(aib) or n_elements(aib) eq 1 then struct.aib = aib else struct.aib = float(strmid(trace_in[12],52,100))
	if keyword_set(arl) or n_elements(arl) eq 1 then struct.arl = arl else struct.arl = float(strmid(trace_in[13],52,100))
	if keyword_set(drl) or n_elements(drl) eq 1 then struct.drl = drl else struct.drl = float(strmid(trace_in[14],52,100))
	if keyword_set(nHp) or n_elements(nHp) eq 1 then struct.nHp = nHp else struct.nHp = float(strmid(trace_in[15],52,100))
	if keyword_set(nHe) or n_elements(nHe) eq 1 then struct.nHe = nHe else struct.nHe = float(strmid(trace_in[16],52,100))
	if keyword_set(nO) or n_elements(nO) eq 1 then struct.nO = nO else struct.nO = float(strmid(trace_in[17],52,100))
	if keyword_set(temp) or n_elements(temp) eq 1 then struct.temp = temp else struct.temp = float(strmid(trace_in[18],52,100))
	if keyword_set(lat) or n_elements(lat) eq 1 then struct.lat = lat else struct.lat = float(strmid(trace_in[19],52,100))
	if keyword_set(final_lat) or n_elements(final_lat) eq 1 then struct.final_lat = final_lat else struct.final_lat = float(strmid(trace_in[20],52,100))
	if keyword_set(longit) or n_elements(longit) eq 1 then struct.longit = longit else struct.longit = float(strmid(trace_in[21],52,100))
	if keyword_set(alt) or n_elements(alt) eq 1 then struct.alt = alt else struct.alt = float(strmid(trace_in[22],52,100))
	if keyword_set(final_alt) or n_elements(final_alt) eq 1 then struct.final_alt = final_alt else struct.final_alt = float(strmid(trace_in[23],52,100))
	if keyword_set(freq) or n_elements(freq) eq 1 then struct.freq = freq else struct.freq = float(strmid(trace_in[24],52,100))
	if keyword_set(theta) or n_elements(theta) eq 1 then struct.theta = theta else struct.theta = float(strmid(trace_in[25],52,100))
	if keyword_set(phi) or n_elements(phi) eq 1 then struct.phi = phi else struct.phi = float(strmid(trace_in[26],52,100))

	return,struct
end
