;+
;*****************************************************************************************
;
;  FUNCTION : read_write_trace_in
;  PURPOSE  : reads the file trace_in.txt (input file for trace.for) and returns its values
;			  and also can change these values
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
;				mmult -> magnetic multiplier
;				model -> density model used
;					ppdc -> plasmapause relative density change
;					pplhw -> plasmapause L-half width
;					pplcp -> plasmapause L-central position
;					drde -> duct relative density enhancement
;					dlhw -> duct L-half width
;					dlcp -> duct L-central position
;				alt -> altitude above Earth's surface (km)
;               final_alt -> final altitude (km)
;				drl -> density at reference altitude (cm-3)
;					nH -> nH+/ne at 1000 km
;					nHe -> nHe+/ne at 1000 km
;					nO -> nO+/ne at 1000 km
;					temp -> temperature at 1000 km (K)
;				lat  -> SM lat (deg)
;				final_lat -> final latitude (deg)
;				longit -> SM long (deg)
;				alt -> initial altitude (km)
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

;TRACE (ray tracing) input values
;___________________________________________________:______________
;MAGNETIC-FIELD multiplier (dipole model):           1.2
;DENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):             0
;  PLASMAPAUSE relative density change:              0.50000
;  PLASMAPAUSE  L-HALF WIDTH:                        0.10000
;  PLASMAPAUSE L-CENTRAL POSITION:                   4.00
;  DUCT relative density enhancement:                      0.0
;  DUCT L-HALF WIDTH:                                      0.5
;  DUCT L-CENTRAL POSITION:                                8.65
;ALTITUDE of REFERENCE LEVEL (km):                        1000.
;DENSITY AT the REFERENCE LEVEL (cm^-3):             47000.0
; nH+/ne at 1000km:                                       0.05
; nHe+/ne at 1000km:                                      0.3
; n0+/ne at 1000km:                                       0.65
; temperature at 1000km (K):                              650.00
;Initial latitude (deg):                             0.7428
;Final latitude (deg):                               80.0000
;Initial longitude (deg):                                 0.
;Initial altitude (km):                              10000.3
;Final altitude (km):                                2000.0
;Frequency (Hz):                                     5000.
;INITIAL VALUE OF THETA (deg):                       20.
;INITIAL VALUE OF Phi (deg):                         0.00000



function read_write_trace_in,freq=freq,lat=lat,alt=alt,longit=longit,theta=theta,phi=phi,$
	final_alt=final_alt,final_lat=final_lat,mmult=mmult,model=model,ppdc=ppdc,pplhw=pplhw,$
	pplcp=pplcp,drde=drde,dlhw=dlhw,dlcp=dlcp,arl=arl,drl=drl,nHp=nHp,nHe=nHe,nO=nO,temp=temp


openr,lun,'~/Desktop/code/Aaron/github.umn.edu/raytrace/trace_in.txt', /get_lun
trace_in= strarr(24)
tmp = ''

;read in trace_in.txt
for i=0,23 do begin
     readf,lun,tmp
     trace_in[i] = tmp
endfor

close,lun
free_lun,lun

;now entire file trace_in.txt has been read in, lets change the appropriate values
if keyword_set(mmult) or n_elements(mmult) eq 1 then trace_in[2] = strmid(trace_in[2],0,51) + ' ' + strtrim(float(mmult),2)
if keyword_set(model) or n_elements(model) eq 1 then trace_in[3] = strmid(trace_in[3],0,51) + ' ' + strtrim(long(model),2)
if keyword_set(ppdc) or n_elements(ppdc) eq 1 then trace_in[4] = strmid(trace_in[4],0,51) + ' ' + strtrim(float(ppdc),2)
if keyword_set(pplhw) or n_elements(pplhw) eq 1 then trace_in[5] = strmid(trace_in[5],0,51) + ' ' + strtrim(float(pplhw),2)
if keyword_set(pplcp) or n_elements(pplcp) eq 1 then trace_in[6] = strmid(trace_in[6],0,51) + ' ' + strtrim(float(pplcp),2)
if keyword_set(drde) or n_elements(drde) eq 1 then trace_in[7] = strmid(trace_in[7],0,51) + ' ' + strtrim(float(drde),2)
if keyword_set(dlhw) or n_elements(dlhw) eq 1 then trace_in[8] = strmid(trace_in[8],0,51) + ' ' + strtrim(float(dlhw),2)
if keyword_set(dlcp) or n_elements(dlcp) eq 1 then trace_in[9] = strmid(trace_in[9],0,51) + ' ' + strtrim(float(dlcp),2)
if keyword_set(arl) or n_elements(arl) eq 1 then trace_in[10] = strmid(trace_in[10],0,51) + ' ' + strtrim(float(arl),2)
if keyword_set(drl) or n_elements(drl) eq 1 then trace_in[11] = strmid(trace_in[11],0,51) + ' ' + strtrim(float(drl),2)
if keyword_set(nHp) or n_elements(nHp) eq 1 then trace_in[12] = strmid(trace_in[12],0,51) + ' ' + strtrim(float(nHp),2)
if keyword_set(nHe) or n_elements(nHe) eq 1 then trace_in[13] = strmid(trace_in[13],0,51) + ' ' + strtrim(float(nHe),2)
if keyword_set(nO) or n_elements(nO) eq 1 then trace_in[14] = strmid(trace_in[14],0,51) + ' ' + strtrim(float(nO),2)
if keyword_set(temp) or n_elements(temp) eq 1 then trace_in[15] = strmid(trace_in[15],0,51) + ' ' + strtrim(float(temp),2)
if keyword_set(lat) or n_elements(lat) eq 1 then trace_in[16] = strmid(trace_in[16],0,51) + ' ' + strtrim(float(lat),2)
if keyword_set(final_lat) or n_elements(final_lat) eq 1 then trace_in[17] = strmid(trace_in[17],0,51) + ' ' + strtrim(float(final_lat),2)
if keyword_set(longit) or n_elements(longit) eq 1 then trace_in[18] = strmid(trace_in[18],0,51) + ' ' + strtrim(float(longit),2)
if keyword_set(alt) or n_elements(alt) eq 1 then trace_in[19] = strmid(trace_in[19],0,51) + ' ' + strtrim(float(alt),2)
if keyword_set(final_alt) or n_elements(final_alt) eq 1 then trace_in[20] = strmid(trace_in[20],0,51) + ' ' + strtrim(float(final_alt),2)
if keyword_set(freq) or n_elements(freq) eq 1 then trace_in[21] = strmid(trace_in[21],0,51) + ' ' + strtrim(float(freq),2)
if keyword_set(theta) or n_elements(theta) eq 1 then trace_in[22] = strmid(trace_in[22],0,51) + ' ' + strtrim(float(theta),2)
if keyword_set(phi) or n_elements(phi) eq 1 then trace_in[23] = strmid(trace_in[23],0,51) + ' ' + strtrim(float(phi),2)


OPENW,lun2,'trace_intemp.txt',/GET_LUN
spawn,'chmod 777 trace_intemp.txt'
PRINTF,lun2,trace_in
CLOSE,lun2
FREE_LUN,lun2
spawn,'mv trace_intemp.txt trace_in.txt'

;dummy struct
struct = {mmult:!values.f_nan,model:!values.f_nan,ppdc:!values.f_nan,pplhw:!values.f_nan,pplcp:!values.f_nan,drde:!values.f_nan,$
	dlhw:!values.f_nan,dlcp:!values.f_nan,arl:!values.f_nan,drl:!values.f_nan,nHp:!values.f_nan,nHe:!values.f_nan,nO:!values.f_nan,$
	temp:!values.f_nan,lat:!values.f_nan,final_lat:!values.f_nan,longit:!values.f_nan,alt:!values.f_nan,final_alt:!values.f_nan,$
	freq:!values.f_nan,theta:!values.f_nan,phi:!values.f_nan}

if keyword_set(mmult) or n_elements(mmult) eq 1 then struct.mmult = mmult else struct.mmult = float(strmid(trace_in[2],52,100))
if keyword_set(model) or n_elements(model) eq 1 then struct.model = model else struct.model = long(strmid(trace_in[3],52,100))
if keyword_set(ppdc) or n_elements(ppdc) eq 1 then struct.ppdc = ppdc else struct.ppdc = float(strmid(trace_in[4],52,100))
if keyword_set(pplhw) or n_elements(pplhw) eq 1 then struct.pplhw = pplhw else struct.pplhw = float(strmid(trace_in[5],52,100))
if keyword_set(pplcp) or n_elements(pplcp) eq 1 then struct.pplcp = pplcp else struct.pplcp = float(strmid(trace_in[6],52,100))
if keyword_set(drde) or n_elements(drde) eq 1 then struct.drde = drde else struct.drde = float(strmid(trace_in[7],52,100))
if keyword_set(dlhw) or n_elements(dlhw) eq 1 then struct.dlhw = dlhw else struct.dlhw = float(strmid(trace_in[8],52,100))
if keyword_set(dlcp) or n_elements(dlcp) eq 1 then struct.dlcp = dlcp else struct.dlcp = float(strmid(trace_in[9],52,100))
if keyword_set(arl) or n_elements(arl) eq 1 then struct.arl = arl else struct.arl = float(strmid(trace_in[10],52,100))
if keyword_set(drl) or n_elements(drl) eq 1 then struct.drl = drl else struct.drl = float(strmid(trace_in[11],52,100))
if keyword_set(nHp) or n_elements(nHp) eq 1 then struct.nHp = nHp else struct.nHp = float(strmid(trace_in[12],52,100))
if keyword_set(nHe) or n_elements(nHe) eq 1 then struct.nHe = nHe else struct.nHe = float(strmid(trace_in[13],52,100))
if keyword_set(nO) or n_elements(nO) eq 1 then struct.nO = nO else struct.nO = float(strmid(trace_in[14],52,100))
if keyword_set(temp) or n_elements(temp) eq 1 then struct.temp = temp else struct.temp = float(strmid(trace_in[15],52,100))
if keyword_set(lat) or n_elements(lat) eq 1 then struct.lat = lat else struct.lat = float(strmid(trace_in[16],52,100))
if keyword_set(final_lat) or n_elements(final_lat) eq 1 then struct.final_lat = final_lat else struct.final_lat = float(strmid(trace_in[17],52,100))
if keyword_set(longit) or n_elements(longit) eq 1 then struct.longit = longit else struct.longit = float(strmid(trace_in[18],52,100))
if keyword_set(alt) or n_elements(alt) eq 1 then struct.alt = alt else struct.alt = float(strmid(trace_in[19],52,100))
if keyword_set(final_alt) or n_elements(final_alt) eq 1 then struct.final_alt = final_alt else struct.final_alt = float(strmid(trace_in[20],52,100))
if keyword_set(freq) or n_elements(freq) eq 1 then struct.freq = freq else struct.freq = float(strmid(trace_in[21],52,100))
if keyword_set(theta) or n_elements(theta) eq 1 then struct.theta = theta else struct.theta = float(strmid(trace_in[22],52,100))
if keyword_set(phi) or n_elements(phi) eq 1 then struct.phi = phi else struct.phi = float(strmid(trace_in[23],52,100))

return,struct
end
