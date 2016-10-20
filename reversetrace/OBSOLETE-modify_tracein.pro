;+
;*****************************************************************************************
;
;  PROCEDURE : modify_tracein
;  PURPOSE  :  modifies the file trace_in.txt (input file for trace.for) by changing 
;				values to the input keywords 
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
;  KEYWORDS:    freq -> frequency (Hz)
;				lat  -> SM lat (deg)
;				longit -> SM long (deg)
;				theta_kb -> initial latitude (deg)
;				phi  -> initial longitude (deg)
;               final_alt -> final altitude (km)
;				final_lat -> final latitude (deg)
;				mmult -> magnetic multiplier
;				model -> density model used
;				ppdc -> plasmapause relative density change
;				pplhw -> plasmapause L-half width
;				pplcp -> plasmapause L-central position
;				drde -> duct relative density enhancement
;				dlhw -> duct L-half width
;				dlcp -> duct L-central position
;				nH -> nH+/ne at 1000 km
;				nHe -> nHe+/ne at 1000 km
;				nO -> nO+/ne at 1000 km
;				temp -> temperature at 1000 km (K)
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:      
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



Pro modify_tracein,freq=freq,lat=lat,alt=alt,longit=longit,theta_kb=theta_kb,phi=phi,$
	final_alt=final_alt,final_lat=final_lat,mmult=mmult,model=model,ppdc=ppdc,pplhw=pplhw,$
	pplcp=pplcp,drde=drde,dlhw=dlhw,dlcp=dlcp,arl=arl,drl=drl,nH=nH,nHe=nHe,nO=nO,temp=temp


openr,lun,'trace_in.txt', /get_lun
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
if keyword_set(mmult) then trace_in[2] = strmid(trace_in[2],0,51) + ' ' + strtrim(mmult,2)
if keyword_set(model) then trace_in[3] = strmid(trace_in[3],0,51) + ' ' + strtrim(model,2)
if keyword_set(ppdc) then trace_in[4] = strmid(trace_in[4],0,51) + ' ' + strtrim(ppdc,2)
if keyword_set(pplhw) then trace_in[5] = strmid(trace_in[5],0,51) + ' ' + strtrim(pplhw,2)
if keyword_set(pplcp) then trace_in[6] = strmid(trace_in[6],0,51) + ' ' + strtrim(pplcp,2)
if keyword_set(drde) then trace_in[7] = strmid(trace_in[7],0,51) + ' ' + strtrim(drde,2)
if keyword_set(dlhw) then trace_in[8] = strmid(trace_in[8],0,51) + ' ' + strtrim(dlhw,2)
if keyword_set(dlcp) then trace_in[9] = strmid(trace_in[9],0,51) + ' ' + strtrim(dlcp,2)
if keyword_set(arl) then trace_in[10] = strmid(trace_in[10],0,51) + ' ' + strtrim(arl,2)
if keyword_set(drl) then trace_in[11] = strmid(trace_in[11],0,51) + ' ' + strtrim(drl,2)
if keyword_set(nH) then trace_in[12] = strmid(trace_in[12],0,51) + ' ' + strtrim(nH,2)
if keyword_set(nHe) then trace_in[13] = strmid(trace_in[13],0,51) + ' ' + strtrim(nHe,2)
if keyword_set(nO) then trace_in[14] = strmid(trace_in[14],0,51) + ' ' + strtrim(nO,2)
if keyword_set(temp) then trace_in[15] = strmid(trace_in[15],0,51) + ' ' + strtrim(temp,2)
if keyword_set(lat) then trace_in[16] = strmid(trace_in[16],0,51) + ' ' + strtrim(lat,2)
if keyword_set(final_lat) then trace_in[17] = strmid(trace_in[17],0,51) + ' ' + strtrim(final_lat,2)
if keyword_set(longit) then trace_in[18] = strmid(trace_in[18],0,51) + ' ' + strtrim(longit,2)
if keyword_set(alt) then trace_in[19] = strmid(trace_in[19],0,51) + ' ' + strtrim(alt,2)
if keyword_set(final_alt) then trace_in[20] = strmid(trace_in[20],0,51) + ' ' + strtrim(final_alt,2)
if keyword_set(freq) then trace_in[21] = strmid(trace_in[21],0,51) + ' ' + strtrim(freq,2)
if keyword_set(theta) then trace_in[22] = strmid(trace_in[22],0,51) + ' ' + strtrim(theta,2)
if keyword_set(phi) then trace_in[23] = strmid(trace_in[23],0,51) + ' ' + strtrim(phi,2)


OPENW,lun2,'trace_intemp.txt',/GET_LUN
spawn,'chmod 777 trace_intemp.txt'
PRINTF,lun2,trace_in
CLOSE,lun2
FREE_LUN,lun2
spawn,'mv trace_intemp.txt trace_in.txt'
end
