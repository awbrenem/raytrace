;+
;*****************************************************************************************
;
;  FUNCTION : read_trace_ta  
;  PURPOSE  : reads in output file from trace.for (trace_ta.txt by default) and returns
;			  structure of ray values
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
;  KEYWORDS:    
;               
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:      
;               
;
;   CREATED:  07/23/2010
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-

;FORMATTING FROM TRACE.FOR


;      WRITE(LUT,605) ident_dens_region,
;     .  Y(7),Y(8),reltstep,relsstep,wavelength,
;     .  Y(1),ALT,FID,LOND,ELE,FGF,FGF*1836.,
;     .  DENS,pH,pHe,pO,FLHR,EMU, akvert,akazi,THD,PSI,PHI,THG,THR
;605   FORMAT(I2,5G10.3,F9.5,F8.0,2F9.4,G10.3,2f10.3,F16.2,3F6.1,
;     &F8.3,G10.3,7F6.1)


function read_trace_ta,file=file

if ~keyword_set(file) then file = 'trace_ta.txt'


;tmp = {version:1.,$
;		datastart:2L,$
;		delimiter:32B,$
;		missingvalue:!values.f_nan,$
;		commentsymbol:'',$
;		fieldcount:26L,$
;		fieldtypes:[3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4],$
;		fieldnames:['Reg','Path','TimeG','per_dTimeG_T','per_dPath_WL','WL','R','Alt','Lat','Long',$
;					'L','f_fce','f_fcH','dens','per_Hplus','per_Heplus','per_Oplus','flhr','n',$
;					'VeK','AzK','ThK','ThG','PhK','Gen','Res'],$
;		fieldlocations:[1,4,14,24,34,44,54,63,72,81,89,102,110,121,128,136,142,148,155,165,172,177,184,190,195,201],$
;		fieldgroups:indgen(26)}

notes = ['Reg -- indentification of the density region',$
 '            0.. Diffusive equilibrium model',$
 '           -1.. Plasmapause gradient (1/2 period sine localized between',$
 '                  AL-DDK and AL+DDK)',$
 '           -2.. Duct (exp model - 0.1% density change detected)',$
 '           -3.. ionosphere below 390 km (.99*PP  decrease detected,',$
 '                  alt0=89km, scale=140km)',$
 'Path         = integrated ray path in RE',$
 'TimeG        = integrated group delay in sec',$
 'per_dTimeG_T = group time step to wave period *100%',$
 'per_dPath_WL = ray path step to wavelength *100%',$
 'WL           = wave length in km',$
 'R            = radial distance (from the centre of the Earth) in Earth radii',$
 'Alt          = altitude above the surface of the Earth',$
 'Lat          = magnetic dipole (e.g., SM coordinates) latitude in degrees',$
 'Long         = magnetic longitude in degrees',$
 'L            = McIlwain L parameter',$
 'f_fce        = wave frequency to the electron cyclotron frequency',$
 'f_fcH        = wave frequency to the  proton cyclotron frequency',$
 'dens         = plasma number density in particles per cc',$
 'per_Hplus    = percentage of H+ in the ion component',$
 'per_Heplus   = percentage of He+',$
 'per_Oplus    = percentage of O+',$
 'flhr         = lower hybrid frequency in kHz',$
 'n            = refractive index n=|k|c/(2 pi f) k wave vector, f frequency',$
 'VeK          = angle in degrees between the radial direction and k.',$
 '                 0 - k points outward, 180 - toward the Earth',$
 'AzK          = azimuth in degrees of k around the radial.',$
 '                 0  - if k is in the magnetic',$
 '                    meridian directed toward geographic N (growing latitude)',$
 '                 90 - toward W, 180- toward S, 270 - toward E, 360=0',$
 'ThK          = angle in degrees between the magnetic field B0 direction and k.',$
 '                 0 - parallel 180 - antraparallel',$
 'ThG          = angle in degrees between the magnetic field B0 direction and the',$
 '                 group velocity (ray tangential direction) -',$
 '                 recalculated, approximation (not directly used in the ray tracing).',$
 'PhK          = azimuth of k around the the magnetic field B0 direction.',$
 '                 0 - k is in the the magnetic meridian plane, directed outward',$
 '                 (toward higher L values, toward N for latitudes >0)',$
 '                 90 - in the right-hand sense, i.e. toward E.',$
 '                 270 - toward W',$
 'Gen          = Aprox. Gendrin angle for the whistler mode (ThK for ThG=0) in',$
 '                 degrees.',$
 'Res          = Approx. oblique resonance angle for the whistler mode'$
 ]


; Select a text file and open for reading
openr,lun1,file,/get_lun
openr,lun2,file,/get_lun

array = replicate(0d,26)
line = replicate(0d,26)
junk = ''
for i=0,1 do readf,lun1,junk 
for i=0,1 do readf,lun2,junk
test = ''
while test ne 'STOP' do begin & $
  readf,lun2,test & $
  if strmid(strtrim(test,2),0,4) eq 'STOP' then test = 'STOP' & $
  if test ne 'STOP' then readf,lun1,line,format='(I2,5G10.3,F9.5,F8.0,2F9.4,G10.3,2f10.3,F16.2,3F6.1,F8.3,G10.3,7F6.1)' & $  
  if test ne 'STOP' then array = [[array],[line]] & $
endwhile
free_lun,lun1
free_lun,lun2

array = array[*,1:n_elements(array[0,*])-1]

str = {Reg:reform(array[0,*]),Path:reform(array[1,*]),TimeG:reform(array[2,*]),per_dTimeG_T:reform(array[3,*]),per_dPath_WL:reform(array[4,*]),$
	WL:reform(array[5,*]),R:reform(array[6,*]),Alt:reform(array[7,*]),Lat:reform(array[8,*]),Long:reform(array[9,*]),L:reform(array[10,*]),f_fce:reform(array[11,*]),$
	f_fcH:reform(array[12,*]),dens:reform(array[13,*]),per_Hplus:reform(array[14,*]),per_Heplus:reform(array[15,*]),per_Oplus:reform(array[16,*]),$
	flhr:reform(array[17,*]),n:reform(array[18,*]),VeK:reform(array[19,*]),AzK:reform(array[20,*]),ThK:reform(array[21,*]),ThG:reform(array[22,*]),PhK:reform(array[23,*]),$
	Gen:reform(array[24,*]),Res:reform(array[25,*])}


;Create rectangular coord positional vars
xcoord = str.R*sin((90.-str.lat)*!dtor)*cos(str.long*!dtor)
ycoord = str.R*sin((90.-str.lat)*!dtor)*sin(str.long*!dtor)
zcoord = str.R*cos((90.-str.lat)*!dtor)

;Create rectangular coord k-vector vars
kx = cos(vek[*,0]*!dtor)                       ;analogous to usual z-hat component
ky = sin(vek[*,0]*!dtor)*sin(azk[*,0]*!dtor)   ;analogous to usual y-hat component
kz = sin(vek[*,0]*!dtor)*cos(azk[*,0]*!dtor)   ;analogous to usual x-hat component


str_element,str,'notes',notes,/ADD_REPLACE
str_element,str,'xcoord',xcoord,/ADD_REPLACE
str_element,str,'ycoord',ycoord,/ADD_REPLACE
str_element,str,'zcoord',zcoord,/ADD_REPLACE
str_element,str,'kx',kx,/ADD_REPLACE
str_element,str,'ky',ky,/ADD_REPLACE
str_element,str,'kz',kz,/ADD_REPLACE

return,str
end




