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
;   NOTES: Automatically determines of the IGRF model is used or not. If so,
;          both GEO coord and SM coord are returned. Else only SM coord
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
;
;
;      WRITE(LUT,605) ident_dens_region,
;     .  Y(7),Y(8),reltstep,relsstep,wavelength,
;     .  Y(1),ALT,FID,LOND,ELE,FGF,FGF*1836.,
;     .  DENS,pH,pHe,pO,FLHR,EMU, akvert,akazi,THD,PSI,PHI,THG,THR
;
;
;format='(I2,5G10.3,F9.5,F8.0,2F9.4,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1)'
;
;
;_____________________
;Key for trace.for
;_____________________
;Region = ident_dens_region
;Path = Y(7)
;TimeG = Y(8)
;per_dTimeG_T = reltstep
;per_dPath_WL = relsstep
;WL = wavelength
;RE = Y(1)
;Altitude = ALT
;magnetic latitude = FID
;magnetic longitude = LOND
;McIlwain L = ELE
;f/fce = FGF
;f/fpH = FGF*1836.
;density = DENS
;percentage H+ = pH
;percentage He+ = pHe
;percentage O+ = pO
;flhr = FLHR
;refractive index = EMU
;Vek = akvert
;Azk = akazi
;theta_kb = THD
;theta_group = PSI
;Phk = PHI
;Gendrin angle = THG
;Resonance cone angle = THR


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




function read_trace_ta,file=file,geotime=geotime


  if ~keyword_set(file) then file = '~/Desktop/code/Aaron/github.umn.edu/raytrace/trace_ta.txt'


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
  'Lat          = magnetic lat (e.g. SM coord) for dipole coord, or geographic lat for IGRF model',$
  'Long         = magnetic long (Deg) for dipole coord, or geographic long for IGRF model',$
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


  ;Find out if the IGRF model is used or not. It returns geographic coord, whereas
  ;the dipole model returns magnetic coord.
  tmp = read_write_trace_in()
  if tmp.bmodel eq 1 then geocoord = 1 else geocoord = 0

  ;make sure we have a reference time for the GEO to SM conversion
  if geocoord and ~KEYWORD_SET(geotime) then begin
      print,'*********************'
      print,'*********************'
      print,'*********************'
      print,'NEED TO INPUT A TIME (GEOTIME) FOR THE TRANSFORMATION FROM GEOGRAPHIC COORD TO SM COORD'
      print,'....returning'
      print,'*********************'
      print,'*********************'
      print,'*********************'
      return,-1
  endif


  ; Select a text file and open for reading
  openr,lun1,file,/get_lun
  openr,lun2,file,/get_lun

  array = replicate(0d,26)
  line = replicate(0d,26)
  junk = ''
  for i=0,1 do readf,lun1,junk
  for i=0,1 do readf,lun2,junk
  test = ''

  while test ne 'STOP' do begin
    readf,lun2,test
    if strmid(strtrim(test,2),0,4) eq 'STOP' then test = 'STOP'
    if test ne 'STOP' then readf,lun1,line,$
    format='(I2,5G10.3,F9.5,F8.0,2F9.4,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1)'
    if test ne 'STOP' then array = [[array],[line]]
  endwhile
  free_lun,lun1
  free_lun,lun2

  array = array[*,1:n_elements(array[0,*])-1]


  ;convert geographic values to SM values, if necessary
  if geocoord then begin


    tmin = time_double(geotime)
    tmax = tmin + 1.
    dt = tmax-tmin
    nelem = n_elements(array[0,*])
    times = dt*indgen(nelem)/(nelem-1) + tmin

    latGEO = reform(array[8,*])
    longGEO = reform(array[9,*])
    radius = reform(array[7,*]) + 6370.

    xcoordGEO = radius*sin((90.-latGEO)*!dtor)*cos(longGEO*!dtor)
    ycoordGEO = radius*sin((90.-latGEO)*!dtor)*sin(longGEO*!dtor)
    zcoordGEO = radius*cos((90.-latGEO)*!dtor)


    store_data,'geocoordtmp',times,[[xcoordGEO],[ycoordGEO],[zcoordGEO]]
    cotrans,'geocoordtmp','geicoordtmp',/geo2gei
    cotrans,'geicoordtmp','gsecoordtmp',/gei2gse
    cotrans,'gsecoordtmp','gsmcoordtmp',/gse2gsm
    cotrans,'gsmcoordtmp','smcoordtmp',/gsm2sm

    get_data,'smcoordtmp',times,vals
    xcoordSM = vals[*,0]
    ycoordSM = vals[*,1]
    zcoordSM = vals[*,2]

    ;clean up...
    store_data,['geocoordtmp','geicoordtmp','gsecoordtmp','gsmcoordtmp','smcoordtmp'],/delete

    longSM = atan(ycoordSM/xcoordSM)/!dtor
    latSM = asin(zcoordSM/radius)/!dtor


  endif else begin
    latSM = reform(array[8,*])
    longSM = reform(array[9,*])
    Lvalue = reform(array[10,*])
    Rval = reform(array[6,*])
    xcoordSM = Rval*cos(latSM*!dtor)*cos(longSM*!dtor)
    ycoordSM = Rval*cos(latSM*!dtor)*sin(longSM*!dtor)
    zcoordSM = Rval*sin(latSM*!dtor)
  endelse

  if geocoord then begin
    str = {Reg:reform(array[0,*]),$
    Path:reform(array[1,*]),$
    TimeG:reform(array[2,*]),$
    per_dTimeG_T:reform(array[3,*]),$
    per_dPath_WL:reform(array[4,*]),$
    WL:reform(array[5,*]),$
    R:reform(array[6,*]),$
    Alt:reform(array[7,*]),$
    LatGEO:latGEO,$
    LatSM:latSM,$
    LongGEO:longGEO,$
    LongSM:longSM,$
    L:reform(array[10,*]),$
    f_fce:reform(array[11,*]),$
    f_fcH:reform(array[12,*]),$
    dens:reform(array[13,*]),$
    per_Hplus:reform(array[14,*]),$
    per_Heplus:reform(array[15,*]),$
    per_Oplus:reform(array[16,*]),$
    flhr:reform(array[17,*]),$
    n:reform(array[18,*]),$
    VeK:reform(array[19,*]),$
    AzK:reform(array[20,*]),$
    ThK:reform(array[21,*]),$
    ThG:reform(array[22,*]),$
    PhK:reform(array[23,*]),$
    Gen:reform(array[24,*]),$
    Res:reform(array[25,*])}
  endif else begin
    str = {Reg:reform(array[0,*]),$
    Path:reform(array[1,*]),$
    TimeG:reform(array[2,*]),$
    per_dTimeG_T:reform(array[3,*]),$
    per_dPath_WL:reform(array[4,*]),$
    WL:reform(array[5,*]),$
    R:reform(array[6,*]),$
    Alt:reform(array[7,*]),$
    LatSM:reform(array[8,*]),$
    LongSM:reform(array[9,*]),$
    L:reform(array[10,*]),$
    f_fce:reform(array[11,*]),$
    f_fcH:reform(array[12,*]),$
    dens:reform(array[13,*]),$
    per_Hplus:reform(array[14,*]),$
    per_Heplus:reform(array[15,*]),$
    per_Oplus:reform(array[16,*]),$
    flhr:reform(array[17,*]),$
    n:reform(array[18,*]),$
    VeK:reform(array[19,*]),$
    AzK:reform(array[20,*]),$
    ThK:reform(array[21,*]),$
    ThG:reform(array[22,*]),$
    PhK:reform(array[23,*]),$
    Gen:reform(array[24,*]),$
    Res:reform(array[25,*])}
  endelse

  ;Create rectangular coord positional vars
  ;These will be in geographic coord for the IGRF model
  ;if geocoord then begin
  ;  xcoordGEO = str.R*sin((90-str.latGEO)*!dtor)*cos((str.longGEO)*!dtor)
  ;  ycoordGEO = str.R*sin((90-str.latGEO)*!dtor)*sin((str.longGEO)*!dtor)
  ;  zcoordGEO = str.R*cos((90-str.latGEO)*!dtor)
  ;endif

  ;xcoordSM = str.R*sin((90-str.latSM)*!dtor)*cos((str.longSM)*!dtor)
  ;ycoordSM = str.R*sin((90-str.latSM)*!dtor)*sin((str.longSM)*!dtor)
  ;zcoordSM = str.R*cos((90-str.latSM)*!dtor)



  ;  ;Rectangular wave vectors
  ;  ;first find projection onto radial (same as spherical coord with x=z,z=x,y=y)
  ;  kx1 = cos(str.vek*!dtor)
  ;  kz1 = sin(str.vek*!dtor)*cos(str.azk*!dtor)
  ;  ky1 = sin(str.vek*!dtor)*sin(str.azk*!dtor)


  ;  ;rotate this by the magnetic latitude
  ;  kx = kx1*cos(str.lat*!dtor) - kz1*sin(str.lat*!dtor)
  ;  kz = kx1*sin(str.lat*!dtor) + kz1*cos(str.lat*!dtor)
  ;  ky = ky1



  vgroup = fltarr(n_elements(xcoordSM))
  ;Calculate group velocity (km/sec)
  for jj=1,n_elements(xcoordSM)-1 do $
  vgroup[jj] = 6370.*(float(str.path[jj])-float(str.path[jj-1]))/(float(str.timeG[jj])-float(str.timeG[jj-1]))
  vgroup[0] = !values.f_nan



  str_element,str,'notes',notes,/ADD_REPLACE
  if geocoord then begin
    str_element,str,'xcoordGEO',xcoordGEO,/ADD_REPLACE
    str_element,str,'ycoordGEO',ycoordGEO,/ADD_REPLACE
    str_element,str,'zcoordGEO',zcoordGEO,/ADD_REPLACE
  endif
  str_element,str,'xcoordSM',xcoordSM,/ADD_REPLACE
  str_element,str,'ycoordSM',ycoordSM,/ADD_REPLACE
  str_element,str,'zcoordSM',zcoordSM,/ADD_REPLACE
  ;  str_element,str,'kx',kx,/ADD_REPLACE
  ;  str_element,str,'ky',ky,/ADD_REPLACE
  ;  str_element,str,'kz',kz,/ADD_REPLACE
  str_element,str,'vgroup',vgroup,/ADD_REPLACE

  return,str
end
