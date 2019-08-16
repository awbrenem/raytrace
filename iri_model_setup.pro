;Set up and tweak density model for trace_in.txt
;Use the IRI (international reference ionosphere) model at:
;http://modelweb.gsfc.nasa.gov/models/iri.html
;****(LINK NO LONGER WORKING...
;****Try: https://ccmc.gsfc.nasa.gov/modelweb/models/iri2016_vitmo.php



;To do this I need to input the GEOGRAPHIC longitude and latitude of
;the meridional equator at L=1
;i.e. convert the point [smlong,0,6370] to geographic coord


;This is definitely working. I get almost identical results if I
;compare to the Interactive map to get geomagnetic or geographic coordinates:
;http://modelweb.gsfc.nasa.gov/models/cgm/cgm.html
;****LINK NO LONGER WORKING
;Also, if I trace a ray at mlat=0 at 1000 km then I get the same values that
;were returned for the supposedly equal geographic coord.

;Program first returns the values for inputting into the IRI model. It then returns
;geographic values at the position of the sc for testing of the model.

;Procedure: Input the model parameters to set up the model.
;	Then tweak the model so that the trace_ta.txt values at sc match those of the
;   model at the sc position.
;
;There are two good approaches to tweaking.
;	1. First adjust the ion ratios (usually by first getting O+ right, then messing with He+), then adjust density.
;	2. Adjust the temp so that correct ion ratios are obtained. Then make small tweaks to density.
;		I find that this method works better. A heated ionosphere will up ratios of heavy ions.
;		This heating could easily occur b/c of transmitter signals.
;Often one or the other of these approaches requires unreasonable input values while the other
;	works well. Also, if the sc is beyond 2000 km alt, then just test at 2000 km.

;WARNING: ALWAYS HAVE YOUR REFERENCE LEVEL BELOW THE ALTITUDE OF THE RAYS, OTHERWISE
;MODEL VALUES AREN'T REALISTIC. I.E. DON'T USE 1000 KM AS REFERENCE IF YOU ARE TRACING
;RAYS AT 500 KM.


sc = 'RBSPa'
;sc = 'STA'
;datetime = '2006-11-06/09:06:53'
datetime = '2017-08-21/01:45:00'

if strmid(datetime,0,10) eq '2006-11-06' and sc eq 'STA' then num = 0.
if strmid(datetime,0,10) eq '2006-11-17' and sc eq 'STA' then num = 1.
if strmid(datetime,0,10) eq '2006-11-29' and sc eq 'STA' then num = 2.
if strmid(datetime,0,10) eq '2006-12-12' and sc eq 'STA' then num = 3.

if strmid(datetime,0,10) eq '2006-11-06' and sc eq 'STB' then num = 4.
if strmid(datetime,0,10) eq '2006-11-17' and sc eq 'STB' then num = 5.
if strmid(datetime,0,10) eq '2006-11-29' and sc eq 'STB' then num = 6.
if strmid(datetime,0,10) eq '2006-12-12' and sc eq 'STB' then num = 7.


restore,'~/Desktop/code/Aaron/datafiles/idlsave/stereo_perigee_GEOcoord.sav'
restore,'~/Desktop/stereo_perigee_GEOcoord.sav'
geo_xyz = {a1:struct_a1,a2:struct_a2,a3:struct_a3,a4:struct_a4,$
	b1:struct_b1,b2:struct_b2,b3:struct_b3,b4:struct_b4}

restore,'~/Desktop/code/Aaron/datafiles/idlsave/stereo_perigee_SMcoord.sav'
sm_xyz = {a1:struct_a1,a2:struct_a2,a3:struct_a3,a4:struct_a4,$
	b1:struct_b1,b2:struct_b2,b3:struct_b3,b4:struct_b4}



;________________________________________________________________________________________
;First find the geographic coord of sc for testing of IRI model

geo = geo_xyz.(num)
geot = geo.times
foo = where(time_double(geot) ge time_double(datetime))
foo = foo[0]



rec = [[geo.state[0,foo]],[geo.state[1,foo]],[geo.state[2,foo]]]


locgeo = cv_coord(from_rect=transpose(rec),/to_sphere,/Degrees,/double)
if locgeo[0] lt 0. then locgeo[0] = 360. + locgeo[0]
;________________________________________________________________________________________



;________________________________________________________________________________________
;Now find geographic coord of sc footpoint for mlat=0 for input to IRI model
geo_footpoints = {A1:REPLICATE(0e0,3L)}

s = sm_xyz.(num)
time = sm_xyz.(num).times   ;replicate('2006-11-06/00:00:00',n_elements(s.times))

;transform to SM lat_long coord
rec = [[reform(s.state[0,*])],[reform(s.state[1,*])],[reform(s.state[2,*])]]
loc1 = cv_coord(from_rect=transpose(rec),/to_sphere,/Degrees,/double) ;(long(deg),lat(deg),r(km))
nelem = n_elements(loc1[0,*])

;get the mlat=0, r=6370 SM coord for field line.
loc1 = transpose([[reform(loc1[0,*])],[replicate(0.,nelem)],[replicate(6370.,nelem)]])
sph = loc1

;convert SM footpoints (long,lat,r) to (x,y,z)
loc = cv_coord(FROM_SPHERE=sph,/TO_RECT,/Degrees,/double) ;(x,y,z)
SM=transpose(loc)
;Get geographic coord
cotrans,SM,gsm,time,/SM2GSM
cotrans,gsm,gse,time,/GSM2GSE
cotrans,gse,gei,time,/GSE2GEI
cotrans,gei,geo,time,/GEI2GEO
geo=transpose(geo)  ;GEO footpoint coord of the L-shell (x,y,z)
loc = cv_coord(FROM_RECT=geo,/TO_SPHERE,/Degrees,/double) ;(Long, Lat, Rad)

footpoints = transpose(loc)



;foo = where(time_double(time) ge time_double(datetime))
;foo = foo[0]

timex = time[foo]
hr = strmid(timex,11,2)
mn = strmid(timex,14,2)
sc = strmid(timex,17,2)

decimal = strtrim(float(mn)/60.,2)


longit = footpoints[foo,0]
lat = footpoints[foo,1]
r = footpoints[foo,2]

if longit lt 0. then longit = 360. + longit

L = r/(cos(!dtor*lat)^2)/6370.  ;L-shell in centered dipole
ilat = acos(sqrt(1/L))/!dtor  ;invariant latitude

print,'#################################################'
print,'EQUATORIAL INPUT PARAMS FOR RAY TRACING MODEL'
print,'ENTER THESE GEOGRAPHIC VALUES INTO THE IRI MODEL'
print,'THESE CORRESPOND TO SMLAT=0,R=6370,LONG=SM_LONG OF SC'
print,'#################################################'
print,'TIME TO INPUT IS: ' + hr + '.' + strmid(decimal,2,2)
print,'GEO LONG IS: ' + strtrim(longit,2)
print,'GEO LAT IS : ' + strtrim(lat,2)


print,'##################################################'
print,'HERE ARE THE GEOGRAPHIC COORD OF SC. ENTER THESE'
print,'VALUES INTO THE IRI MODEL TO SEE WHAT WE SHOULD BE OBSERVING AT SC'
print,'TIME TO INPUT IS: ' + hr + '.' + strmid(decimal,2,2)
print,'GEO LONG SC: ' + strtrim(locgeo[0],2)
print,'GEO LAT SC: ' + strtrim(locgeo[1],2)
print,'ALT SC (km off Earths surface: ' + strtrim(locgeo[2]-6370.,2)
