
;IGRF model in trace.f uses geographic coordinates.
;Determine geographic coord from geomagnetic coord
;It also outputs geographic latitude and longitude that is used for the
;inut to the ray tracer. I wrote this routine b/c the transformation requires
;a specific definition to get to GEOGRAPHIC lat/long, which can be difficult
;to remember.

;smcoord -> SM (x,y,z) coordinates in RE, unless /smlatlong keyword is set, then
;           it's (r,lat,long) where r is in RE
;time -> (e.g. '2016-01-20/19:44'). Needed for tranformation from GEO to SM coord

;I've tested this by comparing results from the RBSP Science Gateway.
;These values are correct according to Science Gateway
;These match the first row of output in trace_ta.txt, which tells me that the input
;and output of trace.f are in GEO coord


function transform_sm_to_geographic_latlong_for_igrf_raytrace_input,$
  smcoord,time,smlatlong=smlatlong

  timetmp = time_double(time) + dindgen(10)




  if KEYWORD_SET(smlatlong) then begin

    xsm = smcoord[0]*sin((90-smcoord[1])*!dtor)*cos((smcoord[2])*!dtor)
    ysm = smcoord[0]*sin((90-smcoord[1])*!dtor)*sin((smcoord[2])*!dtor)
    zsm = smcoord[0]*cos((90-smcoord[1])*!dtor)

    smlong = smcoord[2]
    smlat = smcoord[1]

  endif else begin

    xsm = smcoord[0]
    ysm = smcoord[1]
    zsm = smcoord[2]

    ;calculate the SM lat, long. Not needed here, but I'll return it as
    ;part of the structure for convenience.
    rtmp = sqrt(xsm^2 + ysm^2 + zsm^2)
    smlong = atan(ysm/xsm)/!dtor
    smlat = 90.-acos(zsm/rtmp)/!dtor

  endelse


  xsm = 6370.*replicate(xsm,10)
  ysm = 6370.*replicate(ysm,10)
  zsm = 6370.*replicate(zsm,10)



  store_data,'smcoordtmp',timetmp,[[xsm],[ysm],[zsm]]
  cotrans,'smcoordtmp','gsmcoordtmp',/sm2gsm
  cotrans,'gsmcoordtmp','gsecoordtmp',/GSM2GSE
  cotrans,'gsecoordtmp','geicoordtmp',/GSE2GEI
  cotrans,'geicoordtmp','geocoordtmp',/GEI2GEO


  get_data,'geocoordtmp',tt,geo
  geox = geo[0,0]
  geoy = geo[0,1]
  geoz = geo[0,2]


  ;Now from the GEOGRAPHIC coord calculate the geolat and geolong
  r = sqrt(geox^2 + geoy^2 + geoz^2)
  theta = acos(geoz/r)/!dtor
  phi = atan(geoy/geox)/!dtor

  geolat = 90. - theta
  geolong = -180. + phi
  r = r/6370.

  ;clean up
  store_data,['smcoordtmp','gsmcoordtmp','gsecoordtmp','geicoordtmp','geocoordtmp'],/delete

  str = {radius:r,geolat:geolat,geolong:geolong,$
         smlat:smlat,smlong:smlong,xsm:xsm[0],ysm:ysm[0],zsm:zsm[0]}
  return,str


end
