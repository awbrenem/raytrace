;+
;*****************************************************************************************
;
;  PROCEDURE : create_rays_general
;  PURPOSE  : Creates rays by running trace.for for the requested input values
;			  and turns these quantities into a .sav file
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
;  INPUT:	arrays of input wave values, one for each ray to be created.
;
;  EXAMPLES:
;
;
;  KEYWORDS: geo2sm -> tells read_trace_ta to convert from geographic coord to
;                      SM coord. IGRF model of trace.for returns geographic coord.
;
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:   Be sure to input geographic coord values if calling the IGRF model of trace.for
;
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-


;freq used to determine size of arrays. Must always input a freq value for every freq.

pro create_rays_general,freqv,$
  theta=thetav,$
  alt=altv,$
  lat=latv,$
  long=longv,$
  title=title,rootdir=rootdir,$
  geotime=geotime

  if ~KEYWORD_SET(geotime) then geotime = ''

  nrays = n_elements(freqv)
  npoints = 10000. ;max # of ray points


  path = replicate(!values.f_nan,npoints,nrays)
  timeG = replicate(!values.f_nan,npoints,nrays)
  wavelength = replicate(!values.f_nan,npoints,nrays)
  f_fce = replicate(!values.f_nan,npoints,nrays)
  f_fch = replicate(!values.f_nan,npoints,nrays)
  dens = replicate(!values.f_nan,npoints,nrays)
  flhr = replicate(!values.f_nan,npoints,nrays)
  indexref = replicate(!values.f_nan,npoints,nrays)
  gendrin = replicate(!values.f_nan,npoints,nrays)
  rescone = replicate(!values.f_nan,npoints,nrays)
  thk = replicate(!values.f_nan,npoints,nrays)
  thg = replicate(!values.f_nan,npoints,nrays)
  phk = replicate(!values.f_nan,npoints,nrays)
  azk = replicate(!values.f_nan,npoints,nrays)
  vek = replicate(!values.f_nan,npoints,nrays)

  radius = replicate(!values.f_nan,npoints,nrays)
  latGEO = replicate(!values.f_nan,npoints,nrays)
  latSM = replicate(!values.f_nan,npoints,nrays)
  longitGEO = replicate(!values.f_nan,npoints,nrays)
  longitSM = replicate(!values.f_nan,npoints,nrays)
  lval = replicate(!values.f_nan,npoints,nrays)

  xcoordGEO = replicate(!values.f_nan,npoints,nrays)
  ycoordGEO = replicate(!values.f_nan,npoints,nrays)
  zcoordGEO = replicate(!values.f_nan,npoints,nrays)
  xcoordSM = replicate(!values.f_nan,npoints,nrays)
  ycoordSM = replicate(!values.f_nan,npoints,nrays)
  zcoordSM = replicate(!values.f_nan,npoints,nrays)
;  kx = replicate(!values.f_nan,npoints,nrays)
;  ky = replicate(!values.f_nan,npoints,nrays)
;  kz = replicate(!values.f_nan,npoints,nrays)




  ;These ray values are constant along the entire raypath and represent
  ;the initial value for that ray. e.g. the radial value where the ray was started.
  ;These are used after the triangulation routine for, for ex., selecting a particular
  ;L-slice (say at mlats from 20-40 deg) and knowing where these rays originated from.


  wavelength0 = replicate(!values.f_nan,npoints,nrays)
  f_fce0 = replicate(!values.f_nan,npoints,nrays)
  f_fch0 = replicate(!values.f_nan,npoints,nrays)
  dens0 = replicate(!values.f_nan,npoints,nrays)
  flhr0 = replicate(!values.f_nan,npoints,nrays)
  indexref0 = replicate(!values.f_nan,npoints,nrays)
  gendrin0 = replicate(!values.f_nan,npoints,nrays)
  rescone0 = replicate(!values.f_nan,npoints,nrays)
  thk0 = replicate(!values.f_nan,npoints,nrays)
  thg0 = replicate(!values.f_nan,npoints,nrays)
  phk0 = replicate(!values.f_nan,npoints,nrays)
  azk0 = replicate(!values.f_nan,npoints,nrays)
  vek0 = replicate(!values.f_nan,npoints,nrays)

  radius0 = replicate(!values.f_nan,npoints,nrays)
  latGEO0 = replicate(!values.f_nan,npoints,nrays)
  latSM0 = replicate(!values.f_nan,npoints,nrays)
  longitGEO0 = replicate(!values.f_nan,npoints,nrays)
  longitSM0 = replicate(!values.f_nan,npoints,nrays)
  lval0 = replicate(!values.f_nan,npoints,nrays)

  xcoordGEO0 = replicate(!values.f_nan,npoints,nrays)
  ycoordGEO0 = replicate(!values.f_nan,npoints,nrays)
  zcoordGEO0 = replicate(!values.f_nan,npoints,nrays)
  xcoordSM0 = replicate(!values.f_nan,npoints,nrays)
  ycoordSM0 = replicate(!values.f_nan,npoints,nrays)
  zcoordSM0 = replicate(!values.f_nan,npoints,nrays)
;  kx0 = replicate(!values.f_nan,npoints,nrays)
;  ky0 = replicate(!values.f_nan,npoints,nrays)
;  kz0 = replicate(!values.f_nan,npoints,nrays)



  ;loop through all the theta values
  for qq=0,nrays-1 do begin

    ti = read_write_trace_in(freq=freqv[qq],$
    ;    mmult = .80,$
    lat=latv[qq],$
    long=longv[qq],$
    theta=thetav[qq],$
    ;    phi=0.,$
    alt=altv[qq])
    ;    final_alt=4000.,$
    ;    model=0,$
    ;    final_lat=42,$
    ;    pplcp=3.,$
    ;    pplhw=0.5,$
    ;    drl=10000.)



    ;Change directory to raytrace so I can run the trace script
    cd,'~/Desktop/code/Aaron/github.umn.edu/raytrace',current=currdir
    spawn,'./trace'
    ;Change Back to original directory
    cd,currdir


    ;read in the ray values
    x = read_trace_ta(geotime=geotime)


    ;See if there are any geographical coord tags.
    geocoord = TAG_EXIST(x,'LatGEO')


    npts = n_elements(x.path)
    if npts gt npoints then npts = npoints

    timeG[0:npts-1,qq] = x.timeG[0:npts-1]
    path[0:npts-1,qq] = x.path[0:npts-1]
    radius[0:npts-1,qq] = x.R[0:npts-1]
    if geocoord then latGEO[0:npts-1,qq] = x.latGEO[0:npts-1]
    latSM[0:npts-1,qq] = x.latSM[0:npts-1]
    if geocoord then longitGEO[0:npts-1,qq] = x.longGEO[0:npts-1]
    longitSM[0:npts-1,qq] = x.longSM[0:npts-1]
    lval[0:npts-1,qq] = x.L[0:npts-1]

    timeG[0:npts-1,qq] = x.timeG[0:npts-1]
    if geocoord then xcoordGEO[0:npts-1,qq] = x.xcoordGEO[0:npts-1]
    if geocoord then ycoordGEO[0:npts-1,qq] = x.ycoordGEO[0:npts-1]
    if geocoord then zcoordGEO[0:npts-1,qq] = x.zcoordGEO[0:npts-1]
    xcoordSM[0:npts-1,qq] = x.xcoordSM[0:npts-1]
    ycoordSM[0:npts-1,qq] = x.ycoordSM[0:npts-1]
    zcoordSM[0:npts-1,qq] = x.zcoordSM[0:npts-1]
    thk[0:npts-1,qq] = x.thk[0:npts-1]
    phk[0:npts-1,qq] = x.phk[0:npts-1]
    thg[0:npts-1,qq] = x.thg[0:npts-1]
    azk[0:npts-1,qq] = x.azk[0:npts-1]
    vek[0:npts-1,qq] = x.vek[0:npts-1]
;    kx[0:npts-1,qq] = x.kx[0:npts-1]
;    ky[0:npts-1,qq] = x.ky[0:npts-1]
;    kz[0:npts-1,qq] = x.kz[0:npts-1]
    wavelength[0:npts-1,qq] = x.wl[0:npts-1]
    f_fce[0:npts-1,qq] = x.f_fce[0:npts-1]
    f_fch[0:npts-1,qq] = x.f_fch[0:npts-1]
    gendrin[0:npts-1,qq] = x.thg[0:npts-1]
    rescone[0:npts-1,qq] = x.res[0:npts-1]
    dens[0:npts-1,qq] = x.dens[0:npts-1]



    ;populate the initial value arrays
    wavelength0[0:npts-1,qq] = wavelength[0,qq]
    f_fce0[0:npts-1,qq] =  f_fce[0,qq]
    f_fch0[0:npts-1,qq] =  f_fch[0,qq]
    dens0[0:npts-1,qq] =  dens[0,qq]
    flhr0[0:npts-1,qq] = flhr[0,qq]
    indexref0[0:npts-1,qq] = indexref[0,qq]
    gendrin0[0:npts-1,qq] = gendrin[0,qq]
    rescone0[0:npts-1,qq] = rescone[0,qq]
    thk0[0:npts-1,qq] = thk[0,qq]
    thg0[0:npts-1,qq] = thg[0,qq]
    phk0[0:npts-1,qq] = phk[0,qq]
    azk0[0:npts-1,qq] = azk[0,qq]
    vek0[0:npts-1,qq] = vek[0,qq]

    radius0[0:npts-1,qq] =radius[0,qq]
    if geocoord then latGEO0[0:npts-1,qq] = latGEO[0,qq]
    latSM0[0:npts-1,qq] = latSM[0,qq]
    if geocoord then longitGEO0[0:npts-1,qq] = longitGEO[0,qq]
    longitSM0[0:npts-1,qq] = longitSM[0,qq]
    lval0[0:npts-1,qq] = lval[0,qq]

    if geocoord then xcoordGEO0[0:npts-1,qq] = xcoordGEO[0,qq]
    if geocoord then ycoordGEO0[0:npts-1,qq] = ycoordGEO[0,qq]
    if geocoord then zcoordGEO0[0:npts-1,qq] = zcoordGEO[0,qq]
    xcoordSM0[0:npts-1,qq] = xcoordSM[0,qq]
    ycoordSM0[0:npts-1,qq] = ycoordSM[0,qq]
    zcoordSM0[0:npts-1,qq] = zcoordSM[0,qq]
;    kx0[0:npts-1,qq] = kx0[0,qq]
;    ky0[0:npts-1,qq] = ky0[0,qq]
;    kz0[0:npts-1,qq] = kz0[0,qq]

  endfor



  if ~keyword_set(title) then title=''
  if ~keyword_set(rootdir) then rootdir = '~/Desktop/code/Aaron/github.umn.edu/raytrace/'

  save,filename=rootdir + title + '_rays.sav'

end
