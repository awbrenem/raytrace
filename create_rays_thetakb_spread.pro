;+
;*****************************************************************************************
;
;  PROCEDURE : create_rays_thetakb_spread
;  PURPOSE  : Creates rays by running trace.for for the inputted theta_kb values
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
;  INPUT:	    theta_kb_vals -> array of wave normal angles
;				freqs -> array of freqs (same size of theta_kb_vals)
;               title -> '_rays.sav' will be appended to this
;
;  EXAMPLES:
;
;
;  KEYWORDS:
;
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:     Program is only capable of changing the theta_kb value in the trace_in.txt file
;			   To change other values use read_write_trace_in.pro
;
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-

pro create_rays_thetakb_spread,theta_kb_vals,title=title,rootdir=rootdir,freqs=freqs,geotime=geotime

  if ~KEYWORD_SET(geotime) then geotime = ''

  npoints = 10000. ;max # of ray points

  nthetas = n_elements(theta_kb_vals)

  path = replicate(!values.f_nan,npoints,nthetas)
  timeG = replicate(!values.f_nan,npoints,nthetas)
  wavelength = replicate(!values.f_nan,npoints,nthetas)
  f_fce = replicate(!values.f_nan,npoints,nthetas)
  f_fch = replicate(!values.f_nan,npoints,nthetas)
  dens = replicate(!values.f_nan,npoints,nthetas)
  flhr = replicate(!values.f_nan,npoints,nthetas)
  indexref = replicate(!values.f_nan,npoints,nthetas)
  gendrin = replicate(!values.f_nan,npoints,nthetas)
  rescone = replicate(!values.f_nan,npoints,nthetas)
  thk = replicate(!values.f_nan,npoints,nthetas)
  thg = replicate(!values.f_nan,npoints,nthetas)
  phk = replicate(!values.f_nan,npoints,nthetas)
  azk = replicate(!values.f_nan,npoints,nthetas)
  vek = replicate(!values.f_nan,npoints,nthetas)


  radius = replicate(!values.f_nan,npoints,nthetas)
  latGEO = replicate(!values.f_nan,npoints,nthetas)
  latSM = replicate(!values.f_nan,npoints,nthetas)
  longitGEO = replicate(!values.f_nan,npoints,nthetas)
  longitSM = replicate(!values.f_nan,npoints,nthetas)
  lval = replicate(!values.f_nan,npoints,nthetas)

  xcoordGEO = replicate(!values.f_nan,npoints,nthetas)
  ycoordGEO = replicate(!values.f_nan,npoints,nthetas)
  zcoordGEO = replicate(!values.f_nan,npoints,nthetas)
  xcoordSM = replicate(!values.f_nan,npoints,nthetas)
  ycoordSM = replicate(!values.f_nan,npoints,nthetas)
  zcoordSM = replicate(!values.f_nan,npoints,nthetas)
;  kx = replicate(!values.f_nan,npoints,nthetas)
;  ky = replicate(!values.f_nan,npoints,nthetas)
;  kz = replicate(!values.f_nan,npoints,nthetas)



  ;loop through all the theta values
  for qq=0,nthetas-1 do begin

    theta_kb = theta_kb_vals[qq]
    fq = freqs[qq]


    ;modify trace_in.txt
    if ~keyword_set(freqs) then ti_vals = read_write_trace_in(theta=theta_kb) else $
    ti_vals = read_write_trace_in(theta=theta_kb,freq=fq)


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


  endfor





  if ~keyword_set(title) then title=''
  if ~keyword_set(rootdir) then rootdir = '~/Desktop/code/Aaron/github.umn.edu/raytrace/'

  save,filename=rootdir + title + '_rays.sav'

end
