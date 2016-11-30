;+
;*****************************************************************************************
;
;  PROCEDURE : create_rays
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

pro create_rays,theta_kb_vals,title=title,rootdir=rootdir,freqs=freqs


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
lat = replicate(!values.f_nan,npoints,nthetas)
longit = replicate(!values.f_nan,npoints,nthetas)

xcoord = replicate(!values.f_nan,npoints,nthetas)
ycoord = replicate(!values.f_nan,npoints,nthetas)
zcoord = replicate(!values.f_nan,npoints,nthetas)
kx = replicate(!values.f_nan,npoints,nthetas)
ky = replicate(!values.f_nan,npoints,nthetas)
kz = replicate(!values.f_nan,npoints,nthetas)


;loop through all the theta values
for qq=0,nthetas-1 do begin


;choose first theta_kb value
;theta_kb = theta_kb_vals.th_north[qq]
theta_kb = theta_kb_vals[qq]
fq = freqs[qq]


;modify trace_in.txt
if ~keyword_set(freqs) then ti_vals = read_write_trace_in(theta=theta_kb) else ti_vals = read_write_trace_in(theta=theta_kb,freq=fq)
;create the first ray
spawn,'~/Desktop/code/Aaron/github.umn.edu/raytrace/./trace'

;read in the ray values
x = read_trace_ta()


npts = n_elements(x.path)
if npts gt npoints then npts = npoints

timeG[0:npts-1,qq] = x.timeG[0:npts-1]
path[0:npts-1,qq] = x.path[0:npts-1]
radius[0:npts-1,qq] = x.R[0:npts-1]
lat[0:npts-1,qq] = x.lat[0:npts-1]
longit[0:npts-1,qq] = x.long[0:npts-1]
timeG[0:npts-1,qq] = x.timeG[0:npts-1]
xcoord[0:npts-1,qq] = x.xcoord[0:npts-1]
ycoord[0:npts-1,qq] = x.ycoord[0:npts-1]
zcoord[0:npts-1,qq] = x.zcoord[0:npts-1]
thk[0:npts-1,qq] = x.thk[0:npts-1]
phk[0:npts-1,qq] = x.phk[0:npts-1]
thg[0:npts-1,qq] = x.thg[0:npts-1]
azk[0:npts-1,qq] = x.azk[0:npts-1]
vek[0:npts-1,qq] = x.vek[0:npts-1]
kx[0:npts-1,qq] = x.kx[0:npts-1]
ky[0:npts-1,qq] = x.ky[0:npts-1]
kz[0:npts-1,qq] = x.kz[0:npts-1]

endfor


if ~keyword_set(title) then title=''
if ~keyword_set(rootdir) then rootdir = ''

save,filename=rootdir + title + '_rays.sav'

end
