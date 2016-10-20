;Crib sheet for ray tracing plots....

;Reverse input crib sheet



;------------TEST VALUES------------------------------------------------------
;first let's set up the field and density model
ti_vals = read_write_trace_in(freq=10000.,lat=0.,alt=10000.,longit=0.,$
	final_alt=2000.,final_lat=40.,mmult=1.,model=1)



;first let's get some rays
thetas = create_theta_kb_vals(0.,10.,nthetas=5.,freq=20000.,fce=500000.)
create_rays,thetas.th_north,title='test1',rootdir='~/Desktop/code/Aaron/Raytrace/reversetrace_new/'
;f1 = read_trace_ta()


ti_vals = read_write_trace_in(freq=20000.,lat=0.,alt=10000.,longit=0.,$
	final_alt=2000.,final_lat=40.,mmult=1.,model=1)

thetas = create_theta_kb_vals(0.,10.,nthetas=5.,freq=10000.,fce=500000.)
create_rays,thetas.th_north,title='test2',rootdir='~/Desktop/code/Aaron/Raytrace/reversetrace_new/'
;f2 = read_trace_ta()


restore,'test1_rays.sav'
zvals1 = timeG
xcoord1 = xcoord
zcoord1 = zcoord


;Remove the NaN values before sending to triangulation
xc1 = xcoord1[where(finite(xcoord1[*,0]) ne 0.),0]
for ii=1,n_elements(xcoord1[0,*])-1 do xc1 = [xc1,where(finite(xcoord1[*,ii]) ne 0.)]
zc1 = zcoord1[where(finite(zcoord1[*,0]) ne 0.),0]
for ii=1,n_elements(zcoord1[0,*])-1 do zc1 = [zc1,where(finite(zcoord1[*,ii]) ne 0.)]
zv1 = zvals1[where(finite(zvals1[*,0]) ne 0.),0]
for ii=1,n_elements(zvals1[0,*])-1 do zv1 = [zv1,where(finite(zvals1[*,ii]) ne 0.)]



tri1 = triangulation(xc1,zc1,zv1,xg=xg1,yg=yg1);,gridspac=gridspac,limits=limits)

restore,'test2_rays.sav'
zvals2 = timeG
xcoord2 = xcoord
zcoord2 = zcoord


xc2 = xcoord2[where(finite(xcoord2[*,0]) ne 0.),0]
for ii=1,n_elements(xcoord2[0,*])-1 do xc2 = [xc2,where(finite(xcoord2[*,ii]) ne 0.)]
zc2 = zcoord2[where(finite(zcoord2[*,0]) ne 0.),0]
for ii=1,n_elements(zcoord2[0,*])-1 do zc2 = [zc2,where(finite(zcoord2[*,ii]) ne 0.)]
zv2 = zvals2[where(finite(zvals2[*,0]) ne 0.),0]
for ii=1,n_elements(zvals2[0,*])-1 do zv2 = [zv2,where(finite(zvals2[*,ii]) ne 0.)]




tri2 = triangulation(xc2,zc2,zv2,xg=xg2,yg=yg2);,gridspac=gridspac,limits=limits)

;---------------------------------------------------------------------------


;RAYS FOR NOV06, STA 2006 WHISTLER AT 09:17:14.730

;freq	theta_kb	time
;28		75			6
;24		55			8
;18		85			13
;14		45			19
;11		70			25
;10		85			30

;ilat=26
;lat=-3.011
;long=-150.2
;Re=7878.
;L=1.244
;Bo = 15920.
;fce = 446.
;fci = 0.2428
;flhr = 10.4

alt = 7878. - 6370.
ti_vals = read_write_trace_in(lat=-3.011,alt=alt,longit=0.,$
	final_alt=200.,final_lat=-70.,mmult=0.9654,model=3,drl=23500.)

thetas = [0.,75.,55.,85.,45.,70.,85.]
freqs = [28000.,28000.,24000.,18000.,14000.,11000.,10000.]

create_rays,thetas,freqs=freqs,title='whistler_staNov06_091714_730',rootdir='~/Desktop/code/Aaron/Raytrace/reversetrace_new/'


restore,'~/Desktop/code/Aaron/Raytrace/reversetrace_new/whistler_staNov06_091714_730_rays.sav'

plot_rays,xcoord,ycoord,zcoord,xrange=[0.5,1.5],yrange=[-0.5,0.5],colors=[80,100,120,140,160,180,200]

