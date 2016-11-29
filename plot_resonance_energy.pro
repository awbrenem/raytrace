;+
;*****************************************************************************************
;
;  PROCEDURE : plot_resonance_energy
;  PURPOSE  :  plots relativistic cyclotron resonance energy along ray path from
;							 output of trace.f (trace_ta.txt).
;
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
;  INPUT:   nres -> STRING of cyclotron resonance number (positive)
;						pitch_angle -> pitch angle of resonant e-  [Ec = Ec_min/cos(pa)]
;
;  EXAMPLES:
;
;  KEYWORDS:
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:
;
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  11/01/2011   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;			-Changed all polar plots to xyz plots. For some reason plotting with the polar
;				keyword in IDL is inaccurate.
;*****************************************************************************************
;-


pro plot_resonance_energy,rayx,rayy,rayz,$
		mlat,fq,dens,f_fce,indexref,theta_kb,wavelength,$
		Lsc=Lsc,minkeV=minr,maxkeV=maxr,$
		scposx=scx,scposy=scy,scposz=scz,ps=ps,harmonic=nres,type=type,$
		pitch_angle=alpha,nonrelativistic=nonrelativistic

if ~KEYWORD_SET(nres) then nres = 1
if ~KEYWORD_SET(type) then type = 'cyclotron'

rbsp_efw_init


if ~KEYWORD_SET(minr) then minr=10.
if ~KEYWORD_SET(maxr) then maxr=500.
if ~KEYWORD_SET(alpha) then alpha = 0.



fce = fq/f_fce



Bo = fq/(28.*f_fce)/1d9   ;in Teslas
;convert to Gauss
Bo = Bo*10000.
;Square This
Bo2 = Bo^2 ;units of g/cm/s^2
;divide by 1000 to get to kg
Bo2 = Bo2/1000.

Bo2_N = Bo2/dens   ;units of g*cm^2/s^2
;convert to kg
Bo2_N = Bo2_N/1000.
;convert to m^2
Bo2_N = Bo2_N/100./100.   ;units of kg*m^2/s^2 = N*m = J

Bo2_N_eV = Bo2_N*1.6d19
Bo2_N_keV = Bo2_N_eV/1000.



;--------------------------------------------------------
;Relativistic cyclotron resonance energy
;--------------------------------------------------------

c_ms = 2.99792458d8           ; -Speed of light in vacuum (m/s)

;get kz from wave normal angle and wavelength
kvec = 2*!pi/wavelength   ;1/km

evals = cycl_energies(fq,theta_kb,alpha,fce,kvec,nres)


if type eq 'cyclotron' then begin
	Etots = evals.E_cycl_normal
endif
if type eq 'anomalous' then begin
	Etots = evals.E_cycl_anom
endif
if type eq 'landau' then begin
	Etots = evals.E_landau
endif



if ~keyword_set(Lsc) then Lst = dipole(1.001) else begin
	LstR = 0.
	Lstcolat = 0.
	for i=0,n_elements(Lsc)-1 do begin
		Lsttmp = dipole(Lsc[i])
		LstR = [LstR,Lsttmp.R]
		Lstcolat = [Lstcolat,Lsttmp.colat]
	endfor
	Lst = {R:LstR[1:n_elements(LstR)-1],colat:Lstcolat[1:n_elements(LstR)-1]}
endelse


earthx = COS((2*!PI/99.0)*FINDGEN(100))
earthy = SIN((2*!PI/99.0)*FINDGEN(100))

;latitude lines
lats = [0,10,20,30,40,50,60,70,80]
lats = [-1*reverse(lats[1:n_elements(lats)-1]),lats]

!P.multi=[0,0,2]

L2 = dipole(2.)
L4 = dipole(4.)
L6 = dipole(6.)
L8 = dipole(8.)






;Plot the cyclotron energy as a function of ray path
!p.multi = [0,0,1]
loadct,34


if minr eq 0. then minr = 0.001
s = n_elements(rayx)
logvals = alog10(Etots)
colors = long(bytscl(logvals,min=alog10(minr),max=alog10(maxr)))



if KEYWORD_SET(ps) then popen,'~/Desktop/ray_energy_plot.ps'

cgPlot, rayx, rayz, /NoData, Color='Charcoal', Background='ivory',xrange=[0,6],yrange=[-3,3],xstyle=1,ystyle=1,position=aspect(1)
for j=0,s-2 do cgPlotS, [rayx[j], rayx[j+1]], [rayz[j], rayz[j+1]], Color=StrTrim(colors[j],2), Thick=2


nticks = 7.
tn = (indgen(nticks)/(nticks-1))*(maxr-minr)  + minr
tn = strtrim(string(tn,format='(f8.2)'),2)

colorbar,POSITION=[0.15, 0.85, 0.95, 0.90],$
	divisions=nticks-1,ticknames=tn,charsize = 0.8,range=[minr,maxr]





oplot,earthx,earthy,color=60
;oplot,replicate(1.078,360.),indgen(360.)*!dtor,/polar,color=80

oplot,replicate(1.078,360.)*cos(indgen(360.)*!dtor),replicate(1.078,360.)*sin(indgen(360.)*!dtor),color=80
oplot,L2.R/6370.*cos(L2.colat*!dtor),L2.R/6370.*sin(L2.colat*!dtor),color=120
oplot,L2.R/6370.*cos(L2.colat*!dtor),-1*L2.R/6370.*sin(L2.colat*!dtor),color=120

oplot,L4.R/6370.*cos(L4.colat*!dtor),L4.R/6370.*sin(L4.colat*!dtor),color=120
oplot,L4.R/6370.*cos(L4.colat*!dtor),-1*L4.R/6370.*sin(L4.colat*!dtor),color=120

oplot,L6.R/6370.*cos(L6.colat*!dtor),L6.R/6370.*sin(L6.colat*!dtor),color=120
oplot,L6.R/6370.*cos(L6.colat*!dtor),-1*L6.R/6370.*sin(L6.colat*!dtor),color=120

oplot,L8.R/6370.*cos(L8.colat*!dtor),L8.R/6370.*sin(L8.colat*!dtor),color=120
oplot,L8.R/6370.*cos(L8.colat*!dtor),-1*L8.R/6370.*sin(L8.colat*!dtor),color=120


if keyword_set(Lsc) then oplot,Lst.R/6370.*cos(Lst.colat*!dtor),Lst.R/6370.*sin(Lst.colat*!dtor),color=160 & oplot,Lst.R/6370.*cos(Lst.colat*!dtor),-1*Lst.R/6370.*sin(Lst.colat*!dtor),color=160

for i=0,n_elements(lats)-1 do oplot,[1,50]*cos([lats[i]*!dtor,lats[i]*!dtor]),[1,50]*sin([lats[i]*!dtor,lats[i]*!dtor]),linestyle=3,color=100


if KEYWORD_SET(ps) then pclose


end
