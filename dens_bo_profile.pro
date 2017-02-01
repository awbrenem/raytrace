;+
;*****************************************************************************************
;
;  FUNCTION : dens_bo_profile
;  PURPOSE  : Plots the density vs L and Bo vs L for ray tracing output. Can overplot values at sc
;			  location
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
;       freq -> wave freq in Hz
;       dens_sc  -> density at sc (cm-3)
;       bo_sc -> magnetic field value at sc (nT)
;				L_sc     -> L shell of sc
;				ps       -> Plot PS
;
;
;   CHANGED:  1)  NA [MM/DD/YYYY   v1.0.0]
;
;   NOTES:
;
;
;   CREATED:  MM/DD/YYYY
;   CREATED BY:  Aaron W. Breneman
;    LAST MODIFIED:  MM/DD/YYYY   v1.0.0
;    MODIFIED BY: Aaron W. Breneman
;
;*****************************************************************************************
;-


pro dens_bo_profile,freq,dens_sc=dens_sc,bo_sc=bo_sc,L_sc=L_sc,ps=ps

if keyword_set(ps) then begin
  popen,'~/Desktop/dens_bo_profile.ps'
endif


n = fltarr(100000)
r = fltarr(100000)
ffce = fltarr(100000)
theta = fltarr(100000)
temp = fltarr(14)
junk = ' '
inline = ' '
test = ' '
i=0
j=0

openr, 1, '~/Desktop/code/Aaron/github.umn.edu/raytrace/trace_ta.txt'

for i=0,1 do readf,1,junk

while test ne 'STOP' do begin

  readf, 1, inline
  test = strmid(inline,0,4)
;605   FORMAT(I2,5G10.3,F7.3,F8.0,2F7.2,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1)
 ; print, inline

  ;if test ne 'STOP' then reads, inline, j, temp, FORMAT='(I2,5G10.3,F7.3,F8.0,2F7.2,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3)'
  ;if test ne 'STOP' then reads, inline,
  ;j, temp, FORMAT='(I2,6G10.3,F8.2,2F9.2,F7.2,F12.2,2F10.2)'

  n[i]=strmid(inline,139,9)
  r[i]=strmid(inline,53,8)
  ffce[i]=strmid(inline,107,9)
  i=i+1
endwhile
close,1

b = freq/(28.*ffce)

print, 'n:', n[0:15]
print, 'r:', r[0:15]
print, 'b:', b[0:15]
print, 'theta:', theta[0:15]

n = n[0:i-1]
r = r[0:i-1]
b = b[0:i-1]
theta = r[0:i-1]

if n_elements(L_sc) eq 1 then L_sc = [L_sc,L_sc]
if n_elements(dens_sc) eq 1 then dens_sc = [dens_sc,dens_sc]
if n_elements(bo_sc) eq 1 then bo_sc = [bo_sc,bo_sc]


!p.multi = [0,0,2]
plot, r, n, /ylog, xrange=[2,6], yrange=[1,10000], xgridstyle=1, xticklen=1, $
	ygridstyle=1, yticklen=1, title='Equatorial Density Profile (cm-3)'
if keyword_set(dens_sc) and keyword_set(L_sc) then oplot,L_sc,dens_sc,psym=7


plot, r, b, /ylog, xrange=[2,6], yrange=[1,10000], xgridstyle=1, xticklen=1, $
	ygridstyle=1, yticklen=1, title='Equatorial Bo Profile (nT)'
if keyword_set(bo_sc) and keyword_set(L_sc) then oplot,L_sc,bo_sc,psym=7

if keyword_set(ps) then begin
  pclose
;  device, /close
;  set_plot, 'x'
endif


end
