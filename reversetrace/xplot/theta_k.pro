
function big_moments,stuff
moments = moment(stuff)
avg = moments(0)
var = sqrt(moments(1))
distfreq = histogram(stuff,min=min(stuff))
maxfrq = max(distfreq)
mode = where(distfreq eq maxfrq)+min(stuff)                             
med = median(stuff)
max = max(stuff)
min = min(stuff)                                                
strstuff = 'MEAN: ' + strtrim(string(avg),2) + ' MEDIAN: ' + strtrim(string(med),2) + ' MODE: ' + strtrim(string(mode),2) + ' MAX: ' + strtrim(string(max),2) + ' MIN: ' + strtrim(string(min),2) + ' STD: '  + strtrim(string(var),2)	
return,strstuff
end




;############################################
;###########################################
;#############################################
pro readfiles
common raystuff,theta_k,not_keepers_source,keepers_source,not_keepers_ws,keepers_ws,tgn,trs,unique_freqs

restore,'raydata1.dat'
restore,'raydata2.dat'
restore,'raydata3.dat'
restore,'raydata4.dat'
restore,'raydata5.dat'
restore,'raydata6.dat'

if keyword_set(theta_k_c1_f1) eq 0 then theta_k_c1_f1 = fltarr(401,401)
if keyword_set(theta_k_c1_f2) eq 0 then theta_k_c1_f2 = fltarr(401,401)
if keyword_set(theta_k_c1_f3) eq 0 then theta_k_c1_f3 = fltarr(401,401)
if keyword_set(theta_k_c1_f4) eq 0 then theta_k_c1_f4 = fltarr(401,401)
if keyword_set(theta_k_c1_f5) eq 0 then theta_k_c1_f5 = fltarr(401,401)
if keyword_set(theta_k_c1_f6) eq 0 then theta_k_c1_f6 = fltarr(401,401)
if keyword_set(theta_k_c2_f1) eq 0 then theta_k_c2_f1 = fltarr(401,401)
if keyword_set(theta_k_c2_f2) eq 0 then theta_k_c2_f2 = fltarr(401,401)
if keyword_set(theta_k_c2_f3) eq 0 then theta_k_c2_f3 = fltarr(401,401)
if keyword_set(theta_k_c2_f4) eq 0 then theta_k_c2_f4 = fltarr(401,401)
if keyword_set(theta_k_c2_f5) eq 0 then theta_k_c2_f5 = fltarr(401,401)
if keyword_set(theta_k_c2_f6) eq 0 then theta_k_c2_f6 = fltarr(401,401)
if keyword_set(theta_k_c3_f1) eq 0 then theta_k_c3_f1 = fltarr(401,401)
if keyword_set(theta_k_c3_f2) eq 0 then theta_k_c3_f2 = fltarr(401,401)
if keyword_set(theta_k_c3_f3) eq 0 then theta_k_c3_f3 = fltarr(401,401)
if keyword_set(theta_k_c3_f4) eq 0 then theta_k_c3_f4 = fltarr(401,401)
if keyword_set(theta_k_c3_f5) eq 0 then theta_k_c3_f5 = fltarr(401,401)
if keyword_set(theta_k_c3_f6) eq 0 then theta_k_c3_f6 = fltarr(401,401)
if keyword_set(theta_k_c4_f1) eq 0 then theta_k_c4_f1 = fltarr(401,401)
if keyword_set(theta_k_c4_f2) eq 0 then theta_k_c4_f2 = fltarr(401,401)
if keyword_set(theta_k_c4_f3) eq 0 then theta_k_c4_f3 = fltarr(401,401)
if keyword_set(theta_k_c4_f4) eq 0 then theta_k_c4_f4 = fltarr(401,401)
if keyword_set(theta_k_c4_f5) eq 0 then theta_k_c4_f5 = fltarr(401,401)
if keyword_set(theta_k_c4_f6) eq 0 then theta_k_c4_f6 = fltarr(401,401)

if keyword_set(theta_g_c1_f1) eq 0 then theta_g_c1_f1 = fltarr(401,401)
if keyword_set(theta_g_c1_f2) eq 0 then theta_g_c1_f2 = fltarr(401,401)
if keyword_set(theta_g_c1_f3) eq 0 then theta_g_c1_f3 = fltarr(401,401)
if keyword_set(theta_g_c1_f4) eq 0 then theta_g_c1_f4 = fltarr(401,401)
if keyword_set(theta_g_c1_f5) eq 0 then theta_g_c1_f5 = fltarr(401,401)
if keyword_set(theta_g_c1_f6) eq 0 then theta_g_c1_f6 = fltarr(401,401)
if keyword_set(theta_g_c2_f1) eq 0 then theta_g_c2_f1 = fltarr(401,401)
if keyword_set(theta_g_c2_f2) eq 0 then theta_g_c2_f2 = fltarr(401,401)
if keyword_set(theta_g_c2_f3) eq 0 then theta_g_c2_f3 = fltarr(401,401)
if keyword_set(theta_g_c2_f4) eq 0 then theta_g_c2_f4 = fltarr(401,401)
if keyword_set(theta_g_c2_f5) eq 0 then theta_g_c2_f5 = fltarr(401,401)
if keyword_set(theta_g_c2_f6) eq 0 then theta_g_c2_f6 = fltarr(401,401)
if keyword_set(theta_g_c3_f1) eq 0 then theta_g_c3_f1 = fltarr(401,401)
if keyword_set(theta_g_c3_f2) eq 0 then theta_g_c3_f2 = fltarr(401,401)
if keyword_set(theta_g_c3_f3) eq 0 then theta_g_c3_f3 = fltarr(401,401)
if keyword_set(theta_g_c3_f4) eq 0 then theta_g_c3_f4 = fltarr(401,401)
if keyword_set(theta_g_c3_f5) eq 0 then theta_g_c3_f5 = fltarr(401,401)
if keyword_set(theta_g_c3_f6) eq 0 then theta_g_c3_f6 = fltarr(401,401)
if keyword_set(theta_g_c4_f1) eq 0 then theta_g_c4_f1 = fltarr(401,401)
if keyword_set(theta_g_c4_f2) eq 0 then theta_g_c4_f2 = fltarr(401,401)
if keyword_set(theta_g_c4_f3) eq 0 then theta_g_c4_f3 = fltarr(401,401)
if keyword_set(theta_g_c4_f4) eq 0 then theta_g_c4_f4 = fltarr(401,401)
if keyword_set(theta_g_c4_f5) eq 0 then theta_g_c4_f5 = fltarr(401,401)
if keyword_set(theta_g_c4_f6) eq 0 then theta_g_c4_f6 = fltarr(401,401)

if keyword_set(pathre_c1_f1) eq 0 then pathre_c1_f1 = fltarr(401,401)
if keyword_set(pathre_c1_f2) eq 0 then pathre_c1_f2 = fltarr(401,401)
if keyword_set(pathre_c1_f3) eq 0 then pathre_c1_f3 = fltarr(401,401)
if keyword_set(pathre_c1_f4) eq 0 then pathre_c1_f4 = fltarr(401,401)
if keyword_set(pathre_c1_f5) eq 0 then pathre_c1_f5 = fltarr(401,401)
if keyword_set(pathre_c1_f6) eq 0 then pathre_c1_f6 = fltarr(401,401)
if keyword_set(pathre_c2_f1) eq 0 then pathre_c2_f1 = fltarr(401,401)
if keyword_set(pathre_c2_f2) eq 0 then pathre_c2_f2 = fltarr(401,401)
if keyword_set(pathre_c2_f3) eq 0 then pathre_c2_f3 = fltarr(401,401)
if keyword_set(pathre_c2_f4) eq 0 then pathre_c2_f4 = fltarr(401,401)
if keyword_set(pathre_c2_f5) eq 0 then pathre_c2_f5 = fltarr(401,401)
if keyword_set(pathre_c2_f6) eq 0 then pathre_c2_f6 = fltarr(401,401)
if keyword_set(pathre_c3_f1) eq 0 then pathre_c3_f1 = fltarr(401,401)
if keyword_set(pathre_c3_f2) eq 0 then pathre_c3_f2 = fltarr(401,401)
if keyword_set(pathre_c3_f3) eq 0 then pathre_c3_f3 = fltarr(401,401)
if keyword_set(pathre_c3_f4) eq 0 then pathre_c3_f4 = fltarr(401,401)
if keyword_set(pathre_c3_f5) eq 0 then pathre_c3_f5 = fltarr(401,401)
if keyword_set(pathre_c3_f6) eq 0 then pathre_c3_f6 = fltarr(401,401)
if keyword_set(pathre_c4_f1) eq 0 then pathre_c4_f1 = fltarr(401,401)
if keyword_set(pathre_c4_f2) eq 0 then pathre_c4_f2 = fltarr(401,401)
if keyword_set(pathre_c4_f3) eq 0 then pathre_c4_f3 = fltarr(401,401)
if keyword_set(pathre_c4_f4) eq 0 then pathre_c4_f4 = fltarr(401,401)
if keyword_set(pathre_c4_f5) eq 0 then pathre_c4_f5 = fltarr(401,401)
if keyword_set(pathre_c4_f6) eq 0 then pathre_c4_f6 = fltarr(401,401)

if keyword_set(ffce_c1_f1) eq 0 then ffce_c1_f1 = fltarr(401,401)
if keyword_set(ffce_c1_f2) eq 0 then ffce_c1_f2 = fltarr(401,401)
if keyword_set(ffce_c1_f3) eq 0 then ffce_c1_f3 = fltarr(401,401)
if keyword_set(ffce_c1_f4) eq 0 then ffce_c1_f4 = fltarr(401,401)
if keyword_set(ffce_c1_f5) eq 0 then ffce_c1_f5 = fltarr(401,401)
if keyword_set(ffce_c1_f6) eq 0 then ffce_c1_f6 = fltarr(401,401)
if keyword_set(ffce_c2_f1) eq 0 then ffce_c2_f1 = fltarr(401,401)
if keyword_set(ffce_c2_f2) eq 0 then ffce_c2_f2 = fltarr(401,401)
if keyword_set(ffce_c2_f3) eq 0 then ffce_c2_f3 = fltarr(401,401)
if keyword_set(ffce_c2_f4) eq 0 then ffce_c2_f4 = fltarr(401,401)
if keyword_set(ffce_c2_f5) eq 0 then ffce_c2_f5 = fltarr(401,401)
if keyword_set(ffce_c2_f6) eq 0 then ffce_c2_f6 = fltarr(401,401)
if keyword_set(ffce_c3_f1) eq 0 then ffce_c3_f1 = fltarr(401,401)
if keyword_set(ffce_c3_f2) eq 0 then ffce_c3_f2 = fltarr(401,401)
if keyword_set(ffce_c3_f3) eq 0 then ffce_c3_f3 = fltarr(401,401)
if keyword_set(ffce_c3_f4) eq 0 then ffce_c3_f4 = fltarr(401,401)
if keyword_set(ffce_c3_f5) eq 0 then ffce_c3_f5 = fltarr(401,401)
if keyword_set(ffce_c3_f6) eq 0 then ffce_c3_f6 = fltarr(401,401)
if keyword_set(ffce_c4_f1) eq 0 then ffce_c4_f1 = fltarr(401,401)
if keyword_set(ffce_c4_f2) eq 0 then ffce_c4_f2 = fltarr(401,401)
if keyword_set(ffce_c4_f3) eq 0 then ffce_c4_f3 = fltarr(401,401)
if keyword_set(ffce_c4_f4) eq 0 then ffce_c4_f4 = fltarr(401,401)
if keyword_set(ffce_c4_f5) eq 0 then ffce_c4_f5 = fltarr(401,401)
if keyword_set(ffce_c4_f6) eq 0 then ffce_c4_f6 = fltarr(401,401)

if keyword_set(rdx_c1_f1) eq 0 then rdx_c1_f1 = fltarr(401,401)
if keyword_set(rdx_c1_f2) eq 0 then rdx_c1_f2 = fltarr(401,401)
if keyword_set(rdx_c1_f3) eq 0 then rdx_c1_f3 = fltarr(401,401)
if keyword_set(rdx_c1_f4) eq 0 then rdx_c1_f4 = fltarr(401,401)
if keyword_set(rdx_c1_f5) eq 0 then rdx_c1_f5 = fltarr(401,401)
if keyword_set(rdx_c1_f6) eq 0 then rdx_c1_f6 = fltarr(401,401)
if keyword_set(rdx_c2_f1) eq 0 then rdx_c2_f1 = fltarr(401,401)
if keyword_set(rdx_c2_f2) eq 0 then rdx_c2_f2 = fltarr(401,401)
if keyword_set(rdx_c2_f3) eq 0 then rdx_c2_f3 = fltarr(401,401)
if keyword_set(rdx_c2_f4) eq 0 then rdx_c2_f4 = fltarr(401,401)
if keyword_set(rdx_c2_f5) eq 0 then rdx_c2_f5 = fltarr(401,401)
if keyword_set(rdx_c2_f6) eq 0 then rdx_c2_f6 = fltarr(401,401)
if keyword_set(rdx_c3_f1) eq 0 then rdx_c3_f1 = fltarr(401,401)
if keyword_set(rdx_c3_f2) eq 0 then rdx_c3_f2 = fltarr(401,401)
if keyword_set(rdx_c3_f3) eq 0 then rdx_c3_f3 = fltarr(401,401)
if keyword_set(rdx_c3_f4) eq 0 then rdx_c3_f4 = fltarr(401,401)
if keyword_set(rdx_c3_f5) eq 0 then rdx_c3_f5 = fltarr(401,401)
if keyword_set(rdx_c3_f6) eq 0 then rdx_c3_f6 = fltarr(401,401)
if keyword_set(rdx_c4_f1) eq 0 then rdx_c4_f1 = fltarr(401,401)
if keyword_set(rdx_c4_f2) eq 0 then rdx_c4_f2 = fltarr(401,401)
if keyword_set(rdx_c4_f3) eq 0 then rdx_c4_f3 = fltarr(401,401)
if keyword_set(rdx_c4_f4) eq 0 then rdx_c4_f4 = fltarr(401,401)
if keyword_set(rdx_c4_f5) eq 0 then rdx_c4_f5 = fltarr(401,401)
if keyword_set(rdx_c4_f6) eq 0 then rdx_c4_f6 = fltarr(401,401)

if keyword_set(tgn_c1_f1) eq 0 then tgn_c1_f1 = fltarr(401,401)
if keyword_set(tgn_c1_f2) eq 0 then tgn_c1_f2 = fltarr(401,401)
if keyword_set(tgn_c1_f3) eq 0 then tgn_c1_f3 = fltarr(401,401)
if keyword_set(tgn_c1_f4) eq 0 then tgn_c1_f4 = fltarr(401,401)
if keyword_set(tgn_c1_f5) eq 0 then tgn_c1_f5 = fltarr(401,401)
if keyword_set(tgn_c1_f6) eq 0 then tgn_c1_f6 = fltarr(401,401)
if keyword_set(tgn_c2_f1) eq 0 then tgn_c2_f1 = fltarr(401,401)
if keyword_set(tgn_c2_f2) eq 0 then tgn_c2_f2 = fltarr(401,401)
if keyword_set(tgn_c2_f3) eq 0 then tgn_c2_f3 = fltarr(401,401)
if keyword_set(tgn_c2_f4) eq 0 then tgn_c2_f4 = fltarr(401,401)
if keyword_set(tgn_c2_f5) eq 0 then tgn_c2_f5 = fltarr(401,401)
if keyword_set(tgn_c2_f6) eq 0 then tgn_c2_f6 = fltarr(401,401)
if keyword_set(tgn_c3_f1) eq 0 then tgn_c3_f1 = fltarr(401,401)
if keyword_set(tgn_c3_f2) eq 0 then tgn_c3_f2 = fltarr(401,401)
if keyword_set(tgn_c3_f3) eq 0 then tgn_c3_f3 = fltarr(401,401)
if keyword_set(tgn_c3_f4) eq 0 then tgn_c3_f4 = fltarr(401,401)
if keyword_set(tgn_c3_f5) eq 0 then tgn_c3_f5 = fltarr(401,401)
if keyword_set(tgn_c3_f6) eq 0 then tgn_c3_f6 = fltarr(401,401)
if keyword_set(tgn_c4_f1) eq 0 then tgn_c4_f1 = fltarr(401,401)
if keyword_set(tgn_c4_f2) eq 0 then tgn_c4_f2 = fltarr(401,401)
if keyword_set(tgn_c4_f3) eq 0 then tgn_c4_f3 = fltarr(401,401)
if keyword_set(tgn_c4_f4) eq 0 then tgn_c4_f4 = fltarr(401,401)
if keyword_set(tgn_c4_f5) eq 0 then tgn_c4_f5 = fltarr(401,401)
if keyword_set(tgn_c4_f6) eq 0 then tgn_c4_f6 = fltarr(401,401)

if keyword_set(trs_c1_f1) eq 0 then trs_c1_f1 = fltarr(401,401)
if keyword_set(trs_c1_f2) eq 0 then trs_c1_f2 = fltarr(401,401)
if keyword_set(trs_c1_f3) eq 0 then trs_c1_f3 = fltarr(401,401)
if keyword_set(trs_c1_f4) eq 0 then trs_c1_f4 = fltarr(401,401)
if keyword_set(trs_c1_f5) eq 0 then trs_c1_f5 = fltarr(401,401)
if keyword_set(trs_c1_f6) eq 0 then trs_c1_f6 = fltarr(401,401)
if keyword_set(trs_c2_f1) eq 0 then trs_c2_f1 = fltarr(401,401)
if keyword_set(trs_c2_f2) eq 0 then trs_c2_f2 = fltarr(401,401)
if keyword_set(trs_c2_f3) eq 0 then trs_c2_f3 = fltarr(401,401)
if keyword_set(trs_c2_f4) eq 0 then trs_c2_f4 = fltarr(401,401)
if keyword_set(trs_c2_f5) eq 0 then trs_c2_f5 = fltarr(401,401)
if keyword_set(trs_c2_f6) eq 0 then trs_c2_f6 = fltarr(401,401)
if keyword_set(trs_c3_f1) eq 0 then trs_c3_f1 = fltarr(401,401)
if keyword_set(trs_c3_f2) eq 0 then trs_c3_f2 = fltarr(401,401)
if keyword_set(trs_c3_f3) eq 0 then trs_c3_f3 = fltarr(401,401)
if keyword_set(trs_c3_f4) eq 0 then trs_c3_f4 = fltarr(401,401)
if keyword_set(trs_c3_f5) eq 0 then trs_c3_f5 = fltarr(401,401)
if keyword_set(trs_c3_f6) eq 0 then trs_c3_f6 = fltarr(401,401)
if keyword_set(trs_c4_f1) eq 0 then trs_c4_f1 = fltarr(401,401)
if keyword_set(trs_c4_f2) eq 0 then trs_c4_f2 = fltarr(401,401)
if keyword_set(trs_c4_f3) eq 0 then trs_c4_f3 = fltarr(401,401)
if keyword_set(trs_c4_f4) eq 0 then trs_c4_f4 = fltarr(401,401)
if keyword_set(trs_c4_f5) eq 0 then trs_c4_f5 = fltarr(401,401)
if keyword_set(trs_c4_f6) eq 0 then trs_c4_f6 = fltarr(401,401)

theta_k = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc) -->six possible freqs for each sc
                                ;this comes from 2 freqs and three cc pairs
theta_k(*,*,0,0) = theta_k_c1_f1 & theta_k(*,*,1,0) = theta_k_c1_f2 & theta_k(*,*,2,0) = theta_k_c1_f3
theta_k(*,*,3,0) = theta_k_c1_f4 & theta_k(*,*,4,0) = theta_k_c1_f5 & theta_k(*,*,5,0) = theta_k_c1_f6
theta_k(*,*,0,1) = theta_k_c2_f1 & theta_k(*,*,1,1) = theta_k_c2_f2 & theta_k(*,*,2,1) = theta_k_c2_f3
theta_k(*,*,3,1) = theta_k_c2_f4 & theta_k(*,*,4,1) = theta_k_c2_f5 & theta_k(*,*,5,1) = theta_k_c2_f6
theta_k(*,*,0,2) = theta_k_c3_f1 & theta_k(*,*,1,2) = theta_k_c3_f2 & theta_k(*,*,2,2) = theta_k_c3_f3
theta_k(*,*,3,2) = theta_k_c3_f4 & theta_k(*,*,4,2) = theta_k_c3_f5 & theta_k(*,*,5,2) = theta_k_c3_f6
theta_k(*,*,0,3) = theta_k_c4_f1 & theta_k(*,*,1,3) = theta_k_c4_f2 & theta_k(*,*,2,3) = theta_k_c4_f3
theta_k(*,*,3,3) = theta_k_c4_f4 & theta_k(*,*,4,3) = theta_k_c4_f5 & theta_k(*,*,5,3) = theta_k_c4_f6

theta_g = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
theta_g(*,*,0,0) = theta_g_c1_f1 & theta_g(*,*,1,0) = theta_g_c1_f2 & theta_g(*,*,2,0) = theta_g_c1_f3
theta_g(*,*,3,0) = theta_g_c1_f4 & theta_g(*,*,4,0) = theta_g_c1_f5 & theta_g(*,*,5,0) = theta_g_c1_f6
theta_g(*,*,0,1) = theta_g_c2_f1 & theta_g(*,*,1,1) = theta_g_c2_f2 & theta_g(*,*,2,1) = theta_g_c2_f3
theta_g(*,*,3,1) = theta_g_c2_f4 & theta_g(*,*,4,1) = theta_g_c2_f5 & theta_g(*,*,5,1) = theta_g_c2_f6
theta_g(*,*,0,2) = theta_g_c3_f1 & theta_g(*,*,1,2) = theta_g_c3_f2 & theta_g(*,*,2,2) = theta_g_c3_f3
theta_g(*,*,3,2) = theta_g_c3_f4 & theta_g(*,*,4,2) = theta_g_c3_f5 & theta_g(*,*,5,2) = theta_g_c3_f6
theta_g(*,*,0,3) = theta_g_c4_f1 & theta_g(*,*,1,3) = theta_g_c4_f2 & theta_g(*,*,2,3) = theta_g_c4_f3
theta_g(*,*,3,3) = theta_g_c4_f4 & theta_g(*,*,4,3) = theta_g_c4_f5 & theta_g(*,*,5,3) = theta_g_c4_f6

pathre = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
pathre(*,*,0,0) = pathre_c1_f1 & pathre(*,*,1,0) = pathre_c1_f2 & pathre(*,*,2,0) = pathre_c1_f3
pathre(*,*,3,0) = pathre_c1_f4 & pathre(*,*,4,0) = pathre_c1_f5 & pathre(*,*,5,0) = pathre_c1_f6
pathre(*,*,0,1) = pathre_c2_f1 & pathre(*,*,1,1) = pathre_c2_f2 & pathre(*,*,2,1) = pathre_c2_f3
pathre(*,*,3,1) = pathre_c2_f4 & pathre(*,*,4,1) = pathre_c2_f5 & pathre(*,*,5,1) = pathre_c2_f6
pathre(*,*,0,2) = pathre_c3_f1 & pathre(*,*,1,2) = pathre_c3_f2 & pathre(*,*,2,2) = pathre_c3_f3
pathre(*,*,3,2) = pathre_c3_f4 & pathre(*,*,4,2) = pathre_c3_f5 & pathre(*,*,5,2) = pathre_c3_f6
pathre(*,*,0,3) = pathre_c4_f1 & pathre(*,*,1,3) = pathre_c4_f2 & pathre(*,*,2,3) = pathre_c4_f3
pathre(*,*,3,3) = pathre_c4_f4 & pathre(*,*,4,3) = pathre_c4_f5 & pathre(*,*,5,3) = pathre_c4_f6

trs = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
trs(*,*,0,0) = trs_c1_f1 & trs(*,*,1,0) = trs_c1_f2 & trs(*,*,2,0) = trs_c1_f3
trs(*,*,3,0) = trs_c1_f4 & trs(*,*,4,0) = trs_c1_f5 & trs(*,*,5,0) = trs_c1_f6
trs(*,*,0,1) = trs_c2_f1 & trs(*,*,1,1) = trs_c2_f2 & trs(*,*,2,1) = trs_c2_f3
trs(*,*,3,1) = trs_c2_f4 & trs(*,*,4,1) = trs_c2_f5 & trs(*,*,5,1) = trs_c2_f6
trs(*,*,0,2) = trs_c3_f1 & trs(*,*,1,2) = trs_c3_f2 & trs(*,*,2,2) = trs_c3_f3
trs(*,*,3,2) = trs_c3_f4 & trs(*,*,4,2) = trs_c3_f5 & trs(*,*,5,2) = trs_c3_f6
trs(*,*,0,3) = trs_c4_f1 & trs(*,*,1,3) = trs_c4_f2 & trs(*,*,2,3) = trs_c4_f3
trs(*,*,3,3) = trs_c4_f4 & trs(*,*,4,3) = trs_c4_f5 & trs(*,*,5,3) = trs_c4_f6

tgn = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
tgn(*,*,0,0) = tgn_c1_f1 & tgn(*,*,1,0) = tgn_c1_f2 & tgn(*,*,2,0) = tgn_c1_f3
tgn(*,*,3,0) = tgn_c1_f4 & tgn(*,*,4,0) = tgn_c1_f5 & tgn(*,*,5,0) = tgn_c1_f6
tgn(*,*,0,1) = tgn_c2_f1 & tgn(*,*,1,1) = tgn_c2_f2 & tgn(*,*,2,1) = tgn_c2_f3
tgn(*,*,3,1) = tgn_c2_f4 & tgn(*,*,4,1) = tgn_c2_f5 & tgn(*,*,5,1) = tgn_c2_f6
tgn(*,*,0,2) = tgn_c3_f1 & tgn(*,*,1,2) = tgn_c3_f2 & tgn(*,*,2,2) = tgn_c3_f3
tgn(*,*,3,2) = tgn_c3_f4 & tgn(*,*,4,2) = tgn_c3_f5 & tgn(*,*,5,2) = tgn_c3_f6
tgn(*,*,0,3) = tgn_c4_f1 & tgn(*,*,1,3) = tgn_c4_f2 & tgn(*,*,2,3) = tgn_c4_f3
tgn(*,*,3,3) = tgn_c4_f4 & tgn(*,*,4,3) = tgn_c4_f5 & tgn(*,*,5,3) = tgn_c4_f6

ffce = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
ffce(*,*,0,0) = ffce_c1_f1 & ffce(*,*,1,0) = ffce_c1_f2 & ffce(*,*,2,0) = ffce_c1_f3
ffce(*,*,3,0) = ffce_c1_f4 & ffce(*,*,4,0) = ffce_c1_f5 & ffce(*,*,5,0) = ffce_c1_f6
ffce(*,*,0,1) = ffce_c2_f1 & ffce(*,*,1,1) = ffce_c2_f2 & ffce(*,*,2,1) = ffce_c2_f3
ffce(*,*,3,1) = ffce_c2_f4 & ffce(*,*,4,1) = ffce_c2_f5 & ffce(*,*,5,1) = ffce_c2_f6
ffce(*,*,0,2) = ffce_c3_f1 & ffce(*,*,1,2) = ffce_c3_f2 & ffce(*,*,2,2) = ffce_c3_f3
ffce(*,*,3,2) = ffce_c3_f4 & ffce(*,*,4,2) = ffce_c3_f5 & ffce(*,*,5,2) = ffce_c3_f6
ffce(*,*,0,3) = ffce_c4_f1 & ffce(*,*,1,3) = ffce_c4_f2 & ffce(*,*,2,3) = ffce_c4_f3
ffce(*,*,3,3) = ffce_c4_f4 & ffce(*,*,4,3) = ffce_c4_f5 & ffce(*,*,5,3) = ffce_c4_f6

rdx = fltarr(401,401,6,4)   ;(401,401,nfreqs,nsc)
rdx(*,*,0,0) = rdx_c1_f1 & rdx(*,*,1,0) = rdx_c1_f2 & rdx(*,*,2,0) = rdx_c1_f3
rdx(*,*,3,0) = rdx_c1_f4 & rdx(*,*,4,0) = rdx_c1_f5 & rdx(*,*,5,0) = rdx_c1_f6
rdx(*,*,0,1) = rdx_c2_f1 & rdx(*,*,1,1) = rdx_c2_f2 & rdx(*,*,2,1) = rdx_c2_f3
rdx(*,*,3,1) = rdx_c2_f4 & rdx(*,*,4,1) = rdx_c2_f5 & rdx(*,*,5,1) = rdx_c2_f6
rdx(*,*,0,2) = rdx_c3_f1 & rdx(*,*,1,2) = rdx_c3_f2 & rdx(*,*,2,2) = rdx_c3_f3
rdx(*,*,3,2) = rdx_c3_f4 & rdx(*,*,4,2) = rdx_c3_f5 & rdx(*,*,5,2) = rdx_c3_f6
rdx(*,*,0,3) = rdx_c4_f1 & rdx(*,*,1,3) = rdx_c4_f2 & rdx(*,*,2,3) = rdx_c4_f3
rdx(*,*,3,3) = rdx_c4_f4 & rdx(*,*,4,3) = rdx_c4_f5 & rdx(*,*,5,3) = rdx_c4_f6

tmp = where(theta_k gt 90.)
if tmp(0) ne -1 then theta_k(tmp) = 180 - theta_k(tmp)
tmp = where(theta_g gt 90.)
if tmp(0) ne -1 then theta_g(tmp) = 180 - theta_g(tmp)
tmp = where(rdx gt 90.)
if tmp(0) ne -1 then rdx(tmp) = 180 - rdx(tmp)
tmp = where(trs gt 90.)
if tmp(0) ne -1 then trs(tmp) = 180 - trs(tmp)
tmp = where(tgn gt 90.)
if tmp(0) ne -1 then tgn(tmp) = 180 - tgn(tmp)

end  ;readfiles

;######################################################################################################
;######################################################################################################
;######################################################################################################
pro histcomb
common raystuff,theta_k,not_keepers_source,keepers_source,not_keepers_ws,keepers_ws,tgn,trs,unique_freqs

device,decomposed=0
xvals = indgen(16)*5

;##############################################################
;WHOLE SOURCE HISTOGRAMS
;##############################################################
;#############################################################
;****FINAL****PLAIN AND SIMPLE HISTOGRAM FOR WHOLE SOURCE
;source --> actual overlap b/t all 6 cc plots
;whole source --> source + filled in regions 
;##############################################################
prev_hist = ''
read,prev_hist,prompt='WOULD YOU LIKE TO LOAD PREVIOUS HISTOGRAM SET(S) (y/n)?'
if prev_hist eq 'y' then begin
    print,'LOADING A SINGLE SET: PROGRAM WILL COMBINE THIS SET WITH THE raydataX.dat FILES'
    print,'LOADING MULTIPLE SETS: PROGRAM WILL COMBINE THESE SETS TOGETHER'
endif
;doing this will combine the results of a previous run of this program with the current run
;use this when I want to add a new event to old results. 
;the program runs in the exact same manner as usual. The only difference is the old 
;master_hist is used instead of a new blank one. 


if prev_hist eq 'n' then begin
    master_hist = fltarr(16,20,4) ;16 --> number of 5 degree freq bins
                                ;20 --> number of 500 Hz bins from 0 - 10000 Hz
                                ;4  --> number of sc
    count_overall = fltarr(20,4) ;tallies # of histograms in each bin for all events combined
    n_files = 1
endif
count_tmp = fltarr(20,4) ;this tallies the # of histograms in each bin for a single event

if prev_hist eq 'y' then begin
    pathnm = dialog_pickfile(/read,filter='*.dat',/multiple_files)
    restore,pathnm(0)
    tmp = where(pathnm ne '')
    if tmp(0) ne -1 then n_files = n_elements(tmp)
    if tmp(0) eq -1 then print,'**********NO FILE SELECTED**********'
    if tmp(0) eq -1 then stop
    if n_files eq 1 then for j=0,15 do master_hist(j,*,*) = master_hist(j,*,*) * count_overall ;UNNORMALIZE SO I CAN ADD NEW EVENT
        
endif
freq_bin_siz = 500              ;Hz

loop = 'n'
if prev_hist eq 'n' or n_files eq 1 then read,loop,prompt='COPY FIRST *.DAT FILES (ENTER WHEN READY) '
;#######################################
while loop ne 'n' do begin
    if n_files eq 1 then begin  ;FOR CREATING NEW HISTOGRAMS
        readfiles
        src_slct = strarr(6,4)
        src_slct(*,*) = 'x'
        tmp = where(unique_freqs eq 0.)
        if tmp(0) ne -1 then src_slct(tmp) = 'skip'
        for sc=0,3 do begin
            for freq=0,5 do begin
                if unique_freqs(freq,sc) ne 0. then begin
                    tmp=''
                    repeat begin
                        read,tmp,prompt='SC ' + strtrim(sc+1,2)+ ', freq= '+ strtrim(unique_freqs(freq,sc),2) + ' :w (use whole source?), s (use source?), k (dont use): '
                    endrep until tmp eq 's' or tmp eq 'w' or tmp eq 'k' or tmp eq 'exit'
                    src_slct(freq,sc) = tmp
                endif
            endfor
        endfor

        for sc=0,3 do begin
            
                for freq=0,5 do begin
                    if src_slct(freq,sc) eq 'w' then tmp = (-1)*(not_keepers_ws(*,*)-1)
                    if src_slct(freq,sc) eq 's'  then tmp = (-1)*(not_keepers_source(*,*)-1)  
                    if src_slct(freq,sc) eq 'k' then tmp = -1                  

                    if tmp(0) ne -1 then src = where((tmp ne -10000) and (tmp ne 0))                                     
                    if tmp(0) eq -1 then src = -1
                    
                    if src(0) ne -1 then begin    
                        n_sourcepts = strtrim(n_elements(src),2)
                        arrtmp = tgn(*,*,freq,sc)                       
                        if src(0) ne -1 then max1 = max(arrtmp(src))/5.
                        if src(0) ne -1 then min1 = min(arrtmp(src))/5.                       
                                ;######## 
                        arrtmp = theta_k(*,*,freq,sc)                        
                        bin = ceil(unique_freqs(freq,sc)/freq_bin_siz)
                        if bin ne 0 then begin
                            master_hist(*,bin-1,sc) = master_hist(*,bin-1,sc) + histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(src))
                            count_tmp(bin-1,sc) = count_tmp(bin-1,sc) + 1
                        endif
                                ;########
                        arrtmp = trs(*,*,freq,sc)                      
                        if src(0) ne -1 then max1 = max(arrtmp(src))
                        if src(0) ne -1 then min1 = min(arrtmp(src))
                        max1 = max1/5.
                        min1 = min1/5.                                               
                        
                        if sc eq 0 then title = 'C1 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,0),2)
                        if sc eq 1 then title = 'C2 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,1),2)
                        if sc eq 2 then title = 'C3 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,2),2)
                        if sc eq 3 then title = 'C4 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,3),2)
                        
                    endif       ;for src(0) ne -1                  
                endfor ;freq eq 0 to 5 ...for each possible freq for single sc
                freq=0           
            
        endfor                  ;each sc
        sc = 0.
        read,loop,prompt='READ ANOTHER *.DAT EVENT (y/n)?: '
        tmp=''
        
        if loop eq 'y' then read,tmp,prompt='COPY NEXT *.DAT FILES (ENTER WHEN READY)... '
        if loop eq 'n' then begin
            
            ;if prev_hist eq 'n' then count_overall = count_overall + count_tmp
            ;if prev_hist eq 'y' then begin
;I only do this if I've already loaded a previous *.dat file. 
            ;    count_overall = count_overall + count_tmp              
            ;endif
            
            count_overall = count_overall + count_tmp
            for i=0,15 do master_hist(i,*,*) = master_hist(i,*,*)/float(count_overall)  ;renormalize
            
            nans = where(finite(master_hist) eq 0)
            if nans(0) ne -1 then master_hist(nans) = 0.
        endif
    endif                       ;FOR CREATING NEW HISTOGRAMS
endwhile                        ;for each event
;######################################################################################

if n_files gt 1 then begin
;IF I'M ONLY COMBINING ALREADY MADE HISTOGRAMS FROM THE FILES
    
    master_hist_tmp = fltarr(16,20,4)
    
    filecount = 0.
    count_tmp = fltarr(20)
    while filecount lt n_files do begin
        restore,pathnm(filecount)
        master_hist = master_hist + master_hist_tmp
        filecount = filecount + 1
        master_hist_tmp = master_hist
        for i=0,19 do begin
            tmp = where(count_overall(i,*) ne 0.)
            if tmp(0) ne -1 then count_tmp(i) = count_tmp(i) + 1
        endfor
    endwhile
    master_hist = master_hist_tmp
    for i=0,15 do for j=0,3 do master_hist(i,*,j) = master_hist(i,*,j)/float(count_tmp)
    count_overall = count_tmp
endif                           ;FOR n_files gt 1

comb_sc = ''
read,comb_sc,prompt='WOULD YOU LIKE TO COMBINE THE 4 SC (y/n)? '
if comb_sc eq 'y' then begin
    
    master_hist_comb = fltarr(16,20)
    sc_count = fltarr(20)
    for i=0,3 do begin
        master_hist_comb = master_hist_comb + master_hist(*,*,i)
        for j=0,19 do begin
            tmp = where(master_hist_comb(*,j) ne 0.)
            if tmp(0) ne -1 then sc_count(j) = sc_count(j) + 1 ;add how many of the four sc have a freq in a particular freq bin
        endfor
    endfor
    for j=0,19 do master_hist_comb(*,j) = master_hist_comb(*,j)/sc_count(j)
    tmp = where(finite(master_hist_comb) eq 0.)
    master_hist_comb(tmp) = 0.
    
    
    
    for i=0,4 do begin          ;for each four freq bins
        
        set_plot,'ps'
        if i eq 0 then device,filename='hist_comb_0-2000Hz.ps'
        if i eq 1 then device,filename='hist_comb_2000-4000Hz.ps'
        if i eq 2 then device,filename='hist_comb_4000-6000Hz.ps'
        if i eq 3 then device,filename='hist_comb_6000-8000Hz.ps'
        if i eq 4 then device,filename='hist_comb_8000-10000Hz.ps'
        
        !p.font=0
        !p.multi=[0,2,2]
        
        if n_files eq 1 then begin
            cnt1 = floor(count_overall(i*4+0,0) + count_overall(i*4+0,1) + count_overall(i*4+0,2) + count_overall(i*4+0,3)) ;all 4 sc for first of 4 frq bins in current file
            cnt2 = floor(count_overall(i*4+1,0) + count_overall(i*4+1,1) + count_overall(i*4+1,2) + count_overall(i*4+1,3))
            cnt3 = floor(count_overall(i*4+2,0) + count_overall(i*4+2,1) + count_overall(i*4+2,2) + count_overall(i*4+2,3))
            cnt4 = floor(count_overall(i*4+3,0) + count_overall(i*4+3,1) + count_overall(i*4+3,2) + count_overall(i*4+3,3))
        endif
        
        if n_files gt 1 then begin
            cnt1 = floor(count_overall(i*4+0))
            cnt2 = floor(count_overall(i*4+1))
            cnt3 = floor(count_overall(i*4+2))
            cnt4 = floor(count_overall(i*4+3))
        endif
        bar_graph,xvals,master_hist_comb(*,i*4+0),barcolor=25,barborder=255,title='fr=' + strtrim(i*2000+0,2) + '-' + strtrim(i*2000+500,2) + 'Hz for ' + strtrim(cnt1,2) + ' events'
        bar_graph,xvals,master_hist_comb(*,i*4+1),barcolor=25,barborder=255,title='fr=' + strtrim(i*2000+500,2) + '-' + strtrim(i*2000+1000,2) + 'Hz for ' + strtrim(cnt2,2) + ' events'
        bar_graph,xvals,master_hist_comb(*,i*4+2),barcolor=25,barborder=255,title='fr=' + strtrim(i*2000+1000,2) + '-' + strtrim(i*2000+1500,2) + 'Hz for ' + strtrim(cnt3,2) + ' events'
        bar_graph,xvals,master_hist_comb(*,i*4+3),barcolor=25,barborder=255,title='fr=' + strtrim(i*2000+1500,2) + '-' + strtrim(i*2000+2000,2) + 'Hz for ' + strtrim(cnt4,2) + ' events'
        
        device,/close
        !p.font=-1
        set_plot,'x'     
        
        
    endfor
    
endif                           ;for comb_sc


if n_files eq 1 then begin
    tmp=''
    read,tmp,prompt='WOULD YOU LIKE TO SAVE DATA AS histograms.dat (y/n)?'
    if tmp eq 'y' then save,file='histograms.dat',master_hist,count_overall
endif

end
