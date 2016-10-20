;+
;*****************************************************************************************
;
;  FUNCTION : create_theta_kb_vals  
;  PURPOSE  : creates a spectrum of theta_kb values for the program trace.f
;			  Returns an array of both northward and southward theta values. 
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
;  INPUT:
;               
;
;  EXAMPLES:    
;               
;
;  KEYWORDS:    nthetas -> number of different theta values to create. If not set then default is 2,
;			    and program returns the initial and final values north and south
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


;Maybe create a "spacing" function that continuously varies the distance between
;theta values. 


function create_theta_kb_vals,theta0,thetaF,nthetas=nthetas,freq=freq,fce=fce,resdelta=resdelta,$
	fine_increment=fine_increment,fine_inc_angle=fin_inc_angle

if ~keyword_set(nthetas) then nthetas = 2.


;adjust final angle if necessary
if keyword_set(freq) and keyword_set(fce) then begin
	res_angle = acos(freq/fce)/!dtor
	if thetaF ge res_angle then begin
		if ~keyword_set(resdelta) then thetaF = res_angle - 1. else thetaF = res_angle - resdelta
	endif
endif



;only increment with a constant delta-theta
if ~keyword_set(fin_increment) then thetas = indgen(nthetas)*(thetaF-theta0)/(nthetas-1)


if keyword_set(fin_increment) then begin

	if ~keyword_set(fin_inc_angle) and ~keyword_set(resangle) then fin_inc_angle = 80.
	
endif




;mirror these so that they are on both sides of Bo
thetas_north = [-1*reverse(thetas[1:n_elements(thetas)-1]),thetas]
thetas_south = 180. + thetas_north

thetas = {th_north:thetas_north,th_south:thetas_south}


return,thetas



end
