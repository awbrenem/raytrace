;###################################################################
;###################################################################
;###################################################################
pro LINEARFIT,sdev,maxcc,maxcc_adj,rotate=rotate,linestruct=linestruct,maxstruct,delaystmp=delaystmp,adj_or_not
;uses IDL's LMFIT routine to fit line
;It minimized the chi-square function with respect
;to the parameters m and b in y=mx+b


;  common Colours, Black, White, Grey, Red, Green, Blue, Yellow, Amber
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
   
  maxcc_adj = maxstruct.maxcc_adj
  maxcc = maxstruct.maxcc
  errdelays = maxstruct.errdelays  
  not_nan = maxstruct.not_nan
  
  maxcc_adj = maxcc_adj(not_nan)
  maxcc = maxcc(not_nan)

;maxcc_adj centers the point on the error bars, even if it is not at
;the actual maximum for a particular channel. maxcc always has the 
;error bars centered on the maximum for each channel. 

  t1 = errdelays(not_nan,0)
  t2 = errdelays(not_nan,1)
  errdelays = fltarr(n_elements(maxcc),2)
  errdelays(*,0) = t1
  errdelays(*,1) = t2
  frequenciestmp = frequencies(not_nan)
  sdev = sdev(not_nan)

  if adj_or_not EQ 'yes' then maxcctmp = maxcc_adj
  if adj_or_not EQ 'no' then maxcctmp = maxcc

;if the stddev is zero then lets manually change it to the width of a
;single box

  zero = where(sdev EQ 0.)
  if zero(0) NE -1 then begin
      sdev(zero) = slope/2.
      ;errdelays(zero,0) = maxcc_adj(zero) - slope/2.
      ;errdelays(zero,1) = maxcc_adj(zero) + slope/2.
  endif
;################################
;now let's find the best fit line. Use LINFIT to calculate the best
;guess for the matrix A which is used in LMFIT. Also, the line 
;plotted is from LINFIT. It doesn't work to plot the line from
;LMFIT because it has to guess at both parameters, which gives 
;a horrible line fit. LMFIT is used to fine the one sigma deviation
;from the best fit line.
;################################

;must flip the x and the y so the error bars are vertical.
  linearr = linfit(frequenciestmp,maxcctmp,MEASURE_ERRORS=sdev,PROB=prob,CHISQR=chisqr_line)      ;this returns the [slope,intercept]
;now lets call LMFIT to find delta chi-square
  if(string(errdelays(0)) NE '') then measure_errors = errdelays(*,1)-errdelays(*,0) $
  else begin
      measure_errors = fltarr(n_elements(frequenciestmp))
      measure_errors(*) = 0.
  endelse
  
  if(string(errdelays(0)) NE '') then begin
      A=[5,linearr(1)]                 ;[intercept,slope]
      fita=[1,0]
      measure_errors = sdev*0.5 ;I believe this is correct. It gives the 
      ;same chisqr_line that I calculate by hand
      delaystmp = linearr(0) + frequenciestmp*linearr(1)

;using the known value of the slope calculated from LINFIT (the guess) let's find
;the equation of the line by letting LMFIT minimize the chisqr for the
;intercept
;The equation of the line that I get from coef_line and coef_line2 gives
;the same line as does LINFIT, which is good as all three should be
;the same.
      coef_line = LMFIT(frequenciestmp,maxcctmp,A,measure_errors=measure_errors,/double, $
                   FITA=fita,FUNCTION_NAME = 'myfunct',CHISQ=chisqr2)
;###
      A=[linearr(0),0.1]
      fita=[0,1]
;using the known value of the intercept from LINFIT let's find the
;equation of the line by letting LMFIT minimize the chisqr for the slope
      coef_line2 = LMFIT(frequenciestmp,maxcctmp,A,measure_errors=measure_errors,/double, $
                      FITA=fita,FUNCTION_NAME = 'myfunct',CHISQ=chisqr2)

;#####      
;now let's calculate the
;chi-squared value from the best fit line
      chisqr = abs(coef_line2 - maxcctmp)
      chisqr = chisqr/sdev
      chisqr = chisqr^2
      chisqr = total(chisqr)
;#####
      i=0
      increment=0
      chisqr_tmp = 0
      onesigma_chisqr = 2.3 + chisqr  ;the chisqr for one sigma deviation
                                ;2.3 represents the one-sigma (68%
;confidence) delta-chi-sqr value for two degrees of freedom
      ;delta-chi-sqr = chi-sqr(one-sigma) - chi-sqr(minimum)

print,'chisqr = ',chisqr
print,'one sigma chisqr = ',onesigma_chisqr


;find value of slope that corresponds to a one-sigma variation from
;the best fit line
;#########
      if cc_or_single EQ 'single' then increment2 = abs(linearr(1)/200.)
      if cc_or_single EQ 'cc' then increment2 = abs(linearr(1)/50.)
;##########
      while(chisqr_tmp LT onesigma_chisqr) do begin
          A=[linearr(0),linearr(1)+increment]
                                ;increase m with every loop and get a new chisqr
          ;for b. Do this so until the resulting chisqr 
          ;increases to the one-sigma deviation chi-sqr value for the 
          ;best fit line. This becomes the +/- b to use when
          ;plotting extreme lines.
          fita = [1,0]
          coef_b1 = LMFIT(frequenciestmp,delaystmp,A,measure_errors=measure_errors,/double, $
                       FITA=fita,FUNCTION_NAME = 'myfunct',CHISQ=chisqr_tmp)
          print,'approaching one-sigma chisqr for b = ', chisqr_tmp
          max_slope = linearr(1) + increment ;maximum and minimum slope values.
          min_slope = linearr(1) - increment ;this is the slope that has been put into the above LMFIT call
          ;and that corresponds to the current chisqr_tmp value. 
          increment = increment + increment2
          i=i+1
      endwhile

;find value of intercept that corresponds to a one-sigma variation
;from best fit line
;############
      if cc_or_single EQ 'single' then increment2 = abs(linearr(0)/500.)
      if cc_or_single EQ 'cc' then increment2 = abs(linearr(0)/50.)
;############
      i=0
      increment=0
      chisqr_tmp = 0
      while(chisqr_tmp LT onesigma_chisqr) do begin
          A=[linearr(0)+increment,linearr(1)]
          ;same as above loop but increment b to find the 
          ;one sigma m value.
          fita=[0,1]
          coef_m1 = LMFIT(frequenciestmp,delaystmp,A,measure_errors=measure_errors,/double, $
                       FITA=fita,FUNCTION_NAME = 'myfunct',CHISQ=chisqr_tmp)
          print,'approaching one-sigma chisqr for m = ', chisqr_tmp
          max_intercept = linearr(0) + increment
          min_intercept = linearr(0) - increment
          increment = increment + increment2
          i=i+1
      endwhile

;call LMFIT with the extreme coefficients for the extreme lines that 
;represent the 68% confidence level in parameter space. 
      A=[max_intercept,min_slope]
      coef_maxmin = LMFIT(frequenciestmp,delaystmp,A,measure_errors=measure_errors,/double, $
                       FUNCTION_NAME = 'myfunct',CHISQ = chisqrmaxmin)
      A=[min_intercept,max_slope]
      coef_minmax = LMFIT(frequenciestmp,delaystmp,A,measure_errors=measure_errors,/double, $
                       FUNCTION_NAME = 'myfunct',CHISQ = chisqrminmax)
      
      print,'CHISQR_MAXMIN = ', chisqrmaxmin
      print,'CHISQR_MINMAX = ', chisqrminmax

      save,filename='c2',delaystmp,frequenciestmp,coef_maxmin,coef_minmax
      linestruct = {coef_minmax:coef_minmax,coef_maxmin:coef_maxmin, $
                   coef_line:coef_line,chisqr_line:chisqr_line}
  endif
end



