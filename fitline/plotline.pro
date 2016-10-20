


;##########################
;###################################################################
;###################################################################
;####################################################
pro PLOTLINE,linestruct,maxstruct,rotate=rotate,filt_string,orig_filt,delaystmp,freqrange_adj,timerange_adj,delayrange_adj
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies

;even though the delaystmp have been found as delaystmp = linearr(0) +
;frequenciestmp*linearr(1) for the error bars to be vertical, here we
;want to plot them in the ordinary way. 

  not_nan = maxstruct.not_nan
  frequenciestmp = frequencies(not_nan)
;############
;adjust best fit line and 1 sigma lines to center of FFT box for
;single spacecraft linefits but not for cc fits.
  if cc_or_single EQ 'single' then begin
      delaystmp = delaystmp-0.5*timerange_adj
      linestruct.coef_maxmin = linestruct.coef_maxmin-0.5*timerange_adj
      linestruct.coef_minmax = linestruct.coef_minmax-0.5*timerange_adj
  endif
;adjust freqs to center of box
  frequenciestmp = frequenciestmp + 0.5*freqrange_adj
;############
;############
;now lets adjust the slope of the extreme rays to accomodate the error
;from the FFT box size. Do this for both single sc and cc plots

;I DON'T THINK WE WANT TO DO THIS BECAUSE THE UNCERTAINTY IN THE FFT
;BOX IS ALREADY TAKEN INTO ACCOUNT WITH THE ERROR BARS
  ;scaler = (indgen(n_elements(delaystmp))- 0.5*n_elements(delaystmp))*2/float(n_elements(delaystmp))
  ;scaler = scaler*1000*timerange_adj

  ;if cc_or_single EQ 'single' then scaler = scaler/1000.

  ;if linestruct.coef_maxmin(0) GE linestruct.coef_minmax(0) then begin
  ;    linestruct.coef_maxmin = linestruct.coef_maxmin + (-1)*scaler
  ;    linestruct.coef_minmax = linestruct.coef_minmax + scaler
  ;endif
  ;if linestruct.coef_minmax(0) GE linestruct.coef_maxmin(0) then begin
  ;    linestruct.coef_minmax = linestruct.coef_minmax + (-1)*scaler
  ;    linestruct.coef_maxmin = linestruct.coef_maxmin + scaler
  ;endif

;############
;plot the best fit line adjusted to the center of the FFT box


;I can't figure out why the following adjustment is necessary but
;otherwise the lines don't plot correctly. If I make this change to
;the arrays permanantly then the plot labelling will be messed up so
;dont change unless you figure out why. 
if cc_or_single EQ 'cc' then begin
  oplot,delaystmp-0.5*1000*timerange_adj,frequenciestmp,color=20
  oplot,linestruct.coef_minmax-0.5*1000*timerange_adj,frequenciestmp,color=50
  oplot,linestruct.coef_maxmin-0.5*1000*timerange_adj,frequenciestmp,color=50
endif

if cc_or_single EQ 'single' then begin
  oplot,delaystmp,frequenciestmp,color=20
  oplot,linestruct.coef_minmax,frequenciestmp,color=50
  oplot,linestruct.coef_maxmin,frequenciestmp,color=50
endif

;lets calculate the equations of the best fit and extreme fit lines

  dcb = linestruct.coef_line(n_elements(linestruct.coef_line)-1) - linestruct.coef_line(0)
  dmaxmin = linestruct.coef_maxmin(n_elements(linestruct.coef_maxmin)-1) - linestruct.coef_maxmin(0)
  dminmax = linestruct.coef_minmax(n_elements(linestruct.coef_minmax)-1) - linestruct.coef_minmax(0)

  basedt = abs(linestruct.coef_line(0) - linestruct.coef_minmax(0)) > abs(linestruct.coef_line(0) - linestruct.coef_maxmin(0))

  ;best fit line
  m_bf = (frequenciestmp(n_elements(frequenciestmp)-1) - frequenciestmp(0))/(dcb)
  b_bf = frequenciestmp(0) - m_bf*linestruct.coef_line(0)
  m_bfs = string(m_bf,format='(F9.4)')
  b_bfs = string(b_bf,format='(F9.4)')
  bestfit_string = 'f = ' + m_bfs + '*t + ' + b_bfs
  ;extreme 1 line
  m_e1 = (frequenciestmp(n_elements(frequenciestmp)-1) - frequenciestmp(0))/(dmaxmin)
  b_e1 = frequenciestmp(0) - m_e1*linestruct.coef_maxmin(0)
  m_e1s = string(m_e1,format='(F9.4)')
  b_e1s = string(b_e1,format='(F9.4)')
  extremefit1_string = 'f = ' + m_e1s + '*t + ' + b_e1s
  ;extreme 2 line
  m_e2 = (frequenciestmp(n_elements(frequenciestmp)-1) - frequenciestmp(0))/(dminmax)
  b_e2 = frequenciestmp(0) - m_e2*linestruct.coef_minmax(0)
  m_e2s = string(m_e2,format='(F9.4)')
  b_e2s = string(b_e2,format='(F9.4)')
  extremefit2_string = 'f = ' + m_e2s + '*t + ' + b_e2s

  fts = string(frequenciestmp,format='(F9.4)')
  cb0s = string(linestruct.coef_line,format='(F9.4)')

  cminmaxs = string(linestruct.coef_minmax,format='(F9.4)')
  cmaxmins = string(linestruct.coef_maxmin,format='(F9.4)')
  basedts = string(basedt,format='(F9.4)')

  dcbs = string(dcb,format='(F9.4)')
  
  dmaxmins = string(dmaxmin,format='(F9.4)') 
  dminmaxs = string(dminmax,format='(F9.4)')
 
  if cc_or_single EQ 'single' then s_or_ms = 'sec'
  if cc_or_single EQ 'cc' then s_or_ms = 'msec'
  
  if keyword_set(linestruct.chisqr_line) then begin
      chisqr_line_min = linestruct.chisqr_line/(n_elements(frequenciestmp)-2)
      chisqr_line_min = string(chisqr_line_min,format='(F9.4)')
      chisqr_line = string(linestruct.chisqr_line,format='(F9.4)')
  endif else chisqr_line = 'temp'

  freqrangetmpp = 'Frequency range = ' +fts(0) + ' - ' + fts(n_elements(fts)-1) + ' kHz'
 
  basedelay = 'Base fit = ' + cb0s(0) + ' ' + s_or_ms + ' +/- ' + basedts + ' ' + s_or_ms
  normaldelay = 'Best fit is [' + cb0s(0) + ' - ' + cb0s(n_elements(cb0s)-1) + '] ' + s_or_ms + ', delta-t = ' + dcbs + ' ' + s_or_ms
  extremedelay1 = 'Extreme fit 1 is [' + cmaxmins(0) + ' - ' + cmaxmins(n_elements(cmaxmins)-1) + '] ' + s_or_ms + ', delta-t = ' + dmaxmins + ' ' + s_or_ms
  extremedelay2 = 'Extreme fit 2 is [' + cminmaxs(0) + ' - ' + cminmaxs(n_elements(cminmaxs)-1) + '] ' + s_or_ms + ', delta-t = ' + dminmaxs + ' ' + s_or_ms
  bestfiteqn  = 'Eqn of best fit     :    ' + bestfit_string
  extreme1fit = 'Eqn of extreme fit 1: ' + extremefit1_string
  extreme2fit = 'Eqn of extreme fit 2: ' + extremefit2_string
  chisqr_linestr = 'CHISQR FOR LINE IS ' + chisqr_line
  chisqr_line_minstr = 'REDUCED CHISQR FOR LINE IS ' + chisqr_line_min
  helptxt = '....THE FFT BOX SIZE IS ADDED TO BASE FIT DELTA TIME'
  helptxt2 = '2X FFT BOX SIZE ADDED TO EXTREME FIT 1 AND 2 DELTA TIMES'
  
  
  xyouts,0.1,0.985,/normal,freqrangetmpp,charsize = 0.75,color=5
  xyouts,0.1,0.965,/normal,basedelay,charsize = 0.75,color=5
  xyouts,0.1,0.945,/normal,normaldelay,charsize = 0.75,color=5
  xyouts,0.1,0.925,/normal,extremedelay1,charsize = 0.75,color=5
  xyouts,0.1,0.905,/normal,extremedelay2,charsize = 0.75,color=5
  xyouts,0.1,0.885,/normal,filt_string,charsize=0.75,color=5
  xyouts,0.1,0.865,/normal,orig_filt,charsize=0.75,color=5

  xyouts,0.5,0.985,/normal,bestfiteqn,charsize=0.75,color=5
  xyouts,0.5,0.965,/normal,extreme1fit,charsize=0.75,color=5
  xyouts,0.5,0.945,/normal,extreme2fit,charsize=0.75,color=5
  xyouts,0.5,0.925,/normal,chisqr_linestr,charsize = 0.75,color=5
  xyouts,0.5,0.905,/normal,chisqr_line_minstr,charsize = 0.75,color=5
  xyouts,0.5,0.885,/normal,helptxt,charsize = 0.65,color=5
  xyouts,0.5,0.865,/normal,helptxt2,charsize= 0.65,color=5
end                             ;plotline


