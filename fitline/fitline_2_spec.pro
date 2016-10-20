;NEW VERSION OF FITLINE.PRO. THE OLD VERSION IS BOUND TO IVAR'S PROGRAMS

;1. input FFT spec
;2. Calculate max power/channel
;3. Plot line as well as sigma variations




;###########################################################################
;This program fits a line to the cross-correlation array plot (CorArr) and the
;Spectrogram plots (Spec) generated from Ivar's view-spec-simple.pro 
;program. Also the one-sigma variation from this line is generated. 
;Run Ivar's program to generate the arrays and then run this program. 

;Specify 'single' or 'cc' for the single spacecraft plot or the
;cross-correlation plot. 
;See what the plot looks like
;Window the plot by selecting the frequency and delay range using
;the 'chopfreq' and 'chopdelay' procedures. 

;LMFIT is used to find the one-sigma variation of the slope and
;intercept by holding one constant while varying the other until the
;chisqr is of the correct value for one-sigma. 
;############################################################################





;this is the first part of my slope fitting routing for the
;cross-correlation plots

;the 'prob' that linfit calculates has nothing to do with the standard
;deviation given. If the numbers are perfectly linear, despite a
;horrible standard deviation, prob = 1. Should probably ignore this as
;a measure. 





;#######################################################################
;rcorrprob functions.
;call rcorrprob with probabilty of null result and number of
;points. For example: 99% confidence is probability of 0.01
;if the number of points is 100, then the cal is:
; rnull = rcorrprob(0.01,100)
;
;

;#####################
function integrand, r
common npoints, npoints
  integrand = (1-r*r)^((npoints-4)/2)
return, integrand
end
;#####################

function corrprob, r_zero, nval
common npoints, npoints

npoints = nval

coeff = (2./sqrt(!pi))*exp(lngamma((nval-1)/2.)-lngamma((nval-2)/2.))
;coeff1 = (2./sqrt(!pi))*gamma((nval-1)/2.)/gamma((nval-2)/2.)

;print, 'coeff = ',coeff;, coeff1


lim1 = r_zero
lim2 = 1.0

prob = coeff*qromb('integrand',lim1,lim2,/double)

return, prob

end
;#######################3

function func, r
common cprobvals, prob, npoints

;print,'****enter func****'
;r=fltarr(3)
cprob = r
;print, r

badr = where(r gt 1, badrcnt)
if badrcnt ne 0 then r(badr)=1.0
badr = where(r lt 0, badrcnt)
if badrcnt ne 0 then r(badr)=0.0

ndx = size(r)
;print,'size(r): ', ndx
lim = ndx(1)-1
if ndx(0) eq 0 then lim=0

for i=0,lim do begin
 rin = r(i)
 cprob(i) = corrprob(rin, npoints)
endfor
;print, 'prob,npoints: ',prob, npoints
;print, 'cprob: ', cprob
;print, 'cprob - prob: ', cprob-prob
return, cprob-prob

end
;#######################

function rcorrprob, inputprob, npts
common cprobvals, prob, npoints
forward_function corrprob

prob = inputprob
npoints = npts

deltar = .01
deltaprob = 1.0
rkeep = 0

for i=0,100 do begin
 rfind = i*deltar
 rprob = corrprob(rfind, npoints)
 if abs(rprob - prob) lt deltaprob then begin
   deltaprob = abs(rprob-prob)
   rkeep = rfind
 endif
endfor

;rcheck = indgen(21)*deltar
;print, rcheck
;print, func(rcheck)

;stop

if rkeep eq 1.0 then rkeep = 1.0-deltar
if rkeep eq 0.0 then rkeep = 0.0+deltar
r = [rkeep-deltar,rkeep,rkeep+deltar]
;print,'Initial r vector: ', r

r0 = fx_root(r, 'func',/double)

return, r0

end
;###################
;#######################
function MYFUNCT, x, A
  ;creates the y=mx+b and its partial derivatives with respect to the 
;parameters that LINFIT has found values for (m and b). For the LMFIT function
  return, [[A(1)*x + A(0)],[x],[1]]
end













;#######################
;#######################
;#######################
pro FITLINE
;  common Colours, Black, White, Grey, Red, Green, Blue, Yellow, Amber
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
;'main'
  cc_or_single = 'cc'
  adj_or_not = 'no'  
;if adj_or_not = 'yes' then maxcc_adj is used -->i.e. the point chosen
;for the linefit is centered on the errorbars. 
;if adj_or_not = 'no' then maxcc is used -->the point chosen is the
;max value for each channel. 

  filepath=''
  filepath = dialog_pickfile(path='/usr/users/awb/newcode/crosscor2/',filter='*.dat') ;,/multiple_files)
;  read,filename,PROMPT='Enter name of the .dat file to read in (ex. 256-24jul03-013523-c1,c2,cc12,cc13.dat): '
  posx = strpos(filepath,'medfilt')
  posx2 = strpos(filepath,'-',posx)
  orig_filt = strmid(filepath,posx,posx2-posx)
  orig_filt = 'Filter in Ivars program was ' + orig_filt

  restore,filepath
  
  delaytimes = fltarr(2)
  delaytimes(0) = lagtime(0)
  tmp = n_elements(lagtime)
  delaytimes(1) = lagtime(tmp-1)
  nfreqs=n_elements(CorArr[0,*,0])
  ntimes=n_elements(CorArr[*,0,0])
  nsc = n_elements(Spec[0,0,*])
  npairs = n_elements(CorArr[0,0,*])
  segdeltaT = sdt
  numsegs = nsg
  delayrange = delaytimes
  channelrange = [minchan,maxchan]
  ntimesperchannel = n_elements(Spec(*,0,0))

  dchan = float(channelrange(1) - channelrange(0))
  yrang = float(freqrange(1) - freqrange(0))
  scalefactor = dchan/yrang ;conversion b/t channels and actual freqs

;###############
;These slightly adjust the ranges to correspond with Ivars plot
  freqrange_adj = (freqrange(1)-freqrange(0))/(channelrange(1)-channelrange(0))
;this adjusts the bars to the middle of the FFT box.
  delayrange_adj = (delayrange(1)-delayrange(0)+1)/float(ntimes)
  timerange_adj = segdeltat
;###############

  if(cc_or_single EQ 'cc') then begin
      timeindices = indgen(ntimes)
      slope = (delayrange(1) - delayrange(0))/(timeindices(ntimes - 1)) ;slope
  endif

  if(cc_or_single EQ 'single') then begin
      timeindices = indgen(ntimesperchannel)
      slope = (segdeltaT*numsegs)/(timeindices(ntimesperchannel - 1))  
  endif

  activechannels = RETURNACTIVECHANNELS()
  frequencies = float(activechannels)/scalefactor
  ;frequencies = frequencies - 0.5*freqrange_adj
  ;freqrange = freqrange - 0.5*freqrange_adj

;########
;this is to set all nonessential elements of Spec to zero
;if it has not already been done in Ivars program. 
  timeindicestmp = indgen(ntimesperchannel)
  slopetmp = (segdeltaT*numsegs)/(timeindicestmp(ntimesperchannel - 1))
  indexrange = floor((timerange)/slopetmp)
  seg0 = indexrange(0)
  seg1 = indexrange(1)  
  Spec(0:(seg0-1),*,*) = 0.
  tmp = n_elements(Spec(*,0,0))
  Spec((seg1+1):(tmp-1),*,*) = 0.

;#########
;################################################################
;################################################################
  if(cc_or_single EQ 'cc') then begin
      pair = 2.
;the order of the pairs (0,1,2,3,4,5) is given as
;[c1,c2],[c1,c3],[c1,c4],[c2,c3],[c2,c4],[c3,c4]

      maxcc = ''                ;the array with the time location of the maximum val of the correlation coeff for every channel
      
      tmpchannelrange = channelrange
      
      !p.multi = [2,2,0]
      sdev = ''
     
      maxcc_adj = ''
      
      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj,timerange_adj,adj_or_not

      tmpdelayrange = fltarr(2)
      tmpfreqrange = fltarr(2)
    
      tmpx = ''
      read,tmpx,PROMPT='Enter min freq for plot (kHz): '
      tmpfreqrange(0) = float(tmpx)
      read,tmpx,PROMPT='Enter max freq for plot (kHz): '
      tmpfreqrange(1) = float(tmpx)
      read,tmpx,PROMPT='Enter min delay for plot (msec): '
      tmpdelayrange(0) = float(tmpx)
      read,tmpx,PROMPT='Enter max delay for plot (msec): '
      tmpdelayrange(1) = float(tmpx)

      filt_string = ''
      CorArr =filters(CorArr,tmpfreqrange,tmpdelayrange,'median',neighbors=2)
      filt_string = filt_string + 'median=2 || '
      ;CorArr = filters(CorArr,tmpfreqrange,tmpdelayrange,'median',neighbors=2)
      ;filt_string = filt_string + 'median=2 || '

      ;CorArr = freqfilt(CorArr,tmpfreqrange,tmpdelayrange,neighbors=8)
;############################################
;this will power plot the cross-correlations
      ;POWERPLOT
      ;POWERPLOT
      ;POWERPLOT
;###########################################      
      range = RETURNTIMEINDEXRANGE(tmpdelayrange)
      windowtype = 'chopdelay'
      CorArr(*,*,pair) = WINDOW(range,windowtype,maxcc,maxstruct)
      windowtype = 'chopfreq'
      range = RETURNCHANNELRANGE(tmpfreqrange)
      CorArr(*,*,pair) = WINDOW(range,windowtype,maxcc,maxstruct)
;##############################      
      color=120
      
      sdev = fltarr(nfreqs)

      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj, $ 
        timerange_adj,maxstruct=maxstruct,adj_or_not
      STDDEVS,sdev,errdelays ;gets the std in correlation coefficients
      maxstruct = ''

      FINDMAX,maxstruct=maxstruct,null_hyp=null_hyp,removefreqs='yes',errdelays,freqrange_adj

      maxstruct.maxcc(0:limitsofmaxcc(0)-1) = 0
      maxstruct.maxcc(limitsofmaxcc(1):(n_elements(maxstruct.maxcc)-1))= 0
      
      !p.multi(0) = 0
      !p.multi = [2,2,0]
      
      tmp = transpose(CorArr(*,*,pair))
      CorArr = fltarr(nfreqs,ntimes,npairs)
      CorArr(*,*,pair) = tmp

      LINEARFIT,sdev,maxcc,maxcc_adj,rotate='yes',linestruct=linestruct,maxstruct,delaystmp=delaystmp,adj_or_not

      tmp = transpose(CorArr(*,*,pair))
      CorArr = fltarr(ntimes,nfreqs,npairs)
      CorArr(*,*,pair) = tmp

      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj, $ 
        timerange_adj,maxstruct=maxstruct,adj_or_not,postscript='yes'
      PLOTLINE,linestruct,maxstruct,filt_string,orig_filt,delaystmp,freqrange_adj,timerange_adj,delayrange_adj

      if keyword_set(postscript) then begin
          device,/close
          set_plot,'x'
      endif
      
  endif                         ;for cross-correlation plot
;#####################################################################
;#####################################################################
;#####################################################################
;#####################################################################
  if(cc_or_single EQ 'single') then begin

      null_hyp = 1e-15
      sc = 1   ;(1,2,3,4)
      sc = sc - 1
      if sc EQ -1 then begin
          print,'xxxxxxxxxxxxxxxxxxxx'
          print,'NOT A VALID SC NUMBER'
          print,'xxxxxxxxxxxxxxxxxxxx'
          STOP
      endif
      
      maxcc = ''
      tmpchannelrange = channelrange
      ;frequencies = RETURNFREQRANGE(tmpchannelrange)
      !p.multi=[2,2,0]
    
      sdev = ''
      maxcc_adj = ''
      
      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj,timerange_adj,adj_or_not

      tmpdelayrange = fltarr(2)
      tmpfreqrange = fltarr(2)
   
      tmpx = ''
      read,tmpx,PROMPT='Enter min freq for plot (kHz): '
      tmpfreqrange(0) = float(tmpx)
      read,tmpx,PROMPT='Enter max freq for plot (kHz): '
      tmpfreqrange(1) = float(tmpx)
      read,tmpx,PROMPT='Enter min time for plot (sec): '
      tmpdelayrange(0) = float(tmpx)
      read,tmpx,PROMPT='Enter max time for plot (sec): '
      tmpdelayrange(1) = float(tmpx)

      color=50
      !p.multi(0) = 0

      filt_string = ''
      ;Spec = filters(Spec,tmpfreqrange,tmpdelayrange,'median',neighbors=8)
      ;filt_string = filt_string + 'median=8 || '
      ;Spec = filters(spec,tmpfreqrange,tmpdelayrange,'median',neighbors=8)
      ;filt_string = filt_string + 'median=8 || '
      Spec = filters(spec,tmpfreqrange,tmpdelayrange,'median',neighbors=2)
      filt_string = filt_string + 'median=2 || '
      Spec = filters(spec,tmpfreqrange,tmpdelayrange,'median',neighbors=2)
      filt_string = filt_string + 'median=2 || '

;###########################
;this will power plot the single spacecraft event
      ;POWERPLOT
      ;POWERPLOT
      ;POWERPLOT
;##########################


;######################
      range = RETURNTIMEINDEXRANGE(tmpdelayrange)
      windowtype = 'chopdelay'
      Spec(*,*,sc) = WINDOW(range,windowtype,maxcc,maxstruct)
      windowtype = 'chopfreq'
      range = RETURNCHANNELRANGE(tmpfreqrange)
      Spec(*,*,sc) = WINDOW(range,windowtype,maxcc,maxstruct)
;#######################

;      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj,timerange_adj,maxstruct=maxstruct,adj_or_not
      sdev = fltarr(nfreqs)
      STDDEVS,sdev,errdelays ;gets the std in correlation coefficients
      maxstruct = ''
      FINDMAX,maxstruct=maxstruct,null_hyp=null_hyp,removefreqs='yes',errdelays,freqrange_adj

      maxstruct.maxcc(0:limitsofmaxcc(0)-1) = 0
      tmp = n_elements(maxstruct.maxcc)
      maxstruct.maxcc(limitsofmaxcc(1):(tmp-1))= 0   

      !p.multi(0) = 0
      !p.multi = [2,2,0]

      tmp = transpose(Spec(*,*,sc))
      Spec = fltarr(nfreqs,n_elements(timeindices),nsc)
      Spec(*,*,sc) = tmp      
   
      LINEARFIT,sdev,maxcc,maxcc_adj,rotate='yes',linestruct=linestruct,maxstruct,delaystmp=delaystmp,adj_or_not
      
      tmp = transpose(Spec(*,*,sc))
      Spec = fltarr(n_elements(timeindices),nfreqs,nsc)
      Spec(*,*,sc) = tmp
      
;      PLOTSPEC,maxcc,sdev,maxcc_adj,null_hyp=null_hyp,freqrange_adj,delayrange_adj,timerange_adj,maxstruct=maxstruct,adj_or_not,postscript='yes'
      PLOTLINE,linestruct,maxstruct,filt_string,orig_filt,delaystmp,freqrange_adj,timerange_adj,delayrange_adj

      if keyword_set(postscript) then begin
          device,/close
          set_plot,'x'
      endif
  endif
end

;problems

;1. If the error bar in one direction is long enough that it extends
;into the cutoff region on the other side then it is ignored. Not
;really sure what to do about this except to filter until it is not a
;problem.

;2. For some events rcorrprob is called endlessly unless ctrl c is
;pressed and then continued a few times. 


;things to add
;1. window each channel separately from the point of max value
