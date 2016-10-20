








;##########################################################################################
pro FINDMAX,maxstruct=maxstruct,null_hyp=null_hyp,removefreqs=removefreqs,errdelays,freqrange_adj
;  common Colours, Black, White, Grey, Red, Green, Blue, Yellow, Amber
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
  
  if cc_or_single EQ 'cc' then which = pair
  if cc_or_single EQ 'single' then which = sc

  if(cc_or_single EQ 'cc') then begin
                                ;now lets find the maximum time for each
;channel and attempt to fit a crude line.
      
      activechannels = RETURNACTIVECHANNELS()
      maxcc = fltarr(n_elements(activechannels))
      
      iter = channelrange(1) - channelrange(0) + 1
      for j=0,iter-1 do begin
          x = where(Spec(*,j+channelrange(0),0) GT 0.)
;this gives the number of FFT boxes in each channel that were
;cross-correlated. 
          if(x(0) NE -1) then nonzerotimes = n_elements(x) $
          else nonzerotimes = 4 ;this will give a null_hyp = 0.99
          
          null_hyp = rcorrprob(0.01,nonzerotimes)
          maxval = max(CorArr(*,j+channelrange(0),which),/NaN)
          if(maxval NE 0.) AND (maxval GE null_hyp) then begin
              tmppp = where(CorArr(*,j+channelrange(0),which) EQ maxval)
              if(n_elements(tmppp) EQ 1) then maxcc(j) = slope*tmppp + delayrange(0) $
              else maxcc(j) = slope*tmppp(n_elements(tmppp)*0.5) + delayrange(0)          
          endif
          if(maxval EQ 0.) OR (maxval LE null_hyp) then maxcc(j) = !values.f_nan
      endfor
      ;now lets remove certain any
;frequencies that dont' have a nice power pattern.

      is_nan = where(finite(maxcc) EQ 0)
      not_nan = where(finite(maxcc) NE 0)
      loopnumber = not_nan(n_elements(not_nan)-1) - not_nan(0) + 1
      if is_nan(0) NE -1 then frequencies(is_nan) = !values.f_nan  

;I'm subtracting 0.5*freqrange_adj here to match the freqs with those
;printed by powerplot. 

      for i=0,loopnumber-1 do begin
          print, string(i+1) + ') ' + string(frequencies(i+not_nan(0))-0.5*freqrange_adj,'(F4.2)')
      endfor
      if keyword_set(removefreqs) then begin            
          num_rm_freqs = ''
          read,num_rm_freqs,PROMPT='How many freqs would you like to remove? '
          num_rm_freqs = float(num_rm_freqs)
          for i=0,num_rm_freqs-1 do begin
              tmpx = ''          
              read,tmpx,PROMPT='Enter number ' + strtrim(string(i+1),2) + ' to be removed: '             
              tmpx = float(tmpx)
              
              maxcc(tmpx+not_nan(0)-1) = !values.f_nan        
              frequencies(tmpx+not_nan(0)-1) = !values.f_nan
          endfor
      endif
  endif  
  
;######################
  if(cc_or_single EQ 'single') then begin
                                ;now lets find the maximum time for each
;channel and attempt to fit a crude line.
                        
      activechannels = RETURNACTIVECHANNELS()
      maxcc = fltarr(n_elements(activechannels))
      
      iter = channelrange(1) - channelrange(0) + 1
      for j=0,iter-1 do begin
          maxval = max(Spec(*,j+channelrange(0),which),/NaN)
          if(maxval GE null_hyp) then begin
              tmppp = where(Spec(*,j+channelrange(0),which) EQ maxval)
              if(maxval NE 0.) or (maxval GT null_hyp) then begin
                  if(n_elements(tmppp) EQ 1) then maxcc(j) = slope*tmppp $
                  else maxcc(j) = slope*tmppp(n_elements(tmppp)*0.5)    
              endif
          endif
          if(maxval LE null_hyp) OR (maxval EQ 0.) then maxcc(j) = !values.f_nan                  
      endfor
      
      is_nan = where(finite(maxcc) EQ 0)
      not_nan = where(finite(maxcc) NE 0)
      loopnumber = not_nan(n_elements(not_nan)-1) - not_nan(0) + 1
      if is_nan(0) NE -1 then frequencies(is_nan) = !values.f_nan     
      
      wd = !D.WINDOW
     
   
                                ;now lets remove certain any
;frequencies that dont' have a nice power pattern.
      for i=0,loopnumber-1 do begin
          print, string(i+1) + ') ' + string(frequencies(i+not_nan(0)),'(F4.2)')
      endfor
      if keyword_set(removefreqs) then begin            
          num_rm_freqs = ''
          read,num_rm_freqs,PROMPT='How many freqs would you like to remove? '
          num_rm_freqs = float(num_rm_freqs)
          for i=0,num_rm_freqs-1 do begin
              tmpx = ''          
              read,tmpx,PROMPT='Enter freq ' + strtrim(string(i+1),2) + ' to be removed: '
              tmpx = float(tmpx)
              maxcc(tmpx+not_nan(0)-1) = !values.f_nan
              frequencies(tmpx+not_nan(0)-1) = !values.f_nan             
          endfor
      endif
  endif  
  
  errdelays(is_nan,0) = !values.f_nan
  errdelays(is_nan,1) = !values.f_nan

                                ;now lets create maxcc_adj. The points
;in maxcc_adj are adjusted to be centered in the error bars given by errdelays. 
  maxcc_adj = fltarr(n_elements(maxcc))
  maxcc_adj(*) = errdelays[*,0] + (errdelays[*,1] - errdelays[*,0])/2
  maxcc_adj(is_nan) = !values.f_nan


  yerr = fltarr(n_elements(maxcc),2)
  yerr[*,*]=0

;xerr is always relative to maxcc_adj
  xerr = fltarr(n_elements(maxcc_adj),2)
  xerr[*,0] = maxcc_adj - errdelays(*,0)
  xerr[*,1] = errdelays[*,1] - maxcc_adj
;now lets make sure that the error bar is at least one FFT box size in
;length.
  tmpu = where(xerr EQ 0.)
  if tmpu(0) ne -1 then xerr(tmpu) = slope/2.
;######## 
;here let's adjust the error bars to center on the actual 
;maximum which is given in maxcc. This is an option that can be used
;in place of maxcc_adj



  not_nan = where(finite(maxcc) NE 0)
  maxstruct = {errdelays:errdelays,maxcc_adj:maxcc_adj,maxcc:maxcc, $ 
               xerr:xerr,yerr:yerr,not_nan:not_nan}

end




