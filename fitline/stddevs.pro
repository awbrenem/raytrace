


;#####################################################
;#####################################################
pro STDDEVS,sdev,errdelays
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
 
  if cc_or_single EQ 'cc' then which = pair
  if cc_or_single EQ 'single' then which = sc
  activechannels = RETURNACTIVECHANNELS()
  sdevdelay = fltarr(n_elements(activechannels))
  errdelays = fltarr(n_elements(activechannels),2)
  l=0

  if(cc_or_single EQ 'cc') then begin
      for j=0,n_elements(activechannels)-1 do begin
          tmp = moment(CorArr(*,j+channelrange(0),which),sdev=sdevtmp)
          ;###############
          ;temporary addition
          ;sdevtmp = sdevtmp*0.2
          ;###############
          sdev(l) = sdevtmp
          maxforchan = max(CorArr(*,j+channelrange(0),which))
;sdev is now the variation in correlation coefficient.
;the variation in delays is given by all the delay times whose
;correlation coeff falls within r +/- sdev
          
          tmparr = fltarr(ntimes)
          for i=0,ntimes-1 do begin
              if(CorArr(i,j+channelrange(0),which) NE 0) then begin
                  corrlow = maxforchan - sdevtmp
                  corrhigh = maxforchan + sdevtmp ;max and min allowable correlation coefficients.
                  tmparr(i) = (CorArr(i,j+channelrange(0),which) GE corrlow) AND (CorArr(i,j+channelrange(0),which) LE corrhigh) 
;unity if the correlation coeff is b/t corrlow and corrhigh, zero otherwise
              endif
          endfor
          withinrange = where(tmparr EQ 1)
          if(withinrange(0) NE -1) then begin
              tmptimeindexrange = [withinrange(0),withinrange(n_elements(withinrange) - 1)]
              tmpdelayrange = RETURNDELAYRANGE(tmptimeindexrange)
              errdelays(j,0) = tmpdelayrange(0)
              errdelays(j,1) = tmpdelayrange(1)
              
              if(errdelays(j,0) NE -1) then sdevdelay(j) = (errdelays(j,1) - errdelays(j,0))
              if(errdelays(j,0) EQ -1) then sdevdelay(j) = !values.f_nan              
          endif
      endfor
  endif

  if(cc_or_single EQ 'single') then begin
      for j=0,n_elements(activechannels)-1 do begin
          tmp = moment(Spec(*,j+channelrange(0),which),sdev=sdevtmp,/NaN)
          sdev(l) = sdevtmp
          maxforchan = max(Spec(*,j+channelrange(0),which))

          tmparr = fltarr(numsegs)
          for i=0,numsegs-1 do begin
              if(Spec(i,j+channelrange(0),which) NE 0) then begin
                  speclow = maxforchan - sdevtmp
                  spechigh = maxforchan + sdevtmp ;max and min allowable correlation coefficients.
                  tmparr(i) = (Spec(i,j+channelrange(0),which) GE speclow) AND (Spec(i,j+channelrange(0),which) LE spechigh) 
;unity if the correlation coeff is b/t corrlow and corrhigh, zero otherwise
              endif
          endfor
          withinrange = where(tmparr EQ 1)
          if(withinrange(0) NE -1) then begin
              tmptimeindexrange = [withinrange(0),withinrange(n_elements(withinrange) - 1)]

              tmpdelayrange = RETURNDELAYRANGE(tmptimeindexrange)
              if(tmpdelayrange(1) LE timerange(1)) then begin
                  errdelays(j,0) = tmpdelayrange(0)
                  errdelays(j,1) = tmpdelayrange(1)
                  
                  if(errdelays(j,0) NE -1) then sdevdelay(j) = (errdelays(j,1) - errdelays(j,0))
                  if(errdelays(j,0) EQ -1) then sdevdelay(j) = !values.f_nan
              endif
              if(tmpdelayrange(1) GT timerange(1)) then begin
                  errdelays(j,0) = !values.f_nan
                  errdelays(j,1) = !values.f_nan
                  sdevdelay(j) = !values.f_nan
              endif
          endif
      endfor
  endif
  sdev = sdevdelay
end





