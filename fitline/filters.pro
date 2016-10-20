

;#################################################################
;#################################################################
function FILTERS,array,tmpfreqrange,tmpdelayrange,whichfilter,neighbors=neighbors
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
  channelrangetmp = RETURNCHANNELRANGE(tmpfreqrange)
  timeindexrangetmp = RETURNTIMEINDEXRANGE(tmpdelayrange)
  if cc_or_single EQ 'cc' then which = pair
  if cc_or_single EQ 'single' then which = sc

  if(whichfilter EQ 'butterworth') then begin
      
  endif
  if(whichfilter EQ 'lowpass') then begin      
      j=channelrange(0)
      while(j LT channelrange(1)) do begin
          coeff = digital_filter(0,0.5,50,2)
          array(*,j,which)=convol(array(*,j,which),coeff)
          j=j+1
      endwhile
  endif
  if(whichfilter EQ 'median') then begin
      j=channelrange(0)
      while(j LT channelrange(1)) do begin
          tmp = median(array(*,j,which),neighbors)
          array(*,j,which) = tmp
          j=j+1
      endwhile
  endif  
  if(whichfilter EQ 'boxcar') then begin
      j=channelrange(0)
      while(j LT channelrange(1)) do begin
          tmp = smooth(array(*,j,which),2)
          array(*,j,which) = tmp
          j=j+1
      endwhile
  endif  
 
  return,array
end

function freqfilt,array,tmpfreqrange,tmpdelayrange,neighbors=neighbors
  common readin,nfreqs,ntimes,npairs,delayrange,freqrange,channelrange,CorArr,Spec,scalefactor,timeindices,slope,corrsum,snippettime,numframesinsnippet,sc,nsc,timerange,cc_or_single,segdeltat,numsegs,viewbox,plotrange,limitsofmaxcc,pair,frequencies
;only works for corarr 
  which = pair
  loop = n_elements(array(*,0,0))
  for i=0,loop-1 do begin
      arraytmp = array(i,*,which)
      arraytmp = transpose(arraytmp)
      tmp = median(arraytmp,neighbors)
      array(i,*,which) = tmp
  endfor
  return,array
end





