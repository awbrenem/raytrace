
;+
; NAME:
;	FIRST_DIFF
;
; PURPOSE:
;	This function returns the first difference vector of the input.
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	Result = FIRST_DIFF( Vect )
;
; INPUTS:
;	Vect:  A vector of type integer or floating point.
;
; KEYWORD PARAMETERS:
;	BACKWARD:  Forces calculation of the backward difference.  This
;	           is the default.
;	FORWARD:  Forces calculation of the forward difference.  The
;	          default is the backward difference.
;	CENTRED:  Forces calculation of the centred difference.  The
;	          defalut is the backward difference.
;
; OUTPUTS:
;	Result:  Returns the first difference of the input vector.
;
; PROCEDURE:
;	The function calculates the first difference of the values in
;	the input vector.  Note that it cycles around and includes the
;	difference between the first and last elements.
;
; EXAMPLE:
;	Define a vector.

;	  x = [1,2,3]
;	Calculate the forward first differenc of x.
;	  result = first_diff( x )
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone, 2000-06-27.
;	Modified:	Daithi A. Stone, 2000-07-06 (removed
;			LENGTH.pro).
;	Modified:	Daithi A. Stone, 2000-07-10 (removed for loops).
;-

;***********************************************************************

FUNCTION FIRST_DIFF, Vect, $
                     BACKWARD=backwardopt, FORWARD=forwardopt, $
                       CENTRED=centreopt

;***********************************************************************
;Variables

;Vector length
n = n_elements(vect)

;Output
fdif = fltarr(n)

;Difference type

if keyword_set(backwardopt) then begin
  forwardopt = 0
  centreopt = 0
endif else if keyword_set(forwardopt) then begin
  backwardopt = 0
  centreopt = 0
endif else if keyword_set(centreopt) then begin
  backwardopt = 0
  forwardopt = 0
endif else begin
  backwardopt = 1
  forwardopt = 0
  centreopt = 0
endelse

;***********************************************************************
;Calculate First Difference

;Backward difference
if backwardopt then begin
  id1 = indgen(n)
  ;First value = first value - last value (in other words, a fix)
  id2 = [ n-1, indgen(n-1) ]
  fdif = vect[id1] - vect[id2]
endif

;Forward difference
if forwardopt then begin
  ;Last value = first value - last value (in other words, a fix)
  id1 = [ indgen(n-1)+1, 0 ]
  id2 = indgen(n)
  fdif = vect[id1] - vect[id2]
endif

;Centred difference
if centreopt then begin
  ;First value = second value - first value (in other words, a fix)
  ;Last value = last value - second-last value (in other words, a fix)
  id1 = [ indgen(n-1)+1, 0 ]
  id2 = [ n-1, indgen(n-1) ]
  fdif = (vect[id1] - vect[id2]) / 2
endif

;***********************************************************************
;The End

return, fdif
END


;+
; NAME:
;	VAR_TYPE
;
; PURPOSE:
;	This function returns the IDL code of the variable type.
;
; CATEGORY:
;	Miscellaneous
;
; CALLING SEQUENCE:
;	Result = VAR_TYPE( invar )
;
; INPUTS:
;	Invar:  The variable to have its type returned.
;
; KEYWORD PARAMETERS:
;	HELP:  If set the function prints the name of the variable
;	       type to screen.  Default is no printing.
;	TEXT:  If set the function returns a text string instead of a number.
;
; OUTPUTS:
;	Result:  The IDL code of the variable type.  See the HELP
;	         option section of the function for interpretation
;	         of the code, or use the HELP keyword.
;
; PROCEDURE:
;	This function reads the variable type index from the SIZE
;	function.
;
; EXAMPLE:
;	Define a floating point number.
;	  x = 1.2
;	Find out its variable type.
;	  result = var_type( x, /help )
;
; MODIFICATION HISTORY:
; 	Written by:	Edward C. Wiebe, 2000-01-21.
;	Modified:	Daithi A. Stone, 2000-06-29 (changed behaviour
;			of HELP keyword).
;       Modified:       Edward C. Wiebe, 2001-05-08 (added text keyword)
;-

;***********************************************************************

FUNCTION VAR_TYPE, Invar             $
                 , HELP=helpopt      $
                 , TEXT=text
 
;***********************************************************************
; Determine Variable Type

  siz = size(invar)
  type = siz[ siz[0]+1 ]

  names = ['Undefined','Byte','Integer','Longword integer' $
          ,'Floating point','Double-precision floating'    $
          ,'Complex floating','String','Structure'         $
          ,'Double-precision complex floating'             $
          ,'Pointer','Object reference']

; If HELP is set
  if (Keyword_Set(helpopt)) then begin
    Print, names[type]
  endif
  
  if (Keyword_Set(text)) then type=names[type]

;***********************************************************************
;The End

  return, type
END

;+
; NAME:
;	BAR_GRAPH
;
; PURPOSE:
;	This procedure plots bar graphs.
;
; CATEGORY:
;	Graphics
;
; CALLING SEQUENCE:
;	BAR_GRAPH, [Xval,] Yval
;
; INPUTS:
;	Yval:  A vector of values to be plotted, of type integer or
;	       floating point.
;
; OPTIONAL INPUTS:
;	Xval:  A vector of ordinate values for the plot, of type integer
;	       or floating point.
;
; KEYWORD PARAMETERS:
;	BARBORDER:  The color of the border of the bars.  The default is the
;		same as BARCOLOR.
;	BARCOLOR:  The color of the bars of the bar plot.
;	BARGAP:  The size of the gap between bars in unit of fraction of the
;		bar width.  The default is 0 (no gap).
;	CHARSIZE:  The size of the text characters.
;	COLOR:  The color of the plot axes and text.
;	FONT:  The index of the font table to be used.
;	OVERPLOT:  If set the procedure plots on top of the exising plot.
;	SUBTITLE:  A subtitle for the plot, of type string.
;	TITLE:  A title, of type string, for the plot.
;	VARWIDTH:  If set the bars are all with variable widths such that
;		there are no gaps between them.  The default is to plot all
;		bars with the same width.
;	[X,Y]MINOR:  The number of minor tick marks between major tick marks.
;	[X,Y]RANGE:  A 2-element vector containing the minimum and maximum
;		X- or Y-coordinates to be plotted.
;	[X,Y]STYLE:  See the IDL help for the use of these keywords in plotting
;		routines.
;	[X,Y]THICK:  The line thickness.
;	[X,Y]TICKLEN:  The length of the ticks on the X- or Y-axis, in units of
;		fraction of the window size.  The default is !p.ticklen (0.02).
;	[X,Y]TICKNAME:  A vector, of type string, of labels for the major ticks
;		on the X- or Y-axis.  There should be [X,Y]TICKS + 1 values in
;		the vector.
;	[X,Y]TICKS:  The number of major ticks on the X- or Y-axis minus one.
;	[X,Y]TICKV:  The location of the major ticks on the X- or Y-axis.
;	[X,Y]TITLE:  A label for the X or Y axis, of type string.
;
; USES:
;	FIRST_DIFF.pro
;	VAR_TYPE.pro
;
; PROCEDURE:
;	This procedure uses the POLYFILL procedure to draw solid bar
;	graphs.
;
; EXAMPLE:
;	Plot 10 step-like bars.
;	  bar_graph, indgen(10)+1, barcolor=2, bargap=0.1
;
; MODIFICATION HISTORY:
; 	Written by:	Daithi A. Stone (stoned@atm.ox.ac.uk), 2000-09-11.
;	Modified:	DAS, 2001-10-22 (Added [X,Y]TICKLEN keywords).
;	Modified:	DAS, 2003-06-16 (added FONT, [X,Y]MINOR, [X,Y]THICK
;			keywords)
;-

;***********************************************************************

PRO BAR_GRAPH, $
	Xval, Yval, $
	BARBORDER=barborder, BARCOLOR=barcolor, COLOR=color, $
	BARGAP=bargap, $
	CHARSIZE=charsize, $
	FONT=font, $
	OVERPLOT=overplotopt, $
	TITLE=title, SUBTITLE=subtitle, XTITLE=xtitle, YTITLE=ytitle, $
	VARWIDTH=varwidth, $
	XMINOR=xminor, YMINOR=yminor, $
	XSTYLE=xstyle, YSTYLE=ystyle, $
	XTHICK=xthick, ythick=ythick, $
	XTICKLEN=xticklen, YTICKLEN=yticklen, $
	XTICKNAME=xtickname, YTICKNAME=ytickname, $
	XTICKS=xticks, YTICKS=yticks, $
	XTICKV=xtickv, YTICKV=ytickv, $
	XRANGE=xrange, YRANGE=yrange

;***********************************************************************
; Constants and Variables

; Ordinate vector for data
if n_params() eq 1 then begin
  yval = xval
  xval = indgen( n_elements( yval ) )
endif
nval = n_elements( yval )

; Colors
; Bar fill color
if var_type( barcolor ) eq 0 then barcolor = !p.color
; Bar border color
if var_type( barborder ) eq 0 then barborder = barcolor
; Plot axes and text color
if var_type( color ) eq 0 then color = !p.color

;BARGAP
if not( keyword_set( bargap ) ) then bargap = 0

;***********************************************************************
; Prepare Data for Plotting

; Vector data sorted according to the ordinate vector
id = sort( xval )
xsort = xval[id]
ysort = yval[id]
id = where( (finite( xsort ) eq 1) and (finite( ysort ) eq 1) )
xsort = xsort[id]
ysort = ysort[id]
nval = n_elements( id )

; Ordinate vector for plotting
if keyword_set( varwidth ) then begin
  xcoord = first_diff( xsort,/forward ) / 2.
  xcoord = [ (xsort[1]-xsort[0])/2., xcoord ]
  xcoord[nval] = ( xsort[nval-1] - xsort[nval-2] ) / 2.
endif else begin
  mindiff = min( (first_diff( xsort ,/forward))[0:nval-2] ) / 2.
  xcoord = mindiff + 0 * [0,xsort]
endelse

; XRANGE
if not( keyword_set( xrange )) then begin
  xrange = [ xsort[0]-xcoord[0], xsort[nval-1]+xcoord[nval] ]
endif

; YRANGE
if not( keyword_set( yrange ) ) then yrange = [ min( ysort ), max( ysort ) ]

;***********************************************************************
; Plot Bar Graph

; Plot set-up
if not( keyword_set( overplotopt ) ) then begin
  plot, xrange, yrange, charsize=charsize, color=color, /nodata, $
      subtitle=subtitle, title=title, xstyle=xstyle, ystyle=ystyle, $
      xthick=xthick, ythick=ythick, xticklen=xticklen, yticklen=yticklen, $
      xtickname=xtickname, ytickname=ytickname, xticks=xticks, $
      yticks=yticks, xtickv=xtickv, ytickv=ytickv, xtitle=xtitle, $
      ytitle=ytitle, font=font, xminor=xminor, yminor=yminor
  ; Y=0 line
  oplot, xrange, [0,0], color=color, line=1
endif

; Plot bars
for i = 0, nval - 1 do begin
  ; Solid bar
  polyfill, xsort[i]+(1-bargap)*[-1,1,1,-1,-1]*xcoord[i+[0,1,1,0,0]], $
      [0,0,ysort[i],ysort[i],0], color=barcolor
  ; Bar border
  oplot, xsort[i]+(1-bargap)*[-1,1,1,-1,-1]*xcoord[i+[0,1,1,0,0]], $
      [0,0,ysort[i],ysort[i],0], color=barborder
endfor

;***********************************************************************
; The END

return
END


;##################
pro histoplots
device,decomposed=0
restore,'raydata.dat'

;####################
;RESONANT ENERGY
;####################
;keepers = where(theta_k_c1 ne -10000)
;if keyword_set(freq_all) eq 0 then freq_all = ['4000','5000']
;if keyword_set(density) eq 0 then density = dens_sca(*,*,0)
;freq_all = float(freq_all)
;
;c_sq = 0L
;
;if keepers(0) ne -1 then begin
;fpe_c1 = 8.98e3 * sqrt(density(keepers))
;fce_c1 = (1/ffce_c1(keepers))*freq_all(0)
;f=freq_all(0)
;
;vpar_sq = (3e8)/(f*cos(theta_k_c1(keepers)))
;vpar_sq = vpar_sq^2
;vpar_sq = vpar_sq * (f - fce_c1)^2
;d = fpe_c1^2/(f*(ffce_c1*cos(theta_k_c1(keepers))))
;vpar_sq = vpar_sq/(1-d)
;
;epar = 9.1e-31 * vpar_sq   ;joules
;epar = epar/(1.6022e-19)   ;eV
;epar = epar/1000.          ;keV
;endif


;###############################################################################################
;ACTUAL SOURCE
;###############################################################################################
for q=0,8 do begin
                   if q eq 0 then begin
                       raydata_avg_f1 = {c1:strarr(7),c2:strarr(7),c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_source eq 0.)
                   endif
                   if q eq 1 then begin
                       raydata_avg_ws_f1 = {c1:strarr(7),c2:strarr(7),c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_ws eq 0.)
                   endif
                   if q eq 2 then begin  ;cc12
                       raydata_avg_cc12_f1 = {c1:strarr(7),c2:strarr(7)}
                       tmp = where(not_keepers_cc12 eq 0.)
                   endif
                   if q eq 3 then begin  ;cc13
                       raydata_avg_cc13_f1 = {c1:strarr(7),c3:strarr(7)}
                       tmp = where(not_keepers_cc13 eq 0.)
                   endif
                   if q eq 4 then begin  ;cc14
                       raydata_avg_cc14_f1 = {c1:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc14 eq 0.)
                   endif
                   if q eq 5 then begin  ;cc23
                       raydata_avg_cc23_f1 = {c2:strarr(7),c3:strarr(7)}
                       tmp = where(not_keepers_cc23 eq 0.)
                   endif
                   if q eq 6 then begin  ;cc24
                       raydata_avg_cc24_f1 = {c2:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc24 eq 0.)
                   endif
                   if q eq 7 then begin  ;cc34
                       raydata_avg_cc34_f1 = {c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc34 eq 0.)
                   endif

                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin      		     
                       t2 = where(theta_k_c1_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c1_f1(tmp(t2)) = 180 - theta_k_c1_f1(tmp(t2))        
                       tk1 = theta_k_c1_f1(tmp)
                       moments = moment(tk1)
                       tk1_avg = moments(0)
                       tk1_var = sqrt(moments(1)) 
                       distfreq = histogram(tk1,min=min(tk1))
                       maxfrq = max(distfreq)
                       tk1_mode = where(distfreq eq maxfrq)+min(tk1)                             
                       tk1_med = median(tk1)
                       tk1_max = max(tk1)
                       tk1_min = min(tk1)                                                
                       tk1str = 'MEAN: ' + strtrim(string(tk1_avg),2) + ' MEDIAN: ' + strtrim(string(tk1_med),2) + ' MODE: ' + strtrim(string(tk1_mode),2) + ' MAX: ' + strtrim(string(tk1_max),2) + ' MIN: ' + strtrim(string(tk1_min),2) + ' STD: '  + strtrim(string(tk1_var),2)
                       tk1str = 'THETA_K_SC1--  ' + tk1str
                       tk1str = tk1str(0)
                       if q eq 0 then raydata_avg_f1.c1(0) = tk1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(0) = tk1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(0) = tk1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(0) = tk1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(0) = tk1str(0)
                   endif

                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(theta_k_c2_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c2_f1(tmp(t2)) = 180 - theta_k_c2_f1(tmp(t2)) 
                       tk2 = theta_k_c2_f1(tmp)
                       moments = moment(tk2)
                       tk2_avg = moments(0)
                       tk2_var = sqrt(moments(1))                   
                       distfreq = histogram(tk2,min=min(tk2))
                       maxfrq = max(distfreq)
                       tk2_mode = where(distfreq eq maxfrq)+min(tk2)                             
                       tk2_med = median(tk2)
                       tk2_max = max(tk2)
                       tk2_min = min(tk2)  
                       tk2str = 'MEAN: ' + strtrim(string(tk2_avg),2) + ' MEDIAN: ' + strtrim(string(tk2_med),2) + ' MODE: ' + strtrim(string(tk2_mode),2) + ' MAX: ' + strtrim(string(tk2_max),2) + ' MIN: ' + strtrim(string(tk2_min),2) + ' STD: '  + strtrim(string(tk2_var),2) 
                       tk2str = 'THETA_K_SC2--  ' + tk2str
                       if q eq 0 then raydata_avg_f1.c2(0) = tk2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(0) = tk2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(0) = tk2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(0) = tk2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(0) = tk2str(0)
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(theta_k_c3_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c3_f1(tmp(t2)) = 180 - theta_k_c3_f1(tmp(t2)) 
                       tk3 = theta_k_c3_f1(tmp)
                       moments = moment(tk3)
                       tk3_avg = moments(0)
                       tk3_var = sqrt(moments(1))
                       distfreq = histogram(tk3,min=min(tk3))
                       maxfrq = max(distfreq)
                       tk3_mode = where(distfreq eq maxfrq)+min(tk3)                             
                       tk3_med = median(tk3)
                       tk3_max = max(tk3)
                       tk3_min = min(tk3)   
                       tk3str = 'MEAN: ' + strtrim(string(tk3_avg),2) + ' MEDIAN: ' + strtrim(string(tk3_med),2) + ' MODE: ' + strtrim(string(tk3_mode),2) + ' MAX: ' + strtrim(string(tk3_max),2) + ' MIN: ' + strtrim(string(tk3_min),2) + ' STD: '  + strtrim(string(tk3_var),2)
                       tk3str = 'THETA_K_SC3--  ' + tk3str
                       if q eq 0 then raydata_avg_f1.c3(0) = tk3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(0) = tk3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(0) = tk3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(0) = tk3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(0) = tk3str(0)
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(theta_k_c4_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c4_f1(tmp(t2)) = 180 - theta_k_c4_f1(tmp(t2)) 
                       tk4 = theta_k_c4_f1(tmp)
                       moments = moment(tk4)
                       tk4_avg = moments(0)
                       tk4_var = sqrt(moments(1))
                       distfreq = histogram(tk4,min=min(tk4))
                       maxfrq = max(distfreq)
                       tk4_mode = where(distfreq eq maxfrq)+min(tk4)                             
                       tk4_med = median(tk4)
                       tk4_max = max(tk4)
                       tk4_min = min(tk4)   
                       tk4str = 'MEAN: ' + strtrim(string(tk4_avg),2) + ' MEDIAN: ' + strtrim(string(tk4_med),2) + ' MODE: ' + strtrim(string(tk4_mode),2) + ' MAX: ' + strtrim(string(tk4_max),2) + ' MIN: ' + strtrim(string(tk4_min),2) + ' STD: '  + strtrim(string(tk4_var),2)
                       tk4str = 'THETA_K_SC4--  ' + tk4str
                       if q eq 0 then raydata_avg_f1.c4(0) = tk4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(0) = tk4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(0) = tk4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(0) = tk4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(0) = tk4str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(theta_g_c1_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c1_f1(tmp(t2)) = 180 - theta_g_c1_f1(tmp(t2)) 
                       tg1 = theta_g_c1_f1(tmp)
                       moments = moment(tg1)
                       tg1_avg = moments(0)
                       tg1_var = sqrt(moments(1))
                       distfreq = histogram(tg1,min=min(tg1))
                       maxfrq = max(distfreq)
                       tg1_mode = where(distfreq eq maxfrq)+min(tg1)                             
                       tg1_med = median(tg1)
                       tg1_max = max(tg1)
                       tg1_min = min(tg1)                                                
                       tg1str = 'MEAN: ' + strtrim(string(tg1_avg),2) + ' MEDIAN: ' + strtrim(string(tg1_med),2) + ' MODE: ' + strtrim(string(tg1_mode),2) + ' MAX: ' + strtrim(string(tg1_max),2) + ' MIN: ' + strtrim(string(tg1_min),2) + ' STD: '  + strtrim(string(tg1_var),2)
                       tg1str = 'THETA_G_SC1--  ' + tg1str
                       tg1str = tg1str(0)
                       if q eq 0 then raydata_avg_f1.c1(1) = tg1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(1) = tg1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(1) = tg1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(1) = tg1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(1) = tg1str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(theta_g_c2_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c2_f1(tmp(t2)) = 180 - theta_g_c2_f1(tmp(t2)) 
                       tg2 = theta_g_c2_f1(tmp)
                       moments = moment(tg2)
                       tg2_avg = moments(0)
                       tg2_var = sqrt(moments(1))
                       distfreq = histogram(tg2,min=min(tg2))
                       maxfrq = max(distfreq)
                       tg2_mode = where(distfreq eq maxfrq)+min(tg2)                             
                       tg2_med = median(tg2)
                       tg2_max = max(tg2)
                       tg2_min = min(tg2)   
                       tg2str = 'MEAN: ' + strtrim(string(tg2_avg),2) + ' MEDIAN: ' + strtrim(string(tg2_med),2) + ' MODE: ' + strtrim(string(tg2_mode),2) + ' MAX: ' + strtrim(string(tg2_max),2) + ' MIN: ' + strtrim(string(tg2_min),2) + ' STD: '  + strtrim(string(tg2_var),2)
                       tg2str = 'THETA_G_SC2--  ' + tg2str
                       if q eq 0 then raydata_avg_f1.c2(1) = tg2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(1) = tg2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(1) = tg2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(1) = tg2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(1) = tg2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(theta_g_c3_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c3_f1(tmp(t2)) = 180 - theta_g_c3_f1(tmp(t2)) 
                       tg3 = theta_g_c3_f1(tmp)
                       moments = moment(tg3)
                       tg3_avg = moments(0)
                       tg3_var = sqrt(moments(1))
                       distfreq = histogram(tg3,min=min(tg3))
                       maxfrq = max(distfreq)
                       tg3_mode = where(distfreq eq maxfrq)+min(tg3)                             
                       tg3_med = median(tg3)
                       tg3_max = max(tg3)
                       tg3_min = min(tg3)   
                       tg3str = 'MEAN: ' + strtrim(string(tg3_avg),2) + ' MEDIAN: ' + strtrim(string(tg3_med),2) + ' MODE: ' + strtrim(string(tg3_mode),2) + ' MAX: ' + strtrim(string(tg3_max),2) + ' MIN: ' + strtrim(string(tg3_min),2) + ' STD: '  + strtrim(string(tg3_var),2)
                       tg3str = 'THETA_G_SC3--  ' + tg3str
                       if q eq 0 then raydata_avg_f1.c3(1) = tg3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(1) = tg3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(1) = tg3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(1) = tg3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(1) = tg3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(theta_g_c4_f1(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c4_f1(tmp(t2)) = 180 - theta_g_c4_f1(tmp(t2)) 
                       tg4 = theta_g_c4_f1(tmp)
                       moments = moment(tg4)
                       tg4_avg = moments(0)
                       tg4_var = sqrt(moments(1))
                       distfreq = histogram(tg4,min=min(tg4))
                       maxfrq = max(distfreq)
                       tg4_mode = where(distfreq eq maxfrq)+min(tg4)                             
                       tg4_med = median(tg4)
                       tg4_max = max(tg4)
                       tg4_min = min(tg4)   
                       tg4str = 'MEAN: ' + strtrim(string(tg4_avg),2) + ' MEDIAN: ' + strtrim(string(tg4_med),2) + ' MODE: ' + strtrim(string(tg4_mode),2) + ' MAX: ' + strtrim(string(tg4_max),2) + ' MIN: ' + strtrim(string(tg4_min),2) + ' STD: '  + strtrim(string(tg4_var),2)
                       tg4str = 'THETA_G_SC4--  ' + tg4str
                       if q eq 0 then raydata_avg_f1.c4(1) = tg4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(1) = tg4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(1) = tg4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(1) = tg4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(1) = tg4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin         
                       pre1 = pathre_c1_f1(tmp)
                       moments = moment(pre1)
                       pre1_avg = moments(0)
                       pre1_var = sqrt(moments(1))
                       distfreq = histogram(pre1,min=min(pre1))
                       maxfrq = max(distfreq)
                       pre1_mode = where(distfreq eq maxfrq)+min(pre1)                             
                       pre1_med = median(pre1)
                       pre1_max = max(pre1)
                       pre1_min = min(pre1)                                                    
                       pre1str = 'MEAN: ' + strtrim(string(pre1_avg),2) + ' MEDIAN: ' + strtrim(string(pre1_med),2) + ' MODE: ' + strtrim(string(pre1_mode),2) + ' MAX: ' + strtrim(string(pre1_max),2) + ' MIN: ' + strtrim(string(pre1_min),2) + ' STD: '  + strtrim(string(pre1_var),2)
                       pre1str = 'PATH_RE_SC1--  ' + pre1str
                       if q eq 0 then raydata_avg_f1.c1(2) = pre1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(2) = pre1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(2) = pre1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(2) = pre1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(2) = pre1str(0) 
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       pre2 = pathre_c2_f1(tmp)
                       moments = moment(pre2)
                       pre2_avg = moments(0)
                       pre2_var = sqrt(moments(1))
                       distfreq = histogram(pre2,min=min(pre2))
                       maxfrq = max(distfreq)
                       pre2_mode = where(distfreq eq maxfrq)+min(pre2)                             
                       pre2_med = median(pre2)
                       pre2_max = max(pre2)
                       pre2_min = min(pre2)   
                       pre2str = 'MEAN: ' + strtrim(string(pre2_avg),2) + ' MEDIAN: ' + strtrim(string(pre2_med),2) + ' MODE: ' + strtrim(string(pre2_mode),2) + ' MAX: ' + strtrim(string(pre2_max),2) + ' MIN: ' + strtrim(string(pre2_min),2) + ' STD: '  + strtrim(string(pre2_var),2)
                       pre2str = 'PATH_RE_SC2--  ' + pre2str
                       if q eq 0 then raydata_avg_f1.c2(2) = pre2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(2) = pre2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(2) = pre2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(2) = pre2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(2) = pre2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       pre3 = pathre_c3_f1(tmp)
                       moments = moment(pre3)
                       pre3_avg = moments(0)
                       pre3_var = sqrt(moments(1))
                       distfreq = histogram(pre3,min=min(pre3))
                       maxfrq = max(distfreq)
                       pre3_mode = where(distfreq eq maxfrq)+min(pre3)                             
                       pre3_med = median(pre3)
                       pre3_max = max(pre3)
                       pre3_min = min(pre3)   
                       pre3str = 'MEAN: ' + strtrim(string(pre3_avg),2) + ' MEDIAN: ' + strtrim(string(pre3_med),2) + ' MODE: ' + strtrim(string(pre3_mode),2) + ' MAX: ' + strtrim(string(pre3_max),2) + ' MIN: ' + strtrim(string(pre3_min),2) + ' STD: '  + strtrim(string(pre3_var),2)
                       pre3str = 'PATH_RE_SC3--  ' + pre3str
                       if q eq 0 then raydata_avg_f1.c3(2) = pre3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(2) = pre3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(2) = pre3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(2) = pre3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(2) = pre3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       pre4 = pathre_c4_f1(tmp)
                       moments = moment(pre4)
                       pre4_avg = moments(0)
                       pre4_var = sqrt(moments(1))
                       distfreq = histogram(pre4,min=min(pre4))
                       maxfrq = max(distfreq)
                       pre4_mode = where(distfreq eq maxfrq)+min(pre4)                             
                       pre4_med = median(pre4)
                       pre4_max = max(pre4)
                       pre4_min = min(pre4)   
                       pre4str = 'MEAN: ' + strtrim(string(pre4_avg),2) + ' MEDIAN: ' + strtrim(string(pre4_med),2) + ' MODE: ' + strtrim(string(pre4_mode),2) + ' MAX: ' + strtrim(string(pre4_max),2) + ' MIN: ' + strtrim(string(pre4_min),2) + ' STD: '  + strtrim(string(pre4_var),2)
                       pre4str = 'PATH_RE_SC4--  ' + pre4str
                       if q eq 0 then raydata_avg_f1.c4(2) = pre4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(2) = pre4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(2) = pre4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(2) = pre4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(2) = pre4str(0)                      
                   endif                   
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       ffce1 = ffce_c1_f1(tmp)
                       moments = moment(ffce1)
                       ffce1_avg = moments(0)
                       ffce1_var = sqrt(moments(1))
                       distfreq = histogram(ffce1,min=min(ffce1))
                       maxfrq = max(distfreq)
                       ffce1_mode = where(distfreq eq maxfrq)+min(ffce1)                             
                       ffce1_med = median(ffce1)
                       ffce1_max = max(ffce1)
                       ffce1_min = min(ffce1)                                                    
                       ffce1str = 'MEAN: ' + strtrim(string(ffce1_avg),2) + ' MEDIAN: ' + strtrim(string(ffce1_med),2) + ' MODE: ' + strtrim(string(ffce1_mode),2) + ' MAX: ' + strtrim(string(ffce1_max),2) + ' MIN: ' + strtrim(string(ffce1_min),2) + ' STD: '  + strtrim(string(ffce1_var),2)
                       ffce1str = 'f/fce_SC1--  ' + ffce1str
                       if q eq 0 then raydata_avg_f1.c1(3) = ffce1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(3) = ffce1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(3) = ffce1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(3) = ffce1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(3) = ffce1str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       ffce2 = ffce_c2_f1(tmp)
                       moments = moment(ffce2)
                       ffce2_avg = moments(0)
                       ffce2_var = sqrt(moments(1))
                       distfreq = histogram(ffce2,min=min(ffce2))
                       maxfrq = max(distfreq)
                       ffce2_mode = where(distfreq eq maxfrq)+min(ffce2)                             
                       ffce2_med = median(ffce2)
                       ffce2_max = max(ffce2)
                       ffce2_min = min(ffce2)   
                       ffce2str = 'MEAN: ' + strtrim(string(ffce2_avg),2) + ' MEDIAN: ' + strtrim(string(ffce2_med),2) + ' MODE: ' + strtrim(string(ffce2_mode),2) + ' MAX: ' + strtrim(string(ffce2_max),2) + ' MIN: ' + strtrim(string(ffce2_min),2) + ' STD: '  + strtrim(string(ffce2_var),2)
                       ffce2str = 'f/fce_SC2--  ' + ffce2str
                       if q eq 0 then raydata_avg_f1.c2(3) = ffce2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(3) = ffce2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(3) = ffce2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(3) = ffce2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(3) = ffce2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       ffce3 = ffce_c3_f1(tmp)
                       moments = moment(ffce3)
                       ffce3_avg = moments(0)
                       ffce3_var = sqrt(moments(1))
                       distfreq = histogram(ffce3,min=min(ffce3))
                       maxfrq = max(distfreq)
                       ffce3_mode = where(distfreq eq maxfrq)+min(ffce3)                             
                       ffce3_med = median(ffce3)
                       ffce3_max = max(ffce3)
                       ffce3_min = min(ffce3)   
                       ffce3str = 'MEAN: ' + strtrim(string(ffce3_avg),2) + ' MEDIAN: ' + strtrim(string(ffce3_med),2) + ' MODE: ' + strtrim(string(ffce3_mode),2) + ' MAX: ' + strtrim(string(ffce3_max),2) + ' MIN: ' + strtrim(string(ffce3_min),2) + ' STD: '  + strtrim(string(ffce3_var),2)
                       ffce3str = 'f/fce_SC3--  ' + ffce3str
                       if q eq 0 then raydata_avg_f1.c3(3) = ffce3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(3) = ffce3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(3) = ffce3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(3) = ffce3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(3) = ffce3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       ffce4 = ffce_c4_f1(tmp)
                       moments = moment(ffce4)
                       ffce4_avg = moments(0)
                       ffce4_var = sqrt(moments(1))
                       distfreq = histogram(ffce4,min=min(ffce4))
                       maxfrq = max(distfreq)
                       ffce4_mode = where(distfreq eq maxfrq)+min(ffce4)                             
                       ffce4_med = median(ffce4)
                       ffce4_max = max(ffce4)
                       ffce4_min = min(ffce4)   
                       ffce4str = 'MEAN: ' + strtrim(string(ffce4_avg),2) + ' MEDIAN: ' + strtrim(string(ffce4_med),2) + ' MODE: ' + strtrim(string(ffce4_mode),2) + ' MAX: ' + strtrim(string(ffce4_max),2) + ' MIN: ' + strtrim(string(ffce4_min),2) + ' STD: '  + strtrim(string(ffce4_var),2)
                       ffce4str = 'f/fce_SC4--  ' + ffce4str
                       if q eq 0 then raydata_avg_f1.c4(3) = ffce4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(3) = ffce4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(3) = ffce4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(3) = ffce4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(3) = ffce4str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       rdx1 = rdx_c1_f1(tmp)
                       moments = moment(rdx1)
                       rdx1_avg = moments(0)
                       rdx1_var = sqrt(moments(1))
                       distfreq = histogram(rdx1,min=min(rdx1))
                       maxfrq = max(distfreq)
                       rdx1_mode = where(distfreq eq maxfrq)+min(rdx1)                             
                       rdx1_med = median(rdx1)
                       rdx1_max = max(rdx1)
                       rdx1_min = min(rdx1)                                                  
                       rdx1str = 'MEAN: ' + strtrim(string(rdx1_avg),2) + ' MEDIAN: ' + strtrim(string(rdx1_med),2) + ' MODE: ' + strtrim(string(rdx1_mode),2) + ' MAX: ' + strtrim(string(rdx1_max),2) + ' MIN: ' + strtrim(string(rdx1_min),2) + ' STD: '  + strtrim(string(rdx1_var),2)
                       rdx1str = 'REFNDX_SC1--  ' + rdx1str
                       if q eq 0 then raydata_avg_f1.c1(4) = rdx1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(4) = rdx1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(4) = rdx1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(4) = rdx1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(4) = rdx1str(0)                        
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       rdx2 = rdx_c2_f1(tmp)
                       moments = moment(rdx2)
                       rdx2_avg = moments(0)
                       rdx2_var = sqrt(moments(1))
                       distfreq = histogram(rdx2,min=min(rdx2))
                       maxfrq = max(distfreq)
                       rdx2_mode = where(distfreq eq maxfrq)+min(rdx2)                             
                       rdx2_med = median(rdx2)
                       rdx2_max = max(rdx2)
                       rdx2_min = min(rdx2)   
                       rdx2str = 'MEAN: ' + strtrim(string(rdx2_avg),2) + ' MEDIAN: ' + strtrim(string(rdx2_med),2) + ' MODE: ' + strtrim(string(rdx2_mode),2) + ' MAX: ' + strtrim(string(rdx2_max),2) + ' MIN: ' + strtrim(string(rdx2_min),2) + ' STD: '  + strtrim(string(rdx2_var),2)
                       rdx2str = 'REFNDX_SC2--  ' + rdx2str
                       if q eq 0 then raydata_avg_f1.c2(4) = rdx2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(4) = rdx2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(4) = rdx2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(4) = rdx2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(4) = rdx2str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       rdx3 = rdx_c3_f1(tmp)
                       moments = moment(rdx3)
                       rdx3_avg = moments(0)
                       rdx3_var = sqrt(moments(1))
                       distfreq = histogram(rdx3,min=min(rdx3))
                       maxfrq = max(distfreq)
                       rdx3_mode = where(distfreq eq maxfrq)+min(rdx3)                             
                       rdx3_med = median(rdx3)
                       rdx3_max = max(rdx3)
                       rdx3_min = min(rdx3)   
                       rdx3str = 'MEAN: ' + strtrim(string(rdx3_avg),2) + ' MEDIAN: ' + strtrim(string(rdx3_med),2) + ' MODE: ' + strtrim(string(rdx3_mode),2) + ' MAX: ' + strtrim(string(rdx3_max),2) + ' MIN: ' + strtrim(string(rdx3_min),2) + ' STD: '  + strtrim(string(rdx3_var),2)
                       rdx3str = 'REFNDX_SC3--  ' + rdx3str
                       if q eq 0 then raydata_avg_f1.c3(4) = rdx3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(4) = rdx3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(4) = rdx3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(4) = rdx3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(4) = rdx3str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       rdx4 = rdx_c4_f1(tmp)
                       moments = moment(rdx4)
                       rdx4_avg = moments(0)
                       rdx4_var = sqrt(moments(1))
                       distfreq = histogram(rdx4,min=min(rdx4))
                       maxfrq = max(distfreq)
                       rdx4_mode = where(distfreq eq maxfrq)+min(rdx4)                             
                       rdx4_med = median(rdx4)
                       rdx4_max = max(rdx4)
                       rdx4_min = min(rdx4)   
                       rdx4str = 'MEAN: ' + strtrim(string(rdx4_avg),2) + ' MEDIAN: ' + strtrim(string(rdx4_med),2) + ' MODE: ' + strtrim(string(rdx4_mode),2) + ' MAX: ' + strtrim(string(rdx4_max),2) + ' MIN: ' + strtrim(string(rdx4_min),2) + ' STD: '  + strtrim(string(rdx4_var),2)
                       rdx4str = 'REFNDX_SC4--  ' + rdx4str
                       if q eq 0 then raydata_avg_f1.c4(4) = rdx4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(4) = rdx4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(4) = rdx4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(4) = rdx4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(4) = rdx4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(tgn_c1_f1(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c1_f1(tmp(t2)) = 180 - tgn_c1_f1(tmp(t2)) 
                       tgn1 = tgn_c1_f1(tmp)
                       moments = moment(tgn1)
                       tgn1_avg = moments(0)
                       tgn1_var = sqrt(moments(1))
                       distfreq = histogram(tgn1,min=min(tgn1))
                       maxfrq = max(distfreq)
                       tgn1_mode = where(distfreq eq maxfrq)+min(tgn1)                             
                       tgn1_med = median(tgn1)
                       tgn1_max = max(tgn1)
                       tgn1_min = min(tgn1)                                       
                       tgn1str = 'MEAN: ' + strtrim(string(tgn1_avg),2) + ' MEDIAN: ' + strtrim(string(tgn1_med),2) + ' MODE: ' + strtrim(string(tgn1_mode),2) + ' MAX: ' + strtrim(string(tgn1_max),2) + ' MIN: ' + strtrim(string(tgn1_min),2) + ' STD: '  + strtrim(string(tgn1_var),2)
                       tgn1str = 'THETA_GENDRIN_SC1--  ' + tgn1str
                       if q eq 0 then raydata_avg_f1.c1(5) = tgn1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(5) = tgn1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(5) = tgn1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(5) = tgn1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(5) = tgn1str(0)               
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(tgn_c2_f1(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c2_f1(tmp(t2)) = 180 - tgn_c2_f1(tmp(t2)) 
                       tgn2 = tgn_c2_f1(tmp)
                       moments = moment(tgn2)
                       tgn2_avg = moments(0)
                       tgn2_var = sqrt(moments(1))
                       distfreq = histogram(tgn2,min=min(tgn2))
                       maxfrq = max(distfreq)
                       tgn2_mode = where(distfreq eq maxfrq)+min(tgn2)                             
                       tgn2_med = median(tgn2)
                       tgn2_max = max(tgn2)
                       tgn2_min = min(tgn2)   
                       tgn2str = 'MEAN: ' + strtrim(string(tgn2_avg),2) + ' MEDIAN: ' + strtrim(string(tgn2_med),2) + ' MODE: ' + strtrim(string(tgn2_mode),2) + ' MAX: ' + strtrim(string(tgn2_max),2) + ' MIN: ' + strtrim(string(tgn2_min),2) + ' STD: '  + strtrim(string(tgn2_var),2)
                       tgn2str = 'THETA_GENDRIN_SC2--  ' + tgn2str
                       if q eq 0 then raydata_avg_f1.c2(5) = tgn2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(5) = tgn2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(5) = tgn2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(5) = tgn2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(5) = tgn2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(tgn_c3_f1(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c3_f1(tmp(t2)) = 180 - tgn_c3_f1(tmp(t2)) 
                       tgn3 = tgn_c3_f1(tmp)
                       moments = moment(tgn3)
                       tgn3_avg = moments(0)
                       tgn3_var = sqrt(moments(1))
                       distfreq = histogram(tgn3,min=min(tgn3))
                       maxfrq = max(distfreq)
                       tgn3_mode = where(distfreq eq maxfrq)+min(tgn3)                             
                       tgn3_med = median(tgn3)
                       tgn3_max = max(tgn3)
                       tgn3_min = min(tgn3)   
                       tgn3str = 'MEAN: ' + strtrim(string(tgn3_avg),2) + ' MEDIAN: ' + strtrim(string(tgn3_med),2) + ' MODE: ' + strtrim(string(tgn3_mode),2) + ' MAX: ' + strtrim(string(tgn3_max),2) + ' MIN: ' + strtrim(string(tgn3_min),2) + ' STD: '  + strtrim(string(tgn3_var),2)
                       tgn3str = 'THETA_GENDRIN_SC3--  ' + tgn3str
                       if q eq 0 then raydata_avg_f1.c3(5) = tgn3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(5) = tgn3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(5) = tgn3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(5) = tgn3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(5) = tgn3str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(tgn_c4_f1(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c4_f1(tmp(t2)) = 180 - tgn_c4_f1(tmp(t2)) 
                       tgn4 = tgn_c4_f1(tmp)
                       moments = moment(tgn4)
                       tgn4_avg = moments(0)
                       tgn4_var = sqrt(moments(1))
                       distfreq = histogram(tgn4,min=min(tgn4))
                       maxfrq = max(distfreq)
                       tgn4_mode = where(distfreq eq maxfrq)+min(tgn4)                             
                       tgn4_med = median(tgn4)
                       tgn4_max = max(tgn4)
                       tgn4_min = min(tgn4)   
                       tgn4str = 'MEAN: ' + strtrim(string(tgn4_avg),2) + ' MEDIAN: ' + strtrim(string(tgn4_med),2) + ' MODE: ' + strtrim(string(tgn4_mode),2) + ' MAX: ' + strtrim(string(tgn4_max),2) + ' MIN: ' + strtrim(string(tgn4_min),2) + ' STD: '  + strtrim(string(tgn4_var),2)
                       tgn4str = 'THETA_GENDRIN_SC4--  ' + tgn4str
                       if q eq 0 then raydata_avg_f1.c4(5) = tgn4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(5) = tgn4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(5) = tgn4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(5) = tgn4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(5) = tgn4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(trs_c1_f1(tmp) gt 90)
                       if t2(0) ne -1 then trs_c1_f1(tmp(t2)) = 180 - trs_c1_f1(tmp(t2)) 
                       trs1 = trs_c1_f1(tmp)
                       moments = moment(trs1)
                       trs1_avg = moments(0)
                       trs1_var = sqrt(moments(1))
                       distfreq = histogram(trs1,min=min(trs1))
                       maxfrq = max(distfreq)
                       trs1_mode = where(distfreq eq maxfrq)+min(trs1)                             
                       trs1_med = median(trs1)
                       trs1_max = max(trs1)
                       trs1_min = min(trs1)                                                 
                       trs1str = 'MEAN: ' + strtrim(string(trs1_avg),2) + ' MEDIAN: ' + strtrim(string(trs1_med),2) + ' MODE: ' + strtrim(string(trs1_mode),2) + ' MAX: ' + strtrim(string(trs1_max),2) + ' MIN: ' + strtrim(string(trs1_min),2) + ' STD: '  + strtrim(string(trs1_var),2)
                       trs1str = 'THETA_RES_SC1--  ' + trs1str
                       if q eq 0 then raydata_avg_f1.c1(6) = trs1str(0)              
                       if q eq 1 then raydata_avg_ws_f1.c1(6) = trs1str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c1(6) = trs1str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c1(6) = trs1str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c1(6) = trs1str(0)                        
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(trs_c2_f1(tmp) gt 90)
                       if t2(0) ne -1 then trs_c2_f1(tmp(t2)) = 180 - trs_c2_f1(tmp(t2))                   
                       trs2 = trs_c2_f1(tmp)
                       moments = moment(trs2)
                       trs2_avg = moments(0)
                       trs2_var = sqrt(moments(1))
                       distfreq = histogram(trs2,min=min(trs2))
                       maxfrq = max(distfreq)
                       trs2_mode = where(distfreq eq maxfrq)+min(trs2)                             
                       trs2_med = median(trs2)
                       trs2_max = max(trs2)
                       trs2_min = min(trs2)   
                       trs2str = 'MEAN: ' + strtrim(string(trs2_avg),2) + ' MEDIAN: ' + strtrim(string(trs2_med),2) + ' MODE: ' + strtrim(string(trs2_mode),2) + ' MAX: ' + strtrim(string(trs2_max),2) + ' MIN: ' + strtrim(string(trs2_min),2) + ' STD: '  + strtrim(string(trs2_var),2)
                       trs2str = 'THETA_RES_SC2--  ' + trs2str
                       if q eq 0 then raydata_avg_f1.c2(6) = trs2str(0)
                       if q eq 1 then raydata_avg_ws_f1.c2(6) = trs2str(0)
                       if q eq 2 then raydata_avg_cc12_f1.c2(6) = trs2str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c2(6) = trs2str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c2(6) = trs2str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(trs_c3_f1(tmp) gt 90)
                       if t2(0) ne -1 then trs_c3_f1(tmp(t2)) = 180 - trs_c3_f1(tmp(t2))
                       trs3 = trs_c3_f1(tmp)
                       moments = moment(trs3)
                       trs3_avg = moments(0)
                       trs3_var = sqrt(moments(1))
                       distfreq = histogram(trs3,min=min(trs3))
                       maxfrq = max(distfreq)
                       trs3_mode = where(distfreq eq maxfrq)+min(trs3)                             
                       trs3_med = median(trs3)
                       trs3_max = max(trs3)
                       trs3_min = min(trs3)   
                       trs3str = 'MEAN: ' + strtrim(string(trs3_avg),2) + ' MEDIAN: ' + strtrim(string(trs3_med),2) + ' MODE: ' + strtrim(string(trs3_mode),2) + ' MAX: ' + strtrim(string(trs3_max),2) + ' MIN: ' + strtrim(string(trs3_min),2) + ' STD: '  + strtrim(string(trs3_var),2)
                       trs3str = 'THETA_RES_SC3--  ' + trs3str
                       if q eq 0 then raydata_avg_f1.c3(6) = trs3str(0)
                       if q eq 1 then raydata_avg_ws_f1.c3(6) = trs3str(0)
                       if q eq 3 then raydata_avg_cc13_f1.c3(6) = trs3str(0)
                       if q eq 5 then raydata_avg_cc23_f1.c3(6) = trs3str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c3(6) = trs3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(trs_c4_f1(tmp) gt 90)
                       if t2(0) ne -1 then trs_c4_f1(tmp(t2)) = 180 - trs_c4_f1(tmp(t2))
                       trs4 = trs_c4_f1(tmp)
                       moments = moment(trs4)
                       trs4_avg = moments(0)
                       trs4_var = sqrt(moments(1))
                       distfreq = histogram(trs4,min=min(trs4))
                       maxfrq = max(distfreq)
                       trs4_mode = where(distfreq eq maxfrq)+min(trs4)                             
                       trs4_med = median(trs4)
                       trs4_max = max(trs4)
                       trs4_min = min(trs4)   
                       trs4str = 'MEAN: ' + strtrim(string(trs4_avg),2) + ' MEDIAN: ' + strtrim(string(trs4_med),2) + ' MODE: ' + strtrim(string(trs4_mode),2) + ' MAX: ' + strtrim(string(trs4_max),2) + ' MIN: ' + strtrim(string(trs4_min),2) + ' STD: '  + strtrim(string(trs4_var),2)
                       trs4str = 'THETA_RES_SC4--  ' + trs4str
                       if q eq 0 then raydata_avg_f1.c4(6) = trs4str(0)
                       if q eq 1 then raydata_avg_ws_f1.c4(6) = trs4str(0)
                       if q eq 4 then raydata_avg_cc14_f1.c4(6) = trs4str(0)
                       if q eq 6 then raydata_avg_cc24_f1.c4(6) = trs4str(0)
                       if q eq 7 then raydata_avg_cc34_f1.c4(6) = trs4str(0)                      
                   endif                             

                   if q eq 0 then begin                         
                   openw,lun,'./raydata_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f1.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f1.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f1.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f1.c4
                   endif

                   if q eq 1 then begin
                   openw,lun,'./raydata_ws_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f1.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f1.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f1.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f1.c4
                   endif

                   if q eq 2 then begin
                   openw,lun,'./raydata_cc12_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc12_f1.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc12_f1.c2
                   endif

                   if q eq 3 then begin
                   openw,lun,'./raydata_cc13_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc13_f1.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc13_f1.c3
                   endif

                   if q eq 4 then begin
                   openw,lun,'./raydata_cc14_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc14_f1.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc14_f1.c4
                   endif

                   if q eq 5 then begin
                   openw,lun,'./raydata_cc23_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc23_f1.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc23_f1.c3                   
                   endif

                   if q eq 6 then begin
                   openw,lun,'./raydata_cc24_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc24_f1.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc24_f1.c4                   
                   endif

                   if q eq 7 then begin
                   openw,lun,'./raydata_cc34_f1.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc34_f1.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc34_f1.c4                   
                   endif

                   close,lun
                   free_lun,lun 



                   if q eq 0 then begin
                       raydata_avg_f2 = {c1:strarr(7),c2:strarr(7),c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_source eq 0.)
                   endif
                   if q eq 1 then begin
                       raydata_avg_ws_f2 = {c1:strarr(7),c2:strarr(7),c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_ws eq 0.)
                   endif
                   if q eq 2 then begin  ;cc12
                       raydata_avg_cc12_f2 = {c1:strarr(7),c2:strarr(7)}
                       tmp = where(not_keepers_cc12 eq 0.)
                   endif
                   if q eq 3 then begin  ;cc13
                       raydata_avg_cc13_f2 = {c1:strarr(7),c3:strarr(7)}
                       tmp = where(not_keepers_cc13 eq 0.)
                   endif
                   if q eq 4 then begin  ;cc14
                       raydata_avg_cc14_f2 = {c1:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc14 eq 0.)
                   endif
                   if q eq 5 then begin  ;cc23
                       raydata_avg_cc23_f2 = {c2:strarr(7),c3:strarr(7)}
                       tmp = where(not_keepers_cc23 eq 0.)
                   endif
                   if q eq 6 then begin  ;cc24
                       raydata_avg_cc24_f2 = {c2:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc24 eq 0.)
                   endif
                   if q eq 7 then begin  ;cc34
                       raydata_avg_cc34_f2 = {c3:strarr(7),c4:strarr(7)}
                       tmp = where(not_keepers_cc34 eq 0.)
                   endif

                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin      		     
                       t2 = where(theta_k_c1_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c1_f2(tmp(t2)) = 180 - theta_k_c1_f2(tmp(t2))        
                       tk1 = theta_k_c1_f2(tmp)
                       moments = moment(tk1)
                       tk1_avg = moments(0)
                       tk1_var = sqrt(moments(1))
                       distfreq = histogram(tk1,min=min(tk1))
                       maxfrq = max(distfreq)
                       tk1_mode = where(distfreq eq maxfrq)+min(tk1)                             
                       tk1_med = median(tk1)
                       tk1_max = max(tk1)
                       tk1_min = min(tk1)                                                
                       tk1str = 'MEAN: ' + strtrim(string(tk1_avg),2) + ' MEDIAN: ' + strtrim(string(tk1_med),2) + ' MODE: ' + strtrim(string(tk1_mode),2) + ' MAX: ' + strtrim(string(tk1_max),2) + ' MIN: ' + strtrim(string(tk1_min),2) + ' STD: '  + strtrim(string(tk1_var),2)
                       tk1str = 'THETA_K_SC1--  ' + tk1str
                       tk1str = tk1str(0)
                       if q eq 0 then raydata_avg_f2.c1(0) = tk1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(0) = tk1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(0) = tk1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(0) = tk1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(0) = tk1str(0)
                   endif

                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(theta_k_c2_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c2_f2(tmp(t2)) = 180 - theta_k_c2_f2(tmp(t2)) 
                       tk2 = theta_k_c2_f2(tmp)
                       moments = moment(tk2)
                       tk2_avg = moments(0)
                       tk2_var = sqrt(moments(1))
                       distfreq = histogram(tk2,min=min(tk2))
                       maxfrq = max(distfreq)
                       tk2_mode = where(distfreq eq maxfrq)+min(tk2)                             
                       tk2_med = median(tk2)
                       tk2_max = max(tk2)
                       tk2_min = min(tk2)  
                       tk2str = 'MEAN: ' + strtrim(string(tk2_avg),2) + ' MEDIAN: ' + strtrim(string(tk2_med),2) + ' MODE: ' + strtrim(string(tk2_mode),2) + ' MAX: ' + strtrim(string(tk2_max),2) + ' MIN: ' + strtrim(string(tk2_min),2) + ' STD: '  + strtrim(string(tk2_var),2) 
                       tk2str = 'THETA_K_SC2--  ' + tk2str
                       if q eq 0 then raydata_avg_f2.c2(0) = tk2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(0) = tk2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(0) = tk2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(0) = tk2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(0) = tk2str(0)
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(theta_k_c3_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c3_f2(tmp(t2)) = 180 - theta_k_c3_f2(tmp(t2)) 
                       tk3 = theta_k_c3_f2(tmp)
                       moments = moment(tk3)
                       tk3_avg = moments(0)
                       tk3_var = sqrt(moments(1))
                       distfreq = histogram(tk3,min=min(tk3))
                       maxfrq = max(distfreq)
                       tk3_mode = where(distfreq eq maxfrq)+min(tk3)                             
                       tk3_med = median(tk3)
                       tk3_max = max(tk3)
                       tk3_min = min(tk3)   
                       tk3str = 'MEAN: ' + strtrim(string(tk3_avg),2) + ' MEDIAN: ' + strtrim(string(tk3_med),2) + ' MODE: ' + strtrim(string(tk3_mode),2) + ' MAX: ' + strtrim(string(tk3_max),2) + ' MIN: ' + strtrim(string(tk3_min),2) + ' STD: '  + strtrim(string(tk3_var),2)
                       tk3str = 'THETA_K_SC3--  ' + tk3str
                       if q eq 0 then raydata_avg_f2.c3(0) = tk3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(0) = tk3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(0) = tk3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(0) = tk3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(0) = tk3str(0)
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(theta_k_c4_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_k_c4_f2(tmp(t2)) = 180 - theta_k_c4_f2(tmp(t2)) 
                       tk4 = theta_k_c4_f2(tmp)
                       moments = moment(tk4)
                       tk4_avg = moments(0)
                       tk4_var = sqrt(moments(1))
                       distfreq = histogram(tk4,min=min(tk4))
                       maxfrq = max(distfreq)
                       tk4_mode = where(distfreq eq maxfrq)+min(tk4)                             
                       tk4_med = median(tk4)
                       tk4_max = max(tk4)
                       tk4_min = min(tk4)   
                       tk4str = 'MEAN: ' + strtrim(string(tk4_avg),2) + ' MEDIAN: ' + strtrim(string(tk4_med),2) + ' MODE: ' + strtrim(string(tk4_mode),2) + ' MAX: ' + strtrim(string(tk4_max),2) + ' MIN: ' + strtrim(string(tk4_min),2) + ' STD: '  + strtrim(string(tk4_var),2)
                       tk4str = 'THETA_K_SC4--  ' + tk4str
                       if q eq 0 then raydata_avg_f2.c4(0) = tk4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(0) = tk4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(0) = tk4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(0) = tk4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(0) = tk4str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(theta_g_c1_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c1_f2(tmp(t2)) = 180 - theta_g_c1_f2(tmp(t2)) 
                       tg1 = theta_g_c1_f2(tmp)
                       moments = moment(tg1)
                       tg1_avg = moments(0)
                       tg1_var = sqrt(moments(1))
                       distfreq = histogram(tg1,min=min(tg1))
                       maxfrq = max(distfreq)
                       tg1_mode = where(distfreq eq maxfrq)+min(tg1)                             
                       tg1_med = median(tg1)
                       tg1_max = max(tg1)
                       tg1_min = min(tg1)                                                
                       tg1str = 'MEAN: ' + strtrim(string(tg1_avg),2) + ' MEDIAN: ' + strtrim(string(tg1_med),2) + ' MODE: ' + strtrim(string(tg1_mode),2) + ' MAX: ' + strtrim(string(tg1_max),2) + ' MIN: ' + strtrim(string(tg1_min),2) + ' STD: '  + strtrim(string(tg1_var),2)
                       tg1str = 'THETA_G_SC1--  ' + tg1str
                       tg1str = tg1str(0)
                       if q eq 0 then raydata_avg_f2.c1(1) = tg1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(1) = tg1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(1) = tg1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(1) = tg1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(1) = tg1str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(theta_g_c2_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c2_f2(tmp(t2)) = 180 - theta_g_c2_f2(tmp(t2)) 
                       tg2 = theta_g_c2_f2(tmp)
                       moments = moment(tg2)
                       tg2_avg = moments(0)
                       tg2_var = sqrt(moments(1))
                       distfreq = histogram(tg2,min=min(tg2))
                       maxfrq = max(distfreq)
                       tg2_mode = where(distfreq eq maxfrq)+min(tg2)                             
                       tg2_med = median(tg2)
                       tg2_max = max(tg2)
                       tg2_min = min(tg2)   
                       tg2str = 'MEAN: ' + strtrim(string(tg2_avg),2) + ' MEDIAN: ' + strtrim(string(tg2_med),2) + ' MODE: ' + strtrim(string(tg2_mode),2) + ' MAX: ' + strtrim(string(tg2_max),2) + ' MIN: ' + strtrim(string(tg2_min),2) + ' STD: '  + strtrim(string(tg2_var),2)
                       tg2str = 'THETA_G_SC2--  ' + tg2str
                       if q eq 0 then raydata_avg_f2.c2(1) = tg2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(1) = tg2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(1) = tg2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(1) = tg2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(1) = tg2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(theta_g_c3_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c3_f2(tmp(t2)) = 180 - theta_g_c3_f2(tmp(t2)) 
                       tg3 = theta_g_c3_f2(tmp)
                       moments = moment(tg3)
                       tg3_avg = moments(0)
                       tg3_var = sqrt(moments(1))
                       distfreq = histogram(tg3,min=min(tg3))
                       maxfrq = max(distfreq)
                       tg3_mode = where(distfreq eq maxfrq)+min(tg3)                             
                       tg3_med = median(tg3)
                       tg3_max = max(tg3)
                       tg3_min = min(tg3)   
                       tg3str = 'MEAN: ' + strtrim(string(tg3_avg),2) + ' MEDIAN: ' + strtrim(string(tg3_med),2) + ' MODE: ' + strtrim(string(tg3_mode),2) + ' MAX: ' + strtrim(string(tg3_max),2) + ' MIN: ' + strtrim(string(tg3_min),2) + ' STD: '  + strtrim(string(tg3_var),2)
                       tg3str = 'THETA_G_SC3--  ' + tg3str
                       if q eq 0 then raydata_avg_f2.c3(1) = tg3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(1) = tg3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(1) = tg3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(1) = tg3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(1) = tg3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(theta_g_c4_f2(tmp) gt 90)
                       if t2(0) ne -1 then theta_g_c4_f2(tmp(t2)) = 180 - theta_g_c4_f2(tmp(t2)) 
                       tg4 = theta_g_c4_f2(tmp)
                       moments = moment(tg4)
                       tg4_avg = moments(0)
                       tg4_var = sqrt(moments(1))
                       distfreq = histogram(tg4,min=min(tg4))
                       maxfrq = max(distfreq)
                       tg4_mode = where(distfreq eq maxfrq)+min(tg4)                             
                       tg4_med = median(tg4)
                       tg4_max = max(tg4)
                       tg4_min = min(tg4)   
                       tg4str = 'MEAN: ' + strtrim(string(tg4_avg),2) + ' MEDIAN: ' + strtrim(string(tg4_med),2) + ' MODE: ' + strtrim(string(tg4_mode),2) + ' MAX: ' + strtrim(string(tg4_max),2) + ' MIN: ' + strtrim(string(tg4_min),2) + ' STD: '  + strtrim(string(tg4_var),2)
                       tg4str = 'THETA_G_SC4--  ' + tg4str
                       if q eq 0 then raydata_avg_f2.c4(1) = tg4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(1) = tg4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(1) = tg4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(1) = tg4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(1) = tg4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       pre1 = pathre_c1_f2(tmp)
                       moments = moment(pre1)
                       pre1_avg = moments(0)
                       pre1_var = sqrt(moments(1))
                       distfreq = histogram(pre1,min=min(pre1))
                       maxfrq = max(distfreq)
                       pre1_mode = where(distfreq eq maxfrq)+min(pre1)                             
                       pre1_med = median(pre1)
                       pre1_max = max(pre1)
                       pre1_min = min(pre1)                                                    
                       pre1str = 'MEAN: ' + strtrim(string(pre1_avg),2) + ' MEDIAN: ' + strtrim(string(pre1_med),2) + ' MODE: ' + strtrim(string(pre1_mode),2) + ' MAX: ' + strtrim(string(pre1_max),2) + ' MIN: ' + strtrim(string(pre1_min),2) + ' STD: '  + strtrim(string(pre1_var),2)
                       pre1str = 'PATH_RE_SC1--  ' + pre1str
                       if q eq 0 then raydata_avg_f2.c1(2) = pre1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(2) = pre1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(2) = pre1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(2) = pre1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(2) = pre1str(0) 
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       pre2 = pathre_c2_f2(tmp)
                       moments = moment(pre2)
                       pre2_avg = moments(0)
                       pre2_var = sqrt(moments(1))
                       distfreq = histogram(pre2,min=min(pre2))
                       maxfrq = max(distfreq)
                       pre2_mode = where(distfreq eq maxfrq)+min(pre2)                             
                       pre2_med = median(pre2)
                       pre2_max = max(pre2)
                       pre2_min = min(pre2)   
                       pre2str = 'MEAN: ' + strtrim(string(pre2_avg),2) + ' MEDIAN: ' + strtrim(string(pre2_med),2) + ' MODE: ' + strtrim(string(pre2_mode),2) + ' MAX: ' + strtrim(string(pre2_max),2) + ' MIN: ' + strtrim(string(pre2_min),2) + ' STD: '  + strtrim(string(pre2_var),2)
                       pre2str = 'PATH_RE_SC2--  ' + pre2str
                       if q eq 0 then raydata_avg_f2.c2(2) = pre2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(2) = pre2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(2) = pre2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(2) = pre2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(2) = pre2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       pre3 = pathre_c3_f2(tmp)
                       moments = moment(pre3)
                       pre3_avg = moments(0)
                       pre3_var = sqrt(moments(1))
                       distfreq = histogram(pre3,min=min(pre3))
                       maxfrq = max(distfreq)
                       pre3_mode = where(distfreq eq maxfrq)+min(pre3)                             
                       pre3_med = median(pre3)
                       pre3_max = max(pre3)
                       pre3_min = min(pre3)   
                       pre3str = 'MEAN: ' + strtrim(string(pre3_avg),2) + ' MEDIAN: ' + strtrim(string(pre3_med),2) + ' MODE: ' + strtrim(string(pre3_mode),2) + ' MAX: ' + strtrim(string(pre3_max),2) + ' MIN: ' + strtrim(string(pre3_min),2) + ' STD: '  + strtrim(string(pre3_var),2)
                       pre3str = 'PATH_RE_SC3--  ' + pre3str
                       if q eq 0 then raydata_avg_f2.c3(2) = pre3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(2) = pre3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(2) = pre3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(2) = pre3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(2) = pre3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       pre4 = pathre_c4_f2(tmp)
                       moments = moment(pre4)
                       pre4_avg = moments(0)
                       pre4_var = sqrt(moments(1))
                       distfreq = histogram(pre4,min=min(pre4))
                       maxfrq = max(distfreq)
                       pre4_mode = where(distfreq eq maxfrq)+min(pre4)                             
                       pre4_med = median(pre4)
                       pre4_max = max(pre4)
                       pre4_min = min(pre4)   
                       pre4str = 'MEAN: ' + strtrim(string(pre4_avg),2) + ' MEDIAN: ' + strtrim(string(pre4_med),2) + ' MODE: ' + strtrim(string(pre4_mode),2) + ' MAX: ' + strtrim(string(pre4_max),2) + ' MIN: ' + strtrim(string(pre4_min),2) + ' STD: '  + strtrim(string(pre4_var),2)
                       pre4str = 'PATH_RE_SC4--  ' + pre4str
                       if q eq 0 then raydata_avg_f2.c4(2) = pre4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(2) = pre4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(2) = pre4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(2) = pre4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(2) = pre4str(0)                      
                   endif                   
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       ffce1 = ffce_c1_f2(tmp)
                       moments = moment(ffce1)
                       ffce1_avg = moments(0)
                       ffce1_var = sqrt(moments(1))
                       distfreq = histogram(ffce1,min=min(ffce1))
                       maxfrq = max(distfreq)
                       ffce1_mode = where(distfreq eq maxfrq)+min(ffce1)                             
                       ffce1_med = median(ffce1)
                       ffce1_max = max(ffce1)
                       ffce1_min = min(ffce1)                                                    
                       ffce1str = 'MEAN: ' + strtrim(string(ffce1_avg),2) + ' MEDIAN: ' + strtrim(string(ffce1_med),2) + ' MODE: ' + strtrim(string(ffce1_mode),2) + ' MAX: ' + strtrim(string(ffce1_max),2) + ' MIN: ' + strtrim(string(ffce1_min),2) + ' STD: '  + strtrim(string(ffce1_var),2)
                       ffce1str = 'f/fce_SC1--  ' + ffce1str
                       if q eq 0 then raydata_avg_f2.c1(3) = ffce1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(3) = ffce1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(3) = ffce1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(3) = ffce1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(3) = ffce1str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       ffce2 = ffce_c2_f2(tmp)
                       moments = moment(ffce2)
                       ffce2_avg = moments(0)
                       ffce2_var = sqrt(moments(1))
                       distfreq = histogram(ffce2,min=min(ffce2))
                       maxfrq = max(distfreq)
                       ffce2_mode = where(distfreq eq maxfrq)+min(ffce2)                             
                       ffce2_med = median(ffce2)
                       ffce2_max = max(ffce2)
                       ffce2_min = min(ffce2)   
                       ffce2str = 'MEAN: ' + strtrim(string(ffce2_avg),2) + ' MEDIAN: ' + strtrim(string(ffce2_med),2) + ' MODE: ' + strtrim(string(ffce2_mode),2) + ' MAX: ' + strtrim(string(ffce2_max),2) + ' MIN: ' + strtrim(string(ffce2_min),2) + ' STD: '  + strtrim(string(ffce2_var),2)
                       ffce2str = 'f/fce_SC2--  ' + ffce2str
                       if q eq 0 then raydata_avg_f2.c2(3) = ffce2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(3) = ffce2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(3) = ffce2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(3) = ffce2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(3) = ffce2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       ffce3 = ffce_c3_f2(tmp)
                       moments = moment(ffce3)
                       ffce3_avg = moments(0)
                       ffce3_var = sqrt(moments(1))
                       distfreq = histogram(ffce3,min=min(ffce3))
                       maxfrq = max(distfreq)
                       ffce3_mode = where(distfreq eq maxfrq)+min(ffce3)                             
                       ffce3_med = median(ffce3)
                       ffce3_max = max(ffce3)
                       ffce3_min = min(ffce3)   
                       ffce3str = 'MEAN: ' + strtrim(string(ffce3_avg),2) + ' MEDIAN: ' + strtrim(string(ffce3_med),2) + ' MODE: ' + strtrim(string(ffce3_mode),2) + ' MAX: ' + strtrim(string(ffce3_max),2) + ' MIN: ' + strtrim(string(ffce3_min),2) + ' STD: '  + strtrim(string(ffce3_var),2)
                       ffce3str = 'f/fce_SC3--  ' + ffce3str
                       if q eq 0 then raydata_avg_f2.c3(3) = ffce3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(3) = ffce3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(3) = ffce3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(3) = ffce3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(3) = ffce3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       ffce4 = ffce_c4_f2(tmp)
                       moments = moment(ffce4)
                       ffce4_avg = moments(0)
                       ffce4_var = sqrt(moments(1))
                       distfreq = histogram(ffce4,min=min(ffce4))
                       maxfrq = max(distfreq)
                       ffce4_mode = where(distfreq eq maxfrq)+min(ffce4)                             
                       ffce4_med = median(ffce4)
                       ffce4_max = max(ffce4)
                       ffce4_min = min(ffce4)   
                       ffce4str = 'MEAN: ' + strtrim(string(ffce4_avg),2) + ' MEDIAN: ' + strtrim(string(ffce4_med),2) + ' MODE: ' + strtrim(string(ffce4_mode),2) + ' MAX: ' + strtrim(string(ffce4_max),2) + ' MIN: ' + strtrim(string(ffce4_min),2) + ' STD: '  + strtrim(string(ffce4_var),2)
                       ffce4str = 'f/fce_SC4--  ' + ffce4str
                       if q eq 0 then raydata_avg_f2.c4(3) = ffce4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(3) = ffce4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(3) = ffce4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(3) = ffce4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(3) = ffce4str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       rdx1 = rdx_c1_f2(tmp)
                       moments = moment(rdx1)
                       rdx1_avg = moments(0)
                       rdx1_var = sqrt(moments(1))
                       distfreq = histogram(rdx1,min=min(rdx1))
                       maxfrq = max(distfreq)
                       rdx1_mode = where(distfreq eq maxfrq)+min(rdx1)                             
                       rdx1_med = median(rdx1)
                       rdx1_max = max(rdx1)
                       rdx1_min = min(rdx1)                                                  
                       rdx1str = 'MEAN: ' + strtrim(string(rdx1_avg),2) + ' MEDIAN: ' + strtrim(string(rdx1_med),2) + ' MODE: ' + strtrim(string(rdx1_mode),2) + ' MAX: ' + strtrim(string(rdx1_max),2) + ' MIN: ' + strtrim(string(rdx1_min),2) + ' STD: '  + strtrim(string(rdx1_var),2)
                       rdx1str = 'REFNDX_SC1--  ' + rdx1str
                       if q eq 0 then raydata_avg_f2.c1(4) = rdx1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(4) = rdx1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(4) = rdx1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(4) = rdx1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(4) = rdx1str(0)                        
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       rdx2 = rdx_c2_f2(tmp)
                       moments = moment(rdx2)
                       rdx2_avg = moments(0)
                       rdx2_var = sqrt(moments(1))
                       distfreq = histogram(rdx2,min=min(rdx2))
                       maxfrq = max(distfreq)
                       rdx2_mode = where(distfreq eq maxfrq)+min(rdx2)                             
                       rdx2_med = median(rdx2)
                       rdx2_max = max(rdx2)
                       rdx2_min = min(rdx2)   
                       rdx2str = 'MEAN: ' + strtrim(string(rdx2_avg),2) + ' MEDIAN: ' + strtrim(string(rdx2_med),2) + ' MODE: ' + strtrim(string(rdx2_mode),2) + ' MAX: ' + strtrim(string(rdx2_max),2) + ' MIN: ' + strtrim(string(rdx2_min),2) + ' STD: '  + strtrim(string(rdx2_var),2)
                       rdx2str = 'REFNDX_SC2--  ' + rdx2str
                       if q eq 0 then raydata_avg_f2.c2(4) = rdx2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(4) = rdx2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(4) = rdx2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(4) = rdx2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(4) = rdx2str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       rdx3 = rdx_c3_f2(tmp)
                       moments = moment(rdx3)
                       rdx3_avg = moments(0)
                       rdx3_var = sqrt(moments(1))
                       distfreq = histogram(rdx3,min=min(rdx3))
                       maxfrq = max(distfreq)
                       rdx3_mode = where(distfreq eq maxfrq)+min(rdx3)                             
                       rdx3_med = median(rdx3)
                       rdx3_max = max(rdx3)
                       rdx3_min = min(rdx3)   
                       rdx3str = 'MEAN: ' + strtrim(string(rdx3_avg),2) + ' MEDIAN: ' + strtrim(string(rdx3_med),2) + ' MODE: ' + strtrim(string(rdx3_mode),2) + ' MAX: ' + strtrim(string(rdx3_max),2) + ' MIN: ' + strtrim(string(rdx3_min),2) + ' STD: '  + strtrim(string(rdx3_var),2)
                       rdx3str = 'REFNDX_SC3--  ' + rdx3str
                       if q eq 0 then raydata_avg_f2.c3(4) = rdx3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(4) = rdx3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(4) = rdx3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(4) = rdx3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(4) = rdx3str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       rdx4 = rdx_c4_f2(tmp)
                       moments = moment(rdx4)
                       rdx4_avg = moments(0)
                       rdx4_var = sqrt(moments(1))
                       distfreq = histogram(rdx4,min=min(rdx4))
                       maxfrq = max(distfreq)
                       rdx4_mode = where(distfreq eq maxfrq)+min(rdx4)                             
                       rdx4_med = median(rdx4)
                       rdx4_max = max(rdx4)
                       rdx4_min = min(rdx4)   
                       rdx4str = 'MEAN: ' + strtrim(string(rdx4_avg),2) + ' MEDIAN: ' + strtrim(string(rdx4_med),2) + ' MODE: ' + strtrim(string(rdx4_mode),2) + ' MAX: ' + strtrim(string(rdx4_max),2) + ' MIN: ' + strtrim(string(rdx4_min),2) + ' STD: '  + strtrim(string(rdx4_var),2)
                       rdx4str = 'REFNDX_SC4--  ' + rdx4str
                       if q eq 0 then raydata_avg_f2.c4(4) = rdx4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(4) = rdx4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(4) = rdx4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(4) = rdx4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(4) = rdx4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(tgn_c1_f2(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c1_f2(tmp(t2)) = 180 - tgn_c1_f2(tmp(t2)) 
                       tgn1 = tgn_c1_f2(tmp)
                       moments = moment(tgn1)
                       tgn1_avg = moments(0)
                       tgn1_var = sqrt(moments(1))
                       distfreq = histogram(tgn1,min=min(tgn1))
                       maxfrq = max(distfreq)
                       tgn1_mode = where(distfreq eq maxfrq)+min(tgn1)                             
                       tgn1_med = median(tgn1)
                       tgn1_max = max(tgn1)
                       tgn1_min = min(tgn1)                                       
                       tgn1str = 'MEAN: ' + strtrim(string(tgn1_avg),2) + ' MEDIAN: ' + strtrim(string(tgn1_med),2) + ' MODE: ' + strtrim(string(tgn1_mode),2) + ' MAX: ' + strtrim(string(tgn1_max),2) + ' MIN: ' + strtrim(string(tgn1_min),2) + ' STD: '  + strtrim(string(tgn1_var),2)
                       tgn1str = 'THETA_GENDRIN_SC1--  ' + tgn1str
                       if q eq 0 then raydata_avg_f2.c1(5) = tgn1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(5) = tgn1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(5) = tgn1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(5) = tgn1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(5) = tgn1str(0)               
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(tgn_c2_f2(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c2_f2(tmp(t2)) = 180 - tgn_c2_f2(tmp(t2)) 
                       tgn2 = tgn_c2_f2(tmp)
                       moments = moment(tgn2)
                       tgn2_avg = moments(0)
                       tgn2_var = sqrt(moments(1))
                       distfreq = histogram(tgn2,min=min(tgn2))
                       maxfrq = max(distfreq)
                       tgn2_mode = where(distfreq eq maxfrq)+min(tgn2)                             
                       tgn2_med = median(tgn2)
                       tgn2_max = max(tgn2)
                       tgn2_min = min(tgn2)   
                       tgn2str = 'MEAN: ' + strtrim(string(tgn2_avg),2) + ' MEDIAN: ' + strtrim(string(tgn2_med),2) + ' MODE: ' + strtrim(string(tgn2_mode),2) + ' MAX: ' + strtrim(string(tgn2_max),2) + ' MIN: ' + strtrim(string(tgn2_min),2) + ' STD: '  + strtrim(string(tgn2_var),2)
                       tgn2str = 'THETA_GENDRIN_SC2--  ' + tgn2str
                       if q eq 0 then raydata_avg_f2.c2(5) = tgn2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(5) = tgn2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(5) = tgn2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(5) = tgn2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(5) = tgn2str(0)                       
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(tgn_c3_f2(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c3_f2(tmp(t2)) = 180 - tgn_c3_f2(tmp(t2)) 
                       tgn3 = tgn_c3_f2(tmp)
                       moments = moment(tgn3)
                       tgn3_avg = moments(0)
                       tgn3_var = sqrt(moments(1))
                       distfreq = histogram(tgn3,min=min(tgn3))
                       maxfrq = max(distfreq)
                       tgn3_mode = where(distfreq eq maxfrq)+min(tgn3)                             
                       tgn3_med = median(tgn3)
                       tgn3_max = max(tgn3)
                       tgn3_min = min(tgn3)   
                       tgn3str = 'MEAN: ' + strtrim(string(tgn3_avg),2) + ' MEDIAN: ' + strtrim(string(tgn3_med),2) + ' MODE: ' + strtrim(string(tgn3_mode),2) + ' MAX: ' + strtrim(string(tgn3_max),2) + ' MIN: ' + strtrim(string(tgn3_min),2) + ' STD: '  + strtrim(string(tgn3_var),2)
                       tgn3str = 'THETA_GENDRIN_SC3--  ' + tgn3str
                       if q eq 0 then raydata_avg_f2.c3(5) = tgn3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(5) = tgn3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(5) = tgn3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(5) = tgn3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(5) = tgn3str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(tgn_c4_f2(tmp) gt 90)
                       if t2(0) ne -1 then tgn_c4_f2(tmp(t2)) = 180 - tgn_c4_f2(tmp(t2)) 
                       tgn4 = tgn_c4_f2(tmp)
                       moments = moment(tgn4)
                       tgn4_avg = moments(0)
                       tgn4_var = sqrt(moments(1))
                       distfreq = histogram(tgn4,min=min(tgn4))
                       maxfrq = max(distfreq)
                       tgn4_mode = where(distfreq eq maxfrq)+min(tgn4)                             
                       tgn4_med = median(tgn4)
                       tgn4_max = max(tgn4)
                       tgn4_min = min(tgn4)   
                       tgn4str = 'MEAN: ' + strtrim(string(tgn4_avg),2) + ' MEDIAN: ' + strtrim(string(tgn4_med),2) + ' MODE: ' + strtrim(string(tgn4_mode),2) + ' MAX: ' + strtrim(string(tgn4_max),2) + ' MIN: ' + strtrim(string(tgn4_min),2) + ' STD: '  + strtrim(string(tgn4_var),2)
                       tgn4str = 'THETA_GENDRIN_SC4--  ' + tgn4str
                       if q eq 0 then raydata_avg_f2.c4(5) = tgn4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(5) = tgn4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(5) = tgn4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(5) = tgn4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(5) = tgn4str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 3 or q eq 4) then begin               
                       t2 = where(trs_c1_f2(tmp) gt 90)
                       if t2(0) ne -1 then trs_c1_f2(tmp(t2)) = 180 - trs_c1_f2(tmp(t2)) 
                       trs1 = trs_c1_f2(tmp)
                       moments = moment(trs1)
                       trs1_avg = moments(0)
                       trs1_var = sqrt(moments(1))
                       distfreq = histogram(trs1,min=min(trs1))
                       maxfrq = max(distfreq)
                       trs1_mode = where(distfreq eq maxfrq)+min(trs1)                             
                       trs1_med = median(trs1)
                       trs1_max = max(trs1)
                       trs1_min = min(trs1)                                                 
                       trs1str = 'MEAN: ' + strtrim(string(trs1_avg),2) + ' MEDIAN: ' + strtrim(string(trs1_med),2) + ' MODE: ' + strtrim(string(trs1_mode),2) + ' MAX: ' + strtrim(string(trs1_max),2) + ' MIN: ' + strtrim(string(trs1_min),2) + ' STD: '  + strtrim(string(trs1_var),2)
                       trs1str = 'THETA_RES_SC1--  ' + trs1str
                       if q eq 0 then raydata_avg_f2.c1(6) = trs1str(0)              
                       if q eq 1 then raydata_avg_ws_f2.c1(6) = trs1str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c1(6) = trs1str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c1(6) = trs1str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c1(6) = trs1str(0)                        
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 2 or q eq 5 or q eq 6) then begin
                       t2 = where(trs_c2_f2(tmp) gt 90)
                       if t2(0) ne -1 then trs_c2_f2(tmp(t2)) = 180 - trs_c2_f2(tmp(t2))                   
                       trs2 = trs_c2_f2(tmp)
                       moments = moment(trs2)
                       trs2_avg = moments(0)
                       trs2_var = sqrt(moments(1))
                       distfreq = histogram(trs2,min=min(trs2))
                       maxfrq = max(distfreq)
                       trs2_mode = where(distfreq eq maxfrq)+min(trs2)                             
                       trs2_med = median(trs2)
                       trs2_max = max(trs2)
                       trs2_min = min(trs2)   
                       trs2str = 'MEAN: ' + strtrim(string(trs2_avg),2) + ' MEDIAN: ' + strtrim(string(trs2_med),2) + ' MODE: ' + strtrim(string(trs2_mode),2) + ' MAX: ' + strtrim(string(trs2_max),2) + ' MIN: ' + strtrim(string(trs2_min),2) + ' STD: '  + strtrim(string(trs2_var),2)
                       trs2str = 'THETA_RES_SC2--  ' + trs2str
                       if q eq 0 then raydata_avg_f2.c2(6) = trs2str(0)
                       if q eq 1 then raydata_avg_ws_f2.c2(6) = trs2str(0)
                       if q eq 2 then raydata_avg_cc12_f2.c2(6) = trs2str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c2(6) = trs2str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c2(6) = trs2str(0)                     
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 3 or q eq 5 or q eq 7) then begin
                       t2 = where(trs_c3_f2(tmp) gt 90)
                       if t2(0) ne -1 then trs_c3_f2(tmp(t2)) = 180 - trs_c3_f2(tmp(t2))
                       trs3 = trs_c3_f2(tmp)
                       moments = moment(trs3)
                       trs3_avg = moments(0)
                       trs3_var = sqrt(moments(1))
                       distfreq = histogram(trs3,min=min(trs3))
                       maxfrq = max(distfreq)
                       trs3_mode = where(distfreq eq maxfrq)+min(trs3)                             
                       trs3_med = median(trs3)
                       trs3_max = max(trs3)
                       trs3_min = min(trs3)   
                       trs3str = 'MEAN: ' + strtrim(string(trs3_avg),2) + ' MEDIAN: ' + strtrim(string(trs3_med),2) + ' MODE: ' + strtrim(string(trs3_mode),2) + ' MAX: ' + strtrim(string(trs3_max),2) + ' MIN: ' + strtrim(string(trs3_min),2) + ' STD: '  + strtrim(string(trs3_var),2)
                       trs3str = 'THETA_RES_SC3--  ' + trs3str
                       if q eq 0 then raydata_avg_f2.c3(6) = trs3str(0)
                       if q eq 1 then raydata_avg_ws_f2.c3(6) = trs3str(0)
                       if q eq 3 then raydata_avg_cc13_f2.c3(6) = trs3str(0)
                       if q eq 5 then raydata_avg_cc23_f2.c3(6) = trs3str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c3(6) = trs3str(0)                      
                   endif
                   if tmp(0) ne -1 and (q eq 0 or q eq 1 or q eq 4 or q eq 6 or q eq 7) then begin
                       t2 = where(trs_c4_f2(tmp) gt 90)
                       if t2(0) ne -1 then trs_c4_f2(tmp(t2)) = 180 - trs_c4_f2(tmp(t2))
                       trs4 = trs_c4_f2(tmp)
                       moments = moment(trs4)
                       trs4_avg = moments(0)
                       trs4_var = sqrt(moments(1))
                       distfreq = histogram(trs4,min=min(trs4))
                       maxfrq = max(distfreq)
                       trs4_mode = where(distfreq eq maxfrq)+min(trs4)                             
                       trs4_med = median(trs4)
                       trs4_max = max(trs4)
                       trs4_min = min(trs4)   
                       trs4str = 'MEAN: ' + strtrim(string(trs4_avg),2) + ' MEDIAN: ' + strtrim(string(trs4_med),2) + ' MODE: ' + strtrim(string(trs4_mode),2) + ' MAX: ' + strtrim(string(trs4_max),2) + ' MIN: ' + strtrim(string(trs4_min),2) + ' STD: '  + strtrim(string(trs4_var),2)
                       trs4str = 'THETA_RES_SC4--  ' + trs4str
                       if q eq 0 then raydata_avg_f2.c4(6) = trs4str(0)
                       if q eq 1 then raydata_avg_ws_f2.c4(6) = trs4str(0)
                       if q eq 4 then raydata_avg_cc14_f2.c4(6) = trs4str(0)
                       if q eq 6 then raydata_avg_cc24_f2.c4(6) = trs4str(0)
                       if q eq 7 then raydata_avg_cc34_f2.c4(6) = trs4str(0)                      
                   endif                             

                   if q eq 0 then begin                         
                   openw,lun,'./raydata_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f2.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f2.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f2.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_f2.c4
                   endif

                   if q eq 1 then begin
                   openw,lun,'./raydata_ws_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f2.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f2.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f2.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_ws_f2.c4
                   endif

                   if q eq 2 then begin
                   openw,lun,'./raydata_cc12_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc12_f2.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc12_f2.c2
                   endif

                   if q eq 3 then begin
                   openw,lun,'./raydata_cc13_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc13_f2.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc13_f2.c3
                   endif

                   if q eq 4 then begin
                   openw,lun,'./raydata_cc14_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc14_f2.c1
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc14_f2.c4
                   endif

                   if q eq 5 then begin
                   openw,lun,'./raydata_cc23_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc23_f2.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc23_f2.c3                   
                   endif

                   if q eq 6 then begin
                   openw,lun,'./raydata_cc24_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc24_f2.c2
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc24_f2.c4                   
                   endif

                   if q eq 7 then begin
                   openw,lun,'./raydata_cc34_f2.txt',/get_lun
                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc34_f2.c3
                   printf,lun,'******************'
                   printf,lun,raydata_avg_cc34_f2.c4                   
                   endif

                   close,lun
                   free_lun,lun 

endfor

xvals = indgen(16)*5

;lets subtract (180 - angle) for angles gt 90 degrees. 
tmp = where((tgn_c1_f1 ne -10000) and (tgn_c1_f1 ne 0.))
t2 = where(tgn_c1_f1(tmp) gt 90.)
if t2(0) ne -1 then tgn_c1_f1(tmp(t2)) = 180 - tgn_c1_f1(tmp(t2))
tmp = where((tgn_c2_f1 ne -10000) and (tgn_c2_f1 ne 0.))
t2 = where(tgn_c2_f1(tmp) gt 90.)
if t2(0) ne -1 then tgn_c2_f1(tmp(t2)) = 180 - tgn_c2_f1(tmp(t2))
tmp = where((tgn_c3_f1 ne -10000) and (tgn_c3_f1 ne 0.))
t2 = where(tgn_c3_f1(tmp) gt 90.)
if t2(0) ne -1 then tgn_c3_f1(tmp(t2)) = 180 - tgn_c3_f1(tmp(t2))
tmp = where((tgn_c4_f1 ne -10000) and (tgn_c4_f1 ne 0.))
t2 = where(tgn_c4_f1(tmp) gt 90.)
if t2(0) ne -1 then tgn_c4_f1(tmp(t2)) = 180 - tgn_c4_f1(tmp(t2))


tmp = where((theta_k_c1_f1 ne -10000.) and (theta_k_c1_f1 ne 0.))
t2 = where(theta_k_c1_f1(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c1_f1(tmp(t2)) = 180. - theta_k_c1_f1(tmp(t2))
tmp = where((theta_k_c2_f1 ne -10000.) and (theta_k_c2_f1 ne 0.))
t2 = where(theta_k_c2_f1(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c2_f1(tmp(t2)) = 180. - theta_k_c2_f1(tmp(t2))
tmp = where((theta_k_c3_f1 ne -10000.) and (theta_k_c3_f1 ne 0.))
t2 = where(theta_k_c3_f1(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c3_f1(tmp(t2)) = 180. - theta_k_c3_f1(tmp(t2))
tmp = where((theta_k_c4_f1 ne -10000.) and (theta_k_c4_f1 ne 0.))
t2 = where(theta_k_c4_f1(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c4_f1(tmp(t2)) = 180. - theta_k_c4_f1(tmp(t2))


tmp2 = where((trs_c1_f1 ne -10000.) and (trs_c1_f1 ne 0.))
t2 = where(trs_c1_f1(tmp2) gt 90.)
if t2(0) ne -1 then trs_c1_f1(tmp2(t2)) = 180. - trs_c1_f1(tmp2(t2))
tmp2 = where((trs_c2_f1 ne -10000.) and (trs_c2_f1 ne 0.))
t2 = where(trs_c2_f1(tmp2) gt 90.)
if t2(0) ne -1 then trs_c2_f1(tmp2(t2)) = 180. - trs_c2_f1(tmp2(t2))
tmp2 = where((trs_c3_f1 ne -10000.) and (trs_c3_f1 ne 0.))
t2 = where(trs_c3_f1(tmp2) gt 90.)
if t2(0) ne -1 then trs_c3_f1(tmp2(t2)) = 180. - trs_c3_f1(tmp2(t2))
tmp2 = where((trs_c4_f1 ne -10000.) and (trs_c4_f1 ne 0.))
t2 = where(trs_c4_f1(tmp2) gt 90.)
if t2(0) ne -1 then trs_c4_f1(tmp2(t2)) = 180. - trs_c4_f1(tmp2(t2))



tmp = where((tgn_c1_f2 ne -10000) and (tgn_c1_f2 ne 0.))
t2 = where(tgn_c1_f2(tmp) gt 90.)
if t2(0) ne -1 then tgn_c1_f2(tmp(t2)) = 180 - tgn_c1_f2(tmp(t2))
tmp = where((tgn_c2_f2 ne -10000) and (tgn_c2_f2 ne 0.))
t2 = where(tgn_c2_f2(tmp) gt 90.)
if t2(0) ne -1 then tgn_c2_f2(tmp(t2)) = 180 - tgn_c2_f2(tmp(t2))
tmp = where((tgn_c3_f2 ne -10000) and (tgn_c3_f2 ne 0.))
t2 = where(tgn_c3_f2(tmp) gt 90.)
if t2(0) ne -1 then tgn_c3_f2(tmp(t2)) = 180 - tgn_c3_f2(tmp(t2))
tmp = where((tgn_c4_f2 ne -10000) and (tgn_c4_f2 ne 0.))
t2 = where(tgn_c4_f2(tmp) gt 90.)
if t2(0) ne -1 then tgn_c4_f2(tmp(t2)) = 180 - tgn_c4_f2(tmp(t2))


tmp = where((theta_k_c1_f2 ne -10000.) and (theta_k_c1_f2 ne 0.))
t2 = where(theta_k_c1_f2(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c1_f2(tmp(t2)) = 180. - theta_k_c1_f2(tmp(t2))
tmp = where((theta_k_c2_f2 ne -10000.) and (theta_k_c2_f2 ne 0.))
t2 = where(theta_k_c2_f2(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c2_f2(tmp(t2)) = 180. - theta_k_c2_f2(tmp(t2))
tmp = where((theta_k_c3_f2 ne -10000.) and (theta_k_c3_f2 ne 0.))
t2 = where(theta_k_c3_f2(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c3_f2(tmp(t2)) = 180. - theta_k_c3_f2(tmp(t2))
tmp = where((theta_k_c4_f2 ne -10000.) and (theta_k_c4_f2 ne 0.))
t2 = where(theta_k_c4_f2(tmp) gt 90.)
if t2(0) ne -1 then theta_k_c4_f2(tmp(t2)) = 180. - theta_k_c4_f2(tmp(t2))


tmp2 = where((trs_c1_f2 ne -10000.) and (trs_c1_f2 ne 0.))
t2 = where(trs_c1_f2(tmp2) gt 90.)
if t2(0) ne -1 then trs_c1_f2(tmp2(t2)) = 180. - trs_c1_f2(tmp2(t2))
tmp2 = where((trs_c2_f2 ne -10000.) and (trs_c2_f2 ne 0.))
t2 = where(trs_c2_f2(tmp2) gt 90.)
if t2(0) ne -1 then trs_c2_f2(tmp2(t2)) = 180. - trs_c2_f2(tmp2(t2))
tmp2 = where((trs_c3_f2 ne -10000.) and (trs_c3_f2 ne 0.))
t2 = where(trs_c3_f2(tmp2) gt 90.)
if t2(0) ne -1 then trs_c3_f2(tmp2(t2)) = 180. - trs_c3_f2(tmp2(t2))
tmp2 = where((trs_c4_f2 ne -10000.) and (trs_c4_f2 ne 0.))
t2 = where(trs_c4_f2(tmp2) gt 90.)
if t2(0) ne -1 then trs_c4_f2(tmp2(t2)) = 180. - trs_c4_f2(tmp2(t2))




;##############################################################
;WHOLE SOURCE HISTOGRAMS
;##############################################################
;HISTOGRAMS FOR THETA_K AS PERCENTAGE OF ENTIRE AREA OF OVERLAP FOR THE FIRST FREQ
;SOURCE REGION NORMALIZED TO THE OVERALL REGION (SOURCE + FILLED IN)
;##############################################################

entire_source = 'no'
if entire_source eq 'yes' then begin
for i=0,7 do begin

set_plot,'ps'

if i eq 0 then device,filename='theta_k_hist_source_f1.ps'
if i eq 1 then device,filename='theta_k_hist_ws_f1.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc13_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc14_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc23_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc24_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc34_f1.ps'
!p.font=0
!p.multi=[0,2,2]
;window,xsize=700,ysize=700
;####

if (i eq 0 or i eq 1 or i eq 2 or i eq 3 or i eq 4) then begin
;C1

all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.))

;####
;to find keepers;  -1*(not_keepers - 1) --> keepers

if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c1_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C1 SOURCE'
if i eq 1 then title = 'C1 WS'
if i eq 2 then title = 'C1 CC12'
if i eq 3 then title = 'C1 CC13'
if i eq 4 then title = 'C1 CC14'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###########
;C2

if (i eq 0 or i eq 1 or i eq 2 or i eq 5 or i eq 6) then begin

all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.))
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c2_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C2 SOURCE'
if i eq 1 then title = 'C2 WS'
if i eq 2 then title = 'C2 CC12'
if i eq 5 then title = 'C2 CC23'
if i eq 6 then title = 'C2 CC24'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;#############
;C3

if (i eq 0 or i eq 1 or i eq 3 or i eq 5 or i eq 7) then begin

all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.))
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c3_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C3 SOURCE'
if i eq 1 then title = 'C3 WS'
if i eq 3 then title = 'C3 CC13'
if i eq 5 then title = 'C3 CC23'
if i eq 7 then title = 'C3 CC34'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;##########
;C4

if (i eq 0 or i eq 1 or i eq 4 or i eq 6 or i eq 7) then begin

all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.))
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c4_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C4 SOURCE'
if i eq 1 then title = 'C4 WS'
if i eq 4 then title = 'C4 CC14'
if i eq 6 then title = 'C4 CC24'
if i eq 7 then title = 'C4 CC34'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###############################################################

device,/close
!p.font=-1
set_plot,'x'
endfor


;#########################################################
;##########################################################
;HISTOGRAMS FOR THETA_K FOR THE NUMBER OF ACTUAL SOURCE POINTS FOR THE FIRST FREQ
;ONLY THE SOURCE REGION
;#########################################################

for i=0,7 do begin

set_plot,'ps'

if i eq 0 then device,filename='theta_k_hist_source_f1_2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_f1_2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_f1_2.ps'
if i eq 3 then device,filename='theta_k_hist_cc13_f1_2.ps'
if i eq 4 then device,filename='theta_k_hist_cc14_f1_2.ps'
if i eq 5 then device,filename='theta_k_hist_cc23_f1_2.ps'
if i eq 6 then device,filename='theta_k_hist_cc24_f1_2.ps'
if i eq 7 then device,filename='theta_k_hist_cc34_f1_2.ps'
!p.font=0
!p.multi=[0,2,2]
;####

if (i eq 0 or i eq 1 or i eq 2 or i eq 3 or i eq 4) then begin
;C1

if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C1 SOURCE'
if i eq 1 then title = 'C1 WS'
if i eq 2 then title = 'C1 CC12'
if i eq 3 then title = 'C1 CC13'
if i eq 4 then title = 'C1 CC14'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###########
;C2

if (i eq 0 or i eq 1 or i eq 2 or i eq 5 or i eq 6) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1

;norm = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.))
;tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C2 SOURCE'
if i eq 1 then title = 'C2 WS'
if i eq 2 then title = 'C2 CC12'
if i eq 5 then title = 'C2 CC23'
if i eq 6 then title = 'C2 CC24'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;#############
;C3
if (i eq 0 or i eq 1 or i eq 3 or i eq 5 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C3 SOURCE'
if i eq 1 then title = 'C3 WS'
if i eq 3 then title = 'C3 CC13'
if i eq 5 then title = 'C3 CC23'
if i eq 7 then title = 'C3 CC34'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;##########
;C4

if (i eq 0 or i eq 1 or i eq 4 or i eq 6 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C4 SOURCE'
if i eq 1 then title = 'C4 WS'
if i eq 4 then title = 'C4 CC14'
if i eq 6 then title = 'C4 CC24'
if i eq 7 then title = 'C4 CC34'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###############################################################

device,/close
!p.font=-1
set_plot,'x'
endfor



;##################################################################
;###################################################################
;HISTOGRAMS FOR THETA_K AS PERCENTAGE OF ENTIRE AREA OF OVERLAP FOR THE SECOND FREQ
;SOURCE REGION NORMALIZED TO THE OVERALL REGION (SOURCE + FILLED IN)
;##################################################################

for i=0,7 do begin

set_plot,'ps'

if i eq 0 then device,filename='theta_k_hist_source_f2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_f2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc13_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc14_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc23_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc24_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc34_f2.ps'
!p.font=0
!p.multi=[0,2,2]
;window,xsize=700,ysize=700
;####

if (i eq 0 or i eq 1 or i eq 2 or i eq 3 or i eq 4) then begin
;C1

all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.))

if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c1_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C1 SOURCE'
if i eq 1 then title = 'C1 WS'
if i eq 2 then title = 'C1 CC12'
if i eq 3 then title = 'C1 CC13'
if i eq 4 then title = 'C1 CC14'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###########
;C2

all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.))
if (i eq 0 or i eq 1 or i eq 2 or i eq 5 or i eq 6) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c2_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C2 SOURCE'
if i eq 1 then title = 'C2 WS'
if i eq 2 then title = 'C2 CC12'
if i eq 5 then title = 'C2 CC23'
if i eq 6 then title = 'C2 CC24'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;#############
;C3
all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.))
if (i eq 0 or i eq 1 or i eq 3 or i eq 5 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c3_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C3 SOURCE'
if i eq 1 then title = 'C3 WS'
if i eq 3 then title = 'C3 CC13'
if i eq 5 then title = 'C3 CC23'
if i eq 7 then title = 'C3 CC34'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;##########
;C4

all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.))
if (i eq 0 or i eq 1 or i eq 4 or i eq 6 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c4_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C4 SOURCE'
if i eq 1 then title = 'C4 WS'
if i eq 4 then title = 'C4 CC14'
if i eq 6 then title = 'C4 CC24'
if i eq 7 then title = 'C4 CC34'

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###############################################################

device,/close
!p.font=-1
set_plot,'x'
endfor

;###########################################################
;##########################################################
;HISTOGRAMS FOR THETA_K FOR THE NUMBER OF POINTS OF ACTUAL SOURCE FOR THE SECOND FREQ
;###########################################################
for i=0,7 do begin

set_plot,'ps'

if i eq 0 then device,filename='theta_k_hist_source_f2_2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_f2_2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_f2_2.ps'
if i eq 3 then device,filename='theta_k_hist_cc13_f2_2.ps'
if i eq 4 then device,filename='theta_k_hist_cc14_f2_2.ps'
if i eq 5 then device,filename='theta_k_hist_cc23_f2_2.ps'
if i eq 6 then device,filename='theta_k_hist_cc24_f2_2.ps'
if i eq 7 then device,filename='theta_k_hist_cc34_f2_2.ps'
!p.font=0
!p.multi=[0,2,2]
;window,xsize=700,ysize=700
;####

if (i eq 0 or i eq 1 or i eq 2 or i eq 3 or i eq 4) then begin
;C1

if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C1 SOURCE'
if i eq 1 then title = 'C1 WS'
if i eq 2 then title = 'C1 CC12'
if i eq 3 then title = 'C1 CC13'
if i eq 4 then title = 'C1 CC14'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###########
;C2

if (i eq 0 or i eq 1 or i eq 2 or i eq 5 or i eq 6) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2 
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C2 SOURCE'
if i eq 1 then title = 'C2 WS'
if i eq 2 then title = 'C2 CC12'
if i eq 5 then title = 'C2 CC23'
if i eq 6 then title = 'C2 CC24'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;#############
;C3
if (i eq 0 or i eq 1 or i eq 3 or i eq 5 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2
if i eq 3 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2
if i eq 5 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C3 SOURCE'
if i eq 1 then title = 'C3 WS'
if i eq 3 then title = 'C3 CC13'
if i eq 5 then title = 'C3 CC23'
if i eq 7 then title = 'C3 CC34'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;##########
;C4

if (i eq 0 or i eq 1 or i eq 4 or i eq 6 or i eq 7) then begin
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2
if i eq 4 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2
if i eq 6 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2
if i eq 7 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.

h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C4 SOURCE'
if i eq 1 then title = 'C4 WS'
if i eq 4 then title = 'C4 CC14'
if i eq 6 then title = 'C4 CC24'
if i eq 7 then title = 'C4 CC34'

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif
;###############################################################

device,/close
!p.font=-1
set_plot,'x'
endfor

endif  ;for entire source












;######################################################################
;######################################################################
;HISTOPLOTS AS A PERCENTAGE OF PATH_RE FOR FREQ 1 FOR ENTIRE SOURCE (ACTUAL + FILLED IN)
;######################################################################

pathre_yes_no = 'no'
if pathre_yes_no eq 'yes' then begin

;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

;set_plot,'x'
for i=0,13 do begin

set_plot,'ps'
!p.font=0
if i eq 0 then device,filename='theta_k_hist_source_pre_f1.ps'
if i eq 1 then device,filename='theta_k_hist_ws_pre_f1.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1pre_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2pre_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1pre_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3pre_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1pre_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4pre_f1.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2pre_f1.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3pre_f1.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2pre_f1.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4pre_f1.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3pre_f1.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4pre_f1.ps'

!p.multi=[0,3,2]
for q=0,4 do begin  ;for the four PATH_RE regions

if q eq 0 then extra = ' 0.4-0.7'
if q eq 1 then extra = ' 0.7-1.0'
if q eq 2 then extra = ' 1.0-1.3'
if q eq 3 then extra = ' 1.3-1.6'
if q eq 4 then extra = ' 1.6-1.9'

;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

if q eq 0 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 0.4) and (pathre_c1_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 0.7) and (pathre_c1_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.0) and (pathre_c1_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.3) and (pathre_c1_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.6) and (pathre_c1_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c1_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

if q eq 0 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 0.4) and (pathre_c2_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 0.7) and (pathre_c2_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.0) and (pathre_c2_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.3) and (pathre_c2_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.6) and (pathre_c2_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c2_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

if q eq 0 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 0.4) and (pathre_c3_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 0.7) and (pathre_c3_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.0) and (pathre_c3_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.3) and (pathre_c3_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.6) and (pathre_c3_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c3_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

if q eq 0 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 0.4) and (pathre_c4_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 0.7) and (pathre_c4_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.0) and (pathre_c4_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.3) and (pathre_c4_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.6) and (pathre_c4_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c4_f1(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
;###########

;######################################################################
;HISTOPLOTS AS A PERCENTAGE OF PATH_RE FOR FREQ 2 FOR ENTIRE SOURCE (ACTUAL + FILLED IN)
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

for i=0,13 do begin

set_plot,'ps'
!p.font=0
if i eq 0 then device,filename='theta_k_hist_source_pre_f2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_pre_f2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1pre_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2pre_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1pre_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3pre_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1pre_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4pre_f2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2pre_f2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3pre_f2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2pre_f2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4pre_f2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3pre_f2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4pre_f2.ps'

!p.multi=[0,3,2]
for q=0,4 do begin  ;for the four PATH_RE regions

if q eq 0 then extra = ' 0.4-0.7'
if q eq 1 then extra = ' 0.7-1.0'
if q eq 2 then extra = ' 1.0-1.3'
if q eq 3 then extra = ' 1.3-1.6'
if q eq 4 then extra = ' 1.6-1.9'

;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

if q eq 0 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 0.4) and (pathre_c1_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 0.7) and (pathre_c1_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.0) and (pathre_c1_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.3) and (pathre_c1_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.6) and (pathre_c1_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c1_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

if q eq 0 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 0.4) and (pathre_c2_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 0.7) and (pathre_c2_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.0) and (pathre_c2_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.3) and (pathre_c2_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.6) and (pathre_c2_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c2_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

if q eq 0 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 0.4) and (pathre_c3_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 0.7) and (pathre_c3_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.0) and (pathre_c3_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.3) and (pathre_c3_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.6) and (pathre_c3_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c3_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

if q eq 0 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 0.4) and (pathre_c4_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 0.7) and (pathre_c4_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.0) and (pathre_c4_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.3) and (pathre_c4_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.6) and (pathre_c4_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
h4 = histogram(theta_k_c4_f2(all),binsize=5,min=0,max=80)/float(n_elements(all))

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
;###########

;######################################################################
;######################################################################
;HISTOPLOTS AS A PERCENTAGE OF PATH_RE FOR FREQ 1 FOR ONLY THE ACTUAL SOURCE
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

for i=0,13 do begin

set_plot,'ps'
!p.font=0
if i eq 0 then device,filename='theta_k_hist_source_pre_f1_2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_pre_f1_2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1pre_f1_2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2pre_f1_2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1pre_f1_2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3pre_f1_2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1pre_f1_2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4pre_f1_2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2pre_f1_2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3pre_f1_2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2pre_f1_2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4pre_f1_2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3pre_f1_2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4pre_f1_2.ps'

!p.multi=[0,3,2]
for q=0,4 do begin  ;for the four PATH_RE regions

if q eq 0 then extra = ' 0.4-0.7'
if q eq 1 then extra = ' 0.7-1.0'
if q eq 2 then extra = ' 1.0-1.3'
if q eq 3 then extra = ' 1.3-1.6'
if q eq 4 then extra = ' 1.6-1.9'

;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

if q eq 0 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 0.4) and (pathre_c1_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 0.7) and (pathre_c1_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.0) and (pathre_c1_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.3) and (pathre_c1_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c1_f1 ne -10000) and (theta_k_c1_f1 ne 0.) and (pathre_c1_f1 ge 1.6) and (pathre_c1_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1*tarr

;if q eq 0 then all = where((theta_k_c1_f1(keepers_source) ne -10000) and (theta_k_c1_f1(keepers_source) ne 0.) and (pathre_c1_f1(keepers_source) ge 0.4) and (pathre_c1_f1(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c1_f1(keepers_source) ne -10000) and (theta_k_c1_f1(keepers_source) ne 0.) and (pathre_c1_f1(keepers_source) ge 0.7) and (pathre_c1_f1(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c1_f1(keepers_source) ne -10000) and (theta_k_c1_f1(keepers_source) ne 0.) and (pathre_c1_f1(keepers_source) ge 1.0) and (pathre_c1_f1(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c1_f1(keepers_source) ne -10000) and (theta_k_c1_f1(keepers_source) ne 0.) and (pathre_c1_f1(keepers_source) ge 1.3) and (pathre_c1_f1(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c1_f1(keepers_source) ne -10000) and (theta_k_c1_f1(keepers_source) ne 0.) and (pathre_c1_f1(keepers_source) ge 1.6) and (pathre_c1_f1 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
;h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)/float(n_elements(all))
h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

if q eq 0 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 0.4) and (pathre_c2_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 0.7) and (pathre_c2_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.0) and (pathre_c2_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.3) and (pathre_c2_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c2_f1 ne -10000) and (theta_k_c2_f1 ne 0.) and (pathre_c2_f1 ge 1.6) and (pathre_c2_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1*tarr


;if q eq 0 then all = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.) and (pathre_c2_f1(keepers_source) ge 0.4) and (pathre_c2_f1(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.) and (pathre_c2_f1(keepers_source) ge 0.7) and (pathre_c2_f1(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.) and (pathre_c2_f1(keepers_source) ge 1.0) and (pathre_c2_f1(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.) and (pathre_c2_f1(keepers_source) ge 1.3) and (pathre_c2_f1(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c2_f1(keepers_source) ne -10000) and (theta_k_c2_f1(keepers_source) ne 0.) and (pathre_c2_f1(keepers_source) ge 1.6) and (pathre_c2_f1 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####


;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

if q eq 0 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 0.4) and (pathre_c3_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 0.7) and (pathre_c3_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.0) and (pathre_c3_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.3) and (pathre_c3_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c3_f1 ne -10000) and (theta_k_c3_f1 ne 0.) and (pathre_c3_f1 ge 1.6) and (pathre_c3_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1*tarr


;if q eq 0 then all = where((theta_k_c3_f1(keepers_source) ne -10000) and (theta_k_c3_f1(keepers_source) ne 0.) and (pathre_c3_f1(keepers_source) ge 0.4) and (pathre_c3_f1(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c3_f1(keepers_source) ne -10000) and (theta_k_c3_f1(keepers_source) ne 0.) and (pathre_c3_f1(keepers_source) ge 0.7) and (pathre_c3_f1(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c3_f1(keepers_source) ne -10000) and (theta_k_c3_f1(keepers_source) ne 0.) and (pathre_c3_f1(keepers_source) ge 1.0) and (pathre_c3_f1(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c3_f1(keepers_source) ne -10000) and (theta_k_c3_f1(keepers_source) ne 0.) and (pathre_c3_f1(keepers_source) ge 1.3) and (pathre_c3_f1(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c3_f1(keepers_source) ne -10000) and (theta_k_c3_f1(keepers_source) ne 0.) and (pathre_c3_f1(keepers_source) ge 1.6) and (pathre_c3_f1 lt 1.9))


tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

if q eq 0 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 0.4) and (pathre_c4_f1 lt 0.7))
if q eq 1 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 0.7) and (pathre_c4_f1 lt 1.0))
if q eq 2 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.0) and (pathre_c4_f1 lt 1.3))
if q eq 3 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.3) and (pathre_c4_f1 lt 1.6))
if q eq 4 then all = where((theta_k_c4_f1 ne -10000) and (theta_k_c4_f1 ne 0.) and (pathre_c4_f1 ge 1.6) and (pathre_c4_f1 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1*tarr


;if q eq 0 then all = where((theta_k_c4_f1(keepers_source) ne -10000) and (theta_k_c4_f1(keepers_source) ne 0.) and (pathre_c4_f1(keepers_source) ge 0.4) and (pathre_c4_f1(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c4_f1(keepers_source) ne -10000) and (theta_k_c4_f1(keepers_source) ne 0.) and (pathre_c4_f1(keepers_source) ge 0.7) and (pathre_c4_f1(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c4_f1(keepers_source) ne -10000) and (theta_k_c4_f1(keepers_source) ne 0.) and (pathre_c4_f1(keepers_source) ge 1.0) and (pathre_c4_f1(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c4_f1(keepers_source) ne -10000) and (theta_k_c4_f1(keepers_source) ne 0.) and (pathre_c4_f1(keepers_source) ge 1.3) and (pathre_c4_f1(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c4_f1(keepers_source) ne -10000) and (theta_k_c4_f1(keepers_source) ne 0.) and (pathre_c4_f1(keepers_source) ge 1.6) and (pathre_c4_f1 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
;###########
;######################################################################
;HISTOPLOTS AS A PERCENTAGE OF PATH_RE FOR FREQ 2 FOR ACTUAL SOURCE
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

;set_plot,'x'


for i=0,13 do begin

set_plot,'ps'
!p.font=0
if i eq 0 then device,filename='theta_k_hist_source_pre_f2_2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_pre_f2_2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1pre_f2_2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2pre_f2_2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1pre_f2_2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3pre_f2_2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1pre_f2_2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4pre_f2_2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2pre_f2_2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3pre_f2_2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2pre_f2_2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4pre_f2_2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3pre_f2_2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4pre_f2_2.ps'

!p.multi=[0,3,2]
for q=0,4 do begin  ;for the four PATH_RE regions

if q eq 0 then extra = ' 0.4-0.7'
if q eq 1 then extra = ' 0.7-1.0'
if q eq 2 then extra = ' 1.0-1.3'
if q eq 3 then extra = ' 1.3-1.6'
if q eq 4 then extra = ' 1.6-1.9'

;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

if q eq 0 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 0.4) and (pathre_c1_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 0.7) and (pathre_c1_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.0) and (pathre_c1_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.3) and (pathre_c1_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c1_f2 ne -10000) and (theta_k_c1_f2 ne 0.) and (pathre_c1_f2 ge 1.6) and (pathre_c1_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2*tarr


;if q eq 0 then all = where((theta_k_c1_f2(keepers_source) ne -10000) and (theta_k_c1_f2(keepers_source) ne 0.) and (pathre_c1_f2(keepers_source) ge 0.4) and (pathre_c1_f2(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c1_f2(keepers_source) ne -10000) and (theta_k_c1_f2(keepers_source) ne 0.) and (pathre_c1_f2(keepers_source) ge 0.7) and (pathre_c1_f2(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c1_f2(keepers_source) ne -10000) and (theta_k_c1_f2(keepers_source) ne 0.) and (pathre_c1_f2(keepers_source) ge 1.0) and (pathre_c1_f2(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c1_f2(keepers_source) ne -10000) and (theta_k_c1_f2(keepers_source) ne 0.) and (pathre_c1_f2(keepers_source) ge 1.3) and (pathre_c1_f2(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c1_f2(keepers_source) ne -10000) and (theta_k_c1_f2(keepers_source) ne 0.) and (pathre_c1_f2(keepers_source) ge 1.6) and (pathre_c1_f2 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####



;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

if q eq 0 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 0.4) and (pathre_c2_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 0.7) and (pathre_c2_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.0) and (pathre_c2_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.3) and (pathre_c2_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c2_f2 ne -10000) and (theta_k_c2_f2 ne 0.) and (pathre_c2_f2 ge 1.6) and (pathre_c2_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2*tarr


;if q eq 0 then all = where((theta_k_c2_f2(keepers_source) ne -10000) and (theta_k_c2_f2(keepers_source) ne 0.) and (pathre_c2_f2(keepers_source) ge 0.4) and (pathre_c2_f2(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c2_f2(keepers_source) ne -10000) and (theta_k_c2_f2(keepers_source) ne 0.) and (pathre_c2_f2(keepers_source) ge 0.7) and (pathre_c2_f2(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c2_f2(keepers_source) ne -10000) and (theta_k_c2_f2(keepers_source) ne 0.) and (pathre_c2_f2(keepers_source) ge 1.0) and (pathre_c2_f2(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c2_f2(keepers_source) ne -10000) and (theta_k_c2_f2(keepers_source) ne 0.) and (pathre_c2_f2(keepers_source) ge 1.3) and (pathre_c2_f2(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c2_f2(keepers_source) ne -10000) and (theta_k_c2_f2(keepers_source) ne 0.) and (pathre_c2_f2(keepers_source) ge 1.6) and (pathre_c2_f2 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

if q eq 0 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 0.4) and (pathre_c3_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 0.7) and (pathre_c3_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.0) and (pathre_c3_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.3) and (pathre_c3_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c3_f2 ne -10000) and (theta_k_c3_f2 ne 0.) and (pathre_c3_f2 ge 1.6) and (pathre_c3_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2*tarr


;if q eq 0 then all = where((theta_k_c3_f2(keepers_source) ne -10000) and (theta_k_c3_f2(keepers_source) ne 0.) and (pathre_c3_f2(keepers_source) ge 0.4) and (pathre_c3_f2(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c3_f2(keepers_source) ne -10000) and (theta_k_c3_f2(keepers_source) ne 0.) and (pathre_c3_f2(keepers_source) ge 0.7) and (pathre_c3_f2(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c3_f2(keepers_source) ne -10000) and (theta_k_c3_f2(keepers_source) ne 0.) and (pathre_c3_f2(keepers_source) ge 1.0) and (pathre_c3_f2(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c3_f2(keepers_source) ne -10000) and (theta_k_c3_f2(keepers_source) ne 0.) and (pathre_c3_f2(keepers_source) ge 1.3) and (pathre_c3_f2(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c3_f2(keepers_source) ne -10000) and (theta_k_c3_f2(keepers_source) ne 0.) and (pathre_c3_f2(keepers_source) ge 1.6) and (pathre_c3_f2 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

if q eq 0 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 0.4) and (pathre_c4_f2 lt 0.7))
if q eq 1 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 0.7) and (pathre_c4_f2 lt 1.0))
if q eq 2 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.0) and (pathre_c4_f2 lt 1.3))
if q eq 3 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.3) and (pathre_c4_f2 lt 1.6))
if q eq 4 then all = where((theta_k_c4_f2 ne -10000) and (theta_k_c4_f2 ne 0.) and (pathre_c4_f2 ge 1.6) and (pathre_c4_f2 lt 1.9))

tarr = fltarr(401,401)
tarr(all) = 1.
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2*tarr

;if q eq 0 then all = where((theta_k_c4_f2(keepers_source) ne -10000) and (theta_k_c4_f2(keepers_source) ne 0.) and (pathre_c4_f2(keepers_source) ge 0.4) and (pathre_c4_f2(keepers_source) lt 0.7))
;if q eq 1 then all = where((theta_k_c4_f2(keepers_source) ne -10000) and (theta_k_c4_f2(keepers_source) ne 0.) and (pathre_c4_f2(keepers_source) ge 0.7) and (pathre_c4_f2(keepers_source) lt 1.0))
;if q eq 2 then all = where((theta_k_c4_f2(keepers_source) ne -10000) and (theta_k_c4_f2(keepers_source) ne 0.) and (pathre_c4_f2(keepers_source) ge 1.0) and (pathre_c4_f2(keepers_source) lt 1.3))
;if q eq 3 then all = where((theta_k_c4_f2(keepers_source) ne -10000) and (theta_k_c4_f2(keepers_source) ne 0.) and (pathre_c4_f2(keepers_source) ge 1.3) and (pathre_c4_f2(keepers_source) lt 1.6))
;if q eq 4 then all = where((theta_k_c4_f2(keepers_source) ne -10000) and (theta_k_c4_f2(keepers_source) ne 0.) and (pathre_c4_f2(keepers_source) ge 1.6) and (pathre_c4_f2 lt 1.9))

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
;###########

endif  ;for pathre_yes_no
















;######################################################################
;######################################################################
;HISTOPLOTS IN SECTIONS OF ZGRID FOR FREQ 1 FOR ENTIRE SOURCE
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]
kk = 'yes'
if kk eq 'yes' then begin
for i=0,13 do begin

set_plot,'ps'
!p.font=0
;if i eq 0 then device,filename='theta_k_hist_source_zg_f1.ps'
;if i eq 1 then device,filename='theta_k_hist_ws_zg_f1.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_f1.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_f1.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_f1.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_f1.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_f1.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_f1.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_f1.ps'

!p.multi=[0,3,4]
for q=0,11 do begin  ;for the five z-grid regions

if q eq 0 or q eq 6 then extra = ' ZG 1'
if q eq 1 or q eq 7 then extra = ' ZG 2'
if q eq 2 or q eq 8 then extra = ' ZG 3'
if q eq 3 or q eq 9 then extra = ' ZG 4'
if q eq 4 or q eq 10 then extra = ' ZG 5'
if q eq 5 or q eq 11 then extra = ' ZG 6'

if q eq 0 or q eq 6 then goodz = where((zgrid ge 0.2) and (zgrid lt 1.0))
if q eq 1 or q eq 7 then goodz = where((zgrid ge 0.0) and (zgrid lt 0.2))
if q eq 2 or q eq 8 then goodz = where((zgrid ge -0.2) and (zgrid lt 0.0))
if q eq 3 or q eq 9 then goodz = where((zgrid ge -0.4) and (zgrid lt -0.2))
if q eq 4 or q eq 10 then goodz = where((zgrid ge -0.6) and (zgrid lt -0.4))
if q eq 5 or q eq 11 then goodz = where((zgrid ge -1.0) and (zgrid lt -0.6))
if goodz(0) ne -1 then begin
;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c1_f1(*,goodz(b)) ne -10000) and (theta_k_c1_f1(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c1_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c2_f1(*,goodz(b)) ne -10000) and (theta_k_c2_f1(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c2_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c3_f1(*,goodz(b)) ne -10000) and (theta_k_c3_f1(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c3_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c4_f1(*,goodz(b)) ne -10000) and (theta_k_c4_f1(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c4_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle
endif ;tmp(0) ne -1
endif ;for the correct i
;####
endif
endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
;###########


;######################################################################
;######################################################################
;HISTOPLOTS IN SECTIONS OF ZGRID FOR FREQ 2 FOR ENTIRE SOURCE
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

;set_plot,'x'

for i=0,13 do begin

set_plot,'ps'
!p.font=0
if i eq 0 then device,filename='theta_k_hist_source_zg_f2.ps'
if i eq 1 then device,filename='theta_k_hist_ws_zg_f2.ps'
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_f2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_f2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_f2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_f2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_f2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_f2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_f2.ps'

!p.multi=[0,3,4]
for q=0,11 do begin  ;for the four PATH_RE regions

if q eq 0 or q eq 6 then extra = ' ZG 1'
if q eq 1 or q eq 7 then extra = ' ZG 2'
if q eq 2 or q eq 8 then extra = ' ZG 3'
if q eq 3 or q eq 9 then extra = ' ZG 4'
if q eq 4 or q eq 10 then extra = ' ZG 5'
if q eq 5 or q eq 11 then extra = ' ZG 6'

if q eq 0 or q eq 6 then goodz = where((zgrid ge 0.5) and (zgrid lt 1.0))
if q eq 1 or q eq 7 then goodz = where((zgrid ge 0.25) and (zgrid lt 0.5))
if q eq 2 or q eq 8 then goodz = where((zgrid ge 0.0) and (zgrid lt 0.25))
if q eq 3 or q eq 9 then goodz = where((zgrid ge -0.25) and (zgrid lt 0.0))
if q eq 4 or q eq 10 then goodz = where((zgrid ge -0.5) and (zgrid lt -0.25))
if q eq 5 or q eq 11 then goodz = where((zgrid ge -1.0) and (zgrid lt -0.5))
if goodz(0) ne -1 then begin
;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c1_f2(*,goodz(b)) ne -10000) and (theta_k_c1_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c1_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c2_f2(*,goodz(b)) ne -10000) and (theta_k_c2_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c2_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c3_f2(*,goodz(b)) ne -10000) and (theta_k_c3_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c3_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c4_f2(*,goodz(b)) ne -10000) and (theta_k_c4_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c4_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
endif  ;goodz(0) ne -1
endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i

endif ;for zgrid
;###########













;######################################################################
;######################################################################
;HISTOPLOTS IN SECTIONS OF ZGRID + XGRID FOR FREQ 1 FOR ENTIRE SOURCE 
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

yy = 'no'
if yy eq 'yes' then begin
for b=0,2 do begin ; for the xgrid direction
for i=0,13 do begin

set_plot,'ps'
!p.font=0
if b eq 0 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x1_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x1_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x1_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x1_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x1_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x1_f1.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x1_f1.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x1_f1.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x1_f1.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x1_f1.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x1_f1.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x1_f1.ps'
endif
if b eq 1 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x2_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x2_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x2_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x2_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x2_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x2_f1.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x2_f1.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x2_f1.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x2_f1.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x2_f1.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x2_f1.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x2_f1.ps'
endif
if b eq 2 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x3_f1.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x3_f1.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x3_f1.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x3_f1.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x3_f1.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x3_f1.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x3_f1.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x3_f1.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x3_f1.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x3_f1.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x3_f1.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x3_f1.ps'
endif

if b eq 0 then begin
   xgmin = 4.4
   xgmax = 4.6
endif
if b eq 1 then begin
   xgmin = 4.6
   xgmax = 4.8
endif
if b eq 2 then begin
   xgmin = 4.8
   xgmax = 5.0
endif

!p.multi=[0,3,4]
for q=0,11 do begin  ;for the five z-grid regions

if q eq 0 or q eq 6 then extra = ' ZG 1'
if q eq 1 or q eq 7 then extra = ' ZG 2'
if q eq 2 or q eq 8 then extra = ' ZG 3'
if q eq 3 or q eq 9 then extra = ' ZG 4'
if q eq 4 or q eq 10 then extra = ' ZG 5'
if q eq 5 or q eq 11 then extra = ' ZG 6'

if q eq 0 or q eq 6 then goodz = where((zgrid ge 0.5) and (zgrid lt 1.0))
if q eq 1 or q eq 7 then goodz = where((zgrid ge 0.25) and (zgrid lt 0.5))
if q eq 2 or q eq 8 then goodz = where((zgrid ge 0.0) and (zgrid lt 0.25))
if q eq 3 or q eq 9 then goodz = where((zgrid ge -0.25) and (zgrid lt 0.0))
if q eq 4 or q eq 10 then goodz = where((zgrid ge -0.5) and (zgrid lt -0.25))
if q eq 5 or q eq 11 then goodz = where((zgrid ge -1.0) and (zgrid lt -0.5))

goodx = where((xgrid ge xgmin) and (xgrid lt xgmax))
;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin   
       all_tmp = where((theta_k_c1_f1(*,goodz(b)) ne -10000) and (theta_k_c1_f1(*,goodz(b)) ne 0.))
     
       min_sub = goodx(0) > all_tmp(0)
       max_sub = goodx(n_elements(goodx)-1) < all_tmp(n_elements(all_tmp)-1)
       t0 = where(all_tmp lt min_sub)
       t1 = where(all_tmp gt max_sub)
       if t0(0) ne -1 then all_tmp(t0) = -1
       if t1(0) ne -1 then all_tmp(t1) = -1
       t = where(all_tmp eq -1)
       if t(0) ne -1 then all_tmp2 = all_tmp(t)
       if t(0) ne -1 then all_tmp = all_tmp2

       if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))= 1
       if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f1*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f1*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f1(tmp))
min1 = min(tgn_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c1_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f1(tmp))
min1 = min(trs_c1_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c1_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c2_f1(*,goodz(b)) ne -10000) and (theta_k_c2_f1(*,goodz(b)) ne 0.))

    
       min_sub = goodx(0) > all_tmp(0)
       max_sub = goodx(n_elements(goodx)-1) < all_tmp(n_elements(all_tmp)-1)
       t0 = where(all_tmp lt min_sub)
       t1 = where(all_tmp gt max_sub)
       if t0(0) ne -1 then all_tmp(t0) = -1
       if t1(0) ne -1 then all_tmp(t1) = -1
       t = where(all_tmp eq -1)
       if t(0) ne -1 then all_tmp2 = all_tmp(t)
       if t(0) ne -1 then all_tmp = all_tmp2

       if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))= 1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f1*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f1*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f1(tmp))
min1 = min(tgn_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c2_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f1(tmp))
min1 = min(trs_c2_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c2_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c3_f1(*,goodz(b)) ne -10000) and (theta_k_c3_f1(*,goodz(b)) ne 0.))
      
       min_sub = goodx(0) > all_tmp(0)
       max_sub = goodx(n_elements(goodx)-1) < all_tmp(n_elements(all_tmp)-1)
       t0 = where(all_tmp lt min_sub)
       t1 = where(all_tmp gt max_sub)
       if t0(0) ne -1 then all_tmp(t0) = -1
       if t1(0) ne -1 then all_tmp(t1) = -1
       t = where(all_tmp eq -1)
       if t(0) ne -1 then all_tmp2 = all_tmp(t)
       if t(0) ne -1 then all_tmp = all_tmp2

       if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))= 1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f1*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f1*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f1(tmp))
min1 = min(tgn_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c3_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f1(tmp))
min1 = min(trs_c3_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c3_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c4_f1(*,goodz(b)) ne -10000) and (theta_k_c4_f1(*,goodz(b)) ne 0.))
      
       min_sub = goodx(0) > all_tmp(0)
       max_sub = goodx(n_elements(goodx)-1) < all_tmp(n_elements(all_tmp)-1)
       t0 = where(all_tmp lt min_sub)
       t1 = where(all_tmp gt max_sub)
       if t0(0) ne -1 then all_tmp(t0) = -1
       if t1(0) ne -1 then all_tmp(t1) = -1
       t = where(all_tmp eq -1)
       if t(0) ne -1 then all_tmp2 = all_tmp(t)
       if t(0) ne -1 then all_tmp = all_tmp2

       if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))= 1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f1*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f1*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f1*tarr

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))

if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f1(tmp))
min1 = min(tgn_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c4_f1(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f1(tmp))
min1 = min(trs_c4_f1(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c4_f1(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle
endif ;tmp(0) ne -1
endif ;for the correct i
;####
endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
endfor   ;b
;###########


;######################################################################
;######################################################################
;HISTOPLOTS IN SECTIONS OF ZGRID + XGRID FOR FREQ 2 FOR ENTIRE SOURCE
;######################################################################
;here are the divisions
;[0.4-0.7]    [0.7-1.0]    [1.0-1.3]    [1.3-1.6]    [1.6-1.9]

;set_plot,'x'
for b=0,2 do begin
for i=0,13 do begin

set_plot,'ps'
!p.font=0

if b eq 0 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x1_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x1_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x1_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x1_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x1_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x1_f2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x1_f2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x1_f2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x1_f2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x1_f2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x1_f2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x1_f2.ps'
endif
if b eq 1 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x2_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x2_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x2_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x2_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x2_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x2_f2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x2_f2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x2_f2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x2_f2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x2_f2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x2_f2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x2_f2.ps'
endif
if b eq 2 then begin
if i eq 2 then device,filename='theta_k_hist_cc12_c1zg_x3_f2.ps'
if i eq 3 then device,filename='theta_k_hist_cc12_c2zg_x3_f2.ps'
if i eq 4 then device,filename='theta_k_hist_cc13_c1zg_x3_f2.ps'
if i eq 5 then device,filename='theta_k_hist_cc13_c3zg_x3_f2.ps'
if i eq 6 then device,filename='theta_k_hist_cc14_c1zg_x3_f2.ps'
if i eq 7 then device,filename='theta_k_hist_cc14_c4zg_x3_f2.ps'
if i eq 8 then device,filename='theta_k_hist_cc23_c2zg_x3_f2.ps'
if i eq 9 then device,filename='theta_k_hist_cc23_c3zg_x3_f2.ps'
if i eq 10 then device,filename='theta_k_hist_cc24_c2zg_x3_f2.ps'
if i eq 11 then device,filename='theta_k_hist_cc24_c4zg_x3_f2.ps'
if i eq 12 then device,filename='theta_k_hist_cc34_c3zg_x3_f2.ps'
if i eq 13 then device,filename='theta_k_hist_cc34_c4zg_x3_f2.ps'
endif

if b eq 0 then begin
   xgmin = 4.4
   xgmax = 4.6
endif
if b eq 1 then begin
   xgmin = 4.6
   xgmax = 4.8
endif
if b eq 2 then begin
   xgmin = 4.8
   xgmax = 5.0
endif


!p.multi=[0,3,4]
for q=0,11 do begin  ;for the four PATH_RE regions

if q eq 0 or q eq 6 then extra = ' ZG 1'
if q eq 1 or q eq 7 then extra = ' ZG 2'
if q eq 2 or q eq 8 then extra = ' ZG 3'
if q eq 3 or q eq 9 then extra = ' ZG 4'
if q eq 4 or q eq 10 then extra = ' ZG 5'
if q eq 5 or q eq 11 then extra = ' ZG 6'

if q eq 0 or q eq 6 then goodz = where(((zgrid ge 0.5) and (zgrid lt 1.0)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))
if q eq 1 or q eq 7 then goodz = where(((zgrid ge 0.25) and (zgrid lt 0.5)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))
if q eq 2 or q eq 8 then goodz = where(((zgrid ge 0.0) and (zgrid lt 0.25)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))
if q eq 3 or q eq 9 then goodz = where(((zgrid ge -0.25) and (zgrid lt 0.0)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))
if q eq 4 or q eq 10 then goodz = where(((zgrid ge -0.5) and (zgrid lt -0.25)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))
if q eq 5 or q eq 11 then goodz = where(((zgrid ge -1.0) and (zgrid lt -0.5)) and ((xgrid ge xgmin) and (xgrid lt xgmax)))


;####
if (i eq 0 or i eq 1 or i eq 2 or i eq 4 or i eq 6) then begin
;C1

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c1_f2(*,goodz(b)) ne -10000) and (theta_k_c1_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2*tarr
if i eq 2 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c1_f2*tarr
if i eq 4 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c1_f2*tarr
if i eq 6 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c1_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c1_f2(tmp))
min1 = min(tgn_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c1_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c1_f2(tmp))
min1 = min(trs_c1_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c1_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C1 SOURCE' + extra
if i eq 1 then title = 'C1 WS' + extra
if i eq 2 then title = 'C1 CC12' + extra
if i eq 4 then title = 'C1 CC13' + extra
if i eq 6 then title = 'C1 CC14' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 3 or i eq 8 or i eq 10) then begin
;C2

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c2_f2(*,goodz(b)) ne -10000) and (theta_k_c2_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2*tarr
if i eq 3 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tgn_c2_f2*tarr
if i eq 8 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c2_f2*tarr
if i eq 10 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c2_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c2_f2(tmp))
min1 = min(tgn_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c2_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c2_f2(tmp))
min1 = min(trs_c2_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c2_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C2 SOURCE' + extra
if i eq 1 then title = 'C2 WS' + extra
if i eq 3 then title = 'C2 CC12' + extra
if i eq 8 then title = 'C2 CC23' + extra
if i eq 10 then title = 'C2 CC24' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 5 or i eq 9 or i eq 12) then begin
;C3

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c3_f2(*,goodz(b)) ne -10000) and (theta_k_c3_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2*tarr
if i eq 5 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tgn_c3_f2*tarr
if i eq 9 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tgn_c3_f2*tarr
if i eq 12 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c3_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c3_f2(tmp))
min1 = min(tgn_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c3_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c3_f2(tmp))
min1 = min(trs_c3_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c3_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C3 SOURCE' + extra
if i eq 1 then title = 'C3 WS' + extra
if i eq 5 then title = 'C3 CC13' + extra
if i eq 9 then title = 'C3 CC23' + extra
if i eq 12 then title = 'C3 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####
;####
if (i eq 0 or i eq 1 or i eq 7 or i eq 11 or i eq 13) then begin
;C4

tarr = fltarr(401,401)
count_all=0
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((theta_k_c4_f2(*,goodz(b)) ne -10000) and (theta_k_c4_f2(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
   if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
endfor
all = where(tarr ne 0.)
if i eq 0 then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2*tarr
if i eq 1 then keepers_source = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2*tarr
if i eq 7 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tgn_c4_f2*tarr
if i eq 11 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tgn_c4_f2*tarr
if i eq 13 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tgn_c4_f2*tarr

tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.

tmp = where((keepers_source ne -10000) and (keepers_source ne 0.))
if tmp(0) ne -1 then begin
max1 = max(tgn_c4_f2(tmp))
min1 = min(tgn_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
if q le 5 then h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)/float(count_all)
if q gt 5 then h1 = histogram(theta_k_c4_f2(tmp),binsize=5,min=0,max=80)
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
max1 = max(trs_c4_f2(tmp))
min1 = min(trs_c4_f2(tmp))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if q le 5 then h4 = histogram(theta_k_c4_f2(all),binsize=5,min=0,max=80)/float(count_all)

if i eq 0 then title = 'C4 SOURCE' + extra
if i eq 1 then title = 'C4 WS' + extra
if i eq 7 then title = 'C4 CC14' + extra
if i eq 11 then title = 'C4 CC24' + extra
if i eq 13 then title = 'C4 CC34' + extra

if q le 5 then bar_graph,xvals,h4,barcolor=25,barborder=255,title=title        ;background
if q le 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,overplot='yes'   ;white foreground
if q gt 5 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title   ;white foreground
bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'    ;theta_res
bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'     ;gendrin angle

endif ;tmp(0) ne -1
endif ;for the correct i
;####

endfor  ;q

device,/close
!p.font=-1
set_plot,'x'
endfor   ;i
endfor   ;b
endif ;yy
;###########


;###################
;COLORBAR
;####################
steps = ceil(256/3.)
scalefactor=findgen(steps)/(steps-1)

green1 = 70 + (255-70)*scalefactor 
blue1 = 255 + (0-255)*scalefactor
red1 = 21 + (0-21)*scalefactor

green1(0) = 255
blue1(0) = 255
red1(0) = 255

blue2 = replicate(0,steps)
green2 = 255 + (115-255)*scalefactor
red2 = 0 + (255-0)*scalefactor

green3 = 115 + (0-115)*scalefactor
blue3 = replicate(0,steps)
red3 = 255 + (120-255)*scalefactor

green3(85) = 0
blue3(85) = 0
red3(85) = 0

greenvector=[green1,green2,green3]
redvector=[red1,red2,red3]
bluevector=[blue1,blue2,blue3]

;tmppp = where((tgn_c1_f1 ne -10000.) and (tgn_c1_f1 gt 0.3))
;if tmppp(0) ne -1 then begin
;min_gen = min(tgn_c1_f1(tmppp))
;max_gen = max(tgn_c1_f1(tmppp))

;green_min = 256*min_gen/(70.)
;green_max = 256*max_gen/(70.)

;greenvector(green_min:green_max) = 0
;redvector(green_min:green_max) = 0
;bluevector(green_min:green_max) = 0
;endif
tvlct,redvector,greenvector,bluevector
;##########################
;##########################################

max_thetak = 40.
min_thetak = 0.
max_thetag = 15.
min_thetag = -25.
max_gen = 30.
min_gen = 0.
max_res = 65.
min_res = 40.
tgn_pm_delta = 2.  ;angles within +/- this value are highlighted as the gendrin angle on theta-k plots.

;here let's plot all the wave values at the source to a file. 
           
            divisions = 10. 
	    lvl_divisions = 257/5.
            increment = (max_thetak-min_thetak)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment - min_thetak                         
            lvls(0) = -10000.      
            tmplvls = findgen(divisions+1)*((max_thetak-min_thetak)/divisions) + min_thetak
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)
;#############

for i=0,7 do begin
            set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'theta_k_c1_f1.ps',bits=8,/color
	    if i eq 1 then device,filename = 'theta_k_c2_f1.ps',bits=8,/color
	    if i eq 2 then device,filename = 'theta_k_c3_f1.ps',bits=8,/color
	    if i eq 3 then device,filename = 'theta_k_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'theta_k_c1_ws_f1.ps',bits=8,/color
	    if i eq 5 then device,filename = 'theta_k_c2_ws_f1.ps',bits=8,/color
	    if i eq 6 then device,filename = 'theta_k_c3_ws_f1.ps',bits=8,/color
	    if i eq 7 then device,filename = 'theta_k_c4_ws_f1.ps',bits=8,/color
	
keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c1_f1(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c1_f1(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c2_f1(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c2_f1(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c3_f1(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c3_f1(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c4_f1(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c4_f1(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c1_f1(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c1_f1(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c2_f1(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c2_f1(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c3_f1(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c3_f1(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c4_f1(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c4_f1(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            if !d.name eq 'ps' then device,/close
            set_plot,'x'
endfor
;#############          
;#######################################

;theta_group     
            divisions=10.       
	    lvl_divisions = 257/5.
            increment = (max_thetag-min_thetag)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment + min_thetag          
            lvls(0) = -10000.          
            tmplvls = findgen(divisions+1)*((max_thetag-min_thetag)/divisions) + min_thetag
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)

for i=0,7 do begin

;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        



;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.          
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = 0.
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = 0.

;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'theta_g_c1_f1.ps',bits=8,/color
            if i eq 1 then device,filename = 'theta_g_c2_f1.ps',bits=8,/color
            if i eq 2 then device,filename = 'theta_g_c3_f1.ps',bits=8,/color
            if i eq 3 then device,filename = 'theta_g_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'theta_g_c1_ws_f1.ps',bits=8,/color
            if i eq 5 then device,filename = 'theta_g_c2_ws_f1.ps',bits=8,/color
            if i eq 6 then device,filename = 'theta_g_c3_ws_f1.ps',bits=8,/color
            if i eq 7 then device,filename = 'theta_g_c4_ws_f1.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;##################
;#############
;path_re
for i=0,7 do begin
          lvl_divisions = 257/5.
          increment = (3)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(3/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        
          

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = 0.
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = 0.

;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'pathre_c1_f1.ps',bits=8,/color            
            if i eq 1 then device,filename = 'pathre_c2_f1.ps',bits=8,/color            
            if i eq 2 then device,filename = 'pathre_c3_f1.ps',bits=8,/color            
            if i eq 3 then device,filename = 'pathre_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'pathre_c1_ws_f1.ps',bits=8,/color            
            if i eq 5 then device,filename = 'pathre_c2_ws_f1.ps',bits=8,/color            
            if i eq 6 then device,filename = 'pathre_c3_ws_f1.ps',bits=8,/color            
            if i eq 7 then device,filename = 'pathre_c4_ws_f1.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;#############
;#############
;ffce
          lvl_divisions = 257/5.
          increment = (1)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(1/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
;##################
for i=0,7 do begin
;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        
         
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'ffce_c1_f1.ps',bits=8,/color
            if i eq 1 then device,filename = 'ffce_c2_f1.ps',bits=8,/color
            if i eq 2 then device,filename = 'ffce_c3_f1.ps',bits=8,/color
            if i eq 3 then device,filename = 'ffce_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'ffce_c1_ws_f1.ps',bits=8,/color
            if i eq 5 then device,filename = 'ffce_c2_ws_f1.ps',bits=8,/color
            if i eq 6 then device,filename = 'ffce_c3_ws_f1.ps',bits=8,/color
            if i eq 7 then device,filename = 'ffce_c4_ws_f1.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257


keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;###############
;##################
;refractive index
          lvl_divisions = 257/5.
          increment = (50)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(50/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

for i=0,7 do begin
;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.           
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'rdx_c1_f1.ps',bits=8,/color
            if i eq 1 then device,filename = 'rdx_c2_f1.ps',bits=8,/color
            if i eq 2 then device,filename = 'rdx_c3_f1.ps',bits=8,/color
            if i eq 3 then device,filename = 'rdx_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'rdx_c1_ws_f1.ps',bits=8,/color
            if i eq 5 then device,filename = 'rdx_c2_ws_f1.ps',bits=8,/color
            if i eq 6 then device,filename = 'rdx_c3_ws_f1.ps',bits=8,/color
            if i eq 7 then device,filename = 'rdx_c4_ws_f1.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257


keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;#############
;###################
;gendrin angle          
          lvl_divisions = 257/5.
          increment = (max_gen-min_gen)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_gen
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_gen-min_gen)/divisions) + min_gen
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

   for i=0,7 do begin
;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        
           
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257;
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'tgn_c1_f1.ps',bits=8,/color
            if i eq 1 then device,filename = 'tgn_c2_f1.ps',bits=8,/color
            if i eq 2 then device,filename = 'tgn_c3_f1.ps',bits=8,/color
            if i eq 3 then device,filename = 'tgn_c4_f1.ps',bits=8,/color
            if i eq 4 then device,filename = 'tgn_c1_ws_f1.ps',bits=8,/color
            if i eq 5 then device,filename = 'tgn_c2_ws_f1.ps',bits=8,/color
            if i eq 6 then device,filename = 'tgn_c3_ws_f1.ps',bits=8,/color
            if i eq 7 then device,filename = 'tgn_c4_ws_f1.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device


keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;###############
;################
;resonance cone angle
          divisions = 10.
          lvl_divisions = 257/5.
          increment = (max_res-min_res)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_res          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_res-min_res)/divisions) + min_res
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
for i=0,7 do begin
; thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c1_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.            
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257;
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c2_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c3_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c4_f1
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257;
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c1_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c2_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c3_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c4_f1
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'trs_c1_f1.ps',bits=8,/color
            if i eq 1 then device,filename = 'trs_c2_f1.ps',bits=8,/color
            if i eq 2 then device,filename = 'trs_c3_f1.ps',bits=8,/color
            if i eq 3 then device,filename = 'trs_c4_f1.ps',bits=8,/color   
            if i eq 4 then device,filename = 'trs_c1_ws_f1.ps',bits=8,/color
            if i eq 5 then device,filename = 'trs_c2_ws_f1.ps',bits=8,/color
            if i eq 6 then device,filename = 'trs_c3_ws_f1.ps',bits=8,/color
            if i eq 7 then device,filename = 'trs_c4_ws_f1.ps',bits=8,/color            

;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device


keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c1_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c2_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c3_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c4_f1
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c1_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c2_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c3_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c4_f1
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;###############

keepers_source = 0.
keepers_ws = 0.












;##########################
;##########################################		 

;here let's plot all the wave values at the source to a file. 

            divisions = 10. 
	    lvl_divisions = 257/5.
            increment = (max_thetak-min_thetak)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment + min_thetak
            lvls(0) = -10000.      
            tmplvls = findgen(divisions+1)*((max_thetak-min_thetak)/divisions) + min_thetak
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)
;#############

for i=0,7 do begin
;            window,xsize=700,ysize=700
;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        

;if i eq 0 then contour,theta_k_c1_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 1 then contour,theta_k_c2_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 2 then contour,theta_k_c3_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 3 then contour,theta_k_c4_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257

;if i eq 4 then contour,theta_k_c1_ws_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 5 then contour,theta_k_c2_ws_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 6 then contour,theta_k_c3_ws_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;if i eq 7 then contour,theta_k_c4_ws_f2(*,*),xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257

            set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'theta_k_c1_f2.ps',bits=8,/color
	    if i eq 1 then device,filename = 'theta_k_c2_f2.ps',bits=8,/color
	    if i eq 2 then device,filename = 'theta_k_c3_f2.ps',bits=8,/color
	    if i eq 3 then device,filename = 'theta_k_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'theta_k_c1_ws_f2.ps',bits=8,/color
	    if i eq 5 then device,filename = 'theta_k_c2_ws_f2.ps',bits=8,/color
	    if i eq 6 then device,filename = 'theta_k_c3_ws_f2.ps',bits=8,/color
	    if i eq 7 then device,filename = 'theta_k_c4_ws_f2.ps',bits=8,/color
	


keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c1_f2(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c1_f2(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c2_f2(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c2_f2(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c3_f2(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c3_f2(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_k_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
where_gen = where((keepers_source(*,*) ge (tgn_c4_f2(*,*) - tgn_pm_delta)) and (keepers_source(*,*) le (tgn_c4_f2(*,*) + tgn_pm_delta)) and (keepers_source(*,*) ne 0.) and (keepers_source(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_source(where_gen) = max_thetak
if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c1_f2(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c1_f2(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c2_f2(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c2_f2(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c3_f2(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c3_f2(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_k_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
where_gen = where((keepers_ws(*,*) ge (tgn_c4_f2(*,*) - tgn_pm_delta)) and (keepers_ws(*,*) le (tgn_c4_f2(*,*) + tgn_pm_delta)) and (keepers_ws(*,*) ne 0.) and (keepers_ws(*,*) ne -10000.))
if where_gen(0) ne -1 then keepers_ws(where_gen) = max_thetak
if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.


;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

;	    set_plot,'ps'
;            !p.font = 0
;            if i eq 0 then device,filename = 'theta_k_c1_f2.ps',bits=8,/color
;            if i eq 1 then device,filename = 'theta_k_c2_f2.ps',bits=8,/color
;            if i eq 2 then device,filename = 'theta_k_c3_f2.ps',bits=8,/color
;            if i eq 3 then device,filename = 'theta_k_c4_f2.ps',bits=8,/color
;            if i eq 4 then device,filename = 'theta_k_c1_ws_f2.ps',bits=8,/color
;            if i eq 5 then device,filename = 'theta_k_c2_ws_f2.ps',bits=8,/color
;            if i eq 6 then device,filename = 'theta_k_c3_ws_f2.ps',bits=8,/color
;            if i eq 7 then device,filename = 'theta_k_c4_ws_f2.ps',bits=8,/color
  
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

;            if i eq 0 then contour,theta_k_c1_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 1 then contour,theta_k_c2_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 2 then contour,theta_k_c3_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 3 then contour,theta_k_c4_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 4 then contour,theta_k_c1_ws_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 5 then contour,theta_k_c2_ws_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 6 then contour,theta_k_c3_ws_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1
;            if i eq 7 then contour,theta_k_c4_ws_f2(*,*),xgrid,zgrid,/nodata,position=[0.1,0.1,0.9,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            if !d.name eq 'ps' then device,/close
            set_plot,'x'
endfor
;#############          
;#######################################
;#################################################
;theta_group
for i=0,7 do begin
	    lvl_divisions = 257/5.
            increment = (max_thetag-min_thetag)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment + min_thetag
            lvls(0) = -10000.          
            tmplvls = findgen(divisions+1)*((max_thetag-min_thetag)/divisions) + min_thetag
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)

;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        


;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.          
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = 0.
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = 0.
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'theta_g_c1_f2.ps',bits=8,/color
            if i eq 1 then device,filename = 'theta_g_c2_f2.ps',bits=8,/color
            if i eq 2 then device,filename = 'theta_g_c3_f2.ps',bits=8,/color
            if i eq 3 then device,filename = 'theta_g_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'theta_g_c1_ws_f2.ps',bits=8,/color
            if i eq 5 then device,filename = 'theta_g_c2_ws_f2.ps',bits=8,/color
            if i eq 6 then device,filename = 'theta_g_c3_ws_f2.ps',bits=8,/color
            if i eq 7 then device,filename = 'theta_g_c4_ws_f2.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*theta_g_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*theta_g_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;##################
;#############
;path_re
for i=0,7 do begin
 lvl_divisions = 257/5.
          increment = (3)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(3/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

; thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        
          

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = 0.
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = 0.

;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'pathre_c1_f2.ps',bits=8,/color            
            if i eq 1 then device,filename = 'pathre_c2_f2.ps',bits=8,/color            
            if i eq 2 then device,filename = 'pathre_c3_f2.ps',bits=8,/color            
            if i eq 3 then device,filename = 'pathre_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'pathre_c1_ws_f2.ps',bits=8,/color            
            if i eq 5 then device,filename = 'pathre_c2_ws_f2.ps',bits=8,/color            
            if i eq 6 then device,filename = 'pathre_c3_ws_f2.ps',bits=8,/color            
            if i eq 7 then device,filename = 'pathre_c4_ws_f2.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*pathre_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = 0.

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*pathre_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000. 
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = 0.

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;#############
;#############
;ffce
 lvl_divisions = 257/5.
          increment = (1)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(1/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
;##################
for i=0,7 do begin
; thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        
         
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000. 
;	    if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000. 
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'ffce_c1_f2.ps',bits=8,/color
            if i eq 1 then device,filename = 'ffce_c2_f2.ps',bits=8,/color
            if i eq 2 then device,filename = 'ffce_c3_f2.ps',bits=8,/color
            if i eq 3 then device,filename = 'ffce_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'ffce_c1_ws_f2.ps',bits=8,/color
            if i eq 5 then device,filename = 'ffce_c2_ws_f2.ps',bits=8,/color
            if i eq 6 then device,filename = 'ffce_c3_ws_f2.ps',bits=8,/color
            if i eq 7 then device,filename = 'ffce_c4_ws_f2.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*ffce_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000. 
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257


keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*ffce_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;###############
;##################
;refractive index
 lvl_divisions = 257/5.
          increment = (50)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(50/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

for i=0,7 do begin
;            thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.           
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'rdx_c1_f2.ps',bits=8,/color
            if i eq 1 then device,filename = 'rdx_c2_f2.ps',bits=8,/color
            if i eq 2 then device,filename = 'rdx_c3_f2.ps',bits=8,/color
            if i eq 3 then device,filename = 'rdx_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'rdx_c1_ws_f2.ps',bits=8,/color
            if i eq 5 then device,filename = 'rdx_c2_ws_f2.ps',bits=8,/color
            if i eq 6 then device,filename = 'rdx_c3_ws_f2.ps',bits=8,/color
            if i eq 7 then device,filename = 'rdx_c4_ws_f2.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
;            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*rdx_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257


keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*rdx_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;#############
;###################
;gendrin angle
          lvl_divisions = 257/5.
          increment = (max_gen-min_gen)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_gen
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_gen-min_gen)/divisions) + min_gen
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
for i=0,7 do begin
 ;           thisDevice = !d.name
 ;           set_plot,'z',/copy
 ;           device,set_resolution=[515,510],z_buffer=0
 ;           erase        
           
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'tgn_c1_f2.ps',bits=8,/color
            if i eq 1 then device,filename = 'tgn_c2_f2.ps',bits=8,/color
            if i eq 2 then device,filename = 'tgn_c3_f2.ps',bits=8,/color
            if i eq 3 then device,filename = 'tgn_c4_f2.ps',bits=8,/color
            if i eq 4 then device,filename = 'tgn_c1_ws_f2.ps',bits=8,/color
            if i eq 5 then device,filename = 'tgn_c2_ws_f2.ps',bits=8,/color
            if i eq 6 then device,filename = 'tgn_c3_ws_f2.ps',bits=8,/color
            if i eq 7 then device,filename = 'tgn_c4_ws_f2.ps',bits=8,/color
            
;            tvlct,redvector,greenvector,bluevector
;            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device


keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'
endfor
;###############
;################
;resonance cone angle
 lvl_divisions = 257/5.
          increment = (max_res-min_res)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_res
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_res-min_res)/divisions) + min_res
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
for i=0,7 do begin
; thisDevice = !d.name
;            set_plot,'z',/copy
;            device,set_resolution=[515,510],z_buffer=0
;            erase        

;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c1_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.            
;	    if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c2_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c3_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c4_f2
;tmp = where(keepers_source eq 0.)
;keepers_source(tmp) = -10000.
;            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c1_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;	    if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c2_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c3_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c4_f2
;tmp = where(keepers_ws eq 0.)
;keepers_ws(tmp) = -10000.
;            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=257
;
;            a=TVRD() 
;            device,/close
;            set_plot,thisDevice

	    set_plot,'ps'
            !p.font = 0
            if i eq 0 then device,filename = 'trs_c1_f2.ps',bits=8,/color
            if i eq 1 then device,filename = 'trs_c2_f2.ps',bits=8,/color
            if i eq 2 then device,filename = 'trs_c3_f2.ps',bits=8,/color
            if i eq 3 then device,filename = 'trs_c4_f2.ps',bits=8,/color   
            if i eq 4 then device,filename = 'trs_c1_ws_f2.ps',bits=8,/color
            if i eq 5 then device,filename = 'trs_c2_ws_f2.ps',bits=8,/color
            if i eq 6 then device,filename = 'trs_c3_ws_f2.ps',bits=8,/color
            if i eq 7 then device,filename = 'trs_c4_ws_f2.ps',bits=8,/color            

;            tvlct,redvector,greenvector,bluevector
            
;            px = 0. 
;            py = 0.
;            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device


keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c1_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 0 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c2_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 1 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c3_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 2 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_source = (-1)*(not_keepers_source(*,*)-1)*trs_c4_f2
tmp = where(keepers_source eq 0.)
keepers_source(tmp) = -10000.
            if i eq 3 then contour,keepers_source,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c1_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 4 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c2_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 5 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c3_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 6 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*trs_c4_f2
tmp = where(keepers_ws eq 0.)
keepers_ws(tmp) = -10000.
            if i eq 7 then contour,keepers_ws,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=257

            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257 

            !p.font = -1             ;change font back so that it can be viewed correctly on screen
            device,/close
            set_plot,'x'

endfor
;###############

keepers_source = 0.
keepers_ws = 0.

end