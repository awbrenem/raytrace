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


;##################
pro histoplots
device,decomposed=0
;restore,'raydata.dat'
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
;CROSS-CORRELATION DATA
;###############################################################################################

for pair=0,5 do begin       
	raydata_avg_flo = {one:strarr(7),two:strarr(7)}
        raydata_avg_fhi = {one:strarr(7),two:strarr(7)}
        raydata_avg = fltarr(7,2,2)  ;(values|2 freqs for each pair|2 sc for each pair)
        if pair eq 0 then tmp = where(not_keepers_cc12 eq 0.) 
        if pair eq 1 then tmp = where(not_keepers_cc13 eq 0.)
        if pair eq 2 then tmp = where(not_keepers_cc14 eq 0.)
        if pair eq 3 then tmp = where(not_keepers_cc23 eq 0.)
        if pair eq 4 then tmp = where(not_keepers_cc24 eq 0.)
        if pair eq 5 then tmp = where(not_keepers_cc34 eq 0.)
	nelem = strtrim(n_elements(tmp),2)

;first freq in pair and first sc
;##########################################
	if pair eq 0 then begin
                wh = where(freq_all(0) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc12 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 1 then begin
                wh = where(freq_all(2) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc13 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 2 then begin
                wh = where(freq_all(4) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc14 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 3 then begin
                wh = where(freq_all(6) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c2 in cc23 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 4 then begin
                wh = where(freq_all(8) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c2 in cc24 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 5 then begin
                wh = where(freq_all(10) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c3 in cc34 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif

        t1 = arrtmpk(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_K_SCA-- ' + t1str
        raydata_avg_flo.one(0) = t1str(0)        
        arrtmpk = 0

        t1 = arrtmpg(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_G_SCA-- ' + t1str
        raydata_avg_flo.one(1) = t1str(0)        
        arrtmpg = 0

        t1 = arrtmpre(tmp)
	t1str = big_moments(t1)
        t1str = 'PATHRE_SCA-- ' + t1str
        raydata_avg_flo.one(2) = t1str(0)        
        arrtmpre = 0

        t1 = arrtmpffce(tmp)
	t1str = big_moments(t1)
        t1str = 'FFCE_SCA-- ' + t1str
        raydata_avg_flo.one(3) = t1str(0)        
        arrtmpffce = 0

        t1 = arrtmprdx(tmp)
	t1str = big_moments(t1)
        t1str = 'RDX_SCA-- ' + t1str
        raydata_avg_flo.one(4) = t1str(0)        
        arrtmprdx = 0

        t1 = arrtmptgn(tmp)
	t1str = big_moments(t1)
        t1str = 'TGN_SCA-- ' + t1str
        raydata_avg_flo.one(5) = t1str(0)        
        arrtmptgn = 0

        t1 = arrtmptrs(tmp)
	t1str = big_moments(t1)
        t1str = 'TRS_SCA-- ' + t1str
        raydata_avg_flo.one(6) = t1str(0)        
        arrtmptrs = 0


;first freq in pair and second sc
;###############################################################
	if pair eq 0 then begin
                wh = where(freq_all(0) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c1 in cc12 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 1 then begin
                wh = where(freq_all(2) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c1 in cc13 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif
        if pair eq 2 then begin
                wh = where(freq_all(4) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c1 in cc14 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif
        if pair eq 3 then begin
                wh = where(freq_all(6) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c2 in cc23 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif
        if pair eq 4 then begin
                wh = where(freq_all(8) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c2 in cc24 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif
        if pair eq 5 then begin
                wh = where(freq_all(10) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c3 in cc34 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif

        t1 = arrtmpk(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_K_SCB-- ' + t1str
        raydata_avg_flo.two(0) = t1str(0)        
        arrtmpk = 0

        t1 = arrtmpg(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_G_SCB-- ' + t1str
        raydata_avg_flo.two(1) = t1str(0)        
        arrtmpg = 0

        t1 = arrtmpre(tmp)
	t1str = big_moments(t1)
        t1str = 'PATHRE_SCB-- ' + t1str
        raydata_avg_flo.two(2) = t1str(0)        
        arrtmpre = 0

        t1 = arrtmpffce(tmp)
	t1str = big_moments(t1)
        t1str = 'FFCE_SCB-- ' + t1str
        raydata_avg_flo.two(3) = t1str(0)        
        arrtmpffce = 0

        t1 = arrtmprdx(tmp)
	t1str = big_moments(t1)
        t1str = 'RDX_SCB-- ' + t1str
        raydata_avg_flo.two(4) = t1str(0)        
        arrtmprdx = 0

        t1 = arrtmptgn(tmp)
	t1str = big_moments(t1)
        t1str = 'TGN_SCB-- ' + t1str
        raydata_avg_flo.two(5) = t1str(0)        
        arrtmptgn = 0

        t1 = arrtmptrs(tmp)
	t1str = big_moments(t1)
        t1str = 'TRS_SCB-- ' + t1str
        raydata_avg_flo.two(6) = t1str(0)        
        arrtmptrs = 0

;second freq in pair and first sc
;##########################################

	if pair eq 0 then begin
                wh = where(freq_all(1) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc12 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 1 then begin
                wh = where(freq_all(3) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc13 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 2 then begin
                wh = where(freq_all(5) eq unique_freqs(*,0))
		arrtmpk = theta_k(*,*,wh,0) ;c1 in cc14 low freq
                arrtmpg = theta_g(*,*,wh,0)
                arrtmpre = pathre(*,*,wh,0)
                arrtmpffce = ffce(*,*,wh,0)
                arrtmprdx = rdx(*,*,wh,0)
                arrtmptgn = tgn(*,*,wh,0)
                arrtmptrs = trs(*,*,wh,0)
	endif
        if pair eq 3 then begin
                wh = where(freq_all(7) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c2 in cc23 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 4 then begin
                wh = where(freq_all(9) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c2 in cc24 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 5 then begin
                wh = where(freq_all(11) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c3 in cc34 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif

        t1 = arrtmpk(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_K_SCA-- ' + t1str
        raydata_avg_fhi.one(0) = t1str(0)        
        arrtmpk = 0

        t1 = arrtmpg(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_G_SCA-- ' + t1str
        raydata_avg_fhi.one(1) = t1str(0)        
        arrtmpg = 0

        t1 = arrtmpre(tmp)
	t1str = big_moments(t1)
        t1str = 'PATHRE_SCA-- ' + t1str
        raydata_avg_fhi.one(2) = t1str(0)        
        arrtmpre = 0

        t1 = arrtmpffce(tmp)
	t1str = big_moments(t1)
        t1str = 'FFCE_SCA-- ' + t1str
        raydata_avg_fhi.one(3) = t1str(0)        
        arrtmpffce = 0

        t1 = arrtmprdx(tmp)
	t1str = big_moments(t1)
        t1str = 'RDX_SCA-- ' + t1str
        raydata_avg_fhi.one(4) = t1str(0)        
        arrtmprdx = 0

        t1 = arrtmptgn(tmp)
	t1str = big_moments(t1)
        t1str = 'TGN_SCA-- ' + t1str
        raydata_avg_fhi.one(5) = t1str(0)        
        arrtmptgn = 0

        t1 = arrtmptrs(tmp)
	t1str = big_moments(t1)
        t1str = 'TRS_SCA-- ' + t1str
        raydata_avg_fhi.one(6) = t1str(0)        
        arrtmptrs = 0

;second freq in pair and second sc
;#############################################

	if pair eq 0 then begin
                wh = where(freq_all(1) eq unique_freqs(*,1))
		arrtmpk = theta_k(*,*,wh,1) ;c1 in cc12 low freq
                arrtmpg = theta_g(*,*,wh,1)
                arrtmpre = pathre(*,*,wh,1)
                arrtmpffce = ffce(*,*,wh,1)
                arrtmprdx = rdx(*,*,wh,1)
                arrtmptgn = tgn(*,*,wh,1)
                arrtmptrs = trs(*,*,wh,1)
	endif
        if pair eq 1 then begin
                wh = where(freq_all(3) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c1 in cc13 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif
        if pair eq 2 then begin
                wh = where(freq_all(5) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c1 in cc14 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif
        if pair eq 3 then begin
                wh = where(freq_all(7) eq unique_freqs(*,2))
		arrtmpk = theta_k(*,*,wh,2) ;c2 in cc23 low freq
                arrtmpg = theta_g(*,*,wh,2)
                arrtmpre = pathre(*,*,wh,2)
                arrtmpffce = ffce(*,*,wh,2)
                arrtmprdx = rdx(*,*,wh,2)
                arrtmptgn = tgn(*,*,wh,2)
                arrtmptrs = trs(*,*,wh,2)
	endif
        if pair eq 4 then begin
                wh = where(freq_all(9) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c2 in cc24 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif
        if pair eq 5 then begin
                wh = where(freq_all(11) eq unique_freqs(*,3))
		arrtmpk = theta_k(*,*,wh,3) ;c3 in cc34 low freq
                arrtmpg = theta_g(*,*,wh,3)
                arrtmpre = pathre(*,*,wh,3)
                arrtmpffce = ffce(*,*,wh,3)
                arrtmprdx = rdx(*,*,wh,3)
                arrtmptgn = tgn(*,*,wh,3)
                arrtmptrs = trs(*,*,wh,3)
	endif

        t1 = arrtmpk(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_K_SCB-- ' + t1str
        raydata_avg_fhi.two(0) = t1str(0)        
        arrtmpk = 0

        t1 = arrtmpg(tmp)
	t1str = big_moments(t1)
        t1str = 'THETA_G_SCB-- ' + t1str
        raydata_avg_fhi.two(1) = t1str(0)        
        arrtmpg = 0

        t1 = arrtmpre(tmp)
	t1str = big_moments(t1)
        t1str = 'PATHRE_SCB-- ' + t1str
        raydata_avg_fhi.two(2) = t1str(0)        
        arrtmpre = 0

        t1 = arrtmpffce(tmp)
	t1str = big_moments(t1)
        t1str = 'FFCE_SCB-- ' + t1str
        raydata_avg_fhi.two(3) = t1str(0)        
        arrtmpffce = 0

        t1 = arrtmprdx(tmp)
	t1str = big_moments(t1)
        t1str = 'RDX_SCB-- ' + t1str
        raydata_avg_fhi.two(4) = t1str(0)        
        arrtmprdx = 0

        t1 = arrtmptgn(tmp)
	t1str = big_moments(t1)
        t1str = 'TGN_SCB-- ' + t1str
        raydata_avg_fhi.two(5) = t1str(0)        
        arrtmptgn = 0

        t1 = arrtmptrs(tmp)
	t1str = big_moments(t1)
        t1str = 'TRS_SCB-- ' + t1str
        raydata_avg_fhi.two(6) = t1str(0)        
        arrtmptrs = 0

        if pair eq 0 then openw,lun,'./raydata_cc12.txt',/get_lun
        if pair eq 1 then openw,lun,'./raydata_cc13.txt',/get_lun
        if pair eq 2 then openw,lun,'./raydata_cc14.txt',/get_lun
        if pair eq 3 then openw,lun,'./raydata_cc23.txt',/get_lun
        if pair eq 4 then openw,lun,'./raydata_cc24.txt',/get_lun
        if pair eq 5 then openw,lun,'./raydata_cc34.txt',/get_lun

                   printf,lun,'THIS IS ALL THE RAYDATA FOR THE IDENTIFIED SOURCE REGION'
                   printf,lun,'NUMBER OF ELEMENTS: ' + nelem
                   printf,lun,'******************'
                   printf,lun,'FREQ = ' + strtrim(freq_all(2*pair),2) + ' Hz'                  
                   printf,lun,'******************'
                   printf,lun,raydata_avg_flo.one
                   printf,lun,'******************'
                   printf,lun,raydata_avg_flo.two
                   printf,lun,'******************'
                   printf,lun,'FREQ = ' + strtrim(freq_all((2*pair)+1),2) + ' Hz'                 
                   printf,lun,'******************'
                   printf,lun,raydata_avg_fhi.one
                   printf,lun,'******************'
                   printf,lun,raydata_avg_fhi.two
                   close,lun
                   free_lun,lun           
endfor  ;pair        
;######################################
;END OF CROSS-CORRELATION DATA
;######################################       

for o=0,1 do begin  
  ;first the actual source
  ;next the whole source
        
           if o eq 0 then tmp = where(not_keepers_source eq 0.)
           if o eq 1 then tmp = where(not_keepers_ws eq 0.)

     if tmp(0) ne -1 and n_elements(tmp) gt 1 then begin
           nelem = strtrim(n_elements(tmp),2)		         
           for sc=0,3 do begin                      
                   if o eq 0 then begin
                       if sc eq 0 then openw,lun,'./raydata_c1.txt',/get_lun
                       if sc eq 1 then openw,lun,'./raydata_c2.txt',/get_lun
                       if sc eq 2 then openw,lun,'./raydata_c3.txt',/get_lun
                       if sc eq 3 then openw,lun,'./raydata_c4.txt',/get_lun
                       printf,lun,'THIS IS ALL THE RAYDATA FOR THE ACTUAL SOURCE REGION'
                   endif
                   if o eq 1 then begin
                       if sc eq 0 then openw,lun,'./raydata_ws_c1.txt',/get_lun                   
                       if sc eq 1 then openw,lun,'./raydata_ws_c2.txt',/get_lun
                       if sc eq 2 then openw,lun,'./raydata_ws_c3.txt',/get_lun
                       if sc eq 3 then openw,lun,'./raydata_ws_c4.txt',/get_lun
                       printf,lun,'THIS IS ALL THE RAYDATA FOR THE WHOLE SOURCE REGION'
                   endif         
           for freq=0,5 do begin                    
           raydata_avg = strarr(7)  ;7 parameters to compute and 6 possible freqs for each sc           
;for each of 6 possible freqs
             
;theta_k
                       arrtmp = theta_k(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'THETA_K_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                        
                       raydata_avg(0) = tmpstr
;theta_g
                       arrtmp = theta_g(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'THETA_G_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(1) = tmpstr
;path_re
                       arrtmp = pathre(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'PATHRE_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(2) = tmpstr
;ffce
                       arrtmp = ffce(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'FFCE_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(3) = tmpstr
;rdx
                       arrtmp = rdx(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'RDX_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(4) = tmpstr
;tgn
                       arrtmp = tgn(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'TGN_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(5) = tmpstr
;trs
                       arrtmp = trs(*,*,freq,sc)
                       stuff = arrtmp(tmp)
                       tmpstr = big_moments(stuff)               
                       tmpstr = 'TRS_SC1--  ' + tmpstr
                       tmpstr = tmpstr(0)                       
                       raydata_avg(6) = tmpstr                       
                    
                   if freq eq 0 then printf,lun,'NUMBER OF ELEMENTS: ' + nelem
                   printf,lun,'******************'
                   printf,lun,'FREQ = ' + strtrim(unique_freqs(freq,sc),2) + ' Hz'
                   printf,lun,raydata_avg(0)
                   printf,lun,raydata_avg(1)
                   printf,lun,raydata_avg(2)
                   printf,lun,raydata_avg(3)
                   printf,lun,raydata_avg(4)
                   printf,lun,raydata_avg(5)
                   printf,lun,raydata_avg(6)                  
             endfor  ;freq                                  
                   close,lun
                   free_lun,lun

    
             freq = 0.
             endfor ;sc
             sc = 0. 


            endif  ;for a nonzero # of elements in source or whole source
     endfor ;source then whole source

xvals = indgen(16)*5

;##############################################################
;WHOLE SOURCE HISTOGRAMS
;##############################################################
;#############################################################
;****FINAL****PLAIN AND SIMPLE HISTOGRAM FOR WHOLE SOURCE
;source --> actual overlap b/t all 6 cc plots
;whole source --> source + filled in regions 
;##############################################################
plain_source = 'yes'
if plain_source eq 'yes' then begin

;####
for sc=0,3 do begin
set_plot,'ps'
if sc eq 0 then device,filename='hist_c1.ps'
if sc eq 1 then device,filename='hist_c2.ps'
if sc eq 2 then device,filename='hist_c3.ps'
if sc eq 3 then device,filename='hist_c4.ps'

!p.font=0
!p.multi=[0,3,4]

for ws_s=0,1 do begin  ;whole source, then source
for freq=0,5 do begin

;if (ws_s eq 0) or (ws_s eq 1) then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn(*,*,freq,sc)
;if ws_s eq 0 then keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn(*,*,freq,sc)

if (ws_s eq 0) or (ws_s eq 1) then keepers_source = (-1)*(not_keepers_source(*,*)-1)
if ws_s eq 0 then keepers_ws = (-1)*(not_keepers_ws(*,*)-1)

src = where((keepers_source ne -10000) and (keepers_source ne 0.))
whsrc = where((keepers_ws ne -10000) and (keepers_ws ne 0.))

if ws_s eq 0 then n_sourcepts = strtrim(n_elements(whsrc),2)
if ws_s eq 1 then n_sourcepts = strtrim(n_elements(src),2)

if whsrc(0) ne -1 then begin
arrtmp = tgn(*,*,freq,sc)
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
arrtmp = theta_k(*,*,freq,sc)
if ws_s eq 0 then h1 = histogram(arrtmp(whsrc),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if ws_s eq 1 and src(0) ne -1 then h1 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(src))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
arrtmp = trs(*,*,freq,sc)
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
arrtmp = theta_k(*,*,freq,sc)
if ws_s eq 0 and src(0) ne -1 then h4 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if src(0) eq -1 then h4 = fltarr(16)

if ws_s eq 0 and sc eq 0 then title = 'C1 WS FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,0),2)
if ws_s eq 0 and sc eq 1 then title = 'C2 WS FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,1),2)
if ws_s eq 0 and sc eq 2 then title = 'C3 WS FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,2),2)
if ws_s eq 0 and sc eq 3 then title = 'C4 WS FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,3),2)
if ws_s eq 1 and sc eq 0 then title = 'C1 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,0),2)
if ws_s eq 1 and sc eq 1 then title = 'C2 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,1),2)
if ws_s eq 1 and sc eq 2 then title = 'C3 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,2),2)
if ws_s eq 1 and sc eq 3 then title = 'C4 FOR ' + n_sourcepts + ' PTS - freq= ' + strtrim(unique_freqs(freq,3),2)

if ws_s eq 0 then bar_graph,xvals,h1,barcolor=25,barborder=255.,title=title
if ws_s eq 0 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h4,barcolor=220,barborder=255,overplot='yes'

if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
endif
endfor  ;freq eq 0 to 5 ...for each possible freq for single sc
freq=0
endfor ;source and whole source  ws_s
ws_s = 0
device,/close
!p.font=-1
set_plot,'x'
endfor ;each sc
endif
;######################################################################################

;#######################################################################
;****FINAL****HISTOPLOTS IN SECTIONS OF ZGRID FOR FREQ 1 FOR ENTIRE SOURCE
;TO BE USED WITH WHOLE SOURCE
;######################################################################

kr = 'yes'
if kr eq 'yes' then begin
zzones = [1.0,0.8,0.6,0.4,0.2,0.0,-0.2,-0.4,-0.6,-0.8,-1.0]
zzones_str = strtrim(zzones,2)
for p=0,n_elements(zzones_str)-1 do begin
    dot = strpos(zzones_str(p),'.')
    zzones_str(p) = strmid(zzones_str(p),0,dot+3)
endfor

for sc=0,3 do begin

for zzon=0,9 do begin  ;for the five z-grid regions

!p.multi=[0,3,4]
if sc eq 0 then begin
   if zzon eq 0 then filename = 'hist_zone_' + string(zzones(1),format='(F4.1)') + '-' + string(zzones(0),format='(F4.1)') + '_c1.ps'
   if zzon eq 1 then filename = 'hist_zone_' + string(zzones(2),format='(F4.1)') + '-' + string(zzones(1),format='(F4.1)') + '_c1.ps'
   if zzon eq 2 then filename = 'hist_zone_' + string(zzones(3),format='(F4.1)') + '-' + string(zzones(2),format='(F4.1)') + '_c1.ps'
   if zzon eq 3 then filename = 'hist_zone_' + string(zzones(4),format='(F4.1)') + '-' + string(zzones(3),format='(F4.1)') + '_c1.ps'
   if zzon eq 4 then filename = 'hist_zone_' + string(zzones(5),format='(F4.1)') + '-' + string(zzones(4),format='(F4.1)') + '_c1.ps'
   if zzon eq 5 then filename = 'hist_zone_' + string(zzones(6),format='(F4.1)') + '-' + string(zzones(5),format='(F4.1)') + '_c1.ps'
   if zzon eq 6 then filename = 'hist_zone_' + string(zzones(7),format='(F4.1)') + '-' + string(zzones(6),format='(F4.1)') + '_c1.ps'
   if zzon eq 7 then filename = 'hist_zone_' + string(zzones(8),format='(F4.1)') + '-' + string(zzones(7),format='(F4.1)') + '_c1.ps'
   if zzon eq 8 then filename = 'hist_zone_' + string(zzones(9),format='(F4.1)') + '-' + string(zzones(8),format='(F4.1)') + '_c1.ps'
   if zzon eq 9 then filename = 'hist_zone_' + string(zzones(10),format='(F4.1)') + '-' + string(zzones(9),format='(F4.1)') + '_c1.ps'
endif
if sc eq 1 then begin
   if zzon eq 0 then filename = 'hist_zone_' + string(zzones(1),format='(F4.1)') + '-' + string(zzones(0),format='(F4.1)') + '_c2.ps'
   if zzon eq 1 then filename = 'hist_zone_' + string(zzones(2),format='(F4.1)') + '-' + string(zzones(1),format='(F4.1)') + '_c2.ps'
   if zzon eq 2 then filename = 'hist_zone_' + string(zzones(3),format='(F4.1)') + '-' + string(zzones(2),format='(F4.1)') + '_c2.ps'
   if zzon eq 3 then filename = 'hist_zone_' + string(zzones(4),format='(F4.1)') + '-' + string(zzones(3),format='(F4.1)') + '_c2.ps'
   if zzon eq 4 then filename = 'hist_zone_' + string(zzones(5),format='(F4.1)') + '-' + string(zzones(4),format='(F4.1)') + '_c2.ps'
   if zzon eq 5 then filename = 'hist_zone_' + string(zzones(6),format='(F4.1)') + '-' + string(zzones(5),format='(F4.1)') + '_c2.ps'
   if zzon eq 6 then filename = 'hist_zone_' + string(zzones(7),format='(F4.1)') + '-' + string(zzones(6),format='(F4.1)') + '_c2.ps'
   if zzon eq 7 then filename = 'hist_zone_' + string(zzones(8),format='(F4.1)') + '-' + string(zzones(7),format='(F4.1)') + '_c2.ps'
   if zzon eq 8 then filename = 'hist_zone_' + string(zzones(9),format='(F4.1)') + '-' + string(zzones(8),format='(F4.1)') + '_c2.ps'
   if zzon eq 9 then filename = 'hist_zone_' + string(zzones(10),format='(F4.1)') + '-' + string(zzones(9),format='(F4.1)') + '_c2.ps'
endif
if sc eq 2 then begin
   if zzon eq 0 then filename = 'hist_zone_' + string(zzones(1),format='(F4.1)') + '-' + string(zzones(0),format='(F4.1)') + '_c3.ps'
   if zzon eq 1 then filename = 'hist_zone_' + string(zzones(2),format='(F4.1)') + '-' + string(zzones(1),format='(F4.1)') + '_c3.ps'
   if zzon eq 2 then filename = 'hist_zone_' + string(zzones(3),format='(F4.1)') + '-' + string(zzones(2),format='(F4.1)') + '_c3.ps'
   if zzon eq 3 then filename = 'hist_zone_' + string(zzones(4),format='(F4.1)') + '-' + string(zzones(3),format='(F4.1)') + '_c3.ps'
   if zzon eq 4 then filename = 'hist_zone_' + string(zzones(5),format='(F4.1)') + '-' + string(zzones(4),format='(F4.1)') + '_c3.ps'
   if zzon eq 5 then filename = 'hist_zone_' + string(zzones(6),format='(F4.1)') + '-' + string(zzones(5),format='(F4.1)') + '_c3.ps'
   if zzon eq 6 then filename = 'hist_zone_' + string(zzones(7),format='(F4.1)') + '-' + string(zzones(6),format='(F4.1)') + '_c3.ps'
   if zzon eq 7 then filename = 'hist_zone_' + string(zzones(8),format='(F4.1)') + '-' + string(zzones(7),format='(F4.1)') + '_c3.ps'
   if zzon eq 8 then filename = 'hist_zone_' + string(zzones(9),format='(F4.1)') + '-' + string(zzones(8),format='(F4.1)') + '_c3.ps'
   if zzon eq 9 then filename = 'hist_zone_' + string(zzones(10),format='(F4.1)') + '-' + string(zzones(9),format='(F4.1)') + '_c3.ps'
endif
if sc eq 3 then begin
   if zzon eq 0 then filename = 'hist_zone_' + string(zzones(1),format='(F4.1)') + '-' + string(zzones(0),format='(F4.1)') + '_c4.ps'
   if zzon eq 1 then filename = 'hist_zone_' + string(zzones(2),format='(F4.1)') + '-' + string(zzones(1),format='(F4.1)') + '_c4.ps'
   if zzon eq 2 then filename = 'hist_zone_' + string(zzones(3),format='(F4.1)') + '-' + string(zzones(2),format='(F4.1)') + '_c4.ps'
   if zzon eq 3 then filename = 'hist_zone_' + string(zzones(4),format='(F4.1)') + '-' + string(zzones(3),format='(F4.1)') + '_c4.ps'
   if zzon eq 4 then filename = 'hist_zone_' + string(zzones(5),format='(F4.1)') + '-' + string(zzones(4),format='(F4.1)') + '_c4.ps'
   if zzon eq 5 then filename = 'hist_zone_' + string(zzones(6),format='(F4.1)') + '-' + string(zzones(5),format='(F4.1)') + '_c4.ps'
   if zzon eq 6 then filename = 'hist_zone_' + string(zzones(7),format='(F4.1)') + '-' + string(zzones(6),format='(F4.1)') + '_c4.ps'
   if zzon eq 7 then filename = 'hist_zone_' + string(zzones(8),format='(F4.1)') + '-' + string(zzones(7),format='(F4.1)') + '_c4.ps'
   if zzon eq 8 then filename = 'hist_zone_' + string(zzones(9),format='(F4.1)') + '-' + string(zzones(8),format='(F4.1)') + '_c4.ps'
   if zzon eq 9 then filename = 'hist_zone_' + string(zzones(10),format='(F4.1)') + '-' + string(zzones(9),format='(F4.1)') + '_c4.ps'
endif

set_plot,'ps'
device,filename=filename
!p.font=0

goodz = where((zgrid ge zzones(zzon+1)) and (zgrid lt zzones(zzon)))
for ws_s=0,1 do begin   ;whole source, then source
for freq=0,5 do begin

extra = strtrim(unique_freqs(freq,sc),2) + ' Hz'

;test to see if there are any source elements within the goodz range
if (goodz(0) eq -1) or (n_elements(goodz(0)) eq 1) then tmp = -1
if (goodz(0) ne -1) and (n_elements(goodz(0)) gt 1) then begin
tarr = fltarr(401,401)
count_all=0
arrtmp = theta_k(*,*,freq,sc)
for b=0,n_elements(goodz)-1 do begin
   all_tmp = where((arrtmp(*,goodz(b)) ne -10000) and (arrtmp(*,goodz(b)) ne 0.))
   if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
endfor

;whsrc = where(tarr ne 0.)
;if ws_s eq 0 then keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tgn(*,*,freq,sc)*tarr
;if (ws_s eq 0) or (ws_s eq 1) then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tgn(*,*,freq,sc)*tarr

if ws_s eq 0 then keepers_ws = (-1)*(not_keepers_ws(*,*)-1)*tarr
if (ws_s eq 0) or (ws_s eq 1) then keepers_source = (-1)*(not_keepers_source(*,*)-1)*tarr

src = where((keepers_source ne -10000) and (keepers_source ne 0.))
whsrc = where((keepers_ws ne -10000) and (keepers_ws ne 0.))

endif
if goodz(0) eq -1 then src = -1
if whsrc(0) ne -1 and goodz(0) ne -1 then begin
arrtmp = tgn(*,*,freq,sc)
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
max1 = max1/5.
min1 = min1/5.
arrtmp = theta_k(*,*,freq,sc)
if ws_s eq 0 then h1 = histogram(arrtmp(whsrc),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if ws_s eq 1 and src(0) ne -1 then h1 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(src))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
arrtmp = trs(*,*,freq,sc)
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
if ws_s eq 0 then arrtmp = theta_k(*,*,freq,sc)
if ws_s eq 0 and src(0) ne -1 then h4 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if src(0) eq -1 then h4 = fltarr(16)
if ws_s eq 1 then title = 'S ' + extra
if ws_s eq 0 then title = 'W ' + extra

if ws_s eq 0 then bar_graph,xvals,h1,barcolor=25,barborder=255.,title=title
if ws_s eq 0 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h4,barcolor=220,barborder=255,overplot='yes'

if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
endif   ;whsrc(0) ne -1 and goodz(0) ne -1
endfor ;freq
freq=0
endfor ;ws_s  first for whole source, then source
ws_s = 0

endfor ;zzon
zzon=0
device,/close
!p.font=-1
set_plot,'x'
endfor ;sc
sc = 0
endif
;##############################################################
;CC HISTOGRAMS
;source --> all appropriate cc rays
;whole source --> all possible cc rays
;##############################################################

entire_source = 'yes'
if entire_source eq 'yes' then begin
wh=0
for pair=0,5 do begin
set_plot,'ps'

if pair eq 0 then device,filename='hist_cc12_all.ps'
if pair eq 1 then device,filename='hist_cc13_all.ps'
if pair eq 2 then device,filename='hist_cc14_all.ps'
if pair eq 3 then device,filename='hist_cc23_all.ps'
if pair eq 4 then device,filename='hist_cc24_all.ps'
if pair eq 5 then device,filename='hist_cc34_all.ps'
!p.font=0
!p.multi=[0,2,4]
;####

for sc=0,1 do begin ;2 sc for each cc
for freq=0,1 do begin  ;2 freqs for each cc pair
for ws_s = 0,1 do begin   ;whole source, then source

if sc eq 0 then begin
if pair eq 0 then wh = where(freq_all(0+freq) eq unique_freqs(*,0))
if pair eq 1 then wh = where(freq_all(2+freq) eq unique_freqs(*,0))
if pair eq 2 then wh = where(freq_all(4+freq) eq unique_freqs(*,0))
if pair eq 3 then wh = where(freq_all(6+freq) eq unique_freqs(*,1))
if pair eq 4 then wh = where(freq_all(8+freq) eq unique_freqs(*,1))
if pair eq 5 then wh = where(freq_all(10+freq) eq unique_freqs(*,2))
if pair eq 0 then actsc = 0
if pair eq 1 then actsc = 0
if pair eq 2 then actsc = 0
if pair eq 3 then actsc = 1
if pair eq 4 then actsc = 1
if pair eq 5 then actsc = 2
endif
if sc eq 1 then begin
if pair eq 0 then wh = where(freq_all(0+freq) eq unique_freqs(*,1))
if pair eq 1 then wh = where(freq_all(2+freq) eq unique_freqs(*,2))
if pair eq 2 then wh = where(freq_all(4+freq) eq unique_freqs(*,3))
if pair eq 3 then wh = where(freq_all(6+freq) eq unique_freqs(*,2))
if pair eq 4 then wh = where(freq_all(8+freq) eq unique_freqs(*,3))
if pair eq 5 then wh = where(freq_all(10+freq) eq unique_freqs(*,3))
if pair eq 0 then actsc = 1
if pair eq 1 then actsc = 2
if pair eq 2 then actsc = 3
if pair eq 3 then actsc = 2
if pair eq 4 then actsc = 3
if pair eq 5 then actsc = 3
endif

arrtmp = theta_k(*,*,wh,actsc)
whsrc = where((arrtmp(*,*) ne -10000.) and (arrtmp(*,*) ne 0.))
arrtmp = tgn(*,*,wh,actsc)
if pair eq 0 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)
if pair eq 1 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)
if pair eq 2 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)
if pair eq 3 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)
if pair eq 4 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)
if pair eq 5 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)

src = where((keepers_source ne -10000) and (keepers_source ne 0.))
n_sourcepts = strtrim(n_elements(src),2)
n_wholepts = strtrim(n_elements(whsrc),2)

if whsrc(0) ne -1 then begin
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
arrtmp = theta_k(*,*,wh,actsc)
if ws_s eq 0 then h1 = histogram(arrtmp(whsrc),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if ws_s eq 1 and src(0) ne -1 then h1 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(src))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
arrtmp = trs(*,*,wh,actsc)
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
arrtmp = theta_k(*,*,wh,actsc)
if ws_s eq 0 and src(0) ne -1 then h4 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if src(0) eq -1 then h4 = fltarr(16)

if ws_s eq 0 then npts = n_wholepts
if ws_s eq 1 then npts = n_sourcepts

if freq eq 0 then exxtra = 0
if freq eq 1 then exxtra = 1
if sc eq 0 then begin
if pair eq 0 then title = 'C1: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 1 then title = 'C1: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 2 then title = 'C1: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 3 then title = 'C2: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 4 then title = 'C2: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 5 then title = 'C3: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
endif
if sc eq 1 then begin
if pair eq 0 then title = 'C2: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 1 then title = 'C3: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 2 then title = 'C4: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 3 then title = 'C3: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 4 then title = 'C4: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
if pair eq 5 then title = 'C4: ' + npts + ' PTS,freq= ' + strtrim(freq_all((2*pair)+exxtra),2) + ' Hz'
endif

if ws_s eq 0 then bar_graph,xvals,h1,barcolor=25,barborder=255.,title=title
if ws_s eq 0 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h4,barcolor=220,barborder=255,overplot='yes'

if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endfor ;ws_s  (whole source, then source)
ws_s = 0.
endfor ;freq
freq=0
endfor ;sc
sc=0
device,/close
!p.font = -1
set_plot,'x'
endfor ;each pair
pair = 0
endif

;######################################################################
;Histograms for cc in terms of zzones
;######################################################################

uu='yes'
if uu eq 'yes' then begin
zzones = [1.0,0.2,0.0,-0.2,-0.4,-0.6,-1.0]
for pair=0,5 do begin
!p.font=0

;####

for freq = 0,1 do begin  ;even and then odd elements of freq_all
for sc=0,1 do begin 
newone = 'yes'
!p.multi=[0,3,4]
set_plot,'ps'
for ws_s = 0,1 do begin   ;whole source, then source
for zzo=0,(n_elements(zzones)-2) do begin


if sc eq 0 then begin
if pair eq 0 then wh = where(freq_all(0+freq) eq unique_freqs(*,0))
if pair eq 1 then wh = where(freq_all(2+freq) eq unique_freqs(*,0))
if pair eq 2 then wh = where(freq_all(4+freq) eq unique_freqs(*,0))
if pair eq 3 then wh = where(freq_all(6+freq) eq unique_freqs(*,1))
if pair eq 4 then wh = where(freq_all(8+freq) eq unique_freqs(*,1))
if pair eq 5 then wh = where(freq_all(10+freq) eq unique_freqs(*,2))
if pair eq 0 then actsc = 0
if pair eq 1 then actsc = 0
if pair eq 2 then actsc = 0
if pair eq 3 then actsc = 1
if pair eq 4 then actsc = 1
if pair eq 5 then actsc = 2
endif
if sc eq 1 then begin
if pair eq 0 then wh = where(freq_all(0+freq) eq unique_freqs(*,1))
if pair eq 1 then wh = where(freq_all(2+freq) eq unique_freqs(*,2))
if pair eq 2 then wh = where(freq_all(4+freq) eq unique_freqs(*,3))
if pair eq 3 then wh = where(freq_all(6+freq) eq unique_freqs(*,2))
if pair eq 4 then wh = where(freq_all(8+freq) eq unique_freqs(*,3))
if pair eq 5 then wh = where(freq_all(10+freq) eq unique_freqs(*,3))
if pair eq 0 then actsc = 1
if pair eq 1 then actsc = 2
if pair eq 2 then actsc = 3
if pair eq 3 then actsc = 2
if pair eq 4 then actsc = 3
if pair eq 5 then actsc = 3
endif

if newone eq 'yes' then begin
filename=''
if sc eq 0 then scstr = 'sca'
if sc eq 1 then scstr = 'scb'
fn_ext = scstr + '-f=' + strtrim(unique_freqs(wh,actsc),2) + ' Hz'
filename = filename + fn_ext + '.ps'
if pair eq 0 then device,filename='hist_cc12' + filename
if pair eq 1 then device,filename='hist_cc13' + filename
if pair eq 2 then device,filename='hist_cc14' + filename
if pair eq 3 then device,filename='hist_cc23' + filename
if pair eq 4 then device,filename='hist_cc24' + filename
if pair eq 5 then device,filename='hist_cc34' + filename
newone = 'no'
endif

goodz = where((zgrid ge zzones(zzo+1)) and (zgrid lt zzones(zzo)))

if goodz(0) ne -1 then begin

tarr = fltarr(401,401)
count_all=0

for b=0,n_elements(goodz)-1 do begin
        arrtmp = theta_k(*,*,wh,actsc)
	all_tmp = where((arrtmp(*,goodz(b)) ne -10000) and (arrtmp(*,goodz(b)) ne 0.))
	if all_tmp(0) ne -1 then tarr(all_tmp,goodz(b))=1
endfor
if all_tmp(0) ne -1 then count_all = count_all + n_elements(all_tmp)
whsrc = where((arrtmp(*,*) ne -10000.) and (arrtmp(*,*) ne 0.))
if pair eq 0 then keepers_source = (-1)*(not_keepers_cc12(*,*)-1)*tarr
if pair eq 1 then keepers_source = (-1)*(not_keepers_cc13(*,*)-1)*tarr
if pair eq 2 then keepers_source = (-1)*(not_keepers_cc14(*,*)-1)*tarr
if pair eq 3 then keepers_source = (-1)*(not_keepers_cc23(*,*)-1)*tarr
if pair eq 4 then keepers_source = (-1)*(not_keepers_cc24(*,*)-1)*tarr
if pair eq 5 then keepers_source = (-1)*(not_keepers_cc34(*,*)-1)*tarr

src = where((keepers_source ne -10000) and (keepers_source ne 0.))
n_sourcepts = strtrim(n_elements(src),2)
n_wholepts = strtrim(n_elements(whsrc),2)

if whsrc(0) ne -1 then begin
arrtmp = tgn(*,*,wh,actsc)
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
arrtmp = theta_k(*,*,wh,actsc)
if ws_s eq 0 then h1 = histogram(arrtmp(whsrc),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if ws_s eq 1 and src(0) ne -1 then h1 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(src))
h2 = replicate(0.,n_elements(h1))
h2(max1) = h1(max1)
h2(min1) = h2(min1)
arrtmp = trs(*,*,wh,actsc)
if ws_s eq 0 then max1 = max(arrtmp(whsrc))
if ws_s eq 0 then min1 = min(arrtmp(whsrc))
if ws_s eq 1 and src(0) ne -1 then max1 = max(arrtmp(src))
if ws_s eq 1 and src(0) ne -1 then min1 = min(arrtmp(src))
max1 = max1/5.
min1 = min1/5.
h3 = replicate(0.,n_elements(h1))
h3(max1) = h1(max1)
h3(min1) = h3(min1)
arrtmp = theta_k(*,*,wh,actsc)
if ws_s eq 0 and src(0) ne -1 then h4 = histogram(arrtmp(src),binsize=5,min=0,max=80)/float(n_elements(whsrc))
if src(0) eq -1 then h4 = fltarr(16)

if ws_s eq 0 then npts = n_wholepts
if ws_s eq 1 then npts = n_sourcepts

title = 'zzone' + strtrim(zzo,2) + ' ' + npts + ' points'

if ws_s eq 0 then bar_graph,xvals,h1,barcolor=25,barborder=255.,title=title
if ws_s eq 0 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'
if ws_s eq 0 then bar_graph,xvals,h4,barcolor=220,barborder=255,overplot='yes'

if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h1,barcolor=220,barborder=255.,title=title
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h2,barcolor=150,barborder=255,overplot='yes'
if ws_s eq 1 and src(0) ne -1 then bar_graph,xvals,h3,barcolor=80,barborder=255,overplot='yes'

endif
endif ;goodz(0) ne -1
endfor ;zzones

endfor ;ws_s  (whole source, then source)
ws_s = 0
device,/close
!p.font = -1
set_plot,'x'
endfor ;for sc
sc = 0.
endfor ;even odd (freq)
freq = 0.
endfor ;pair
endif  ;uu
;##################
;END OF HISTOGRAMS
;###################

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

    points=(2*!pi/999.0)*findgen(1000)
    xcenter=0
    ycenter=0
    radius=1
;for plotting the plasmasphere
  ;L_ps=3.3
    r_ps=(sin(points))^2
    y_ps=r_ps*cos(points)
    x_ps=r_ps*sin(points)
    
    x1 = 0.04*findgen(2000) & y1 = x1*sin(1*!pi/180) & y2 = x1*sin(2*!pi/180)
    y3 = x1*sin(3*!pi/180) & y4 = x1*sin(4*!pi/180) & y5 = x1*sin(5*!pi/180)
    y10 = x1*sin(10*!pi/180) & y20 = x1*sin(20*!pi/180) & y30 = x1*sin(30*!pi/180)
    y40 = x1*sin(40*!pi/180) & y50 = x1*sin(50*!pi/180) & y60 = x1*sin(60*!pi/180)
    yn1 = -x1*sin(1*!pi/180) & yn2 = -x1*sin(2*!pi/180) & yn3 = -x1*sin(3*!pi/180)
    yn4 = -x1*sin(4*!pi/180) & yn5 = -x1*sin(5*!pi/180) & yn10 = -x1*sin(10*!pi/180)
    yn20 = -x1*sin(20*!pi/180) & yn30 = -x1*sin(30*!pi/180) & yn40 = -x1*sin(40*!pi/180)
    yn50 = -x1*sin(50*!pi/180) & yn60 = -x1*sin(60*!pi/180)
;############################################
max_thetak = 30.
min_thetak = 10.
max_thetag = 15.
min_thetag = -25.
max_gen = 30.
min_gen = 0.
max_res = 65.
min_res = 40.
tgn_pm_delta = 2.  ;angles within +/- this value are highlighted as the gendrin angle on theta-k plots.
;############################################

for allplots=0,6 do begin

if allplots eq 0 then begin
;####theta_k           
            divisions = 10. 
	    lvl_divisions = 257/5.
            increment = (max_thetak-min_thetak)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment + min_thetak                         
            lvls(0) = -10000.      
            tmplvls = findgen(divisions+1)*((max_thetak-min_thetak)/divisions) + min_thetak
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)
endif
if allplots eq 1 then begin
;####theta_group     
            divisions=10.       
	    lvl_divisions = 257/5.
            increment = (max_thetag-min_thetag)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment + min_thetag          
            lvls(0) = -10000.          
            tmplvls = findgen(divisions+1)*((max_thetag-min_thetag)/divisions) + min_thetag
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)
endif
;####path_re
if allplots eq 2 then begin
          lvl_divisions = 257/5.
          increment = (3)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(3/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
endif
if allplots eq 3 then begin
;####ffce
          lvl_divisions = 257/5.
          increment = (1)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(1/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
endif
if allplots eq 4 then begin
;####refractive index
          lvl_divisions = 257/5.
          increment = (50)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(50/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
endif
if allplots eq 5 then begin
;####gendrin angle          
          lvl_divisions = 257/5.
          increment = (max_gen-min_gen)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_gen
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_gen-min_gen)/divisions) + min_gen
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
endif
if allplots eq 6 then begin
;####resonance cone angle
          divisions = 10.
          lvl_divisions = 257/5.
          increment = (max_res-min_res)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment + min_res          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*((max_res-min_res)/divisions) + min_res
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
endif
for sc=0,3 do begin
	for ws_s = 0,1 do begin
                uf = where(unique_freqs(*,sc) ne 0.)
		for freq=0,n_elements(uf)-1 do begin
		set_plot,'ps'
		!p.font=0
if allplots eq 0 then begin
if ws_s eq 1 then device,filename='theta_k_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='theta_k_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 1 then begin
if ws_s eq 1 then device,filename='theta_g_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='theta_g_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 2 then begin
if ws_s eq 1 then device,filename='pathre_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='pathre_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 3 then begin
if ws_s eq 1 then device,filename='ffce_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='ffce_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 4 then begin
if ws_s eq 1 then device,filename='rdx_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='rdx_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 5 then begin
if ws_s eq 1 then device,filename='tgn_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='tgn_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif
if allplots eq 6 then begin
if ws_s eq 1 then device,filename='trs_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
if ws_s eq 0 then device,filename='trs_ws_c' + strtrim(sc+1,2) + '_f=' + strtrim(unique_freqs(uf(freq),sc),2) + '.ps',bits=8,/color
endif

		if allplots eq 0 then arrtmp = theta_k(*,*,uf(freq),sc)
		if allplots eq 1 then arrtmp = theta_g(*,*,uf(freq),sc)
		if allplots eq 2 then arrtmp = pathre(*,*,uf(freq),sc)
		if allplots eq 3 then arrtmp = ffce(*,*,uf(freq),sc)
		if allplots eq 4 then arrtmp = rdx(*,*,uf(freq),sc)
		if allplots eq 5 then arrtmp = tgn(*,*,uf(freq),sc)
		if allplots eq 6 then arrtmp = trs(*,*,uf(freq),sc)

		if ws_s eq 0 then plot_array = (-1)*(not_keepers_ws(*,*)-1)*arrtmp
		if ws_s eq 1 then plot_array = (-1)*(not_keepers_source(*,*)-1)*arrtmp
		
		tmp = where(plot_array eq 0.)
		plot_array(tmp) = -10000.
		nelem = n_elements(where(plot_array ne -10000.))
		nelem = strtrim(nelem,2)
		ytitle = 'Cartesian z - Along B-Field for ' + nelem + ' points'
		arrtmp = tgn(*,*,uf(freq),sc)
		if allplots eq 0 then where_gen = where((plot_array(*,*) ge (arrtmp(*,*) - tgn_pm_delta)) and (plot_array(*,*) le (arrtmp(*,*) + tgn_pm_delta)) and (plot_array(*,*) ne 0.) and (plot_array(*,*) ne -10000.))
		if allplots eq 0 then if where_gen(0) ne -1 then plot_array(where_gen) = max_thetak


		contour,plot_array,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.2,xtitle='Cartesian x - Earth Radii   Gendrin angle +/- ' + strtrim(tgn_pm_delta,2) + ' degrees',ytitle=ytitle,xstyle=1,ystyle=1,color=257

                plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
                COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],color=257

                !p.font = -1             ;change font back so that it can be viewed correctly on screen
                if !d.name eq 'ps' then device,/close
                set_plot,'x'

		endfor ;freq
		freq=0
	endfor ;ws_s
	ws_s=0
endfor ;sc
sc=0
endfor ;allplots
end

pro plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
  color=200

  oplot,x1,y1,psym=2,symsize=0.1,color=color & oplot,x1,y2,psym=2,symsize=0.1,color=color & oplot,x1,y3,psym=2,symsize=0.1,color=color 
  oplot,x1,y4,psym=2,symsize=0.1,color=color & oplot,x1,y5,psym=2,symsize=0.1,color=color & oplot,x1,y10,psym=2,symsize=0.1,color=color
  oplot,x1,y20,psym=2,symsize=0.1,color=color & oplot,x1,y30,psym=2,symsize=0.1,color=color & oplot,x1,y40,psym=2,symsize=0.1,color=color
  oplot,x1,y50,psym=2,symsize=0.1,color=color & oplot,x1,y60,psym=2,symsize=0.1,color=color & oplot,x1,yn2,psym=2,symsize=0.1,color=color
  oplot,x1,yn3,psym=2,symsize=0.1,color=color & oplot,x1,yn4,psym=2,symsize=0.1,color=color & oplot,x1,yn5,psym=2,symsize=0.1,color=color
  oplot,x1,yn10,psym=2,symsize=0.1,color=color & oplot,x1,yn20,psym=2,symsize=0.1,color=color & oplot,x1,yn30,psym=2,symsize=0.1,color=color
  oplot,x1,yn40,psym=2,symsize=0.1,color=color & oplot,x1,yn50,psym=2,symsize=0.1,color=color & oplot,x1,yn60,psym=2,symsize=0.1,color=color
  oplot,x1,yn1,psym=2,symsize=0.1,color=color 

;  OPLOT,3.3*x_ps,y_ps,color=color  ;PLASMASPHERE
  OPLOT,4*x_ps,4*y_ps,color=color ;L=4
  oplot,5*x_ps,5*y_ps,color=color ;L=5
  oplot,6*x_ps,6*y_ps,color=color ;L=6
end