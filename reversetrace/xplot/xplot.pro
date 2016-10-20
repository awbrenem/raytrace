;The
;cross-correlation plot, however, takes into account the negative
;values. (sc2 - sc1) is the order as seen from Ivars correlation plots.
;I have verified this by manually testing individual rays. The results
;are shown in the second lab manual on pg. 14.
;The time differences in this program are always Scy-Scx where x < y.
;x would be the first file read in and y the second. I have verified
;this by checking how I create the tt3 files for this program. In
;reverse.pro for the mastertiming array, which becomes the
;plot5500.dat1 file, for ex., the timing is always Scno_2 - Scno_1, 
;where no_2 and no_1 are found in the reverseinput file. 

;Always load in frequencies from low to high in all input programs and
;this program just to be safe. 

;###### TEST RUN OF TIMING VALUES ##################
;11/07/2006. 
;I completed a test run of the cross-correation values from loading
;the tt3 files as compared to what they are from manally calculating
;the tt3 with the tt1 files. I found perfect agreement b/t the values
;(time difference for a single freq b/t 2 sc, and b/t the max and min
;freq on the cross-correlation plot) obtained at a test point from
;cc12, and the values obtained from the tt1 files c1 and c2 by manual
;subtraction. 
;####################################################

pro xplot_event, event

  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs

  widget_control,event.id,get_uvalue=eventid
  widget_control,event.top,get_uvalue=info
  CASE eventid OF
      'cycle' : BEGIN     
          unique_freqs = fltarr(6,4)  ;six possible freqs for each of the four sc
          info.cyclebool = 'yes'
          for j=0,5 do begin
              pseudoEvent = {widget_button,id:info.findfile,top:event.top,handler:0L,select:1}
              widget_control,info.findfile,send_event=pseudoevent

              pseudoEvent = {widget_button,id:info.readinfiles,top:event.top,handler:0L,select:1}
              widget_control,info.readinfiles,send_event=pseudoevent

              for i=0,9 do begin
                  widget_control,info.fillwindow_diff,sensitive=1         
                  pseudoEvent = {WIDGET_BUTTON, ID:info.fillwindow_diff, TOP:event.top, $
                                 HANDLER:0L, SELECT:1}
                  Widget_Control, info.fillwindow_diff, Send_Event=pseudoEvent
              endfor                                          
          endfor  ;for each sc pair
      END
      'findfile' : BEGIN 
          if info.cyclebool eq 'yes' then fileindexx=0
          filepath=''
          filepath = dialog_pickfile(path='/data/awb/',filter='*.dat1',/multiple_files)
          for w=0,n_elements(filepath)-1 do begin

          if(filepath(w) NE '') then begin
              filestruct.fullpath(fileindexx) = filepath(w)

              tmp = 0
              pos = fltarr(20)
              i = 0
              filetmp = filepath(w)
              tmp = strpos(filetmp,'/')
              while(tmp NE -1) do begin
                  pos(i) = tmp
                  tmp = strpos(filetmp,'/',tmp + 1)
                  i=i+1
              endwhile
              tmp = where(pos NE 0.)
              print,'tmp: ', tmp
              if(tmp(0) NE -1) then begin
                  pos2 = fltarr(n_elements(tmp))
                  pos2 = pos(tmp) ;pos2 contains the locations of all the '/' in the entire filename
                  tmp = n_elements(pos2)
;##############
                  n=(pos2(tmp-1)-1) - (pos2(tmp-2)+1) + 1
                  finaldir = strmid(filepath(w),pos2(tmp-2)+1,n) ;ex. 'tt3-cc12', 'tt1-c1'

                  strend = strlen(finaldir)
                  strdash=strpos(finaldir,'-')
                  scgoo = strmid(finaldir,strdash+1,strend-strdash+1) 
;##############
                  filename=strmid(filepath(w),pos2(tmp-1) + 1)
                  filestruct.filename(fileindexx) = filename ;filename
                  filestruct.sc(fileindexx) = scgoo
;###############          
                  tt_tmp = strmid(filepath(w),pos2(tmp - 2) + 1,3) 
                  filestruct.tt(fileindexx) = tt_tmp
;##############
                  f1 = strmid(filename,4,4)
                  f2 = strmid(filename,9,4)
                  filestruct.freq(fileindexx) = f1
              endif
;##############
;set button names to the names of the files selected     
              
              goo = strmid(filestruct.filename(fileindexx),4,5) ;freq
              name = goo + '-' + scgoo

              if(fileindexx EQ 0) then begin 
                  widget_control,info.b11,set_value=name,sensitive=1
                  widget_control,info.c1,sensitive=1
                  widget_control,info.d1,sensitive=1
                  widget_control,info.e1,sensitive=1
                  widget_control,info.f1,sensitive=1
              endif
              if(fileindexx EQ 1) then begin
                  widget_control,info.b22,set_value=name,sensitive=1
                  widget_control,info.c2,sensitive=1
                  widget_control,info.d2,sensitive=1
                  widget_control,info.e2,sensitive=1
                  widget_control,info.f2,sensitive=1
              endif
              if(fileindexx EQ 2) then begin
                  widget_control,info.b33,set_value=name,sensitive=1 
                  widget_control,info.c3,sensitive=1
                  widget_control,info.d3,sensitive=1
                  widget_control,info.e3,sensitive=1
                  widget_control,info.f3,sensitive=1
              endif
              if(fileindexx EQ 3) then begin
                  widget_control,info.b44,set_value=name,sensitive=1 
                  widget_control,info.c4,sensitive=1
                  widget_control,info.d4,sensitive=1
                  widget_control,info.e4,sensitive=1
                  widget_control,info.f4,sensitive=1
              endif
              if(fileindexx EQ 4) then begin
                  widget_control,info.b55,set_value=name,sensitive=1 
                  widget_control,info.c5,sensitive=1
                  widget_control,info.d5,sensitive=1
                  widget_control,info.e5,sensitive=1
                  widget_control,info.f5,sensitive=1
              endif
              if(fileindexx EQ 5) then begin
                  widget_control,info.b66,set_value=name,sensitive=1 
                  widget_control,info.c6,sensitive=1
                  widget_control,info.d6,sensitive=1
                  widget_control,info.e6,sensitive=1
                  widget_control,info.f6,sensitive=1
              endif
              if(fileindexx EQ 6) then begin
                  widget_control,info.b77,set_value=name,sensitive=1 
                  widget_control,info.c7,sensitive=1
                  widget_control,info.d7,sensitive=1
                  widget_control,info.e7,sensitive=1
                  widget_control,info.f7,sensitive=1
              endif
              if(fileindexx EQ 7) then begin
                  widget_control,info.b88,set_value=name,sensitive=1
                  widget_control,info.c8,sensitive=1
                  widget_control,info.d8,sensitive=1
                  widget_control,info.e8,sensitive=1
                  widget_control,info.f8,sensitive=1
              endif

              fileindexx = fileindexx + 1
              widget_control,info.readinfiles,sensitive=1 ;activate button now that there are files to read in.
          endif  
      endfor
      END
;%%%%%%%%%%%%%%%%
      'readinfiles' : BEGIN
          i=0
          while(i LT 8) do begin
              if(filestruct.filename(i) NE '') AND (filestruct.read_in(i) EQ 'empty') then begin
;if there is a file that has not been read in then read it in
                  filestruct.read_in(i) = 'no'
              endif
              i=i+1
          endwhile
          
          widget_control,info.readinfiles,/hourglass
          print,'READING IN ALL THE SELECTED FILES...PLEASE WAIT'
          read_files
          foo = where(filestruct.read_in EQ 'no',count)
          if(count NE 0) then filestruct.read_in(foo) = 'yes'

          if(filestruct.filename(0) NE '') then widget_control,info.b1,set_button=1 
          if(filestruct.filename(1) NE '') then widget_control,info.b2,set_button=1 
          if(filestruct.filename(2) NE '') then widget_control,info.b3,set_button=1 
          if(filestruct.filename(3) NE '') then widget_control,info.b4,set_button=1 
          if(filestruct.filename(4) NE '') then widget_control,info.b5,set_button=1 
          if(filestruct.filename(5) NE '') then widget_control,info.b6,set_button=1 
          if(filestruct.filename(6) NE '') then widget_control,info.b7,set_button=1
          if(filestruct.filename(7) NE '') then widget_control,info.b8,set_button=1 
          print,'DONE READING IN SELECTED FILES'
      END

      'b11' : begin
          if(event.select EQ 1) then filestruct.plot(0) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(0) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
              print,'string: ',string
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b22' : begin
          if(event.select EQ 1) then filestruct.plot(1) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(1) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b33' : begin
          if(event.select EQ 1) then filestruct.plot(2) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(2) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b44' : begin
          if(event.select EQ 1) then filestruct.plot(3) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(3) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b55' : begin
          if(event.select EQ 1) then filestruct.plot(4) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(4) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b66' : begin
          if(event.select EQ 1) then filestruct.plot(5) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(5) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b77' : begin
          if(event.select EQ 1) then filestruct.plot(6) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(6) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end
      'b88' : begin
          if(event.select EQ 1) then filestruct.plot(7) = 'yes' ;click to add
          if(event.select EQ 0) then filestruct.plot(7) = 'no'  ;click to remove file
          tmp = where(filestruct.plot EQ 'yes')
          if(tmp(0) NE -1) then begin
              goo = strmid(filestruct.filename(tmp),4,4)
              goosc = filestruct.sc(tmp)
              string = goo + ' Hz-' + goosc
          endif
          widget_control,info.make_plots,sensitive=1
          widget_control,info.make_diff_plot,sensitive=1
          tmp = where(filestruct.plot NE 'no')
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow,sensitive=0
          if(n_elements(tmp) EQ 1) then widget_control,info.fillwindow_diff,sensitive=1
          if(n_elements(tmp) NE 1) then widget_control,info.fillwindow_diff,sensitive=0
      end

      'includerays' : begin    ;push to include rays
          if(event.select EQ 1) then info.include_rays = 'yes'
          if(event.select EQ 0) then info.include_rays = 'no'
      end
      'originalzoom' : begin
          widget_control,info.xmin,set_value=limits(0)
          widget_control,info.xmax,set_value=limits(2)
          widget_control,info.ymin,set_value=limits(1)
          widget_control,info.ymax,set_value=limits(3)

          info.xsize_px = info.plotwin_xsize
          info.ysize_px = info.plotwin_ysize
      end
;%%%%%%%%%%%%%%%%
      'corr_time' : BEGIN
          info.cr_time_max = event.value
      END
;%%%%%%%%%%%%%%%%
      'corr_time2' : BEGIN
          info.cr_time_min = event.value
      END
;%%%%%%%%%%%%%%%%
      'base_time' : BEGIN
          info.base_time = event.value
      END
;%%%%%%%%%%%%%%%%
      'filter_to_firstfreq' : BEGIN
          if(event.select EQ 1) then info.filter_to_firstfreq = 'yes'
          if(event.select EQ 0) then info.filter_to_firstfreq = 'no'
      END
;%%%%%%%%%%%%%%%%
      'pm_firsttime' : BEGIN
          tmp = event.value
          info.pm_firsttime = tmp
          print,info.pm_firsttime, ' pm firsttime'
          print,tmp, ' tmp'
      END
;%%%%%%%%%%%%%%%%
;if x and y coord are entered then calculate all the fields.
      'xcoord' : BEGIN
          widget_control,info.xcoord,get_value=x
          widget_control,info.ycoord,get_value=y
          x=float(x)
          y=float(y)
          r = sqrt(x^2 + y^2)
          alt = 6370.*r - 6370.
          lat = atan(y,x)
          lat = lat*180./!pi ;convert back to degrees for display

          widget_control,info.xcoord,set_value=x
          widget_control,info.rval,set_value=r
          widget_control,info.alt,set_value=alt
          widget_control,info.lat,set_value=lat 
      END
      'ycoord' : BEGIN
          widget_control,info.xcoord,get_value=x
          widget_control,info.ycoord,get_value=y
          x=float(x)
          y=float(y)
          r = sqrt(x^2 + y^2)
          alt = 6370.*r - 6370.
          lat = atan(y,x)
          lat = lat*180./!pi
          widget_control,info.rval,set_value=r
          widget_control,info.alt,set_value=alt
          widget_control,info.lat,set_value=lat          
      END
;%%%%%%%%%%%%%%%%
;if rval and lat, or alt and lat are entered calculate all the fields.
      'rval' : BEGIN
          widget_control,info.rval,get_value=r
          widget_control,info.lat,get_value=lat
          r = float(r)
          lat = float(lat)
          lat = lat*!pi/180. ;must be in radians for calculation
          x = r*cos(lat)
          y = r*sin(lat)
          alt = 6370.*r - 6370.
          widget_control,info.xcoord,set_value=x
          widget_control,info.ycoord,set_value=y
          widget_control,info.alt,set_value=alt
      END
      'lat' : BEGIN
          widget_control,info.rval,get_value=r
          widget_control,info.lat,get_value=lat
          r = float(r)
          lat = float(lat)
          lat = lat*!pi/180.
          x = r*cos(lat)
          y = r*sin(lat)
          widget_control,info.xcoord,set_value=x
          widget_control,info.ycoord,set_value=y
      END
      'alt' : BEGIN
          widget_control,info.alt,get_value=alt
          widget_control,info.lat,get_value=lat
          lat = float(lat)
          alt = float(alt)
          lat = lat*!pi/180.
          r = alt/6370.
          x = r*cos(lat)
          y = r*sin(lat)
          widget_control,info.xcoord,set_value=x
          widget_control,info.ycoord,set_value=y
          widget_control,info.rval,set_value=r
      END
;%%%%%%%%%%%%%%%%
;fills window with regular plots
      'make_plots' : BEGIN
          if(info.ps1 EQ 'no') then begin

              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw1,get_value=draw1
              wset,draw1
          endif

          widget_control,info.originalzoom,sensitive=1
          regular_or_diff = 'regular'
          sp_offset = 0  ;single plot offset (not applicable here)
          one_plot = 'no'

          widget_control,info.make_plots,/hourglass
          widget_control,info.xmin,get_value = xmin
          widget_control,info.xmax,get_value = xmax
          widget_control,info.ymin,get_value = ymin
          widget_control,info.ymax,get_value = ymax
          
          plot_pro,info.draw1,info.cb_time,one_plot,xmin,xmax,ymin,ymax,info.include_rays,sp_offset,regular_or_diff,info.ps1
          PRINT,'XXXXXXXXXXXXXXXX'
          PRINT,'DONE MAKING PLOTS'
          PRINT,'XXXXXXXXXXXXXXXX'
      END
;%%%%%%%%%%%%%%%%
      'fillwindow' : BEGIN
          if(info.ps1 EQ 'no') then begin
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw1,get_value=draw1
              wset,draw1
          endif

          widget_control,info.originalzoom,sensitive=1 
          one_plot = 'yes'
          regular_or_diff = 'regular'
          widget_control,info.make_plots,/hourglass
          widget_control,info.xmin,get_value = xmin
          widget_control,info.xmax,get_value = xmax
          widget_control,info.ymin,get_value = ymin
          widget_control,info.ymax,get_value = ymax
          tmp_include_rays = 'no'
          tmp = where(filestruct.plot EQ 'yes') ;only one file can be selected at this point 
          sp_offset = tmp  ;single plot offset. Allows me to select the right element out of raytimes
          plot_pro,info.draw1,info.cb_time,one_plot,xmin,xmax,ymin,ymax,tmp_include_rays,sp_offset,regular_or_diff,info.ps1
          PRINT,'XXXXXXXXXXXXXXXX'
          PRINT,'DONE MAKING PLOTS'
          PRINT,'XXXXXXXXXXXXXXXX'

          widget_control,info.make_corr,sensitive = 1
      END
;%%%%%%%%%%%%%%%%
      'fillwindow_diff' : BEGIN
          if(info.ps1 EQ 'no') then begin
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw1,get_value=draw1
              wset,draw1
          endif
          widget_control,info.originalzoom,sensitive=1
          one_plot = 'yes'
          regular_or_diff = 'diff'

          widget_control,info.make_plots,/hourglass
          widget_control,info.xmin,get_value = xmin
          widget_control,info.xmax,get_value = xmax
          widget_control,info.ymin,get_value = ymin
          widget_control,info.ymax,get_value = ymax
          tmp_include_rays = 'no'
          tmp = where(filestruct.plot EQ 'yes')
          sp_offset = tmp  ;single plot offset. Allows me to select the right element out of raytimes
          plot_pro,info.draw1,info.cb_time,one_plot,xmin,xmax,ymin,ymax,tmp_include_rays,sp_offset,regular_or_diff,info.ps1
          PRINT,'XXXXXXXXXXXXXXXX'
          PRINT,'DONE MAKING PLOTS'
          PRINT,'XXXXXXXXXXXXXXXX'
          widget_control,info.make_corr,sensitive = 1
      END

      'other' : BEGIN
          if(info.ps1 EQ 'no') then begin
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw1,get_value=draw1
              wset,draw1
          endif
          widget_control,info.originalzoom,sensitive=1
          one_plot = 'yes'
          regular_or_diff = 'other'

          widget_control,info.make_plots,/hourglass
          widget_control,info.xmin,get_value = xmin
          widget_control,info.xmax,get_value = xmax
          widget_control,info.ymin,get_value = ymin
          widget_control,info.ymax,get_value = ymax

          tmp_include_rays = 'no'
          tmp = where(filestruct.plot EQ 'yes')
          sp_offset = tmp  ;single plot offset. Allows me to select the right element out of raytimes
          plot_pro,info.draw1,info.cb_time,one_plot,xmin,xmax,ymin,ymax,tmp_include_rays,sp_offset,regular_or_diff,info.ps1
          PRINT,'XXXXXXXXXXXXXXXX'
          PRINT,'DONE MAKING PLOTS'
          PRINT,'XXXXXXXXXXXXXXXX'
          widget_control,info.make_corr,sensitive = 1
      END
;%%%%%%%%%%%%%%%%%      
      'make_diff_plot' : BEGIN       
          if(info.ps1 EQ 'no') then begin
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw1,get_value=draw1
              wset,draw1
          endif
          widget_control,info.originalzoom,sensitive=1
          sp_offset = 0         ;single plot offset (not applicable here)
          regular_or_diff = 'diff'
          one_plot = 'no'

          widget_control,info.make_plots,/hourglass
          widget_control,info.xmin,get_value = xmin
          widget_control,info.xmax,get_value = xmax
          widget_control,info.ymin,get_value = ymin
          widget_control,info.ymax,get_value = ymax
          
          plot_pro,info.draw1,info.cb_time,one_plot,xmin,xmax,ymin,ymax,info.include_rays,sp_offset,regular_or_diff,info.ps1
          PRINT,'XXXXXXXXXXXXXXXX'
          PRINT,'DONE MAKING PLOTS'
          PRINT,'XXXXXXXXXXXXXXXX'                            
      end
;%%%%%%%%%%%%%%%%
      'make_corr' : BEGIN
          if(info.ps2 EQ 'no') then begin
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              widget_control,info.draw2,get_value=draw2
              wset,draw2
              erase
          endif
          widget_control,info.make_corr,/hourglass
         ;these are from reverse.pro and represent the absolute limits to the plot, not just the visible limits which are xmin and xmax
          xminimum=limits(0)
          xmaximum=limits(2)
          yminimum=limits(1)
          ymaximum=limits(3)

          widget_control,info.xcoord,get_value = xplot_val
          widget_control,info.ycoord,get_value = zplot_val  ;coordinates of mouse click on plot
          widget_control,info.xmin_corr,get_value = xmin_corr
          widget_control,info.xmax_corr,get_value = xmax_corr
          widget_control,info.ymin_corr,get_value = ymin_corr
          widget_control,info.ymax_corr,get_value = ymax_corr
          alt = info.alt
          rval = info.rval
          lat = info.lat

          corr_plot,xmaximum,xminimum,ymaximum,yminimum,xplot_val,zplot_val,info.draw2,info.ps2,alt,rval,lat,info.cr_time_max,info.cr_time_min,info.base_time,info.filter_to_firstfreq,xmin_corr,xmax_corr,ymin_corr,ymax_corr,info.pm_firsttime
      END
;%%%%%%%%%%%%%%%%%
      'draw1' : BEGIN

          print,event.type, 'event.type'
         
          if(event.type EQ 0) then begin     ;for button press
              xpix_pos = event.x  ;scaled to a window that is plotwin_xsize*plotwin_ysize
              ypix_pos = event.y     ;relative to lower left corner (0,0)     

              widget_control,info.xmin,get_value = xmin
              widget_control,info.xmax,get_value = xmax
              widget_control,info.ymin,get_value = ymin
              widget_control,info.ymax,get_value = ymax

              xmin = double(xmin(0))  ;I must do this because "get_value = xmin" is a string
              xmax = double(xmax(0))
              ymin = double(ymin(0))
              ymax = double(ymax(0))      

              xcoord = xmin + xpix_pos*(xmax - xmin)/info.plotwin_xsize
              ycoord = ymin + ypix_pos*(ymax - ymin)/info.plotwin_ysize
      
              rval = sqrt(xcoord^2 + ycoord^2)
              alt = 6370 * rval
              lat = atan(ycoord,xcoord)
              lat = lat*180./!pi
              
              widget_control,info.xcoord,set_value = xcoord
              widget_control,info.ycoord,set_value = ycoord
              widget_control,info.rval,set_value = rval
              widget_control,info.alt,set_value =  alt
              widget_control,info.lat,set_value = lat
          endif

          if(event.type EQ 1) then begin  ;for button release
              xpix_pos2 = event.x
              ypix_pos2 = event.y

              if(xpix_pos2 NE xpix_pos) AND (ypix_pos2 NE ypix_pos) then begin  
        ;lets zoom in if mouse has been dragged.                   

                  widget_control,info.xmin,get_value = xmin
                  widget_control,info.xmax,get_value = xmax
                  widget_control,info.ymin,get_value = ymin
                  widget_control,info.ymax,get_value = ymax

                  xmin_tmp = double(xmin(0)) ;i must do this because "get_value = xmin" is a string
                  xmax_tmp = double(xmax(0))
                  ymin_tmp = double(ymin(0))
                  ymax_tmp = double(ymax(0))        
                 
                  xpix_min = xpix_pos2 < xpix_pos
                  ypix_min = ypix_pos2 < ypix_pos
                  xpix_max = xpix_pos2 > xpix_pos
                  ypix_max = ypix_pos2 > ypix_pos                  

                  xmin_adj = xpix_min*(xmax_tmp-xmin_tmp)/info.plotwin_xsize
                  xmax_adj = xpix_max*(xmax_tmp-xmin_tmp)/info.plotwin_xsize
                  ymin_adj = ypix_min*(ymax_tmp-ymin_tmp)/info.plotwin_ysize
                  ymax_adj = ypix_max*(ymax_tmp-ymin_tmp)/info.plotwin_ysize

                  xmin = xmin_tmp + xmin_adj
                  xmax = xmin_tmp + xmax_adj
                  ymin = ymin_tmp + ymin_adj
                  ymax = ymin_tmp + ymax_adj

                  info.xsize_px = abs(xmax - xmin)*(xpix_max)/xmax_adj
                  info.ysize_px = abs(ymax - ymin)*(ypix_max)/ymax_adj       

                  xmin = string(xmin)
                  xmax = string(xmax)
                  ymin = string(ymin)
                  ymax = string(ymax)
                 
                  widget_control,info.xmin,set_value = xmin
                  widget_control,info.xmax,set_value = xmax
                  widget_control,info.ymin,set_value = ymin
                  widget_control,info.ymax,set_value = ymax                           
              endif                        
            
              if(xpix_pos2 eq xpix_pos) AND (ypix_pos2 eq ypix_pos) then begin
                  if info.cutoff_index0 ne 100. then begin                                            
                      xminimum=limits(0)
                      xmaximum=limits(2)
                      yminimum=limits(1)
                      ymaximum=limits(3)
                      widget_control,info.xcoord,get_value = xplot_val
                      widget_control,info.ycoord,get_value = zplot_val ;coordinates of mouse click on plot
                     
                      xgrid_pos = (gridspac-1)*(xplot_val - xminimum)/(xmaximum - xminimum)
                      zgrid_pos = (gridspac-1)*(zplot_val - yminimum)/(ymaximum - yminimum) ;coordinates of the n_names*gridspac x gridspac grid
                      xgrid_pos = round(xgrid_pos)
                      zgrid_pos = round(zgrid_pos)
                     
                      oplot,xplot_val,zplot_val,psym=1                     

                      if info.sc_toggle eq 'sca' then begin
                          tk = theta_k_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tg = theta_g_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          pRe = path_re_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          f_fce = f_fce_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          rdx = refndx_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tgen = theta_gen_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tres = theta_res_sca(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          print,tk, ' tk'
                          print,tg, ' tg'
                          
                      endif
                      
                      if info.sc_toggle eq 'scb' then begin
                          tk = theta_k_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tg = theta_g_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          pRe = path_re_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          f_fce = f_fce_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          rdx = refndx_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tgen = theta_gen_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          tres = theta_res_scb(xgrid_pos,zgrid_pos,info.cutoff_index0)
                          print,tk, ' tk'
                          print,tg, ' tg'
                          
                      endif
                      if info.ps1 eq 'yes' then begin
                          set_plot,'ps'
                          !p.font = 0                       
                          device,filename = 'rayvalues.ps' ;,bits=8,/color
                      endif                   
                      
                      if info.ps1 eq 'no' then begin
                          widget_control,info.draw2,get_value=draw2
                          wset,draw2
                          erase
                      endif
                      
                      sc = filestruct.sc(info.cutoff_index0)
                      if timetype eq 3. then begin
                          if info.sc_toggle eq 'sca' then sc = 'c' + strmid(sc,2,1)
                          if info.sc_toggle eq 'scb' then sc = 'c' + strmid(sc,3,1)
                      endif
                      widget_control,info.xcoord,get_value=xcoord
                      widget_control,info.ycoord,get_value=ycoord
                      widget_control,info.lat,get_value=lat
                      widget_control,info.rval,get_value=rval                      

                      cs = 1.1
                      if info.ps1 eq 'no' then color=255
                      if info.ps1 eq 'yes' then color = 10

                      xyouts,0.1,0.80,sc,color=color,/normal,charsize=cs
                      xyouts,0.5,0.80,'freq = ' + strtrim(string(filestruct.freq(info.cutoff_index0)),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.70,'x (Mer) = ' + strtrim(string(xcoord),2),color=color,/normal,charsize=cs
                      xyouts,0.5,0.70,'z (Mer) = ' + strtrim(string(ycoord),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.60,'lat = ' + strtrim(string(lat),2),color=color,/normal,charsize=cs
                      xyouts,0.5,0.60,'rval = ' + strtrim(string(rval),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.50,'theta k = ' + strtrim(string(tk),2),color=color,/normal,charsize=cs
                      xyouts,0.5,0.50,'theta group = ' + strtrim(string(tg),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.40,'raypath dist (RE) = ' + strtrim(string(pRe),2),color=color,/normal,charsize=cs
                      xyouts,0.5,0.40,'f/fce = ' + strtrim(string(f_fce),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.30,'ref index = ' + strtrim(string(rdx),2),color=color,/normal,charsize=cs
                      xyouts,0.5,0.30,'theta Gendrin = ' + strtrim(string(tgen),2),color=color,/normal,charsize=cs
                      xyouts,0.1,0.20,'theta res = ' + strtrim(string(tres),2),color=color,/normal,charsize=cs                                       
                      if info.ps1 eq 'yes' then begin
                          device,/close
                          set_plot,'x'
                      endif
                      if info.ps1 eq 'no' then begin
                          widget_control,info.draw1,get_value=draw1
                          wset,draw1
                      endif
                  endif
                  widget_control,info.print_ps1, set_button=0
                  info.ps1 = 'no'                                  
              endif
          endif
      END
;%%%%%%%%%%%%%%%%
      'xmin' : BEGIN
          xmin = event.value        
          widget_control,info.xmin,set_value=xmin        
      END
      'xmax' : BEGIN
          xmax = event.value
          widget_control,info.xmax,set_value=xmax
      END
      'ymin' : BEGIN
          ymin = event.value
          widget_control,info.ymin,set_value=ymin
      END
      'ymax' : BEGIN
          ymax = event.value
          widget_control,info.ymax,set_value=ymax
      END

      'print_ps1' : BEGIN
          if(event.select EQ 1) then begin 
              info.ps1 = 'yes'
          endif
          if(event.select EQ 0) then begin 
              info.ps1 = 'no'
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              print,!d.window
          endif
      END
      
      'print_ps2' : BEGIN
          if(event.select EQ 1) then begin
              info.ps2 = 'yes'
              set_plot,'ps'
              device,filename='corr_plot.ps',bits=8,/color
              print,!d.window
          endif
          if(event.select EQ 0) then begin
              info.ps2 = 'no'
              if(!d.window EQ -1) then begin
                  device,/close
                  set_plot,'x'
              endif
              print,!d.window
          endif
      END

      'xmin_corr' : Begin
          xmin_corr = event.value
          widget_control,info.xmin_corr,set_value=xmin_corr
      END
      'xmax_corr' : Begin
          xmax_corr = event.value
          widget_control,info.xmax_corr,set_value=xmax_corr
      END
      'ymin_corr' : Begin
          ymin_corr = event.value
          widget_control,info.ymin_corr,set_value=ymin_corr
      END
      'ymax_corr' : Begin
          ymax_corr = event.value
          widget_control,info.ymax_corr,set_value=ymax_corr
      END

      'c1' : BEGIN          
          info.selected_freq0 = 0
          widget_control,info.d1,sensitive=0        
          widget_control,info.d2,sensitive=1
          widget_control,info.d3,sensitive=1
          widget_control,info.d4,sensitive=1
          widget_control,info.d5,sensitive=1
          widget_control,info.d6,sensitive=1
          widget_control,info.d1,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
          
      END
      
      'c2' : BEGIN
          info.selected_freq0 = 1
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=1
          widget_control,info.d4,sensitive=1
          widget_control,info.d5,sensitive=1
          widget_control,info.d6,sensitive=1
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c3' : BEGIN
          info.selected_freq0 = 2
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=0
          widget_control,info.d4,sensitive=1
          widget_control,info.d5,sensitive=1
          widget_control,info.d6,sensitive=1
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          widget_control,info.d3,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c4' : BEGIN
          info.selected_freq0 = 3
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=0
          widget_control,info.d4,sensitive=0
          widget_control,info.d5,sensitive=1
          widget_control,info.d6,sensitive=1
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          widget_control,info.d3,set_button=0
          widget_control,info.d4,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c5' : BEGIN
          info.selected_freq0 = 4
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=0
          widget_control,info.d4,sensitive=0
          widget_control,info.d5,sensitive=0
          widget_control,info.d6,sensitive=1
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          widget_control,info.d3,set_button=0
          widget_control,info.d4,set_button=0
          widget_control,info.d5,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c6' : BEGIN
          info.selected_freq0 = 5
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=0
          widget_control,info.d4,sensitive=0
          widget_control,info.d5,sensitive=0
          widget_control,info.d6,sensitive=0
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          widget_control,info.d3,set_button=0
          widget_control,info.d4,set_button=0
          widget_control,info.d5,set_button=0
          widget_control,info.d6,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c7' : BEGIN
          info.selected_freq0 = 6
          widget_control,info.d1,sensitive=0
          widget_control,info.d2,sensitive=0
          widget_control,info.d3,sensitive=0
          widget_control,info.d4,sensitive=0
          widget_control,info.d5,sensitive=0
          widget_control,info.d6,sensitive=0
          widget_control,info.d1,set_button=0
          widget_control,info.d2,set_button=0
          widget_control,info.d3,set_button=0
          widget_control,info.d4,set_button=0
          widget_control,info.d5,set_button=0
          widget_control,info.d6,set_button=0
          if info.selected_freq1 LE info.selected_freq0 then info.selected_freq1 = 0.
      END
      'c8' : BEGIN
          info.selected_freq0 = 7
      END
      'd1' : BEGIN
          info.selected_freq1 = 0
      END
      'd2' : BEGIN
          info.selected_freq1 = 1
      END
      'd3' : BEGIN
          info.selected_freq1 = 2
      END
      'd4' : BEGIN
          info.selected_freq1 = 3
      END
      'd5' : BEGIN
          info.selected_freq1 = 4
      END
      'd6' : BEGIN
          info.selected_freq1 = 5
      END
      'd7' : BEGIN
          info.selected_freq1 = 6
      END
      'd8' : BEGIN
          info.selected_freq1 = 7
      END

      'e1' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 0
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=1
          widget_control,info.f3,sensitive=1
          widget_control,info.f4,sensitive=1
          widget_control,info.f5,sensitive=1
          widget_control,info.f6,sensitive=1
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1                 
          widget_control,info.f1,set_button=0
      END
      'e2' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 1
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=1
          widget_control,info.f4,sensitive=1
          widget_control,info.f5,sensitive=1
          widget_control,info.f6,sensitive=1
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
      END
      'e3' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 2
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=1
          widget_control,info.f5,sensitive=1
          widget_control,info.f6,sensitive=1
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
      END
      'e4' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 3
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=0
          widget_control,info.f5,sensitive=1
          widget_control,info.f6,sensitive=1
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
          widget_control,info.f4,set_button=0
      END
      'e5' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 4
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=0
          widget_control,info.f5,sensitive=0
          widget_control,info.f6,sensitive=1
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
          widget_control,info.f4,set_button=0
          widget_control,info.f5,set_button=0
      END
      'e6' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 5
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=0
          widget_control,info.f5,sensitive=0
          widget_control,info.f6,sensitive=0
          widget_control,info.f7,sensitive=1
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
          widget_control,info.f4,set_button=0
          widget_control,info.f5,set_button=0
          widget_control,info.f6,set_button=0
      END
      'e7' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 6
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=0
          widget_control,info.f5,sensitive=0
          widget_control,info.f6,sensitive=0
          widget_control,info.f7,sensitive=0
          widget_control,info.f8,sensitive=1
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
          widget_control,info.f4,set_button=0
          widget_control,info.f5,set_button=0
          widget_control,info.f6,set_button=0
          widget_control,info.f7,set_button=0
      END
      'e8' : BEGIN
          if(event.select EQ 1) then info.cutoff_index0 = 7
          if(event.select EQ 0) then info.cutoff_index0 = 100
          if event.select eq 1 then begin
              info.which_cutoff_but = info.which_cutoff_but + 1
          endif
          if event.select eq 0 then begin             
              info.which_cutoff_but = info.which_cutoff_but - 1
          endif
          if info.which_cutoff_but eq 1 then widget_control,info.other,sensitive=1
          if info.which_cutoff_but ne 1 then widget_control,info.other,sensitive=0
          widget_control,info.f1,sensitive=0
          widget_control,info.f2,sensitive=0
          widget_control,info.f3,sensitive=0
          widget_control,info.f4,sensitive=0
          widget_control,info.f5,sensitive=0
          widget_control,info.f6,sensitive=0
          widget_control,info.f7,sensitive=0
          widget_control,info.f8,sensitive=0
          widget_control,info.f1,set_button=0
          widget_control,info.f2,set_button=0
          widget_control,info.f3,set_button=0
          widget_control,info.f4,set_button=0
          widget_control,info.f5,set_button=0
          widget_control,info.f6,set_button=0
          widget_control,info.f7,set_button=0
          widget_control,info.f8,set_button=0
      END

      'f1' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 0
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f2' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 1
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f3' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 2
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f4' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 3
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f5' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 4
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f6' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 5
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f7' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 6
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'f8' : BEGIN
          if(event.select EQ 1) then info.cutoff_index1 = 7
          if(event.select EQ 0) then info.cutoff_index1 = 100
      END
      'dt1' : BEGIN
          tmp = event.value
          widget_control,info.dt1,set_value=tmp
      END
      'dt2' : BEGIN
          tmp = event.value
          widget_control,info.dt2,set_value=tmp
      END
      'dt3' : BEGIN
          tmp = event.value
          widget_control,info.dt3,set_value=tmp
      END      
      'dt4' : BEGIN
          tmp = event.value
          widget_control,info.dt4,set_value=tmp
      END      
      'dt5' : BEGIN
          tmp = event.value
          widget_control,info.dt5,set_value=tmp
      END      
      'dt6' : BEGIN
          tmp = event.value
          widget_control,info.dt6,set_value=tmp
      END
      'dt7' : BEGIN
          tmp = event.value
          widget_control,info.dt7,set_value=tmp
      END
      'dt8' : BEGIN
          tmp = event.value
          widget_control,info.dt8,set_value=tmp
      END
      
      'sca_but' : BEGIN
          info.sc_toggle = 'sca'
          widget_control,info.theta_k,sensitive=1
          widget_control,info.theta_g,sensitive=1
          widget_control,info.path_re,sensitive=1
          widget_control,info.f_fce,sensitive=1
          widget_control,info.refndx,sensitive=1
          widget_control,info.theta_gen,sensitive=1
          widget_control,info.theta_res,sensitive=1
      END
      'scb_but' : BEGIN
          info.sc_toggle = 'scb'
          widget_control,info.theta_k,sensitive=1
          widget_control,info.theta_g,sensitive=1
          widget_control,info.path_re,sensitive=1
          widget_control,info.f_fce,sensitive=1
          widget_control,info.refndx,sensitive=1
          widget_control,info.theta_gen,sensitive=1
          widget_control,info.theta_res,sensitive=1
      END
      'theta_k' : BEGIN
          info.what_other_but = 'tk'
      END
      'theta_g' : BEGIN
          info.what_other_but = 'tg'
      END
      'path_re' : BEGIN
          info.what_other_but = 'pRe'
      END
      'f_fce' : BEGIN
          info.what_other_but = 'fce'
      END
      'refndx' : BEGIN
          info.what_other_but = 'rdx'
      END
      'theta_gen' : BEGIN
          info.what_other_but = 'tgn'
      END
      'theta_res' : BEGIN
          info.what_other_but = 'trs'
      END
      'plot_grd' : BEGIN
          if event.select eq 1 then info.plot_grid = 'yes'        
      END
;      'stop fill' : BEGIN
;          
;      ENDIF


  ENDCASE
  widget_control,event.top,set_uvalue = info 
end
;############################################################################
;############################################################################
;############################################################################
;############################################################################
;############################################################################
;############################################################################
;############################################################################
;############################################################################ 
pro read_files
  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs
  

  tmp = where(filestruct.read_in EQ 'no',count)
  if(count NE 0) then begin
      pathnames = strarr(n_elements(tmp))
      
      i=0
      while(i LT n_elements(tmp)) do begin
          pathnames(i) = filestruct.fullpath(tmp(i))
          i=i+1
      endwhile
    
      restore,pathnames(0)
      n_names = n_elements(pathnames)

      if timetype eq 1. then widget_control,info.scb_but,sensitive=0
      allinfo = strarr(31,n_names)
   
      names = strarr(n_elements(pathnames))
      pathtmp = strarr(n_elements(pathnames)) ;path
      filetmp = strarr(n_elements(pathnames)) ;file
      i=0
      while(i LT n_elements(pathnames)) do begin
          tmp = strpos(pathnames(i),'plot') ;returns position of p
          tmplet = strpos(pathnames(i),'.dat')
          letter = strmid(pathnames(i),tmplet-1,1) 
          if letter ne 'a' and letter ne 'b' and letter ne 'c' and letter ne 'd' and letter ne 'x' then letter = ''
          pathtmp(i) = strmid(pathnames(i),0,tmp-1)
          filetmp(i) = strmid(pathnames(i),tmp+4,4) ;everything after 'plot'
          names(i) = pathtmp(i) + '/infoarray' + filetmp(i) + letter + '.dat4'
          i=i+1
      endwhile
      
      print,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      print,'here are the infoarrays being read in: ',names
      print,'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'      

      line=''
      i=0                           
      infoarray=strarr(31)
      q=0
      
      while(q LT n_names) do begin
          openr,lun,names(q),/get_lun ;infoarray

          while(not eof(lun)) do begin
              readf,lun,line
              line=strtrim(line,2)
              infoarray(i) = line
              i=i+1
          endwhile         
          allinfo(*,q) = infoarray                             
          i=0
          q=q+1
          close,lun
          free_lun,lun
      endwhile

      gridspac = strmid(infoarray(9),16,3)
      gridspac = float(gridspac)
      raytimes = fltarr(gridspac,gridspac,n_names)    

;      allinfo = strarr(31,n_names)
      if include_details eq 'yes' then begin
          theta_k_sca = fltarr(gridspac,gridspac,n_names)         
          theta_g_sca = fltarr(gridspac,gridspac,n_names)          
          path_re_sca = fltarr(gridspac,gridspac,n_names)                  
          f_fce_sca = fltarr(gridspac,gridspac,n_names)          
          refndx_sca = fltarr(gridspac,gridspac,n_names)          
          theta_gen_sca = fltarr(gridspac,gridspac,n_names)                   
          theta_res_sca = fltarr(gridspac,gridspac,n_names)                  
          density = fltarr(gridspac,gridspac)
          if timetype eq 3 then begin
              theta_k_scb = fltarr(gridspac,gridspac,n_names)
              theta_g_scb = fltarr(gridspac,gridspac,n_names)
              path_re_scb = fltarr(gridspac,gridspac,n_names)
              f_fce_scb = fltarr(gridspac,gridspac,n_names)
              refndx_scb = fltarr(gridspac,gridspac,n_names)
              theta_gen_scb = fltarr(gridspac,gridspac,n_names)
              theta_res_scb = fltarr(gridspac,gridspac,n_names)  
          endif
      endif


      density(*,*) = dens_sc1(0:(gridspac-1),0:(gridspac-1))     
      q = 0.
      while q lt n_names do begin        
          restore,pathnames(q)
          mastertiming = mastertiming(0:(gridspac-1),0:(gridspac-1)) 

;sometimes it's created with 402 elements for some reason. The correct
;elements to remove are the last. 
          
          if include_details eq 'yes' then begin
              theta_k_sca(*,*,q) = theta_k_sc1(0:(gridspac-1),0:(gridspac-1))
              theta_g_sca(*,*,q) = theta_g_sc1(0:(gridspac-1),0:(gridspac-1))
              path_re_sca(*,*,q) = path_re_sc1(0:(gridspac-1),0:(gridspac-1))
              f_fce_sca(*,*,q) = f_fce_sc1(0:(gridspac-1),0:(gridspac-1))
              refndx_sca(*,*,q) = refndx_sc1(0:(gridspac-1),0:(gridspac-1))
              theta_gen_sca(*,*,q) = theta_gen_sc1(0:(gridspac-1),0:(gridspac-1))
              theta_res_sca(*,*,q) = theta_res_sc1(0:(gridspac-1),0:(gridspac-1))

              
              if timetype eq 3 then begin
                  theta_k_scb(*,*,q) = theta_k_sc2(0:(gridspac-1),0:(gridspac-1)) 
                  theta_g_scb(*,*,q) = theta_g_sc2(0:(gridspac-1),0:(gridspac-1))
                  path_re_scb(*,*,q) = path_re_sc2(0:(gridspac-1),0:(gridspac-1))
                  f_fce_scb(*,*,q) = f_fce_sc2(0:(gridspac-1),0:(gridspac-1))
                  refndx_scb(*,*,q) = refndx_sc2(0:(gridspac-1),0:(gridspac-1))
                  theta_gen_scb(*,*,q) = theta_gen_sc2(0:(gridspac-1),0:(gridspac-1))
                  theta_res_scb(*,*,q) = theta_res_sc2(0:(gridspac-1),0:(gridspac-1))
              endif
          endif
        
        raytimes(*,*,q) = mastertiming
        if keyword_set(xray_sc1x) eq 1 then begin
          if keyword_set(xrays1x) ne 1 then xrays1x = fltarr(2000000,n_names)      
          if keyword_set(zrays1x) ne 1 then zrays1x = fltarr(2000000,n_names)      
          tmp = n_elements(xray_sc1x)
          xrays1x(0:tmp-1,q) = xray_sc1x
          tmp = n_elements(zray_sc1x)
          zrays1x(0:tmp-1,q) = zray_sc1x
        endif
        if keyword_set(xray_sc1a) eq 1 then begin        
          if keyword_set(xrays1a) ne 1 then xrays1a = fltarr(2000000,n_names)    
          if keyword_set(zrays1a) ne 1 then zrays1a = fltarr(2000000,n_names)    
          tmp = n_elements(xray_sc1a)
          xrays1a(0:tmp-1,q) = xray_sc1a
          tmp = n_elements(zray_sc1a)
          zrays1a(0:tmp-1,q) = zray_sc1a
        endif
        if keyword_set(xray_sc1b) eq 1 then begin
          if keyword_set(xrays1b) ne 1 then xrays1b = fltarr(2000000,n_names)      
          if keyword_set(zrays1b) ne 1 then zrays1b = fltarr(2000000,n_names)     
          tmp = n_elements(xray_sc1b)
          xrays1b(0:tmp-1,q) = xray_sc1b
          tmp = n_elements(zray_sc1b)
          zrays1b(0:tmp-1,q) = zray_sc1b
        endif
        if keyword_set(xray_sc1c) eq 1 then begin
          if keyword_set(xrays1c) ne 1 then xrays1c = fltarr(2000000,n_names)      
          if keyword_set(zrays1c) ne 1 then zrays1c = fltarr(2000000,n_names)    
          tmp = n_elements(xray_sc1c)
          xrays1c(0:tmp-1,q) = xray_sc1c
          tmp = n_elements(zray_sc1c)
          zrays1c(0:tmp-1,q) = zray_sc1c
        endif

          if timetype eq 3. then begin
            if keyword_set(xray_sc2x) eq 1 then begin
            if keyword_set(xrays2x) ne 1 then xrays2x = fltarr(2000000,n_names)      
            if keyword_set(zrays2x) ne 1 then zrays2x = fltarr(2000000,n_names)  
              tmp = n_elements(xray_sc2x)
              xrays2x(0:tmp-1,q) = xray_sc2x
              tmp = n_elements(zray_sc2x)
              zrays2x(0:tmp-1,q) = zray_sc2x
            endif
            if keyword_set(xray_sc2a) eq 1 then begin
            if keyword_set(xrays2a) ne 1 then xrays2a = fltarr(2000000,n_names)      
            if keyword_set(zrays2a) ne 1 then zrays2a = fltarr(2000000,n_names)  
              tmp = n_elements(xray_sc2a)
              xrays2a(0:tmp-1,q) = xray_sc2a
              tmp = n_elements(zray_sc2a)
              zrays2a(0:tmp-1,q) = zray_sc2a
            endif
            if keyword_set(xray_sc2b) eq 1 then begin
            if keyword_set(xrays2b) ne 1 then xrays2b = fltarr(2000000,n_names)      
            if keyword_set(zrays2b) ne 1 then zrays2b = fltarr(2000000,n_names)  
              tmp = n_elements(xray_sc2b)
              xrays2b(0:tmp-1,q) = xray_sc2b
              tmp = n_elements(zray_sc2b)
              zrays2b(0:tmp-1,q) = zray_sc2b
            endif
            if keyword_set(xray_sc2c) eq 1 then begin
            if keyword_set(xrays2c) ne 1 then xrays2c = fltarr(2000000,n_names)      
            if keyword_set(zrays2c) ne 1 then zrays2c = fltarr(2000000,n_names)  
              tmp = n_elements(xray_sc2c)
              xrays2c(0:tmp-1,q) = xray_sc2c
              tmp = n_elements(zray_sc2c)
              zrays2c(0:tmp-1,q) = zray_sc2c
            endif
          endif
       
          q=q+1
      endwhile       
  endif

  widget_control,info.xmin,set_value=limits(0)
  widget_control,info.xmax,set_value=limits(2)
  widget_control,info.ymin,set_value=limits(1)
  widget_control,info.ymax,set_value=limits(3)

  widget_control,info.xmin,set_value=4.2
  widget_control,info.xmax,set_value=5.4
  widget_control,info.ymin,set_value=-1.8
  widget_control,info.ymax,set_value=1.0



end                             ;for read_files

;####################################################################################
;####################################################################################
;####################################################################################
;####################################################################################
PRO plot_image_colorbar, image, xdata, ydata, $
		position=position, xrange=xrange, $
		yrange=yrange, zrange=zrange, $
		xlog=xlog, ylog=ylog, zlog=zlog, $
		xtitle=xtitle, ytitle=ytitle, ztitle=ztitle, title=title, $
		top=top, Interp=Interp, erase=erase, $
		grey_value=grey_value, square=square, aspect=aspect, $
		noerase=noerase, offset=offset,cr_time_min=cr_time_min,cr_time_max=cr_time_max

;CHECK Keywords
;#############################
IF N_Elements(noerase) NE 1 THEN noerase=0

IF N_Elements(offset) NE 1 THEN offset=.85 ELSE BEGIN
  IF offset GT 1. THEN BEGIN
	message, 'Offset too large'
	return
  ENDIF
ENDELSE

IF N_Elements(position) EQ 0 THEN position = [0.1, 0.1, 0.9, 0.9]

IF N_Elements(zlog) NE 1 THEN zlog=0

IF N_Elements(xrange ) NE 2 THEN BEGIN
	xrange=[min(xdata),max(xdata)]
ENDIF ELSE BEGIN
	test=where(xdata GE xrange[0] and xdata LE xrange[1], count)
	IF count LE 1 THEN BEGIN
	message, 'Not enough data in X range'
	return
	ENDIF
image=image[test,*]
xdata=xdata[test]
ENDELSE


IF N_Elements(yrange) NE 2 THEN BEGIN
	yrange=[min(ydata), max(ydata)]
ENDIF ELSE BEGIN
	test=where(ydata GE yrange[0] and ydata LE yrange[1], count)
	IF count LE 1 THEN BEGIN
	message, 'Not enough data in Y range'
	return
	ENDIF
image=image[*,test]
ydata=ydata[test]
ENDELSE
	xrange=[min(xdata),max(xdata)]
	yrange=[min(ydata), max(ydata)]

IF N_Elements(zrange) NE 2 THEN BEGIN
	zrange=[min(image), max(image)]
	preset=0
	ENDIF ELSE preset=1
zrange_store=zrange

IF N_Elements(top) NE 1 THEN top=253

IF N_Elements(square) NE 1 THEN square=0
IF N_Elements(aspect) NE 1 THEN aspect=0.

IF zlog EQ 1 THEN BEGIN
      IF N_Elements(grey_value) EQ 0 then BEGIN
         good_val=where(image GT 0.)
         image[good_val] = ALOG10(image[good_val])
      ENDIF ELSE BEGIN
	 good_val=where(image NE grey_value AND image GT 0., count)
         if count GT 0 then image[good_val]=ALOG10(image[good_val])
      ENDELSE

      IF zrange[0] LE 0. THEN zrange[0]=0.000001

      zrange=ALOG10(zrange)
      zrange[0]=FIX(zrange[0])
      zrange[1]=FIX(zrange[1])+1.
ENDIF

IF noerase EQ 0 THEN plot, [0,0], [1,1], /nodata, ystyle=7, xstyle=7

s=Size(image, /Dimensions)
imgXsize=s[0]
imgYsize=s[1]

IF !P.ticklen LE 0 THEN ticklen_offset=!p.ticklen ELSE ticklen_offset=0.0

xsize = (position[2] - position[0]+ticklen_offset)*!D.X_VSize
ysize = (position[3] - position[1])*!D.Y_VSize
xstart = position[0] * !D.X_VSize
ystart = position[1] * !D.Y_VSize

IF square THEN BEGIN
  newsize=min([xsize,ysize])
  xsize=newsize
  ysize=newsize
ENDIF
;print, xsize, ysize, ysize/xsize, aspect
IF aspect NE 0. THEN BEGIN

	window_aspect=FLOAT(!D.Y_VSize)/FLOAT(!D.X_VSize)

	IF window_aspect LE aspect THEN xsize=ysize/aspect $
				ELSE ysize = xsize*aspect
		
;print, xsize, ysize, ysize/xsize, aspect, !D.X_VSize, !D.Y_VSize
;print, xstart, ystart
ENDIF

plotxmin=xstart
plotymin=ystart
plotxsize=offset*xsize
plotysize=0.92*ysize

newposition=[plotxmin/!D.X_VSize, plotymin/!D.Y_VSize, plotxsize/!D.X_VSize +position[0], plotysize/!D.Y_VSize + position[1]]

min_val=min(image)
IF N_Elements(grey_value) EQ 1 THEN BEGIN

bytimage=image

tvlct, R, G, B, /get
old_r1 = R(!d.table_size-2)
old_g1 = G(!d.table_size-2)
old_b1 = B(!d.table_size-2)
R(!d.table_size-2) = 179
G(!d.table_size-2) = 179
B(!d.table_size-2) = 179
tvlct, R, G, B

image_gr = where(image EQ grey_value, vals)
image_else = where(image NE grey_value, realvals)
IF preset NE 1 THEN BEGIN
CASE vals OF
 0:BEGIN
if TOTAL(grey_value EQ zrange[0]) then zrange[0]=min(image[image_else])
if TOTAL(grey_value EQ zrange[1]) then zrange[1]=max(image[image_else])
  END
 n_elements(image): BEGIN
   zrange[0]=0.
   zrange[1]=1.
  END
 ELSE: BEGIN
zrange[0]=min(image[image_else])
zrange[1]=max(image[image_else])
END
ENDCASE 
ENDIF

IF zlog EQ 1 then begin
  zrange[0]=FIX(zrange[0])
  zrange[1]=(FIX(zrange[1]*10.)+1.)/10.
ENDIF

IF vals NE 0 THEN bytimage[image_gr]=254
IF realvals NE 0 THEN BEGIN

bytimage[image_else]=bytscl(image[image_else],min=zrange[0], max=zrange[1], top=top, /NAN)
ENDIF

ENDIF ELSE BEGIN

bytimage=BYTSCL(image, min=zrange[0], max=zrange[1], top=top, /NAN)
ENDELSE


;Calculate Colorbar position
;############################################################################
    xbarmin=0.96*xsize + xstart
    xbarmax=0.98*xsize + xstart
    ybarmin=0.20*ysize + ystart
    ybarmax=0.80*ysize + ystart
    xbarlen=(xbarmax-xbarmin)
    ybarlen=(ybarmax-ybarmin)
    bar=intarr(2,top+1)
    for i=0,top do bar(*,i)=i
    xbar=[0,1]

;###############
;modified by aaron for use in xplot.pro

;cbposition=[xbarmin/!D.X_VSize,ybarmin/!D.Y_VSize,xbarmax/!D.X_VSize,ybarmax/!D.Y_VSize]
;print, cbposition

cbposition=[0.05,0.94,0.95,0.99]
;################

IF zlog EQ 1 then BEGIN
	z_range=10.^zrange
ENDIF ELSE z_range=zrange
    ybar = z_range


;Plot Data
;################################################################
;###############
;Modified by Aaron for xplot.pro
plotxsize=xsize
plotysize=ysize
;###############


IF !D.Name EQ 'PS' THEN BEGIN 
img_size=size(bytimage)
TV, CONGRID(bytimage, img_size[1]*1, img_size[2]*1, Interp=Interp), $
		plotxmin+1, plotymin+1, xsize=plotxsize, ysize=plotysize 

ENDIF ELSE $
TV, CONGRID(bytimage, plotxsize, plotysize, Interp=Interp), plotxmin+1, plotymin+1

;plot, xdata, ydata, /Nodata, /noerase, $
;		xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
;		xtitle=xtitle, ytitle=ytitle, title=title, $
;		position=newposition, xstyle=9, ystyle=9

;##########
;modified by Aaron for use in xplot.pro
plot, xdata, ydata, /Nodata, /noerase, $
  xrange=xrange, yrange=yrange, xlog=xlog, ylog=ylog, $
  xtitle=xtitle, ytitle=ytitle, title=title, $
  position=[0,0,1,1], xstyle=9, ystyle=9
;##########

zrange=zrange_store

divisions=10.
increment = 0.001*(cr_time_max*0.001 - cr_time_min*0.001)/divisions
lvls = findgen(divisions+1)*increment + cr_time_min*0.001
strlvls = strtrim(string(lvls*1000),2)
strlvls=strmid(strlvls,0,5)
if(lvls(0) EQ 0.) then lvls(0) = 0.000001

;Draw Colorbar

COLORBAR,position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255

;################################################################
;IF !D.Name EQ 'PS' THEN $
;  TV, bytscl(bar, top=top), xbarmin, ybarmin, xsize=xbarlen, ysize=ybarlen ELSE BEGIN
;    STOP
;    tv,congrid(bytscl(bar,top=top),xbarlen,ybarlen),xbarmin+1,ybarmin+1
;    plot,xbar,ybar,/nodata, position=cbposition,$
;      yticklen=-0.1,xstyle=4,xminor=1, yminor=0, $
;      ystyle=9, yrange = [z_range[0],z_range[1]], $
;      ytickname=replicate(' ',10), $
;      title=ztitle, ylog=zlog, charsize=.75, /noerase
;    
;    axis,yaxis=1,yticklen=-0.1,/noerase, ylog=zlog
;ENDELSE
END  ;plot_image_colorbar  (Dfanning)

;####################################################################################
;####################################################################################
;####################################################################################
;####################################################################################
;####################################################################################

pro plot_pro,draw1ID,maxval,one_plot,xmin,xmax,ymin,ymax,include_rays,sp_offset,regular_or_diff,ps
;if one_plot = 'yes' then I want a single plot to fill entire plot
;window. i.e. no colorbar or plot info.
  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs


  widget_control,info.print_ps2, set_button=0 ;make sure the 'print corr plot to ps' button is not selected
;Program will crash if it is. 
  info.ps2 = 'no'

  device,decomposed=0

;now I'll create a tmp array of the timing difference b/t the max freq
;and the min freq. to help filter 

  if info.cyclebool eq 'yes' then begin

      maxside = 0.07  ;max triangle side length
      info.filter_to_firstfreq = 'yes'
      widget_control,info.filter_butt,set_button = 1

      if info.maccyclecount eq 0 then begin  ;cc12
          info.corr_time2 = -38.5488
          info.corr_time = 52.9326
          info.base_time = 20.5195
          info.pm_firsttime = 19.5997
      endif

;      if info.maccyclecount eq 0 then begin  ;cc12
;          info.corr_time2 = -32.024
;          info.corr_time = 64.3714
;          info.base_time = 13.2655
;          info.pm_firsttime = 19.7262
;      endif

      if info.maccyclecount eq 1 then begin ;cc13
          info.corr_time2 = -23.4273
          info.corr_time = 27.6561
          info.base_time = 15.4883
          info.pm_firsttime = 11.718
      endif

;      if info.maccyclecount eq 1 then begin  ;cc13
;          info.corr_time2 = -3.8341
;          info.corr_time = 17.8261
;          info.base_time = 17.2911
;          info.pm_firsttime = 9.2167
;       endif

      if info.maccyclecount eq 2 then begin ;cc14
          info.corr_time2 = 14.1191
          info.corr_time = 52.4481
          info.base_time = 16.2008
          info.pm_firsttime = 5.4237
      endif

;      if info.maccyclecount eq 2 then begin  ;cc14
;          info.corr_time2 = 17.1213
;          info.corr_time = 44.7952
;          info.base_time = 28.7993
;          info.pm_firsttime = 23.8595
;      endif

      if info.maccyclecount eq 3 then begin ;cc23
          info.corr_time2 = -41.3672
          info.corr_time = 14.3298
          info.base_time = 3.7117
          info.pm_firsttime = 14.8672
      endif

;      if info.maccyclecount eq 3 then begin ;cc23
;          info.corr_time2 = -61.9583
;          info.corr_time = -11.5857
;          info.base_time = 9.855
;          info.pm_firsttime = 33.0845
;      endif

      if info.maccyclecount eq 4 then begin ;cc24
          info.corr_time2 = -9.9244
          info.corr_time = 13.2109
          info.base_time = 1.9819
          info.pm_firsttime = 7.0502
      endif

;      if info.maccyclecount eq 4 then begin  ;cc24
;          info.corr_time2 = -34.6895
;          info.corr_time = 22.5937
;          info.base_time = 22.2712
;          info.pm_firsttime = 25.4908
;      endif

      if info.maccyclecount eq 5 then begin ;cc34
          info.corr_time2 = -13.1072
          info.corr_time = 28.3481
          info.base_time = 4.4044
          info.pm_firsttime = 9.4137
      endif

;      if info.maccyclecount eq 5 then begin  ;cc34
;          info.corr_time2 = 13.9519
;          info.corr_time = 70.4721
;          info.base_time = 7.885
;          info.pm_firsttime = 5.9211
;      endif

      info.cr_time_min = info.corr_time2
      info.cr_time_max = info.corr_time

      widget_control,info.corr_time2_butt,set_value=info.corr_time2
      widget_control,info.corr_time_butt,set_value=info.corr_time
      widget_control,info.base_time_butt,set_value=info.base_time
      widget_control,info.pm_firsttime_butt,set_value=info.pm_firsttime

      if info.cyclecount eq 0 then begin       ;aa
          widget_control,info.b11,set_button=1
          widget_control,info.c1,set_button=1
          widget_control,info.d5,set_button=1
          info.selected_freq0 = 0
          info.selected_freq1 = 4
          filestruct.plot(*) = 'no'
          filestruct.plot(0) = 'yes'
          filestruct.plot(4) = 'yes'
          filename = filestruct.sc(0) + 'aa-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 1 then begin       ;ab
          widget_control,info.b11,set_button=1
          widget_control,info.c1,set_button=1
          widget_control,info.d6,set_button=1
          info.selected_freq0 = 0
          info.selected_freq1 = 5
          filestruct.plot(*) = 'no'
          filestruct.plot(0) = 'yes'
          filestruct.plot(5) = 'yes'
          filename = filestruct.sc(0) + 'ab-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 2 then begin       ;ac
          widget_control,info.b11,set_button=1
          widget_control,info.c1,set_button=1
          widget_control,info.d7,set_button=1
          info.selected_freq0 = 0
          info.selected_freq1 = 6
          filestruct.plot(*) = 'no'
          filestruct.plot(0) = 'yes'
          filestruct.plot(6) = 'yes'
          filename = filestruct.sc(0) + 'ac-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 3 then begin       ;ba
          widget_control,info.b11,set_button=0
          widget_control,info.b22,set_button=1
          widget_control,info.c2,set_button=1
          widget_control,info.d5,set_button=1
          info.selected_freq0 = 1
          info.selected_freq1 = 4
          filestruct.plot(*) = 'no'
          filestruct.plot(1) = 'yes'
          filestruct.plot(4) = 'yes'
          filename = filestruct.sc(0) + 'ba-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 4 then begin ;bb
          widget_control,info.b22,set_button=1
          widget_control,info.c2,set_button=1
          widget_control,info.d6,set_button=1
          info.selected_freq0 = 1
          info.selected_freq1 = 5
          filestruct.plot(*) = 'no'
          filestruct.plot(1) = 'yes'
          filestruct.plot(5) = 'yes'
          filename = filestruct.sc(0) + 'bb-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 5 then begin ;bc
          widget_control,info.b22,set_button=1
          widget_control,info.c2,set_button=1
          widget_control,info.d7,set_button=1
          info.selected_freq0 = 1
          info.selected_freq1 = 6
          filestruct.plot(*) = 'no'
          filestruct.plot(1) = 'yes'
          filestruct.plot(6) = 'yes'
          filename = filestruct.sc(0) + 'bc-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 6 then begin ;ca
          widget_control,info.b22,set_button=0
          widget_control,info.b33,set_button=1
          widget_control,info.c3,set_button=1
          widget_control,info.d5,set_button=1
          info.selected_freq0 = 2
          info.selected_freq1 = 4
          filestruct.plot(*) = 'no'
          filestruct.plot(2) = 'yes'
          filestruct.plot(4) = 'yes'
          filename = filestruct.sc(0) + 'ca-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 7 then begin ;cb
          widget_control,info.b33,set_button=1
          widget_control,info.c3,set_button=1
          widget_control,info.d6,set_button=1
          info.selected_freq0 = 2
          info.selected_freq1 = 5
          filestruct.plot(*) = 'no'
          filestruct.plot(2) = 'yes'
          filestruct.plot(5) = 'yes'
          filename = filestruct.sc(0) + 'cb-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 8 then begin ;cc
          widget_control,info.b33,set_button=1
          widget_control,info.c3,set_button=1
          widget_control,info.d7,set_button=1
          info.selected_freq0 = 2
          info.selected_freq1 = 6
          filestruct.plot(*) = 'no'
          filestruct.plot(2) = 'yes'
          filestruct.plot(6) = 'yes'
          filename = filestruct.sc(0) + 'cc-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
      if info.cyclecount eq 9 then begin ;xx
          widget_control,info.b33,set_button=0
          widget_control,info.b44,set_button=1
          widget_control,info.c4,set_button=1
          widget_control,info.d8,set_button=1
          info.selected_freq0 = 3
          info.selected_freq1 = 7
          filestruct.plot(*) = 'no'
          filestruct.plot(3) = 'yes'
          filestruct.plot(7) = 'yes'
          filename = filestruct.sc(0) + 'xx-' + filestruct.freq(info.selected_freq0) + '-' + filestruct.freq(info.selected_freq1) + '.ps'
      endif
  endif  ;for cyclebool
;#########

  if info.selected_freq0 ne info.selected_freq1 then begin

  first_times = raytimes(*,*,info.selected_freq0)
  last_times = raytimes(*,*,info.selected_freq1)

;find the appropriate delta-times to add to the first and last
;frequencies plotted

  if(info.selected_freq0 EQ 0) then widget_control,info.dt1,get_value = artificial_dt1
  if(info.selected_freq0 EQ 1) then widget_control,info.dt2,get_value = artificial_dt1
  if(info.selected_freq0 EQ 2) then widget_control,info.dt3,get_value = artificial_dt1
  if(info.selected_freq0 EQ 3) then widget_control,info.dt4,get_value = artificial_dt1
  if(info.selected_freq0 EQ 4) then widget_control,info.dt5,get_value = artificial_dt1
  if(info.selected_freq0 EQ 5) then widget_control,info.dt6,get_value = artificial_dt1

  if(info.selected_freq1 EQ 0) then widget_control,info.dt1,get_value = artificial_dt2
  if(info.selected_freq1 EQ 1) then widget_control,info.dt2,get_value = artificial_dt2
  if(info.selected_freq1 EQ 2) then widget_control,info.dt3,get_value = artificial_dt2
  if(info.selected_freq1 EQ 3) then widget_control,info.dt4,get_value = artificial_dt2
  if(info.selected_freq1 EQ 4) then widget_control,info.dt5,get_value = artificial_dt2
  if(info.selected_freq1 EQ 5) then widget_control,info.dt6,get_value = artificial_dt2
  if(info.selected_freq1 EQ 6) then widget_control,info.dt6,get_value = artificial_dt2
  if(info.selected_freq1 EQ 7) then widget_control,info.dt6,get_value = artificial_dt2
 
;########
  foo = where(filestruct.filename NE '') ;the correlation involves all the files
  n_freqs = n_elements(foo)   ;reduced matrices off of filestruct.xxx
  freqs = fltarr(n_freqs)     
  freqs = filestruct.freq(foo)
  freq_cutoff_low = 0.
  freq_cutoff_high = 0.
  letters = strmid(filestruct.filename,8,1)
    
  if info.cutoff_index0 NE 100. then freq_cutoff_low = raytimes(*,*,info.cutoff_index0)
  if info.cutoff_index1 NE 100. then freq_cutoff_high = raytimes(*,*,info.cutoff_index1)

                                ;I only want to take the time
;difference if there is a common elements in both arrays
  
  x = where((first_times NE 0.) AND (last_times NE 0.) AND (finite(first_times) NE 0.) AND (finite(last_times) NE 0.))
  y = where((finite(first_times) eq 0.) or (finite(last_times) eq 0.))

;time difference of first and last freq.
  diff = fltarr(gridspac,gridspac)        ;only if they have common elements
  ft_tmp = fltarr(gridspac,gridspac)
  lt_tmp = fltarr(gridspac,gridspac)

  if y(0) ne -1 then begin
      diff(y) = -10000.
      ft_tmp(y) = -10000.
      lt_tmp(y) = -10000.
  endif
  if y(0) eq -1 then begin
      zeros = where((first_times eq 0.) or (last_times eq 0.))
      if zeros(0) NE -1 then begin
          diff(zeros) = -10000.
          ft_tmp(zeros) = -10000.
          lt_tmp(zeros) = -10000.
      endif
  endif           
;##################################
;can choose to add a specific artificial delta-time to each frequency 

  if x(0) ne -1 then begin
      for i=0L,n_elements(x)-1 do begin
          ft_tmp(x(i)) = first_times(x(i)) + artificial_dt1*0.001
          lt_tmp(x(i)) = last_times(x(i)) + artificial_dt2*0.001
      endfor
  endif
  no_times = 'no'
  if x(0) eq -1 then begin
      print,'NO AREA TO PLOT'
      no_times = 'yes'
  endif
  if x(0) ne -1 then begin
      diff(x) = lt_tmp(x) - ft_tmp(x)
      goo = total(first_times - last_times,/nan)
      if(goo EQ 0) then diff = first_times ;so there are still rays to plot if only one file is read in.
;##################################
;now let's filter the diff plot by the cutoff frequencies. A cutoff
;frequency is one that doesn't appear on the spectrogram for 
;a given chorus element. 
;Any region in the diff plot that overlaps with a region where the cutoff
;frequency exists is eliminated from the plot. 

      if(info.cutoff_index0 NE 100) then begin
          x = where((diff NE 0.) AND (freq_cutoff_low NE 0.))
          print,'total of overlap: ',total(diff(x),/nan)
          diff(x) = 0.
      endif
      if(info.cutoff_index1 NE 100) then begin
          x = where((diff NE 0.) AND (freq_cutoff_high NE 0.))
          print,'total of overlap: ',total(x,/nan)
          diff(x) = 0.
      endif
                                ;now I'm going to filter the ray plots
;to areas that only fall within info.cr_time_max and info.cr_time_min
      
      if(info.filter_to_firstfreq EQ 'no') then begin      
          keepers = where((diff LE info.cr_time_max*0.001) AND (diff GE info.cr_time_min*0.001)) ;keepers
          not_keepers = where((diff GT info.cr_time_max*0.001) OR (diff LT info.cr_time_min*0.001)) ;get rid of
      endif
;#########
      if(info.filter_to_firstfreq EQ 'yes') then begin
          base_time_min = (info.base_time - info.pm_firsttime)*0.001
          base_time_max = (info.base_time + info.pm_firsttime)*0.001      
          
          a = where((first_times GE base_time_min) AND (first_times LE base_time_max))
          b = where((diff GE info.cr_time_min*0.001) AND (diff LE info.cr_time_max*0.001))     
;######## here's an intersection routine from Coyote's website ##########
          minab = MIN(a,MAX=maxa,/nan) > MIN(b,Max=maxb,/nan)
          maxab = maxa < maxb
          
          if maxab LT minab OR maxab LT 0 then begin
              keepers = -1.
              count = 0.
          endif else begin
              keepers = 1.
          endelse
          if(keepers NE -1.) then keepers = where((histogram(a,min=minab,max=maxab) NE 0.) AND $
                                                  (histogram(b,min=minab,max=maxab) NE 0.),count)
          if count EQ 0. then keepers=-1
          if count NE 0. then keepers = keepers + minab
          
;########################################################################    
;########################################################################
          a = where((first_times LT base_time_min) OR (first_times GT base_time_max))
          b = where((diff LT info.cr_time_min*0.001) OR (diff GT info.cr_time_max*0.001))
          
          if a[0] LT 0 then not_keepers = b
          if b[0] LT 0 then not_keepers = a
          if((a[0] GT -1) AND (b[0] GT -1)) then not_keepers = where(histogram([a,b],Omin=omin)) + omin
          
;########################################################################
      endif
      diff_work = fltarr(gridspac,gridspac)
;###  
      nan = where(finite(diff) eq 0.)
      if nan(0) NE -1 then begin
          diff_work(nan) = -10000.
      endif
;###
      if(keepers(0) NE -1) then begin
;these are the places where rays from both frequencies exist and their
;timing differences do fall within the chosen limits b/t info.cr_time_max
;and info.cr_time_min
          diff_work(keepers) = diff(keepers)     
      endif
      if(not_keepers(0) NE -1) then begin
;these are the places where rays from both frequencies exist but their
;timing difference does not fall within the chosen limits. 
          
          diff_work(not_keepers) = -10000.
      endif 
      if(keepers(0) EQ -1) then begin
          diff_work = diff
          diff_work(*,*) = -10000. ;all elements are above cutoff time    
      endif 
      if(not_keepers(0) EQ -1) then begin
          diff_work = diff
      endif  
  endif
  if no_times eq 'yes' then begin
      diff_work = fltarr(gridspac,gridspac)
      diff_work(*,*) = -10000.
  endif
endif                           ;for the difference times
;##########################################################
;this is for the regular plots
;##########################################################

if (info.selected_freq0 eq info.selected_freq1) or (info.selected_freq1 eq 0) then begin
    if(info.selected_freq0 EQ 0) then widget_control,info.dt1,get_value = artificial_dt1
    if(info.selected_freq0 EQ 1) then widget_control,info.dt2,get_value = artificial_dt1
    if(info.selected_freq0 EQ 2) then widget_control,info.dt3,get_value = artificial_dt1
    if(info.selected_freq0 EQ 3) then widget_control,info.dt4,get_value = artificial_dt1
    if(info.selected_freq0 EQ 4) then widget_control,info.dt5,get_value = artificial_dt1
    if(info.selected_freq0 EQ 5) then widget_control,info.dt6,get_value = artificial_dt1

    which_files = where(filestruct.plot eq 'yes',count)
    work_raytimes = fltarr(gridspac,gridspac,count)
    for q=0,count-1 do begin
        for j=0,(gridspac-1) do begin
            y = where((raytimes(j,*,which_files(q)) eq 0.) or (finite(raytimes(j,*,which_files(q))) eq 0.))
            if y(0) ne -1 then work_raytimes(j,y,q) = -100000.
        endfor

        for j=0,(gridspac-1) do begin
            keepers = where((raytimes(j,*,which_files(q)) LE info.cr_time_max*0.001) AND (raytimes(j,*,which_files(q)) GE info.cr_time_min*0.001)) 
            not_keepers = where((raytimes(j,*,which_files(q)) GT info.cr_time_max*0.001) OR (raytimes(j,*,which_files(q)) LT info.cr_time_min*0.001))
            if keepers(0) ne -1 then work_raytimes(j,keepers,q) = raytimes(j,keepers,which_files(q))
            if not_keepers(0) ne -1 then work_raytimes(j,not_keepers,q) = -100000.
            if keepers(0) eq -1 then work_raytimes(j,*,q) = -100000. 
            if not_keepers(0) eq -1 then work_raytimes(j,*,q) = raytimes(j,*,which_files(q))
        endfor
    endfor
endif
;##########################################

;need david fannings colorbar function for the colorbar stuff
;to work. Saved in current directory as colorbar.pro
  ;LOADCT, 2

if(allinfo(0) eq 'event: ev31-aug23-03') then begin & xsc=[4.55966,4.567,4.5457,4.55391] & zsc=[-0.50016,-0.57156,-0.57431,-0.63553] & endif
if(allinfo(0) eq 'event: ev29-aug23-03') then begin & xsc=[4.58514,4.58474,4.56175,4.56399] & zsc=[-1.55633,-1.62495,-1.63119,-1.6892] & endif
if(allinfo(0) eq 'event: ev25-aug23-03') then begin & xsc=[4.58402,4.58817,4.56643,4.57219] & zsc=[-0.90434,-0.97489,-0.97904,-1.03923] & endif
if(allinfo(0) eq 'event: ev24-aug23-03') then begin & xsc=[4.58452,4.58861,4.5668,4.57247] & zsc=[-0.91882,-0.98935,-0.99354,-1.05368] & endif
if(allinfo(0) eq 'event: ev23-aug23-03') then begin & xsc=[4.58694,4.5905,4.56851,4.57373] & zsc=[-0.99633,-1.06667,-1.07112,-1.13102] & endif
if(allinfo(0) eq 'event: ev22-aug23-03') then begin & xsc=[4.58747,4.59087,4.56888,4.57398] & zsc=[-1.01533,-1.08562,-1.09013,-1.14997] & endif
if(allinfo(0) eq 'event: ev21-aug23-03') then begin & xsc=[4.58835,4.59143,4.56943,4.57431] & zsc=[-1.05269,-1.12287,-1.12751,-1.18725] & endif
if(allinfo(0) eq 'event: ev21-2-aug23-03') then begin & xsc=[4.58835,4.59143,4.56943,4.57431] & zsc=[-1.05269,-1.12287,-1.12751,-1.18725] & endif
if(allinfo(0) eq 'event: ev19-aug23-03') then begin & xsc=[4.58938,4.59206,4.56994,4.57455] & zsc=[-1.08545,-1.15554,-1.16029,-1.21993] & endif
if(allinfo(0) eq 'event: ev16-aug23-03') then begin & xsc=[4.56012,4.56739,4.54607,4.55429] & zsc=[-0.50549,-0.57688,-0.57964,-0.64086] & endif
if(allinfo(0) eq 'event: ev20-aug23-03') then begin & xsc=[4.58857,4.59156,4.56956,4.57437] & zsc=[-1.06385,-1.134,-1.13868,-1.19838] & endif
if(allinfo(0) eq 'event: ev15-aug23-03') then begin & xsc=[4.59035,4.59215,4.56976,4.57369] & zsc=[-1.23683,-1.3065,-1.31172,-1.37088] & endif
if(allinfo(0) eq 'event: ev13-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-1-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-2-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-3-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-4-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-5-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-6-aug23-03') then begin & xsc=[4.58181,4.58637,4.56469,4.57074] & zsc=[-0.84965,-0.92034,-0.92431,-0.98463] & endif
if(allinfo(0) eq 'event: ev13-nov17-03') then begin & xsc=[4.11586,4.15614,4.13669,4.16086] & zsc=[1.21881,1.11464,1.11043,1.04834] & endif
if(allinfo(0) eq 'event: ev5-dec06-03') then begin & xsc=[4.18835,4.23061,4.19978,4.23025] & zsc=[0.8512,0.70808,0.74503,0.63989] & endif
if(allinfo(0) eq 'event: ev5-nov17-03') then begin & xsc=[4.12167,4.1617,4.1423,4.16635] & zsc=[1.20396,1.09973,1.09547,1.03335] & endif
if(allinfo(0) eq 'event: ev4-dec06-03') then begin & xsc=[4.45944,4.47878,4.45406,4.46829] & zsc=[-0.35739,-0.50093,-0.46807,-0.57271] & endif
if(allinfo(0) eq 'event: ev3-dec06-03') then begin & xsc=[4.42647,4.44958,4.42401,4.44082] & zsc=[-0.14886,-0.29284,-0.25917,-0.36425] & endif
if(allinfo(0) eq 'event: ev5-nov27-00') then begin & xsc=[3.74433,3.74782,3.72286,3.78669] & zsc=[0.79219,0.67321,0.71854,0.57166] & endif
if(allinfo(0) eq 'event: ev3-nov27-00') then begin & xsc=[3.74433,3.74782,3.72286,3.78669] & zsc=[0.79219,0.67321,0.71854,0.57166] & endif
if(allinfo(0) eq 'event: ev13-apr18-02') then begin & xsc=[4.27252,4.27292,4.27718,4.26808] & zsc=[0.89090,0.86242,0.85034,0.87477] & endif
if(allinfo(0) eq 'event: ev10-apr18-02') then begin & xsc=[4.36791,4.36546,4.36863,4.36197] & zsc=[0.02502,-0.0051,-0.0169,0.007] & endif
if(allinfo(0) eq 'event: ev4-apr18-02') then begin & xsc=[4.27140,4.27181,4.27609,4.26696] & zsc=[0.89771,0.86924,0.85716,0.88159] & endif
if(allinfo(0) eq 'event: ev11-apr18-02') then begin & xsc=[4.36783,4.36539,4.36856,4.3619] & zsc=[0.02639,-0.0037,-0.0155,0.00837] & endif
if(allinfo(0) eq 'event: ev9-apr18-02') then begin & xsc=[4.36853,4.36603,4.36917,4.36255] & zsc=[0.01393,-0.01621,-0.02799,-0.0041] & endif
if(allinfo(0) eq 'event: ev12-aug23-03') then begin & xsc=[4.5837,4.5880,4.56623,4.5720] & zsc=[-0.8971,-0.9676,-0.9718,-1.0320] & endif
if(allinfo(0) eq 'event: ev11-aug23-03') then begin & xsc=[4.58,4.5849,4.5632,4.5695] & zsc=[-0.81,-0.8809,-0.8847,-0.9451] & endif
if(allinfo(0) eq 'event: ev10-aug23-03') then begin & xsc=[4.5886,4.5916,4.5696,4.5744] & zsc=[-1.0678,-1.1379,-1.1426,-1.2023] & endif
if(allinfo(0) EQ 'event: ev4-aug23-03') then begin & xsc=[4.5902,4.5916,4.5692,4.5728] & zsc=[-1.2882,-1.3577,-1.3631,-1.42207] & endif
if(allinfo(0) eq 'event: ev2-aug23-03') then begin & xsc=[4.5903,4.5922,4.5699,4.5739] & zsc=[-1.2225,-1.2922,-1.2974,-1.3566] & endif
if(allinfo(0) eq 'event: ev6-aug23-03') then begin & xsc=[4.5895,4.5921,4.5700,4.5745] & zsc=[-1.1240,-1.1940,-1.1989,-1.2584] & endif
if(allinfo(0) eq 'event: ev1-nov27-00') then begin & xsc=[3.74433,3.74782,3.72286,3.78669] & zsc=[0.79219,0.67321,0.71854,0.57166] & endif
if(allinfo(0) eq 'event: ev2-nov27-00-single-r') then begin & xsc=[3.744329,3.747824,3.722858] & zsc=[0.79219,0.673205,0.718544] & endif
if(allinfo(0) eq 'event: ev2-nov27-00') then begin & xsc=[3.74433,3.74782,3.72286,3.78669] & zsc=[0.79219,0.673205,0.718544,0.57166] & endif
if(allinfo(0) eq 'event: ev2-nov27-00-r') then begin & xsc=[3.744329,3.747824,3.722858] & zsc=[0.79219,0.673205,0.718544] & endif
if(allinfo(0) eq 'event: ev3-aug23-03') then begin & xsc=[4.5826,4.5817,4.5586,4.5605] & zsc=[-1.6244,-1.6927,-1.6992,-1.7570] & endif
if(allinfo(0) eq 'event: ev7-aug23-03') then begin & xsc=[4.58876,4.59169,4.56964,4.57442] & zsc=[-1.07501,-1.14513,-1.14985,-1.2095] & endif
if(allinfo(0) eq 'event: ev5-aug23-03') then begin & xsc=[4.59002,4.59228,4.5701,4.57436] & zsc=[-1.1652,-1.2351,-1.2401,-1.29945] & endif
if(allinfo(0) eq 'event: ev3-apr18-02') then begin & xsc=[4.270039,4.270468,4.27477,4.265611] & zsc=[0.905841,0.877387,0.865301,0.889739] & endif
if(allinfo(0) eq 'event: ev1-dec06-03') then begin & xsc=[4.18835,4.23061,4.19978,4.23025] & zsc=[0.8512,0.70808,0.74503,0.63989] & endif
if(allinfo(0) eq 'event: ev2-dec06-03') then begin & xsc=[4.46396,4.48272,4.45812,4.47196] & zsc=[-0.38893,-0.53239,-0.49966,-0.60422] & endif
if(allinfo(0) eq 'event: ev2-sep16-03') then begin & xsc=[4.66334,4.67883,4.65753,4.66904] & zsc=[-0.5223,-0.6277,-0.63372,-0.69627] & endif
if(allinfo(0) eq 'event: ev1-jul24-03') then begin & xsc=[4.57794,4.58684,4.56242,4.57217] & zsc=[-0.65115,-0.79715,-0.71915,-0.86372] & endif
if(allinfo(0) eq 'event: ev2-jul24-03') then begin & xsc=[4.32935,4.36709,4.32593,4.36407] & zsc=[0.931531,0.784979,0.868794,0.722151] & endif
if(allinfo(0) eq 'event: ev8-aug23-03') then begin & xsc=[4.56290,4.56991,4.54856,4.55652] & zsc=[-0.54003,-0.61136,-0.61424,-0.67538] & endif
if(allinfo(0) eq 'event: ev9-aug23-03') then begin & xsc=[4.58500,4.58899,4.56716,4.57275] & zsc=[-0.93198,-1.00247,-1.00671,-1.06681] & endif
if(allinfo(0) eq 'event: ev4-jul24-03') then begin & xsc=[4.57755,4.58656,4.56209,4.57193] & zsc=[-0.64520,-0.79121,-0.71318,-0.85778] & endif
if(allinfo(0) eq 'event: ev6-jul24-03') then begin & xsc=[4.57768,4.58665,4.56220,4.57201] & zsc=[-0.64718,-0.79319,-0.71517,-0.85976] & endif
if(allinfo(0) eq 'event: ev2-nov17-03') then begin & xsc=[4.16096,4.19943,4.18007,4.20330] & zsc=[1.10079,0.99616,0.99155,0.92926] & endif
if(allinfo(0) eq 'event: ev4-nov17-03') then begin & xsc=[4.12273,4.16271,4.14332,4.16735] & zsc=[1.20124,1.09699,1.09273,1.03060] & endif
if(allinfo(0) eq 'event: ev7-nov17-03') then begin & xsc=[4.11586,4.15614,4.13669,4.16087] & zsc=[1.21881,1.11464,1.11043,1.04834] & endif
if(allinfo(0) eq 'event: ev9-nov17-03') then begin & xsc=[4.11238,4.15282,4.13333,4.15759] & zsc=[1.22760,1.12348,1.11928,1.05721] & endif
if(allinfo(0) eq 'event: ev10-nov17-03') then begin & xsc=[4.10974,4.15026,4.13079,4.1551] & zsc=[1.23439,1.13028,1.12612,1.06406] & endif


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

;tmppp = where((tgn_c1 ne -10000.) and (tgn_c1 gt 0.3))
;if tmppp(0) ne -1 then begin
;min_gen = min(tgn_c1(tmppp))
;max_gen = max(tgn_c1(tmppp))

;green_min = 256*min_gen/(70.)
;green_max = 256*max_gen/(70.)

;greenvector(green_min:green_max) = 0
;redvector(green_min:green_max) = 0
;bluevector(green_min:green_max) = 0

;endif


;END OF COLORBAR
;##########################
;##########################################

;    greenvector(0) = 0. ;set first element to white
;    bluevector(0) = 0.
;    redvector(0) = 0.
;
;    greenvector(255) = 0.
;    bluevector(255) = 0.
;    redvector(255) = 0.  ;make last element black

;greenvector(254) = 0.
;bluevector(254) = 0.
;redvector(254) = 0.


    tvlct,redvector,greenvector,bluevector

;################  
    divisions = 10.        
    increment = 0.001*(info.cr_time_max - info.cr_time_min)/(divisions)
    tmplvls = findgen(divisions+1)*increment + 0.001*info.cr_time_min
    strlvls = strtrim(string(tmplvls*1000),2)
    strlvls = strmid(strlvls,0,5)
    if(tmplvls(0) EQ 0.) then tmplvls(0) = 0.000001

 if info.cr_time_max eq info.cr_time_min then info.cr_time_max = 0.001

    ;lvl_divisions = 255.  ;this leads to 255 different colors (too many!)
    lvl_divisions = 255/5. ;''   ''   '' 255/10 different colors
    increment = 0.001*(info.cr_time_max - info.cr_time_min)/(lvl_divisions)
    lvls = findgen(lvl_divisions+2)*increment + 0.001*info.cr_time_min
    lvls(0) = -10000.*0.001
    lvls(lvl_divisions+1) = 10000.*0.001  ;unreachable index number for black. 
;To use black, call it explicitly with color=255
    
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
;#######################
    !p.multi = [0,5,5]
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    tmp = where(filestruct.plot EQ 'yes')
    filenames2 = strarr(10)
    if(tmp(0) NE -1) then begin 
        filenames2(tmp) = filestruct.filename(tmp)
        n_names = n_elements(tmp)
    endif
    if(tmp(0) EQ -1) then begin
        n_names = 0
    endif
    
    px_left = fltarr(10)
    px_right = fltarr(10)
    py_top = fltarr(10)
    py_bottom = fltarr(10)
    print,'where filestruct.plot EQ yes: ',tmp
    print,'number of plots selected (n_names): ',n_names
    print,'filestruct.plot: ',filestruct.plot
    print,'yyyyyyyyyyyyyyyyyyyyyyyyyy'
    print,'files I am plotting: ',filenames2
;#########
    if(n_names EQ 1) then begin
        px_left(tmp(0)) = 0.1 & px_right(tmp(0)) = 0.75 & py_top(tmp(0)) = 0.80 & py_bottom(tmp(0)) = 0.15
    endif
    if(n_names EQ 2) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.475 & px_right(tmp(0)) = 0.375 & px_right(tmp(1)) = 0.75
        py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85 & py_bottom(tmp(0)) = 0.575 & py_bottom(tmp(1)) = 0.575
    endif
    if(n_names EQ 3) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.475 & px_left(tmp(2)) = 0.1 & px_right(tmp(0)) = 0.375
        px_right(tmp(1)) = 0.75 & px_right(tmp(2)) = 0.375 & py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85
        py_top(tmp(2)) = 0.495 & py_bottom(tmp(0)) = 0.575 & py_bottom(tmp(1)) = 0.575 & py_bottom(tmp(2)) = 0.22
    endif
    if(n_names EQ 4) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.475 & px_left(tmp(2)) = 0.1 & px_left(tmp(3)) = 0.475
        px_right(tmp(0)) = 0.375 & px_right(tmp(1)) = 0.75 & px_right(tmp(2)) = 0.375 & px_right(tmp(3)) = 0.75
        py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85 & py_top(tmp(2)) = 0.495 & py_top(tmp(3)) = 0.495
        py_bottom(tmp(0)) = 0.575 & py_bottom(tmp(1)) = 0.575 & py_bottom(tmp(2)) = 0.22 & py_bottom(tmp(3)) = 0.22
    endif
    if(n_names EQ 5) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.39 & px_left(tmp(2)) = 0.1 & px_left(tmp(3)) = 0.39
        px_left(tmp(4)) = 0.1 & px_right(tmp(0)) = 0.31 & px_right(tmp(1)) = 0.6 & px_right(tmp(2)) = 0.31
        px_right(tmp(3)) = 0.6 & px_right(tmp(4)) = 0.31 & py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85
        py_top(tmp(2)) = 0.56 & py_top(tmp(3)) = 0.56 & py_top(tmp(4)) = 0.27 & py_bottom(tmp(0)) = 0.64
        py_bottom(tmp(1)) = 0.64 & py_bottom(tmp(2)) = 0.35 & py_bottom(tmp(3)) = 0.35 & py_bottom(tmp(4)) = 0.06
    endif
    if(n_names EQ 6) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.39 & px_left(tmp(2)) = 0.1 & px_left(tmp(3)) = 0.39
        px_left(tmp(4)) = 0.1 & px_left(tmp(5)) = 0.39 & px_right(tmp(0)) = 0.31 & px_right(tmp(1)) = 0.6
        px_right(tmp(2)) = 0.31 & px_right(tmp(3)) = 0.6 & px_right(tmp(4)) = 0.31 & px_right(tmp(5)) = 0.6
        py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85 & py_top(tmp(2)) = 0.56 & py_top(tmp(3)) = 0.56
        py_top(tmp(4)) = 0.27 & py_top(tmp(5)) = 0.27 & py_bottom(tmp(0)) = 0.64 & py_bottom(tmp(1)) = 0.64
        py_bottom(tmp(2)) = 0.35 & py_bottom(tmp(3)) = 0.35 & py_bottom(tmp(4)) = 0.06 & py_bottom(tmp(5)) = 0.06
    endif
    if(n_names EQ 7) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.39 & px_left(tmp(2)) = 0.1 & px_left(tmp(3)) = 0.39
        px_left(tmp(4)) = 0.1 & px_left(tmp(5)) = 0.39 & px_left(tmp(6)) = 0.1 & px_right(tmp(0)) = 0.31
        px_right(tmp(1)) = 0.6 & px_right(tmp(2)) = 0.31 & px_right(tmp(3)) = 0.6 & px_right(tmp(4)) = 0.31
        px_right(tmp(5)) = 0.6 & px_right(Tmp(6)) = 0.31 & py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85
        py_top(tmp(2)) = 0.6325 & py_top(tmp(3)) = 0.6325 & py_top(tmp(4)) = 0.415 & py_top(tmp(5)) = 0.415
        py_top(tmp(6)) = 0.1975 & py_bottom(tmp(0)) = 0.7125 & py_bottom(tmp(1)) = 0.7125 & py_bottom(tmp(2)) = 0.495
        py_bottom(tmp(3)) = 0.495 & py_bottom(tmp(4)) = 0.2775 & py_bottom(tmp(5)) = 0.2775 & py_bottom(tmp(6)) = 0.06
    endif
    if(n_names EQ 8) then begin
        px_left(tmp(0)) = 0.1 & px_left(tmp(1)) = 0.39 & px_left(tmp(2)) = 0.1 & px_left(tmp(3)) = 0.39
        px_left(tmp(4)) = 0.1 & px_left(tmp(5)) = 0.39 & px_left(tmp(6)) = 0.1 & px_left(tmp(7)) = 0.39
        px_right(tmp(0)) = 0.31 & px_right(tmp(1)) = 0.6 & px_right(tmp(2)) = 0.31 & px_right(tmp(3)) = 0.6
        px_right(tmp(4)) = 0.31 & px_right(tmp(5)) = 0.6 & px_right(tmp(6)) = 0.31 & px_right(tmp(7)) = 0.6
        py_top(tmp(0)) = 0.85 & py_top(tmp(1)) = 0.85 & py_top(tmp(2)) = 0.6325 & py_top(tmp(3)) = 0.6325
        py_top(tmp(4)) = 0.415 & py_top(tmp(5)) = 0.415 & py_top(tmp(6)) = 0.1975 & py_top(tmp(7)) = 0.1975 
        py_bottom(tmp(0)) = 0.7125 & py_bottom(tmp(1)) = 0.7125 & py_bottom(tmp(2)) = 0.495 & py_bottom(tmp(3)) = 0.495
        py_bottom(tmp(4)) = 0.2775 & py_bottom(tmp(5)) = 0.2775 & py_bottom(tmp(6)) = 0.06 & py_bottom(tmp(7)) = 0.06
    endif
;#############
;********* now for the plots ***********
;    print,'total of diff_work: ',total(diff_work,/nan)
    scone = fltarr(2)
    sctwo = fltarr(2)
;the following few lines work but should probably be changed
    if timetype eq 1. then begin
        scone(0) = 1. 
        sctwo(0) = -1.
    endif
    if timetype eq 3. then begin
        scone(0) = 1.
        sctwo(0) = 1.
    endif
    
;#########################################################
;######################################################### 
;This corresponds to the 'SINGLE PLOT DIFF' button
;To use this you must select a beginning and final frequency
;of which to take the time difference with the 
;'1ST FREQ' and 'LAST FREQ' buttons.
;This can be used with the correlation plots or the 
;single spacecraft plots.
;only one plot can be displayed at a time in this mode
;#########################################################
;###########  SINGLE PLOT DIFF ###########################


    ;which_files = where(filestruct.plot eq 'yes',count)
    if(regular_or_diff EQ 'diff') AND (one_plot EQ 'yes') then begin        
;z-buffer
        thisDevice = !d.name
        set_plot,'z',/copy
        device,set_resolution=[515,510],z_buffer=0
        erase
        !p.position=[0,0,1,1]                                  

        contour,diff_work,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
        a=TVRD() 
        device,/close
        set_plot,thisDevice
        
        if info.ps1 eq 'yes' or info.cyclebool eq 'yes' then begin
            set_plot,'ps'
            !p.font = 0
            if info.ps1 eq 'yes' then device,filename = 'diff_full.ps',bits=8,/color
            if info.cyclebool eq 'yes' then device,filename=filename,bits=8,/color
            print,!d.window
        endif

        !p.position=[0,0,1,1]
        tvlct,redvector,greenvector,bluevector
        
        px = 0.
        py = 0.
        tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
        
;plot with no data
        contour,diff_work,xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255     
        
        xyouts,0.05,0.10,'freq1 ' + '(' + filestruct.sc(info.selected_freq0) + ')' + ' = ' + filestruct.freq(info.selected_freq0) + ' Hz',/normal,font=0,charsize = 0.8,color=255
        xyouts,0.05,0.07,'freq2 ' + '(' + filestruct.sc(info.selected_freq1) + ')' + ' = ' + filestruct.freq(info.selected_freq1) + ' Hz',/normal,font=0,charsize = 0.8,color=255
        tmp = strmid(filestruct.sc(info.selected_freq0),3,1)
        if(tmp NE ',') then xyouts,0.05,0.13,'time difference (msec) b/t the two listed freqs for a single sc',/normal,font=0,charsize=0.8,color=255
        if(tmp EQ ',') then xyouts,0.05,0.13,'time difference (msec) b/t the two listed freqs on the cross-correlation plot',/normal,font=0,charsize=0.8,color=255       
        xyouts,0.05,0.05,'min filter time = ' + strtrim(string(info.cr_time_min),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        xyouts,0.05,0.03,'max filter time = ' + strtrim(string(info.cr_time_max),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        if info.filter_to_firstfreq eq 'yes' then xyouts,0.05,0.01,'base freq time = ' + strtrim(string(info.base_time),2) + ' msec +/-' + strtrim(string(info.pm_firsttime),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        
        COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255       
        if(info.include_rays EQ 'yes') then begin
;first cc pair
            if(scone(0) ne -1) then begin
            if letters(info.selected_freq0) eq 'x' then oplot,xrays1x(*,info.selected_freq0),zrays1x(*,info.selected_freq0),color=30,psym=1
            if letters(info.selected_freq0) eq 'a' then oplot,xrays1a(*,info.selected_freq0),zrays1a(*,info.selected_freq0),color=30,psym=1
            if letters(info.selected_freq0) eq 'b' then oplot,xrays1b(*,info.selected_freq0),zrays1b(*,info.selected_freq0),color=30,psym=1
            if letters(info.selected_freq0) eq 'c' then oplot,xrays1c(*,info.selected_freq0),zrays1c(*,info.selected_freq0),color=30,psym=1
print,'RAYS FOR SCA FREQ1'
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(info.selected_freq0) eq 'x' then oplot,xrays2x(*,info.selected_freq0),zrays2x(*,info.selected_freq0),color=60,psym=1
            if letters(info.selected_freq0) eq 'a' then oplot,xrays2a(*,info.selected_freq0),zrays2a(*,info.selected_freq0),color=60,psym=1
            if letters(info.selected_freq0) eq 'b' then oplot,xrays2b(*,info.selected_freq0),zrays2b(*,info.selected_freq0),color=60,psym=1
            if letters(info.selected_freq0) eq 'c' then oplot,xrays2c(*,info.selected_freq0),zrays2c(*,info.selected_freq0),color=60,psym=1
print,'RAYS FOR SCB FREQ1'
stop
            endif
;second cc pair           
            if(scone(0) ne -1) then begin
            if letters(info.selected_freq1) eq 'x' then oplot,xrays1x(*,info.selected_freq1),zrays1x(*,info.selected_freq1),color=120,psym=1
            if letters(info.selected_freq1) eq 'a' then oplot,xrays1a(*,info.selected_freq1),zrays1a(*,info.selected_freq1),color=120,psym=1
            if letters(info.selected_freq1) eq 'b' then oplot,xrays1b(*,info.selected_freq1),zrays1b(*,info.selected_freq1),color=120,psym=1
            if letters(info.selected_freq1) eq 'c' then oplot,xrays1c(*,info.selected_freq1),zrays1c(*,info.selected_freq1),color=120,psym=1
print,'RAYS FOR SCA FREQ2'
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(info.selected_freq1) eq 'x' then oplot,xrays2x(*,info.selected_freq1),zrays2x(*,info.selected_freq1),color=190,psym=1
            if letters(info.selected_freq1) eq 'a' then oplot,xrays2a(*,info.selected_freq1),zrays2a(*,info.selected_freq1),color=190,psym=1
            if letters(info.selected_freq1) eq 'b' then oplot,xrays2b(*,info.selected_freq1),zrays2b(*,info.selected_freq1),color=190,psym=1
            if letters(info.selected_freq1) eq 'c' then oplot,xrays2c(*,info.selected_freq1),zrays2c(*,info.selected_freq1),color=190,psym=1
print,'RAYS FOR SCB FREQ2'
stop
            endif
        endif          

;now, if the print to ps is selected I want to print the location
;array to a file              
    endif

;######################################################################
;this is a diff plot - same as above but without filling the entire screen
;################ PLOT DIFF ###############################

    if(regular_or_diff EQ 'diff') AND (one_plot EQ 'no') then begin            
        thisDevice = !d.name
        set_plot,'z',/copy
        device,set_resolution=[515,510],z_buffer=0
        erase      
        !p.position=[0.1,0.2,0.75,0.90] 
        
        contour,diff_work,xgrid,zgrid,position=[0.1,0.2,0.75,0.90],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255 ;multiple plots       
        
        a=TVRD() 
        device,/close
        set_plot,thisDevice

        if info.ps1 eq 'yes' then begin
            set_plot,'ps'
            !p.font = 0
            device,filename = 'diff_notfull.ps',bits=8,/color
            print,!d.window
        endif
        
        !p.position=[0.1,0.2,0.75,0.90]
        tvlct,redvector,greenvector,bluevector
        
        px = 0.
        py = 0.
        tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
        
        contour,diff_work,xgrid,zgrid,/nodata,position=[0.1,0.2,0.75,0.9],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255 ;multiple plots        

        if(info.include_rays EQ 'yes') then begin
            if(scone(0) ne -1) then begin
            if letters(info.selected_freq0) eq 'x' then oplot,xrays1x(*,info.selected_freq0),zrays1x(*,info.selected_freq0),color=30,psym=3
            if letters(info.selected_freq0) eq 'a' then oplot,xrays1a(*,info.selected_freq0),zrays1a(*,info.selected_freq0),color=30,psym=3
            if letters(info.selected_freq0) eq 'b' then oplot,xrays1b(*,info.selected_freq0),zrays1b(*,info.selected_freq0),color=30,psym=3
            if letters(info.selected_freq0) eq 'c' then oplot,xrays1c(*,info.selected_freq0),zrays1c(*,info.selected_freq0),color=30,psym=3
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(info.selected_freq0) eq 'x' then oplot,xrays2x(*,info.selected_freq0),zrays2x(*,info.selected_freq0),color=60,psym=3
            if letters(info.selected_freq0) eq 'a' then oplot,xrays2a(*,info.selected_freq0),zrays2a(*,info.selected_freq0),color=60,psym=3
            if letters(info.selected_freq0) eq 'b' then oplot,xrays2b(*,info.selected_freq0),zrays2b(*,info.selected_freq0),color=60,psym=3
            if letters(info.selected_freq0) eq 'c' then oplot,xrays2c(*,info.selected_freq0),zrays2c(*,info.selected_freq0),color=60,psym=3
stop
            endif
;second cc pair           
            if(scone(0) ne -1) then begin
            if letters(info.selected_freq1) eq 'x' then oplot,xrays1x(*,info.selected_freq1),zrays1x(*,info.selected_freq1),color=120,psym=3
            if letters(info.selected_freq1) eq 'a' then oplot,xrays1a(*,info.selected_freq1),zrays1a(*,info.selected_freq1),color=120,psym=3
            if letters(info.selected_freq1) eq 'b' then oplot,xrays1b(*,info.selected_freq1),zrays1b(*,info.selected_freq1),color=120,psym=3
            if letters(info.selected_freq1) eq 'c' then oplot,xrays1c(*,info.selected_freq1),zrays1c(*,info.selected_freq1),color=120,psym=3
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(info.selected_freq1) eq 'x' then oplot,xrays2x(*,info.selected_freq1),zrays2x(*,info.selected_freq1),color=190,psym=3
            if letters(info.selected_freq1) eq 'a' then oplot,xrays2a(*,info.selected_freq1),zrays2a(*,info.selected_freq1),color=190,psym=3
            if letters(info.selected_freq1) eq 'b' then oplot,xrays2b(*,info.selected_freq1),zrays2b(*,info.selected_freq1),color=190,psym=3
            if letters(info.selected_freq1) eq 'c' then oplot,xrays2c(*,info.selected_freq1),zrays2c(*,info.selected_freq1),color=190,psym=3
stop
            endif

;            if(scone(0) ne -1) then oplot,xrays1(*,info.selected_freq0),zrays1(*,info.selected_freq0),color=120,psym=3
;            if(sctwo(0) ne -1) then oplot,xrays2(*,info.selected_freq1),zrays2(*,info.selected_freq1),color=160,psym=3
        endif

        xyouts,0.05,0.10,'freq1 ' + '(' + filestruct.sc(info.selected_freq0) + ')' + ' = ' + filestruct.freq(info.selected_freq0) + ' Hz',/normal,font=0,charsize = 0.8,color=255
        xyouts,0.05,0.07,'freq2 ' + '(' + filestruct.sc(info.selected_freq1) + ')' + ' = ' + filestruct.freq(info.selected_freq1) + ' Hz',/normal,font=0,charsize = 0.8,color=255
        tmp = strmid(filestruct.sc(info.selected_freq0),3,1)
        if(tmp NE ',') then xyouts,0.05,0.13,'time difference (msec) b/t the two listed freqs for a single sc',/normal,font=0,charsize=0.8,color=255
        if(tmp EQ ',') then xyouts,0.05,0.13,'time difference (msec) b/t the two listed freqs on the cross-correlation plot',/normal,font=0,charsize=0.8,color=255       
        xyouts,0.05,0.05,'min filter time = ' + strtrim(string(info.cr_time_min),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        xyouts,0.05,0.03,'max filter time = ' + strtrim(string(info.cr_time_max),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        if info.filter_to_firstfreq eq 'yes' then xyouts,0.05,0.01,'base freq time = ' + strtrim(string(info.base_time),2) + ' msec +/-' + strtrim(string(info.pm_firsttime),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        
        COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255
    endif    
;####################################################################
;###################################################################
;******** The regular Plots *****************
;##### These are regular plots (not difference plots which are the
;time difference b/t the two selected freqs. i.e. these are the
;straight up timing values for a single frequency on a single sc.
;There can be up to 8 of these and I have to space them accordingly.
;Also, if there is only one there is the option of having it take
;up the entire screen
;########  SINGLE PLOT ##############################################
    which_files = where(filestruct.plot eq 'yes',count)
    if regular_or_diff eq 'regular' and one_plot EQ 'yes' then begin      

;z-buffer
        thisDevice = !d.name
        set_plot,'z',/copy
        device,set_resolution=[515,510],z_buffer=0        
        erase        
        !p.position=[0,0,1,1]       

        contour,work_raytimes(*,*,sp_offset),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255 ;fill entire plot window with single plot
          
        a=TVRD() 
        device,/close
        set_plot,thisDevice

        if info.ps1 eq 'yes' then begin
            set_plot,'ps'
            !p.font = 0
            device,filename = 'regular_full.ps',bits=8,/color
            print,!d.window
        endif

        !p.position=[0,0,1,1]
        tvlct,redvector,greenvector,bluevector
        
        px = 0. 
        py = 0.
        tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device

        contour,work_raytimes(*,*,sp_offset),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255 ;fill entire plot window with single plot
         
        if(info.include_rays EQ 'yes') then begin
            if(scone(0) ne -1) then begin
            if letters(sp_offset) eq 'x' then oplot,xrays1x(*,sp_offset),zrays1x(*,sp_offset),color=30,psym=3
            if letters(sp_offset) eq 'a' then oplot,xrays1a(*,sp_offset),zrays1a(*,sp_offset),color=30,psym=3
            if letters(sp_offset) eq 'b' then oplot,xrays1b(*,sp_offset),zrays1b(*,sp_offset),color=30,psym=3
            if letters(sp_offset) eq 'c' then oplot,xrays1c(*,sp_offset),zrays1c(*,sp_offset),color=30,psym=3
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(sp_offset) eq 'x' then oplot,xrays2x(*,sp_offset),zrays2x(*,sp_offset),color=60,psym=3
            if letters(sp_offset) eq 'a' then oplot,xrays2a(*,sp_offset),zrays2a(*,sp_offset),color=60,psym=3
            if letters(sp_offset) eq 'b' then oplot,xrays2b(*,sp_offset),zrays2b(*,sp_offset),color=60,psym=3
            if letters(sp_offset) eq 'c' then oplot,xrays2c(*,sp_offset),zrays2c(*,sp_offset),color=60,psym=3
stop
            endif
;second cc pair           
;            if(scone(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays1x(*,info.selected_freq1),zrays1x(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays1a(*,info.selected_freq1),zrays1a(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays1b(*,info.selected_freq1),zrays1b(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays1c(*,info.selected_freq1),zrays1c(*,info.selected_freq1),color=120,psym=3
;stop
;            endif          
;            if(sctwo(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays2x(*,info.selected_freq1),zrays2x(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays2a(*,info.selected_freq1),zrays2a(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays2b(*,info.selected_freq1),zrays2b(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays2c(*,info.selected_freq1),zrays2c(*,info.selected_freq1),color=190,psym=3
;stop
;            endif

;            if(scone(0) ne -1) then oplot,xrays1(*,sp_offset),zrays1(*,sp_offset),color=120,psym=3
;            if(sctwo(0) ne -1) then oplot,xrays2(*,sp_offset),zrays2(*,sp_offset),color=160,psym=3
        endif
        
        COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255
        
        xyouts,0.05,0.8,'freq ' + '(' + filestruct.sc(info.selected_freq0) + ')' + ' = ' + filestruct.freq(info.selected_freq0) + ' Hz',/normal,font=0,charsize = 0.8,color=255
        tmp = strmid(filestruct.sc(info.selected_freq0),3,1)
        if(timetype eq 1.) then xyouts,0.05,0.11,'travel time (msec) for the listed freq for a single sc',/normal,font=0,charsize=0.8,color=255
        if(timetype eq 3.) then xyouts,0.05,0.11,'time difference (scb-sca)(msec) for the listed freq',/normal,font=0,charsize=0.8,color=255     
        xyouts,0.05,0.05,'max filter time = ' + strtrim(string(info.cr_time_max),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        xyouts,0.05,0.03,'min filter time = ' + strtrim(string(info.cr_time_min),2) + ' msec',/normal,font=0,charsize=0.8,color=255
        if info.filter_to_firstfreq eq 'yes' then xyouts,0.05,0.01,'base freq time = ' + strtrim(string(info.base_time),2) + ' msec +/-' + strtrim(string(info.pm_firsttime),2) + ' msec',/normal,font=0,charsize=0.8,color=255       
    endif

;##############################################################
;This is for the regular timing values for up to 8 plots that do not
;fill up the entire screen
;################  PLOT  #######################################
          
    if regular_or_diff eq 'regular' and one_plot eq 'no' then begin
        tmpss = where(filestruct.plot eq 'yes')
        for q=0,count-1 do begin      
            
            contour,work_raytimes(*,*,q),xgrid,zgrid,position=[px_left(which_files(q)),py_bottom(which_files(q)),px_right(which_files(q)),py_top(which_files(q))],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,title='frequency = ' + filestruct.freq(tmpss(q)) + ' Hz for ' + filestruct.sc(tmpss(q)),color=255 ;multiple plots

            if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'regular_notfull.ps',bits=8,/color
                print,!d.window
            endif
            
            if(info.include_rays EQ 'yes') then begin
            if(scone(0) ne -1) then begin
            if letters(which_files(q)) eq 'x' then oplot,xrays1x(*,which_files(q)),zrays1x(*,which_files(q)),color=30,psym=3
            if letters(which_files(q)) eq 'a' then oplot,xrays1a(*,which_files(q)),zrays1a(*,which_files(q)),color=30,psym=3
            if letters(which_files(q)) eq 'b' then oplot,xrays1b(*,which_files(q)),zrays1b(*,which_files(q)),color=30,psym=3
            if letters(which_files(q)) eq 'c' then oplot,xrays1c(*,which_files(q)),zrays1c(*,which_files(q)),color=30,psym=3
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(which_files(q)) eq 'x' then oplot,xrays2x(*,which_files(q)),zrays2x(*,which_files(q)),color=60,psym=3
            if letters(which_files(q)) eq 'a' then oplot,xrays2a(*,which_files(q)),zrays2a(*,which_files(q)),color=60,psym=3
            if letters(which_files(q)) eq 'b' then oplot,xrays2b(*,which_files(q)),zrays2b(*,which_files(q)),color=60,psym=3
            if letters(which_files(q)) eq 'c' then oplot,xrays2c(*,which_files(q)),zrays2c(*,which_files(q)),color=60,psym=3
stop
            endif
;second cc pair           
;            if(scone(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays1x(*,info.selected_freq1),zrays1x(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays1a(*,info.selected_freq1),zrays1a(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays1b(*,info.selected_freq1),zrays1b(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays1c(*,info.selected_freq1),zrays1c(*,info.selected_freq1),color=120,psym=3
;stop
;            endif          
;            if(sctwo(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays2x(*,info.selected_freq1),zrays2x(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays2a(*,info.selected_freq1),zrays2a(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays2b(*,info.selected_freq1),zrays2b(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays2c(*,info.selected_freq1),zrays2c(*,info.selected_freq1),color=190,psym=3
;stop
;            endif





;                if(scone(0) ne -1) then oplot,xrays1(*,which_files(q)),zrays1(*,which_files(q)),color=120,psym=3
;                if(sctwo(0) ne -1) then oplot,xrays2(*,which_files(q)),zrays2(*,which_files(q)),color=160,psym=3
            endif      
            
            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255
            
            tmp = strmid(filestruct.sc(info.selected_freq0),3,1)
            if(timetype eq 1.) then xyouts,0.05,0.08,'travel time (msec) for the listed freqs for a single sc',/normal,font=0,charsize=0.8,color=255
            if(timetype eq 3.) then xyouts,0.05,0.08,'time difference (scb-sca)(msec) for the listed freq',/normal,font=0,charsize=0.8,color=255     
            xyouts,0.05,0.05,'max filter time = ' + strtrim(string(info.cr_time_max),2) + ' msec',/normal,font=0,charsize=0.8,color=255
            xyouts,0.05,0.03,'min filter time = ' + strtrim(string(info.cr_time_min),2) + ' msec',/normal,font=0,charsize=0.8,color=255
            if info.filter_to_firstfreq eq 'yes' then xyouts,0.05,0.01,'base freq time = ' + strtrim(string(info.base_time),2) + ' msec +/-' + strtrim(string(info.pm_firsttime),2) + ' msec',/normal,font=0,charsize=0.8,color=255            
            
            if(allinfo(1,which_files(q)) EQ 'timetype = 3') then allinfo(14,which_files(q)) = filestruct.sc(0)
            plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
        endfor             
    endif
;########################################################################
;this corresponds to the 'other' button and is used for plotting any
;of the data other than ray travel times
;################  OTHER
;##############################################

    if regular_or_diff eq 'other' then begin
;z-buffer
        thisDevice = !d.name
        set_plot,'z',/copy
        device,set_resolution=[515,510],z_buffer=0
        erase
        
        !p.position=[0,0,1,1]
;#########################################
        ;extract the sc
        if timetype eq 1 then spacecraft = filestruct.sc(info.cutoff_index0)
        if timetype eq 3 then begin
            tmp = filestruct.sc(info.cutoff_index0)
            if info.sc_toggle eq 'sca' then spacecraft = 'c' + strmid(tmp,2,1)
            if info.sc_toggle eq 'scb' then spacecraft = 'c' + strmid(tmp,3,1)
        endif

        if info.what_other_but eq 'tk' then begin                           
            lvl_divisions = 255/5.
            increment = (90)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment - increment                         
            lvls(0) = -10000. 
     
            tmplvls = findgen(divisions+1)*(90/divisions)
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)

            
            for j=0,(gridspac-1) do begin
                if info.sc_toggle eq 'sca' then begin
                    tmp = where(theta_k_sca(j,*,info.cutoff_index0) eq 0.)
                    if tmp(0) ne -1 then theta_k_sca(j,tmp,info.cutoff_index0) = -10000.                 
                    tmp = where(theta_k_sca(j,*,info.cutoff_index0) GE 90.)
                    if tmp(0) ne -1 then (theta_k_sca(j,tmp,info.cutoff_index0) = (theta_k_sca(j,tmp,info.cutoff_index0) - 180.)*(-1))
                endif
                if info.sc_toggle eq 'scb' then begin
                    tmp = where(theta_k_scb(j,*,info.cutoff_index0) eq 0.)
                    if tmp(0) ne -1 then theta_k_scb(j,tmp,info.cutoff_index0) = -10000.
                    tmp = where(theta_k_scb(j,*,info.cutoff_index0) GE 90.)
                    if tmp(0) ne -1 then (theta_k_scb(j,tmp,info.cutoff_index0) = (theta_k_scb(j,tmp,info.cutoff_index0) - 180.)*(-1))
                endif
            endfor
            
            if info.sc_toggle eq 'sca' then begin
                contour,theta_k_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
            endif
            if info.sc_toggle eq 'scb' then begin
                contour,theta_k_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
            endif
            
            a=TVRD() 
            device,/close
            set_plot,thisDevice

            if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'theta_k.ps',bits=8,/color
                print,!d.window
            endif
            
            !p.position=[0,0,1,1]
            tvlct,redvector,greenvector,bluevector
            
            px = 0. 
            py = 0.
            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
            
;plot with no data
            if info.sc_toggle eq 'sca' then begin 
                contour,theta_k_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
            endif
            if info.sc_toggle eq 'scb' then begin 
                contour,theta_k_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
            endif
            
            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 

            xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
            tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
            if(tmp NE ',') then xyouts,0.05,0.16,'wave normal angles for the',/normal,font=0,charsize=1.5,color=255
            if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255
                       
        endif                   ;for theta_k
;####################
        if info.what_other_but eq 'tg' then begin
            lvl_divisions = 255/5.
            increment = (180)/lvl_divisions
            lvls = findgen(lvl_divisions+2)*increment + (-90.)          
            lvls(0) = -10000.          
            tmplvls = findgen(divisions+1)*(180/divisions) + (-90.)
            strlvls = strtrim(string(tmplvls),2)
            strlvls = strmid(strlvls,0,5)

            for j=0,(gridspac-1) do begin
                if info.sc_toggle eq 'sca' then begin
                    tmp = where(theta_g_sca(j,*,info.cutoff_index0) eq 0.)
                    if tmp(0) ne -1 then theta_g_sca(j,tmp,info.cutoff_index0) = -10000.         
                    tmp = where((theta_g_sca(j,*,info.cutoff_index0) GE 90.) and (theta_g_sca(j,*,info.cutoff_index0) LE 180.))
                    if tmp(0) ne -1 then (theta_g_sca(j,tmp,info.cutoff_index0) = (theta_g_sca(j,tmp,info.cutoff_index0) - 180.)*(-1))      
                    tmp = where((theta_g_sca(j,*,info.cutoff_index0) LE -90.) and (theta_g_sca(j,*,info.cutoff_index0) GE -180.))
                    if tmp(0) ne -1 then (theta_g_sca(j,tmp,info.cutoff_index0) = (theta_g_sca(j,tmp,info.cutoff_index0) + 180.)*(-1))
                endif       
                if info.sc_toggle eq 'scb' then begin
                    tmp = where(theta_g_scb(j,*,info.cutoff_index0) eq 0.)
                    if tmp(0) ne -1 then theta_g_scb(j,tmp,info.cutoff_index0) = -10000.  
                    tmp = where((theta_g_scb(j,*,info.cutoff_index0) GE 90.) and (theta_g_scb(j,*,info.cutoff_index0) LE 180.))
                    if tmp(0) ne -1 then (theta_g_scb(j,tmp,info.cutoff_index0) = (theta_g_scb(j,tmp,info.cutoff_index0) - 180.)*(-1))
                    tmp = where((theta_g_scb(j,*,info.cutoff_index0) LE -90.) and (theta_g_scb(j,*,info.cutoff_index0) GE -180.))
                    if tmp(0) ne -1 then (theta_g_scb(j,tmp,info.cutoff_index0) = (theta_g_scb(j,tmp,info.cutoff_index0) + 180.)*(-1))

                endif
            endfor
            if info.sc_toggle eq 'sca' then begin
                contour,theta_g_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
            endif
            if info.sc_toggle eq 'scb' then begin
                contour,theta_g_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
            endif          
            
            a=TVRD() 
            device,/close
            set_plot,thisDevice

            if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'theta_g.ps',bits=8,/color
                print,!d.window
            endif
            
            !p.position=[0,0,1,1]
            tvlct,redvector,greenvector,bluevector
            
            px = 0. 
            py = 0.
            tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
            
;plot with no data
            if info.sc_toggle eq 'sca' then begin 
                contour,theta_g_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
            endif
            if info.sc_toggle eq 'scb' then begin 
                contour,theta_g_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
            endif
            
            COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 

            xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
            tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
            if(tmp NE ',') then xyouts,0.05,0.16,'wave group velocity angles for',/normal,font=0,charsize=1.5,color=255
            if(tmp NE ',') then xyouts,0.05,0.13,'the listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255
            
        endif                   ;theta group
;#########################
      if info.what_other_but eq 'pRe' then begin
          lvl_divisions = 255/5.
          increment = (3)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(3/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

          for j=0,(gridspac-1) do begin
              if info.sc_toggle eq 'sca' then begin
                  tmp = where(path_re_sca(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then path_re_sca(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((path_re_sca(j,*,info.cutoff_index0) GE 90) and (path_re_sca(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then path_re_sca(j,tmp,info.cutoff_index0) = (path_re_sca(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((path_re_sca(j,*,info.cutoff_index0) GT 270) and (path_re_sca(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then path_re_sca(j,tmp,info.cutoff_index0) = path_re_sca(j,tmp,info.cutoff_index0) - 360                 
              endif           
              if info.sc_toggle eq 'scb' then begin
                  tmp = where(path_re_scb(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then path_re_scb(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((path_re_scb(j,*,info.cutoff_index0) GE 90) and (path_re_scb(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then path_re_scb(j,tmp,info.cutoff_index0) = (path_re_scb(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((path_re_scb(j,*,info.cutoff_index0) GT 270) and (path_re_scb(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then path_re_scb(j,tmp,info.cutoff_index0) = path_re_scb(j,tmp,info.cutoff_index0) - 360                 
              endif
          endfor
                  
          if info.sc_toggle eq 'sca' then begin
              contour,path_re_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          if info.sc_toggle eq 'scb' then begin
              contour,path_re_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          
          a=TVRD() 
          device,/close
          set_plot,thisDevice

          if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'raydist_RE.ps',bits=8,/color
                print,!d.window
            endif
          
          !p.position=[0,0,1,1]
          tvlct,redvector,greenvector,bluevector
          
          px = 0. 
          py = 0.
          tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
          
;plot with no data
          if info.sc_toggle eq 'sca' then begin 
              contour,path_re_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
          if info.sc_toggle eq 'scb' then begin 
              contour,path_re_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
         
          COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 

          xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
          tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
          if(tmp NE ',') then xyouts,0.05,0.16,'ray path distance (RE) for the',/normal,font=0,charsize=1.5,color=255
          if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255
        
      endif ;ray path distance in RE
;#####################
      if info.what_other_but eq 'fce' then begin
          lvl_divisions = 255/5.
          increment = (1)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(1/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

          for j=0,(gridspac-1) do begin
              if info.sc_toggle eq 'sca' then begin
                  tmp = where(f_fce_sca(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then f_fce_sca(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((f_fce_sca(j,*,info.cutoff_index0) GE 90) and (f_fce_sca(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then f_fce_sca(j,tmp,info.cutoff_index0) = (f_fce_sca(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((f_fce_sca(j,*,info.cutoff_index0) GT 270) and (f_fce_sca(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then f_fce_sca(j,tmp,info.cutoff_index0) = f_fce_sca(j,tmp,info.cutoff_index0) - 360                 
              endif           
              if info.sc_toggle eq 'scb' then begin
                  tmp = where(f_fce_scb(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then f_fce_scb(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((f_fce_scb(j,*,info.cutoff_index0) GE 90) and (f_fce_scb(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then f_fce_scb(j,tmp,info.cutoff_index0) = (f_fce_scb(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((f_fce_scb(j,*,info.cutoff_index0) GT 270) and (f_fce_scb(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then f_fce_scb(j,tmp,info.cutoff_index0) = f_fce_scb(j,tmp,info.cutoff_index0) - 360                 
              endif
          endfor
                  
          if info.sc_toggle eq 'sca' then begin
              contour,f_fce_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          if info.sc_toggle eq 'scb' then begin
              contour,f_fce_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          
          a=TVRD() 
          device,/close
          set_plot,thisDevice

          if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'f_fce.ps',bits=8,/color
                print,!d.window
            endif
          
          !p.position=[0,0,1,1]
          tvlct,redvector,greenvector,bluevector
          
          px = 0. 
          py = 0.
          tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
          
;plot with no data
          if info.sc_toggle eq 'sca' then begin 
              contour,f_fce_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
          if info.sc_toggle eq 'scb' then begin 
              contour,f_fce_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
         
          COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 
        
          xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
          tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
          if(tmp NE ',') then xyouts,0.05,0.16,'f/fce for the',/normal,font=0,charsize=1.5,color=255
          if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255

      endif ;f/fce
;###################
      if info.what_other_but eq 'rdx' then begin
          lvl_divisions = 255/5.
          increment = (50)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(50/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

          for j=0,(gridspac-1) do begin
              if info.sc_toggle eq 'sca' then begin
                  tmp = where(refndx_sca(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then refndx_sca(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((refndx_sca(j,*,info.cutoff_index0) GE 90) and (refndx_sca(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then refndx_sca(j,tmp,info.cutoff_index0) = (refndx_sca(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((refndx_sca(j,*,info.cutoff_index0) GT 270) and (refndx_sca(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then refndx_sca(j,tmp,info.cutoff_index0) = refndx_sca(j,tmp,info.cutoff_index0) - 360                 
              endif           
              if info.sc_toggle eq 'scb' then begin
                  tmp = where(refndx_scb(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then refndx_scb(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where((refndx_scb(j,*,info.cutoff_index0) GE 90) and (refndx_scb(j,*,info.cutoff_index0) LE 270))
                  if tmp(0) ne -1 then refndx_scb(j,tmp,info.cutoff_index0) = (refndx_scb(j,tmp,info.cutoff_index0) - 180)*(-1)              
                  tmp = where((refndx_scb(j,*,info.cutoff_index0) GT 270) and (refndx_scb(j,*,info.cutoff_index0) LE 360))
                  if tmp(0) ne -1 then refndx_scb(j,tmp,info.cutoff_index0) = refndx_scb(j,tmp,info.cutoff_index0) - 360                 
              endif
          endfor
                  
          if info.sc_toggle eq 'sca' then begin
              contour,refndx_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          if info.sc_toggle eq 'scb' then begin
              contour,refndx_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          
          a=TVRD() 
          device,/close
          set_plot,thisDevice

          if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'ref_ndx.ps',bits=8,/color
                print,!d.window
            endif
          
          !p.position=[0,0,1,1]
          tvlct,redvector,greenvector,bluevector
          
          px = 0. 
          py = 0.
          tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
          
;plot with no data
          if info.sc_toggle eq 'sca' then begin 
              contour,refndx_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
          if info.sc_toggle eq 'scb' then begin 
              contour,refndx_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
         
          xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
          tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
          if(tmp NE ',') then xyouts,0.05,0.16,'refractive index for the',/normal,font=0,charsize=1.5,color=255
          if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255

          COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 
        
      endif ;refractive index
;#########################
      if info.what_other_but eq 'tgn' then begin
          lvl_divisions = 255/5.
          increment = (80)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment - increment
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(80/divisions)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)

          for j=0,(gridspac-1) do begin
              if info.sc_toggle eq 'sca' then begin
                  tmp = where(theta_gen_sca(j,*,info.cutoff_index0) eq 0.0)
                  if tmp(0) ne -1 then theta_gen_sca(j,tmp,info.cutoff_index0) = -10000.         
                  tmp = where(theta_gen_sca(j,*,info.cutoff_index0) GE 90.)
                  if tmp(0) ne -1 then (theta_gen_sca(j,tmp,info.cutoff_index0) = (theta_gen_sca(j,tmp,info.cutoff_index0) - 180.)*(-1))
              endif           
              if info.sc_toggle eq 'scb' then begin
                  tmp = where(theta_gen_scb(j,*,info.cutoff_index0) eq 0.0)
                  if tmp(0) ne -1 then theta_gen_scb(j,tmp,info.cutoff_index0) = -10000.
                  tmp = where(theta_gen_scb(j,*,info.cutoff_index0) GE 90.)
                  if tmp(0) ne -1 then (theta_gen_scb(j,tmp,info.cutoff_index0) = (theta_gen_scb(j,tmp,info.cutoff_index0) - 180.)*(-1))
              endif
          endfor
                  
          if info.sc_toggle eq 'sca' then begin
              contour,theta_gen_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          if info.sc_toggle eq 'scb' then begin
              contour,theta_gen_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          
          a=TVRD() 
          device,/close
          set_plot,thisDevice

          if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'theta_gen.ps',bits=8,/color
                print,!d.window
            endif
          
          !p.position=[0,0,1,1]
          tvlct,redvector,greenvector,bluevector
          
          px = 0. 
          py = 0.
          tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
          
;plot with no data
          if info.sc_toggle eq 'sca' then begin 
              contour,theta_gen_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
          if info.sc_toggle eq 'scb' then begin 
              contour,theta_gen_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
         
          COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 
        
          xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
          tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
          if(tmp NE ',') then xyouts,0.05,0.16,'Gendrin angle for the',/normal,font=0,charsize=1.5,color=255
          if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255

      endif ;theta gendrin
;###########################################
      if info.what_other_but eq 'trs' then begin
          lvl_divisions = 255/5.
          increment = (30)/lvl_divisions
          lvls = findgen(lvl_divisions+2)*increment + (50)          
          lvls(0) = -10000.          
          tmplvls = findgen(divisions+1)*(30/divisions) + (50)
          strlvls = strtrim(string(tmplvls),2)
          strlvls = strmid(strlvls,0,5)
          for j=0,(gridspac-1) do begin
              if info.sc_toggle eq 'sca' then begin
                  tmp = where(theta_res_sca(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then theta_res_sca(j,tmp,info.cutoff_index0) = -10000.                
                  tmp = where(theta_res_sca(j,*,info.cutoff_index0) GE 90.)
                  if tmp(0) ne -1 then (theta_res_sca(j,tmp,info.cutoff_index0) = (theta_res_sca(j,tmp,info.cutoff_index0) - 180.)*(-1))
              endif           
              if info.sc_toggle eq 'scb' then begin
                  tmp = where(theta_res_scb(j,*,info.cutoff_index0) eq 0.)
                  if tmp(0) ne -1 then theta_res_scb(j,tmp,info.cutoff_index0) = -10000.              
                  tmp = where(theta_res_scb(j,*,info.cutoff_index0) GE 90.)
                  if tmp(0) ne -1 then (theta_res_scb(j,tmp,info.cutoff_index0) = (theta_res_scb(j,tmp,info.cutoff_index0) - 180.)*(-1))
              endif
          endfor
                  
          if info.sc_toggle eq 'sca' then begin
              contour,theta_res_sca(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          if info.sc_toggle eq 'scb' then begin
              contour,theta_res_scb(*,*,info.cutoff_index0),xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255
          endif
          
          a=TVRD() 
          device,/close
          set_plot,thisDevice

          if info.ps1 eq 'yes' then begin
                set_plot,'ps'
                !p.font = 0
                device,filename = 'theta_res.ps',bits=8,/color
                print,!d.window
            endif
          
          !p.position=[0,0,1,1]
          tvlct,redvector,greenvector,bluevector
          
          px = 0. 
          py = 0.
          tv,a,px,py,xsize=1,ysize=1,/normal ;this plots the content of z-buffer to the screen or ps device
          
;plot with no data
          if info.sc_toggle eq 'sca' then begin 
              contour,theta_res_sca(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
          if info.sc_toggle eq 'scb' then begin 
              contour,theta_res_scb(*,*,info.cutoff_index0),xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='Cartesian x - Earth Radii',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=255
          endif
         
          COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=divisions,ticknames=[strlvls],charsize=1.5,color=255 
        
          xyouts,0.05,0.10,'freq ' + '(' + spacecraft + ')' + ' = ' + filestruct.freq(info.cutoff_index0) + ' Hz',/normal,font=0,charsize = 1.5,color=255
          tmp = strmid(filestruct.sc(info.cutoff_index0),3,1)
          if(tmp NE ',') then xyouts,0.05,0.16,'Resonance cone angle for the',/normal,font=0,charsize=1.5,color=255
          if(tmp NE ',') then xyouts,0.05,0.13,'listed freq and spacecraft',/normal,font=0,charsize=1.5,color=255

      endif ;theta resonance           
      if(info.include_rays EQ 'yes') then begin
            if(scone(0) ne -1) then begin
            if letters(info.cutoff_index0) eq 'x' then oplot,xrays1x(*,info.cutoff_index0),zrays1x(*,info.cutoff_index0),color=30,psym=3
            if letters(info.cutoff_index0) eq 'a' then oplot,xrays1a(*,info.cutoff_index0),zrays1a(*,info.cutoff_index0),color=30,psym=3
            if letters(info.cutoff_index0) eq 'b' then oplot,xrays1b(*,info.cutoff_index0),zrays1b(*,info.cutoff_index0),color=30,psym=3
            if letters(info.cutoff_index0) eq 'c' then oplot,xrays1c(*,info.cutoff_index0),zrays1c(*,info.cutoff_index0),color=30,psym=3
stop
            endif          
            if(sctwo(0) ne -1) then begin
            if letters(info.cutoff_index0) eq 'x' then oplot,xrays2x(*,info.cutoff_index0),zrays2x(*,info.cutoff_index0),color=60,psym=3
            if letters(info.cutoff_index0) eq 'a' then oplot,xrays2a(*,info.cutoff_index0),zrays2a(*,info.cutoff_index0),color=60,psym=3
            if letters(info.cutoff_index0) eq 'b' then oplot,xrays2b(*,info.cutoff_index0),zrays2b(*,info.cutoff_index0),color=60,psym=3
            if letters(info.cutoff_index0) eq 'c' then oplot,xrays2c(*,info.cutoff_index0),zrays2c(*,info.cutoff_index0),color=60,psym=3
stop
            endif
;second cc pair           
;            if(scone(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays1x(*,info.selected_freq1),zrays1x(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays1a(*,info.selected_freq1),zrays1a(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays1b(*,info.selected_freq1),zrays1b(*,info.selected_freq1),color=120,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays1c(*,info.selected_freq1),zrays1c(*,info.selected_freq1),color=120,psym=3
;stop
;            endif          
;            if(sctwo(0) ne -1) then begin
;            if letters(info.selected_freq1) eq 'x' then oplot,xrays2x(*,info.selected_freq1),zrays2x(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'a' then oplot,xrays2a(*,info.selected_freq1),zrays2a(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'b' then oplot,xrays2b(*,info.selected_freq1),zrays2b(*,info.selected_freq1),color=190,psym=3
;            if letters(info.selected_freq1) eq 'c' then oplot,xrays2c(*,info.selected_freq1),zrays2c(*,info.selected_freq1),color=190,psym=3
;stop
;            endif



;          if(scone(0) ne -1) then oplot,xrays1(*,info.cutoff_index0),zrays1(*,info.cutoff_index0),color=120,psym=3
;          if(sctwo(0) ne -1) then oplot,xrays2(*,info.cutoff_index0),zrays2(*,info.cutoff_index0),color=160,psym=3
      endif
   endif
   
   plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
   
   print,!d.window
   widget_control,info.print_ps1, set_button=0
   widget_control,info.includerays, set_button=0
   info.include_rays = 'no'
   info.ps1 = 'no'
   
   if(!d.window EQ -1) then begin
       !p.font = -1             ;change font back so that it can be viewed correctly on screen
       device,/close
       set_plot,'x'
   endif
   
;##################################################################################
;##################################################################################
   keepers = 0
      
   if info.cyclebool eq 'yes' then begin

   if keyword_set(diff_work_union) eq 0 then diff_work_union = replicate(-10000.,gridspac,gridspac) 
   if keyword_set(diff_work_intersection) eq 0 then diff_work_intersection = replicate(-10000.,gridspac,gridspac)
   if keyword_set(not_keepers_source) eq 0 then not_keepers_source = fltarr(gridspac,gridspac)

   if info.cyclecount eq 0 then begin
   if keyword_set(freq_all) eq 1 then freq_all = [freq_all,[freqs(0),freqs(4)]]
   if keyword_set(freq_all) eq 0 then freq_all = [freqs(0),freqs(4)]       
   endif

;   if keyword_set(freq_all) eq 1 then begin
;       freq_all = [freq_all,[freqs(0),freqs(4)]]
;       h = where(freq_all eq freqs(0))
;       if h(0) eq -1 then freq_all = [freq_all,freqs(0)]
;       h = where(freq_all eq freqs(4))
;       if h(0) eq -1 then freq_all = [freq_all,freqs(4)]
;   endif
       
       set_plot,'x'
       device,decomposed=0
       for i=0,(gridspac-1) do begin
           keepers = where(diff_work(*,i) ne -10000.)           
           if keepers(0) ne -1 then diff_work_union(keepers,i) = 1
       endfor           
              
       if info.maccyclecount eq 0 then begin  ;cc12
;these arrays are only for the source positions that have not been 
;filled in by the triangulation routine. 

	   unique_freqs(0,0) = freqs(3)  ;sc1
           unique_freqs(1,0) = freqs(7)
 	   unique_freqs(0,1) = freqs(3)  ;sc2
           unique_freqs(1,1) = freqs(7) 

           theta_k_c1_f1 = theta_k_sca(*,*,3)          
           theta_k_c2_f1 = theta_k_scb(*,*,3)
           theta_g_c1_f1 = theta_g_sca(*,*,3)
           theta_g_c2_f1 = theta_g_scb(*,*,3)
           pathre_c1_f1 = path_re_sca(*,*,3)
           pathre_c2_f1 = path_re_scb(*,*,3)
           ffce_c1_f1 = f_fce_sca(*,*,3)
           ffce_c2_f1 = f_fce_scb(*,*,3)
           rdx_c1_f1 = refndx_sca(*,*,3)
           rdx_c2_f1 = refndx_scb(*,*,3)
           tgn_c1_f1 = theta_gen_sca(*,*,3)
           tgn_c2_f1 = theta_gen_scb(*,*,3)
           trs_c1_f1 = theta_res_sca(*,*,3)
           trs_c2_f1 = theta_res_scb(*,*,3)	   
           theta_k_c1_f2 = theta_k_sca(*,*,7)
           theta_k_c2_f2 = theta_k_scb(*,*,7)
           theta_g_c1_f2 = theta_g_sca(*,*,7)
           theta_g_c2_f2 = theta_g_scb(*,*,7)
           pathre_c1_f2 = path_re_sca(*,*,7)
           pathre_c2_f2 = path_re_scb(*,*,7)
           ffce_c1_f2 = f_fce_sca(*,*,7)
           ffce_c2_f2 = f_fce_scb(*,*,7)
           rdx_c1_f2 = refndx_sca(*,*,7)
           rdx_c2_f2 = refndx_scb(*,*,7)
           tgn_c1_f2 = theta_gen_sca(*,*,7)
           tgn_c2_f2 = theta_gen_scb(*,*,7)
           trs_c1_f2 = theta_res_sca(*,*,7)
           trs_c2_f2 = theta_res_scb(*,*,7)
       endif
       if info.maccyclecount eq 1 then begin  ;cc13
                
           tmp = where(unique_freqs(*,0) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c1_f3 = theta_k_sca(*,*,3)        
           theta_g_c1_f3 = theta_g_sca(*,*,3)         
           pathre_c1_f3 = path_re_sca(*,*,3)       
           ffce_c1_f3 = f_fce_sca(*,*,3)         
           rdx_c1_f3 = refndx_sca(*,*,3)        
           tgn_c1_f3 = theta_gen_sca(*,*,3)
           trs_c1_f3 = theta_res_sca(*,*,3)
           unique_freqs(2,0) = freqs(3)
           endif

           tmp = where(unique_freqs(*,0) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c1_f4 = theta_k_sca(*,*,7)        
           theta_g_c1_f4 = theta_g_sca(*,*,7)         
           pathre_c1_f4 = path_re_sca(*,*,7)       
           ffce_c1_f4 = f_fce_sca(*,*,7)         
           rdx_c1_f4 = refndx_sca(*,*,7)        
           tgn_c1_f4 = theta_gen_sca(*,*,7)
           trs_c1_f4 = theta_res_sca(*,*,7)
           unique_freqs(3,0) = freqs(7)
           endif

           theta_k_c3_f1 = theta_k_scb(*,*,3)        
           theta_g_c3_f1 = theta_g_scb(*,*,3)         
           pathre_c3_f1 = path_re_scb(*,*,3)       
           ffce_c3_f1 = f_fce_scb(*,*,3)         
           rdx_c3_f1 = refndx_scb(*,*,3)        
           tgn_c3_f1 = theta_gen_scb(*,*,3)
           trs_c3_f1 = theta_res_scb(*,*,3)
           theta_k_c3_f2 = theta_k_scb(*,*,7)        
           theta_g_c3_f2 = theta_g_scb(*,*,7)         
           pathre_c3_f2 = path_re_scb(*,*,7)       
           ffce_c3_f2 = f_fce_scb(*,*,7)         
           rdx_c3_f2 = refndx_scb(*,*,7)        
           tgn_c3_f2 = theta_gen_scb(*,*,7)
           trs_c3_f2 = theta_res_scb(*,*,7)
           unique_freqs(0,2) = freqs(3)
           unique_freqs(1,2) = freqs(7)
       endif
       if info.maccyclecount eq 2 then begin  ;cc14         

           tmp = where(unique_freqs(*,0) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c1_f5 = theta_k_scb(*,*,3)
           theta_g_c1_f5 = theta_g_scb(*,*,3)         
           pathre_c1_f5 = path_re_scb(*,*,3)       
           ffce_c1_f5 = f_fce_scb(*,*,3)         
           rdx_c1_f5 = refndx_scb(*,*,3)        
           tgn_c1_f5 = theta_gen_scb(*,*,3)
           trs_c1_f5 = theta_res_scb(*,*,3)
           unique_freqs(4,0) = freqs(3)
           endif
       
           tmp = where(unique_freqs(*,0) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c1_f6 = theta_k_scb(*,*,7)
           theta_g_c1_f6 = theta_g_scb(*,*,7)         
           pathre_c1_f6 = path_re_scb(*,*,7)       
           ffce_c1_f6 = f_fce_scb(*,*,7)         
           rdx_c1_f6 = refndx_scb(*,*,7)        
           tgn_c1_f6 = theta_gen_scb(*,*,7)
           trs_c1_f6 = theta_res_scb(*,*,7)
           unique_freqs(5,0) = freqs(7)
           endif

           theta_k_c4_f1 = theta_k_scb(*,*,3)
           theta_g_c4_f1 = theta_g_scb(*,*,3)         
           pathre_c4_f1 = path_re_scb(*,*,3)       
           ffce_c4_f1 = f_fce_scb(*,*,3)         
           rdx_c4_f1 = refndx_scb(*,*,3)        
           tgn_c4_f1 = theta_gen_scb(*,*,3)
           trs_c4_f1 = theta_res_scb(*,*,3)
           theta_k_c4_f2 = theta_k_scb(*,*,7)
           theta_g_c4_f2 = theta_g_scb(*,*,7)         
           pathre_c4_f2 = path_re_scb(*,*,7)       
           ffce_c4_f2 = f_fce_scb(*,*,7)         
           rdx_c4_f2 = refndx_scb(*,*,7)        
           tgn_c4_f2 = theta_gen_scb(*,*,7)
           trs_c4_f2 = theta_res_scb(*,*,7)
           unique_freqs(0,3) = freqs(3)
           unique_freqs(1,3) = freqs(7)
       endif

       if info.maccyclecount eq 3 then begin  ;cc23         

           tmp = where(unique_freqs(*,1) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c2_f3 = theta_k_scb(*,*,3)
           theta_g_c2_f3 = theta_g_scb(*,*,3)         
           pathre_c2_f3 = path_re_scb(*,*,3)       
           ffce_c2_f3 = f_fce_scb(*,*,3)         
           rdx_c2_f3 = refndx_scb(*,*,3)        
           tgn_c2_f3 = theta_gen_scb(*,*,3)
           trs_c2_f3 = theta_res_scb(*,*,3)
           unique_freqs(2,1) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,1) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c2_f4 = theta_k_scb(*,*,7)
           theta_g_c2_f4 = theta_g_scb(*,*,7)         
           pathre_c2_f4 = path_re_scb(*,*,7)       
           ffce_c2_f4 = f_fce_scb(*,*,7)         
           rdx_c2_f4 = refndx_scb(*,*,7)        
           tgn_c2_f4 = theta_gen_scb(*,*,7)
           trs_c2_f4 = theta_res_scb(*,*,7)
           unique_freqs(3,1) = freqs(7)
           endif

           tmp = where(unique_freqs(*,2) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c3_f3 = theta_k_scb(*,*,3)
           theta_g_c3_f3 = theta_g_scb(*,*,3)         
           pathre_c3_f3 = path_re_scb(*,*,3)       
           ffce_c3_f3 = f_fce_scb(*,*,3)         
           rdx_c3_f3 = refndx_scb(*,*,3)        
           tgn_c3_f3 = theta_gen_scb(*,*,3)
           trs_c3_f3 = theta_res_scb(*,*,3)
           unique_freqs(2,2) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,2) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c3_f4 = theta_k_scb(*,*,7)
           theta_g_c3_f4 = theta_g_scb(*,*,7)         
           pathre_c3_f4 = path_re_scb(*,*,7)       
           ffce_c3_f4 = f_fce_scb(*,*,7)         
           rdx_c3_f4 = refndx_scb(*,*,7)        
           tgn_c3_f4 = theta_gen_scb(*,*,7)
           trs_c3_f4 = theta_res_scb(*,*,7)
           unique_freqs(3,2) = freqs(7)
           endif
       endif

       if info.maccyclecount eq 4 then begin  ;cc24         
           tmp = where(unique_freqs(*,1) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c2_f5 = theta_k_scb(*,*,3)
           theta_g_c2_f5 = theta_g_scb(*,*,3)         
           pathre_c2_f5 = path_re_scb(*,*,3)       
           ffce_c2_f5 = f_fce_scb(*,*,3)         
           rdx_c2_f5 = refndx_scb(*,*,3)        
           tgn_c2_f5 = theta_gen_scb(*,*,3)
           trs_c2_f5 = theta_res_scb(*,*,3)
           unique_freqs(4,1) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,1) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c2_f6 = theta_k_scb(*,*,7)
           theta_g_c2_f6 = theta_g_scb(*,*,7)         
           pathre_c2_f6 = path_re_scb(*,*,7)       
           ffce_c2_f6 = f_fce_scb(*,*,7)         
           rdx_c2_f6 = refndx_scb(*,*,7)        
           tgn_c2_f6 = theta_gen_scb(*,*,7)
           trs_c2_f6 = theta_res_scb(*,*,7)
           unique_freqs(5,1) = freqs(7)
           endif
           tmp = where(unique_freqs(*,3) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c4_f3 = theta_k_scb(*,*,3)
           theta_g_c4_f3 = theta_g_scb(*,*,3)         
           pathre_c4_f3 = path_re_scb(*,*,3)       
           ffce_c4_f3 = f_fce_scb(*,*,3)         
           rdx_c4_f3 = refndx_scb(*,*,3)        
           tgn_c4_f3 = theta_gen_scb(*,*,3)
           trs_c4_f3 = theta_res_scb(*,*,3)
           unique_freqs(2,3) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,3) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c4_f4 = theta_k_scb(*,*,7)
           theta_g_c4_f4 = theta_g_scb(*,*,7)         
           pathre_c4_f4 = path_re_scb(*,*,7)       
           ffce_c4_f4 = f_fce_scb(*,*,7)         
           rdx_c4_f4 = refndx_scb(*,*,7)        
           tgn_c4_f4 = theta_gen_scb(*,*,7)
           trs_c4_f4 = theta_res_scb(*,*,7)
           unique_freqs(3,3) = freqs(7)
           endif
       endif

       if info.maccyclecount eq 5 then begin  ;cc34         
           tmp = where(unique_freqs(*,2) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c3_f5 = theta_k_scb(*,*,3)
           theta_g_c3_f5 = theta_g_scb(*,*,3)         
           pathre_c3_f5 = path_re_scb(*,*,3)       
           ffce_c3_f5 = f_fce_scb(*,*,3)         
           rdx_c3_f5 = refndx_scb(*,*,3)        
           tgn_c3_f5 = theta_gen_scb(*,*,3)
           trs_c3_f5 = theta_res_scb(*,*,3)
           unique_freqs(4,2) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,2) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c3_f6 = theta_k_scb(*,*,7)
           theta_g_c3_f6 = theta_g_scb(*,*,7)         
           pathre_c3_f6 = path_re_scb(*,*,7)       
           ffce_c3_f6 = f_fce_scb(*,*,7)         
           rdx_c3_f6 = refndx_scb(*,*,7)        
           tgn_c3_f6 = theta_gen_scb(*,*,7)
           trs_c3_f6 = theta_res_scb(*,*,7)
           unique_freqs(5,2) = freqs(7)
           endif

           tmp = where(unique_freqs(*,3) eq freqs(3))
           if tmp(0) eq -1 then begin
           theta_k_c4_f5 = theta_k_scb(*,*,3)
           theta_g_c4_f5 = theta_g_scb(*,*,3)         
           pathre_c4_f5 = path_re_scb(*,*,3)       
           ffce_c4_f5 = f_fce_scb(*,*,3)         
           rdx_c4_f5 = refndx_scb(*,*,3)        
           tgn_c4_f5 = theta_gen_scb(*,*,3)
           trs_c4_f5 = theta_res_scb(*,*,3)
           unique_freqs(4,3) = freqs(3)
           endif       
           tmp = where(unique_freqs(*,3) eq freqs(7))
           if tmp(0) eq -1 then begin
           theta_k_c4_f6 = theta_k_scb(*,*,7)
           theta_g_c4_f6 = theta_g_scb(*,*,7)         
           pathre_c4_f6 = path_re_scb(*,*,7)       
           ffce_c4_f6 = f_fce_scb(*,*,7)         
           rdx_c4_f6 = refndx_scb(*,*,7)        
           tgn_c4_f6 = theta_gen_scb(*,*,7)
           trs_c4_f6 = theta_res_scb(*,*,7)
           unique_freqs(5,3) = freqs(7)
           endif
       endif

       lvl_divisions = 255/127.5
       increment = (2)/lvl_divisions       
       lvls = [-10000.,0,1.1]                 
       strlvls=['-10000','1','2']
       
       greenvector(255/2.2:255) = 0.   
       bluevector(255/2.2:255) = 0.
       redvector(255/2.2:255) = 255.
       
       greenvector(0:255/2.2) = 255.
       bluevector(0:255/2.2) = 255.
       redvector(0:255/2.2) = 255.
       
       tvlct,redvector,greenvector,bluevector

       contour,diff_work_union,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5;,color=255 ;multiple plots
;       COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=2,ticknames=['x','no','yes'],charsize=1.5 ;,color=255 
       loadct,1

       contour,diff_work_union,xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=0 ;multiple plots 
       plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps              
;#######    
       ;if info.cyclecount eq 9 then begin
       ;    set_plot,'ps'
       ;    !p.font=0
       ;    device,filename = 'overlap-' + filestruct.sc(0) + '.ps',bits=8,/color                  
       ;endif

       lvl_divisions = 255/127.5
       increment = (2)/lvl_divisions       
       lvls = [-10000.,0,1.1]                 
       strlvls=['-10000','1','2']
       
       greenvector(255/2.2:255) = 0.   
       bluevector(255/2.2:255) = 0.
       redvector(255/2.2:255) = 255.
       
       greenvector(0:255/2.2) = 255.
       bluevector(0:255/2.2) = 255.
       redvector(0:255/2.2) = 255.
       
       tvlct,redvector,greenvector,bluevector

       contour,diff_work_union,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255 ;multiple plots
;       COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=2,ticknames=['x','no','yes'],charsize=1.5;,color=255 

       loadct,1
       contour,diff_work_union,xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=0 ;multiple plots
       
       plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps

       ;if info.cyclecount eq 9 then begin
       ;    device,/close
       ;    !p.font=-1
       ;    set_plot,'x'
       ;endif
;#########################################################################
  		if info.cyclecount eq 9 then begin

                   if info.maccyclecount eq 0 then begin
                        not_keepers_cc12 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc12(tmp,i) = 1.
                        endfor
                   endif
                   if info.maccyclecount eq 1 then begin
                        not_keepers_cc13 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc13(tmp,i) = 1.
                        endfor
                   endif
                   if info.maccyclecount eq 2 then begin
                        not_keepers_cc14 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc14(tmp,i) = 1.
                        endfor
                   endif
                   if info.maccyclecount eq 3 then begin
                        not_keepers_cc23 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc23(tmp,i) = 1.
                        endfor
                   endif
                   if info.maccyclecount eq 4 then begin
                        not_keepers_cc24 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc24(tmp,i) = 1.
                        endfor
                   endif
                   if info.maccyclecount eq 5 then begin
                        not_keepers_cc34 = fltarr(gridspac,gridspac)
                        for i=0,(gridspac-1) do begin     
                            tmp = where(diff_work_union(*,i) eq -10000.)
                            if tmp(0) ne -1 then not_keepers_cc34(tmp,i) = 1.
                        endfor
                   endif

;this happens six times and so it represents only the true source
                   for i=0,(gridspac-1) do begin
                       tmp = where(diff_work_union(*,i) eq -10000.)
                       if tmp(0) ne -1 then not_keepers_source(tmp,i) = 1.
                   endfor
	endif	     

;#########################################################################
;now here's where we want to re-triangulate to fill in the spotty areas. 

       if info.cyclecount eq 9 then begin
           for smcnt=0,1 do begin
               
               if smcnt eq 0 then begin
                   vertices = fltarr(200000,2)
                   vertices(*,*) = -10000.
                   ctr = 0
                   for i=0,(gridspac-1) do begin
                       xt = where(diff_work_union(*,i) ne -10000.)
                       if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),0) = xt
                       if xt(0) ne -1 then vertices(ctr:(ctr + n_elements(xt)-1),1) = i
                       ctr = ctr + n_elements(xt)
                   endfor
                   tmp = where(vertices(*,0) ne -10000.)
                 if tmp(0) eq -1 or n_elements(tmp) lt 3 then print,'######NO AREA######'
                 if tmp(0) eq -1 then stop
                 if tmp(0) ne -1 and n_elements(tmp) gt 2 then begin
                   vertices_tmp = fltarr(n_elements(tmp),2)
                   vertices_tmp(*,0) = vertices(tmp,0)
                   vertices_tmp(*,1) = vertices(tmp,1)
                   vertices = vertices_tmp
                   
                   triangulate,vertices(*,0),vertices(*,1),tr2                                 

                   u=0L
                   nelem=n_elements(tr2(0,*))
                   while(u LT nelem) do begin
                       xc=vertices(tr2(*,u),0) ;vertices of current triangle
                       zc=vertices(tr2(*,u),1)
                       
                       for q=0,2 do begin
                           xc(q) = float(xmax) - (gridspac - xc(q))*(float(xmax)-float(xmin))/gridspac
                           zc(q) = float(ymax) - (gridspac - zc(q))*(float(ymax)-float(ymin))/gridspac
                       endfor	
                       
                       sideA=sqrt((xc(0)-xc(1))^2 + (zc(0)-zc(1))^2)
                       sideB=sqrt((xc(2)-xc(0))^2 + (zc(2)-zc(0))^2)
                       sideC=sqrt((xc(2)-xc(1))^2 + (zc(2)-zc(1))^2)
                       
                       test='no'
;0.0157 is equivalent to 100 km resolution. 

                       
                       if(sideA GT maxside) then test='yes'
                       if(sideB GT maxside) then test='yes' ;gets rid of the long triangles that mess up the 
                       if(sideC GT maxside) then test='yes' ;triangulation
                       if(test EQ 'yes') then tr2(*,u) = 0
                       u=u+1
                   endwhile
                   
                   vertx = (limits(2) - limits(0))*vertices(*,0)/(gridspac-1)  + limits(0)
                   verty = (limits(3) - limits(1))*vertices(*,1)/(gridspac-1)  + limits(1)
                                      
                   oplot,vertx,verty,psym=1,color=0
                   for i=0L,n_elements(tr2)/3 - 1 do begin
                       t=[tr2[*,i],tr2[0,i]]
                       plots,vertx(t),verty(t),color=0
                   endfor
                   print,'LONG TRIANGLES REMOVED'
                   stop
                                      
                   z = fltarr(n_elements(vertices(*,0)))
                   z(*)= 1
                   
                   gridspacing = fltarr(2)
                   gridspacing(0) = (limits(2)-limits(0))/(gridspac-1)
                   gridspacing(1) = (limits(3)-limits(1))/(gridspac-1)
                   result=trigrid(vertx,verty,z,tr2,xgrid=xg,ygrid=yg,gridspacing,limits)
	           result = result(0:(gridspac-1),0:(gridspac-1))                   

                   diff_work_union = result
                   
                   x = where(diff_work_union eq 0.)
                   diff_work_union(x) = -10000.
                 endif
               endif
               
               set_plot,'x'
               device,decomposed=0
               
               if smcnt eq 1 then begin
                   set_plot,'ps'
                   !p.font=0
                   device,filename = 'overlap-' + filestruct.sc(0) + '.ps',bits=8,/color                  
               endif
               
               lvl_divisions = 255/127.5
               increment = (2)/lvl_divisions       
               lvls = [-10000.,0,1.1]                 
               strlvls=['-10000','1','2']
               
               greenvector(255/2.2:255) = 0.   
               bluevector(255/2.2:255) = 0.
               redvector(255/2.2:255) = 255.
               
               greenvector(0:255/2.2) = 255.
               bluevector(0:255/2.2) = 255.
               redvector(0:255/2.2) = 255.
               
               tvlct,redvector,greenvector,bluevector
       
;with all of the filled in areas        
               contour,diff_work_union,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union after fill',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5,color=255 ;multiple plots
               
;               if smcnt eq 0 then COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=2,ticknames=['x','no','yes'],charsize=1.5 ;,color=255 
               
               loadct,1
               contour,diff_work_union,xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='union',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=0 ;multiple plots
               
               plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
               
               print,'FINAL STOP'
               if smcnt eq 0 then stop
               
               if smcnt eq 1 then begin
                   device,/close
                   !p.font=-1
                   set_plot,'x'
               endif
           endfor
       endif
   
       info.cyclecount = info.cyclecount + 1
       if info.cyclecount eq 10. then begin                      
           if info.maccyclecount eq 0. then diff_work_intersection = diff_work_union
           info.cyclecount=0
                                ;take intersection of all cc12 with all of cc13           
           
           if info.maccyclecount gt 0 then begin               
               keepers=0
               
               a = where(diff_work_union ne -10000.)
               b = where(diff_work_intersection ne -10000.)
               
;######## here's an intersection routine from Coyote's website ##########
               minab = MIN(a,MAX=maxa,/nan) > MIN(b,Max=maxb,/nan)
               maxab = maxa < maxb
               
               if maxab LT minab OR maxab LT 0 then begin
                   keepers = -1.
                   count = 0.
               endif else begin
                   keepers = 1.
               endelse
               if(keepers NE -1.) then keepers = where((histogram(a,min=minab,max=maxab) NE 0.) AND $
                                                       (histogram(b,min=minab,max=maxab) NE 0.),count)
               if count EQ 0. then keepers=-1
               if count NE 0. then keepers = keepers + minab           
               
               diff_work_intersection(*,*) = -10000.
               if keepers(0) ne -1 then diff_work_intersection(keepers) = 1

               if info.maccyclecount eq 5 then begin
                   set_plot,'ps'
                   !p.font=0
                   device,filename = 'overlap-all.ps',bits=8,/color                  
               endif

               lvl_divisions = 255/127.5
               increment = (2)/lvl_divisions       
               lvls = [-10000.,0,1.1]                 
               strlvls=['-10000','1','2']
               
               greenvector(255/2.2:255) = 0.   
               bluevector(255/2.2:255) = 0.
               redvector(255/2.2:255) = 255.
               
               greenvector(0:255/2.2) = 255.
               bluevector(0:255/2.2) = 255.
               redvector(0:255/2.2) = 255.
               
               tvlct,redvector,greenvector,bluevector

               contour,diff_work_intersection,xgrid,zgrid,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='intersection',ytitle='Cartesian z - Along B-Field',xstyle=5,ystyle=5;,color=255 ;multiple plots                
;               COLORBAR, position=[0.05,0.94,0.95,0.99],divisions=2,ticknames=['x','no','yes'],charsize=1.5;,color=255 

               loadct,1

               contour,diff_work_intersection,xgrid,zgrid,/nodata,position=[0,0,1,1],levels=[lvls],ticklen=1,/fill,xrange=[xmin,xmax],yrange=[ymin,ymax],charsize=1.5,xtitle='intersection',ytitle='Cartesian z - Along B-Field',xstyle=1,ystyle=1,color=0 ;multiple plots
               plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps
              
               if info.maccyclecount eq 5 then begin
                   device,/close
                   !p.font=-1
                   set_plot,'x'
               endif
;###############################################################
;###############################################################             

        	if info.maccyclecount eq 5. and info.cyclecount eq 0. then begin
	            not_keepers_ws = fltarr(gridspac,gridspac)
	
        	           for i=0,(gridspac-1) do begin
                	        tmp = where(diff_work_intersection(*,i) eq -10000.)
	                        if tmp(0) ne -1 then not_keepers_ws(tmp,i) = 1. 
        	           endfor		
	        endif      


;        	if info.maccyclecount eq 5 then save,filename='raydata.dat',theta_k_c1_f1,theta_k_c1_f2,theta_k_c1_f3,theta_k_c1_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f1,theta_g_c1_f2,theta_g_c1_f3,theta_g_c1_f4,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f1,pathre_c1_f2,pathre_c1_f3,pathre_c1_f4,pathre_c1_f5,pathre_c1_f6,ffce_c1_f1,ffce_c1_f2,ffce_c1_f3,ffce_c1_f4,ffce_c1_f5,ffce_c1_f6,rdx_c1_f1,rdx_c1_f2,rdx_c1_f3,rdx_c1_f4,rdx_c1_f5,rdx_c1_f6,tgn_c1_f1,tgn_c1_f2,tgn_c1_f3,tgn_c1_f4,tgn_c1_f5,tgn_c1_f6,trs_c1_f1,trs_c1_f2,trs_c1_f3,trs_c1_f4,trs_c1_f5,trs_c1_f6,theta_k_c2_f1,theta_k_c2_f2,theta_k_c2_f3,theta_k_c2_f4,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f1,theta_g_c2_f2,theta_g_c2_f3,theta_g_c2_f4,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f1,pathre_c2_f2,pathre_c2_f3,pathre_c2_f4,pathre_c2_f5,pathre_c2_f6,ffce_c2_f1,ffce_c2_f2,ffce_c2_f3,ffce_c2_f4,ffce_c2_f5,ffce_c2_f6,rdx_c2_f1,rdx_c2_f2,rdx_c2_f3,rdx_c2_f4,rdx_c2_f5,rdx_c2_f6,tgn_c2_f1,tgn_c2_f2,tgn_c2_f3,tgn_c2_f4,tgn_c2_f5,tgn_c2_f6,trs_c2_f1,trs_c2_f2,trs_c2_f3,trs_c2_f4,trs_c2_f5,trs_c2_f6,theta_k_c3_f1,theta_k_c3_f2,theta_k_c3_f3,theta_k_c3_f4,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f1,theta_g_c3_f2,theta_g_c3_f3,theta_g_c3_f4,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f1,pathre_c3_f2,pathre_c3_f3,pathre_c3_f4,pathre_c3_f5,pathre_c3_f6,ffce_c3_f1,ffce_c3_f2,ffce_c3_f3,ffce_c3_f4,ffce_c3_f5,ffce_c3_f6,rdx_c3_f1,rdx_c3_f2,rdx_c3_f3,rdx_c3_f4,rdx_c3_f5,rdx_c3_f6,tgn_c3_f1,tgn_c3_f2,tgn_c3_f3,tgn_c3_f4,tgn_c3_f5,tgn_c3_f6,trs_c3_f1,trs_c3_f2,trs_c3_f3,trs_c3_f4,trs_c3_f5,trs_c3_f6,theta_k_c4_f1,theta_k_c4_f2,theta_k_c4_f3,theta_k_c4_f4,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f1,theta_g_c4_f2,theta_g_c4_f3,theta_g_c4_f4,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f1,pathre_c4_f2,pathre_c4_f3,pathre_c4_f4,pathre_c4_f5,pathre_c4_f6,ffce_c4_f1,ffce_c4_f2,ffce_c4_f3,ffce_c4_f4,ffce_c4_f5,ffce_c4_f6,rdx_c4_f1,rdx_c4_f2,rdx_c4_f3,rdx_c4_f4,rdx_c4_f5,rdx_c4_f6,tgn_c4_f1,tgn_c4_f2,tgn_c4_f3,tgn_c4_f4,tgn_c4_f5,tgn_c4_f6,trs_c4_f1,trs_c4_f2,trs_c4_f3,trs_c4_f4,trs_c4_f5,trs_c4_f6,xmin,xmax,ymin,ymax,xgrid,zgrid,density,freq_all,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34
       endif ;for maccyclecount gt 0. 

                if info.maccyclecount eq 0 then begin
save,filename='raydata1.dat',theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,xmin,xmax,ymin,ymax,xgrid,zgrid,density,not_keepers_cc12

;           theta_k_c1_f1 = 0 & theta_k_c2_f1 = 0 & theta_g_c1_f1 = 0 & theta_g_c2_f1 = 0 & pathre_c1_f1 = 0 & pathre_c2_f1 = 0
;           ffce_c1_f1 = 0 & ffce_c2_f1 = 0 & rdx_c1_f1 = 0 & rdx_c2_f1 = 0 & tgn_c1_f1 = 0 & tgn_c2_f1 = 0 & trs_c1_f1 = 0
;           trs_c2_f1 = 0 & theta_k_c1_f2 = 0 & theta_k_c2_f2 = 0 & theta_g_c1_f2 = 0 & theta_g_c2_f2 = 0 & pathre_c1_f2 = 0
;           pathre_c2_f2 = 0 & ffce_c1_f2 = 0 & ffce_c2_f2 = 0 & rdx_c1_f2 = 0 & rdx_c2_f2 = 0 & tgn_c1_f2 = 0
;           tgn_c2_f2 = 0 & trs_c1_f2 = 0 & trs_c2_f2 = 0

endif
                if info.maccyclecount eq 1 then begin
save,filename='raydata2.dat',theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,not_keepers_cc13

;           theta_k_c1_f3 = 0 & theta_k_c3_f1 = 0 & theta_g_c1_f3 = 0 & theta_g_c3_f1 = 0 & pathre_c1_f3 = 0 & pathre_c3_f1 = 0
;           ffce_c1_f3 = 0 & ffce_c3_f1 = 0 & rdx_c1_f3 = 0 & rdx_c3_f1 = 0 & tgn_c1_f3 = 0 & tgn_c3_f1 = 0 & trs_c1_f3 = 0
;           trs_c3_f1 = 0 & theta_k_c1_f4 = 0 & theta_k_c3_f2 = 0 & theta_g_c1_f4 = 0 & theta_g_c3_f2 = 0 & pathre_c1_f4 = 0
;           pathre_c3_f2 = 0 & ffce_c1_f4 = 0 & ffce_c3_f2 = 0 & rdx_c1_f4 = 0 & rdx_c3_f2 = 0 & tgn_c1_f4 = 0
;           tgn_c3_f2 = 0 & trs_c1_f4 = 0 & trs_c3_f2 = 0
endif

                if info.maccyclecount eq 2 then begin
save,filename='raydata3.dat',theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,not_keepers_cc14

;           theta_k_c1_f5 = 0 & theta_k_c4_f1 = 0 & theta_g_c1_f5 = 0 & theta_g_c4_f1 = 0 & pathre_c1_f5 = 0 & pathre_c4_f1 = 0
;           ffce_c1_f5 = 0 & ffce_c4_f1 = 0 & rdx_c1_f5 = 0 & rdx_c4_f1 = 0 & tgn_c1_f5 = 0 & tgn_c4_f1 = 0 & trs_c1_f5 = 0
;           trs_c4_f1 = 0 & theta_k_c1_f6 = 0 & theta_k_c4_f2 = 0 & theta_g_c1_f6 = 0 & theta_g_c4_f2 = 0 & pathre_c1_f6 = 0
;           pathre_c4_f2 = 0 & ffce_c1_f6 = 0 & ffce_c4_f2 = 0 & rdx_c1_f6 = 0 & rdx_c4_f2 = 0 & tgn_c1_f6 = 0
;           tgn_c4_f2 = 0 & trs_c1_f6 = 0 & trs_c4_f2 = 0

endif

                if info.maccyclecount eq 3 then begin
save,filename='raydata4.dat',theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,not_keepers_cc23

;           theta_k_c2_f3 = 0 & theta_k_c3_f3 = 0 & theta_g_c2_f3 = 0 & theta_g_c3_f3 = 0 & pathre_c2_f3 = 0 & pathre_c3_f3 = 0
;           ffce_c2_f3 = 0 & ffce_c3_f3 = 0 & rdx_c2_f3 = 0 & rdx_c3_f3 = 0 & tgn_c2_f3 = 0 & tgn_c3_f3 = 0 & trs_c2_f3 = 0
;           trs_c3_f3 = 0 & theta_k_c2_f4 = 0 & theta_k_c3_f4 = 0 & theta_g_c2_f4 = 0 & theta_g_c3_f4 = 0 & pathre_c2_f4 = 0
;           pathre_c3_f4 = 0 & ffce_c2_f4 = 0 & ffce_c3_f4 = 0 & rdx_c2_f4 = 0 & rdx_c3_f4 = 0 & tgn_c2_f4 = 0
;           tgn_c3_f4 = 0 & trs_c2_f4 = 0 & trs_c3_f4 = 0
endif

                if info.maccyclecount eq 4 then begin 
save,filename='raydata5.dat',theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,not_keepers_cc24

;           theta_k_c2_f5 = 0 & theta_k_c4_f3 = 0 & theta_g_c2_f5 = 0 & theta_g_c4_f3 = 0 & pathre_c2_f5 = 0 & pathre_c4_f3 = 0
;           ffce_c2_f5 = 0 & ffce_c4_f3 = 0 & rdx_c2_f5 = 0 & rdx_c4_f3 = 0 & tgn_c2_f5 = 0 & tgn_c4_f3 = 0 & trs_c2_f5 = 0
;           trs_c4_f3 = 0 & theta_k_c2_f6 = 0 & theta_k_c4_f4 = 0 & theta_g_c2_f6 = 0 & theta_g_c4_f4 = 0 & pathre_c2_f6 = 0
;           pathre_c4_f4 = 0 & ffce_c2_f6 = 0 & ffce_c4_f4 = 0 & rdx_c2_f6 = 0 & rdx_c4_f4 = 0 & tgn_c2_f6 = 0
;           tgn_c4_f4 = 0 & trs_c2_f6 = 0 & trs_c4_f4 = 0

endif

                if info.maccyclecount eq 5 then begin
save,filename='raydata6.dat',theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,unique_freqs,not_keepers_cc34,not_keepers_source,not_keepers_ws,freq_all

;           theta_k_c3_f5 = 0 & theta_k_c4_f5 = 0 & theta_g_c3_f5 = 0 & theta_g_c4_f5 = 0 & pathre_c3_f5 = 0 & pathre_c4_f5 = 0
;           ffce_c3_f5 = 0 & ffce_c4_f5 = 0 & rdx_c3_f5 = 0 & rdx_c4_f5 = 0 & tgn_c3_f5 = 0 & tgn_c4_f5 = 0 & trs_c3_f5 = 0
;           trs_c4_f5 = 0 & theta_k_c3_f6 = 0 & theta_k_c4_f6 = 0 & theta_g_c3_f6 = 0 & theta_g_c4_f6 = 0 & pathre_c3_f6 = 0
;           pathre_c4_f6 = 0 & ffce_c3_f6 = 0 & ffce_c4_f6 = 0 & rdx_c3_f6 = 0 & rdx_c4_f6 = 0 & tgn_c3_f6 = 0
;           tgn_c4_f6 = 0 & trs_c3_f6 = 0 & trs_c4_f6 = 0
endif



           widget_control,info.b44,set_button=0
           info.maccyclecount = info.maccyclecount + 1
           filestruct.read_in(*) = 'no'
           diff_work_union(*,*) = -10000.
       endif ;for cyclecount eq 10.

     
   endif  ;for 
end
;######################################################################################
;####################################################################################
;######################################################################################
;####################################################################################
pro plot_radials,x1,y1,y2,y3,y4,y5,y10,y20,y30,y40,y50,y60,yn1,yn2,yn3,yn4,yn5,yn10,yn20,yn30,yn40,yn50,yn60,xsc,zsc,x_ps,y_ps

  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs
	

;  if info.cyclebool eq 'yes' then color=0
;  if info.cyclebool ne 'yes' then color=255

  color=200

  oplot,x1,y1,psym=2,symsize=0.1,color=color & oplot,x1,y2,psym=2,symsize=0.1,color=color & oplot,x1,y3,psym=2,symsize=0.1,color=color 
  oplot,x1,y4,psym=2,symsize=0.1,color=color & oplot,x1,y5,psym=2,symsize=0.1,color=color & oplot,x1,y10,psym=2,symsize=0.1,color=color
  oplot,x1,y20,psym=2,symsize=0.1,color=color & oplot,x1,y30,psym=2,symsize=0.1,color=color & oplot,x1,y40,psym=2,symsize=0.1,color=color
  oplot,x1,y50,psym=2,symsize=0.1,color=color & oplot,x1,y60,psym=2,symsize=0.1,color=color & oplot,x1,yn2,psym=2,symsize=0.1,color=color
  oplot,x1,yn3,psym=2,symsize=0.1,color=color & oplot,x1,yn4,psym=2,symsize=0.1,color=color & oplot,x1,yn5,psym=2,symsize=0.1,color=color
  oplot,x1,yn10,psym=2,symsize=0.1,color=color & oplot,x1,yn20,psym=2,symsize=0.1,color=color & oplot,x1,yn30,psym=2,symsize=0.1,color=color
  oplot,x1,yn40,psym=2,symsize=0.1,color=color & oplot,x1,yn50,psym=2,symsize=0.1,color=color & oplot,x1,yn60,psym=2,symsize=0.1,color=color
  oplot,x1,yn1,psym=2,symsize=0.1,color=color 

  foo = where(filestruct.filename NE '',n_files)
  which_sc = strarr(n_files)
  which_sc = filestruct.sc(foo)

  tmp1 = where(which_sc EQ 'c1',c1)
  tmp2 = where(which_sc EQ 'c2',c2)
  tmp3 = where(which_sc EQ 'c3',c3)
  tmp4 = where(which_sc EQ 'c4',c4)
  tmp5 = where(which_sc NE 'c1' AND which_sc NE 'c2' AND which_sc NE 'c3' AND which_sc NE 'c4',c5)
;all of the elements of tmp5 will be the same since you can only load
;one type of tt3 file at a time.

  if(c5 NE 0) then begin
      ;now I must use the filename to figure out which two sc to plot.
      if(filestruct.sc(tmp5(0)) EQ 'cc12') then oplot,[xsc(0),xsc(1)],[zsc(0),zsc(1)],psym=7,color=color
      if(filestruct.sc(tmp5(0)) EQ 'cc13') then oplot,[xsc(0),xsc(2)],[zsc(0),zsc(2)],psym=7,color=color
      if(filestruct.sc(tmp5(0)) EQ 'cc14') then oplot,[xsc(0),xsc(3)],[zsc(0),zsc(3)],psym=7,color=color
      if(filestruct.sc(tmp5(0)) EQ 'cc23') then oplot,[xsc(1),xsc(2)],[zsc(1),zsc(2)],psym=7,color=color
      if(filestruct.sc(tmp5(0)) EQ 'cc24') then oplot,[xsc(1),xsc(3)],[zsc(1),zsc(3)],psym=7,color=color
      if(filestruct.sc(tmp5(0)) EQ 'cc34') then oplot,[xsc(2),xsc(3)],[zsc(2),zsc(3)],psym=7,color=color
  endif

  if(c1 NE 0) then oplot,[xsc(0),0],[zsc(0),0],psym=7,color=color
  if(c2 NE 0) then oplot,[xsc(1),0],[zsc(1),0],psym=7,color=color
  if(c3 NE 0) then oplot,[xsc(2),0],[zsc(2),0],psym=7,color=color
  if(c4 NE 0) then oplot,[xsc(3),0],[zsc(3),0],psym=7,color=color

;if we are doing forward ray-tracing from the source to sc then oplot
;all of the sc and the source
  if which_sc(0) eq 'src' then oplot,xsc,zsc,psym=7,color=color

;  OPLOT,3.3*x_ps,y_ps,color=color  ;PLASMASPHERE
  OPLOT,4*x_ps,4*y_ps,color=color ;L=4
  oplot,5*x_ps,5*y_ps,color=color ;L=5
  oplot,6*x_ps,6*y_ps,color=color ;L=6

;find the L-shell for the sc

  rsc = sqrt(xsc^2 + zsc^2)
  latsc = asin(zsc/float(rsc))
  tmp = where(finite(latsc) NE 0.)
  latsc = latsc(tmp)
  rsc = rsc(tmp)
  lshell = rsc/(sin((!pi/2.)-latsc)^2)
  
  for i=0,n_elements(lshell)-1 do oplot,lshell(i)*x_ps,lshell(i)*y_ps,color=color

  if info.plot_grid eq 'yes' then begin
      for i=0,(gridspac-1) do begin
          for j=0,(gridspac-1) do begin
              xtmp = fltarr(1)
              xtmp(0) = xgrid(i)
              ztmp = fltarr(1)
              ztmp(0) = zgrid(j)
              oplot,xtmp,ztmp,psym=1,color=color
          endfor
      endfor
      info.plot_grid = 'no'
      widget_control,info.plot_grd,set_button = 0
  endif
end
;##################################################################################################################
;######################################################################################
;####################################################################################

pro corr_plot,xmaximum,xminimum,ymaximum,yminimum,xplot_val,zplot_val,draw2ID,ps,alt,rval,lat,cr_time_max,cr_time_min,base_time,filter_to_firstfreq,xmin_corr,xmax_corr,ymin_corr,ymax_corr,pm_firsttime
;  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs

  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs

  print,!d.window
  widget_control,info.print_ps1, set_button=0 ;make sure the 'print ray plot to ps' is not selected because
;it will cause errors.
  info.ps1 = 'no'
  print,!d.window

  foo = where(filestruct.filename NE '') ;the correlation involves all the files
 
  n_freqs = n_elements(foo)   ;reduced matrices off of filestruct.xxx
  freqs = fltarr(n_freqs)     
  freqs = filestruct.freq(foo)
  which_sc = strarr(n_freqs)
  which_sc = filestruct.sc(foo)

  widget_control,info.dt1,get_value = dt1
  widget_control,info.dt2,get_value = dt2
  widget_control,info.dt3,get_value = dt3
  widget_control,info.dt4,get_value = dt4
  widget_control,info.dt5,get_value = dt5
  widget_control,info.dt6,get_value = dt6

  xgrid_pos = (gridspac-1)*(xplot_val - xminimum)/(xmaximum - xminimum)
  zgrid_pos = (gridspac-1)*(zplot_val - yminimum)/(ymaximum - yminimum) ;coordinates of the n_names*gridspac x gridspac grid
  xgrid_pos = round(xgrid_pos)
  zgrid_pos = round(zgrid_pos)

  times = fltarr(n_freqs)
  for i=0,n_freqs-1 do begin

 ;the timing values at the point of mouse click
      if i eq 0 then times(0) = raytimes(xgrid_pos,zgrid_pos,0) + 0.001*dt1
      if i eq 1 then times(1) = raytimes(xgrid_pos,zgrid_pos,1) + 0.001*dt2
      if i eq 2 then times(2) = raytimes(xgrid_pos,zgrid_pos,2) + 0.001*dt3
      if i eq 3 then times(3) = raytimes(xgrid_pos,zgrid_pos,3) + 0.001*dt4
      if i eq 4 then times(4) = raytimes(xgrid_pos,zgrid_pos,4) + 0.001*dt5
      if i eq 5 then times(5) = raytimes(xgrid_pos,zgrid_pos,5) + 0.001*dt6
  endfor 

  if(filestruct.tt(0) EQ 'tt1') then begin
      minimumtime = min(times) ;can't have negative times for tt1
      tp = where(times NE 0)
      if tp(0) NE -1 then times(tp) = times(tp) - minimumtime + 0.1  ;offsets times to zero
                                ;the + 0.1 is important so the
;following line of code does not get rid of this element
  endif
;#########
  fooboo = n_elements(freqs)
  tmp_times = times(0:fooboo-1)  ;cut off array at the number of files
  nofile=where(tmp_times EQ 0.0,nfcnt) 

  tmp = where(times NE 0.)
  if(tmp(0) EQ -1) then begin   ;if there are no files
      tmp_times = fltarr(10)
      tmp_times(*) = 0
      freqs = fltarr(10)
      freqs(*) = 0
  endif
;#########
  help,tmp_times
  tmp_times = 1000*tmp_times   ;in milliseconds
  print,'tmp times: ',tmp_times
;########
  !p.multi = [0,2,2]
;##########
  tmp_sc1 = where(which_sc EQ 'c1')
  tmp_sc2 = where(which_sc EQ 'c2')
  tmp_sc3 = where(which_sc EQ 'c3')
  tmp_sc4 = where(which_sc EQ 'c4')
  ;tmp_both = where(which_sc EQ 'both')  ;for plotting tt3
  tmp_both = where(which_sc NE 'c1' AND which_sc NE 'c2' AND which_sc NE 'c3' AND which_sc NE 'c4')

;########
  times_red1 = fltarr(1)
  times_red2 = fltarr(1)
  freqs_red1 = fltarr(1)
  freqs_red2 = fltarr(1)
  times_red1(0) = 1000*times(info.selected_freq0) ;for oplotting the red boxes and x 'es in the corr-plot.
  times_red2(0) = 1000*times(info.selected_freq1) ;they represent the limiting freqs set by selected_freq0
  freqs_red1(0) = freqs(info.selected_freq0)      ;and selected_freq1
  freqs_red2(0) = freqs(info.selected_freq1)  

  print,'selected_freq0 ',info.selected_freq0
  print,'selected_freq1 ',info.selected_freq1
  print,'extreme times ',times_red1(0),times_red2(0)
  print,'extreme freqs ',freqs_red1(0),freqs_red2(0)
  print,'tmp_times ',tmp_times
  print,'freqs ',freqs
  print,'which_sc ', which_sc
  print,'tmp_sc1 ', tmp_sc1
  print,'tmp_sc2 ', tmp_sc2
;###############################################################################
;######  straight up timing values for a single spacecraft #####################
;###############################################################################

  ;only sc1 files
  if(tmp_sc1(0) NE -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc3(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      plot,tmp_times,freqs,position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spectrogram for Sc1',psym=7,xstyle=1,ystyle=1,color=120
      if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=7     
      if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=7
      if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1  ;no file at this point for a 
;particular freq.
  endif     

  ;only sc2 files
  if(tmp_sc2(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc3(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      plot,tmp_times,freqs,position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spectrogram for Sc2',psym=7,xstyle=1,ystyle=1,color=120
      if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=7     
      if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=7
      if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
  endif

  ;only sc3 files
  if(tmp_sc3(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      plot,tmp_times,freqs,position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spectrogram for Sc3',psym=7,xstyle=1,ystyle=1,color=120
      if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=7     
      if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=7
      if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
  endif     

  ;only sc4 files
  if(tmp_sc4(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc3(0) EQ -1) then begin
      plot,tmp_times,freqs,position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spectrogram for Sc4',psym=7,xstyle=1,ystyle=1,color=120
      if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=7     
      if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=7
      if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
  endif    
;################################################################
  ;only tt3 files
  if(tmp_both(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc3(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      str1='Spectrogram Correlation b/t ' + filestruct.sc(0)
      str2= 'Time differences for a single freq are Scy-Scx, where x < y'
      plot,tmp_times,freqs,position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Delay (msec)',title=str1 + '!C' + str2,psym=7,xstyle=1,ystyle=1,charsize = 0.8,color=120
      if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=7     
      if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=7
      if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
  endif     
;################################################################

;#############################################################################
;both sc1 and sc2 files
;#############################################################################
;simulates the straight up timing values for sc1 and sc2
; sc1 -- psym=2 Asterisk
; sc2 -- psym=6 Square
; sc3 -- psym=5 Triangle
; sc4 -- psym=4 Diamond


  if(tmp_sc1(0) NE -1) AND (tmp_sc2(0) NE -1) AND (tmp_sc3(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc2 (Square)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc2(Square)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc2(Square)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc2(Square)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif

;#############################################################################
  ;both sc1 and sc3 files
;#############################################################################
  if(tmp_sc1(0) NE -1) AND (tmp_sc3(0) NE -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
    
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc3(Triangle)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc3(Triangle)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc3(Triangle)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc3(Triangle)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif

;#############################################################################
  ;both sc1 and sc4 files
;#############################################################################
  if(tmp_sc1(0) NE -1) AND (tmp_sc4(0) NE -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc3(0) EQ -1) then begin
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc4 (Diamond)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c1') AND (filestruct.sc(info.selected_freq1) EQ 'c1') then begin
          plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc4 (Diamond)',psym=2,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=2     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=2
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc1 (Asterisk) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc1),freqs(tmp_sc1),psym=2
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif

;#############################################################################
;both sc2 and sc3 files
;#############################################################################
  if(tmp_sc2(0) NE -1) AND (tmp_sc3(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc3 (Triangle)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc3 (Triangle)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc3 (Triangle)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc3 (Triangle)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif
  
;#############################################################################
  ;both sc2 and sc4 files
;#############################################################################
  if(tmp_sc2(0) NE -1) AND (tmp_sc4(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc3(0) EQ -1) then begin
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc4 (Diamond)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c2') AND (filestruct.sc(info.selected_freq1) EQ 'c2') then begin
          plot,tmp_times(tmp_sc2),freqs(tmp_sc2),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc4 (Diamond)',psym=6,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=6     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=6
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc2 (Square) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif

;#############################################################################
  ;both sc3 and sc4 files
;#############################################################################
  if(tmp_sc3(0) NE -1) AND (tmp_sc4(0) NE -1) AND (tmp_sc1(0) EQ -1) AND (tmp_sc2(0) EQ -1) then begin
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc3 (Triangle) and Sc4 (Diamond)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc3 (Triangle) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c3') AND (filestruct.sc(info.selected_freq1) EQ 'c3') then begin
          plot,tmp_times(tmp_sc3),freqs(tmp_sc3),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc3 (Triangle) and Sc4 (Diamond)',psym=5,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc4),freqs(tmp_sc4),psym=4
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=5     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=5
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
      if(filestruct.sc(info.selected_freq0) EQ 'c4') AND (filestruct.sc(info.selected_freq1) EQ 'c4') then begin
          plot,tmp_times(tmp_sc4),freqs(tmp_sc4),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='Frequency (Hz)',xtitle='Time (msec)',title='F/T Spec for Sc3 (Triangle) and Sc4 (Diamond)',psym=4,xstyle=1,ystyle=1,color=120
          oplot,tmp_times(tmp_sc3),freqs(tmp_sc3),psym=5
          if (times_red1(0) NE 0) then oplot,times_red1,freqs_red1,color=120,psym=4     
          if (times_red2(0) NE 0) then oplot,times_red2,freqs_red2,color=120,psym=4
          if(nfcnt NE 0) then oplot,tmp_times(nofile),freqs(nofile),psym=1
      endif
  endif
;###############################################
  ;neither sc1,sc2,sc3 or sc4
;  if(tmp_sc1(0) EQ -1) AND (tmp_sc2(0) EQ -1) AND (tmp_sc3(0) EQ -1) AND (tmp_sc4(0) EQ -1) then begin
;      print,'NO DATA POINTS TO PLOT'
;  endif
;###############################################

;  if(tmp_sc1(0) NE -1) then begin
;      plot,tmp_times(tmp_sc1),freqs(tmp_sc1),position =[0.15,0.20,0.70,0.85],xrange=[xmin_corr,xmax_corr],yrange=[ymin_corr,ymax_corr],ytitle='frequency',xtitle='time(ms)',title='Freq by freq correlation',psym=7
;      oplot,goobar0,foobar0,color=120,psym=7
;  endif
;  if(tmp_sc2(0) NE -1) then begin
;      oplot,tmp_times(tmp_sc2),freqs(tmp_sc2),psym=6
;      oplot,goobar1,foobar1,color=120,psym=6
;  endif
  

  rval = sqrt(xplot_val^2 + zplot_val^2)
  lat = atan(zplot_val,xplot_val)
  lat = lat*180./!pi
  alt = 6370.*rval - 6370.

  xplot_val = string(xplot_val)
  tmp = strpos(xplot_val,'.')
  xplot_val = strmid(xplot_val,0,tmp+3)
  zplot_val = string(zplot_val)
  tmp = strpos(zplot_val,'.')
  zplot_val = strmid(zplot_val,0,tmp+3)
  lat = string(lat)
  lat = strtrim(lat,2)
  tmp = strpos(lat,'.')
  lat = strmid(lat,0,tmp+3)
  alt = string(alt)
  alt = strtrim(alt,2)
  tmp = strpos(alt,'.')
  alt = strmid(alt,0,tmp)
  rval = string(rval)
  rval = strtrim(rval,2)
  tmp = strpos(rval,'.')
  rval = strmid(rval,0,tmp+3)
  ;tmp = strpos(cr_time_max,'.')
  ;cr_time_max = strmid(cr_time_max,0,tmp+4)
  ;tmp = strpos(cr_time_min,'.')
  ;cr_time_min = strmid(cr_time_min,0,tmp+4)

  plot,position=[0.75,0.1,0.95,0.9],strarr(5),xstyle=4,ystyle=4,/nodata
  xyouts,0,0.8,'x = ' + xplot_val + ' RE',charsize=1.0
  xyouts,0,0.7,'y = ' + zplot_val + ' RE',charsize=1.0
  xyouts,0,0.6,'r = ' + rval + ' RE',charsize=1.0
  xyouts,0,0.5,'lat = ' + lat + ' deg',charsize=1.0
  xyouts,0,0.4,'alt = ' + alt + ' km',charsize=1.0
  tmp1 = float(cr_time_max)
  tmp1 = fix(tmp1)
  tmp1 = strtrim(string(tmp1),2)
  tmp2 = float(cr_time_min)
  tmp2 = fix(tmp2)
  tmp2 = strtrim(string(tmp2),2)

  xyouts,0,0.3,'max filter time:',charsize=0.8
  xyouts,0,0.23,tmp1 + ' msec',charsize=0.8
  xyouts,0,0.15,'min filter time:',charsize=0.8
  xyouts,0,0.09,tmp2 + ' msec',charsize=0.8
  xyouts,0,0.02,'min freq time:',charsize=0.8
  tmp1 = fix(base_time)
  tmp1 = strtrim(string(tmp1),2)
  tmp2 = fix(pm_firsttime)
  tmp2 = strtrim(string(tmp2),2)
  xyouts,0,-0.05,tmp1 + ' +/- ' + tmp2 + ' msec',charsize=0.8

  print,!d.window
  widget_control,info.print_ps2, set_button=0
  info.ps2 = 'no'

  if(!d.window EQ -1) then begin
      device,/close
      set_plot,'x'
  endif
  print,!d.window
end

;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
;###########################################################################
pro xplot
;  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs

  common arrays,raytimes,xgrid,zgrid,allinfo,xrays1x,zrays1x,xrays2x,zrays2x,xrays1a,zrays1a,xrays2a,zrays2a,xrays1b,zrays1b,xrays2b,zrays2b,xrays1c,zrays1c,xrays2c,zrays2c,selected_freq,filestruct,fileindexx,xpix_pos,ypix_pos,xpix_pos2,ypix_pos2,info,timetype,limits,theta_k_sca,theta_g_sca,path_re_sca,f_fce_sca,refndx_sca,theta_gen_sca,theta_res_sca,theta_k_scb,theta_g_scb,path_re_scb,f_fce_scb,refndx_scb,theta_gen_scb,theta_res_scb,density,diff_work_union,diff_work_intersection,theta_k_c1_f1,theta_k_c1_f2,theta_g_c1_f1,theta_g_c1_f2,pathre_c1_f1,pathre_c1_f2,ffce_c1_f1,ffce_c1_f2,rdx_c1_f1,rdx_c1_f2,tgn_c1_f1,tgn_c1_f2,trs_c1_f1,trs_c1_f2,theta_k_c2_f1,theta_k_c2_f2,theta_g_c2_f1,theta_g_c2_f2,pathre_c2_f1,pathre_c2_f2,ffce_c2_f1,ffce_c2_f2,rdx_c2_f1,rdx_c2_f2,tgn_c2_f1,tgn_c2_f2,trs_c2_f1,trs_c2_f2,theta_k_c3_f1,theta_k_c3_f2,theta_g_c3_f1,theta_g_c3_f2,pathre_c3_f1,pathre_c3_f2,ffce_c3_f1,ffce_c3_f2,rdx_c3_f1,rdx_c3_f2,tgn_c3_f1,tgn_c3_f2,trs_c3_f1,trs_c3_f2,theta_k_c4_f1,theta_k_c4_f2,theta_g_c4_f1,theta_g_c4_f2,pathre_c4_f1,pathre_c4_f2,ffce_c4_f1,ffce_c4_f2,rdx_c4_f1,rdx_c4_f2,tgn_c4_f1,tgn_c4_f2,trs_c4_f1,trs_c4_f2,theta_k_c1_f3,theta_k_c1_f4,theta_g_c1_f3,theta_g_c1_f4,pathre_c1_f3,pathre_c1_f4,ffce_c1_f3,ffce_c1_f4,rdx_c1_f3,rdx_c1_f4,tgn_c1_f3,tgn_c1_f4,trs_c1_f3,trs_c1_f4,theta_k_c2_f3,theta_k_c2_f4,theta_g_c2_f3,theta_g_c2_f4,pathre_c2_f3,pathre_c2_f4,ffce_c2_f3,ffce_c2_f4,rdx_c2_f3,rdx_c2_f4,tgn_c2_f3,tgn_c2_f4,trs_c2_f3,trs_c2_f4,theta_k_c3_f3,theta_k_c3_f4,theta_g_c3_f3,theta_g_c3_f4,pathre_c3_f3,pathre_c3_f4,ffce_c3_f3,ffce_c3_f4,rdx_c3_f3,rdx_c3_f4,tgn_c3_f3,tgn_c3_f4,trs_c3_f3,trs_c3_f4,theta_k_c4_f3,theta_k_c4_f4,theta_g_c4_f3,theta_g_c4_f4,pathre_c4_f3,pathre_c4_f4,ffce_c4_f3,ffce_c4_f4,rdx_c4_f3,rdx_c4_f4,tgn_c4_f3,tgn_c4_f4,trs_c4_f3,trs_c4_f4,theta_k_c1_f5,theta_k_c1_f6,theta_g_c1_f5,theta_g_c1_f6,pathre_c1_f5,pathre_c1_f6,ffce_c1_f5,ffce_c1_f6,rdx_c1_f5,rdx_c1_f6,tgn_c1_f5,tgn_c1_f6,trs_c1_f5,trs_c1_f6,theta_k_c2_f5,theta_k_c2_f6,theta_g_c2_f5,theta_g_c2_f6,pathre_c2_f5,pathre_c2_f6,ffce_c2_f5,ffce_c2_f6,rdx_c2_f5,rdx_c2_f6,tgn_c2_f5,tgn_c2_f6,trs_c2_f5,trs_c2_f6,theta_k_c3_f5,theta_k_c3_f6,theta_g_c3_f5,theta_g_c3_f6,pathre_c3_f5,pathre_c3_f6,ffce_c3_f5,ffce_c3_f6,rdx_c3_f5,rdx_c3_f6,tgn_c3_f5,tgn_c3_f6,trs_c3_f5,trs_c3_f6,theta_k_c4_f5,theta_k_c4_f6,theta_g_c4_f5,theta_g_c4_f6,pathre_c4_f5,pathre_c4_f6,ffce_c4_f5,ffce_c4_f6,rdx_c4_f5,rdx_c4_f6,tgn_c4_f5,tgn_c4_f6,trs_c4_f5,trs_c4_f6,freq_all,gridspac,not_keepers_source,not_keepers_ws,not_keepers_cc12,not_keepers_cc13,not_keepers_cc14,not_keepers_cc23,not_keepers_cc24,not_keepers_cc34,unique_freqs

  filestruct = {fullpath:strarr(8),filename:strarr(8),freq:strarr(8),sc:strarr(8),read_in:strarr(8),tt:strarr(8),plot:strarr(8)} ;contains all useful information about the files.
  
  filestruct.read_in(*) = 'empty'
  filestruct.plot(*) = 'no'

  include_rays = 'no'
  xpix_pos = 0d
  ypix_pos = 0d
  xpix_pos2 = 0d
  ypix_pos2 = 0d
  fileindexx=0
  ps1 = 'no'   ;'yes' to save current plot to a postscript file.
  ps2 = 'no'

  plotwin_xsize=515 ;I can't go any bigger than these values or the window clips (even though it doesn't do so visually)
  plotwin_ysize=510 ;trust me!  Don't make larger!!!

                                ;selected_freq = 1  ;used to determine
;which element in structure contains the last file
  selected_freq0=0
  selected_freq1=0
  cutoff_index0=100 ;distinguishable value because this may not be used.
  cutoff_index1=100
;  xgrid = fltarr(gridspac)
;  zgrid = fltarr(gridspac)
  firstfreq = 0.
  finalfreq = 0.

  xsize_win=1050
  ysize_win=800 ;used for the bases
;###########################
  main_base = widget_base(title='test',/row,/frame,xsize=xsize_win,ysize=ysize_win)
  left_base = widget_base(main_base,/column,/frame,xsize=xsize_win*0.5)  
  left_base_top = widget_base(left_base,/column)
  left_base_bottom = widget_base(left_base,/row)
  left_base_bottomleft = widget_base(left_base_bottom)
  left_base_bottomright = widget_base(left_base_bottom,/column)
  right_base=widget_base(main_base,/column,xsize=xsize_win*0.47,ysize=ysize_win,/base_align_left)
  file_button_base = widget_base(right_base,/row)
  file_base = widget_base(right_base,/row)
  bottom_base = widget_base(right_base,/row)
  bottom_left_base = widget_base(bottom_base,/column)
  bottom_right_base = widget_base(bottom_base,/row)
  
  plot_params_base=widget_base(bottom_left_base,/column)
  plot_params_base_upper = widget_base(plot_params_base,/row)
  plot_params_base_lower = widget_base(plot_params_base,/row)
  corr_params_base=widget_base(bottom_left_base,/column)
  corr_params_base_top = widget_base(corr_params_base,/column)
  corr_params_base_bottom = widget_base(corr_params_base,/column)
  coord_label_base = widget_base(bottom_right_base,row=4)
  ;coord_label_base2 = widget_base(bottom_right_base,/column)
  print_ps_base = widget_base(coord_label_base,row=4,/nonexclusive)
  min_max_base = widget_base(file_base,column=1)
  checkbox_base = widget_base(file_base,column=1,/nonexclusive)
  filebox_base = widget_base(file_base,/column)
  filebox_base_upper = widget_base(filebox_base,column=1,/nonexclusive)
  filebox_base_lower = widget_base(filebox_base,column=1)
  select_base = widget_base(file_base,/column,/exclusive) ;select first and final freq to take delta time
  select_base2 = widget_base(file_base,/column,/exclusive)
  cutoff1_base = widget_base(file_base,/column,/nonexclusive)
  cutoff2_base = widget_base(file_base,/column,/nonexclusive)
  deltatime_base = widget_base(file_base,/column)
  absolute_bottom = widget_base(right_base,/row)
  absolute_bottom_left = widget_base(absolute_bottom,/column)
  absolute_bottom_center = widget_base(absolute_bottom,/column,/exclusive)
  absolute_bottom_right = widget_base(absolute_bottom,/row,/exclusive)
;###########################

;the lower frequency in the difference plot
  c1 = widget_button(select_base,uvalue='c1',value='1',sensitive=0)
  c2 = widget_button(select_base,uvalue='c2',value='s',sensitive=0)
  c3 = widget_button(select_base,uvalue='c3',value='t',sensitive=0)
  c4 = widget_button(select_base,uvalue='c4',value=' ',sensitive=0) 
  c5 = widget_button(select_base,uvalue='c5',value='f',sensitive=0)
  c6 = widget_button(select_base,uvalue='c6',value='r',sensitive=0)
  c7 = widget_button(select_base,uvalue='c7',value='e',sensitive=0)
  c8 = widget_button(select_base,uvalue='c8',value='q',sensitive=0)

;the upper frequency in the difference plot
  d1 = widget_button(select_base2,uvalue='d1',value='l',sensitive=0)
  d2 = widget_button(select_base2,uvalue='d2',value='a',sensitive=0)
  d3 = widget_button(select_base2,uvalue='d3',value='s',sensitive=0)
  d4 = widget_button(select_base2,uvalue='d4',value='t',sensitive=0)
  d5 = widget_button(select_base2,uvalue='d5',value='f',sensitive=0)
  d6 = widget_button(select_base2,uvalue='d6',value='r',sensitive=0)
  d7 = widget_button(select_base2,uvalue='d7',value='e',sensitive=0)
  d8 = widget_button(select_base2,uvalue='d8',value='q',sensitive=0)

  cycle = widget_button(select_base,uvalue='cycle',value='cy',sensitive=1)
;  stop_fill_butt = widget_button(select_base,uvalue='stop fill',value='sf',sensitive=1)

;the lower cutoff frequency
  e1 = widget_button(cutoff1_base,uvalue='e1',value='c',sensitive=0)
  e2 = widget_button(cutoff1_base,uvalue='e2',value='u',sensitive=0)
  e3 = widget_button(cutoff1_base,uvalue='e3',value='t',sensitive=0)
  e4 = widget_button(cutoff1_base,uvalue='e4',value='o',sensitive=0)
  e5 = widget_button(cutoff1_base,uvalue='e5',value='f',sensitive=0)
  e6 = widget_button(cutoff1_base,uvalue='e6',value='f',sensitive=0)
  e7 = widget_button(cutoff1_base,uvalue='e7',value=' ',sensitive=0)
  e8 = widget_button(cutoff1_base,uvalue='e8',value='1',sensitive=0)

;the upper cutoff frequency
  f1 = widget_button(cutoff2_base,uvalue='f1',value='c',sensitive=0)
  f2 = widget_button(cutoff2_base,uvalue='f2',value='u',sensitive=0)
  f3 = widget_button(cutoff2_base,uvalue='f3',value='t',sensitive=0)
  f4 = widget_button(cutoff2_base,uvalue='f4',value='o',sensitive=0)
  f5 = widget_button(cutoff2_base,uvalue='f5',value='f',sensitive=0)
  f6 = widget_button(cutoff2_base,uvalue='f6',value='f',sensitive=0)
  f7 = widget_button(cutoff2_base,uvalue='f7',value=' ',sensitive=0)
  f8 = widget_button(cutoff2_base,uvalue='f8',value='2',sensitive=0)
  
  dt1 = cw_field(deltatime_base,uvalue='dt1',value=0.0,/return_events,xsize=4,ysize=0.5,title='')
  dt2 = cw_field(deltatime_base,uvalue='dt2',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  dt3 = cw_field(deltatime_base,uvalue='dt3',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  dt4 = cw_field(deltatime_base,uvalue='dt4',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  dt5 = cw_field(deltatime_base,uvalue='dt5',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  dt6 = cw_field(deltatime_base,uvalue='dt6',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  ;dt7 = cw_field(deltatime_base,uvalue='dt7',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)
  ;dt8 = cw_field(deltatime_base,uvalue='dt8',value=0.0,/return_events,xsize=4,ysize=0.5,title='',/floating)

  print,'dt1: ',dt1

  xcoord = cw_field(coord_label_base,uvalue='xcoord',title = 'x',value=0,/column,/return_events,xsize=7)
  ycoord = cw_field(coord_label_base,uvalue='ycoord',title = 'y',value=0,/column,/return_events,xsize=7)
  rval = cw_field(coord_label_base,uvalue='rval',title='r value (RE)',value=0,/column,/return_events,xsize=7)
  lat = cw_field(coord_label_base,uvalue='lat',title='lat (deg)',value=0,/column,/return_events,xsize=7)
  alt = cw_field(coord_label_base,uvalue='alt',title='altitude',value=0,/column,/return_events,xsize=7)
  pm_firsttime_butt = cw_field(coord_label_base,uvalue='pm_firsttime',title='+/- time for first freq',value=10.0,/return_events,xsize=7)
 
  print_ps1 = widget_button(print_ps_base,uvalue='print_ps1',value='print ray plot to ps',sensitive=1)
  print_ps2 = widget_button(print_ps_base,uvalue='print_ps2',value='print corr plot to ps',sensitive=1)
;  filter_to_firstfreq = widget_button(print_ps_base,uvalue='filter_to_firstfreq',value='filter to first freq',sensitive=1)
  filter_butt = widget_button(print_ps_base,uvalue='filter_to_firstfreq',value='filter to first freq',sensitive=1)

  b11 = widget_button(filebox_base_upper,value='no file sel',uvalue='b11',sensitive=0)
  b22 = widget_button(filebox_base_upper,value='no file sel',uvalue='b22',sensitive=0)
  b33 = widget_button(filebox_base_upper,value='no file sel',uvalue='b33',sensitive=0)
  b44 = widget_button(filebox_base_upper,value='no file sel',uvalue='b44',sensitive=0)
  b55 = widget_button(filebox_base_upper,value='no file sel',uvalue='b55',sensitive=0)
  b66 = widget_button(filebox_base_upper,value='no file sel',uvalue='b66',sensitive=0)
  b77 = widget_button(filebox_base_upper,value='no file sel',uvalue='b77',sensitive=0)
  b88 = widget_button(filebox_base_upper,value='no file sel',uvalue='b88',sensitive=0)
  includerays = widget_button(filebox_base_upper,value='inc rays',uvalue='includerays',sensitive=1) ;include_rays button
  originalzoom = widget_button(filebox_base_lower,value='original zoom',uvalue='originalzoom',sensitive=0) ;original zoom button
  plot_grd = widget_button(cutoff1_base,value='grid',uvalue='plot_grd',sensitive=1)
  findfile = widget_button(file_button_base,uvalue='findfile',value='Find file (*.dat1)')
  readinfiles = widget_button(file_button_base,uvalue='readinfiles',value='Read in files',sensitive=0)

  b1 = widget_button(checkbox_base,value='1',sensitive=0)
  b2 = widget_button(checkbox_base,value='2',sensitive=0)
  b3 = widget_button(checkbox_base,value='3',sensitive=0)
  b4 = widget_button(checkbox_base,value='4',sensitive=0)
  b5 = widget_button(checkbox_base,value='5',sensitive=0)
  b6 = widget_button(checkbox_base,value='6',sensitive=0)
  b7 = widget_button(checkbox_base,value='7',sensitive=0)
  b8 = widget_button(checkbox_base,value='8',sensitive=0) 


;I have these buttons as /noedit because manually changing them messes
;up the coordinates somehow. 
  xmin = cw_field(min_max_base,uvalue='xmin',title='xmin',value=4.0,/return_events,/column,xsize=6)
  xmax = cw_field(min_max_base,uvalue='xmax',title='xmax',value=5.5,/return_events,/column,xsize=6)
  ymin = cw_field(min_max_base,uvalue='ymin',title='ymin',value=-1.0,/return_events,/column,xsize=6)
  ymax = cw_field(min_max_base,uvalue='ymax',title='ymax',value=1.0,/return_events,/column,xsize=6)

;###########################
  make_plots = widget_button(plot_params_base_upper,value ='Plot',uvalue='make_plots',sensitive=0)
  make_diff_plot = widget_button(plot_params_base_upper,value='Plot diff',uvalue='make_diff_plot',sensitive=0)
  make_corr = widget_button(plot_params_base_upper,value='Plot correlation',/align_center,uvalue='make_corr',sensitive=0)
                              
  fillwindow = widget_button(plot_params_base_lower,uvalue='fillwindow',value='Single plot',sensitive=0)
  fillwindow_diff = widget_button(plot_params_base_lower,uvalue='fillwindow_diff',value='Single plot diff',sensitive=0)
;###########################
  label6 = widget_label(corr_params_base_top,value='Time diff b/t 2nd')
  label7 = widget_label(corr_params_base_top,value='freq and 1st freq on cc')
  label7 = widget_label(corr_params_base_top,value='plot or single sc plot')
  label8 = widget_label(corr_params_base_top,value='Min delta-time in msec(top)')
  label9 = widget_label(corr_params_base_top,value='Max delta-time in msec(bottom)')
  corr_time2_butt = CW_FIELD(corr_params_base_bottom,uvalue='corr_time2',title='min delta-t',value=0.0,/return_events,xsize=10,ysize=1,/floating)
  corr_time_butt = CW_FIELD(corr_params_base_bottom,uvalue='corr_time',title='max delta-t',value=0.0,/return_events,xsize=10,ysize=1,/floating)
  base_time_butt = CW_FIELD(corr_params_base_bottom,uvalue='base_time',title='first time',value=0.0,/return_events,xsize=10,ysize=1,/floating)

  other = widget_button(absolute_bottom_left,uvalue='other',value='Other',sensitive=0)
  sca_but = widget_button(absolute_bottom_center,value='sca',uvalue='sca_but')
  scb_but = widget_button(absolute_bottom_center,value='scb',uvalue='scb_but')
  theta_k = widget_button(absolute_bottom_right,value='tk',uvalue='theta_k',sensitive=0)
  theta_g = widget_button(absolute_bottom_right,value='tg',uvalue='theta_g',sensitive=0)
  path_re = widget_button(absolute_bottom_right,value='pRe',uvalue='path_re',sensitive=0)
  f_fce = widget_button(absolute_bottom_right,value='fce',uvalue='f_fce',sensitive=0)
  refndx = widget_button(absolute_bottom_right,value='rdx',uvalue='refndx',sensitive=0)
  theta_gen = widget_button(absolute_bottom_right,value='tgn',uvalue='theta_gen',sensitive=0)
  theta_res = widget_button(absolute_bottom_right,value='trs',uvalue='theta_res',sensitive=0)
  

;###########################
  draw1 = widget_draw(left_base_top,uvalue='draw1',xsize=plotwin_xsize,ysize=plotwin_ysize,/button_events)
  draw2= widget_draw(left_base_bottomleft,uvalue='draw2',ysize=plotwin_ysize*0.33,xsize=plotwin_ysize*0.83)
;###########################

  xmin_corr = cw_field(left_base_bottomright,uvalue='xmin_corr',title='xmin',value=-100.0,/return_events,xsize=6,ysize=1,/floating)
  xmax_corr = cw_field(left_base_bottomright,uvalue='xmax_corr',title='xmax',value=100.0,/return_events,xsize=6,ysize=1,/floating)
  ymin_corr = cw_field(left_base_bottomright,uvalue='ymin_corr',title='ymin',value=5000.0,/return_events,xsize=6,ysize=1,/floating)
  ymax_corr = cw_field(left_base_bottomright,uvalue='ymax_corr',title='ymax',value=8000.0,/return_events,xsize=6,ysize=1,/floating)

  widget_control,main_base,/realize

  corr_time = 20.
  corr_time2 = 0.
  cb_time = 0.5
  cr_time_max = 0.     ;default initial values
  cr_time_min = 0.
  base_time = 0.0
  filter_to_firstfreq = 'no'
  xsize_px = double(plotwin_xsize)  ;arbitrary size I've chosen for the upper plot window
  ysize_px = double(plotwin_ysize)
  pm_firsttime = 10.0
  sc_toggle = 'sca'  ;we'll assume sc1 is selected for the 'option' command
  which_cutoff_but = 0
  what_other_but = 'none'  ;values are 'tk','tg','pRe'......
  plot_grid = 'no'
  cyclecount = 0.
  maccyclecount = 0. 
  cyclebool = 'no'
;  stop_fill = 'no'

  info={                                    $
         main_base:main_base,               $
         right_base:right_base,             $ ;base for setup
         left_base:left_base,               $ ;base for all plots
         file_base:file_base,               $
         plot_params_base:plot_params_base, $ ;timing plot input
         corr_params_base:corr_params_base, $ ;correlation plot input
         findfile:findfile,                 $ ;find another file
         make_plots:make_plots,             $ ;make ray plots
         corr_time:corr_time,               $ ;max correlation time
         corr_time2:corr_time2,             $
         corr_time_butt:corr_time_butt,     $
         corr_time2_butt:corr_time2_butt,   $
         base_time_butt:base_time_butt,     $
         pm_firsttime_butt:pm_firsttime_butt, $
         make_corr:make_corr,               $ ;make correlation plot.
         draw1:draw1,                       $ ;main plots draw window
         draw2:draw2,                       $ ;correlation plot window
         cb_time:cb_time,                   $ ;var that saves max time
         cr_time_max:cr_time_max,           $ ;var that saves max time for plot filtering
         cr_time_min:cr_time_min,           $ ;' '  ' '   '  ' min ''   ''   ''   ''
         base_time:base_time,             $ ;var that contains the time offset for the first freq (used for the cross-correlation plots)
         filter_to_firstfreq:filter_to_firstfreq,             $ ;button that turns on the base_time filtering
         b11:b11,b22:b22,b33:b33,b44:b44,   $ ;buttons with filenames
         b55:b55,b66:b66,b77:b77,b88:b88,   $
         filter_butt:filter_butt,           $
         includerays:includerays,           $
         originalzoom:originalzoom,         $
         b1:b1,b2:b2,b3:b3,b4:b4,b5:b5,     $ ;checkbox buttons
         b6:b6,b7:b7,b8:b8,                 $
         c1:c1,c2:c2,c3:c3,c4:c4,c5:c5,     $
         c6:c6,c7:c7,c8:c8,                 $
         d1:d1,d2:d2,d3:d3,d4:d4,d5:d5,     $
         d6:d6,d7:d7,d8:d8,                 $
         e1:e1,e2:e2,e3:e3,e4:e4,e5:e5,     $
         e6:e6,e7:e7,e8:e8,                 $
         f1:f1,f2:f2,f3:f3,f4:f4,f5:f5,     $
         f6:f6,f7:f7,f8:f8,                 $
         dt1:dt1,dt2:dt2,dt3:dt3,dt4:dt4,   $ ;buttons for adding an artificial delta-time
         dt5:dt5,dt6:dt6,                   $
         readinfiles:readinfiles,           $
         pm_firsttime:pm_firsttime,         $
         xcoord:xcoord,ycoord:ycoord,       $ ;coord fields
         alt:alt,rval:rval,lat:lat,         $
         fillwindow:fillwindow,             $
         xmin:xmin,xmax:xmax,               $
         ymin:ymin,ymax:ymax,               $ ;physical coord. limits of plot window
         include_rays:include_rays,         $
         fillwindow_diff:fillwindow_diff,   $
         make_diff_plot:make_diff_plot,     $
         plotwin_xsize:plotwin_xsize,       $
         plotwin_ysize:plotwin_ysize,       $
         print_ps1:print_ps1,               $
         print_ps2:print_ps2,               $
         ps1:ps1,                           $
         ps2:ps2,                           $
         xmin_corr:xmin_corr,               $
         xmax_corr:xmax_corr,               $ ;physical coord. limits of correlation window
         ymin_corr:ymin_corr,               $
         ymax_corr:ymax_corr,               $
         xsize_px:xsize_px,ysize_px:ysize_px,$ ;size of draw window (adjusts when zoomed in)
         selected_freq0:selected_freq0,     $ ;the first and last file I select
         selected_freq1:selected_freq1,     $ ;for the difference plot time subtraction
         cutoff_index0:cutoff_index0,       $
         cutoff_index1:cutoff_index1,       $
         theta_k:theta_k,                   $
         theta_g:theta_g,                   $
         path_re:path_re,                   $
         f_fce:f_fce,                       $
         refndx:refndx,                     $
         theta_gen:theta_gen,               $
         theta_res:theta_res,               $
         other:other,                       $
         sca_but:sca_but,                   $
         scb_but:scb_but,                   $
         sc_toggle:sc_toggle,               $    
         which_cutoff_but:which_cutoff_but, $
         what_other_but:what_other_but,     $
         plot_grid:plot_grid,               $  ;yes or no value
         plot_grd:plot_grd,                 $  ;for setting the button
         cycle:cycle,                       $
         cyclecount:cyclecount,             $
         maccyclecount:maccyclecount,       $
         cyclebool:cyclebool                $
  }
;program in the event ID's into info. With these ID's I can get all
;the information I need about an event through the event specifier
;that is passed as "event" to the event handler.

  widget_control,main_base,set_uvalue=info
  xmanager,'xplot',main_base,/no_block  ;I think this needs to be last.
end

