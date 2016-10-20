;this program performs reverse ray-tracing at the point of the sc. It
;launches rays at the base freq (freq1) at various theta angles, then
;does the same for the second freq (freq2). A 2-D array is created for
;every theta value as follows  array(4,5000)
;raydata(0,*) = timing
;raydata(1,*) = theta values
;raydata(2,*) = x positions
;raydata(3,*) = y positions
;The x and y values and timing of this array are then inserted into the
;TRIANGULATE function as TRIANGULATE,x,y,Triangles.
;Triangles is designed to be passed to the function TRIGRID.
;result = TRIGRID(x,y,Triangles).
;The result is then a 2-D array of timing values extrapolated to
;the newly created x-y grid.
;After doing this for freq1 and freq2 I can subtract
;the timing values and put into the final array called "mastertiming"
;with a corresponding string array called "masterinput".
;After all the rays are created for each theta value I am left with 
;the final "mastertiming" array, which is 
;2-D, with the sides representing the x and y grid coord. and the 
;values representing the maximum deltatime for each gridpoint. 
; ;Finally use the program plot.pro to plot.   

;Different angles spans lead to different filenames
;ex    plot3500x.dat1, plot3500a.dat1, plot3500b.dat1, plot3500c.dat1
;  'x' -->create rays in entire span of angles from one resonance cone
;  to the other
;  'a' -->the earthward half of the cone of angles
;  'b' -->the anti-earthward half of the cone of angles up to 
;  20 degrees off of the anti-earthward resonance cone
;  'c' -->20 degrees from anti-earthward resonance cone angle up to 
;  the anti-earthward resonance cone angle.

;include_sections = 'yes'--> create 'x','a','b','c' sections
;include_sections = 'no' --> create only 'x' section




pro reverse_trace
;common stuff,timetype,magnetic_multiplier,density_ref,phi,r_sc1,r_sc2,lat_sc1,lat_sc2,thetaoffres1,thetaoffres2,final_lat,final_alt,region,alt_sc1,alt_sc2,event,infoarray,finalfreq,limits,include_details,ppdensitychange,ppL_halfwidth,ppL_centralpos,raydata_sc1,raydata_sc2,gridspac,abcx,zzones,sca_f1_lo,sca_f1_hi,sca_f2_lo,sca_f2_hi,scb_f1_lo,scb_f1_hi,scb_f2_lo,scb_f2_hi

  openr,lun,'reverseinput.txt', /get_lun
  junk = ''
  readf,lun,junk
  readf,lun,junk
  readf,lun,junk
;read in data ##############################################
  event = strmid(junk,63,23)               & readf,lun,junk
  which_sc = strmid(junk,63,23)            & readf,lun,junk
  include_sections = strmid(junk,63,23)    & readf,lun,junk
  include_details = strmid(junk,63,23)     & readf,lun,junk
  region = strmid(junk,63,23)              & readf,lun,junk
  r_sc1 = strmid(junk,63,23)               & readf,lun,junk
  r_sc2 = strmid(junk,63,23)               & readf,lun,junk
  lat_sc1 = strmid(junk,63,23)             & readf,lun,junk
  lat_sc2 = strmid(junk,63,23)             & readf,lun,junk
  freqs_str = strmid(junk,63,80)           & readf,lun,junk
  thetaoffres1 = strmid(junk,63,23)        & readf,lun,junk
  thetaoffres2 = strmid(junk,63,23)        & readf,lun,junk
  phi = strmid(junk,63,23)                 & readf,lun,junk
  magnetic_multiplier = strmid(junk,63,23) & readf,lun,junk
  density_ref = strmid(junk,63,23)         & readf,lun,junk
  ppdensitychange = strmid(junk,63,23)     & readf,lun,junk
  ppL_halfwidth = strmid(junk,63,23)       & readf,lun,junk
  ppL_centralpos = strmid(junk,63,23)      & readf,lun,junk
  final_lat = strmid(junk,63,23)           & readf,lun,junk
  final_alt = strmid(junk,63,23)           & readf,lun,junk
  lg_thetaincrement = strmid(junk,63,23)   & readf,lun,junk
  sm_thetaincrement = strmid(junk,63,23)   & readf,lun,junk
  sm_anglestart = strmid(junk,63,23)       & readf,lun,junk
  limits_tmp = strmid(junk,63,28)          & readf,lun,junk
  gridspac = strmid(junk,63,28)            & readf,lun,junk
  zzones = strtrim(strmid(junk,63,40),2)    & readf,lun,junk
  sca_f1_lo = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  sca_f1_hi = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  sca_f2_lo = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  sca_f2_hi = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  scb_f1_lo = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  scb_f1_hi = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  scb_f2_lo = strtrim(strmid(junk,63,40),2) & readf,lun,junk
  scb_f2_hi = strtrim(strmid(junk,63,40),2)     

;remove whitespace #########################################
  event = strtrim(event,2) & which_sc = strtrim(which_sc,2) & include_sections = strtrim(include_sections,2)
  include_details = strtrim(include_details,2) & region = strtrim(region,2)
  r_sc1 = strtrim(r_sc1,2) & r_sc1 = float(r_sc1)
  r_sc2 = strtrim(r_sc2,2) &  r_sc2 = float(r_sc2) & lat_sc1 = strtrim(lat_sc1,2)
  lat_sc1 = float(lat_sc1) & lat_sc2 = strtrim(lat_sc2,2) & lat_sc2 = float(lat_sc2)
  gridspac = float(gridspac) & gridspac = strtrim(gridspac,2)
  freqs_str = strtrim(freqs_str,2)

  thetaOffRes2 = strtrim(thetaOffRes2,2) & thetaOffRes2 = float(thetaOffRes2)
  thetaoffres1 = strtrim(thetaoffres1,1) & thetaoffres1 = float(thetaoffres1)
  phi = strtrim(phi,2) & phi = float(phi) & magnetic_multiplier = strtrim(magnetic_multiplier,2)
  magnetic_multiplier = float(magnetic_multiplier) & density_ref = strtrim(density_ref,2)
  density_ref = float(density_ref) & final_lat = strtrim(final_lat,2)
  final_lat = float(final_lat) & final_alt = strtrim(final_alt,2) & final_alt = float(final_alt)
  lg_thetaincrement = float(lg_thetaincrement) & sm_thetaincrement = float(sm_thetaincrement)
  sm_anglestart = float(sm_anglestart) & ppdensitychange=strtrim(ppdensitychange,2) & ppdensitychange=float(ppdensitychange)
  ppL_halfwidth=strtrim(ppL_halfwidth,2) & ppL_halfwidth=float(ppL_halfwidth)
  ppL_centralpos=strtrim(ppL_centralpos,2) & ppL_centralpos=float(ppL_centralpos) 

  zzones=str_sep(zzones,',')
  sca_f1_lo = str_sep(sca_f1_lo,',')
  sca_f1_hi = str_sep(sca_f1_hi,',')
  sca_f2_lo = str_sep(sca_f2_lo,',')
  sca_f2_hi = str_sep(sca_f2_hi,',')
  scb_f1_lo = str_sep(scb_f1_lo,',')
  scb_f1_hi = str_sep(scb_f1_hi,',')
  scb_f2_lo = str_sep(scb_f2_lo,',')
  scb_f2_hi = str_sep(scb_f2_hi,',')



  freqs = fltarr(50)
  pos1 = 0.
  endbool = 'no'

  i=0
  while endbool ne 'yes' do begin
  pos2 = strpos(freqs_str,',',pos1+1)
  if pos2 eq -1 then endbool = 'yes'
  if pos2 eq -1 then pos2 = strlen(freqs_str)
  freqs(i) = strmid(freqs_str,pos1,pos2-pos1)
  pos1 = pos2 +1
  i=i+1
  endwhile

  tmp = where(freqs ne 0.)
  freqs = freqs(tmp)


  limits = fltarr(4)
  limits_str = strtrim(limits_tmp,2)

  tmp = strpos(limits_str,',')
  brak = strpos(limits_str,']')
  limits(0) = strmid(limits_str,1,tmp-1)
  limits_tmp = strmid(limits_str,tmp+1,brak)
  tmp = strpos(limits_tmp,',')
  limits(1) = strmid(limits_tmp,0,tmp)
  limits_tmp = strmid(limits_tmp,tmp+1,brak)
  tmp = strpos(limits_tmp,',')
  limits(2) = strmid(limits_tmp,0,tmp)
  limits_tmp = strmid(limits_tmp,tmp+1,brak)
  limits(3) = strmid(limits_tmp,0,brak)

;  samefreqs = 'no'
;  if freqinit eq freqfinal then samefreqs='yes'
;############################################################  
  close,lun
  free_lun,lun

  ;sm_thetaincrement = 0.1 
  ;sm_anglestart = 15.  ;angle off of the resonance cone to start incrementing in small steps. 
  ;lg_thetaincrement = 0.25

  if which_sc EQ 'c1' OR which_sc eq 'c2' $
    or which_sc eq 'c3' or which_sc eq 'c4' or which_sc eq 'source' then timetype = 1.
  if which_sc EQ 'cc12' OR which_sc eq 'cc13' $ 
    or which_sc eq 'cc14' or which_sc eq 'cc23' $
    or which_sc eq 'cc24' or which_sc eq 'cc34' then timetype = 3.

  ;print_xgrid = 'yes'
;###############################################################
;###############################################################
;this is the main loop
   for yy=0, n_elements(freqs)-1 do begin
   
      freq_current = freqs(yy)   

      infoarray = STRARR(31)
      infoarray(0) = 'event: ' + event
      infoarray(1) = 'timetype = ' + strtrim(timetype,2)
      infoarray(2) = 'sc: ' + strtrim(which_sc,2) 
      infoarray(5) = 'start incrementing in small steps at ' + strtrim(sm_anglestart,2) + ' degrees off of estimated anti-earthward resonance cone'
      infoarray(6) = 'large angle increment (deg) = ' + strtrim(lg_thetaincrement,2)
      infoarray(7) = 'small angle increment (deg)= ' + strtrim(sm_thetaincrement,2)
      infoarray(9) = 'n grids (x,z) = ' + gridspac + ' x ' + gridspac
      infoarray(10) = 'magnetospheric limits for gridding (rectangular coord - RE) [x0,z0,x1,z1] = ' + limits_str
      infoarray(16) = 'final ray lat = ' + strtrim(final_lat,2)
      infoarray(17) = 'final ray alt = ' + strtrim(final_alt,2)
      infoarray(18) = 'density at reference point (cm^-3) = ' + strtrim(density_ref,2)
      infoarray(19) = 'magnetic dipole multiplier = ' + strtrim(magnetic_multiplier,2)
      infoarray(20) = 'rval sc1 = ' + strtrim(r_sc1,2)
      infoarray(21) = 'rval sc2 = ' + strtrim(r_sc2,2)
      infoarray(22) = 'lat sc1 = ' + strtrim(lat_sc1,2)
      infoarray(23) = 'lat sc2 = ' + strtrim(lat_sc2,2)
      infoarray(24) = 'phi = ' + strtrim(phi,2)
      infoarray(27) = 'PLASMAPAUSE relative density change = ' + strtrim(ppdensitychange,2)
      infoarray(28) = 'PLASMAPAUSE L-HALF WIDTH = ' + strtrim(ppL_halfwidth,2)
      infoarray(29) = 'PLASMAPAUSE L-CENTRAL POSITION = ' + strtrim(ppL_centralpos,2)
      infoarray(30) = 'Are all the ray details included? ' + include_details

;mocktheta is the theta_k value for a test ray used to find the
;resonance cone angle
      if(region EQ 'north') then mocktheta = 0.01 
      if(region EQ 'south') then mocktheta = 170.

      alt_sc1 = (6370.0*r_sc1) - 6370.0 ;distance above surface.
      if timetype eq 3. then alt_sc2 = (6370.0*r_sc2) - 6370.0
;##############################################################################################

      if include_sections eq 'yes' then begin
      for loopnum=0,3 do begin
      
;lets find the resonance cone angle for the current frequency for sc1
          createfile,freq_current,mocktheta,which_switch=1
          resangle = ''
          thetares,resangle,infoarray,which_switch=1
         
;          if loopnum eq 0 then angles,resangle,anglemin_sc1,anglemax_sc1,abcx='x'
;          if loopnum eq 1 then angles,resangle,anglemin_sc1,anglemax_sc1,abcx='a'
;          if loopnum eq 2 then angles,resangle,anglemin_sc1,anglemax_sc1,abcx='b'
;          if loopnum eq 3 then angles,resangle,anglemin_sc1,anglemax_sc1,abcx='c'

          if loopnum eq 0 then abcx='x'
          if loopnum eq 1 then abcx='a'
          if loopnum eq 2 then abcx='b'
          if loopnum eq 3 then abcx='c'
          angles,resangle,anglemin_sc1,anglemax_sc1

;lets find the resonance cone angle for the current frequency for sc2
          if timetype eq 3. then begin
              createfile,freq_current,mocktheta,which_switch=2
              resangle = ''
              thetares,resangle,infoarray,which_switch=2
              ;if loopnum eq 0 then angles,resangle,anglemin_sc2,anglemax_sc2,abcx='x'
              ;if loopnum eq 1 then angles,resangle,anglemin_sc2,anglemax_sc2,abcx='a'
              ;if loopnum eq 2 then angles,resangle,anglemin_sc2,anglemax_sc2,abcx='b'
              ;if loopnum eq 3 then angles,resangle,anglemin_sc2,anglemax_sc2,abcx='c'

              if loopnum eq 0 then abcx='x'
              if loopnum eq 1 then abcx='a'
              if loopnum eq 2 then abcx='b'
              if loopnum eq 3 then abcx='c'

              angles,resangle,anglemin_sc2,anglemax_sc2

          endif
;##########
;#############################
;now here's where we need to create a huge array before we make a
;single call to allthetas.                   

          allthetas,anglemin_sc1,anglemax_sc1,anglemin_sc2,anglemax_sc2,freq_current,thetaincrement_sc1,thetaincrement_sc2,mastertiming,xgrid,zgrid,xray_sc1,xray_sc2,zray_sc1,zray_sc2,sm_thetaincrement,sm_anglestart,lg_thetaincrement,timingfreq_sc1,timingfreq_sc2,theta_k_sc1=theta_k_sc1,theta_k_sc2=theta_k_sc2,theta_g_sc1=theta_g_sc1,theta_g_sc2=theta_g_sc2,path_re_sc1=path_re_sc1,path_re_sc2=path_re_sc2,f_fce_sc1=f_fce_sc1,f_fce_sc2=f_fce_sc2,refndx_sc1=refndx_sc1,refndx_sc2=refndx_sc2,theta_gen_sc1=theta_gen_sc1,theta_gen_sc2=theta_gen_sc2,theta_res_sc1=theta_res_sc1,theta_res_sc2=theta_res_sc2,dens_sc1=dens_sc1,dens_sc2=dens_sc2,samefreqs,freqs
          
;#############################
          
;now all the arrays have been created
;and mastertiming,xgrid and zgrid have been returned

          tmpp = where(xray_sc1 ne 0.)
          if tmpp(0) ne -1 then xray_sc1 = xray_sc1(where(xray_sc1 NE 0))
          tmpp = where(zray_sc1 ne 0.)
          if tmpp(0) ne -1 then zray_sc1 = zray_sc1(where(zray_sc1 NE 0))
          if timetype eq 3. then begin
              tmpp = where(xray_sc2 ne 0.)
              if tmpp(0) ne -1 then xray_sc2 = xray_sc2(where(xray_sc2 NE 0))
              tmpp = where(zray_sc2 ne 0.)
              if tmpp(0) ne -1 then zray_sc2 = zray_sc2(where(zray_sc2 NE 0))
          endif
          
          print,'total time: ',total(mastertiming)      
          tmpfreq = strtrim(freq_current,2)
          dot = strpos(tmpfreq,'.')
          tmpfreq = strmid(tmpfreq,0,dot)
;##############################
          
;I want to save infoarray to a file in the traditional way (i.e. not
;IDL's save command) so I can read it with the 'more' command
         
              if loopnum eq '0' then openw,lun0,'/data/awb/raytrace/infoarray' + tmpfreq + 'x' + '.dat4',/get_lun
              if loopnum eq '1' then openw,lun0,'/data/awb/raytrace/infoarray' + tmpfreq + 'a' + '.dat4',/get_lun
              if loopnum eq '2' then openw,lun0,'/data/awb/raytrace/infoarray' + tmpfreq + 'b' + '.dat4',/get_lun
              if loopnum eq '3' then openw,lun0,'/data/awb/raytrace/infoarray' + tmpfreq + 'c' + '.dat4',/get_lun

              i=0
              while(i LT n_elements(infoarray)) do begin
                  printf,lun0,infoarray(i)
                  i=i+1
              endwhile                    
              
              if timetype eq 1. then begin
                  if include_details eq 'no' then begin
                      xray_sc1x = xray_sc1
                      zray_sc1x = zray_sc1                      
                      if loopnum eq '0' then save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'x' + '.dat1'
                      if loopnum eq '1' then save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'a' + '.dat1'
                      if loopnum eq '2' then save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'b' + '.dat1'
                      if loopnum eq '3' then save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'c' + '.dat1'
                  endif

                  if include_details eq 'yes' then begin
                      if loopnum eq '0' then begin
                      xray_sc1x = xray_sc1
                      zray_sc1x = zray_sc1
                      save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,dens_sc1,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'x' + '.dat1'
                      endif
                      if loopnum eq '1' then begin
                      xray_sc1a = xray_sc1
                      zray_sc1a = zray_sc1
                      save,mastertiming,xgrid,zgrid,xray_sc1a,zray_sc1a,timingfreq_sc1,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,dens_sc1,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'a' + '.dat1'
                      endif
                      if loopnum eq '2' then begin
                      xray_sc1b = xray_sc1
                      zray_sc1b = zray_sc1
                      save,mastertiming,xgrid,zgrid,xray_sc1b,zray_sc1b,timingfreq_sc1,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,dens_sc1,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'b' + '.dat1'
                      endif
                      if loopnum eq '3' then begin
                      xray_sc1c = xray_sc1
                      zray_sc1c = zray_sc1
                      save,mastertiming,xgrid,zgrid,xray_sc1c,zray_sc1c,timingfreq_sc1,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,dens_sc1,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'c' + '.dat1'
                      endif
                  endif
              endif

              if timetype eq 3. then begin
                  if loopnum eq '0' then begin
                  xray_sc1x = xray_sc1
                  zray_sc1x = zray_sc1
                  xray_sc2x = xray_sc2
                  zray_sc2x = zray_sc2
                  save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,xray_sc2x,zray_sc2x,timingfreq_sc1,timingfreq_sc2,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,theta_k_sc2,theta_g_sc2,path_re_sc2,f_fce_sc2,refndx_sc2,theta_gen_sc2,theta_res_sc2,dens_sc1,dens_sc2,include_details,filename='/data/awb/raytrace/plot' + tmpfreq + 'x' + '.dat1'                
                  endif
                  if loopnum eq '1' then begin
	          xray_sc1a = xray_sc1
                  zray_sc1a = zray_sc1
                  xray_sc2a = xray_sc2
                  zray_sc2a = zray_sc2
                  save,mastertiming,xgrid,zgrid,xray_sc1a,zray_sc1a,xray_sc2a,zray_sc2a,timingfreq_sc1,timingfreq_sc2,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,theta_k_sc2,theta_g_sc2,path_re_sc2,f_fce_sc2,refndx_sc2,theta_gen_sc2,theta_res_sc2,dens_sc1,dens_sc2,include_details,filename='/data/awb/raytrace/plot' + tmpfreq + 'a' + '.dat1'
                  endif
                  if loopnum eq '2' then begin
                  xray_sc1b = xray_sc1
                  zray_sc1b = zray_sc1
                  xray_sc2b = xray_sc2
                  zray_sc2b = zray_sc2
                  save,mastertiming,xgrid,zgrid,xray_sc1b,zray_sc1b,xray_sc2b,zray_sc2b,timingfreq_sc1,timingfreq_sc2,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,theta_k_sc2,theta_g_sc2,path_re_sc2,f_fce_sc2,refndx_sc2,theta_gen_sc2,theta_res_sc2,dens_sc1,dens_sc2,include_details,filename='/data/awb/raytrace/plot' + tmpfreq + 'b' + '.dat1'
                  endif
                  if loopnum eq '3' then begin
                  xray_sc1c = xray_sc1
                  zray_sc1c = zray_sc1
                  xray_sc2c = xray_sc2
                  zray_sc2c = zray_sc2
                  save,mastertiming,xgrid,zgrid,xray_sc1c,zray_sc1c,xray_sc2c,zray_sc2c,timingfreq_sc1,timingfreq_sc2,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,theta_k_sc2,theta_g_sc2,path_re_sc2,f_fce_sc2,refndx_sc2,theta_gen_sc2,theta_res_sc2,dens_sc1,dens_sc2,include_details,filename='/data/awb/raytrace/plot' + tmpfreq + 'c' + '.dat1'
                  endif 
              endif                   
          xray_sc1 = 0. &  xray_sc2 = 0. & zray_sc1 = 0. &  zray_sc2 = 0. & xray_sc1x = 0. & xray_sc2x = 0.
          zray_sc1x = 0. & zray_sc2x = 0. & xray_sc1a = 0. & xray_sc2a = 0. & zray_sc1a = 0. & zray_sc2a = 0.
          xray_sc1b = 0. & xray_sc2b = 0. & zray_sc1b = 0. & zray_sc2b = 0. & xray_sc1c = 0. & xray_sc2c = 0.
          zray_sc1c = 0. & zray_sc2c = 0. & timingfreq_sc1 = 0. & timingfreq_sc2 = 0. & theta_k_sc1 = 0.
          theta_k_sc2 = 0. & theta_g_sc1 = 0. & theta_g_sc2 = 0. & path_re_sc1 = 0. & path_re_sc2 = 0.
          f_fce_sc1 = 0. & f_fce_sc2 = 0. & refndx_sc1 = 0. & refndx_sc2 = 0. & theta_gen_sc1 = 0.
          theta_gen_sc2 = 0. & theta_res_sc1 = 0. & theta_res_sc2 = 0. & dens_sc1 = 0. & dens_sc2 = 0.
          raydata_sc1 = 0. & raydata_sc2 = 0. & raydata_sc1_tmp = 0. & raydata_sc2_tmp = 0.
          
      endfor                ;for 'x' 'a' 'b' 'c' types
      endif

      if include_sections eq 'no' then begin
      
;lets find the resonance cone angle for the current frequency for sc1
          createfile,freq_current,mocktheta,which_switch=1
          resangle = ''
          thetares,resangle,infoarray,which_switch=1
         
          abcx='x'
;          angles,resangle,anglemin_sc1,anglemax_sc1,abcx='x'
          angles,resangle,anglemin_sc1,anglemax_sc1
;lets find the resonance cone angle for the current frequency for sc2
          if timetype eq 3. then begin
              createfile,freq_current,mocktheta,which_switch=2
              resangle = ''
              thetares,resangle,infoarray,which_switch=2
;              angles,resangle,anglemin_sc2,anglemax_sc2,abcx='x'
              abcx='x'
              angles,resangle,anglemin_sc2,anglemax_sc2
          endif
;##########
;#############################
;now here's where we need to create a huge array before we make a
;single call to allthetas.
          
          allthetas,anglemin_sc1,anglemax_sc1,anglemin_sc2,anglemax_sc2,freq_current,thetaincrement_sc1,thetaincrement_sc2,mastertiming,xgrid,zgrid,xray_sc1,xray_sc2,zray_sc1,zray_sc2,sm_thetaincrement,sm_anglestart,lg_thetaincrement,timingfreq_sc1,timingfreq_sc2,theta_k_sc1=theta_k_sc1,theta_k_sc2=theta_k_sc2,theta_g_sc1=theta_g_sc1,theta_g_sc2=theta_g_sc2,path_re_sc1=path_re_sc1,path_re_sc2=path_re_sc2,f_fce_sc1=f_fce_sc1,f_fce_sc2=f_fce_sc2,refndx_sc1=refndx_sc1,refndx_sc2=refndx_sc2,theta_gen_sc1=theta_gen_sc1,theta_gen_sc2=theta_gen_sc2,theta_res_sc1=theta_res_sc1,theta_res_sc2=theta_res_sc2,dens_sc1=dens_sc1,dens_sc2=dens_sc2,samefreqs,freqs
          
;#############################
          
;now all the arrays have been created
;and mastertiming,xgrid and zgrid have been returned
          
          xray_sc1 = xray_sc1(where(xray_sc1 NE 0))
          zray_sc1 = zray_sc1(where(zray_sc1 NE 0))
          if timetype eq 3. then begin
              xray_sc2 = xray_sc2(where(xray_sc2 NE 0))
              zray_sc2 = zray_sc2(where(zray_sc2 NE 0))
          endif
          
          print,'total time: ',total(mastertiming)      
          tmpfreq = strtrim(freq_current,2)
          dot = strpos(tmpfreq,'.')
          tmpfreq = strmid(tmpfreq,0,dot)
;##############################
          
;I want to save infoarray to a file in the traditional way (i.e. not
;IDL's save command) so I can read it with the 'more' command
         
              openw,lun0,'/data/awb/raytrace/infoarray' + tmpfreq + 'x' + '.dat4',/get_lun

              i=0
              while(i LT n_elements(infoarray)) do begin
                  printf,lun0,infoarray(i)
                  i=i+1
              endwhile                    

              close,lun0
              free_lun,lun0
              
              if timetype eq 1. then begin
                  xray_sc1x = xray_sc1
                  zray_sc1x = zray_sc1
                  save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,timingfreq_sc1,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,dens_sc1,include_details,filename = '/data/awb/raytrace/plot' + tmpfreq + 'x' + '.dat1'
              endif
              if timetype eq 3. then begin
                  xray_sc1x = xray_sc1
                  zray_sc1x = zray_sc1
                  xray_sc2x = xray_sc2
                  zray_sc2x = zray_sc2
                  save,mastertiming,xgrid,zgrid,xray_sc1x,zray_sc1x,xray_sc2x,zray_sc2x,timingfreq_sc1,timingfreq_sc2,timetype,limits,theta_k_sc1,theta_g_sc1,path_re_sc1,f_fce_sc1,refndx_sc1,theta_gen_sc1,theta_res_sc1,theta_k_sc2,theta_g_sc2,path_re_sc2,f_fce_sc2,refndx_sc2,theta_gen_sc2,theta_res_sc2,dens_sc1,dens_sc2,include_details,filename='/data/awb/raytrace/plot' + tmpfreq + 'x' + '.dat1'
              endif                            
          endif ;for only the 'x' section

;          if freqincrement eq 0. then freqincrement = 1. ;so the loop is guaranteed to end.
;          freq_current = float(freq_current) + float(freqincrement)


;here I'm setting all the arrays to a scalar values so that IDL
;deletes them from memory. 

          mastertiming = 0. & xgrid = 0. & zgrid = 0.
          xray_sc1 = 0. &  xray_sc2 = 0. & zray_sc1 = 0. &  zray_sc2 = 0. & xray_sc1x = 0. & xray_sc2x = 0.
          zray_sc1x = 0. & zray_sc2x = 0. & xray_sc1a = 0. & xray_sc2a = 0. & zray_sc1a = 0. & zray_sc2a = 0.
          xray_sc1b = 0. & xray_sc2b = 0. & zray_sc1b = 0. & zray_sc2b = 0. & xray_sc1c = 0. & xray_sc2c = 0.
          zray_sc1c = 0. & zray_sc2c = 0. & timingfreq_sc1 = 0. & timingfreq_sc2 = 0. & theta_k_sc1 = 0.
          theta_k_sc2 = 0. & theta_g_sc1 = 0. & theta_g_sc2 = 0. & path_re_sc1 = 0. & path_re_sc2 = 0.
          f_fce_sc1 = 0. & f_fce_sc2 = 0. & refndx_sc1 = 0. & refndx_sc2 = 0. & theta_gen_sc1 = 0.
          theta_gen_sc2 = 0. & theta_res_sc1 = 0. & theta_res_sc2 = 0. & dens_sc1 = 0. & dens_sc2 = 0.
          raydata_sc1 = 0. & raydata_sc2 = 0. & raydata_sc1_tmp = 0. & raydata_sc2_tmp = 0.
  endfor ;for each frequency
end
