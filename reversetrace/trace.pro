pro trace,bw=bw
fil = FINDFILE('*.dat',count=nfil)
p=ptrarr(50)
np=0 ;number of tracings
for i=0,nfil-1 do $
 if strmid(fil(i),strlen(fil(i))-1,1) ne '\' then begin
  openr,lun,fil(i),/get_lun
  s=fstat(lun)
  if s.size gt 1310L then begin
   tit='' & fr='' & readf,lun,tit,fr,form='(a34,a14)'
   if tit eq 'TRACE (ray tracing) @ frequency of' then begin
;     rd_trac,lun,(s.size-414-124)/142,p,np
;old files
;    
     print, 'reading ',fil(i),'...'
     nl=(s.size-253L-1675L)/200L
     rd_trac,lun,nl,p,np
     print, '...',nl,' lines read from ',fil(i),'.'     
     np=np+1
    endif
  endif
  close,lun
  free_lun,lun
 endif
plo_trac,p,np
;plo_the,p,np
ptr_free,p
end

pro rd_trac,lun,nrec,p,np
 d=replicate({REG:0,path:0.,TG:0.,dpath:0.,dTG:0., wavel:0.,$
              DIST:0.,ALT:0.,LAT:0.,$
              LON:0.,L:0.,fFGe:0.,fFGh:0.,DENS:0.,pH:0.,pHe:0.,$
              pO:0.,FLHR:0.,N:0.,kv:0.,ka:0.,THE:0.,TheRay:0.,$
              PHI:0.,THG:0.,THR:0.},nrec)
 dum='' & readf,lun,dum,form='(a)'
 
;  -->(I2,5G10.3,F7.3,F8.0,2F7.2,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1) correction that allows code to work
 
; readf,lun,d;,FORMAT='(I2,5G10.3,F7.4,F8.0,F6.2,2F7.2,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1)'
 readf,lun,d,FORMAT='(I2,5G10.3,F7.3,F8.0,2F7.2,G10.3,2f10.3,F10.2,3F6.1,F8.3,G10.3,7F6.1)'
 p(np)=ptr_new(d)
end

pro plo_trac,p,np,bw=bw
;
;rmax=7.
;zmax=5.
;zmin=-2.
;mlttext='MLT=6h'
;midmlt=6.
rmax=6.
zmax=3.
zmin=-3.
mlttext='';'MLT=7h'
midmlt=0.
arrowint=0.1 ; seconds between two k vectors
arrowlen=0.3 ; Re
;
  suml=0.
  nl=0.
  for i=0l,np-1 do for j =0l,n_elements((*p(i)))-1 do begin
    suml=suml+(*p(i))(j).LON
    nl=nl+1
;    x=(*p(i))(j).DIST * cos((*p(i))(j).LAT*!dtor)
;    y=(*p(i))(j).DIST * sin((*p(i))(j).LAT*!dtor)
;    if x gt maxx then maxx=x
;    if y gt maxx then maxx=y
  endfor
  avel=suml/nl
  avel=90.
  print,'average MLT=',avel/15.
  avel=midmlt*15.
  !P.font=6
  circ=findgen(1000)/999.*2.*!pi-!pi
  plot,[0,1],xrange=[0.,rmax],yrange=[zmin,zmax],$
      /nodata,ystyle=5,xstyle=5,pos=[0.1,0.12,0.49,0.9]
   ;plasmasphere
    L=4.8 & r=L*sin(!pi/2.-circ)^2
     iout=where((r gt 1)and(cos(circ) gt 0)and(r*cos(circ) le rmax)and(r*sin(circ) ge zmin)and(r*sin(circ) le zmax),ni)
     iiout=where((r gt 1)and(cos(circ) gt 0)and(sin(circ) ge zmin)and(r*sin(circ) ge zmin)and(r*sin(circ) le zmax),ni)
     if max(r) gt rmax then begin
;       polyfill,[r(iout)*cos(circ(iout)),reverse(cos(circ(iiout))),1,rmax],$
;             [r(iout)*sin(circ(iout)),reverse(sin(circ(iiout))),zmin,zmin],/clip,color=200
     endif else begin
;       polyfill,[r(iout)*cos(circ(iout)),reverse(cos(circ(iout)))],$
;             [r(iout)*sin(circ(iout)),reverse(sin(circ(iout)))],/clip,color=200
     endelse
      dummy=min(r(iout),imin)
    print,'plasmasphere L=',L,'  mlat=',circ(iout(imin))/!dtor
   ; 
   ;auroral lines
   ; lat1=70.
   ; lat2=80.
   ; circ4=circ(n_elements(circ)/2:n_elements(circ)/4*3)
   ; L1=1./sin(!pi/2.-lat1*!dtor)^2 & r1=L1*sin(!pi/2.-circ4)^2
   ;  iout1=where((r1 ge 1)and(r1*cos(circ4)le rmax)and $
   ;              (r1*sin(circ4)le zmax)and(r1*sin(circ4)le shift(r1*sin(circ4),1)),n1);&iout1=iout1(20:n1-1)
   ; L2=1./sin(!pi/2.-lat2*!dtor)^2 & r2=L2*sin(!pi/2.-circ4)^2
   ;  iout2=where((r2 ge 1)and(r2*cos(circ4)le rmax)and $
   ;              (r2*sin(circ4)le zmax)and(r2*sin(circ4)le shift(r2*sin(circ4),1)),n2) ;&iout2=iout2(30:n2-1)
   ; polyfill,[r1(iout1)*cos(circ4(iout1)),reverse(r2(iout2)*cos(circ4(iout2)))],$
   ;          [r1(iout1)*sin(circ4(iout1)),reverse(r2(iout2)*sin(circ4(iout2)))],color=120
   ;print,'aur. lines L=',L1(0),'-',L2(0),'  mlat=',lat1,'-',lat2
   ;oplot,r1(iout1)*cos(circ(iout1)),r1(iout1)*sin(circ(iout1))
  ;
;___Meridional plane 
  plot,cos(circ),sin(circ),xrange=[-5.,5],yrange=[-2,2],tit='Meridional plane',$
       xtit='R!dE!n',ytit='R!dE!n',pos=[0.1,0.12,0.49,0.9],/noerase,xstyle=1,ystyle=17
  xyouts,/data,0.1,zmax-(zmax-zmin)*0.1,mlttext
  ;___trace
  for i=0,np-1 do begin
    x=(*p(i)).DIST * cos((*p(i)).LAT*!dtor)* cos(((*p(i)).LON-avel)*!dtor)
    y=(*p(i)).DIST * sin((*p(i)).LAT*!dtor)* cos(((*p(i)).LON-avel)*!dtor)
    oplot,x,y
    ;___kvectors
    lasttg=-arrowint
    n=n_elements(x)
    print,'Meridian: Tg=',(*p(i))(0).tg,'-',(*p(i))(n-1).tg,' -> ',$
          long(((*p(i))(n-1).tg-(*p(i))(0).tg)/arrowint),' k arrows.'
    for j=0,n-1 do if lasttg+arrowint le (*p(i))(j).tg then begin
      lasttg=lasttg+arrowint
      kr=arrowlen*cos( (*p(i))(j).kv*!dtor)
      kl=arrowlen*sin( (*p(i))(j).kv*!dtor)*cos( (*p(i))(j).ka*!dtor)
      ka=arrowlen*sin( (*p(i))(j).kv*!dtor)*sin( (*p(i))(j).ka*!dtor)
      kx=kr*cos((*p(i))(j).LAT*!dtor)-kl*sin((*p(i))(j).LAT*!dtor)
      ky=kr*sin((*p(i))(j).LAT*!dtor)+kl*cos((*p(i))(j).LAT*!dtor)
      arrow,x(j),y(j), x(j)+kx, y(j)+ky,hsize=-0.2,/solid,/data
    end
  endfor
;
;____Equatorial plane
;  plot,cos(circ),sin(circ),xrange=[-rmax/2.,rmax/2.],yrange=[0.,rmax],$;,xticks=50,xtickv=-fix(rmax)/2+indgen(rmax),$
;      tit='Equatorial plane',xtit='R!dE!n',xstyle=1,ystyle=8,yticks=rmax,$
;      ytickname=replicate(' ',30),pos=[0.51,0.12,0.9,0.9],/noerase
;  axis,yax=1,ytit='R!dE!n',yrange=[0.,rmax],ystyle=9
;  ;___trace
;  for i=0,np-1 do begin
;    ii=where((*p(i)).LAT gt -100)
;    x=-(*p(i))(ii).DIST * cos((*p(i))(ii).LAT*!dtor)* sin(((*p(i))(ii).LON-avel)*!dtor)
;    y=(*p(i))(ii).DIST * cos((*p(i))(ii).LAT*!dtor)* cos(((*p(i))(ii).LON-avel)*!dtor)
;    oplot,x,y
;    ;___kvectors
;    lasttg=-arrowint
;    n=n_elements(x)
;    print,'Equator : Tg=',(*p(i))(ii(0)).tg,'-',(*p(i))(ii(n-1)).tg,' -> ',$
;          long(((*p(i))(ii(n-1)).tg-(*p(i))(ii(0)).tg)/arrowint),' k arrows.'
;    for j=0,n-1 do if lasttg+arrowint le (*p(i))(ii(j)).tg then begin
;      lasttg=lasttg+arrowint
;      kr=arrowlen*cos( (*p(i))(ii(j)).kv*!dtor)
;      kl=arrowlen*sin( (*p(i))(ii(j)).kv*!dtor)*cos( (*p(i))(ii(j)).ka*!dtor)
;      km=kr*cos((*p(i))(j).LAT*!dtor)-kl*sin((*p(i))(j).LAT*!dtor)
;      ka=arrowlen*sin( (*p(i))(ii(j)).kv*!dtor)*sin( (*p(i))(ii(j)).ka*!dtor)
;      kx=-km*sin(((*p(i))(ii(j)).LON-avel)*!dtor)+ka*cos(((*p(i))(ii(j)).LON-avel)*!dtor)
;      ky=+km*cos(((*p(i))(ii(j)).LON-avel)*!dtor)+ka*sin(((*p(i))(ii(j)).LON-avel)*!dtor)
;      arrow,x(j),y(j), x(j)+kx, y(j)+ky,hsize=-0.2,/solid,/data
;    end
;  endfor
;

 ;arrow,/data,0,zmin+0.8*(zmax-zmin),0,zmin+0.95*(zmax-zmin),/solid,thick=6,hsize=-0.05
 ;xyouts,/data,0.,zmin+1.*(zmax-zmin),mlttext
end


pro plo_the,p,np,bw=bw
;
colors=[150,185,240,255]
linesco=50
plasmaco=3
text=['1550','1552','1554','jh;']
;
  maxx=8.
  !P.font=6
  plot,[0,1],yrange=[1.,maxx],xrange=[0,180.],$;tit='Meridional plane',$
       ytit='R (R!dE!n)',xtit='theta (deg)',pos=[0.1,0.12,0.47,0.9],xstyle=1,ystyle=17,col=255
  for i=0,np-1 do begin
    oplot,(*p(i)).the,(*p(i)).dist,col=colors(i)
  endfor
  ;
  plot,[0,1],yrange=[1.,maxx],xrange=[0.,360.],$;tit='Meridional plane',$
       xtit='phi (deg)',pos=[0.53,0.12,0.9,0.9],/noerase,xstyle=1,ystyle=25,col=255,$
       ytickname=replicate(' ',30)
  axis,yax=1,ytit='R (R!dE!n)',yrange=[1.,maxx],ystyle=9,col=255
  for i=0,np-1 do begin
     xx=(*p(i)).phi
     ix=where(xx lt 0,nx)
     if nx gt 0 then xx(ix)=xx(ix)+360.
    oplot,xx,(*p(i)).dist,col=colors(i)
  endfor
  ;
 for i=0,np-1 do begin
  y=1.+(0.9-0.1*i)*(maxx-1)
  plots,/data,[240,260],[y,y],col=colors(i)
  xyouts,/data,270,y,text(i),align=0.,col=255
 endfor
end
