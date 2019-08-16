c Aaron's attempt to  understand trace.for


c_________________________
c  VARIABLE KEY
c  _______________________
c  Region = ident_dens_region
c  Path = Y(7)
c  TimeG = Y(8)
c  per_dTimeG_T = reltstep
c  per_dPath_WL = relsstep
c  WL = wavelength
c  RE = Y(1)
c  Altitude = ALT
c  magnetic latitude = FID
c  magnetic longitude = LOND
c  McIlwain L = ELE
c  f/fce = FGF
c  f/fpH = FGF*1836.
c  density = DENS
c  percentage H+ = pH
c  percentage He+ = pHe
c  percentage O+ = pO
c  flhr = FLHR
c  refractive index = EMU
c  Vek = akvert
c  Azk = akazi
c  theta_kb = THD
c  theta_group = PSI
c  Phk = PHI
c  Gendrin angle = THG
c  Resonance cone angle = THR

c      Y(2)=(90.-LAT0)*CONVE
c      Y(3)= LON0*CONVE


      Y2=(90.-LAT1)*CONVE

      FI0=90.-LAT0


c  THE = THD or THE = 180 - THD




c-----------------------------------------------------------
c QUANTITIES INVOLVING THET0, MAGNETIC FIELD DIRECTION, PHI0
c      NHX0=SIN(THET0*CONVE)*COS((PHI0+180.)*CONVE)
c      NHY0=SIN(THET0*CONVE)*SIN((PHI0+180.)*CONVE)
c      NHZ0=COS(THET0*CONVE)
c      ALP=ATAN2(BN,BE)
c      BET=ACOS(BV)
c      A(1,1)=COS(BET)*COS(ALP)
c      A(1,2)=COS(BET)*SIN(ALP)
c      A(1,3)=-SIN(BET)
c      A(2,1)=-SIN(ALP)
c      A(2,2)=COS(ALP)
c      A(2,3)=0.
c      A(3,1)=SIN(BET)*COS(ALP)
c      A(3,2)=SIN(BET)*SIN(ALP)
c      A(3,3)=COS(BET)
c      NH10=A(1,1)*NHX0+A(2,1)*NHY0+A(3,1)*NHZ0
c      NH20=A(1,2)*NHX0+A(2,2)*NHY0+A(3,2)*NHZ0
c      NH30=A(1,3)*NHX0+A(2,3)*NHY0+A(3,3)*NHZ0
c-----------------------------------------------------------


c      EMU = refractive index
c      Y(5)=-NH20*EMU   --> involves thet0, Bw, phi0, index ref
c      Y(6)=NH10*EMU --> involves thet0, Bw, phi0, index ref
c          CONVE=0.0174532925 = pi/360




      X1=1000.
      R1=1.+ALT1/RT
      RFP=1/(F**2)
c      Y(1)= 1.+ALT0/RT        --> RE  
c      Y(2)=(90.-LAT0)*CONVE   --> Initial latitude
c      FI0=90.-LAT0            --> Initial latitude
c      Y(3)= LON0*CONVE        --> Initial Longitude
c      Y2=(90.-LAT1)*CONVE     --> Stepped latitude
      CALL BAD (Y,B,DB,BT)

c.....MAGNETIC FIELD
c      BV=B(1)/BT
c      BN=-B(2)/BT
c      BE=B(3)/BT
c      BT = SQRT(B(1)**2 + B(2)**2 + B(3)**2)









      DATA  AM/1836./
c      XX=Y(1)*COS(Y(2))  --> XX = RE*cos((90.-LAT0))
c      YY=Y(1)*SIN(Y(2))  --> YY = RE*sin((90.-LAT0))
c      FID=90.-Y(2)/CONVE  --> FID = 90 - (90.-LAT0)
c      LOND=Y(3)/CONVE     --> LOND = LON0

c QUANTITIES INVOLVING THET0, MAGNETIC FIELD DIRECTION, PHI0, index ref
c      FKE=Y(6)/EMU
c      FKN=-Y(5)/EMU
c      FKV=Y(4)/EMU


c QUANTITIES INVOLVING theta_kb, Bfield, PHI0 and ____ and ___
      CALL  ENXY(B(3),-B(2),B(1),FKE,FKN,FKV,FKX,FKY,FKZ,ALP,BET,THD,PHI)
      IECR1=IECR
      IECR2=IECR
      ALT=(Y(1)-1.)*RT
c___ f/fge
      FGF=F/FH(4)    
c___ whistler mode Gendrin angle
      COSG=2.*FGF
      IF(COSG.GT.1.)COSG=1.
      THG=ACOS(COSG  )/CONVE
c___ whistler mode resonance angle
      THR=1.570796
      IF(FGF.LE.1.) THR=ACOS(FGF)/CONVE
c____
c      IF(THD.GT.90.)THG=180.-THG
c      IF(THD.GT.90.)THR=180.-THR
c      ELE= Y(1) /(SIN(Y(2)))**2   --> McIllwain L




      Q=0.
      DO 92 I=2,4
      QI(I)=ALP0(I-1)*EXNOR(I)
      Q=Q+QI(I)
92    CONTINUE
      DO 94 I=1,4
      ALPHA(I)=QI(I)/Q
94    CONTINUE
      F0E=SQRT(FP2(4))/1000.
      GF=FH(4)/1000.
      FLHR=SQRT(((ALPHA(2)+ALPHA(3)/4.+ALPHA(4)/16.)/AM)
     1/(1./(F0E*F0E)+1./(GF*GF)))
      THE=THD
      IF(THD.GT.90.)THE=180.-THD
      ST=SIN(THE*CONVE)
      CT=COS(THE*CONVE)
c   --correction of PHI to the interval 0-360
      PHI=PHI+180.
c   --theta (angle from B0) for the group velocity
      PSI=ATAN2(ST*(CT-2.*FGF),1.+CT*(CT-2.*FGF))/CONVE
      IF(THD.GT.90.)PSI=180.-PSI
c   --angle between k and vertial, 0 up, 180 down
      CALL ANG(0.,0.,1.,Y(6), Y(5),Y(4),akvert)
c   --azimuth of k around the radial direction, 0 at meridian, growing lat
      akazi=ATAN2(-Y(6),-Y(5))/conve
      if (akazi.lt.-1e-6) akazi=akazi+360.
c   --step relative to the wave length (km)
      wavelength=299792458./EMU/F/1000.
      relsstep=(Y(7)-YPRC(7))*RT/wavelength*100.
c   --step relative to the wave period
      reltstep=(Y(8)-YPRC(8))*F*100.







c old version
       DO 100 step=0,npoints
         Y(2)=1.570796
         Y(1)=rstart+step*(rstop-rstart)/npoints
         call densi(Y)
         DENS=FP2(4)/(1.E+6*CFN(4))
         pH=FP2(1)/(1.E+6*CFN(1))/Dens*100.
         pHe=FP2(2)/(1.E+6*CFN(2))/Dens*100.
         pO=FP2(3)/(1.E+6*CFN(3))/Dens*100.
         WRITE(LUT,'(2g10.3,i3,4g10.3)')
     .    Y(1),(Y(1)-1)*RT,ident_dens_region,DENS,pH,pHe,pO
  100  continue



