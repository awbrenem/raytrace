      program trace
c  f77 trace.for -o trace
c
c
c---->
c----> HISTORY:
c---->
c
c Original version written by J.C. Cerisier 1970
c   modified by L. Cairo 1986.
c
c Modified by O. Santolik 1998-2004
c       modifications for using disk files
c       (unused historical layers still remain in the code)
c       new input and output files
c       wave-mode control and stop criteria modified
c       integration algorithm rewritten
c           (Runge-Kutta 4th order with one midpoint and adaptive
c            integration step, search for
c            --INTEGRATION ALGORITHM CONTROLS --
c            in the code to modify the step expansion/contraction criteria)
c       group-time-step/wave-period and trace-step/wavelength
c           output and stop criteria added
c
c modified o.s. 2006
c       magnetic field scaling added
c
c---->
c----> REFERENCES:
c---->
c
c Cairo, L., and F. Lefeuvre, Localization of sources of ELF/VLF hiss
c  observed in the magnetosphere: Three-dimensional ray tracing,
c  J. Geophys. Res., 91, 4352-4364, 1986.
c
c Cerisier, J.C., Propagation perpendiculaire au voisinage de la
c  fréquence de la résonance hybride basse, in Plasma Waves in Space
c  and in the Laboratory, vol.2, pp. 487-521,
c  Edinburgh University Press, Edinburgh, 1970.
c
c Muto, H. and M. Hayakawa,
c  Ray-tracing study of the propagation in the magnetosphere
c  of whistler-mode VLF emissions with frequency above one
c  half the gyrofrequency,
c  Planet. Space Sci., 35, 1397-1404, 1987.
c
c
c---->
c----> INPUT FILE:
c---->
c
c (Remove 'c's in the beginning of each line. File starts by the next line)
cTRACE (ray tracing) input values
c___________________________________________________:______________
c           MAGNETIC-FIELD multiplier (dipole model):   1.00000
cDENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):             1
c                PLASMAPAUSE relative density change:  0.600000E-01
c                          PLASMAPAUSE  L-HALF WIDTH:  0.500000E-01
c                     PLASMAPAUSE L-CENTRAL POSITION:   4.80000
c                  DUCT relative density enhancement: -0.600000
c                                  DUCT L-HALF WIDTH:  0.500000
c                            DUCT L-CENTRAL POSITION:   6.65000
c                   ALTITUDE of REFERENCE LEVEL (km):   22195.0
c             DENSITY AT the REFERENCE LEVEL (cm^-3):   120.000
c                                   nH+/ne at 1000km:   1.00000
c                                  nHe+/ne at 1000km:        0.
c                                   n0+/ne at 1000km:        0.
c                          temperature at 1000km (K):   1000.00
c                             Initial latitude (deg):        0.
c                               Final latitude (deg):   80.0000
c                            Initial longitude (deg):        0.
c                              Initial altitude (km):   24000.0
c                                Final altitude (km):   300.000
c                                     Frequency (Hz):   1800.00
c                       INITIAL VALUE OF THETA (deg):       30.
c                         INITIAL VALUE OF Phi (deg):      180.
c^Z (File ended in the end of the preceeding line)
c
c
c---->
c----> OUTPUT FILE (header and data fields):
c---->
c
cTRACE (ray tracing) @ frequency of    1800.00     Hz
cReg Path(Re)  TimeG(s) %dTimeG/T %dPath/WL    WL(km)  R(Re) Alt(km) Lat^o
cLong^o      L     F/Fge     F/FgH   n(cm-3)   %H+  %He+   %O+ LHR/kHz RefrInd
cVeK^o AzK^o ThK^o ThG^o PhK^o Gen^o Res^o
c  ....values as obtained along the ray trace....
c
c Reg      = indentification of the density region
c             0.. Diffusive equilibrium model
c            -1.. Plasmapause gradient (1/2 period sine localized between
c                 AL-DDK and AL+DDK)
c            -2.. Duct (exp model - 0.1% density change detected)
c            -3.. ionosphere below 390 km (.99*PP  decrease detected,
c                          alt0=89km, scale=140km)
c Path(Re) = integrated ray path in Earth radii
c TimeG(s) = integrated group delay in seconds
c %dTimeG/T= group time step to wave period *100%
c %dPath/WL= ray path step to wavelength *100%
c WL(km)   = wave length in km
c R(Re)    = radial distance (from the centre of the Earth) in Earth radii
c Alt(km)  = altitude above the surface of the Earth
c Lat^o    = magnetic dipole (e.g., SM coordinates) latitude in degrees
c Long^o   = magnetic longitude in degrees
c L        = McIlwain's L parameter (equatorial radial
c            distance of the field line in Earth radii)
c F/Fge    = wave frequency to the electron cyclotron frequency
c F/FgH    = wave frequency to the  proton cyclotron frequency
c n(cm-3)  = plasma number density in particles per cc
c %H+      = percentage of H+ in the ion component
c %He+     = percentage of He+
c %O+      = percentage of O+
c LHR/kHz  = lower hybrid frequency in kHz
c RefrInd  = refractive index N=|k|c/(2 pi f) k wave vector, f frequency
c VeK^o    = angle in degrees between the radial direction and k.
c             0 - k points outward; 180 - toward the Earth
c AzK^o    = azimuth in degrees of k around the radial.
c            0  - if k is in the magnetic
C                 meridian directed toward geographic N (growing latitude)
c            90 - toward W; 180- toward S; 270 - toward E; 360=0
c ThK^o    = angle in degrees between the magnetic field B0 direction and k.
c             0 - parallel 180 - antraparallel
c ThG^o    = angle in degrees between the magnetic field B0 direction and the
c            group velocity (ray tangential direction) -
c            recalculated, approximation (not directly used in the ray tracing).
c PhK^o    = azimuth of k around the the magnetic field B0 direction.
c            0 - k is in the the magnetic meridian plane, directed outward
c                (toward higher L values; toward N for latitudes >0)
c            90 - in the right-hand sense, i.e. toward E.
c            270 - toward W
c Gen^o   = Aprox. Gendrin's angle for the whistler mode (ThK for ThG=0) in
cdegrees.
c Res^o    = Approx. oblique resonance angle for the whistler mode
c
c
c---->
c----> OUTPUT FILE (stop criteria in the end)
c---->
c
c  STOP#1: Final latitude crossed.
c   ...as specified after 'Final latitude (deg):' in the input file
c  STOP#2: Radial distance is below its lower limit.
c   ...as specified after 'Final altitude (deg):' in the input file
c  STOP#3: Radial distance is > 13 Earth radii.
c   ...hard stop to limit the rays within a resonable magnetispheric region
c  STOP#4: Limit of the W.K.B. approximation encountered.
c   ...limit of the W.K.B. approximation (approx. of geometric optics)
c      WKB=Wentzel-Kramers-Brillouin; K.G. Budden, The propagation of
c        radio waves, Cambridge University Press, New York, 1988,
cisbn:0-521-36952-2:
c        an analogy of eq. 7.61, p.178: 1/k |d/dz(1/n_z)|<=0.5
c         1/|vk|^2*abs[d(vk)/d(s)]<=0.5 N (vk is the wavevector, N refr. ind)
c        Similarly Swanson D.G. Plasma Waves, Academic Press, San Diego CA, 1989
c        p. 236: |1/k^2 dk/dx| << 1
c        Here we use "<=0.5" on the RHS, and modify for the discrete steps,
c             1/|vk_i|^2 * |vk_i-vk_{i-1}| / |r_i-r_{i-1}|<=0.5
c      Actual procedure is:
c        (a) the step of the internal integration variable is controled
c        to always have |vk_i-vk_{i-1}|/|vk| between 10^-4 and 10^-3
c        (b) ray tracing stops whenever
c        |r_i-r_{i-1}|/wavelength goes below 0.318e-3 (0.318e-3=1/pi*10^-3)
c  STOP#5: Step decreased below its numerical limit.
c   ...step in the internal integration variable goes below 10^-30
c      shouldn't normally happen STOP#4 acting sooner
c
c  (a copy of the input file follows in the end of the output file)
c
c
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
c
c
c---------------------------------------------------------------------------
      EXTERNAL DERIV,SORTV
      DIMENSION Y(8),U(4,8),W(5,8),Z(8)
c                ....
      DIMENSION G(11,11),H(11,11),CONST(11,11)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH,F,RFP,KBTYP
      COMMON/BB/ G,H,CONST
      COMMON/BB/ TDG(65),TDH(65),TDDG(65),TDDH(65)
      COMMON/DUCT/KDUCT,DEF,DD,AL0,EP,DDK,ALK
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
      COMMON/III/FIP,ANH10,ANH20,ANH30,ALAT0,ALAT1,
     .           ALON0,ALT0,ALT1,THET0,PHI0

      real bmult
      DATA NPHI/20/,NTHET/10/

      write(*,*) '-----------------'
      write(*,*) 'Start ray tracing.'
      write(*,*) '-----------------'
      LUT=9
      IPROG=1
        krun=0
c       input file
      open(1,file='trace_in.txt')
      read(1,'(//52x,g14.6)') bmult
      KBTYP=0
      read(1,'(52x,I14)') KDUCT
      read(1,'(52x,g14.6)')EP
      read(1,'(52x,g14.6)')DDK
      read(1,'(52x,g14.6)')ALK
      read(1,'(52x,g14.6)')DEF
      read(1,'(52x,g14.6)')DD
      read(1,'(52x,g14.6)')AL0
      read(1,'(52x,g14.6)')HBASE
      RBASE=HBASE+RT
      read(1,'(52x,g14.6)')AN0
      read(1,'(52x,g14.6)')ALP0(1)
      read(1,'(52x,g14.6)')ALP0(2)
      read(1,'(52x,g14.6)')ALP0(3)
      read(1,'(52x,g14.6)')THERM
      read(1,'(52x,g14.6)')ALAT0
      read(1,'(52x,g14.6)')ALAT1
      read(1,'(52x,g14.6)')ALON0
      read(1,'(52x,g14.6)')ALT0
      read(1,'(52x,g14.6)')ALT1
      read(1,'(52x,g14.6)')F
      read(1,'(52x,g14.6)')THET0
      read(1,'(52x,g14.6)')PHI0
      IECR=4
        LUT=3
        open(LUT,file='trace_ta.txt')
c _____ Output file header
c
      WRITE(LUT,'(a,g14.6,a)')
     . 'TRACE (ray tracing) @ frequency of ',f,' Hz'
      write(LUT,606)
606     FORMAT('Reg Path(Re)  TimeG(s) %dTimeG/T %dPath/WL    WL(km)',
     &'    R(Re) Alt(km)    Lat^o   Long^o     L(Re)',
     &'         F/Fge           F/FgH         n(cm-3)       %H+  ',
     &'%He+   %O+',
     &' LHR/kHz',
     &'   RefrInd VeK^o AzK^o ThK^o ThG^o PhK^o Gen^o Res^o')
c _____
c

5     IF(KBTYP.EQ.3)GO TO 8
      DO 6 I=1,65
      TDG(I)=0.
      TDH(I)=0.
      TDDG(I)=0.
6     TDDH(I)=0.
      DO 7 I=1,11
      DO 7 J=1,11
      G(I,J)=0.
      H(I,J)=0.
7     CONST(I,J)=0.
      G(2,1)=0.304012*bmult
      IF(KBTYP.EQ.1)GO TO 8
      G(2,2)=0.021638
      H(2,2)=-0.057782
8     T=6.16
      K=0
         DO 10 I=2,11
         DO 10 J=1,I
      K=K+1
        G(I,J) =(G(I,J) + T*TDG(K) + (T**2)*TDDG(K)/2.) / 10000.
 10     H(I,J) =(H(I,J) + T*TDH(K) + (T**2)*TDDH(K)/2. ) / 10000.
      CALL INIT (X,Y,X1,R1,Y2)
      KEXPL=0
      NF=8
      CALL RUKMI (X,X1,R1,Y2,NF,Y,Z,U,W,IFIN)
      WRITE(LUT,'(a)') 'TRACE (ray tracing) input values'
      write(LUT,'(a,a)') '______________________________',
     ,             '_____________________:______________'
      write(LUT,'(a52,I14)')
     . 'MAGNETIC-FIELD multiplier    (1. for normal dipole):', bmult
      write(LUT,'(a52,I14)')
     . 'DENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):', KDUCT
      write(LUT,'(a52,g14.6)')
     . 'PLASMAPAUSE relative density change:',EP
      write(LUT,'(a52,g14.6)')
     . '  PLASMAPAUSE  L-HALF WIDTH:',DDK
      write(LUT,'(a52,g14.6)')
     . '  PLASMAPAUSE L-CENTRAL POSITION:',ALK
      write(LUT,'(a52,g14.6)')
     . '  DUCT relative density enhancement:',DEF
      write(LUT,'(a52,g14.6)')
     . '  DUCT L-HALF WIDTH:',DD
      write(LUT,'(a52,g14.6)')
     . '  DUCT L-CENTRAL POSITION:',AL0
      write(LUT,'(a52,g14.6)')
     . 'ALTITUDE of REFERENCE LEVEL (km):',HBASE
      write(LUT,'(a52,g14.6)')
     . 'DENSITY AT the REFERENCE LEVEL (cm^-3):',AN0
      write(LUT,'(a52,g14.6)')
     . ' nH+/ne at 1000km:',ALP0(1)
      write(LUT,'(a52,g14.6)')
     . ' nHe+/ne at 1000km:',ALP0(2)
      write(LUT,'(a52,g14.6)')
     . ' n0+/ne at 1000km:',ALP0(3)
      write(LUT,'(a52,g14.6)')
     . ' temperature at 1000km (K):',THERM
      write(LUT,'(a52,g14.6)')
     . 'Initial latitude (deg):',ALAT0
      write(LUT,'(a52,g14.6)')
     . 'Final latitude (deg):',ALAT1
      write(LUT,'(a52,g14.6)')
     . 'Initial longitude (deg):',ALON0
      write(LUT,'(a52,g14.6)')
     . 'Initial altitude (km):',ALT0
      write(LUT,'(a52,g14.6)')
     . 'Final altitude (km):',ALT1
      write(LUT,'(a52,g14.6)')
     . 'Frequency (Hz):',F
      write(LUT,'(a52,g14.6)')
     . 'INITIAL VALUE OF THETA (deg):',THET0
      write(LUT,'(a52,g14.6)')
     . 'INITIAL VALUE OF Phi (deg):',PHI0
      call print_density
      close(1)
        close(LUT)
      write(*,*) '-----------------'
      write(*,*) 'Ray tracing done.'
      write(*,*) '-----------------'
      STOP
      END






      BLOCK DATA
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP
      COMMON/BB/    G(11,11),H(11,11),CONST(11,11)
      COMMON/BB/ TDG(65),TDH(65),TDDG(65),TDDH(65)
      COMMON/WORK/CFN(4),ARRAY(16) ,BT,B(3),FH(4),CFH(4),IEPS(4),EMU
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
      COMMON/DUCT/KDUCT,DEF,DD,AL0,EP,DDK,ALK
      COMMON/III/FIP,ANH10,ANH20,ANH30,ALAT0,ALAT1,ALON0,ALT0,
     ,               ALT1,THET0,PHI0
      DATA CFH/19.36,4.84,1.21,3.55E+04/,ALP0/0.9,0.1,0./
      DATA CFN/.0438,.01095,.00274,80.61/,IEPS/3*1,-1/,AN0/3.2  /
      DATA RT,THERM,RBASE,RZERO,SCBOT /6371.,1000.,43317.,6460.,140./
      DATA KDUCT,DEF,DD,AL0/0,-0.6,0.5,6.65/,EP,DDK,ALK/ 1.,0.7,2.7/
      DATA ANH10,ANH20,ANH30,ALAT0,ALAT1,ALON0,ALT0,ALT1
     &/0.,0.,-1.,29.   ,-45., 3.   ,25000.,10000./
      DATA G /
     1   .000000E+00 ,.304012E+00 ,.231015E-01,-.326775E-01,-.415318E-01
     2  ,.183881E-01,-.710324E-02,-.193586E-01,-.427324E-02,-.987593E-02
     3  ,.523234E-02 ,.000000E+00 ,.216380E-01,-.519251E-01 ,.608973E-01
     4 ,-.444655E-01,-.361625E-01,-.108692E-01 ,.190471E-01,-.435702E-02
     5 ,-.738940E-02 ,.218957E-02 ,.000000E+00 ,.000000E+00,-.137724E-01
     6 ,-.247251E-01,-.196790E-01,-.175530E-01 ,.119553E-03,-.228790E-02
     7  ,.521565E-02,-.814875E-02 ,.463522E-02 ,.000000E+00 ,.000000E+00
     8  ,.000000E+00,-.696649E-02 ,.831849E-02 ,.135538E-02 ,.237413E-01
     A ,-.319462E-02 ,.397627E-02 ,.125304E-01,-.132224E-02 ,.000000E+00
     B  ,.000000E+00 ,.000000E+00 ,.000000E+00,-.197079E-02 ,.350305E-02
     C  ,.818528E-04 ,.300078E-02 ,.163090E-02,-.682146E-02 ,.327238E-02
     D  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     E  ,.436370E-03 ,.465362E-04 ,.222280E-03,-.815681E-03,-.158347E-02
     F ,-.473059E-02 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     G  ,.000000E+00 ,.000000E+00 ,.731473E-03,-.375382E-03 ,.556083E-03
     H ,-.347958E-04,-.194204E-02 ,.000000E+00 ,.000000E+00 ,.000000E+00
     I  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00,-.233013E-04
     J ,-.325887E-03,-.120536E-03 ,.400863E-04 ,.000000E+00 ,.000000E+00
     K  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     L  ,.000000E+00,-.463762E-04,-.232557E-04,-.147286E-03 ,.000000E+00
     M  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     N  ,.000000E+00 ,.000000E+00 ,.000000E+00,-.121809E-05,-.530956E-04
     O  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     P  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     Q ,-.652989E-05 /
      DATA H /
     1   .000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     2  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     3  ,.000000E+00 ,.000000E+00,-.577820E-01 ,.334632E-01 ,.130251E-01
     4 ,-.887097E-02,-.518495E-03 ,.228727E-02 ,.190471E-01,-.361968E-02
     5  ,.285383E-01 ,.243286E-03 ,.000000E+00 ,.000000E+00,-.175716E-02
     6 ,-.441132E-02 ,.107336E-01,-.905317E-02,-.156017E-01 ,.793525E-02
     7  ,.656163E-02,-.149937E-01,-.948113E-02 ,.000000E+00 ,.000000E+00
     8  ,.000000E+00 ,.105778E-02,-.481079E-04 ,.540272E-02,-.563895E-02
     9  ,.165874E-02,-.173962E-02,-.522791E-02 ,.165280E-02 ,.000000E+00
     A  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.182363E-02 ,.241597E-02
     B  ,.127690E-02,-.864424E-03 ,.409063E-02 ,.169127E-02,-.303864E-02
     C  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     D ,-.578085E-03 ,.344368E-03,-.150039E-02,-.682206E-03 ,.640127E-03
     E  ,.325228E-02 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     F  ,.000000E+00 ,.000000E+00 ,.893351E-04 ,.544910E-03,-.150348E-02
     G ,-.156581E-02 ,.537160E-03 ,.000000E+00 ,.000000E+00 ,.000000E+00
     H  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.138513E-03
     I  ,.175477E-04,-.866354E-03 ,.721554E-03 ,.000000E+00 ,.000000E+00
     J  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     K  ,.000000E+00 ,.107166E-03,-.258397E-05,-.327303E-03 ,.000000E+00
     L  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     M  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.913573E-05,-.265478E-04
     N  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     O  ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00 ,.000000E+00
     P  ,.118725E-04 /
      DATA TDG/
     1 -.1403E-03,-.8760E-04 ,.3493E-03 ,.1558E-05 ,.3949E-04 ,.2325E-04
     2 ,.3251E-03,-.4473E-04 ,.4656E-04,-.6343E-04,-.4980E-04 ,.6847E-04
     3,-.1380E-04 ,.2225E-04,-.1267E-03,-.6099E-04,-.2566E-03 ,.1882E-05
     4 ,.1331E-04,-.1234E-04 ,.6063E-04,-.1550E-03,-.1225E-03,-.2341E-03
     5,-.4529E-04,-.2326E-06,-.1544E-05 ,.1528E-03 ,.1205E-03 ,.4170E-03
     6 ,.1843E-03,-.3704E-05 ,.3704E-04 ,.4117E-05 ,.4142E-05,-.1759E-03
     7,-.3351E-03,-.9533E-03 ,.4556E-04,-.9090E-04 ,.1038E-04,-.2952E-04
     8 ,.3760E-05 ,.2632E-05 ,.9496E-04 ,.1656E-03 ,.1303E-02,-.6638E-04
     9 ,.4510E-04 ,.1313E-03 ,.6263E-04,-.3540E-04,-.9560E-05 ,.2801E-05
     A ,.1804E-04 ,.3162E-03,-.1854E-02 ,.2975E-03,-.1986E-03 ,.1478E-04
     B,-.2066E-04,-.3407E-04,-.1309E-04 ,.6105E-05,-.1840E-05 /
      DATA TDH /
     1  .0000E+00 ,.3710E-04 ,.0000E+00 ,.2478E-03 ,.1439E-03 ,.0000E+00
     2,-.1592E-03,-.4899E-04 ,.5518E-04 ,.0000E+00 ,.1211E-03 ,.5478E-05
     3,-.3932E-04 ,.4821E-04 ,.0000E+00,-.2277E-03,-.1221E-03 ,.1228E-03
     4,-.1109E-04 ,.8418E-06 ,.0000E+00,-.9451E-05,-.1344E-04,-.2540E-03
     5 ,.6493E-04,-.7678E-05,-.5642E-05 ,.0000E+00 ,.3405E-03,-.2896E-05
     6,-.8805E-04,-.9261E-04 ,.2037E-04,-.1186E-04,-.5825E-05 ,.0000E+00
     7 ,.3351E-03 ,.1177E-03,-.1242E-04 ,.2112E-03,-.7415E-05,-.6865E-05
     8 ,.9024E-05 ,.2694E-05 ,.0000E+00,-.8408E-03,-.5867E-03,-.2489E-04
     9,-.1973E-03 ,.1010E-04 ,.1739E-05,-.3390E-04 ,.1291E-05,-.4567E-05
     A ,.0000E+00 ,.1484E-02 ,.1348E-02,-.3305E-04,-.5843E-04 ,.4656E-03
     B ,.2892E-04,-.1403E-04 ,.2454E-05 ,.1194E-04 ,.1187E-06 /
      DATA TDDG /
     1   .620E-06,-.114E-05 ,.231E-05 ,.311E-06 ,.219E-05 ,.307E-05
     2  ,.826E-06,-.542E-06 ,.144E-05,-.437E-07 ,.243E-05,-.665E-06
     3 ,-.146E-06 ,.717E-06,-.354E-05,-.101E-06,-.576E-05,-.376E-06
     4 ,-.332E-06,-.392E-06 ,.866E-06,-.283E-05,-.149E-05,-.498E-05
     5  ,.600E-06,-.604E-06,-.154E-06 ,.375E-05 ,.212E-05 ,.984E-05
     6  ,.819E-06 ,.740E-06 ,.166E-05 ,.242E-07 ,.258E-07,-.301E-05
     7 ,-.536E-05,-.218E-04 ,.331E-05,-.401E-05 ,.296E-06,-.343E-06
     8  ,.200E-06 ,.438E-07 ,.474E-05 ,.127E-05 ,.293E-04,-.414E-05
     9  ,.394E-05 ,.202E-05 ,.156E-05,-.452E-06,-.129E-06 ,.548E-07
     A  ,.541E-05 ,.729E-05,-.421E-04 ,.132E-04,-.818E-05,-.739E-06
     B ,-.413E-06,-.200E-06,-.409E-06 ,.530E-07,-.237E-07 /
      DATA TDDH /
     1   .000E+00 ,.430E-06 ,.000E+00,-.935E-06 ,.138E-06 ,.000E+00
     2 ,-.290E-05 ,.135E-06,-.624E-06 ,.000E+00,-.221E-06,-.219E-05
     3  ,.732E-06 ,.347E-06 ,.000E+00 ,.467E-05,-.537E-06 ,.329E-06
     4 ,-.221E-07 ,.168E-06 ,.000E+00,-.378E-05 ,.164E-05,-.149E-05
     5  ,.158E-05,-.674E-06 ,.671E-07 ,.000E+00 ,.496E-05,-.463E-05
     6 ,-.286E-05,-.617E-06 ,.493E-06,-.387E-06,-.711E-07 ,.000E+00
     7  ,.100E-04 ,.672E-05,-.207E-05 ,.294E-05 ,.000E+00 ,.205E-06
     8  ,.225E-06 ,.188E-07 ,.000E+00,-.280E-04,-.760E-05 ,.165E-05
     9 ,-.507E-05,-.202E-05 ,.173E-06,-.678E-06 ,.103E-06,-.115E-06
     A  ,.000E+00 ,.291E-04 ,.294E-04,-.165E-05,-.116E-05 ,.813E-05
     B  ,.413E-06,-.200E-06 ,.818E-07 ,.159E-06,-.593E-08 /
      DATA CONST /
     1  .0000E+00 ,.0000E+00 ,.3333E+00 ,.2666E+00 ,.2571E+00 ,.2539E+00
     2 ,.2525E+00 ,.2517E+00 ,.2513E+00 ,.2509E+00 ,.2507E+00 ,.0000E+00
     3 ,.0000E+00 ,.0000E+00 ,.2000E+00 ,.2285E+00 ,.2380E+00 ,.2424E+00
     4 ,.2447E+00 ,.2461E+00 ,.2470E+00 ,.2477E+00 ,.0000E+00 ,.0000E+00
     5,-.1000E+01 ,.0000E+00 ,.1428E+00 ,.1904E+00 ,.2121E+00 ,.2237E+00
     6 ,.2307E+00 ,.2353E+00 ,.2383E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     7,-.3333E+00 ,.0000E+00 ,.1111E+00 ,.1616E+00 ,.1888E+00 ,.2051E+00
     8 ,.2157E+00 ,.2229E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     9,-.2000E+00 ,.0000E+00 ,.9091E-01 ,.1398E+00 ,.1692E+00 ,.1882E+00
     A ,.2012E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     B,-.1428E+00 ,.0000E+00 ,.7692E-01 ,.1230E+00 ,.1529E+00 ,.1733E+00
     C ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     D,-.1111E+00 ,.0000E+00 ,.6666E-01 ,.1098E+00 ,.1393E+00 ,.0000E+00
     E ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     F,-.9091E-01 ,.0000E+00 ,.5882E-01 ,.9907E-01 ,.0000E+00 ,.0000E+00
     G ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     H,-.7692E-01 ,.0000E+00 ,.5263E-01 ,.0000E+00 ,.0000E+00 ,.0000E+00
     I ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     J,-.6666E-01 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     K ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00 ,.0000E+00
     L,-.5882E-01 /
      
      END 



      SUBROUTINE BAD(Y,B,DB,BT)
      real Y(8),  B(3), DB(3,3),   G(11,11), H(11,11), P(11,11),
     1  DP(11,11), DDP(11,11), CONST(11,11), SP(11), CP(11)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH,F,RFP
      COMMON/BB/    G,H,CONST,TDG(65),TDH(65),TDDG(65),TDDH(65)
      Y(1)=Y(1)*6371200.
      DO 39 I=1,3
      B(I) = 0.
      DO 39 J=1,3
 39    DB(I,J) = 0.
      DO 40 I1=1,11
      DO 40 I2=1,11
      P(I1,I2) = 0.
      DDP(I1,I2) = 0.
 40    DP(I1,I2) = 0.
      P(1,1) = 1.
      ST = SIN(Y(2))
      CT = COS(Y(2))
      COT = CT/ST
      SP(1) = 0.
      CP(1) = 1.
       SP(2) = SIN(Y(3))
      CP(2) = COS(Y(3))
      DO 41 M=3,11
      SP(M) = SP(2)*CP(M-1) + CP(2)*SP(M-1)
 41   CP(M) = CP(2)*CP(M-1) - SP(2)*SP(M-1)
      AOR =  0.63712E+07/Y(1)
      AR1 = AOR**2
      P(2,2) =  ST
      DP(2,2) = CT
      DDP(2,2) = -ST
      P(2,1) = CT
      DP(2,1) = -ST
      DDP(2,1) = -CT
      DO 8 N=2,11
      AR1 = AOR*AR1
      RN = FLOAT(N)
      RN2 = FLOAT(N+1)/0.63712E+07
      DO 8 M=1,N
      IF(N-2) 42,7,42
 42   IF(N-M) 6,5,6
 5    P(N,N) = ST * P(N-1,N-1)
      DP(N,N) = ST * DP(N-1,N-1) + CT* P(N-1,N-1)
      DDP(N,N) = 2.* CT* DP(N-1,N-1) + ST* DDP(N-1,N-1) -ST* P(N-1,N-1)
      GO TO 7
 6     P(N,M) = CT* P(N-1,M) - CONST(N,M) * P(N-2,M)
      DP(N,M) = CT* DP(N-1,M) -ST* P(N-1,M) -CONST(N,M) *DP(N-2,M)
      DDP(N,M) = -2.* ST* DP(N-1,M) + CT* DDP(N-1,M) -
     1 CT* P(N-1,M) - CONST(N,M) * DDP(N-2,M)
 7     TAR1 = (G(N,M)*CP(M) + H(N,M)*SP(M)) * AR1
       TAR2 =(-G(N,M)*SP(M) + H(N,M)*CP(M)) * AR1
      PT1 = P(N,M) * TAR1
      DPT1 = DP(N,M) * TAR1
      PT2 = P(N,M) * TAR2
      DPT2 = DP(N,M) * TAR2
      FL = FLOAT(M-1)
      RMS = FL/ST
      B(1) = B(1) - RN*PT1
      B(2) = B(2) + DPT1
      B(3) = B(3) + RMS*PT2
      DB(1,1) = DB(1,1) + RN * RN2* PT1 * AOR
      DB(1,2) = DB(1,2) - RN* DPT1
      DB(1,3) = DB(1,3) - FL* RN* PT2
      DB(2,1) = DB(2,1) - RN2* DPT1* AOR
      DB(2,2) = DB(2,2) + DDP(N,M)* TAR1
      DB(2,3) = DB(2,3) + FL* DPT2
      DB(3,1) = DB(3,1) - RN2* RMS* PT2* AOR
      DB(3,2) = DB(3,2) + RMS* (DPT2- COT*PT2)
 8     DB(3,3) = DB(3,3) - FL* RMS*PT1
      BT = SQRT(B(1)**2 + B(2)**2 + B(3)**2)
      COEC=1.256636E-06
      DO 21 I=1,3
   21 B(I)=B(I)/COEC
      DCOEC= 6371200./COEC
      DO 22 I=1,3
   22 DB(I,1)=DB(I,1)*DCOEC
      DO 24 I=1,3
      DO 24 J=1,2
   24 DB(I,J+1)=DB(I,J+1)/COEC
      BT=BT/COEC
      Y(1)=Y(1)/6371200.
      RETURN
      END


      SUBROUTINE ENXY(HE,HN,HV,FKE,FKN,FKV,FKX,FKY,FKZ,ALP,BET,THET,PHI)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH
      BT=SQRT ( HE**2+HN**2+HV**2)
      BE=HE/BT
      BN=HN/BT
      BV=HV/BT
      ALP= ATAN2(BN,BE)
      BET=ACOS(BV)
      AM11=COS(BET)*COS(ALP)
      AM12=COS(BET)*SIN(ALP)
      AM13=-SIN(BET)
      AM21=-SIN(ALP)
      AM22=COS(ALP)
      AM23=0.
      AM31=SIN(BET)*COS(ALP)
      AM32=SIN(BET)*SIN(ALP)
      AM33=COS(BET)
      FKX=AM11*FKE+AM12*FKN+AM13*FKV
      FKY=AM21*FKE+AM22*FKN+AM23*FKV
      FKZ=AM31*FKE+AM32*FKN+AM33*FKV
      PHI=ATAN2(FKY,FKX)/CONVE
      ABSK=SQRT(FKX**2+FKY**2+FKZ**2)
      COST=FKZ/ABSK
      SINT=SQRT ( 1.-COST**2)
      THET=ATAN2(SINT,COST)/CONVE
      ALP=ALP/CONVE
      BET=BET/CONVE
      RETURN
      END


      SUBROUTINE ANG(X1,Y1,Z1,X2,Y2,Z2,A)
      AM1=X1*X1+Y1*Y1+Z1*Z1
      AM2=X2*X2+Y2*Y2+Z2*Z2
      COSA=(X1*X2+Y1*Y2+Z1*Z2)/(SQRT ( AM1*AM2))
      VXY1=Y1*Z2-Y2*Z1
      VXY2=Z1*X2-X1*Z2
      VXY3=X1*Y2-Y1*X2
      VXY=  SQRT ( VXY1**2+VXY2**2+VXY3**2)
      SINA=VXY/SQRT ( AM1*AM2)
      A=ATAN2( SINA  ,COSA )/0.017453292
      RETURN
      END


      FUNCTION ACOS(X)
      ACOS=ATAN2(SQRT(1.-X*X),X)
      RETURN
      END


      subroutine print_density
        DIMENSION B(3)
        DIMENSION Y(8),FP2(4),DFP2(4,3)
        COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP,KBTYP
        COMMON/WORK/CFN(4),FP2,DFP2,BT,B,FH(4),CFH(4),IEPS(4),EMU
        COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
c     ____ identification of the density region
c        0.. Diffusive equilibrium model
c       -1.. Plasmapause gradient (sine localized between AL-DDK and AL+DDK)
c       -2.. Duct (exp model - 0.1% density change)
c       -3.. ionosphere below 390 km (.99*PP  decrease, alt0=89km, scale=140km)
      integer ident_dens_region
      common/ident_dens_region/ ident_dens_region
        real Dens, pH,pHe,pO, rstart,rstop
        integer npoints
       rstart=1.
       rstop=7.
       npoints=300
       WRITE(LUT,'(a)') '--------- Density model ----------'
         Y(2)=1.570796
         Y(1)=rbase/RT
         call densi(Y)
         DENS=FP2(4)/(1.E+6*CFN(4))
         pH=FP2(1)/(1.E+6*CFN(1))/Dens*100.
         pHe=FP2(2)/(1.E+6*CFN(2))/Dens*100.
         pO=FP2(3)/(1.E+6*CFN(3))/Dens*100.
       WRITE(LUT,'(a)') 'Reference point: '
       WRITE(LUT,'(a)')
     .    '  R (Re)    Alt (km) ID   n(cm-3)      %H+     %He+     %O+'
         WRITE(LUT,'(2g10.3,i3,4g10.3)')
     .    Y(1),(Y(1)-1)*RT,ident_dens_region,DENS,pH,pHe,pO
       WRITE(LUT,'(a)') 'Density profile at the dipole equator: '
       WRITE(LUT,'(a)')
     .    '  L (Re)    Alt (km) ID   n(cm-3)      %H+     %He+     %O+'
       do 100 step=0,npoints
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
      return
      end


      SUBROUTINE DENSI(Y)
C     ELECTRON DENSITY DISTRIBUTION.............
C     KDUCT=0....DIFFUSIF EQUILIBRIUM MODEL
C     KDUCT=1....ID+PLASMAPAUSE
C     KDUCT=2....ID+DUCT
C     PLASMAPAUSE PARAMETERS :
C     EP(DENSITY FALL),DDK(L-HALF WIDTH),ALK(L-CENTRAL POSITION)
C     DUCT PARAMETERS :
C     DEF(DENSITY ENHANCEMENT),DD(L-HALF WIDTH),ALK(L-CENTRAL POSITION)
      DIMENSION Y(1),  PSH(3),P(3),AH(3),AN(4)
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
      COMMON/WORK/CFN(4),FP2(4),DFP2(4,3) ,
     ,            BT,B(3),FH(4),CFH(4),IEPS(4),EMU

      COMMON/DUCT/KDUCT,DEF,DD,AL0,EP,DDK,ALK
c     ____ indentification of the density region
c        0.. Diffusive equilibrium model
c       -1.. Plasmapause gradient (sine localized between ALK-DDK and ALK+DDK)
c       -2.. Duct (exp model - 0.1% density change)
c       -3.. ionosphere below 390 km (.99*PP  decrease, alt0=89km, scale=140km)
      integer ident_dens_region
      common/ident_dens_region/ ident_dens_region
      DATA  CK/1.38E-23/,PM/1.67E-27/
      R0=RBASE/RT
      G0=9.81/R0**2
      AH(1)=CK*THERM/(PM*G0*RT*1000.)
      AH(2)=AH(1)/4.
      AH(3)=AH(1)/16.
20    ANW=1.
      SIFI=SIN(Y(2))
      COFI=COS(Y(2))
      R=Y(1)
      EZ=R0*(R-R0)/R
      Q=0
      DQDZ=0
      DO 201 J=1,3
      P(J)=ALP0(J)*EXP(-EZ/AH(J))
      Q=Q+P(J)
      PSH(J)=-P(J)/AH(J)
      DQDZ=DQDZ+PSH(J)
  201 CONTINUE
C....MODELE EQUILIBRE DIFFUSIF........................
      RZR2=(R0/R)**2.
      DO 251 J=1,3
      AN(J)=AN0*P(J)/SQRT(Q)
      FP2(J)=1.E+6*CFN(J)*AN(J)
      DFP2(J,1)=-FP2(J)*(1/AH(J)+DQDZ/(2*Q))*RZR2
      DFP2(J,2)=0
      DFP2(J,3)=0.
  251 CONTINUE
      J=4
      AN(J)=AN0*SQRT(Q)
      FP2(J)=1.E+6*CFN(J)*AN(J)
      DFP2(J,1)=FP2(J)*DQDZ*RZR2/(2*Q)
      DFP2(J,2)=0
      DFP2(J,3)=0
      ident_dens_region=0
C....MODELE DE BASSE IONOSPHERE........
      R=R*RT
      EXPB=EXP(-((R-RZERO)/SCBOT)**2)
      DO 60 J=1,4
      FP2(J)=FP2(J)*(1.-EXPB)
60    DFP2(J,1)=DFP2(J,1)*(1.-EXPB)+2.*FP2(J)*(R-RZERO)*EXPB/(SCBOT**2)
      if (EXPB.gt.0.01) ident_dens_region=-3
C....MODELE DE PLASMAPAUSE.............................
      AL=R/(RT*SIFI**2)
      IF(KDUCT.EQ.0)GO TO 150
      IF(AL-ALK+DDK)73,74,74
   73 FPL=1.
      DFR=0.0
      DFFI=0.0
      GO TO 92
   74 IF (AL-ALK-DDK)75,76,76
   75 VFI=1.57079*ANW*(AL-ALK)/DDK
      PI=3.14159
      FPL=((EP-1.)/2.)*SIN(VFI)+(EP+1.)/2.
      DFR=-ANW*PI*(1.-EP)*COS(VFI)/(4.*DDK*(SIFI**2))
      DFFI=(PI*(1-EP)*COS(VFI)*R*COFI/(2.*DDK*(SIFI**3)))*ANW
      ident_dens_region=-1
      GO TO 92
   76 FPL=EP
      DFR=0.0
      DFFI=0.0
   92 DO 93 J=1,4
      DFP2(J,1)=DFP2(J,1)*FPL+FP2(J)*DFR
      DFP2(J,2)=DFP2(J,2)*FPL+FP2(J)*DFFI
      DFP2(J,3)=0.
   93 FP2(J)=FP2(J)*FPL
C....MODELE DE GUIDE...................................
100   IF(KDUCT.LT.2)GO TO 150
      FPL=1.+DEF*EXP(-0.5*(AL-AL0)*(AL-AL0)/(DD*DD))
      DFR=-   (AL-AL0)*(FPL-1.)/(DD*SIFI**2.)
      DFFI=2.*(AL-AL0)*(FPL-1.)*R*COFI/(DD*SIFI**3.)
      DO 110 J=1,4
      DFP2(J,1)=DFP2(J,1)*FPL+FP2(J)*DFR
      DFP2(J,2)=DFP2(J,2)*FPL+FP2(J)*DFFI
      DFP2(J,3)=0.
110   FP2(J)=FP2(J)*FPL
      if (abs(1-FPL).gt.0.001) ident_dens_region=-2
150   RETURN
      END




      SUBROUTINE RUKMI(X,X1,R1,Y2,NF ,Y,Z,U,W,IFIN)
C INTEGRATION OF THE SISTEM OF DIFFERENTIAL EQUATIONS   DY/DX=F(X,Y)
C BY THE MILNE METHOD (PREDICTOR-CORRECTOR ) OF 4TH ORDER
C RUNGE-KUTTA METHOD OF 4TH-ORDER FOR THE 4 FIRST VALUES OF THE DERIVATIVE
C
C ARGUMENTS :
C X  :INITIAL VALUES AND THEN THE VALUES OF X VARIABLE
C X1 :FINAL VALUE OF X
C XP :STEP OF INTEGRATION ON X  ----> redefined below
C NF :TOTAL NUMBER OF FONCTIONS TREATED
C Y(NF):ARRAY OF VALUES OF THE N FONCTIONS TO INTEGRATE
C Z(NF):ARRAY OF VALUES OF THE NF DERIVATIVES
C U(4,NF):ARRAY OF SUBSIDIARY DERIVATIVES(RUNGE-KUTTA)
C W(5,NF):ARRAY OF DERIVATIVES FOR THE PRECEDING STEPS(MILNE)
C DERIV(X,Y,Z,R1,Y2,IFIN):SUBROUTINE TO COMPUTE THE DERIVATIVES
C SORTI(X,Y,Z):SUBROUTINE FOR THE OUTPUT OF RESULTS AT ANY STEP
C
C THE X VARIABLE AND THE Y FONCTIONS ARE INITIALISED BY THE CALL
C THE INITIAL VALUES ARE DESTROYED
C U AND W ARE WORKING ARRAYS
C
      real Y(8),Z(8),U(4,8),W(5,8)
      real A(10),T(4),PRE(4),COR(5)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP
c
      COMMON/WORK/CFN(4),FP2(4),DFP2(4,3),BT,B(3),FH(4),
     ,            CFH(4),IEPS(4),EMU
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
c
      real ZPRC(8),YPRC(8),relsstep
      common /LastPoint/ YPRC
c
      integer l,i,j,j1
      integer*4 k
      character*100 fins
c
c --INTEGRATION ALGORITHM CONTROLS --
c -----step expansion/contraction criteria     -------------
c   integration method : Runge-Kutta 4th order with one midpoint
c   initial integration step:
      DATA XPp/0.01/
c   minimum internal step
      DATA PAMIN/1e-30/
c   minimum-step-length / wave-length coefficient (should be ~DROMA/pi)
      DATA step_limit/0.318e-3/
c      DATA step_limit/0.1e-3/
c   min and max relative change of wave vector for step expansion/compression
      DATA DROMI,DROMA/ 1.E-4,1.E-3/
c  step contraction and expansion coef
      data contr, expan/0.8,1.2/
c printout control 0-.. no, 1.. resonances, 2+..all
      data prtout/2/
c file output control : internal steps per output step
      data ioutstep/10/
c ---------------------------------------
C
      DATA NRK,MI,MIC,EPS/4,4,5,1.E-10/, T/.5,0.,.5,0./
     D  ,A/.5,-.5,.5,0.,-.5,1.,.16666667,.33333333,-.66666667,.16666667/
     A    ,PRE/-.375,1.54166667,-2.45833333,2.2916666667/
     T    ,COR/.348611111,-1.39444444,2.09166667,-1.39444444,.348611111/
     A   YPRC/0.,0.,0.,0.,0.,0.,0.,0./
c
      HH=XPp
      IFIN=0
      CALL DERIV(X,Y,Z,R1,Y2,IFIN)
      CALL SORTV(X,Y,Z,IFIN)
      K=0
C
   10 K=K+1
   11 continue
c-----os added : store previous step
      XPRC=X
      DO 141 L=1,NF
      YPRC(L)=Y(L)
      ZPRC(L)=Z(L)
  141 continue
c---------------------
      J1=0
      DO 80 I=1,NRK
      X=X+HH*T(I)
      DO 70 J=1,I
      J1=J1+1
      DO 70 L=1,NF
      U(I,L)=Z(L)
      Y(L)=Y(L)+HH*A(J1)*U(J,L)
   70 CONTINUE
      CALL DERIV(X,Y,Z,R1,Y2,IFIN)
  80  CONTINUE
c ------ refine step if resonance
      if (ifin.eq.2) then
        ifin=0
        if (prtout.gt.0) write (*,'(a,f7.3,a,e9.2)')
     .    'Resonance @ Tg=',Y(8),' -> contract to ', hh*contr
        goto 78
      endif
c  ----- check the step length
      DRO=SQRT( ((YPRC(4)-Y(4))**2+(YPRC(5)-Y(5))**2+
     +           (YPRC(6)-Y(6))**2)/ (Y(4)**2+Y(5)**2+Y(6)**2))
      IF(DRO.ge.DROMA) then
           if (prtout.gt.1) write (*,'(a,f7.3,a,e9.2,a,f10.3,a)')
     .     'Long step @ Tg=',Y(8),' -> contract to ',
     .     hh*contr,' (dk/k=',DRO*100.,'%)'
           goto 78
      endif
      IF (DRO.lt.DROMI) then
           if (prtout.gt.1) write (*,'(a,f7.3,a,e9.2,a,f10.3,a)')
     .     'Shrt step @ Tg=',Y(8),' ->   expand to ',
     .     hh*expan,' (dk/k=',DRO*100.,'%)'
           goto 75
      endif
c          ---> nothing happend => output
   7  if (mod(k,ioutstep).eq.0) then
        CALL SORTV (X,Y,Z,IFIN)
      endif
      if (mod(k,1000).eq.0) then
        write(*,*) '------> Step#',k
      endif
c
C.....STOP CRETERIA :
  100 continue
      if (((YPRC(2).lt.Y2).and.(Y(2).ge.Y2)).or.
     .    ((YPRC(2).gt.Y2).and.(Y(2).le.Y2))) then
        fins='STOP#1: Final latitude crossed.'
        write(*,*) fins
        ifin=1
        goto 185
      endif
      if (Y(1).le.R1) then
        fins='STOP#2: Radial distance is below its lower limit.'
        write(*,*) fins
        ifin=2
        goto 185
      endif
      if (Y(1).ge.13.) then
        fins='STOP#3: Radial distance is > 13 Earth radii.'
        write(*,*) fins
        ifin=3
        goto 185
      endif
      wavelength=299792458./EMU/F/1000.
      relsstep=(Y(7)-YPRC(7))*RT/wavelength
      if (relsstep.le.step_limit) then
        fins='STOP#4: Limit of the W.K.B. approximation encountered.'
        write(*,*) fins
        ifin=4
        goto 185
      endif
      goto 10
c      --> loop
c
c ----- Contraction ---------------
   78 CONTINUE
  107 X=XPRC
      if (HH.lt.pamin) then
        fins='STOP#5: Step decreased below its numerical limit.'
        write(*,*) fins
        ifin=5
        goto 185
      endif
      HH=HH*contr
      DO 83 L=1,NF
      Y(L)=YPRC(L)
   83 Z(L)=ZPRC(L)
      GO TO 11
c ----------------- EXPANSION______________
   75 HH=HH*expan
      GO TO 7
c
185   CALL FIN2(fins)
      RETURN
      END


      SUBROUTINE DERIV(X,Y,Z,R1,Y2,IFIN)
C Computation of derivatives for the functions r,phi,lon,ror,rophi,rolon,s,tg
C wich are necessary to solve the Haselgrove's equations.
C r is  the geocentric distance in RT units
C phi    the colatitude
C lon   the longitude
C ror,rophi,rolon the corresponding components of the array refractif index
C defined as the product of the array refractif index for the wave
C Notation:
C Z(1)...Z(8)=the derivatives
C Y(1)...Y(8)=the fonctions
C EMU=the refractif index
C DMURO(1)...(3)=derivatives of the refractif index on the 3 ro
C DMU(1)...(3)=derivatives of the refractif index on r,phi,lon
      real*8 KR,KL,KPA,KPE,KRKL,KPAE,KARD
      real*8 ROP(3),RO2(3),DRO(3),RO3(3), RO1(3)
      DIMENSION Y(8),Z(8),B(3),DB(3,3),DBT(3),DFH(4,3),DFP2(4,3)
      DIMENSION FH(4),CFH(4),RFR(4),RFL(4),DMUH(4),DMUP(4),
     DDKLFP(4),DKRFP(4),RFR2(4),RFL2(4),PINT(4),FP2(4),DKRFH(4),
     DDKLFH(4),DKRF(4),DKLF(4),DKPF(4),DSRLP(4),DPRLP(4),DAFP(4),
     DDBFP(4),DCFP(4),DSRLH(4),DPRLH(4),DAFH(4),DBFH(4),DCFH(4),
     DDCTRO(3),DMURO(3),DCT(3),DP(3),DMU(3)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP
      COMMON/WORK/CFN(4),FP2,DFP2,BT,B,FH,CFH,IEPS(4),EMU
      IF(IFIN.EQ.2)RETURN
      C=2.123E-2
      SI= SIN(Y(2))
      COFI=COS(Y(2))
      CALL BAD(Y,B,DB,BT)
      CALL DENSI(Y)
      KR=1
      KL=1
      KPA=1
      DKRDF=0
      DKLDF=0
      DKPDF=0
      DO 204 I=1,3
  204 DBT(I)= (B(1)*DB(1,I)+B(2)*DB(2,I)+B(3)*DB(3,I))/BT
      DO 301 J=1,4
      FH(J)=CFH(J)*BT
      DO 302 I=1,3
  302 DFH(J,I)=CFH(J)*DBT(I)
      RFR(J)=1/(F*(F+IEPS(J)*FH(J)))
      RFL(J)=1/(F*(F-IEPS(J)*FH(J)))
      DKLFP(J)=-RFL(J)
      DKRFP(J)=-RFR(J)
      RFR2(J)=RFR(J)**2
      RFL2(J)=RFL(J)**2
      PINT(J)=IEPS(J)*FP2(J)*F
      DKRFH(J)=PINT(J)*RFR2(J)
      DKLFH(J)=-PINT(J)*RFL2(J)
      DKRF(J)=FP2(J)*(2*F+IEPS(J)*FH(J))*RFR2(J)
      DKLF(J)=FP2(J)*(2*F-IEPS(J)*FH(J))*RFL2(J)
      DKPF(J)=2*FP2(J)/(F**3)
      KR=KR-FP2(J)*RFR(J)
      KL=KL-FP2(J)*RFL(J)
      KPA=KPA-FP2(J)*RFP
      DKLDF=DKLDF+DKLF(J)
      DKRDF=DKRDF+DKRF(J)
      DKPDF=DKPDF+DKPF(J)
  301 CONTINUE
      KPE=(KR+KL)/2
      KRKL=KR*KL
      KPAE=KPA*KPE
      ROM=SQRT( Y(4)**2+Y(5)**2+Y(6)**2)
      CTH=(Y(4)*B(1)+Y(5)*B(2)+Y(6)*B(3))/(BT*ROM)
      CTH2=CTH**2
      STH2=1-CTH2
      COA=KPE*STH2+KPA*CTH2
      COB=KRKL*STH2+KPAE*(1+CTH2)
      COC=KRKL*KPA
      COD=SQRT( (KPA*(KL-KR))**2*CTH2+((KRKL-KPAE)*STH2)**2)
      EMU2=(COB-COD)/(2*COA)
c      added:     _______________________________
      EMU2_=(COB+COD)/(2*COA)
        isgnmode=-1
        if ((EMU2.lt.0.).or.((EMU2_.gt.0.).and.(EMU2_.lt. EMU2))) then
          EMU2 = EMU2_
          isgnmode=1
        endif
c    ___________^FAST MODE SELECTION___________
      IF(EMU2.GE.0.)GO TO 80
      IFIN=2
      RETURN
80    EMU=SQRT( EMU2)
      EMU4=EMU2**2
      IF (ABS(EMU-ROM)-1.E-10)85,85,974
  974 IF (CTH2-0.9) 84,84,85
   85 DO 86 J=1,3
   86 Y(J+3)=Y(J+3)*EMU/ROM
      GO TO 54
C       CORRECTION OF THE ARRAY RO BY R.L. SMITH METHOD
C        **************************************************
   84 STH=SQRT ( STH2)
      DO 60 I=1,3
   60 RO1(I)=Y(I+3)*EMU/ROM
      ALF=1.0/STH
      BET=-ALF*CTH
      ROM2=Y(4)**2+Y(5)**2+Y(6)**2
      ROM4=ROM2**2
      ROM=SQRT ( ROM2)
      CTH22=(-KPE*ROM4+(KRKL+KPAE)*ROM2-COC)/((KPA-KPE)*ROM4-(KPAE-
     1 KRKL)*ROM2)
      IF (CTH22) 51,51,52
   52 CTH12=SQRT ( CTH22)
      CTH12=SIGN(CTH12,CTH)
      STH12=SQRT ( ABS(1-CTH22))
      TH2=ATAN(STH12/CTH12)
      TH0=ATAN(STH/CTH)
      DTH=TH0-TH2
      IF (DTH-1.E-06) 85,721,721
  721 DO 61 I=1,3
      ROP(I)=ALF*B(I)/BT+BET*Y(I+3)/ROM
   61 RO2(I)=Y(I+3)+ROM*DTH*ROP(I)
      DRO21=0.
      DO 62 I=1,3
      DRO(I)=RO1(I)-RO2(I)
   62 DRO21=DRO21+DRO(I)**2
      ALAM=1./(1.+(((EMU-ROM)/( ROM*DTH))**2))
      DO 63 I=1,3
   63 RO3(I)=RO2(I)+ALAM*DRO(I)
      DO 55 J=1,3
   55 Y(J+3)=RO3(J)
      EMU2=(RO3(1)**2+RO3(2)**2+RO3(3)**2)
      EMU=SQRT ( EMU2)
      EMU4=EMU2**2
      CTH2=abs(
     1 (-KPE*EMU4+ (KRKL+KPAE)*EMU2-COC)/((KPA-KPE)*EMU4-(
     1 KPAE-KRKL)*EMU2)
     1  )
      CTH=SIGN(SQRT ( CTH2),CTH)
      STH2=1.- CTH2
c   ___    abs added, cod condition added
      if ((KPA*(KL-KR))**2*CTH2+((KRKL-KPAE)*STH2)**2.gt.0.) then
        COD=SQRT ( (KPA*(KL-KR))**2*CTH2+((KRKL-KPAE)*STH2)**2)
      else
        COD=0.
      endif
      GO TO 54
   51 DO 53 J=1,3
   53 Y(J+3)=RO1(J)
C        **************************************************
   54 KARD=1./(isgnmode*2.*COD*EMU)
c__________________^mode____________________________
c    modified from:
c   54 KARD=1./(-2.*COD*EMU)
      DADCT=2*CTH*(KPA-KPE)
      DBDCT=2*CTH*(KPAE-KRKL)
      DMUCT=(-DADCT*EMU4+DBDCT*EMU2)*KARD
      DO 215 J=1,3
      DCTRO(J)=(EMU*B(J)-BT*Y(J+3)*CTH)/(BT*EMU2)
  215 DMURO(J)=DMUCT*DCTRO(J)
      DO 205 J=1,3
  205 DCT(J)=0
      DO 206 J=1,3
      DP(J)=BT*Y(J+3)-EMU*CTH*B(J)
      DO 206 I=1,3
  206 DCT(I)=DCT(I)+(DP(J)*DB(J,I))/(EMU*(BT**2))
      DO 207 I=1,3
  207 DMU(I)=DCT(I)*DMUCT
      DSRLF=(DKRDF+DKLDF)/2
      DPRLF=KR*DKLDF+KL*DKRDF
      DADF=DSRLF*STH2+DKPDF*CTH2
      DBDF=DPRLF*STH2+(KPA*DSRLF+KPE*DKPDF)*(1+CTH2)
      DCDF=KPA*DPRLF+KRKL*DKPDF
      DMUDF=-(DADF*EMU4-DBDF*EMU2+DCDF)*KARD
      DO 401 J=1,4
      DSRLP(J)=(DKRFP(J)+DKLFP(J))/2
      DPRLP(J)=KR*DKLFP(J)+KL*DKRFP(J)
      DAFP(J)=DSRLP(J)*STH2-RFP*CTH2
      DBFP(J)=DPRLP(J)*STH2+(DSRLP(J)*KPA-KPE*RFP)*(1+CTH2)
      DCFP(J)=-KRKL*RFP+KPA*DPRLP(J)
      DSRLH(J)=(DKRFH(J)+DKLFH(J))/2
      DPRLH(J)=KR*DKLFH(J)+KL*DKRFH(J)
      DAFH(J)=DSRLH(J)*STH2
      DBFH(J)=DPRLH(J)*STH2+KPA*DSRLH(J)*(1+CTH2)
      DCFH(J)=KPA*DPRLH(J)
      DMUH(J)=-(EMU4*DAFH(J)-EMU2*DBFH(J)+DCFH(J))*KARD
      DMUP(J)=-(EMU4*DAFP(J)-EMU2*DBFP(J)+DCFP(J))*KARD
      DO 208 I=1,3
  208 DMU(I)=DMU(I)+DMUP(J)*DFP2(J,I)+DMUH(J)*DFH(J,I)
  401 CONTINUE
      DTGDT=(1+F*DMUDF/EMU)*C
      Z(1)=(Y(4)/EMU-DMURO(1))/EMU
      Z(2)=(Y(5)/EMU-DMURO(2))/(EMU*Y(1))
      Z(3)=(Y(6)/EMU-DMURO(3))/(EMU*Y(1)*SI)
      Z(4)=DMU(1)/EMU+Y(5)*Z(2)+SI*Y(6)*Z(3)
      Z(5)=(DMU(2)/EMU-Y(5)*Z(1)+Y(1)*COFI*Y(6)*Z(3))/Y(1)
      Z(6)=(DMU(3)/EMU-SI*Y(6)*Z(1)-Y(1)*COFI*Y(6)*Z(2))/(Y(1)*SI)
      Z(7)=SQRT(Z(1)*Z(1)+Y(1)*Y(1)*Z(2)*Z(2)+Y(1)*Y(1)*SI*SI*Z(3)*Z(3))
      Z(8)=DTGDT
        RETURN
      END




      SUBROUTINE SORTV(X,Y,Z,IFIN)
      REAL LOND
      dimension B(3),EXNOR(4),QI(4),SH(4)
      DIMENSION Y(8),Z(8),ALPHA(4),FP2(4),DFP2(4,3)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP,KBTYP
      COMMON/WORK/CFN(4),FP2,DFP2,BT,B,FH(4),CFH(4),IEPS(4),EMU
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
      COMMON/III/FIP,ANH10,ANH20,ANH30,ALAT0,ALAT1,ALON0,ALT0,
     ,           ALT1,THET0,PHI0
c
c     ____ indentification of the density region
c        0.. Diffusive equilibrium model
c       -1.. Plasmapause gradient (sine localized between AL-DDK and AL+DDK)
c       -2.. Duct (exp model - 0.1% density change)
c       -3.. ionosphere below 390 km (.99*PP  decrease, alt0=89km, scale=140km)
      integer ident_dens_region
      common/ident_dens_region/ ident_dens_region
c
      real YPRC(8),relsstep
      common /LastPoint/ YPRC
c
      DATA  AM/1836./
      XX=Y(1)*COS(Y(2))
      YY=Y(1)*SIN(Y(2))
      FID=90.-Y(2)/CONVE
      LOND=Y(3)/CONVE
      FKE=Y(6)/EMU
      FKN=-Y(5)/EMU
      FKV=Y(4)/EMU
c      CALL ENXY(B(3),-B(2),B(1),FKE,FKN,FKV,FKX,FKY,FKZ,ALP,BET,THD,PHI)
      CALL ENXY(B(3),-B(2),B(1),FKE,FKN,FKV,FKX,FKY,FKZ,ALP,BET,THD,PHI)
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
      IF(THD.GT.90.)THG=180.-THG
      IF(THD.GT.90.)THR=180.-THR
      ELE= Y(1) /(SIN(Y(2)))**2
      DENS=FP2(4)/(1.E+6*CFN(4))
      pH=FP2(1)/(1.E+6*CFN(1))/Dens*100.
      pHe=FP2(2)/(1.E+6*CFN(2))/Dens*100.
      pO=FP2(3)/(1.E+6*CFN(3))/Dens*100.
      RB7370=RBASE/7370.
      SH(2)=1.1506*THERM*RB7370*RB7370
      SH(3)=SH(2)/4.
      SH(4)=SH(3)/4.
      GPH=RBASE*(1.-RBASE/(Y(1)*RT))
      EXNOR(2)=EXP(-GPH/SH(2))
      EXNOR(3)=EXNOR(2)*EXNOR(2)*EXNOR(2)*EXNOR(2)
      EXNOR(4)=EXNOR(3)*EXNOR(3)*EXNOR(3)*EXNOR(3)
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
c
      WRITE(LUT,605) ident_dens_region,
     .  Y(7),Y(8),reltstep,relsstep,wavelength,
     .  Y(1),ALT,FID,LOND,ELE,FGF,FGF*1836.,
     .  DENS,pH,pHe,pO,FLHR,EMU, akvert,akazi,THD,PSI,PHI,THG,THR
605   FORMAT(I2,5G10.3,F9.5,F8.0,2F9.4,G12.4,2f16.5,F16.2,3F6.1,
     &F8.3,G10.3,7F6.1)
c      ___________________________________________________________________
c
130   RETURN
      END


      SUBROUTINE FIN2(fins)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP,KBTYP
      character*100 fins
       write(lut,'(a)') fins
      ENDFILE LUT
      RETURN
      END


      SUBROUTINE INIT (X,Y,X1,R1,Y2)
      REAL KR,KL,KPA,KPE,KRKL,KPAE,LAT0,LAT1,LON0
      REAL NH10,NH20,NH30,  NHX0,NHY0,NHZ0
      DIMENSION Y(8),DB(3,3),A(3,3)
      DIMENSION     RFR(4),RFL(4),RFR2(4),RFL2(4),PINT(4)
      COMMON LU,LUT,IECR,IPROG,CONVE,HH ,F,RFP,KBTYP
      COMMON/WORK/CFN(4),FP2(4),DFP2(4,3),
     ,            BT,B(3),FH(4),CFH(4),IEPS(4),EMU
      COMMON/DDD/THERM,RBASE,RZERO,SCBOT,RT,ALP0(3),AN0
      COMMON/DUCT/KDUCT,DEF,DD,AL0,EP,DDK,ALK
      COMMON/III/FIP,NH10,NH20,NH30,LAT0,LAT1,LON0,
     .           ALT0,ALT1,THET0,PHI0
      CONVE=0.0174532925
      X1=1000.
      R1=1.+ALT1/RT
      RFP=1/(F**2)
      Y(1)= 1.+ALT0/RT
      Y(2)=(90.-LAT0)*CONVE
      FI0=90.-LAT0
      Y(3)= LON0*CONVE
      Y2=(90.-LAT1)*CONVE
      CALL BAD (Y,B,DB,BT)
      BV=B(1)/BT
      BN=-B(2)/BT
      BE=B(3)/BT
      NHX0=SIN(THET0*CONVE)*COS((PHI0+180.)*CONVE)
      NHY0=SIN(THET0*CONVE)*SIN((PHI0+180.)*CONVE)
      NHZ0=COS(THET0*CONVE)
      ALP=ATAN2(BN,BE)
      BET=ACOS(BV)
      A(1,1)=COS(BET)*COS(ALP)
      A(1,2)=COS(BET)*SIN(ALP)
      A(1,3)=-SIN(BET)
      A(2,1)=-SIN(ALP)
      A(2,2)=COS(ALP)
      A(2,3)=0.
      A(3,1)=SIN(BET)*COS(ALP)
      A(3,2)=SIN(BET)*SIN(ALP)
      A(3,3)=COS(BET)
      NH10=A(1,1)*NHX0+A(2,1)*NHY0+A(3,1)*NHZ0
      NH20=A(1,2)*NHX0+A(2,2)*NHY0+A(3,2)*NHZ0
      NH30=A(1,3)*NHX0+A(2,3)*NHY0+A(3,3)*NHZ0
      CALL ENXY(BE,BN,BV,NH10,NH20,NH30,FKX,FKY,FKZ,ALP,BET,THET1,PHI1)
      CALL ANG(1.,0.,0.,NH10,NH20,NH30,A10)
      CALL ANG(0.,1.,0.,NH10,NH20,NH30,A20)
      CALL ANG(0.,0.,1.,NH10,NH20,NH30,A30)
      CALL ANG(1.,0.,0.,BE,BN,BV,B30)
      CALL ANG(0.,1.,0.,BE,BN,BV,B20)
      CALL ANG(0.,0.,1.,BE,BN,BV,B10)
      BNN=-B(2)
      CALL DENSI (Y)
      CTH=(NH30*B(1)-NH20*B(2)+NH10*B(3))/BT
      CTH2=CTH**2
      STH2=1-CTH2
      KR=1
      KL=1
      KPA=1
      DO 70 J=1,4
      FH(J)=CFH(J)*BT
      RFR(J)=1/(F*(F+IEPS(J)*FH(J)))
      RFL(J)=1/(F*(F-IEPS(J)*FH(J)))
      RFR2(J)=RFR(J)**2
      RFL2(J)=RFL(J)**2
      PINT(J)=IEPS(J)*FP2(J)*F
      KR=KR-FP2(J)*RFR(J)
      KL=KL-FP2(J)*RFL(J)
      KPA=KPA-FP2(J)*RFP
  70  CONTINUE
      KPE=(KR+KL)/2
      KRKL=KR*KL
      KPAE=KPA*KPE
      COA=KPE*STH2+KPA*CTH2
      COB=KRKL*STH2+KPAE*(1+CTH2)
      COC=KRKL*KPA
      COD=SQRT ((KPA*(KL-KR))**2*CTH2+((KRKL-KPAE)*STH2)**2)
      EMU2=(COB-COD)/(2*COA)
      if (emu2.ge.0.) then
        EMU=SQRT( EMU2)
      else
        EMU=1.
      endif
       Y(4)=NH30*EMU
       Y(5)=-NH20*EMU
      Y(6)=NH10*EMU
      X=0
      RETURN
      END


c TRACE (ray tracing) input values
c ___________________________________________________:______________
c          MAGNETIC-FIELD multiplier (dipole model):   1.00000
c DENS MODEL (0..DIF EQUIL 1..+PLASMAPAUSE 2...+DUCT):             1
c                PLASMAPAUSE relative density change:  0.600000E-01
c                         PLASMAPAUSE  L-HALF WIDTH:  0.500000E-01
c                    PLASMAPAUSE L-CENTRAL POSITION:   4.80000
c                 DUCT relative density enhancement: -0.600000
c                                 DUCT L-HALF WIDTH:  0.500000
c                           DUCT L-CENTRAL POSITION:   6.65000
c                  ALTITUDE of REFERENCE LEVEL (km):   22195.0
c            DENSITY AT the REFERENCE LEVEL (cm^-3):   120.000
c                                  nH+/ne at 1000km:   1.00000
c                                 nHe+/ne at 1000km:        0.
c                                  n0+/ne at 1000km:        0.
c                         temperature at 1000km (K):   1000.00
c                            Initial latitude (deg):        0.
c                              Final latitude (deg):   80.0000
c                           Initial longitude (deg):        0.
c                             Initial altitude (km):   24000.0
c                               Final altitude (km):   300.000
c                                    Frequency (Hz):   1800.00
c                      INITIAL VALUE OF THETA (deg):       30.
c                        INITIAL VALUE OF Phi (deg):      180.