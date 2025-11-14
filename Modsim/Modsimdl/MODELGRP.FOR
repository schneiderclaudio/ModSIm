      PROGRAM ModelGraphs
C     *******************
C Plots any special graphs for specific models.
      USE MSFLIB
      INTEGER*4 NoArgs
      INTEGER*2 Arg,status
      CHARACTER*256 JobPath
      CHARACTER*256 PathBuffer
      CHARACTER*10 BUFFER
      CHARACTER*4 MODEL,KEY
      INTEGER*4 result

c Get the job path name, unit number, model and index from the command line argument
      NoArgs = NARGS()
      IF(NoArgs .GT. 4) THEN
        Arg = 1
        CALL GETARG(Arg,JobPath,status)
        DO I = 2,NoArgs-4
          !Blank spaces occurred in one or more directory names in the path
          Arg = INT2(I)
          CALL GETARG(Arg,PathBuffer,status)
          LENPATH = LEN_TRIM(Pathbuffer)
          LENG = LEN_TRIM(JobPath)
          JobPath = JobPath(1:LENG)//' '//PathBuffer(1:LENPATH)
        END DO
        LENPATH = LEN_TRIM(JobPath)
        Arg = Arg + 1
        CALL GETARG(Arg,BUFFER,status)
        READ(BUFFER,*) NUNIT
        Arg = Arg + 1
        CALL GETARG(Arg,BUFFER,status)
        MODEL = BUFFER(1:4)
        Arg = NoArgs-1
        CALL GETARG(Arg,BUFFER,status)
        READ(BUFFER,*) Index
      ELSE
        result = MESSAGEBOXQQ('Too few arguments in call to MODELGRP'C,
     *           'ERROR'C,MB$OK)
        CALL EXIT(1)
      END IF

C Get the data for the graph from a file.
      OPEN(22,FILE = JobPath(1:LENPATH)//'ModelGrp.OUT')

C Search for the data for this model
      KEY = ''
      DO WHILE (KEY .NE. MODEL)
        READ(22,'(A4)',END = 9998)KEY
        IF(KEY .EQ. MODEL)THEN
          READ(22,*) ID,Ind
          IF(ID .NE. NUNIT .OR. Ind .NE. Index) KEY = ''
        END IF
      END DO

      SELECT CASE (MODEL)

        CASE ('MILL','GMIL','GMI1','GMSU','RODL','RODM','HFMI','HFML',
     *        'HFSU','SAGM','FAGM')
            IF(Index .EQ. 0) CALL SelFuncGraph(JobPath)
            IF(Index .EQ. 2) CALL AMDGRP(JobPath)

        CASE ('KYNC')
            CALL THICKEN_CONSTRUCT(JobPath)

        CASE ('CONE')
            CALL StratificationProfiles(JobPath)

        CASE ('SJIG')
            CALL JigStratProfiles(JobPath)

      END SELECT
      CLOSE (22)

      CALL EXIT(-1)!Normal termination
 9999 CONTINUE
      CALL EXIT(1) !Too few arguments in the call to the program.
 9998 CONTINUE
      result = MESSAGEBOXQQ('Unit was not located in MODELGRP.OUT'C,
     *           'ERROR'C,MB$OK)
      CALL EXIT(2) !Model was not found.

      END

      SUBROUTINE SelFuncGraph(JobPath)
C     ********************************
C Plots the selection function for a mill.
      USE MSFLIB
      CHARACTER*256 JobPath
      REAL SIZE(100),SelFunc(100)
      CHARACTER*30 TITLE
      CHARACTER*6 Model
      REAL ALN
      EXTERNAL ALN

      READ(22,'(A30)',END = 9999) TITLE
      LABLEN = LEN_TRIM(TITLE)
      READ(22,'(A6)',END = 9999)Model
      READ(22,*,END = 9999) NDC
      DO I = 1,NDC
        READ(22,*,END = 9999)SIZE(I),SelFunc(I)
      END DO
      SIZE = SIZE*1000
      XMIN = MINVAL(SelFunc,MASK = SelFunc .GT. 0.0)
      XMAX = MAXVAL(SelFunc)
      CALL WRTAB(JobPath)
      CALL LBSIZE(14,23)
      CALL MGMOVE(300,730)
      CALL LABELC(TITLE,LABLEN)
      CALL BBOX(150,100,800,600)
      CALL LOGLIM(SIZE(1),SIZE(NDC),XO,XF,NDEC)
      CALL LGAXIS(XO,XF,1,'Y','Y')
      CALL LABAX('Particle size',13,'mm',2,13,1)
      CALL LOGLIM(XMIN,XMAX,XO,XF,NDEC)
      CALL LGAXIS(XO,XF,2,'Y','Y')
      IF(Model .EQ. 'Austin') THEN
        CALL LABAX('Selection function',18,'min~-1~',7,13,2)
      ELSE IF (Model .EQ. 'Herbst') THEN
        CALL LABAX('Selection function',18,'tonnes/kWhr',11,13,2)
      END IF
      CALL PLGRPH(SIZE,SelFunc,NDC,ALN,ALN)

      CALL GROUTCOORD
      CALL HDCOPY(1.0)
      CALL CLOSE
      RETURN

 9999 CALL EXIT(3) !Not enough data

      END


      SUBROUTINE THICKEN_CONSTRUCT(JobPath)
C     *************************************
C Draws the flux curve and operating line for a Kynch thickener.
      USE MSFLIB
      CHARACTER*256 JobPath
      REAL ALPHA(10),BETA(10)
      REAL C(0:100),PSI(0:100)
      CHARACTER*36 TITLE
      LOGICAL OvLoad
      REAL LINTR
      EXTERNAL LINTR


C Get the data for the construction from a file.
      READ(22,'(A36)',END = 9999) TITLE
      LABLEN = LEN_TRIM(TITLE)
      READ(22,'(L1)',END = 9999) OvLoad
      READ(22,'(G10.4,I5)',END = 9999) Vt,NTERMS
      DO I = 1, NTERMS
        READ(22,'(2G10.4)',END = 9999) ALPHA(I),BETA(I)
      END DO
      READ(22,'(6G10.4)',END = 9999) PsiF,CI,PsiMax,Cm,Cd,CO
      CN = ULIM(Cd)
      YN = ULIM(PsiMax)
      DELC = CN/100.0
      DO I = 0,100
        C(I) = I*DELC
        PSI(I) = FLUX(C(I),ALPHA,BETA,NTERMS,Vt)
      END DO

      CALL WRTAB(JobPath)
      CALL LBSIZE(14,23)
      CALL MGMOVE(300,730)
      CALL LABELC(TITLE,LABLEN)
      CALL BBOX(150,100,800,600)
      CALL LINAX(0.0,CN,10,5.0,1,'Y','Y')
      CALL LINAX(0.0,YN,10,5.2,2,'Y','Y')
      CALL LABAX('Slurry concentration',20,'kg/m~3~',7,13,1)
      CALL LABAX('Settling flux',13,'kg/m~2~ s',9,13,2)
      CALL PLSMGR(C,PSI,101,1,LINTR,LINTR)
      C(0) = 0.0
      PSI(0) = PsiF
      C(1) = Cd
      PSI(1) = 0.0
      CALL PLGRPH(C,PSI,2,LINTR,LINTR)

c Plot the upper zone operating line if the thickener is overloaded.
      IF(OvLoad) Then
        C(0) = 0.0
        PSI(0) = 0.0
        C(1) = CO
        PSI(1) = Flux(CO,ALPHA,BETA,NTERMS,Vt)
        CALL PLGRPH(C,PSI,2,LINTR,LINTR)
        CALL MGMOVE(500,600)
        CALL LBSIZE(13,23)
        CALL LABELC('The thickener is overloaded',27)
      END IF

      CALL GROUTCOORD
      CALL HDCOPY(1.0)
      CALL CLOSE
      RETURN

 9999 CALL EXIT(3) !Not enough data
      END

      FUNCTION Flux(C,ALPHA,BETA,NTERMS,Vt)
C     *************************************
C Calculates the flux.
      REAL ALPHA(*),BETA(*)
      WS1 = 1.0
      DO I = 1,NTERMS
        WS1 = WS1 + ALPHA(I)*C**BETA(I)
      END DO
      Flux = Vt*C/WS1
      RETURN
      END

      SUBROUTINE StratificationProfiles(JobPath)
c     ******************************************
c Plots the equilibrium stratification profiles
      USE MSFLIB
      CHARACTER*256 JobPath
      CHARACTER*36 TITLE1
      CHARACTER*80 TITLE2,TITLE3
      CHARACTER*18 FormatText
      CHARACTER*10   IDText
      INTEGER Stage
      INTEGER*4 result
      REAL V(0:100),RHO(0:100)
      REAL, ALLOCATABLE :: C(:,:)
      REAL, ALLOCATABLE :: TOT(:)
      REAL LINTR
      EXTERNAL LINTR

      CALL WRTAB(JobPath)

      READ(22,'(A36,I5)')TITLE1,NGC
      READ(22,'(A80)')TITLE2
      LEN2 = LEN_TRIM(TITLE2)
      WRITE(FormatText,'(''(F6.3,''I2,''F8.4,F8.1)'')')NGC
      ALLOCATE (C(0:100,NGC),TOT(NGC))

C Get the number of stages
      NSTAGES = 0
      IF(TITLE2(28:29) .EQ. 'SV') NSTAGES = 1
      IF(TITLE2(28:30) .EQ. 'DSV') NSTAGES = 2
      IF(TITLE2(28:31) .EQ. '2DSV') NSTAGES = 4
      IF(TITLE2(28:31) .EQ. '3DSV') NSTAGES = 6
      IF(TITLE2(28:31) .EQ. '4DSV') NSTAGES = 8
      IF(TITLE2(28:32) .EQ. 'DSVSV') NSTAGES = 3
      IF(TITLE2(28:33) .EQ. '2DSVSV') NSTAGES = 6
      IF(TITLE2(28:33) .EQ. '3DSVSV') NSTAGES = 9
      IF(TITLE2(28:37) .EQ. '2DSVSV.DSV') NSTAGES = 8
      IF(NSTAGES .EQ. 0) THEN
        result = MESSAGEBOXQQ('Could not identify cone type'C,
     *           'ERROR'C,MB$ICONEXCLAMATION.OR.MB$OK)
        RETURN
      END IF

      DO Stage = 1,NSTAGES

        READ(22,'(A80)')TITLE3
        LEN3 = LEN_TRIM(TITLE3)
c Read the concentration profile
        DO N = 0,100
          READ(22,FormatText)V(N),(C(N,J),J = 1,NGC),RHO(N)
C          V(N) = 1.0 - V(N)
        end do
        CALL LNTYPE(1)
        CALL COLOUR(1)
        CALL LBSIZE(14,23)
        CALL MGMOVE(300,730)
        CALL LABELC(TITLE1,36)
        CALL LBSIZE(12,23)
        CALL MGMOVE(50,700)
        CALL LABELC(TITLE2,LEN2)
        CALL MGMOVE(50,670)
        CALL LABELC(TITLE3,LEN3)
        CALL BBOX(100,70,300,400)
        CALL LINAX(0.0,1.0,10,4.2,1,'Y','Y')
        CALL LINAX(0.0,1.0,10,4.2,2,'Y','Y')
        CALL LABAX('Concentration',13,' ',1,13,1)
        CALL LABAX('Vertical position in the bed',28,' ',1,13,2)
        ICOL = 8
        CALL COLOUR(ICOL)
        CALL LBSIZE(9,18)
        DO J = 1,NGC
          CALL LNTYPE(J)
          CALL PLGRPH(C(0,J),V(0),101,LINTR,LINTR)
          WRITE(IDText,'(''G-Class '',I2)')J
          IX = 50 + MOD(160*(J-1),800)
          IY = 560 + ((J-1)/5)*20
          CALL LINIDC(IX,IY,70,IDText,10)
          CALL CHANGECOL(ICOL)
        END DO
        CALL LNTYPE(1)

c  Plot the recovery profiles
        DO J = 1,NGC
          TOT(J) = 0.0
          DO N = 1,100
            TOT(J) = TOT(J) + C(N,J)
          END DO
        END DO
        DO J = 1,NGC
          C(0,J) = 0.0
          DO N = 1,100
            C(N,J) = C(N-1,J) + C(N,J)
          END DO
          DO N = 1,100
            C(N,J) = C(N,J)/TOT(J)
          END DO
        END DO
        CALL LNTYPE(1)
        CALL COLOUR(1)
        CALL BBOX(550,70,300,400)
        CALL LINAX(0.0,1.0,10,4.2,1,'Y','Y')
        CALL LINAX(0.0,1.0,10,4.2,2,'Y','Y')
        CALL LABAX('Cumulative recovery',19,' ',1,13,1)
        CALL LABAX('Vertical position in the bed',28,' ',1,13,2)
        ICOL = 8
        CALL COLOUR(ICOL)
        CALL LBSIZE(9,18)
        DO J = 1,NGC
          CALL LNTYPE(J)
          CALL PLGRPH(C(0,J),V(0),101,LINTR,LINTR)
          CALL CHANGECOL(ICOL)
        END DO

C Now plot the bed density
        WS = MAXVAL(RHO)
        DO UPPER = 3000,20000,1000
          IF(WS .LE. UPPER) EXIT
        END DO
        WS = MINVAL(RHO)
        DO XLOWER = UPPER,0.0,-1000
          IF(WS .GE. XLOWER) EXIT
        END DO
        CALL LINAX(XLOWER,UPPER,10,5.0,3,'N','N')
        CALL LABAX('Bed density',11,'kg/m~3~ ',7,13,3)
        CALL LNTYPE(NGC + 1)
        CALL PLGRPH(RHO(0),V(0),101,LINTR,LINTR)
        CALL LBSIZE(9,18)
        CALL LINIDC(700,545,60,'Bed density',11)

        CALL GROUT
        CALL HDCOPY(1.0)
        CALL CHPAGE
      END DO
      CALL CLOSE

      DEALLOCATE (C,TOT)

      RETURN
      END

      SUBROUTINE JigStratprofiles(JobPath)
c     ************************************
c Plots the equilibrium stratification profiles
      USE MSFLIB
      CHARACTER*256 JobPath
      CHARACTER*36 TITLE1
      CHARACTER*18 FormatText
      CHARACTER*10   IDText
      INTEGER Stage
      REAL V(0:100),RHO(0:100)
      REAL, ALLOCATABLE :: C(:,:)
      REAL, ALLOCATABLE :: TOT(:)
      REAL LINTR
      EXTERNAL LINTR

      CALL WRTAB(JobPath)
      READ(22,'(A36,I5)')TITLE1,NGC

      WRITE(FormatText,'(''(F6.3,''I2,''F8.4,F8.1)'')')NGC
      ALLOCATE (C(0:100,NGC),TOT(NGC))

C Get the number of stages
      NSTAGES = 1

      DO Stage = 1,NSTAGES

c Read the concentration profile
        DO N = 0,100
          READ(22,FormatText)V(N),(C(N,J),J = 1,NGC),RHO(N)
C          V(N) = 1.0 - V(N)
        end do
        CALL LNTYPE(1)
        CALL COLOUR(1)
        CALL LBSIZE(14,23)
        CALL MGMOVE(300,730)
        CALL LABELC(TITLE1,36)
        CALL BBOX(100,70,300,400)
        CALL LINAX(0.0,1.0,10,4.2,1,'Y','Y')
        CALL LINAX(0.0,1.0,10,4.2,2,'Y','Y')
        CALL LABAX('Concentration',13,' ',1,13,1)
        CALL LABAX('Vertical position in the bed',28,' ',1,13,2)
        ICOL = 8
        CALL COLOUR(ICOL)
        CALL LBSIZE(9,18)
        DO J = 1,NGC
          CALL LNTYPE(J)
          CALL PLGRPH(C(0,J),V(0),101,LINTR,LINTR)
          WRITE(IDText,'(''G-Class '',I2)')J
          IX = 50 + MOD(160*(J-1),800)
          IY = 560 + ((J-1)/5)*20
          CALL LINIDC(IX,IY,70,IDText,10)
          CALL CHANGECOL(ICOL)
        END DO
        CALL LNTYPE(1)

c  Plot the recovery profiles
        DO J = 1,NGC
          TOT(J) = 0.0
          DO N = 1,100
            TOT(J) = TOT(J) + C(N,J)
          END DO
        END DO
        DO J = 1,NGC
          C(0,J) = 0.0
          DO N = 1,100
            C(N,J) = C(N-1,J) + C(N,J)
          END DO
          DO N = 1,100
            C(N,J) = C(N,J)/TOT(J)
          END DO
        END DO
        CALL LNTYPE(1)
        CALL COLOUR(1)
        CALL BBOX(550,70,300,400)
        CALL LINAX(0.0,1.0,10,4.2,1,'Y','Y')
        CALL LINAX(0.0,1.0,10,4.2,2,'Y','Y')
        CALL LABAX('Cumulative recovery',19,' ',1,13,1)
        CALL LABAX('Vertical position in the bed',28,' ',1,13,2)
        ICOL = 8
        CALL COLOUR(ICOL)
        CALL LBSIZE(9,18)
        DO J = 1,NGC
          CALL LNTYPE(J)
          CALL PLGRPH(C(0,J),V(0),101,LINTR,LINTR)
          CALL CHANGECOL(ICOL)
        END DO

C Now plot the bed density
        WS = MAXVAL(RHO)
        DO UPPER = 3000,20000,1000
          IF(WS .LE. UPPER) EXIT
        END DO
        WS = MINVAL(RHO)
        DO XLOWER = UPPER,0.0,-1000
          IF(WS .GE. XLOWER) EXIT
        END DO
        CALL LINAX(XLOWER,UPPER,10,5.0,3,'N','N')
        CALL LABAX('Bed density',11,'kg/m~3~ ',7,13,3)
        CALL LNTYPE(NGC + 1)
        CALL PLGRPH(RHO(0),V(0),101,LINTR,LINTR)
        CALL LBSIZE(9,18)
        CALL LINIDC(700,545,60,'Bed density',11)

        CALL GROUT
        CALL HDCOPY(1.0)
        CALL CHPAGE
      END DO
      CALL CLOSE

      DEALLOCATE (C,TOT)

      RETURN
      END

      SUBROUTINE  AMDGRP(JobPath)
C     ***************************
c Plots the current AMD
      USE MSFLIB
      CHARACTER*256 JobPath
      CHARACTER*4 MODEL
      REAL SIZE(25)
      CHARACTER*33 TITLE
      INTEGER*2 Bvg(12,25,12,25)

      LENPATH = LEN_TRIM(JobPath)
      CALL WRTAB(JobPath)
C Get the data for the graph from a file.
      CALL LBSIZE(9,18)
      READ(22,'(A33)',END = 9990) TITLE
      CALL MGMOVE(100,700)
      CALL LABELC(TITLE,33)
      READ(22,*,END = 9990)NDC,NGC
      DO I = 1,NDC
        READ(22,*,END = 9990)SIZE(I)
      END DO
      READ(22,'(A4)',END = 9990)MODEL
      CALL MGMOVE(100,670)
      CALL LABELC(MODEL//' model was used',19)
      READ(22,*,END = 9990)NDParent,NGparent
      CLOSE(22)

C Open a file to receive the calculated A-M diagram
      CALL READAMD(JobPath,MODEL,Bvg,NDC,NGC)
C Plot the diagram
      CALL PLOTAMD(NDC,NGC,NDParent,NGParent,Bvg,SIZE)
      CALL CLOSE

      RETURN

 9990 CONTINUE
      CALL EXIT(1)
      RETURN
      END


      SUBROUTINE READAMD(JobPath,MODEL,Bvg,NDC,NGC)
c     *********************************************
C  Reads a precomputed Andrews-Mika Diagram.
      USE MSFLIB
      CHARACTER*256 JobPath
      CHARACTER*4 MODEL
      INTEGER*2 Bvg(12,25,12,25)
      INTEGER*4 result

      LENG = LEN_TRIM(JobPath)
      IF(MODEL .EQ. 'BETA') THEN
        OPEN(42, FILE = JobPath(1:LENG)//'BETAAMD.DAT',ERR = 9990)
      ELSE IF(MODEL .EQ. 'LJUB') THEN
        OPEN(42, FILE = JobPath(1:LENG)//'LJUBAMD.DAT',ERR = 9990)
      ELSE
        CALL EXIT(1)
      END IF

      DO  L = 1,NDC
        DO  K = 1,NGC
          DO  J = L+1,NDC
            READ(42,'(42I6)',ERR = 9990)(Bvg(I,J,K,L),I = 1,NGC)
          END DO
          READ(42,*)
        END DO
      END DO
      CLOSE(42)
      RETURN

 9990 CONTINUE
      result = MESSAGEBOXQQ(
     *'Error opening file containing Andrews-Mika diagram'C,'ERROR'C,
     *MB$OK)
      CALL EXIT(1)
      END


      SUBROUTINE PLOTAMD(NDC,NGC,NDParent,NGParent,Bvg,SIZE)
c     ******************************************************
c     Plots the Andrews-Mika diagram
      REAL SIZE(25)
      INTEGER*2 Bvg(12,25,12,25)
      REAL AMD(12,25)
      AMD = 0.0
      AMD(NGParent,NDParent) = 10000
      DO J = NDParent + 1,NDC
        DO I = 1,NGC
          AMD(I,J) = Bvg(I,J,NGParent,NDParent)
        END DO
      END DO

C Find the largest element of AMD and scale
      PMAX = MAXVAL(AMD)
      DO I = 1,NGC
        DO J = 1,NDC
          AMD(I,J) = AMD(I,J)/PMAX
        END DO
      END DO

C Plot the distributions
      CALL DISPLY(3)
      CALL THREE_D_BARCHART(AMD,NGC,NDC,SIZE)

c Annotate the graph
      CALL GROUT
      CALL HDCOPY(1.0)

      RETURN
      END

      SUBROUTINE THREE_D_BARCHART(P,NX,NY,SIZE)
C     *****************************************
C Plots a 3-D barchart.
      REAL  P(12,25),SIZE(25)
      CHARACTER*9 XLABEL(12)
      CHARACTER*7 YLABEL(25)
      COMMON/THREE_D_BAR_CHART/DISTANCE,THETA,PHI
      PARAMETER (PI = 3.1415927)
      DATA XLABEL/'    0%   ','  0 - 10 ',' 10 - 20 ',' 20 - 30 ',
     *' 30 - 40 ',' 40 - 50 ',' 50 - 60 ',' 60 - 70 ',' 70 - 80 ',
     *' 80 - 90 ',' 90 -100 ','   100%  '/

      DISTANCE = 5000.0
      THETA = 120
      PHI = 120
      THETA = THETA*PI/180.0
      PHI = PHI*PI/180.0
      XSPAN = 400
      YSPAN = 1.10*NY*XSPAN/NX
      XMARGIN = 70
      YMARGIN = 80
      XGAP = 0.02*XSPAN
      YGAP = 1.1*XGAP
      WIDTH = (XSPAN - 2*XMARGIN)/NX
      DEPTH = (YSPAN - 2*YMARGIN)/NY
      WIDTH = WIDTH - XGAP
      DEPTH = DEPTH - YGAP
      HEIGHT = 260
      DO I = 1,NY
        ISIZE = NINT(SIZE(I)*1E6)
        WRITE(YLABEL(I),'(I6,'' '')')ISIZE
      END DO
      XORG = -XSPAN/2
      YORG = -YSPAN/2
      ZORG = 50

      CALL VIEWPLANE(DISTANCE,THETA,PHI)
   10 CONTINUE
      CALL PSLNWD(0.2)
      CALL MGGRAYFILL(0.85)
      CALL PEDESTAL
     *(XORG,YORG,ZORG,NX,NY,WIDTH,DEPTH,100.0,XMARGIN,YMARGIN,
     *XGAP,YGAP)
      CALL LBSIZE(14,26)
      CALL MGBOTTOMWLAB(XORG+0.5*XSPAN-140,YORG,ZORG-60,
     *'Particle composition  %',23,1,THETA,PHI)
      CALL MGRIGHTWLAB(XORG+XSPAN,YORG+0.5*YSPAN-130,ZORG-60,
     *'Particle size  \2m\1m',21,1,THETA,PHI)
      CALL MGLEFTWLAB(XORG,YORG+0.5*YSPAN+130,ZORG-60,
     *'Particle size  \2m\1m',21,1,THETA,PHI)
      CALL MGGRAYFILL(0.95)
      CALL LBSIZE(9,16)
      DO I = 1,NX
        X = XORG + XMARGIN + 0.5*XGAP + (I-1)*(WIDTH + XGAP)
        DO J = 1,NY
          Y = YORG + YMARGIN + 0.5*YGAP + (NY-J)*(DEPTH + YGAP)
          IF(I .EQ. 1) THEN
            CALL MGFLOORLAB(XORG+5,Y-0.3*YGAP,ZORG,
     *      YLABEL(J),7,1,THETA,PHI)
          END IF
          IF(I .EQ. NX) THEN
            CALL MGFLOORLAB(XORG+XSPAN-XMARGIN+5,Y-0.3*YGAP,ZORG,
     *      YLABEL(J),7,1,THETA,PHI)
          END IF
          CALL THREED_BAR(X,Y,ZORG,WIDTH,DEPTH,P(I,J)*HEIGHT)
        END DO
        CALL MGFLOORLAB(X+0.9*XGAP,YORG+10,ZORG,XLABEL(I),9,2,
     *THETA,PHI)
      END DO
      RETURN
      END

      SUBROUTINE THREED_BAR(X,Y,Z,WIDTH,DEPTH,HEIGHT)
c     ***********************************************
C Sets the 3-D bar at world coordinate X Y Z
      PARAMETER (PI = 3.1415927)

      COMMON/THREE_D_BAR_CHART/DISTANCE,THETA,PHI


C Draw the visible faces.
      IF(THETA .GT. 0.5*PI .AND. THETA .LE. 1.5*PI) THEN
        CALL RIGHTFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
      ELSE
        CALL LEFTFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
      END IF
      IF(THETA .GT. 0.0 .AND. THETA .LE. 180.0) THEN
        CALL BOTTOMFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
      ELSE
        CALL TOPFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
      END IF
      CALL BARTOP(X,Y,Z,WIDTH,DEPTH,HEIGHT)
      RETURN
      END


      SUBROUTINE TOPFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
C     ********************************************
C Construct the top face of the bar.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)

      W = WIDTH
      D = DEPTH
      CALL VIEWC(X, Y+D, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+W, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X+W, Y+D, Z, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE BOTTOMFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
C     ***********************************************
C Construct the bottom face of the bar.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)

      W = WIDTH
      D = DEPTH
      CALL VIEWC(X, Y, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+W, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X+W, Y, Z, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE LEFTFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
C     **********************************************
C Construct the left hand face of the bar.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)

      W = WIDTH
      D = DEPTH
      CALL VIEWC(X, Y, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X, Y+D, Z, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE RIGHTFACE(X,Y,Z,WIDTH,DEPTH,HEIGHT)
C     **********************************************
C Construct the right hand face of the bar.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)

      W = WIDTH
      D = DEPTH
      CALL VIEWC(X+W, Y, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X+W, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+W, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X+W, Y+D, Z, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE BARTOP(X,Y,Z,WIDTH,DEPTH,HEIGHT)
C     *******************************************
C Construct the roof face of the bar.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)

      W = WIDTH
      D = DEPTH
      CALL VIEWC(X+W, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X+W, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X, Y+D, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X, Y, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE PEDESTAL(X,Y,Z,NX,NY,W,D,H,XMARGIN,YMARGIN,XGAP,YGAP)
C     ****************************************************************
C Draw the pedestal for the 3-D bar chart
      WIDTH = XMARGIN + NX*(W+XGAP) + XMARGIN
      DEPTH = YMARGIN + NY*(D+YGAP) + YMARGIN
      CALL THREED_BAR(X,Y,Z-H,WIDTH,DEPTH,H)
C Draw the tick marks
      CALL MG3DMOVE(X+XMARGIN,Y+YMARGIN,Z)
      CALL MG3DPLOT(X+WIDTH-XMARGIN,Y+YMARGIN,Z)
      CALL MG3DPLOT(X+WIDTH-XMARGIN,Y+DEPTH-YMARGIN,Z)
      CALL MG3DPLOT(X+XMARGIN,Y+DEPTH-YMARGIN,Z)
      CALL MG3DPLOT(X+XMARGIN,Y+YMARGIN,Z)
      DO I = 1,NX-1
        XT = X + XMARGIN + I*(W+XGAP)
        CALL X_TICK('+',XT,Y+YMARGIN,Z,0.5*YGAP)
        CALL X_TICK('-',XT,Y+DEPTH-YMARGIN,Z,0.5*YGAP)
      END DO
      DO I = 1,NY-1
        YT = Y + YMARGIN + I*(D+YGAP)
        CALL Y_TICK('+',X+XMARGIN,YT,Z,0.5*XGAP)
        CALL Y_TICK('-',X+WIDTH-XMARGIN,YT,Z,0.5*XGAP)
      END DO
      RETURN
      END

      SUBROUTINE X_TICK(ORIENT,X,Y,Z,LENGTH)
C     **************************************
C Draws a tick on the x-axis
      CHARACTER*1 ORIENT
      REAL LENGTH
      CALL MG3DMOVE(X,Y,Z)
      IF(ORIENT .EQ. '+') THEN
        CALL MG3DPLOT(X,Y+LENGTH,Z)
      ELSE
        CALL MG3DPLOT(X,Y-LENGTH,Z)
      END IF
      RETURN
      END

      SUBROUTINE Y_TICK(ORIENT,X,Y,Z,LENGTH)
C     **************************************
C Draws a tick on the y-axis
      CHARACTER*1 ORIENT
      REAL LENGTH
      CALL MG3DMOVE(X,Y,Z)
      IF(ORIENT .EQ. '+') THEN
        CALL MG3DPLOT(X+LENGTH,Y,Z)
      ELSE
        CALL MG3DPLOT(X-LENGTH,Y,Z)
      END IF
      RETURN
      END
