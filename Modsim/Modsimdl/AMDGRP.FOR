      PROGRAM  AMDGRP
C     ***************
c Plots the current AMD
      USE MSFLIB
      INTEGER*4 NoArgs
      INTEGER*2 Arg,status
      CHARACTER*256 JobPath
      CHARACTER*256 PathBuffer
      CHARACTER*4 MODEL
      REAL SIZE(25)
      INTEGER*2 Bvg(12,25,12,25)

c Get the job path name from the command line argument
      NoArgs = NARGS()
      IF(NoArgs .GT. 1) THEN
        Arg = 1
        CALL GETARG(Arg,JobPath,status)
        DO I = 3,NoArgs-2
          !Blank spaces occured in one or more directory names in the path
          Arg = INT2(I-1)
          CALL GETARG(Arg,PathBuffer,status)
          LENPATH = LEN_TRIM(Pathbuffer)
          LENG = LEN_TRIM(JobPath)
          JobPath = JobPath(1:LENG)//' '//PathBuffer(1:LENPATH)
        END DO
        LENPATH = LEN_TRIM(JobPath)
      ELSE
        CALL EXIT(1)
      END IF

      CALL WRTAB(JobPath)
C Get the data for the graph from a file.
      OPEN(22,FILE = JobPath(1:LENPATH)//'AMDGRP.OUT')
      READ(22,*,END = 9990)NDC,NGC
      DO I = 1,NDC
        READ(22,*,END = 9990)SIZE(I)
      END DO
      READ(22,'(A4)',END = 9990)MODEL
      READ(22,*,END = 9990)NDParent,NGparent
      CLOSE(22)

C Open a file to receive the calculated A-M diagram
      CALL READAMD(JobPath,MODEL,Bvg,NDC,NGC)

C Plot the diagram
      NDC = 19
      CALL PLOTAMD(NDC,NGC,NDParent,NGParent,Bvg,SIZE)
      CALL CLOSE

      CALL EXIT(0)

 9990 CONTINUE
      CALL EXIT(1)

      END


      SUBROUTINE READAMD(JobPath,MODEL,Bvg,NDC,NGC)
c     *********************************************
C  Reads a precomputed Andrews-Mika Diagram.
      CHARACTER*256 JobPath
      CHARACTER*4 MODEL
      INTEGER*2 Bvg(12,25,12,25)

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
      CALL EXIT(1)

      END


      SUBROUTINE PLOTAMD(NDC,NGC,NDParent,NGParent,Bvg,SIZE)
c     ******************************************************
c     Plots the Andrews-Mika diagram
      REAL SIZE(25)
      INTEGER*2 Bvg(12,25,12,25)
      CHARACTER*80 ANNOTATION
      REAL AMD(12,25)
      INTEGER*2 IAMD(12)
      AMD = 0.0
      AMD(NGParent,NDParent) = 10000
      DO J = NDParent + 1,NDC
        DO I = 1,NGC
          AMD(I,J) = Bvg(I,J,NGParent,NDParent)
          IAMD(I) =  Bvg(I,J,NGParent,NDParent)
        END DO
        WRITE(1,'(12I6)')(IAMD(I),I=1,NGC)
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
      CALL MGMOVE(50,730)
      WRITE(ANNOTATION,'(''Andrews-Mika Diagram '')')
      CALL LABELC(ANNOTATION,21)
C      CALL MGMOVE(50,710)
c      CALL LABELC(FILENAME,LENG)
c      WRITE(ANNOTATION,'(''Brittleness ratio = '',F5.2)')
c     *  BrittlenessRatio
c      call mgmove(50,690)
c      CALL LABELC(ANNOTATION,25)
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
      XSPAN = 500
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
      XORG = 0    !-XSPAN/2
      YORG = -YSPAN/2
      ZORG = 0

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
