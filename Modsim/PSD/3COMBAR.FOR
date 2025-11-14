      PROGRAM ThreeComponentBarChart
C     *******************************
C Program to display grade distribution for a three-phase system
C as a 3-D histogram.
C This version works with the VB interface
      USE MSFLIB

      PARAMETER (PI = 3.1415927)
      REAL R(43),S(43),P(43)
      CHARACTER*256 JobPath,Buffer
      INTEGER*4 NoArgs
      INTEGER*2 FirstArg,status
      INTEGER*4 response
      LOGICAL ChkHeading,ChkID
      CHARACTER*6 GRADS(7)
      CHARACTER*3 GRADS2(6)
      CHARACTER*6 Axlab(3)
      CHARACTER*80 HEADING,IDENTIFICATION
	INTEGER Scale
	CHARACTER*3 ScaleLAB
      INTEGER J(43)
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      DATA GRADS/'  0   ',' 0-20 ','20-40 ','40-60 ','60-80 ',
     *           '80-100',' 100  '/
      DATA GRADS2/' 0 ','20 ','40 ','60 ','80 ','100'/
      DATA AxLab/' Coal ',' Ash  ','Pyrite'/


C Parameters for the three-phase plots
      T30 = TAN(30.0*PI/180.0)
      C30 = COS(30.0*PI/180.0)
      DISTANCE = 500.0

c Get the job path name from the command line argument
      NoArgs = NARGS()
      FirstArg = 1
      IF(NoArgs .GT. 1) THEN
        CALL GETARG(FirstArg,JobPath,status)
        DO I = 3,NoArgs
          !Blank spaces occured in one or more directory names in the path
          FirstArg = INT2(I-1)
          CALL GETARG(FirstArg,Buffer,status)
          LENG = LEN_TRIM(JobPath)
          LenBuff = LEN_TRIM(Buffer)
          JobPath = JobPath(1:LENG)//' '//Buffer(1:LenBuff)
          Buffer = ''
        END DO
      END IF
      LENG = LEN_TRIM(JobPath)

C      response = MESSAGEBOXQQ(JobPath(1:LENG)//'3ComBar.out'//CHAR(0),
C     *'Path'C,MB$OK)

c Get the data for the plot
      OPEN(32, FILE = JobPath(1:LENG)//'3ComBar.out')
      READ(32,'(L1)') ChkHeading
      READ(32,'(A80)')HEADING
      READ(32,'(L1)') ChkID
      READ(32,'(A80)')IDENTIFICATION
      READ(32,*) SIZE,THETA,PHIdash,RScale
	IF(RScale .LE. 1.0) THEN
	  Scale = RScale * 100
      Else
	  Scale = RScale
      END IF
	DO I = 1,3
	  READ(32,'(A6)') AxLab(I)
      END DO
	DO I = 1,43
	  READ(32,*) P(I)
	END DO
        
      CLOSE(32)

      THETA = THETA*PI/180.0
      PHI = 180 - PHIdash
      PHIdash = PHIdash*PI/180.0
      PHI = PHI*PI/180.0
      TPHIdash = TAN(PHIdash)
      TPHI = TAN(PHI)
      PlinthHeight = 10
      HEIGHT = PlinthHeight
      TICK = 30
      GradGap = 10
      LabGap = 0.5*SIZE
      LabWidth = 6*30
      ArrowLen = 150

      NumberOfInternalClasses = 5
      DEL = SIZE/FLOAT(NumberOfInternalClasses + 2)
      BarSize = 0.5*DEL
      b = 0.5*DEL
      a = b/T30
      c = b/C30
      d = a - c
      Sizes = SIZE - 6*b
      Dels = Sizes/FLOAT(NumberOfInternalClasses)
      BarSizes = 0.5*dels
      beta = 0.5*Dels
      alpha = beta/T30
      gamma = beta/C30
      delta = alpha - gamma
      ticks = 0.75*TICK

      CALL WRTAB(JobPath)
c Place the headings
      IX = 500 - 0.5*SIZE
      IY = SIZE + 200
      IF(ChkHeading) then
        CALL MGMOVE(IX,IY)
        LENG = LEN_TRIM(HEADING)
        CALL LABELC(HEADING,LENG)
      END IF
      IF(ChkID) then
        CALL LBSIZE(13,26)
        CALL MGMOVE(IX,IY - 20)
        LENG = LEN_TRIM(IDENTIFICATION)
        CALL LABELC(IDENTIFICATION,LENG)
      END IF
      CALL MGGRAYFILL(0.95)
      CALL VIEWPLANE(DISTANCE,THETA,PHI)

C Place the componentlabels for each apex of the triangle.
      CALL LBSIZE(13,26)
	X = 0.0 + 2*13
	Y = -0.5*SIZE/C30 - 2*Gradgap
      CALL MGFLOORLABEL(X,Y,0.0,AxLab(2),6,180.0)
	X = -SIZE/2 - 2*Gradgap
	Y = 0.5*SIZE*T30 + 2*Gradgap - 2*13
      CALL MGFLOORLABEL(X,Y,0.0,AxLab(1),6,60.0)
	X = Size/2 + 2*Gradgap
	Y =0.5*SIZE*T30 + 2*Gradgap +2*13
      CALL MGFLOORLABEL(X,Y,0.0,AxLab(3),6,-60.0)

C Label the axes
C **************
c First axis
      CALL LBSIZE(10,18)
      DO I = 1,6
        XI = I*b
        YI =  -0.5*SIZE/C30 + I*a
        XI = XI + 0.5*GradGap
        YI = YI - C30*GradGap
        CALL MGFLOORLABEL(XI-0.5*b, YI-0.5*a, 0.0, GRADS(I), 6, -60.0)
      END DO
      CALL MGFLOORLABEL(XI+0.5*b, YI+0.5*a, 0.0, GRADS(7), 6, -60.0)
      X = LabGap*C30 - 0.5*LabWidth*0.5
      Y = -(LabGap*0.5 + 0.5*LabWidth*C30)
      X1 = X + 0.5*ArrowLen
      Y1 = Y + C30*ArrowLen
      CALL MGFLOORARRO(X, Y, X1, Y1, 0.0)
      X = 0.75*X + 0.25*X1 - 10*C30
      Y = 0.75*Y + 0.25*Y1 + 10*0.5
      CALL MGFLOORLABEL(X,Y,0.0,AxLab(3),6,60.0)

c Second axis
      DO I = 1,6
        XI = 3.5*DEL - (I-1)*DEL
        YI =  d + 2*a
        XI = XI + 0.5*GradGap
        YI = YI + 0.5*GradGap
        CALL MGFLOORLABEL(XI-b, YI, 0.0, GRADS(I), 6, 60.0)
      END DO
      CALL MGFLOORLABEL(XI-b-DEL, YI, 0.0, GRADS(7), 6, 60.0)
      X = 0.5*ArrowLen
      Y =  LabGap
      X1 = -0.5*ArrowLen
      Y1 = Y
      CALL MGFLOORARRO(X, Y, X1, Y1, 0.0)
      X = 0.25*X1 + 0.75*X
      Y = 0.25*Y1 + 0.75*Y - 10
      CALL MGFLOORLABEL(X, Y, 0.0, AxLab(1), 6, 180.0)


c Third axis
      DO I = 1,6
        XI = (I-7)*b
        YI =  0.5*SIZE*T30 - I*a
        XI = XI - GradGap
        YI = YI
        CALL MGFLOORLABEL(XI-0.5*b, YI+0.5*a, 0.0, GRADS(I), 6, 180.0)
      END DO
      CALL MGFLOORLABEL(XI+0.5*b, YI-0.5*a, 0.0, GRADS(7), 6, 180.0)
      X = -LabGap*C30 - 0.5*ArrowLen*0.5
      Y = -LabGap*0.5 + C30*ArrowLen*0.5
      X1 = X + 0.5*ArrowLen
      Y1 = Y - C30*ArrowLen
      CALL MGFLOORARRO(X, Y, X1, Y1, 0.0)
      X = 0.75*X + 0.25*X1 + 10*0.5
      Y = 0.75*Y + 0.25*Y1 + 10*C30
      CALL MGFLOORLABEL(X, Y, 0.0, AxLab(2), 6, -60.0)

C Draw the plinth
      HEIGHT = PlinthHeight
      CALL PRISM(0.0, 0.0, 0.0, SIZE, HEIGHT)

c  Place the inner triangle
      CALL ThreeDLINE(-2*DEL, a+d, HEIGHT, 2*DEL, a+d, HEIGHT)
      CALL ThreeDLINE(2*DEL, a+d, HEIGHT, 0.0, -(c+2*a), HEIGHT)
      CALL ThreeDLINE(0.0, -(c+2*a), HEIGHT, -2*DEL, a+d, HEIGHT)
      CALL LNTYPE(1)

C Graduate the faces
      DO I = 1,6
        XI = I*b
        YI =  -0.5*SIZE/C30 + I*a
        IF(THETA .GT. PI/3 .AND. THETA .LT. 4*PI/3) THEN
          CALL ThreeDLINE(XI, YI, 0.0, XI, YI, HEIGHT)
        END IF
        CALL ThreeDLINE(XI, YI, HEIGHT, XI-0.5*TICK, YI+TICK*C30,HEIGHT)
      END DO

      DO I = 1,6
        XI = 3.5*DEL - I*DEL
        YI =  d + 2*a
        IF(THETA .GT. PI .AND. THETA .LT. 2*PI) THEN
          CALL ThreeDLINE(XI, YI, 0.0, XI, YI, HEIGHT)
        END IF
        CALL ThreeDLINE(XI, YI, HEIGHT, XI-0.5*TICK, YI-0.5*TICK/T30,
     *  HEIGHT)
      END DO

      DO I = 1,6
        XI = (I-7)*b
        YI =  0.5*SIZE*T30 - I*a
        IF(THETA .GT. 5*PI/3 .OR. THETA .LT. 2*PI/3) THEN
          CALL ThreeDLINE(XI, YI, 0.0, XI, YI, HEIGHT)
        END IF
        CALL ThreeDLINE(XI, YI, HEIGHT, XI+TICK, YI,HEIGHT)
      END DO

      !Tick the inner triangle
      CALL LBSIZE(9,18)
      X = 0.0
      Y = -(delta + 3*alpha)
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(1), 3, -60.0)
      DO I = 2,5
        X = X + beta
        Y = Y + alpha
        CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(I), 3, -60.0)
        CALL ThreeDLINE(X, Y, HEIGHT, X-0.5*ticks, Y+ticks*C30, HEIGHT)
      END DO
      X = X + beta
      Y = Y + alpha
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(6), 3, -60.0)

      X = 0.5*sizes
      Y = gamma + alpha
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(1), 3, 60.0)
      DO I= 2,5
        X = X - dels
        CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(I), 3, 60.0)
        CALL ThreeDLINE(X, Y, HEIGHT, X-0.5*ticks, Y-ticks*C30, HEIGHT)
      END DO
      X = X - dels
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(6), 3, 60.0)

      X = - 0.5*sizes
      Y = gamma + alpha
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(1), 3, 180.0)
      DO I= 2,5
        X = X + 0.5*dels
        Y = Y - alpha
        CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(I), 3, 180.0)
        CALL ThreeDLINE(X, Y, HEIGHT, X+ticks, Y, HEIGHT)
      END DO
      X = X + 0.5*dels
      Y = Y - alpha
      CALL MGFLOORLABEL(X, Y, HEIGHT, GRADS2(6), 3, 180.0)

      !Place the bars on the plinth.
      R(1) = -3*DEL
      S(1) = 2*a
      R(2) = 0.0
      S(2) = -4*a
      R(3) = 3*DEL
      S(3) = 2*a
      DO I = 4,8
        R(I) = R(1) + (I-3)*0.5*DEL
        S(I) = S(1) - (I-3)*a
      END DO
      DO I = 9,13
        R(I) = R(2) + (I-8)*0.5*DEL
        S(I) = S(2) + (I-8)*a
      END DO
      DO I = 14,18
        R(I) = R(3) - (I-13)*DEL
        S(I) = S(3)
      END DO
      !Now the inner triangle
      DO I = 19,22
        R(I) = -2*dels + (I-19)*0.5*dels
        S(I) = 2*gamma - (I-19)*alpha
      END DO
      DO I = 23,26
        R(I) = (I-23)*0.5*dels
        S(I) = -2*delta - 2*alpha +(I-23)*alpha
      END DO
      DO I = 27,30
        R(I) = 2*dels - (I-27)*dels
        S(I) = 2*gamma
      END DO
      DO I = 31,33
        R(I) = -beta - dels + (I-31)*0.5*dels
        S(I) = alpha - (I-31)*alpha
      END DO
      DO I = 34,36
        R(I) = (I-34)*0.5*dels
        S(I) = -2*alpha + (I-34)*alpha
      END DO
      DO I = 37,39
        R(I) = beta + dels - (I-37)*dels
        S(I) = alpha
      END DO
      R(40) = -0.5*dels
      S(40) = gamma - delta
      R(41) = 0.0
      S(41) = -2*delta
      R(42) = 0.5*dels
      S(42) = gamma - delta
      R(43) = 0.0
      S(43) = 0.0

      IF(THETA .GE. PI/3 .AND. THETA .LT. PI) THEN
        J(1)  = 1
        J(2)  = 4
        J(3)  = 18
        J(4)  = 5
        J(5)  = 19
        J(6)  = 17
        J(7)  = 31
        J(8)  = 20
        J(9)  = 30
        J(10) = 6
        J(11) = 32
        J(12) = 39
        J(13) = 21
        J(14) = 40
        J(15) = 29
        J(16) = 16
        J(17) = 7
        J(18) = 33
        J(19) = 43
        J(20) = 38
        J(21) = 22
        J(22) = 41
        J(23) = 42
        J(24) = 28
        J(25) = 15
        J(26) = 8
        J(27) = 34
        J(28) = 35
        J(29) = 36
        J(30) = 37
        J(31) = 23
        J(32) = 24
        J(33) = 25
        J(34) = 26
        J(35) = 27
        J(36) = 14
        J(37) = 2
        J(38) = 9
        J(39) = 10
        J(40) = 11
        J(41) = 12
        J(42) = 13
        J(43) = 3
      ELSE IF ( THETA .GE. PI .AND. THETA .LT. 5*PI/3) THEN
        J(1)  = 2
        J(2)  = 9
        J(3)  = 8
        J(4)  = 10
        J(5)  = 23
        J(6)  = 7
        J(7)  = 34
        J(8)  = 24
        J(9)  = 22
        J(10) = 35
        J(11) = 33
        J(12) = 11
        J(13) = 6
        J(14) = 25
        J(15) = 41
        J(16) = 21
        J(17) = 36
        J(18) = 43
        J(19) = 32
        J(20) = 12
        J(21) = 5
        J(22) = 26
        J(23) = 42
        J(24) = 40
        J(25) = 20
        J(26) = 37
        J(27) = 38
        J(28) = 39
        J(29) = 31
        J(30) = 13
        J(31) = 4
        J(32) = 27
        J(33) = 28
        J(34) = 29
        J(35) = 30
        J(36) = 19
        J(37) = 3
        J(38) = 14
        J(39) = 15
        J(40) = 16
        J(41) = 17
        J(42) = 18
        J(43) = 1
      ELSE
        J(1)  = 3
        J(2)  = 14
        J(3)  = 13
        J(4)  = 27
        J(5)  = 37
        J(6)  = 12
        J(7)  = 15
        J(8)  = 26
        J(9)  = 28
        J(10) = 36
        J(11) = 38
        J(12) = 11
        J(13) = 16
        J(14) = 25
        J(15) = 42
        J(16) = 29
        J(17) = 35
        J(18) = 43
        J(19) = 39
        J(20) = 10
        J(21) = 17
        J(22) = 24
        J(23) = 41
        J(24) = 40
        J(25) = 30
        J(26) = 34
        J(27) = 33
        J(28) = 32
        J(29) = 31
        J(30) = 23
        J(31) = 22
        J(32) = 21
        J(33) = 20
        J(34) = 19
        J(35) = 9
        J(36) = 18
        J(37) = 2
        J(38) = 8
        J(39) = 7
        J(40) = 6
        J(41) = 5
        J(42) = 4
        J(43) = 1
      END IF

	DO I = 1,43
	  P(I) = P(I)*200/RScale
      END DO

      CALL MGGRAYFILL(0.8)
      DO I = 1,43
        IF(J(I) .LT. 19) THEN
          CALL PRISM(R(J(I)), S(J(I)), HEIGHT, BarSize, P(J(I)))
        ELSE IF(J(I) .LT. 31) THEN
          CALL PRISM(R(J(I)),S(J(I)),HEIGHT,BarSizes,P(J(I)))
        ELSE IF (J(I) .LT. 40) THEN
          CALL RPRISM(R(J(I)),S(J(I)),HEIGHT,BarSizes,P(J(I)))
        ELSE IF (J(I) .LT. 43) THEN
          CALL PRISM(R(J(I)),S(J(I)),HEIGHT,BarSizes,P(J(I)))
        ELSE
          CALL RPRISM(R(J(I)),S(J(I)),HEIGHT,BarSizes,P(J(I)))
        END IF
      END DO

C Draw scale posts
      CALL LBSIZE(10,26)
      CALL ScalePosts(SIZE,HEIGHT,200.0)
	WRITE(ScaleLAB,'(I3)')Scale
      WS = HEIGHT + 205
	CALL COLOUR(12)
      CALL MGTOPWLAB(0.5*SIZE,0.5*SIZE*T30,WS,ScaleLAB,3,1,THETA,PHI)
      CALL MGRIGHTPRISMWLAB(0.0,WS,SIZE,ScaleLAB,3,1,THETA,PHI)
      CALL MGLEFTPRISMWLAB(-0.5*SIZE,WS,SIZE,ScaleLAB,3,1,THETA,PHI)
	WRITE(ScaleLAB,'(I3)')Scale/2
      WS = HEIGHT + 105
      CALL MGTOPWLAB(0.5*SIZE,0.5*SIZE*T30,WS,ScaleLAB,3,1,THETA,PHI)
      CALL MGRIGHTPRISMWLAB(0.0,WS,SIZE,ScaleLAB,3,1,THETA,PHI)
      CALL MGLEFTPRISMWLAB(-0.5*SIZE,WS,SIZE,ScaleLAB,3,1,THETA,PHI)
	CALL COLOUR(0)
      call grout

      CALL HDCOPY(1.0)
      CALL CLOSE
      END


      SUBROUTINE PRISM(X,Y,Z,SIZE,HEIGHT)
C     ***********************************
C Draw the prism.
      PARAMETER (PI = 3.1415927)
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      CALL FLAT(X,Y,Z,SIZE)
      IF(THETA .GE. 0.0 .AND. THETA .LT. 2*PI/3.0) THEN
        CALL TOPFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RIGHTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL LEFTFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      IF(THETA .GE. 2*PI/3.0 .AND. THETA .LT. 4*PI/3.0) THEN
        CALL LEFTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL TOPFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RIGHTFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      IF(THETA .GE. 4*PI/3.0 .AND. THETA .LE. 2*PI) THEN
        CALL RIGHTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL LEFTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL TOPFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      CALL FLAT(X,Y,Z+HEIGHT,SIZE)
      RETURN
      END

      SUBROUTINE RPRISM(X,Y,Z,SIZE,HEIGHT)
C     ***********************************
C Draw the prism.
      PARAMETER (PI = 3.1415927)
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      CALL RFLAT(X,Y,Z,SIZE)
      IF(THETA .GE. PI/3.0 .AND. THETA .LT. PI) THEN
        CALL RRIGHTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RLEFTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RTOPFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      IF(THETA .GE. PI .AND. THETA .LT. 5*PI/3.0) THEN
        CALL RTOPFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RRIGHTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RLEFTFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      IF(THETA .GE. 5*PI/3.0 .OR. THETA .LT. PI/3) THEN
        CALL RLEFTFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RTOPFACE(X,Y,Z,SIZE,HEIGHT)
        CALL RRIGHTFACE(X,Y,Z,SIZE,HEIGHT)
      END IF
      CALL RFLAT(X,Y,Z+HEIGHT,SIZE)
      RETURN
      END

      SUBROUTINE FLAT(X,Y,Z,SIZE)
C     ***************************
C Construct the triangular bottom or top.
C X, Y and Z are the world coordinates of the back corner.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      STRUCTURE/xyrcoord/
        REAL x
        REAL y
      END STRUCTURE
      RECORD/xyrcoord/VERT(3)
C Get the coords of the vertices before viewing.
      CALL VIEWC(X-0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y-0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL MGPOLY(3,VERT,3)
      RETURN
      END

      SUBROUTINE RFLAT(X,Y,Z,SIZE)
C     ***************************
C Construct the triangular bottom or top.
C X, Y and Z are the world coordinates of the back corner.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      STRUCTURE/xyrcoord/
        REAL x
        REAL y
      END STRUCTURE
      RECORD/xyrcoord/VERT(3)
C Get the coords of the vertices before viewing.
      CALL VIEWC(X-0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y+0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL MGPOLY(3,VERT,3)
      RETURN
      END
C
      SUBROUTINE LEFTFACE(X,Y,Z,SIZE,HEIGHT)
C     **************************************
C Construct the left face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI

	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)
      CALL VIEWC(X-0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X, Y-0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X, Y-0.5*SIZE/C30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X-0.5*SIZE, Y+0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END
C
      SUBROUTINE RLEFTFACE(X,Y,Z,SIZE,HEIGHT)
C     **************************************
C Construct the left face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI

	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)
      CALL VIEWC(X, Y+0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X+0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+0.5*SIZE, Y-0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X, Y+0.5*SIZE/C30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END
C
      SUBROUTINE RIGHTFACE(X,Y,Z,SIZE,HEIGHT)
C     ***************************************
C Construct the right face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)
      CALL VIEWC(X, Y-0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X+0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X+0.5*SIZE, Y+0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X, Y-0.5*SIZE/C30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END
C
      SUBROUTINE RRIGHTFACE(X,Y,Z,SIZE,HEIGHT)
C     ***************************************
C Construct the right face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(4)
      CALL VIEWC(X, Y+0.5*SIZE/C30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X-0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X-0.5*SIZE, Y-0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X, Y+0.5*SIZE/C30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE TOPFACE(X,Y,Z,SIZE,HEIGHT)
C     *************************************
C Construct the top face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(5)
      CALL VIEWC(X+0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X-0.5*SIZE, Y+0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X-0.5*SIZE, Y+0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X+0.5*SIZE, Y+0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE RTOPFACE(X,Y,Z,SIZE,HEIGHT)
C     *************************************
C Construct the top face of the wedge.
      COMMON /CONSTANTS/T30,C30,THETA,PHI
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(5)
      CALL VIEWC(X+0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(1).x = XV
      VERT(1).y = YV
      CALL VIEWC(X-0.5*SIZE, Y-0.5*SIZE*T30, Z, XV, YV, ZV)
      VERT(2).x = XV
      VERT(2).y = YV
      CALL VIEWC(X-0.5*SIZE, Y-0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(3).x = XV
      VERT(3).y = YV
      CALL VIEWC(X+0.5*SIZE, Y-0.5*SIZE*T30, Z+HEIGHT, XV, YV, ZV)
      VERT(4).x = XV
      VERT(4).y = YV
      CALL MGPOLY(3,VERT,4)
      RETURN
      END

      SUBROUTINE MGFLOOR(X,Y,Z,VERT,N,SCALE)
C     **************************************
C Draws a polygon on the floor using X,Y,Z as the reference point.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(*),VERTV(16)
      DO 10 I = 1,N
        WX = X + VERT(I).x*SCALE
        WY = Y + VERT(I).y*SCALE
        CALL VIEWC(WX, WY, Z, VERTV(I).x, VERTV(I).y, ZV)
   10 CONTINUE
      CALL MGPOLY(3,VERTV,3)
      RETURN
      END

      SUBROUTINE MGWALL(X,Y,Z,VERT,N,SCALE,HEIGHT,NSTART)
C     ***************************************************
C Draws a vertical wall on the polygon defined by VERT with X,Y,Z as the
C reference point.
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERT(*)
      I = NSTART
      IN = NSTART + 1
      DO 10 NV = 1,N
        IF(IN .EQ. N+1) IN = 1
        WX1 = X + VERT(I).x*SCALE
        WY1 = Y + VERT(I).y*SCALE
        WX2 = X + VERT(IN).x*SCALE
        WY2 = Y + VERT(IN).y*SCALE
        CALL WALL_ON_LINE(WX1,WY1,Z,WX2,WY2,Z,HEIGHT)
        I = IN
        IN = IN + 1
   10 CONTINUE
      RETURN
      END

      SUBROUTINE WALL_ON_LINE(X1,Y1,Z1,X2,Y2,Z2,HEIGHT)
C     *************************************************
C Draws a wall on the line
	STRUCTURE/xyrcoord/
	  REAL x
	  REAL y
	END STRUCTURE
      RECORD/xyrcoord/VERTV(16)
      CALL VIEWC(X1, Y1, Z1, VERTV(1).x, VERTV(1).y, ZV)
      CALL VIEWC(X2, Y2, Z2, VERTV(2).x, VERTV(2).y, ZV)
      CALL VIEWC(X2, Y2, Z2+HEIGHT, VERTV(3).x, VERTV(3).y, ZV)
      CALL VIEWC(X1, Y1, Z1+HEIGHT, VERTV(4).x, VERTV(4).y, ZV)
      CALL MGPOLY(3,VERTV,4)
      RETURN
      END

      SUBROUTINE ThreeDLINE(X1,Y1,Z1,X2,Y2,Z2)
C     ****************************************
C Draws a line from world coordinates x1,y1,z1 to x2,y2,z2.
      CALL MG3DMOVE(X1, Y1, Z1)
      CALL MG3DPLOT(X2, Y2, Z2)
      RETURN
      END

      SUBROUTINE ScalePosts(SIZE,PlinthHeight,Upper)
c     **********************************************
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      CALL LNTYPE(1)
      CALL LINEWD(0.1)
      CALL COLOUR(7)
      !Top thread first
      CALL ThreeDLINE(-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper,
     *0.0,-0.5*SIZE/C30,PlinthHeight+Upper)
      CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PlinthHeight+Upper,
     *0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper )
      CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper,
     *-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper )
      !Now the intermediate thread
      CALL ThreeDLINE(-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2,
     *0.0,-0.5*SIZE/C30,PlinthHeight+Upper/2)
      CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PlinthHeight+Upper/2,
     *0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2 )
      CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2,
     *-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2 )
      CALL RESLWD
      CALL COLOUR(0)

      !Now the posts
      CALL ThreeDLINE(-0.5*SIZE,0.5*SIZE*T30,PlinthHeight,
     *-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper+10)
      CALL PostTick(1,SIZE,PlinthHeight,0.5*Upper)
      CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PlinthHeight,
     *0.0,-0.5*SIZE/C30,PlinthHeight+Upper+10)
      CALL PostTick(2,SIZE,PlinthHeight,0.5*Upper)
      CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PlinthHeight,
     *0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper+10)
      CALL PostTick(3,SIZE,PlinthHeight,0.5*Upper)
      RETURN
      END

      SUBROUTINE PostTick(Vertex,SIZE,PHeight,Upper)
c     **********************************************
c Ticks the vertical scale posts
      INTEGER Vertex
      COMMON /CONSTANTS/T30,C30,THETA,PHI
      TL = 0.02*SIZE
      If (Vertex .EQ. 1) THEN
        CALL ThreeDLINE(-0.5*SIZE,0.5*SIZE*T30,PHeight+Upper,
     *  -0.5*SIZE + TL, 0.5*SIZE*T30,PHeight+Upper)
        CALL ThreeDLINE(-0.5*SIZE, 0.5*SIZE*T30, PHeight+Upper,
     *  -0.5*SIZE+0.5*TL,0.5*SIZE*T30-TL*C30,PHeight+Upper)
      END IF
      IF (Vertex .EQ. 2) THEN
        CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PHeight+Upper,
     *  -0.5*TL,-0.5*SIZE/C30+TL/C30,PHeight+Upper)
        CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PHeight+Upper,
     *  0.5*TL,-0.5*SIZE/C30+TL/C30,PHeight+Upper)
      END IF
      IF(Vertex .EQ. 3) THEN
        CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PHeight+Upper,
     *  0.5*SIZE - TL, 0.5*SIZE*T30,PHeight+Upper)
        CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PHeight+Upper,
     *  0.5*SIZE - 0.5*TL, 0.5*SIZE*T30-TL*C30,PHeight+Upper)
      END IF
      RETURN
      END
