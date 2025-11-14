Attribute VB_Name = "Module1"

'4    FUNCTION CHAR(I)
'     ******************
'  THIS SUBROUTINE CONVERTS AN INTEGER TO THE CORRESPONDING ASCI OR
'  EBCDIC CHARACTER.
'  IT SHOULD BE ENABLED IF YOUR COMPILER DOES NOT SUPPORT THE CHAR
'  FUNCTION.
'
'4    CHARACTER*1 CHAR
'4    CHAR=I
'4    RETURN
'4    END
'
'4    FUNCTION ICHAR(CHR)
'     *********************
'
'  THIS SUBROUTINE CONVERTS A CHARACTER TO ITS ASCI OR EBCDIC INTEGER
'  EQUIVALENT.
'  IT SHOULD BE ENABLED IF YOUR COMPILER DOES NOT SUPPORT THE CHAR
'  FUNCTION.
'
'4    CHARACTER*1 CHR
'4    ICHAR=CHR
'4    RETURN
'4    END
'
Sub RESET()
'****************
'  RESETS THE HARDCOPY BUFFERS.
' CALL HRESET
Call PRESET
End Sub
'
Sub HDCOPY(SF)
'**************
' CREATES PostScript AND HPGL FILES FOR SUBSEQUENT PRINTING
'
' Unload the PostScript buffer to the output file.
  Call PSPRINT(SF)
'
' Unload the HPGL buffer to the output file.
' CALL HPCOPY(SF)
 End Sub
'
 Sub BOX(XORG, YORG, SPAN, RANGE, NTICX, NTICY)
'*****************************************
 Dim XORG As Integer, YORG As INTERGER, SPAN As Integer, RANGE As Integer
 Dim NTICX As Integer, NTICY As Integer
'Draw a box with lower left at (XORG,YORG) and
'X-axis length = SPAN, Y-axis length = RANGE with
'NTICX ticks on the X-axis and NTICY ticks on the Y-axis
 IXORG = XORG
 IYORG = YORG
 ISPAN = SPAN
 IRANGE = RANGE
 IX = XORG
 IORGX = XORG
 IORGY = YORG
 Call MGMOVE(XORG, YORG)
 ITICK = 6
 DELY = Float(RANGE) / Float(NTICY)
 DELX = Float(SPAN) / Float(NTICX)
 For K = 1 To 2
   For I = 1 To NTICY
     IY = IORGY + IFIX(I * DELY)
     Call PLOT(IX, IY)
     Call PLOT(IX + ITICK, IY)
     Call PLOT(IX, IY)
   Next I
   For I = 1 To NTICX
      IX = IORGX + IFIX(I * DELX)
      Call PLOT(IX, IY)
      Call PLOT(IX, IY - ITICK)
      Call PLOT(IX, IY)
   Next I
   DELX = -DELX
   DELY = -DELY
   ITICK = -ITICK
   IORGY = IY
   IORGX = IX
 Next K
 Call PSSTR
 End Sub

Sub LGYBOX(XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer, NTICX As Integer, NDECY As Integer)
'***************************************************
'Draw a box with ticks on the Y-axis arranged on a logarithmic scale (See SUB BOX)
 IXORG = XORG
 IYORG = YORG
 ISPAN = SPAN
 IRANGE = RANGE
 IX = XORG
 IORGX = XORG
 IORGY = YORG
 ITICK = 6
 DELY = Float(RANGE) / Float(NDECY)
 DELX = Float(SPAN) / Float(NTICX)
 For K = 1 To 2
   Call MGMOVE(IORGX, IORGY)
   For L = 1 To NDECY
     For I = 2 To 10
       IY = DELY * (L - 1) + IORGY + ALOG10(Float(I)) * DELY
       Call PLOT(IX, IY)
       Call PLOT(IX + ITICK, IY)
       IF(I = 10) CALL PLOT(IX+2*ITICK,IY)
       Call PLOT(IX, IY)
     Next I
   Next L
   IF(K = 2) IY=IORGY
   Call PLOT(IX, IY)
   For I = 1 To NTICX
     IX = IORGX + IFIX(I * DELX)
     Call PLOT(IX, IY)
     Call PLOT(IX, IY - ITICK)
     Call PLOT(IX, IY)
   Next I
   DELX = -DELX
   ITICK = -ITICK
   IORGX = XORG + SPAN
 Next K
 Call PSSTR
 Return
 End Sub
'
'
'
'
 Sub LGXSPL(XORG, YORG, SPAN, RANGE, X(), Y(), N, XO, YO, XF, YF, NS)
'*********************************************************
' PLOTS SMOOTH CURVE WITH LOG SCALE ON THE X-AXIS.
'
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Dim XT(NMAX) As Single
 EXTERNAL ALN
 If (N > NMAX) Then
   MESSAGE = "N in SUB LGXSPL should be less than " + Format(NMAX, "####")
   Response = MsgBox(MESSAGE, 0, "ERROR")
   CALL CLOSE
   Stop
 End If
 Call TRANSF(ALN, X, N, XO, XF, XT, XTO, XTF)
 Call SPLHPL(XORG, YORG, SPAN, RANGE, XT, Y, N, XTO, YO, XTF, YF, NS)
 Call PSSTR
 End Sub
'
'
 Sub LGYSPL(XORG, YORG, SPAN, RANGE, X, Y, N, XO, YO, XF, YF, NS)
'************************************************************
' PLOTS SMOOTH CURVE WITH LOG SCALE ON THE Y-AXIS.
'
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Dim YT(NMAX) As Single
 EXTERNAL ALN
 If (N > NMAX) Then
   MESSAGE = "N in SUB LGYSPL should be less than " + Format(NMAX, "####")
   Response = MsgBox(MESSAGE, 0, "ERROR")
   CALL CLOSE
   Stop
 End If
 Call TRANSF(ALN, Y, N, YO, YF, YT, YTO, YTF)
 Call SPLHPL(XORG, YORG, SPAN, RANGE, X, YT, N, XO, YTO, XF, YTF, NS)
 Call PSSTR
 Return
 End Sub
'
'
 Sub LXYSPL(XORG, YORG, SPAN, RANGE, X, Y, N, XO, YO, XF, YF, NS)
'*****************************************************
' PLOTS SMOOTH CURVE WITH LOG SCALE ON BOTH AXES.
'
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Dim YT(NMAX) As Single, XT(NMAX) As Single
 EXTERNAL ALN
 If (N > NMAX) Then
   WRITE(*,1000)NMAX
   MESSAGE = "N in SUB LXYSPL should be less than " + Format(NMAX, "####")
   Call MsgBox(MESSAGE, 0, "ERROR")
   CALL CLOSE
   Stop
 End If
 Call TRANSF(ALN, X, N, XO, XF, XT, XTO, XTF)
 Call TRANSF(ALN, Y, N, YO, YF, YT, YTO, YTF)
 Call SPLHPL(XORG, YORG, SPAN, RANGE, XT, YT, N, XTO, YTO, XTF, YTF, NS)
 Call PSSTR
 Return
 End Sub
'
'
 Sub LGYGRP(XORG, YORG, SPAN, RANGE, X, Y, N, XO, YO, XF, YF)
'*********************************************************
'This SUB plots the graph of y against x with y on log scale
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 For I = 1 To N
   XX = ((X(I) - XO) / (XF - XO)) * SPAN + XORG
   YY = ALOG10(Y(I) / YO) * RANGE / ALOG10(YF / YO) + YORG
   IF(I = 1) CALL MGRMVE(XX,YY)
   IF(I > 1) CALL RPLOT(XX,YY)
   Next I
   Call PSSTR
   Return
   End Sub
'
   Sub LGYDIS(XORG, YORG, SPAN, RANGE, X, Y, N, XO, YO, XF, YF, ISYM)
'  **************************************************************
'  This SUB displays data points of y against x with y
'  on a log scale
   Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
   Call LNTYPE(1)
 For I = 1 To N
   IX = IFIX(((X(I) - XO) / (XF - XO)) * SPAN) + XORG
   IY = IFIX(ALOG10(Y(I) / YO) * RANGE / ALOG10(YF / YO)) + YORG
   Call SYMBOL(IX, IY, ISYM)
 Next I
 Call PSSTR
 Return
 End Sub
'
 Sub LGXBOX(XORG, YORG, SPAN, RANGE, NDECX, NTICY)
'***************************************************
'DRAW A BOX WITH TICKS ON THE X-AXIS ARRANGED ON A LOGARITHMIC
'SCALE (SEE SUB BOX)
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 IXORG = XORG
 IYORG = YORG
 ISPAN = SPAN
 IRANGE = RANGE
 IY = YORG
 IORGX = XORG
 IORGY = YORG
 Call MGMOVE(XORG, YORG)
 ITICK = 6
 DELX = Float(SPAN) / Float(NDECX)
 DELY = Float(RANGE) / Float(NTICY)
 For K = 1 To 2
   For L = 1 To NDECX
     For I = 2 To 10
       IX = DELX * (L - 1) + IORGX + ALOG10(Float(I)) * DELX
       Call PLOT(IX, IY)
       Call PLOT(IX, IY + ITICK)
       IF(I = 10) CALL PLOT(IX,IY+2*ITICK)
       Call PLOT(IX, IY)
     Next I
   Next L
   IF(K = 2) IX=IORGX
   Call PLOT(IX, IY)
   For I = 1 To NTICY
     IY = IORGY + IFIX(I * DELY)
     Call PLOT(IX, IY)
     Call PLOT(IX - ITICK, IY)
     Call PLOT(IX, IY)
   Next I
   Call PLOT(IORGX, IY)
   DELY = -DELY
   ITICK = -ITICK
   IORGY = IY
 Next K
 Call PSSTR
 Return
 End Sub
'
 Sub LGXGRP(XORG, YORG, SPAN, RANGE, X(), Y(), N, XO, YO, XF, YF)
'*********************************************************
'THIS SUB PLOTS THE GRAPH OF Y AGAINST X WITH X ON LOG SCALE
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 For I = 1 To N
   YY = ((Y(I) - YO) / (YF - YO)) * RANGE + YORG
   XX = ALOG10(X(I) / XO) * SPAN / ALOG10(XF / XO) + XORG
   IF(I = 1) CALL MGRMVE(XX,YY)
   IF(I > 1) CALL RPLOT(XX,YY)
 Next I
 Call PSSTR
 End Sub
'
 Sub LGXDIS(XORG, YORG, SPAN, RANGE, X(), Y(), N, XO, YO, XF, YF, ISYM)
'**************************************************************
'THIS SUB DISPLAYS DATA POINTS OF Y AGAINST X WITH X
'ON A LOG SCALE
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 Call LNTYPE(1)
 For I = 1 To N
   IY = IFIX(((Y(I) - YO) / (YF - YO)) * RANGE) + YORG
   IX = IFIX(ALOG10(X(I) / XO) * SPAN / ALOG10(XF / XO)) + XORG
   Call SYMBOL(IX, IY, ISYM)
 Next I
 Call PSSTR
 End Sub
'
'
 Sub LXYBOX(XORG, YORG, SPAN, RANGE, NDECX, NDECY)
'****************************************************
'
'DRAW A BOX WITH X AND Y AXES ON LOGARITHMIC SCALES
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 IXORG = XORG
 IYORG = YORG
 ISPAN = SPAN
 IRANGE = RANGE
 DELY = Float(RANGE) / Float(NDECY)
 DELX = Float(SPAN) / Float(NDECX)
 ITICK = 6
 Call MGMOVE(XORG, YORG)
 IX = XORG
 For K = 1 To 2
   For L = 1 To NDECY
     For I = 2 To 10
       IY = DELY * (L - 1) + YORG + ALOG10(Float(I)) * DELY
       Call PLOT(IX, IY)
       Call PLOT(IX + ITICK, IY)
       IF (I = 10) CALL PLOT (IX+2*ITICK,IY)
       Call PLOT(IX, IY)
     Next I
   IF (K = 2) IX=XORG
   IF (K = 2) IY=YORG
   Call MGMOVE(IX, IY)
   Next L
   For L = 1 To NDECX
     For I = 2 To 10
       IX = XORG + DELX * (L - 1) + ALOG10(Float(I)) * DELX
       Call PLOT(IX, IY)
       Call PLOT(IX, IY - ITICK)
       IF (I = 10) CALL PLOT(IX,IY-2*ITICK)
       Call PLOT(IX, IY)
     Next I
   Next L
   Call MGMOVE(IX, YORG)
   ITICK = -ITICK
 Next K
 Call PSSTR
 End Sub
'
'
 Sub LXYGRP(XORG, YORG, SPAN, RANGE, X(), Y(), N, XO, YO, XF, YF)
'*********************************************************
'
' PLOT THE GRAPH OF Y AGAINST X ON LOG-LOG SCALES
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 For I = 1 To N
   XX = ALOG10(X(I) / XO) * SPAN / ALOG10(XF / XO) + XORG
   YY = ALOG10(Y(I) / YO) * RANGE / ALOG10(YF / YO) + YORG
   IF(I = 1) CALL MGRMVE(XX,YY)
   IF(I > 1) CALL RPLOT (XX,YY)
 Next I
 Call PSSTR
 End Sub
'
'
 Sub LXYDIS(XORG, YORG, SPAN, RANGE, X(), Y(), N, XO, YO, XF, YF, ISYM)
'****************************************************************
' DISPLAY OF DATA POINTS ON LOG-LOG SCALES
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 Call LNTYPE(1)
 For I = 1 To N
   IX = IFIX(ALOG10(X(I) / XO) * SPAN / ALOG10(XF / XO)) + XORG
   IY = IFIX(ALOG10(Y(I) / YO) * RANGE / ALOG10(YF / YO)) + YORG
   Call SYMBOL(IX, IY, ISYM)
 Next I
 Call PSSTR
 End Sub
'
'
'
'
 SUB LAXIS1(IX,IY,LEN,XMIN,XMAX,LAB,LBLEN,LUNIT,LN,FORM,NTICK,ORIEN,LBS)
'***********************************************************************
'
'  LABELS AN AXIS WITH LINEAR COORDINATES.
'   IX,IY...COORDINATES OF AXIS ORIGIN.
'   LEN...LENGTH OF AXIS
'   XMIN... MINIMUM VALUE OF THE VARIABLE TO BE PLOTTED AGAINST THE AXIS
'   XMAX... MAXIMUM VALUE OF THE VARIABLE TO BE PLOTTED AGAINST THE AXIS
'   LAB...CHARACTER STRING TO IDENTIFY AXIS
'   LBLEN...LENGTH OF LABEL
'   LUNIT...CHARACTER STRING TO IDENTIFY UNITS FOR NUMERICAL VALUES
'   LN...LENGTH OF THE STRING
'   FORM...FORMAT FOR NUMBERS ON AXIS (F FORMAT ONLY)
'   NTICK...NUMBER OF TICK MARKS ON AXIS
'   ORIEN... ORIENTATION OF THE AXIS  1=BOTTOM, 2=LEFT, 3=TOP, 4=RIGHT

'   LBS...SIZE OF CHARACTERS IN THE LABEL
'
 INTEGER ORIEN
 CHARACTER*80 LAB,LUNIT
 Call LBSIZE(LBS, 2 * LBS)
 iw = IFIX(Form) * LBS
 NO=LEN/(3*IW)
 INT=NTICK/NO
 IF(INT < 1) INT = 1
 IF(NTICK < 1)NTICK=1
 Del = (XMAX - XMIN) / Float(NTICK)
 I1 = NTICK + 1
 FOR I=1 TO I1 STEP INT
   Val = XMIN + (I - 1) * Del
   IPOS=(FLOAT(I-1)/FLOAT(NTICK))*LEN
   Call AXNUMB(IX, IY, IPOS, ORIEN, Val, Form, LBS)
 Next I
 LB = LBS + 3
 CALL AXLAB(IX,IY,LEN,IW,LB,ORIEN,LAB,LBLEN,LUNIT,LN)
 End Sub
'
'
 SUB LAXIS2(IX,IY,LEN,XMIN,LAB,LBLEN,LUNIT,LN,FORM,NDEC,ORIEN,LBS)
'*****************************************************************
'  LABELS AN AXIS WITH LOGARITHMIC COORDINATES.
'  SEE SUB LAXIS1 FOR DEFINITION OF ARGUMENTS.
'  NDEC...NUMBER OF DECADES TO DISPLAYED.
 CHARACTER*80 LAB,LUNIT
 INTEGER ORIEN
 Call LBSIZE(LBS, 2 * LBS)
 iw = IFIX(Form) * LBS
 DEL=FLOAT(LEN)/FLOAT(NDEC)
 L1 = NDEC + 1
 For L = 1 To L1
   VAL=XMIN*(10.0**(L-1))
   IPOS = (L - 1) * Del
   Call AXNUMB(IX, IY, IPOS, ORIEN, Val, Form, LBS)
 Next L
 LB = LBS + 3
 CALL AXLAB(IX,IY,LEN,IW,LB,ORIEN,LAB,LBLEN,LUNIT,LN)
 End Sub
'
 SUB EXTRAC(X1,Y1,X2,Y2,N1,INT,N2)
'****************************************
'This SUB extracts points at interval int from arrays x1 & y
'to form new arrays x2 & y2. n1=no. of points in input arrays. n2 =
'no. of points in output arrays.
 DIMENSION X1(1), Y1(1), X2(1), Y2(1)
 N2=N1/INT
 For N = 1 To N2
    J=(N-1)*INT + 1
    X2(N) = X1(J)
    Y2(N) = Y1(J)
 Next N
'
'  KEEP THE SAME TERMINAL VALUES.
 If (J < N1) Then
   N2 = N2 + 1
   X2(N2) = X1(N1)
   Y2(N2) = Y1(N1)
 End If
 End Sub
'
 Sub GRID(XORG, YORG, SPAN, RANGE, NTICKX, NTICKY)
'***************************************************
'This draws a grid inside a BOX
 Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
 DX = Float(SPAN) / Float(NTICKX)
 DY = Float(RANGE) / Float(NTICKY)
 IX = XORG
 IY = YORG
 Call LNTYPE(1)
 Call LINEWD(0.28)
 I1 = NTICKY - 1
 For I = 1 To I1
   IY = YORG + IFIX(DY * Float(I))
   Call MGMOVE(IX, IY)
   Call PLOT(IX + SPAN, IY)
 Next I
 I1 = NTICKX - 1
 IY = YORG + RANGE
 For I = 1 To I1
   IX = XORG + IFIX(I * DX)
   Call MGMOVE(IX, IY)
   Call PLOT(IX, YORG)
 Next I
 Call LNTYPE(1)
 End Sub
'
'
 Sub RECT(XORG, YORG, SPAN, RANGE)
'************************************
'This draws a rectangle
 INTEGER XORG,YORG,SPAN,RANGE
 Call MGMOVE(XORG, YORG)
 Call PLOT(XORG + SPAN, YORG)
 Call PLOT(XORG + SPAN, YORG + RANGE)
 Call PLOT(XORG, YORG + RANGE)
 Call PLOT(XORG, YORG)
 End Sub
'
'
 Sub MGARRO(IXM, IYM, IX, IY)
'********************************
'  DRAWS AN ARROW FROM (IXM,IYM) TO (IX,IY).
 If (IXM = IX.And.IYM = IY) Then Return
 Call ARROW(IXM, IYM, IX, IY, IX1, IY1)
 Call MGMOVE(IXM, IYM)
 Call PLOT(IX1, IY1)
 End Sub


Sub MGRARRO(XM As Single, YM As Single, X As Single, Y As Single)
'*************************************************************
  ' DRAWS AN ARROW FROM (XM,YM) TO (X,Y).
  Dim X1 As Single, Y1 As Single
  If (XM = X And YM = Y) Then
    Exit Sub
  Else
    Call RARROW(XM, YM, X, Y, X1, Y1)
    Call MGRMVE(XM, YM)
    Call RPLOT(X1, Y1)
  End If
End Sub


Sub RARROW(XM As Single, YM As Single, X As Single, Y As Single, X1 As Single, Y1 As Single)
'*************************************************************************************
  ' Draws an arrowhead in real precision.
  ' Arrowhead is scaled to the length of the arrow.
  ' D1 is the relative head length and D2 is the relative head width.
  Dim D As Single, D1 As Single, D2 As Single
  Dim WX As Single, WY As Single
  Dim XB As Single, YB As Single
  Dim XT As Single, YT As Single
  If (XM = X And YM = Y) Then
    Exit Sub
  Else
  D = Sqr((XM - X) ^ 2 + (YM - Y) ^ 2)
  D1 = 10#
  D2 = 3.3
  WX = (XM - X) / D
  WY = (YM - Y) / D
  X1 = WX * D1 + X
  Y1 = WY * D1 + Y
  XB = X1 - WY * D2
  YB = Y1 + WX * D2
  XT = X1 + WY * D2
  YT = Y1 - WX * D2
  Call MGRMVE(XT, YT)
  Call RPLOT(X, Y)
  Call RPLOT(XB, YB)
  Call RPLOT(XT, YT)
  Return
  End

'
 Sub ARROW(IXM, IYM, IX, IY, IX1, IY1)
'***************************************
' Draws an arrowhead.
' Arrowhead is scaled to the length of the arrow.
' D1 is the relative head length and D2 is the relative head width.
 If (IXM = IX And IYM = IY) Then Return
 XM = IXM
 YM = IYM
 X = IX
 Y = IY
 D = Sqr((XM - X) ^ 2 + (YM - Y) ^ 2)
 D1 = 10#
 D2 = 3.3
 WX = (XM - X) / D
 WY = (YM - Y) / D
 X1 = WX * D1 + X
 Y1 = WY * D1 + Y
 XB = X1 - WY * D2
 YB = Y1 + WX * D2
 XT = X1 + WY * D2
 YT = Y1 - WX * D2
 IX1 = X1
 IY1 = Y1
 Call MGRMVE(XT, YT)
 Call RPLOT(X, Y)
 Call RPLOT(XB, YB)
 Call RPLOT(XT, YT)
 End Sub
'
 SUB PLARRO(LEN)
'**********************
'  Places an arrow of length LEN at the cursor position.
'  The keystroke will determine the orientation of the arrow as follows.
'  L..LEFT, R..RIGHT, U..UP, D..DOWN
 Character ORIEN
 CALL MGMESSAGE('Select arrow orientation. L..LEFT, R..RIGHT, U..UP
&, D..DOWN  SPACE to proceed',76)
 Call CURS(ORIEN, IXM, IYM)
 IX = IXM
 IY = IYM
 IF(ORIEN = 'L'.OR.ORIEN = 'l')IX=IX-LEN
 IF(ORIEN = 'R'.OR.ORIEN = 'r')IX=IX+LEN
 IF(ORIEN = 'U'.OR.ORIEN = 'u')IY=IY+LEN
 IF(ORIEN = 'D'.OR.ORIEN = 'd')IY=IY-LEN
 If (IXM = IX.And.IYM = IY) Then GoTo 10
 Call MGARRO(IXM, IYM, IX, IY)
 End Sub
'
 Sub INARRO()
'*****************
'  Draws an arrow interactively. Cursor locates tail of arrow
'  then the head of th arrow.
      CHARACTER*1 CHR
      CALL MGMESSAGE('Locate the tail of the arrow then the head. SPACE
     &to proceed',60)
 Call CURS(Chr, IXM, IYM)
 Call CURS(Chr, IX, IY)
 If (IXM = IX.And.IYM = IY) Then GoTo 10
 Call MGARRO(IXM, IYM, IX, IY)
 End Sub
'
 Sub TCIRC(X, Y, FACT)
'**************************
'  Draws a circle centre at (X,Y) and radius FACT on the screen.
 ANG = 0#
 For I = 1 To 30
   ANG = ANG + (6.2832 / 29#)
   XX = FACT * Cos(ANG) + X
   YY = FACT * Sin(ANG) + Y
   IF(I = 1) CALL TRMOVE(XX,YY)
   IF(I > 1) CALL TRPLOT(XX,YY)
 Next I
 End Sub
'
 SUB CIRCLE(IX,IY,RADIUS)
'*******************************
'  Draws a circle centre at (IX,IY) and radius RADIUS.
 X = IX
 Y = IY
 Call TCIRC(X, Y, Radius)
 Call PSCIRC(X, Y, Radius)
' IF(IPTYPE(IZ) = 2.OR.IPTYPE(IZ) = 4)THEN
'   CALL HPCIRC(IX,IY,FACT)
' END IF
 End Sub
'
'
 Sub CIRCL(XO As Integer, YO As Integer, THO As Integer, THF As Integer, RO As Integer, RF As Integer)
'*****************************************************************************************************
'This SUB draws a circle,arc or spiral
'Starting point is (XO,YO) at an angle of THO, the final angle is
'THF, the initial radius is RO and the final radius is RF.
 If (THF < THO) Then Call SWAP(THO, THF)
 CON = 0.017453293
 TH = Float(THO) * CON
 XC = XO - Float(RO) * Cos(TH)
 YC = YO - Float(RO) * Sin(TH)
 DTH = THF - THO
 If (DTH = 0) Then Exit Sub
 Call MGMOVE(XO, YO)
 For I = 1 To DTH Step 10
   TH = CON * (Float(I) + Float(THO))
   R = Float(RF - RO) * Float(I) / Float(DTH) + RO
   X = R * Cos(TH) + XC
   Y = R * Sin(TH) + YC
   Call RPLOT(X, Y)
 Next I
 End Sub

 Sub SWAP(THO, THF)
 '*****************
 INTEGER THO,THF,DUM
 DUM = THF
 THF = THO
 THO = DUM
 End Sub
'
'
 Sub SETUP()
'****************
'  SETS UP THE DATA IN TAB FOR INTERNAL REPRESENTATION OF ASCII
'  CHARACTERS.  THIS SUB IS CALLED ONLY IN PROGRAMS THAT REQUIRE
'  SUBS IN THIS FILE AND DONT USE THE GRAPHICS SUBS.
'
 INTEGER TAB(128),TAB2(256),POINT,STPOIN,PNTA,STPNTA,HPBUFS
 CHARACTER*6 CODE
 COMMON /TEK/TAB,TAB2,POINT,STPOIN,PNTA,STPNTA,HPBUFS
'
'  IDENTIFY CHARACTER REPRESENTATION CODE.  ASCII OR EBCDIC.
 IF(ICHAR('A') = 65)  CODE='ASCII'
 IF(ICHAR('A') = 193) CODE='EBCDIC'
 IF(CODE = 'ASCII') THEN
 For I = 1 To 128
   TAB(I)=I
 Next I
 ELSEIF (CODE = 'EBCDIC') THEN
   CALL ASCEBC(TAB)
 Else
   WRITE(*,*)' METGRAPH does not recognize internal character code'
   Stop
 End If
 Return
 End Sub
'
 Sub CONTRL(N, I1, I2)
'**************************
'SEND SUB one or two characters directly to screen
 INTEGER TAB(128),TAB2(256),POINT,STPOIN,PNTA,STPNTA,HPBUFS
 COMMON /TEK/TAB,TAB2,POINT,STPOIN,PNTA,STPNTA,HPBUFS
 CHARACTER*1 CHAR
 CHARACTER*1 ASCI(80)
 Call CLEAR(ASCI)
 ASCI(1)=CHAR(TAB(I1))
 IF (N = 2) ASCI(2)=CHAR(TAB(I2))
 Call TERMOP(ASCI)
 Call CLEAR(ASCI)
 End Sub
'
 Sub WAIT()
'***************
'Wait until a character followed by RETURN is pressed
 WRITE(*,100)
  100 FORMAT(' PRESS ANY KEY AND RETURN (ENTER) TO CONTINUE.')
      READ(*,1)IWAIT
1 Format (A1)
 Return
 End Sub
'
 SUB SGN(IVAL,IRET)
'*************************
'Return sign of IVAL in IRET : 1=Positive,-1=Negative,0=Zero
 IRET = IVAL
 IF (IRET = 0) RETURN
 IRET = IVAL / IABS(IVAL)
 End Sub
'
'
'
 Sub TPLOT(IX, IY)
'***********************
'  LOW RESOLUTION SCREEN PLOT PRIMITIVE. THE PLOT IS NOT SENT TO
'  THE PLOTTER.
 X = IX
 Y = IY
 Call TRPLOT(X, Y)
 End Sub
'
 Sub GPLOT(X, Y, MODE)
'**************************
'Move or draw to given coordinate
'Modes : 0 - move to given point
'        1 - draw to given point
'        2 - place a dot at the given point
 INTEGER X,Y,MODE
 IF ((MODE = 0).OR.(MODE = 2)) CALL MGMOVE(X,Y)
 IF (MODE = 1) CALL PLOT(X,Y)
 IF (MODE = 2) CALL PLOT(X,Y)
 End Sub
'
 Sub LABELT(STRNG, N)
'**************************
' Writes a text label to the screen at the current position. The label
' is not transmitted to the hardcopy image.
 CHARACTER*80 STRNG
 INTEGER IDENT(80)
 For I = 1 To N
   IDENT(I)=ICHAR(STRNG(I:I))
 Next I
 Call TLABEL(IDENT, N)
 Return
 End Sub

'
 Sub NUMLAB(I)
'********************
'   THIS SUB DRAWS THE NUMBER I AS A LABEL.
'
 INTEGER NUM(10)
 Call FORMT(I, IDIG, NUM)
 NUM(IDIG+1)=ICHAR(' ')
 II = IDIG + 1
 Call LABEL(NUM, II)
 End Sub

'
'
'
'
'
 Sub LINID(IX, IY, LENGT, LABLE, N)
'*************************************
'  THIS SUB PROVIDES A LINE IDENTIFICATION.
'  IT DRAWS A LINE OF LENGTH LENGT , STARTING AT (IX,IY) AND
'  IDENTIFIES IT WITH LABLE HAVING N CHARACTERS.
'  THE CURRENT PEN AND LINE TYPE ARE USED.
 INTEGER LABLE(N)
 Call MGMOVE(IX, IY)
 Call PLOT(IX + LENGT, IY)
 Call MGMOVE(IX + LENGT + 5, IY)
 Call LABEL(LABLE, N)
 End Sub
'
'
 Sub LINIDC(IX, IY, LENGT, STRNG, N)
'**************************************
'  PROVIDES AN IDENTIFIER FOR A PLOTTED LINE. A LINE OF LENGTH LENGT IS
'  DRAWN, STARTING AT (IX,IY) AND IT IS IDENTIFIED WITH A LABEL STRNG N
'  CHARACTERS LONG.
'
 CHARACTER*80 STRNG
 INTEGER IDENT(80)
 FOR I=1 TON
   IDENT(I)=ICHAR(STRNG(I:I))
 Next I
 Call LINID(IX, IY, LENGT, IDENT, N)
 End Sub
'
 Sub LABRDC(LABEL As String, LABLEN As Integer, LU As Integer, ENDOF As Boolean)
'*******************************************************************************
'
'READS A LABEL IN CHARACTER MODE ON LOGICAL UNIT LU AND RETURNS IT.
'THE LABEL LENGTH IS RETURNED IN LEN
'END SUBOF IS SET TRUE IF AN END SUB-OF-FILE IS READ.
'
 LOGICAL END SUBOF
 CHARACTER*1 LABEL(80),BL
 BL=' '
 ENDOF=.FALSE.
 If (LU = 9) Then
   READ(*,101)(LABEL(I),I=1,80)
 Else
   READ (LU,101,END SUB=9999)(LABEL(I),I=1,80)
 End If
  101 FORMAT(80 A1)
 For I = 1 To 80
   IF(ICHAR(LABEL(I)).NE.ICHAR(BL)) LEN=I
 Next I
 Return
 9999 ENDOF=.TRUE.
 End Sub
'
 Function LLENGTH(LABEL)
'***********************
' Returns the length of the label.
 CHARACTER*80 LABEL
 Call LABLEN(LABEL, LLENGTH)
 Return
 End Sub

 SUB LABLEN(LABEL,LEN)
'****************************
'  RETURNS THE LENGTH OF LABEL IN LEN
'
 CHARACTER*80 LABEL
 For I = 80 To 1 Step -1
   IF(LABEL(I:I).NE.' ') GO TO Lbl20
 Next I
Lbl20:
 LEN=I
 Return
 End Sub
'
'
 Sub LABRD(LABLE, N, LU)
'****************************
'  THIS SUB READS A LABEL FROM THE DATA FILE ON LOGICAL UNIT LU
'  NOTE THAT THE SUB RETURNS THE LENGTH OF THE LABEL TO THE
'  CALLING PROGRAM.
'  A BLANK IS ALWAYS DRAWN AFTER THE LABEL. TO SUPPRESS THIS, THE VALUE
'  OF N MUST BE REDUCED BY 1 AFTER RETURNING FROM THIS SUB.
 INTEGER LABLE(1)
 CHARACTER*1 CHR(80)
  101 FORMAT(80A1)
 READ(LU,101) (CHR(I),I=1,80)
 Call LABCON(Chr, 80, LABLE, N)
 N = N + 1
 LABLE(N) = ICHAR(' ')
 End Sub
'
'
 Sub LABSEG(TEXT, L, ISEG, IX, IY, ANGLE)
'******************************************
'  OPENS A SEGMENT TO TAKE A TEXT STRING and writes the label.
'
 CHARACTER*80 TEXT
 Call MGONSG
 Call MGQSEG(ISEG)
 Call MGMOVE(IX, IY)
 IF(ANGLE.NE.0.0) CALL ROTATE(ANGLE)
 Call LABELC(TEXT, L)
 IF(ANGLE.NE.0.0) CALL CANROT
 Call MGCSEG
 End Sub
'
'
 Sub CLEAR(ARRAY)
'***********************
'  CLEARS THE ARRAY TO EBCDIC CODE FOR "SPACE"
'
 CHARACTER*1 ARRAY(80),SPACE
 DATA SPACE/' '/
 For I = 1 To 80
   ARRAY(I) = SPACE
 Next I
 End Sub
'
'
 Sub MCLEAR(OP, IROWS)
'***************************
'  CLEARS THE LOGICAL ARRAY OP,WHICH IROWS ROWS, TO SPACES.
'
 CHARACTER*1 OP(IROWS,80)
 CHARACTER*1 SPACE
 DATA SPACE/' '/
 FOR 10 I=1,IROWS
   FOR 10 J=1,80
     OP(I, J) = SPACE
   Next J
 Next I
 End Sub
'
'
 Sub INSERT(OP, NROWS, NUM, ROW, COL)
'****************************************
'INSERTS INTEGER NUMBER NUM AS A CHARACTER STRING INTO CHARACTER
'ARRAY OP WHICH HAS NROWS ROWS.THE CHARACTER STRING IS INSERTED
'IN ROW ROW AND IS RIGHT JUSTIFIED AT COLUMN COL.
'
 INTEGER ROW,COL,FNUM(10)
 CHARACTER*1 OP(NROWS,80)
 CHARACTER*1 CHAR
 Call FORMT(NUM, ICH, FNUM)
 For I = 1 To ICH
   IS=COL-ICH+I
   OP(ROW,IS)=CHAR(FNUM(I))
 Next I
 End Sub
'
'
 Sub INSRTR(OP, NROWS, X, F, ROW, COL)
'***************************************
'INSERTS A REAL NUMBER X AS A CHARACTER STRING USING FORMAT F INTO
'CHARACTER ARRAY OP WHICH HAS NROWS AND 80 COLUMNS.THE CHARACTER
'STRING IS INSERTED IN ROW ROW AND IS RIGHT JUSTIFIED AT COLUMN COL
'
 INTEGER ROW,COL,FNUM(80)
 CHARACTER*1 CHAR
 CHARACTER*1 OP(NROWS,80)
 Call FORMTR(X, F, FNUM, ICH)
     FOR 800 I=1,ICH
      IS=COL-ICH+I
      OP(ROW,IS)=CHAR(FNUM(I))
800 CONTINUE
      Return
      End Sub
'
      Sub INSRTC(OP, NROWS, WORD, NC, IROW, ICOL)
'     *********************************************
'  INSERTS A WORD OF NC CHARACTERS INTO ARRAY OP WHICH HAS NROWS AND 80
'  COLUMNS. WORD IS RIGHT JUSTIFIED AT COLUMN ICOL.
      CHARACTER*1 OP(NROWS,80),WRD(80)
      CHARACTER*80 WORD,WORDWS
      EQUIVALENCE (WORDWS,WRD(1))
      WORDWS = WORD
     FOR 10 I=1,NC
      IS=ICOL-NC+I
      OP(IROW,IS)=WRD(I)
10 CONTINUE
      Return
      End Sub
'
'
 Sub CHAPEN(I)
'********************
' THIS SUB CHANGES THE PEN NUMBER BY 1
' THE RESULT IS RETURNED MODULO 6.
 I = I + 1
 If (I > 6) Then I = 1
 Call COLOUR(I)
 Return
 End Sub
'
 Sub MGLOGO(IX, IY)
'************************
'  DRAWS THE METGRAPH LOGO AT IX,IY
'
 Call LBSIZE(7, 14)
 Call MGMOVE(IX, IY)
 CALL LABELC('METGRAPH',8)
 End Sub
'
'
 Sub BSORT(A, N, S)
'***********************
'  THIS SUB SORTS THE ARRAY OF REAL NUMBERS INTO ASCENDING ORDER.
'  A SIMPLE BUBBLE SORT IS USED.
'  THE SORTED ARRAY IS IN S
'
 Real A(N), S(N)
'  TRANSFER THE INPUT ARRAY TO A WORKING ARRAY.
 For I = 1 To N
    S(I) = A(I)
 Next I
'
'  PERFORM THE SORT.
 For I = 1 To N - 1
   For J = 1 To N - 1
      If (S(J) > S(J + 1)) Then
        HOLD = S(J + 1)
        S(J + 1) = S(J)
        S(J) = HOLD
      End If
   Next J
 Next I
 End Sub
'
 Function NEXT_LU(LU)
'********************
' Gets the next unallocated logical unit number
 Logical is_named
 NEXT_LU = LU
 is_named = .TRUE.
 DO WHILE (is_named .AND. NEXT_LU  <  100)
   NEXT_LU = NEXT_LU + 1
   INQUIRE(UNIT=NEXT_LU, NAMED=is_named)
 END DO
 End Sub

'
 Function PRESP(QUES, NC)
'***********************
' RETURNS TRUE IF RESPONSE TO THE QUESTION POSED IS POSITVE.
' THE QUESTION HAS NC CHARACTERS.
Logical PRESP
CHARACTER*127 QUES,QUESWS
CHARACTER*1 CHR,YES,LCY,QU(127)
EQUIVALENCE (QUESWS,QU(1))
DATA YES,LCY/'Y','y'/
QUESWS = QUES
PRESP=.FALSE.
WRITE(*,1001)(QU(I),I=1,NC)
 1001 FORMAT(80A1)
      READ(*,1002)CHR
1002 Format (A1)
      IF(CHR = YES.OR.CHR = LCY) PRESP=.TRUE.
 End Sub
'
'
 Function NRESP(QUES, NC)
'***********************
'  RETURNS TRUE IF RESPONSE TO THE QUESTION POSED IS NEGATIVE.
'  THE QUESTION HAS NC CHARACTERS.
 Logical NRESP
 CHARACTER*127 QUES,QUESWS
 CHARACTER*1 CHR,NO,LCN,QU(127)
 EQUIVALENCE (QUESWS,QU(1))
 DATA NO,LCN/'N','n'/
 QUESWS = QUES
 NRESP=.FALSE.
 WRITE(*,1001)(QU(I),I=1,NC)
 1001 FORMAT(80A1)
 READ(*,1002)CHR
1002 Format (A1)
 IF(CHR = NO.OR.CHR = LCN) NRESP=.TRUE.
 Return
 End Sub
'
'
 Function RESP(QUES, NC)
'**********************
'  RETURNS TRUE IF RESPONSE TO THE QUESTION POSED IS POSITIVE,FALSE
'  IF RESPONSE IS NEGATIVE. OTHERWISE THE QUESTION IS REPEATED.
'  THE QUESTION HAS NC CHARACTERS.
 Logical RESP
 CHARACTER*127 QUES,QUESWS
 CHARACTER*1 CHR,NO,LCN,QU(127)
 CHARACTER*1 YES,LCY
 EQUIVALENCE (QUESWS,QU(1))
 DATA YES,LCY/'Y','y'/
 DATA NO,LCN/'N','n'/
 QUESWS = QUES
10 CONTINUE
 WRITE(*,1001)(QU(I),I=1,NC)
 1001 FORMAT(80A1)
 READ(*,1002)CHR
1002 Format (A1)
 IF(CHR = YES.OR.CHR = LCY)RESP=.TRUE.
 IF(CHR = NO.OR.CHR = LCN) RESP=.FALSE.
 If (Chr.NE.YES.And.Chr.NE.LCY.And.Chr.NE.NO.And.Chr.NE.LCN) Then GoTo 10
 Return
 End Sub
'
 SUB ASCEBC(TAB)
'**********************
'  SETS UP THE ASCII-EBCDIC TRANSLATION TABLE
'
 INTEGER TAB(128),EBCD(127)
 DATA EBCD/1,2,3,55,45,46,47,22,5,37,11,12,13,14,15,16,17,18,19,60,
     &61,50,38,24,25,63,39,28,29,30,31,64,90,127,123,91,108,80,125,77,
     &93,92,78,107,96,75,97,240,241,242,243,244,245,246,247,248,249,122,
     &94,76,126,110,111,124,193,194,195,196,197,198,199,200,201,209,210,
     &211,212,213,214,215,216,217,226,227,228,229,230,231,232,233,173,
     &224,189,95,109,121,129,130,131,132,133,134,135,136,137,145,146,
     &147,148,149,150,151,152,153,162,163,164,165,166,167,168,169,192,
     &106,208,161,7/
     FOR 10 I=1,127
        TAB(I)=EBCD(I)
10 CONTINUE
      Return
      End Sub
