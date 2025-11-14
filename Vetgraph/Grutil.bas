Attribute VB_Name = "MdlGrutil"
 Option Explicit
 
 Public Type PageData
  XORG As Integer
  YORG As Integer
  SPAN As Integer
  RANGE As Integer
  XMIN As Single
  XMAX As Single
  YMIN As Single
  YMAX As Single
  TransformX As String
  TransformY As String
  TruncationSize As Single
  XO As Single
  XF As Single
  YO As Single
  YF As Single
End Type
Public Page(25) As PageData
' FORMC transmits the format specification to LABAX
Private FORMC As Single
Private transformation As String    'Holds the current transformation
Private XWS(5001) As Single, YWS(5001) As Single

Sub BBOX(IX As Integer, IY As Integer, ISPAN As Integer, IRANGE As Integer)
'**************************************************************************
'  DRAWS A BOX WITHOUT TICK MARKS
  Dim Vert(4) As xyrcoord
  Dim X As Single, Y As Single, SX As Single, SY As Single
  Dim xspan As Single, yRange As Single, sSpan As Single, sRange As Single
  FORMC = 1#
  Call LINEWD(0.5)
  Call MGGRAYFILL(1#)
  Vert(1).xcoord = IX
  Vert(1).ycoord = IY
  Vert(2).xcoord = IX + ISPAN
  Vert(2).ycoord = IY
  Vert(3).xcoord = IX + ISPAN
  Vert(3).ycoord = IY + IRANGE
  Vert(4).xcoord = IX
  Vert(4).ycoord = IY + IRANGE
  'Call PSPOLY(3, Vert, 4)
  MGPage(PageNumber).pctMG.FillColor = &HFFFFFF
  Call MGPOLY(5, Vert, 4)
  'X = IX
  'Y = IY
  'Call CoordConv(X, Y, SX, SY)
  'xSpan = ISPAN + IX
  'yRange = IRANGE + IY
  'Call CoordConv(xSpan, yRange, sSpan, sRange)
  'MGPage(PageNumber).pctMG.Line (SX, SY)-(sSpan, sRange), , B
  Call RESLWD
  'Save the page data
  Page(PageNumber).XORG = IX
  Page(PageNumber).YORG = IY
  Page(PageNumber).SPAN = ISPAN
  Page(PageNumber).RANGE = IRANGE
End Sub

Sub CLIPBOX()
'************
'  Clips the graph at the bounding box
  Dim X1 As Single, Y1 As Single, X2 As Single, Y2 As Single
  Dim SX As Single, SY As Single
  Dim IX1 As Long, IY1 As Long, IX2 As Long, IY2 As Long
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  X1 = XORG
  Y1 = YORG
  X2 = XORG + SPAN
  Y2 = YORG + RANGE
  Call MGCLIP(X1, Y1, X2, Y2)
End Sub
'
  Sub CLIPOFF()
' ***********
' Resets the clip region to the whole screen
  Call MGCLIPOFF
  End Sub
'
  Sub DRTICK(POS As Single, ORIEN As Integer, TLEN As Integer)
' ************************************************************
'  DRAWS A TICK AT POS RELATIVE TO THE AXIS ORIGIN.
  Dim X As Single, Y As Single
  X = Page(PageNumber).XORG
  Y = Page(PageNumber).YORG
  If (ORIEN = 1) Then
    Call MGRMVE(X + POS, Y)
    Call RPLOT(X + POS, Y + TLEN)
  ElseIf (ORIEN = 2) Then
    Call MGRMVE(X, Y + POS)
    Call RPLOT(X + TLEN, Y + POS)
  ElseIf (ORIEN = 3) Then
    Call MGRMVE(X + POS, Y + Page(PageNumber).RANGE)
    Call RPLOT(X + POS, Y + Page(PageNumber).RANGE - TLEN)
  ElseIf (ORIEN = 4) Then
    Call MGRMVE(X + Page(PageNumber).SPAN, Y + POS)
    Call RPLOT(X + Page(PageNumber).SPAN - TLEN, Y + POS)
  End If
  End Sub
'
'
  Sub DRAXNM(POS As Single, ORIEN As Integer, Val As Single, Form As Single, LBS As Integer)
' ******************************************************************************************
'  SETS THE NUMERICAL LABEL FOR THE AXIS TICK
' FORMC transmits the format specification to LABAX
  Dim IPOS As Integer
  FORMC = Form
  IPOS = POS
  If (ORIEN = 1) Then
    Call AXNUMB(Page(PageNumber).XORG, Page(PageNumber).YORG, IPOS, ORIEN, Val, Form, LBS)
  ElseIf (ORIEN = 2) Then
    Call AXNUMB(Page(PageNumber).XORG, Page(PageNumber).YORG, IPOS, ORIEN, Val, Form, LBS)
  ElseIf (ORIEN = 3) Then
    Call AXNUMB(Page(PageNumber).XORG, Page(PageNumber).YORG + Page(PageNumber).RANGE, IPOS, ORIEN, Val, Form, LBS)
  ElseIf (ORIEN = 4) Then
    Call AXNUMB(Page(PageNumber).XORG + Page(PageNumber).SPAN, Page(PageNumber).YORG, IPOS, ORIEN, Val, Form, LBS)
  End If
  End Sub
'
'
  Sub DRGRID(POS As Single, ORIEN As Integer, TCKLEN As Integer)
' **************************************************************
'  DRAWS A GRID LINE INSIDE A GRAPH BOX
  Dim X As Single, Y As Single, GAP As Single
  
  X = Page(PageNumber).XORG
  Y = Page(PageNumber).YORG
  GAP = TCKLEN + 4
  Call LINEWD(0.1)
  Call LNTYPE(0)
  MGPage(PageNumber).pctMG.ForeColor = &HC0C0C0
  If (ORIEN = 1# Or ORIEN = 3) Then
    Call MGRMVE(X + POS, Y + GAP)
    Call RPLOT(X + POS, Y + Page(PageNumber).RANGE - GAP)
  ElseIf (ORIEN = 2# Or ORIEN = 4) Then
    Call MGRMVE(X + GAP, Y + POS)
    Call RPLOT(X + Page(PageNumber).SPAN - GAP, Y + POS)
  End If
  MGPage(PageNumber).pctMG.ForeColor = RGB(0, 0, 0)
  Call RESLWD
  End Sub
  
  Sub MGLABAX(Lable As String, LBS As Integer, ORIEN As Integer)
  '*************************************************************
    Dim Leng As Integer
    Leng = Len(Lable)
    Call LABAX(Lable, Leng, "", 0, LBS, ORIEN)
  End Sub
  Sub LABAX(LAB As String, LBLEN As Integer, LUNIT As String, LUN As Integer, LBS As Integer, ORIEN As Integer)
' ***********************************************************************************************
'  Sets the axis label.
  Dim AXLEN As Integer, iw As Integer
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  iw = Fix(Abs(FORMC)) * LBS
  If (FORMC <= 0#) Then iw = iw + 3 * LBS
  If (ORIEN = 1) Then
    AXLEN = SPAN
    Call AXLAB(XORG, YORG, AXLEN, iw, LBS, ORIEN, LAB, LBLEN, LUNIT, LUN)
  ElseIf (ORIEN = 2) Then
    AXLEN = RANGE
    Call AXLAB(XORG, YORG, AXLEN, iw, LBS, ORIEN, LAB, LBLEN, LUNIT, LUN)
  ElseIf (ORIEN = 3) Then
    AXLEN = SPAN
    Call AXLAB(XORG, YORG + RANGE, AXLEN, iw, LBS, ORIEN, LAB, LBLEN, LUNIT, LUN)
  ElseIf (ORIEN = 4) Then
    AXLEN = RANGE
    Call AXLAB(XORG + SPAN, YORG, AXLEN, iw, LBS, ORIEN, LAB, LBLEN, LUNIT, LUN)
  End If
  End Sub
'
  Sub LINAX(XMIN As Single, XMAX As Single, NTICK As Integer, FMT As Single, ORIEN As Integer, OPP As String, Grid As String)
'****************************************************************************************************************************
'  Ticks and labels an axis with linear coords.
  Dim TICK(101) As Single
  Dim TICKFL(101) As Boolean
  Dim Interval As Integer, I As Integer
  Dim SPAN As Integer, RANGE As Integer
  Dim Del As Single
  
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  If (NTICK = 0) Then NTICK = 1
  If (ORIEN = 1# Or ORIEN = 3) Then
    Interval = (9 * (Fix(FMT) + 1) * NTICK) / SPAN + 1
  Else
    Interval = 36 * NTICK / RANGE + 1
  End If
  Del = (XMAX - XMIN) / NTICK
  For I = 1 To NTICK + 1
    TICK(I) = XMIN + (I - 1) * Del
    TICKFL(I) = False
  Next I
  For I = 1 To NTICK + 1 Step Interval
    TICKFL(I) = True
  Next I
  Call GAXIS(NTICK + 1, ORIEN, FMT, OPP, Grid, "LINTR", TICK, TICKFL)
  
  'Save the page data
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).XMIN = XMIN
    Page(PageNumber).XMAX = XMAX
  Else
    Page(PageNumber).YMIN = XMIN
    Page(PageNumber).YMAX = XMAX
  End If
    
  End Sub

Sub TRUNCLINAX(XMIN As Single, XMAX As Single, NTICK As Integer, TRUNCS As Single, FMT As Single, ORIEN As Integer, OPP As String, Grid As String)
'*************************************************************************************************************************************************
'C  Ticks and labels an axis with linear coordinates.
      Dim TICK(201) As Single
      Dim TICKFL(201) As Boolean
      Dim IntPart As Integer
      Dim SPAN As Integer, RANGE As Integer
      Dim XiMax As Single
      Dim XiMin As Single
      Dim EtaMAX As Single
      Dim EtaMIN As Single
      Dim DelEta As Single
      Dim ETA As Single, XI As Single
      Dim I As Integer
      
      Page(PageNumber).TruncationSize = TRUNCS
      SPAN = Page(PageNumber).SPAN
      RANGE = Page(PageNumber).RANGE
      If NTICK = 0 Then NTICK = 1
      If ORIEN = 1 Or ORIEN = 3 Then
        IntPart = (9 * (Fix(FMT) + 1) * NTICK) / SPAN + 1
      Else
        IntPart = 36 * NTICK / RANGE + 1
      End If
      XiMax = XMAX / TRUNCS
      XiMin = XMIN / TRUNCS
      EtaMAX = XiMax / (1 - XiMax)
      EtaMIN = XiMin / (1 - XiMin)
      DelEta = (EtaMAX - EtaMIN) / NTICK
      For I = 1 To NTICK + 1
        ETA = EtaMIN + (I - 1) * DelEta
        XI = ETA / (1 + ETA)
        TICK(I) = XI * TRUNCS
        TICKFL(I) = False
      Next I
      For I = 1 To NTICK + 1 Step IntPart
        TICKFL(I) = True
      Next I
      Call GAXIS(NTICK + 1, ORIEN, FMT, OPP, Grid, "TRUNCLIN", TICK, TICKFL)
End Sub

Sub LGAXIS(XMIN As Single, XMAX As Single, ORIEN As Integer, OPP As String, Grid As String)
'********************************************************************************************
'  Ticks and labels an axis with logarithmic coPage(PageNumber).TransformYs.
 Dim TICK(101) As Single
 Dim TICKFL(101) As Boolean
 Dim NDEC As Integer, FMT As Single, I As Integer, J As Integer
 Dim nt As Integer, I1 As Integer, I2 As Integer
 Dim XOW As Single, XFW As Single
  
  For I = 1 To 101
    TICKFL(I) = False
  Next I
  Call LOGLIM(XMIN, XMAX, XOW, XFW, NDEC)
  Call LOGFMT(XOW, NDEC, FMT)
  If (FMT > 4#) Then FMT = -0#
  nt = NDEC * 9 + 1
  TICK(1) = XOW
  For I = 1 To nt Step 9
    TICKFL(I) = True
    If (I < nt) Then
      For J = 2 To 10
        TICK(I + J - 1) = J * TICK(I)
      Next J
    End If
  Next I
  Call TICKRG(XMIN, XMAX, TICK, nt, I1, I2)
  J = 0
  For I = I1 To I2
    J = J + 1
    TICK(J) = TICK(I)
    TICKFL(J) = TICKFL(I)
  Next I
  Call GAXIS(I2 - I1 + 1, ORIEN, FMT, OPP, Grid, "ALN", TICK, TICKFL)
  
  'Save the page data
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).XMIN = XMIN
    Page(PageNumber).XMAX = XMAX
  Else
    Page(PageNumber).YMIN = XMIN
    Page(PageNumber).YMAX = XMAX
  End If
  End Sub

Sub TRUNCLOGAX(XMIN As Single, XMAX As Single, TRUNCS As Single, ORIEN As Integer, OPP As String, Grid As String)
'****************************************************************************************************************
' Ticks and labels an axis with logarithmic coordinates.
  Dim TICK(201) As Single, ATICK(201) As Single
  Dim TICKFL(201) As Boolean, ATICKFL(201) As Boolean
  Dim XiMax As Single
  Dim XiMin As Single
  Dim EtaMAX As Single
  Dim EtaMIN As Single
  Dim nt As Integer, I As Integer, J As Integer, I1 As Integer, I2 As Integer
  Dim XO As Single, XF As Single, NDEC As Integer
  Dim XOW As Single, XFW As Single
  Dim FMT As Single
  
  On Error GoTo ErrHandler

  Page(PageNumber).TruncationSize = TRUNCS
  For I = 1 To 101
    TICKFL(I) = False
  Next I
  Call LOGLIM(XMIN, XMAX, XOW, XFW, NDEC)
  Call LOGFMT(XOW, NDEC, FMT)
  If (FMT > 4#) Then FMT = -0#
  nt = NDEC * 9 + 1
  TICK(1) = XOW
  For I = 1 To nt Step 9
    TICKFL(I) = True
    If (I < nt) Then
      For J = 2 To 10
        TICK(I + J - 1) = J * TICK(I)
      Next J
    End If
  Next I
  Call TICKRG(XMIN, XMAX, TICK, nt, I1, I2)
  J = 0
  For I = I1 To I2
    J = J + 1
    TICK(J) = TICK(I)
    If TICK(J) > TRUNCS Then
      J = J - 1
      Exit For
    End If
    TICKFL(J) = TICKFL(I)
  Next I
  
  Call GAXIS(J, ORIEN, FMT, OPP, Grid, "TRUNCLOG", TICK, TICKFL)
  Exit Sub
ErrHandler:
  MsgBox "Error when drawing truncated log axis", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub TRUNCTICKRG(XMIN As Single, XMAX As Single, TICK() As Single, N As Integer, I1 As Integer, I2 As Integer)
'************************************************************************************************************
  'Finds the subrange of the tick vector to scale the axis for truncated distributions.
  Dim I As Integer
  I1 = 1
  If XMIN > TICK(1) Then
    For I = 2 To N
      If XMIN <= TICK(I) Then Exit For
      I1 = I
    Next I
  End If
  I2 = N
  If XMAX < TICK(N) Then
    For I = N - 1 To 1 Step -1
      If XMAX >= TICK(I) And Page(PageNumber).TruncationSize > TICK(I + 1) Then Exit For
      I2 = I
    Next I
  End If
End Sub

  Sub RRAXIS(XMIN As Single, XMAX As Single, ORIEN As Integer, OPP As String, Grid As String)
' *********************************************************************************************
'  TICKS AND LABELS AN AXIS WITH LOG-LOG-RECIPROCAL COPage(PageNumber).TransformYS.
  Dim I As Integer, I1 As Integer, I2 As Integer
  Dim TICK(14) As Single, ATICK(14) As Single
  Dim TICKFL(14) As Boolean, ATICKFL(14) As Boolean
  TICK(1) = 0.1
  TICK(2) = 0.3
  TICK(3) = 1#
  TICK(4) = 3
  TICK(5) = 10
  TICK(6) = 20
  TICK(7) = 30
  TICK(8) = 40
  TICK(9) = 50
  TICK(10) = 63.2
  TICK(11) = 80
  TICK(12) = 90
  TICK(13) = 99
  TICK(14) = 99.9
  For I = 1 To 14
    TICKFL(I) = True
  Next I
  Call TICKRG(XMIN, XMAX, TICK, 14, I1, I2)
  For I = I1 To I2
    ATICK(I - I1 + 1) = TICK(I)
    ATICKFL(I - I1 + 1) = TICKFL(I)
  Next I
  Call GAXIS(I2 - I1 + 1, ORIEN, 4.1, OPP, Grid, "LLRC", ATICK, ATICKFL)
  
  'Save the page data
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).XMIN = XMIN
    Page(PageNumber).XMAX = XMAX
  Else
    Page(PageNumber).YMIN = XMIN
    Page(PageNumber).YMAX = XMAX
  End If
  End Sub
'
  Sub GISAXIS(XMIN As Single, XMAX As Single, ORIEN As Integer, OPP As String, Grid As String)
'*************************************************************************************************
'  TICKS AND LABELS AN AXIS WITH LOGISTIC COPage(PageNumber).TransformYS.
  
  Dim I As Integer, I1 As Integer, I2 As Integer
  Dim TICK(14) As Single, ATICK(14) As Single
  Dim TICKFL(14) As Boolean, ATICKFL(14) As Boolean
  TICK(1) = 0.1
  TICK(2) = 0.3
  TICK(3) = 1#
  TICK(4) = 3
  TICK(5) = 10
  TICK(6) = 20
  TICK(7) = 30
  TICK(8) = 40
  TICK(9) = 50
  TICK(10) = 70#
  TICK(11) = 80
  TICK(12) = 90
  TICK(13) = 99
  TICK(14) = 99.9
  For I = 1 To 14
    TICKFL(I) = True
  Next I
  Call TICKRG(XMIN, XMAX, TICK, 14, I1, I2)
  For I = I1 To I2
    ATICK(I - I1 + 1) = TICK(I)
    ATICKFL(I - I1 + 1) = TICKFL(I)
  Next I
  Call GAXIS(I2 - I1 + 1, ORIEN, 4.1, OPP, Grid, "LOGIST", ATICK, ATICKFL)
  
  'Save the page data
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).XMIN = XMIN
    Page(PageNumber).XMAX = XMAX
  Else
    Page(PageNumber).YMIN = XMIN
    Page(PageNumber).YMAX = XMAX
  End If
  End Sub
'
  Sub PROBAX(XMIN As Single, XMAX As Single, ORIEN As Integer, OPP As String, Grid As String)
' *******************************************
'  TICKS AND LABELS AN AXIS WITH PROBABILITY COPage(PageNumber).TransformYS.
  Dim I1 As Integer, I2 As Integer, I As Integer
  Dim TICK(11) As Single, ATICK(11) As Single
  Dim TICKFL(11) As Boolean, ATICKFL(11) As Boolean
  TICK(1) = 0.1
  TICK(2) = 1#
  TICK(3) = 3
  TICK(4) = 10
  TICK(5) = 30
  TICK(6) = 50
  TICK(7) = 70
  TICK(8) = 90
  TICK(9) = 97
  TICK(10) = 99
  TICK(11) = 99.9
  For I = 1 To 11
    TICKFL(I) = True
  Next I
  Call TICKRG(XMIN, XMAX, TICK, 11, I1, I2)
    For I = I1 To I2
    ATICK(I - I1 + 1) = TICK(I)
    ATICKFL(I - I1 + 1) = TICKFL(I)
  Next I
  Call GAXIS(I2 - I1 + 1, ORIEN, 4.1, OPP, Grid, "GAUINV", ATICK, ATICKFL)
  
  'Save the page data
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).XMIN = XMIN
    Page(PageNumber).XMAX = XMAX
  Else
    Page(PageNumber).YMIN = XMIN
    Page(PageNumber).YMAX = XMAX
  End If
  End Sub
'
  Sub GAXIS(N As Integer, ORIEN As Integer, Form As Single, OPP As String, Grid As String, transform As String, TICK() As Single, TICKFL() As Boolean)
' ******************************************************************************************************************************************************
'  Ticks and labels an axis using the transformation transform.
  Dim TCKLEN As Integer, LBS As Integer, IOPP As Integer
  Dim POS As Single
  Dim AXLEN As Integer
  Dim Ys As Single, YE As Single
  Dim I As Integer
  
  'Save the page data
  transformation = transform
  If (N < 2) Then N = 2
  Ys = TRANSF(TICK(1))
  YE = TRANSF(TICK(N))
  If (ORIEN = 1# Or ORIEN = 3) Then
    AXLEN = Page(PageNumber).SPAN
  ElseIf (ORIEN = 2# Or ORIEN = 4) Then
    AXLEN = Page(PageNumber).RANGE
  End If
  LBS = 9
  If (AXLEN < 500) Then LBS = 8
  If (AXLEN > 850) Then LBS = 10
  Call LBSIZE(LBS, 2 * LBS)
  For I = 1 To N
    POS = (TRANSF(TICK(I)) - Ys) * AXLEN / (YE - Ys)
    If (TICKFL(I)) Then
      Call DRAXNM(POS, ORIEN, TICK(I), Form, LBS)
      TCKLEN = 10
    Else
      TCKLEN = 7
    End If
    Call DRTICK(POS, ORIEN, TCKLEN)
    If (OPP = "Y") Then
      IOPP = (ORIEN + 1) Mod 4 + 1
      Call DRTICK(POS, IOPP, TCKLEN)
    End If
    If (Grid = "Y" And I > 1 And I < N) Then
      Call DRGRID(POS, ORIEN, TCKLEN)
    End If
  Next I
  If ORIEN = 1 Or ORIEN = 3 Then
    Page(PageNumber).TransformX = transform
    Page(PageNumber).XO = Ys
    Page(PageNumber).XF = YE
  End If
  If ORIEN = 2 Or ORIEN = 4 Then
    Page(PageNumber).TransformY = transform
    Page(PageNumber).YO = Ys
    Page(PageNumber).YF = YE
  End If
  End Sub
'
  Sub TICKRG(XMIN As Single, XMAX As Single, TICK() As Single, N As Integer, I1 As Integer, I2 As Integer)
' ********************************************************************************************************
'  Finds the subrange of the tick vector to scale the axis.
Dim I As Integer
  I1 = 1
  If (XMIN > TICK(1)) Then
    For I = 2 To N
      If (XMIN < TICK(I)) Then Exit For
      I1 = I
    Next I
  End If
  I2 = N
  If (XMAX < TICK(N)) Then
    For I = N - 1 To 1 Step -1
      If (XMAX > TICK(I)) Then Exit For
      I2 = I
    Next I
  End If
  End Sub
'
  Function LINTR(X As Single) As Single
' *************************************
'  A straight-forward identity transformation.
  LINTR = X
  End Function
'
  Function LLRC(X As Single) As Single
' ************************************
'  Calculates the log-log-reciprocal-complement transformation.
'  The argument is in percent.
  Dim WS As Single
  WS = -Log(1# - 0.01 * X)
  LLRC = Log(WS)
  End Function
  Function LOGIST(X As Single) As Single
' **************************************
'  Calculates the logistic transformation.
'  The argument is in percent.
  If (X >= 100#) Then
  MESSAGE = "Argument of the logistic function must be less than 100"
  response = MsgBox(MESSAGE, 0, "ERROR")
   End
  End If
  If (X <= 0#) Then
  MESSAGE = "Argument of the logistic function must be greater than 0"
  response = MsgBox(MESSAGE, 0, "ERROR")
    End
  End If
  LOGIST = -Log(100# / X - 1#)
  End Function
'
  Function GAUINV(X As Single) As Single
' **************************************
'  Calculates the inverse of the Gaussian probability function.
'  Uses the rational approximation given by Abramowitz & Stegun 26.2.23
'  The argument is in percent.
  Dim P As Single, Q As Single
  Dim wp As Single, T As Single
  Dim c(0 To 2) As Single, d(1 To 3) As Single
  Dim WS As Single
  c(0) = 2.515517
  c(1) = 0.802853
  c(2) = 0.010328
  d(1) = 1.432788
  d(2) = 0.189269
  d(3) = 0.001308
  P = 0.01 * X
  If (P > 0.5) Then Q = 1# - P
  If (P <= 0.5) Then Q = P
  If (Q <= 0# Or Q > 0.5) Then
    Call MGCLOSE
  MESSAGE = "Probability for function GAUINV out of bounds"
  response = MsgBox(MESSAGE, 0, "ERROR")
    Stop
  End If
  wp = Abs(Q)
  T = Sqr(-2# * Log(wp))
  WS = T - (c(0) + c(1) * T + c(2) * T * T) / (1# + d(1) * T + d(2) * T * T + d(3) * T * T * T)
  If P - 0.5 < 0 Then
    GAUINV = -Abs(WS)
  Else
    GAUINV = Abs(WS)
  End If
  End Function
  
  Static Function ALN(X As Single) As Single
' ******************************************
'  Calculates the log function for use as an external function.
  Dim XX As Double
  On Error GoTo ErrHandler
  XX = X
  ALN = Log(XX)
  Exit Function
ErrHandler:
  MsgBox "Error when calculating log function", vbCritical, "ERROR"
  Exit Function
  End Function

  Function INVLLRC(Y As Single) As Single
  '**************************************
  'Calculate the inverse of the LLRC transformation.
    INVLLRC = 100 * (1# - Exp(-Exp(Y)))
  End Function

  Function INVLOGIST(Y As Single) As Single
  '****************************************
  'Calculate the inverse of the logistic transformation.
    INVLOGIST = 100# / (1# + Exp(-Y))
  End Function
   
  Function GAUSPROB(Y As Single) As Single
  '***************************************
   'Calculates the Gaussian probability using the rational approximation
    'given by Abramovitz and Stegun 26.2.19
    'Result is returned in %
    Dim I As Integer
    Dim d(6) As Double, X As Double, WS As Double
    d(1) = 0.049867347
    d(2) = 0.0211410061
    d(3) = 0.0032776263
    d(4) = 0.0000380036
    d(5) = 0.0000488906
    d(6) = 0.000005383
    X = Abs(Y)
    WS = 1#
    For I = 1 To 6
      WS = WS + d(I) * X ^ I
    Next I
    I = 16
    WS = WS ^ (-I)
    If (Y >= 0#) Then
      GAUSPROB = 100 * (1# - 0.5 * WS)
    Else
      GAUSPROB = 100 * 0.5 * WS
    End If
  End Function

Function INVALN(Y As Single) As Single
'*************************************
 'Calculates the inverse of the ALN transformation
  INVALN = Exp(Y)
End Function

Function TRUNCLOG(X As Single) As Single
'***************************************
' Transformation of the log scale when a distribution is truncated.
  Dim WS As Single
  WS = X / Page(PageNumber).TruncationSize
  WS = WS / (1# - WS)
  TRUNCLOG = Log(WS)
End Function

Function INVTRUNCLOG(Y As Single) As Single
'******************************************
'Calculate the inverse of the truncated log transformation.
  INVTRUNCLOG = Page(PageNumber).TruncationSize * Exp(Y) / (1# + Exp(Y))
End Function

Function TRUNCLIN(X As Single) As Single
'***************************************
'Transformation of the linear scale when a distribution is truncated.
  Dim WS As Single
  WS = X / Page(PageNumber).TruncationSize
  TRUNCLIN = WS / (1# - WS)
End Function

Function INVTRUNCLIN(Y As Single) As Single
'******************************************
'Calculate the inverse of the trunclin transformation.
      INVTRUNCLIN = Page(PageNumber).TruncationSize * Y / (Y + 1)
End Function
  
  Sub TRANSFM(transform As String, X() As Single, N As Integer, XO As Single, XF As Single, XT() As Single)
' *********************************************************************************************************
'  General transformation of an array X
  Dim I As Integer
  transformation = transform
  For I = 1 To N
    XT(I) = TRANSF(X(I))
  Next I
  End Sub
'
  Sub PLGRPH(X() As Single, Y() As Single, N As Integer, TRNSF1 As String, TRNSF2 As String)
' ******************************************************************************************
'  Plots a graph using straight line segments in any coPage(PageNumber).TransformYs system.
'  TRNSF1 is the coPage(PageNumber).TransformY transformation for the X-axis.
'  TRNSF2 is the coPage(PageNumber).TransformY transformation for the Y-axis.
  Dim I As Integer
  Dim XO As Single, XF As Single, YO As Single, YF As Single
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  XO = Page(PageNumber).XO
  YO = Page(PageNumber).YO
  XF = Page(PageNumber).XF
  YF = Page(PageNumber).YF
  If (N > 5001) Then
  MESSAGE = "Graphs are limited to 5001 points"
  response = MsgBox(MESSAGE, 0, "ERROR")
    Call MGCLOSE
    Stop
  End If

  For I = 1 To N
    transformation = TRNSF1
    XWS(I) = TRANSF(X(I))
    transformation = TRNSF2
    YWS(I) = TRANSF(Y(I))
  Next I
  Call GRPHPL(XORG, YORG, SPAN, RANGE, XWS, YWS, N, XO, YO, XF, YF)
  End Sub
'
  Sub PLSMGR(X() As Single, Y() As Single, N As Integer, NS As Integer, TRNSF1 As String, TRNSF2 As String)
' *********************************************************************************************************
'  Plots a smooth curve in any coordinate system.
  Dim XWS(5001) As Single, YWS(5001) As Single
  Dim I As Integer
  Dim XO As Single, XF As Single, YO As Single, YF As Single
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  XO = Page(PageNumber).XO
  YO = Page(PageNumber).YO
  XF = Page(PageNumber).XF
  YF = Page(PageNumber).YF
  If (N > 5001) Then
  MESSAGE = "Graphs are limited to 5001 points"
  response = MsgBox(MESSAGE, 0, "ERROR")
    Call MGCLOSE
    Stop
  End If

  For I = 1 To N
    transformation = TRNSF1
    XWS(I) = TRANSF(X(I))
    transformation = TRNSF2
    YWS(I) = TRANSF(Y(I))
  Next I
  Call SPLHPL(XORG, YORG, SPAN, RANGE, XWS, YWS, N, XO, YO, XF, YF, NS)
  End Sub

Sub SelectColumn(Matrix() As Single, Row As Integer, Column As Integer, NumberOfPoints As Integer, Vector() As Single)
'*********************************************************************************************************************
'Selects a column vector from a matrix to plot.
'Data starts at Row,Column and contains NumberOfPoints elements.
'Vector for plotting is returned in Vector.
  Dim I As Integer
  For I = 1 To NumberOfPoints
    Vector(I) = Matrix(Row - 1 + I, Column)
  Next I
End Sub

Sub DISPNT(X() As Single, Y() As Single, N As Integer, ISYM As Integer, TRNSF1 As String, TRNSF2 As String)
'***********************************************************************************************************
'  Displays data points in any coordinate system.
  Dim I As Integer
  Dim XO As Single, XF As Single, YO As Single, YF As Single
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  XO = Page(PageNumber).XO
  YO = Page(PageNumber).YO
  XF = Page(PageNumber).XF
  YF = Page(PageNumber).YF
  If (N > 5001) Then
  MESSAGE = "Graphs are limited to 5001 points"
  response = MsgBox(MESSAGE, 0, "ERROR")
    Call MGCLOSE
    End
  End If

  For I = 1 To N
    transformation = TRNSF1
    XWS(I) = TRANSF(X(I))
    transformation = TRNSF2
    YWS(I) = TRANSF(Y(I))
  Next I
  Call EXPDIS(XORG, YORG, SPAN, RANGE, XWS, YWS, N, XO, YO, XF, YF, ISYM)
End Sub

Sub DISPSomePNTS(X() As Single, Y() As Single, istart As Integer, IEnd As Integer, Istep As Integer, ISYM As Integer, TRNSF1 As String, TRNSF2 As String)
'********************************************************************************************************************************************************
'  Displays a selection data points in any coordinate system.
  Dim I As Integer, J As Integer
  Dim XO As Single, XF As Single, YO As Single, YF As Single
  Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
  XORG = Page(PageNumber).XORG
  YORG = Page(PageNumber).YORG
  SPAN = Page(PageNumber).SPAN
  RANGE = Page(PageNumber).RANGE
  XO = Page(PageNumber).XO
  YO = Page(PageNumber).YO
  XF = Page(PageNumber).XF
  YF = Page(PageNumber).YF

  J = 0
  For I = istart To IEnd Step Istep
    J = J + 1
    transformation = TRNSF1
    XWS(J) = TRANSF(X(I))
    transformation = TRNSF2
    YWS(J) = TRANSF(Y(I))
  Next I
  Call EXPDIS(XORG, YORG, SPAN, RANGE, XWS, YWS, J, XO, YO, XF, YF, ISYM)
End Sub

Sub CONVERTGRPH(X As Single, Y As Single, SX As Single, SY As Single)
'********************************************************************
' Converts graph axis coordinates to screen coordinates in the linear coordinate system.
    Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
    Dim XO As Single, XF As Single, YO As Single, YF As Single
    XORG = Page(PageNumber).XORG
    YORG = Page(PageNumber).YORG
    SPAN = Page(PageNumber).SPAN
    RANGE = Page(PageNumber).RANGE
    XO = Page(PageNumber).XO
    XF = Page(PageNumber).XF
    YO = Page(PageNumber).YO
    YF = Page(PageNumber).YF
  SX = GRPHPT(X, XO, XF, SPAN, XORG)
  SY = GRPHPT(Y, YO, YF, RANGE, YORG)
End Sub

  Sub CONVERTSCRN(SX As Single, SY As Single, X, Y)
' *************************************************
' Converts screen coordinates to graph axis coordinates in the linear coordinate system.
    Dim XORG As Single, YORG As Single, SPAN As Single, RANGE As Single
    Dim XO As Single, XF As Single, YO As Single, YF As Single
    
    XORG = Page(PageNumber).XORG
    YORG = Page(PageNumber).YORG
    SPAN = Page(PageNumber).SPAN
    RANGE = Page(PageNumber).RANGE
    XO = Page(PageNumber).XO
    XF = Page(PageNumber).XF
    YO = Page(PageNumber).YO
    YF = Page(PageNumber).YF
    X = GRSCAL(XORG, SPAN, XO, XF, SX)
    Y = GRSCAL(YORG, RANGE, YO, YF, SY)
  End Sub

Sub CONVERTGRPH_TRANS(X As Single, Y As Single, SX As Single, SY As Single, TRANSF1 As String, TRANSF2 As String)
' *****************************************************************************************************************
' Converts graph axis coordinates to screen coordinates using the
' transformations TRANSF1 and TRANSF2
    Dim XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer
    Dim XO As Single, XF As Single, YO As Single, YF As Single
    On Error GoTo ErrHandler
    XORG = Page(PageNumber).XORG
    YORG = Page(PageNumber).YORG
    SPAN = Page(PageNumber).SPAN
    RANGE = Page(PageNumber).RANGE
    XO = Page(PageNumber).XO
    XF = Page(PageNumber).XF
    YO = Page(PageNumber).YO
    YF = Page(PageNumber).YF
  transformation = TRANSF1
  SX = GRPHPT(TRANSF(X), XO, XF, SPAN, XORG)
  transformation = TRANSF2
  SY = GRPHPT(TRANSF(Y), YO, YF, RANGE, YORG)
  Exit Sub
ErrHandler:
  MsgBox "Error when converting graph coordinates", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub CONVERTSCRN_TRANS(SX As Single, SY As Single, X As Single, Y As Single)
' ***************************************************************************
' Converts screen coordinates to graph axis coordinates .
    Dim XORG As Single, YORG As Single, SPAN As Single, RANGE As Single
    Dim XO As Single, XF As Single, YO As Single, YF As Single
    On Error GoTo ErrHandler
    XORG = Page(PageNumber).XORG
    YORG = Page(PageNumber).YORG
    SPAN = Page(PageNumber).SPAN
    RANGE = Page(PageNumber).RANGE
    XO = Page(PageNumber).XO
    XF = Page(PageNumber).XF
    YO = Page(PageNumber).YO
    YF = Page(PageNumber).YF
    transformation = Page(PageNumber).TransformX
    X = INVTRANSF(GRSCAL(XORG, SPAN, XO, XF, SX))
    transformation = Page(PageNumber).TransformY
    Y = INVTRANSF(GRSCAL(YORG, RANGE, YO, YF, SY))
    Exit Sub
ErrHandler:
  MsgBox "Error when converting screen coordinates", vbCritical, "ERROR"
  Exit Sub
End Sub
'
Function GRSCAL(XORG As Single, SPAN As Single, XO As Single, XF As Single, IX As Single) As Single
'***************************************************************************************************
'  SCALES A SCREEN COORDINATE TO GRAPH COORDINATES
  On Error GoTo ErrHandler
  GRSCAL = (IX - XORG) * (XF - XO) / SPAN + XO
  Exit Function
ErrHandler:
  MsgBox "Error when scaling screen coordinates", vbCritical, "ERROR"
  Exit Function
End Function

Static Function TRANSF(X As Single)
'***********************************
'Selects the transformation for graph plotting.
Select Case transformation
  Case "LINTR"
    TRANSF = LINTR(X)
  Case "LLRC"
    TRANSF = LLRC(X)
  Case "GAUINV"
    TRANSF = GAUINV(X)
  Case "ALN"
    TRANSF = ALN(X)
  Case "LOGIST"
    TRANSF = LOGIST(X)
  Case "TRUNCLOG"
    TRANSF = TRUNCLOG(X)
  Case "TRUNCLIN"
    TRANSF = TRUNCLIN(X)
End Select
End Function

Static Function INVTRANSF(X As Single) As Single
'***********************************************
'Selects the inverse transformation for graph plotting.
Select Case transformation
  Case "LINTR"
    INVTRANSF = LINTR(X)
  Case "LLRC"
    INVTRANSF = INVLLRC(X)
  Case "GAUINV"
    INVTRANSF = GAUSPROB(X)
  Case "ALN"
    INVTRANSF = INVALN(X)
  Case "LOGIST"
    INVTRANSF = INVLOGIST(X)
  Case "TRUNCLOG"
    INVTRANSF = INVTRUNCLOG(X)
  Case "TRUNCLIN"
    INVTRANSF = INVTRUNCLIN(X)
End Select
End Function

Public Sub SetVertex(X As Single, Y As Single, Vertex As xyrcoord)
'*****************************************************************
  'Sets up Vertex
  Vertex.xcoord = X
  Vertex.ycoord = Y
End Sub

Public Sub MGVertexMove(Vertex As xyrcoord)
'******************************************
  'Moves to vertex
  Call MGRMVE(Vertex.xcoord, Vertex.ycoord)
End Sub

Public Sub MGVertexPlot(Vertex As xyrcoord)
'******************************************
  'Moves to vertex
  Call MGPLOT(Vertex.xcoord, Vertex.ycoord)
End Sub

