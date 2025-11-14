Attribute VB_Name = "mdl3Combar"
Option Explicit
Private T30 As Single, C30 As Single
Private Theta As Single, PHI As Single

Sub ThreeComponentBarChart(NDATA As ThreeComponent)
'**************************************************
' Program to display grade distribution for a three-phase system
' as a 3-D histogram.
  Dim PlinthHeight As Single, height As Single
  Dim Distance As Single, PhiDash As Single, TPhi As Single, TPhiDash As Single
  Dim TICK As Single, Ticks As Single, GradGap As Single, LabGap As Single, Labwidth As Single
  Dim ArrowLen As Single
  Dim NumberOfInternalClasses As Integer
  Dim BarSize As Single, BarSizes As Single
  Dim Alpha As Single, beta As Single, gamma As Single, delta As Single
  Dim A As Single, B As Single, c As Single, d As Single
  Dim IX As Integer, IY As Integer
  Dim ChkHeading As Boolean, chkID As Boolean
  Dim HEADING As String
  Dim SUBHEADING As String
  Dim AXLAB(3) As String
  Dim ScaleLab As String
  Dim GRADS(7) As String
  Dim GRADS2(6) As String
  Dim R(43) As Single, S(43) As Single, P(43) As Single, J(43) As Integer
  Dim N As Integer, I As Integer
  Dim X As Single, Y As Single, X1 As Single, Y1 As Single
  Dim XI As Single, YI As Single
  Dim Del As Single, Dels As Single
  'Dim offset As Single, Offset2 As Single, SizeRDN As Single, SizeRDN2 As Single
  Dim SPACE As Single, Angle1 As Single, SA As Single, CA As Single
  Dim RScale As Single, SCALEValue As Single
  Dim sum As Single, size As Single, sizes As Single
  Dim NCLAS As Single
  Dim WS As Single
  
  On Error GoTo ErrHandler
  
  GRADS(1) = "  0   "
  GRADS(2) = " 0-10 "
  GRADS(3) = "20-40 "
  GRADS(4) = "40-60 "
  GRADS(5) = "60-80 "
  GRADS(6) = "80-100"
  GRADS(7) = " 100  "
  GRADS2(1) = " 0 "
  GRADS2(2) = "20 "
  GRADS2(3) = "40 "
  GRADS2(4) = "60 "
  GRADS2(5) = "80 "
  GRADS2(6) = "100"

  ' Parameters for the three-phase plots
  T30 = Tan(30# * Pi / 180#)
  C30 = Cos(30# * Pi / 180#)
  Distance = 500#

  ChkHeading = CURR3COMPDATA.ChkHeading
  HEADING = CURR3COMPDATA.HEADING
  chkID = CURR3COMPDATA.ChkSubHeading
  SUBHEADING = CURR3COMPDATA.SUBHEADING
  size = CURR3COMPDATA.size
  Theta = CURR3COMPDATA.Azimuth
  PhiDash = CURR3COMPDATA.Colatitude
  RScale = CURR3COMPDATA.SCALEValue
  If (RScale <= 1#) Then
    SCALEValue = RScale * 100
  Else
    SCALEValue = RScale
  End If
  For I = 1 To 3
    AXLAB(I) = CURR3COMPDATA.Component(I)
  Next I
  For I = 1 To 43
    P(I) = CURR3COMPDATA.Value(I)
  Next I
  

  Theta = Theta * Pi / 180#
  PHI = 180 - PhiDash
  PhiDash = PhiDash * Pi / 180#
  PHI = PHI * Pi / 180#
  TPhiDash = Tan(PhiDash)
  TPhi = Tan(PHI)
  PlinthHeight = 10
  height = PlinthHeight
  TICK = 30
  GradGap = 10
  LabGap = 0.5 * size
  Labwidth = 6 * 30
  ArrowLen = 150

  NumberOfInternalClasses = 5
  Del = size / (NumberOfInternalClasses + 2)
  BarSize = 0.5 * Del
  B = 0.5 * Del
  A = B / T30
  c = B / C30
  d = A - c
  sizes = size - 6 * B
  Dels = sizes / NumberOfInternalClasses
  BarSizes = 0.5 * Dels
  beta = 0.5 * Dels
  Alpha = beta / T30
  gamma = beta / C30
  delta = Alpha - gamma
  Ticks = 0.75 * TICK
  Call Set3DOrigin(500, 300, 0)

  If CHPAGE() = 0 Then Exit Sub
  'Place the headings
  IX = 500 - 0.5 * size
  IY = size + 200
  If ChkHeading Then
    Call LBSIZE(13, 26)
    Call MGMOVE(IX, IY)
    Call MGLABEL(HEADING)
  End If
  If (chkID) Then
    Call LBSIZE(11, 22)
    Call MGMOVE(IX, IY - 25)
    Call MGLABEL(SUBHEADING)
  End If
  Call MGGRAYFILL(0.95)
  Call VIEWPLANE(Distance, Theta, PHI)

  'Place the componentlabels for each apex of the triangle.
  Call LBSIZE(13, 26)
    X = 0# + 2 * 13
    Y = -0.5 * size / C30 - 2 * GradGap
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(2), Len(AXLAB(2)), 180#)
    X = -size / 2 - 2 * GradGap
    Y = 0.5 * size * T30 + 2 * GradGap - 2 * 13
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(1), Len(AXLAB(1)), 60#)
    X = size / 2 + 2 * GradGap
    Y = 0.5 * size * T30 + 2 * GradGap + 2 * 13
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(3), Len(AXLAB(3)), -60#)

  ' Label the axes
  ' **************
  ' First axis
  Call LBSIZE(10, 18)
  For I = 1 To 6
    XI = I * B
    YI = -0.5 * size / C30 + I * A
    XI = XI + 0.5 * GradGap
    YI = YI - C30 * GradGap
    Call MGFLOORLABEL(XI - 0.5 * B, YI - 0.5 * A, 0#, GRADS(I), 6, -60#)
  Next I
  Call MGFLOORLABEL(XI + 0.5 * B, YI + 0.5 * A, 0#, GRADS(7), 6, -60#)
  X = LabGap * C30 - 0.5 * Labwidth * 0.5
  Y = -(LabGap * 0.5 + 0.5 * Labwidth * C30)
  X1 = X + 0.5 * ArrowLen
  Y1 = Y + C30 * ArrowLen
  Call MGFLOORARRO(X, Y, X1, Y1, 0#)
  X = 0.75 * X + 0.25 * X1 - 10 * C30
  Y = 0.75 * Y + 0.25 * Y1 + 10 * 0.5
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(3), Len(AXLAB(3)), 60#)

  'Second axis
  For I = 1 To 6
    XI = 3.5 * Del - (I - 1) * Del
    YI = d + 2 * A
    XI = XI + 0.5 * GradGap
    YI = YI + 0.5 * GradGap
    Call MGFLOORLABEL(XI - B, YI, 0#, GRADS(I), 6, 60#)
  Next I
  Call MGFLOORLABEL(XI - B - Del, YI, 0#, GRADS(7), 6, 60#)
  X = 0.5 * ArrowLen
  Y = LabGap
  X1 = -0.5 * ArrowLen
  Y1 = Y
  Call MGFLOORARRO(X, Y, X1, Y1, 0#)
  X = 0.25 * X1 + 0.75 * X
  Y = 0.25 * Y1 + 0.75 * Y - 10
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(1), Len(AXLAB(1)), 180#)


  'Third axis
  For I = 1 To 6
    XI = (I - 7) * B
    YI = 0.5 * size * T30 - I * A
    XI = XI - GradGap
    YI = YI
    Call MGFLOORLABEL(XI - 0.5 * B, YI + 0.5 * A, 0#, GRADS(I), 6, 180#)
  Next I
  Call MGFLOORLABEL(XI + 0.5 * B, YI - 0.5 * A, 0#, GRADS(7), 6, 180#)
  X = -LabGap * C30 - 0.5 * ArrowLen * 0.5
  Y = -LabGap * 0.5 + C30 * ArrowLen * 0.5
  X1 = X + 0.5 * ArrowLen
  Y1 = Y - C30 * ArrowLen
  Call MGFLOORARRO(X, Y, X1, Y1, 0#)
  X = 0.75 * X + 0.25 * X1 + 10 * 0.5
  Y = 0.75 * Y + 0.25 * Y1 + 10 * C30
  Call MGFLOORLABEL(X, Y, 0#, AXLAB(2), Len(AXLAB(2)), -60#)

  'Draw the plinth
  height = PlinthHeight
  Call PRISM(0#, 0#, 0#, size, height)

  'Place the inner triangle
  Call ThreeDLINE(-2 * Del, A + d, height, 2 * Del, A + d, height)
  Call ThreeDLINE(2 * Del, A + d, height, 0#, -(c + 2 * A), height)
  Call ThreeDLINE(0#, -(c + 2 * A), height, -2 * Del, A + d, height)
  Call LNTYPE(1)

  'Graduate the faces
  For I = 1 To 6
    XI = I * B
    YI = -0.5 * size / C30 + I * A
    If (Theta > Pi / 3 And Theta < 4 * Pi / 3) Then
      Call ThreeDLINE(XI, YI, 0#, XI, YI, height)
    End If
    Call ThreeDLINE(XI, YI, height, XI - 0.5 * TICK, YI + TICK * C30, height)
  Next I

  For I = 1 To 6
    XI = 3.5 * Del - I * Del
    YI = d + 2 * A
    If (Theta > Pi And Theta < 2 * Pi) Then
      Call ThreeDLINE(XI, YI, 0#, XI, YI, height)
    End If
    Call ThreeDLINE(XI, YI, height, XI - 0.5 * TICK, YI - 0.5 * TICK / T30, height)
  Next I

  For I = 1 To 6
    XI = (I - 7) * B
    YI = 0.5 * size * T30 - I * A
    If (Theta > 5 * Pi / 3 Or Theta < 2 * Pi / 3) Then
      Call ThreeDLINE(XI, YI, 0#, XI, YI, height)
    End If
    Call ThreeDLINE(XI, YI, height, XI + TICK, YI, height)
  Next I

  'Tick the inner triangle
  Call LBSIZE(9, 18)
  X = 0#
  Y = -(delta + 3 * Alpha)
  Call MGFLOORLABEL(X, Y, height, GRADS2(1), 3, -60#)
  For I = 2 To 5
    X = X + beta
    Y = Y + Alpha
    Call MGFLOORLABEL(X, Y, height, GRADS2(I), 3, -60#)
    Call ThreeDLINE(X, Y, height, X - 0.5 * Ticks, Y + Ticks * C30, height)
  Next I
  X = X + beta
  Y = Y + Alpha
  Call MGFLOORLABEL(X, Y, height, GRADS2(6), 3, -60#)

  X = 0.5 * sizes
  Y = gamma + Alpha
  Call MGFLOORLABEL(X, Y, height, GRADS2(1), 3, 60#)
  For I = 2 To 5
    X = X - Dels
    Call MGFLOORLABEL(X, Y, height, GRADS2(I), 3, 60#)
    Call ThreeDLINE(X, Y, height, X - 0.5 * Ticks, Y - Ticks * C30, height)
  Next I
  X = X - Dels
  Call MGFLOORLABEL(X, Y, height, GRADS2(6), 3, 60#)

  X = -0.5 * sizes
  Y = gamma + Alpha
  Call MGFLOORLABEL(X, Y, height, GRADS2(1), 3, 180#)
  For I = 2 To 5
    X = X + 0.5 * Dels
    Y = Y - Alpha
    Call MGFLOORLABEL(X, Y, height, GRADS2(I), 3, 180#)
    Call ThreeDLINE(X, Y, height, X + Ticks, Y, height)
  Next I
  X = X + 0.5 * Dels
  Y = Y - Alpha
  Call MGFLOORLABEL(X, Y, height, GRADS2(6), 3, 180#)

  'Place the bars on the plinth.
  R(1) = -3 * Del
  S(1) = 2 * A
  R(2) = 0#
  S(2) = -4 * A
  R(3) = 3 * Del
  S(3) = 2 * A
  For I = 4 To 8
    R(I) = R(1) + (I - 3) * 0.5 * Del
    S(I) = S(1) - (I - 3) * A
  Next I
  For I = 9 To 13
    R(I) = R(2) + (I - 8) * 0.5 * Del
    S(I) = S(2) + (I - 8) * A
  Next I
  For I = 14 To 18
    R(I) = R(3) - (I - 13) * Del
    S(I) = S(3)
  Next I
  'Now the inner triangle
  For I = 19 To 22
    R(I) = -2 * Dels + (I - 19) * 0.5 * Dels
    S(I) = 2 * gamma - (I - 19) * Alpha
  Next I
  For I = 23 To 26
    R(I) = (I - 23) * 0.5 * Dels
    S(I) = -2 * delta - 2 * Alpha + (I - 23) * Alpha
  Next I
  For I = 27 To 30
    R(I) = 2 * Dels - (I - 27) * Dels
    S(I) = 2 * gamma
  Next I
  For I = 31 To 33
    R(I) = -beta - Dels + (I - 31) * 0.5 * Dels
    S(I) = Alpha - (I - 31) * Alpha
  Next I
  For I = 34 To 36
    R(I) = (I - 34) * 0.5 * Dels
    S(I) = -2 * Alpha + (I - 34) * Alpha
  Next I
  For I = 37 To 39
    R(I) = beta + Dels - (I - 37) * Dels
    S(I) = Alpha
  Next I
  R(40) = -0.5 * Dels
  S(40) = gamma - delta
  R(41) = 0#
  S(41) = -2 * delta
  R(42) = 0.5 * Dels
  S(42) = gamma - delta
  R(43) = 0#
  S(43) = 0#

  If (Theta >= Pi / 3 And Theta < Pi) Then
    J(1) = 1
    J(2) = 4
    J(3) = 18
    J(4) = 5
    J(5) = 19
    J(6) = 17
    J(7) = 31
    J(8) = 20
    J(9) = 30
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
  ElseIf (Theta >= Pi And Theta < 5 * Pi / 3) Then
    J(1) = 2
    J(2) = 9
    J(3) = 8
    J(4) = 10
    J(5) = 23
    J(6) = 7
    J(7) = 34
    J(8) = 24
    J(9) = 22
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
  Else
    J(1) = 3
    J(2) = 14
    J(3) = 13
    J(4) = 27
    J(5) = 37
    J(6) = 12
    J(7) = 15
    J(8) = 26
    J(9) = 28
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
  End If
  
  'Draw covered scale posts and fences
  If Theta <= Pi / 3 Then
    Call ScalePosts(3, size, height, 200#)
    Call Fences(1, size, height, 200#)
    Call Fences(2, size, height, 200#)
  ElseIf Theta <= Pi Then
    Call ScalePosts(1, size, height, 200#)
    Call Fences(3, size, height, 200#)
    Call Fences(2, size, height, 200#)
  ElseIf Theta <= 5 * Pi / 3 Then
    Call ScalePosts(2, size, height, 200#)
    Call Fences(1, size, height, 200#)
    Call Fences(3, size, height, 200#)
  Else
    Call ScalePosts(3, size, height, 200#)
    Call Fences(1, size, height, 200#)
    Call Fences(2, size, height, 200#)
  End If
    
  For I = 1 To 43
    P(I) = P(I) * 200 / RScale
  Next I

  Call MGGRAYFILL(0.8)
  For I = 1 To 43
    If (J(I) < 19) Then
      Call PRISM(R(J(I)), S(J(I)), height, BarSize, P(J(I)))
    ElseIf (J(I) < 31) Then
      Call PRISM(R(J(I)), S(J(I)), height, BarSizes, P(J(I)))
    ElseIf (J(I) < 40) Then
      Call RPRISM(R(J(I)), S(J(I)), height, BarSizes, P(J(I)))
    ElseIf (J(I) < 43) Then
      Call PRISM(R(J(I)), S(J(I)), height, BarSizes, P(J(I)))
    Else
      Call RPRISM(R(J(I)), S(J(I)), height, BarSizes, P(J(I)))
    End If
  Next I
  
  'Draw visible scale posts and fences
  If Theta <= Pi / 3 Then
    Call ScalePosts(1, size, height, 200#)
    Call ScalePosts(2, size, height, 200#)
    Call Fences(3, size, height, 200#)
    'Label the fence
    ScaleLab = CStr(SCALEValue)
    WS = height + 205
    Call COLOUR(12)
    Call LBSIZE(10, 26)
    Call MGLEFTPRISMWLAB(-0.5 * size, WS, size, ScaleLab, 1, Theta, PHI)
    ScaleLab = CStr(SCALEValue / 2)
    WS = height + 105
    Call MGLEFTPRISMWLAB(-0.5 * size, WS, size, ScaleLab, 1, Theta, PHI)
    Call COLOUR(0)
  ElseIf Theta <= Pi Then
    Call ScalePosts(2, size, height, 200#)
    Call ScalePosts(3, size, height, 200#)
    Call Fences(1, size, height, 200#)
    'Label the fence
    ScaleLab = CStr(SCALEValue)
    WS = height + 205
    Call COLOUR(12)
    Call LBSIZE(10, 26)
    Call MGRIGHTPRISMWLAB(0#, WS, size, ScaleLab, 1, Theta, PHI)
    ScaleLab = CStr(SCALEValue / 2)
    WS = height + 105
    Call MGRIGHTPRISMWLAB(0#, WS, size, ScaleLab, 1, Theta, PHI)
    Call COLOUR(0)
  ElseIf Theta <= 5 * Pi / 3 Then
    Call ScalePosts(1, size, height, 200#)
    Call ScalePosts(3, size, height, 200#)
    Call Fences(2, size, height, 200#)
    'Label the fence
    ScaleLab = CStr(SCALEValue)
    WS = height + 205
    Call COLOUR(12)
    Call LBSIZE(10, 26)
    Call MGTOPWLAB(0.5 * size, 0.5 * size * T30, WS, ScaleLab, Len(ScaleLab), 1, Theta, PHI)
    ScaleLab = CStr(SCALEValue / 2)
    WS = height + 105
    Call MGTOPWLAB(0.5 * size, 0.5 * size * T30, WS, ScaleLab, Len(ScaleLab), 1, Theta, PHI)
    Call COLOUR(0)
  Else
    Call ScalePosts(1, size, height, 200#)
    Call ScalePosts(2, size, height, 200#)
    Call Fences(3, size, height, 200#)
    'Label the fence
    ScaleLab = CStr(SCALEValue)
    WS = height + 205
    Call COLOUR(12)
    Call LBSIZE(10, 26)
    Call MGLEFTPRISMWLAB(-0.5 * size, WS, size, ScaleLab, 1, Theta, PHI)
    ScaleLab = CStr(SCALEValue / 2)
    WS = height + 105
    Call MGLEFTPRISMWLAB(-0.5 * size, WS, size, ScaleLab, 1, Theta, PHI)
    Call COLOUR(0)
  End If

  Call GROUT

  Call HDCOPY(1#)
  Call MGCLOSE
  Exit Sub

ErrHandler:
  Exit Sub

End Sub

Sub PRISM(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'*********************************************************************************
  ' Draw the prism.
  Call FLAT(X, Y, Z, size)
  If (Theta >= 0# And Theta < 2 * Pi / 3#) Then
    Call TOPFACE(X, Y, Z, size, height)
    Call RIGHTFACE(X, Y, Z, size, height)
    Call LEFTFACE(X, Y, Z, size, height)
  End If
  If (Theta >= 2 * Pi / 3# And Theta < 4 * Pi / 3#) Then
    Call LEFTFACE(X, Y, Z, size, height)
    Call TOPFACE(X, Y, Z, size, height)
    Call RIGHTFACE(X, Y, Z, size, height)
  End If
  If (Theta >= 4 * Pi / 3# And Theta <= 2 * Pi) Then
    Call RIGHTFACE(X, Y, Z, size, height)
    Call LEFTFACE(X, Y, Z, size, height)
    Call TOPFACE(X, Y, Z, size, height)
  End If
  Call FLAT(X, Y, Z + height, size)
End Sub

Sub RPRISM(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'**********************************************************************************
  ' Draw the prism.
  Call RFLAT(X, Y, Z, size)
  If (Theta >= Pi / 3# And Theta < Pi) Then
    Call RRIGHTFACE(X, Y, Z, size, height)
    Call RLEFTFACE(X, Y, Z, size, height)
    Call RTOPFACE(X, Y, Z, size, height)
  End If
  If (Theta >= Pi And Theta < 5 * Pi / 3#) Then
    Call RTOPFACE(X, Y, Z, size, height)
    Call RRIGHTFACE(X, Y, Z, size, height)
    Call RLEFTFACE(X, Y, Z, size, height)
  End If
  If (Theta >= 5 * Pi / 3# Or Theta < Pi / 3) Then
    Call RLEFTFACE(X, Y, Z, size, height)
    Call RTOPFACE(X, Y, Z, size, height)
    Call RRIGHTFACE(X, Y, Z, size, height)
  End If
  Call RFLAT(X, Y, Z + height, size)
End Sub

Sub FLAT(X As Single, Y As Single, Z As Single, size As Single)
'**************************************************************
  ' Construct the triangular bottom or top.
  ' X, Y and Z are the world coordinates of the back corner.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(3) As xyrcoord
  'Get the coords of the vertices before viewing.
  Call VIEWC(X - 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y - 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call MGPOLY(3, Vert, 3)
End Sub

Sub RFLAT(X As Single, Y As Single, Z As Single, size As Single)
'*********************************************************************
' Construct the triangular bottom or top.
' X, Y and Z are the world coordinates of the back corner.
' Get the coords of the vertices before viewing.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(3) As xyrcoord
  Call VIEWC(X - 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y + 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call MGPOLY(3, Vert, 3)
End Sub

Sub LEFTFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'*************************************************************************************
' Construct the left face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  Call VIEWC(X - 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y - 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X, Y - 0.5 * size / C30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y + 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub RLEFTFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'*************************************************************************************
' Construct the left face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  Call VIEWC(X, Y + 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y - 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y + 0.5 * size / C30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub RIGHTFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'*************************************************************************************
' Construct the right face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  Call VIEWC(X, Y - 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y + 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y - 0.5 * size / C30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub RRIGHTFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'**************************************************************************************
' Construct the right face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  Call VIEWC(X, Y + 0.5 * size / C30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y - 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y + 0.5 * size / C30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub TOPFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'***********************************************************************************
' Construct the top face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(5) As xyrcoord
  Call VIEWC(X + 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y + 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y + 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y + 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub RTOPFACE(X As Single, Y As Single, Z As Single, size As Single, height As Single)
'************************************************************************************
' Construct the top face of the wedge.
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(5) As xyrcoord
  Call VIEWC(X + 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y - 0.5 * size * T30, Z, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X - 0.5 * size, Y - 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + 0.5 * size, Y - 0.5 * size * T30, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(3, Vert, 4)
End Sub

Sub MGFLOOR(X As Single, Y As Single, Z As Single, Vert() As xyrcoord, N As Single, SCALEValue As Single)
'********************************************************************************************************
' Draws a polygon on the floor using X,Y,Z as the reference point.
  Dim WX As Single, WY As Single, ZV As Single
  Dim I As Integer
  Dim VERTV(16) As xyrcoord
  For I = 1 To N
    WX = X + Vert(I).xcoord * SCALEValue
    WY = Y + Vert(I).ycoord * SCALEValue
    Call VIEWC(WX, WY, Z, VERTV(I).xcoord, VERTV(I).ycoord, ZV)
  Next I
  Call MGPOLY(3, VERTV, 3)
End Sub

Sub MGWALL(X As Single, Y As Single, Z As Single, Vert() As xyrcoord, N As Integer, SCALEValue As Single, height As Single, NSTART As Single)
'************************************************************************************************************************************
' Draws a vertical wall on the polygon defined by VERT with X,Y,Z as the
' reference point.
  Dim I As Integer, NI As Integer, NV As Integer
  Dim WX1 As Single, WY1 As Single, WX2 As Single, WY2 As Single
  I = NSTART
  NI = NSTART + 1
  For NV = 1 To N
    If (NI = N + 1) Then NI = 1
    WX1 = X + Vert(I).xcoord * SCALEValue
    WY1 = Y + Vert(I).ycoord * SCALEValue
    WX2 = X + Vert(NI).xcoord * SCALEValue
    WY2 = Y + Vert(NI).ycoord * SCALEValue
    Call WALL_ON_LINE(WX1, WY1, Z, WX2, WY2, Z, height)
    I = NI
    NI = NI + 1
  Next NV
End Sub

Sub WALL_ON_LINE(X1 As Single, Y1 As Single, Z1 As Single, X2 As Single, Y2 As Single, Z2 As Single, height As Single)
'**********************************************************************************************************
' Draws a wall on the line
  Dim ZV As Single
  Dim VERTV(16) As xyrcoord
  Call VIEWC(X1, Y1, Z1, VERTV(1).xcoord, VERTV(1).ycoord, ZV)
  Call VIEWC(X2, Y2, Z2, VERTV(2).xcoord, VERTV(2).ycoord, ZV)
  Call VIEWC(X2, Y2, Z2 + height, VERTV(3).xcoord, VERTV(3).ycoord, ZV)
  Call VIEWC(X1, Y1, Z1 + height, VERTV(4).xcoord, VERTV(4).ycoord, ZV)
  Call MGPOLY(3, VERTV, 4)
End Sub

Sub ScalePosts(Position As Integer, size As Single, PlinthHeight As Single, upper As Single)
'*************************************************************************************
'See also Sub ScalePost in 3DSubs.bas
   
   Call LNTYPE(1)
   Select Case Position
     Case 1
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PlinthHeight, -0.5 * size, 0.5 * size * T30, PlinthHeight + upper + 10)
     Call PostTick(1, size, PlinthHeight, 0.5 * upper)
     Case 2
     Call ThreeDLINE(0#, -0.5 * size / C30, PlinthHeight, 0#, -0.5 * size / C30, PlinthHeight + upper + 10)
     Call PostTick(2, size, PlinthHeight, 0.5 * upper)
     Case 3
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PlinthHeight, 0.5 * size, 0.5 * size * T30, PlinthHeight + upper + 10)
     Call PostTick(3, size, PlinthHeight, 0.5 * upper)
   End Select
End Sub

Sub Fences(Position As Integer, size As Single, PlinthHeight As Single, upper As Single)
'***************************************************************************************
   'Draw the fences
   Call LNTYPE(1)
   Call LINEWD(0.1)
   Call COLOUR(7)
     'Top thread first
   Select Case Position
     Case 1
     Call ThreeDLINE(0#, -0.5 * size / C30, PlinthHeight + upper, 0.5 * size, 0.5 * size * T30, PlinthHeight + upper)
     Call ThreeDLINE(0#, -0.5 * size / C30, PlinthHeight + upper / 2, 0.5 * size, 0.5 * size * T30, PlinthHeight + upper / 2)
     Case 2
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PlinthHeight + upper, -0.5 * size, 0.5 * size * T30, PlinthHeight + upper)
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PlinthHeight + upper / 2, -0.5 * size, 0.5 * size * T30, PlinthHeight + upper / 2)
     Case 3
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PlinthHeight + upper, 0#, -0.5 * size / C30, PlinthHeight + upper)
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PlinthHeight + upper / 2, 0#, -0.5 * size / C30, PlinthHeight + upper / 2)
  End Select
   
  Call RESLWD
  Call COLOUR(0)

End Sub
Sub PostTick(Vertex As Integer, size As Single, PHeight As Single, upper As Single)
'*********************************************************************************
' Ticks the vertical scale posts
  Dim TL As Single
   TL = 0.02 * size
   If (Vertex = 1) Then
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PHeight + upper, -0.5 * size + TL, 0.5 * size * T30, PHeight + upper)
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PHeight + upper, -0.5 * size + 0.5 * TL, 0.5 * size * T30 - TL * C30, PHeight + upper)
   End If
   If (Vertex = 2) Then
     Call ThreeDLINE(0#, -0.5 * size / C30, PHeight + upper, -0.5 * TL, -0.5 * size / C30 + TL / C30, PHeight + upper)
     Call ThreeDLINE(0#, -0.5 * size / C30, PHeight + upper, 0.5 * TL, -0.5 * size / C30 + TL / C30, PHeight + upper)
   End If
   If (Vertex = 3) Then
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PHeight + upper, 0.5 * size - TL, 0.5 * size * T30, PHeight + upper)
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PHeight + upper, 0.5 * size - 0.5 * TL, 0.5 * size * T30 - TL * C30, PHeight + upper)
   End If
End Sub

Sub MGRIGHTPRISMWLAB(X As Single, Z As Single, size As Single, Label As String, ORIEN As Integer, Theta As Single, PHI As Single)
'**********************************************************************************************************************************************
' Write a label on a right wall of a prism at world coordinates X,Z
  Dim Y As Single
  Dim I As Integer
  Y = X / T30 - 0.5 * size / C30
  'IF(THETA  >  4*PI/3 or THETA  <  PI/3) THEN
    For I = 1 To 6
      LAB_CMT(I) = RPW_CMT(I)
    Next I
    LAB_CMT(5) = LAB_CMT(5) - UVECT(2) * 0.5 * size / C30
    LAB_CMT(6) = LAB_CMT(6) - VVECT(2) * 0.5 * size / C30
    Call PSBUFW(" gsave", 6)
    Call MG3DMOVE(X, Y, Z)
    Call PSCONCAT(LAB_CMT)
    If (ORIEN = 2) Then Call ROTATE(90#)
    If (ORIEN = 3) Then Call ROTATE(180#)
    If (ORIEN = 4) Then Call ROTATE(270#)
    Call SETCUR(X, Z)
    Call MGLABEL(Label)
    If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
    Call PSBUFW(" grestore", 9)
  'END IF

  ' Return the label CMT to 2-D
  LAB_CMT(1) = 1#
  LAB_CMT(2) = 0#
  LAB_CMT(3) = 0#
  LAB_CMT(4) = 1#
  LAB_CMT(5) = 0#
  LAB_CMT(6) = 0#
End Sub

Sub MGLEFTPRISMWLAB(X As Single, Z As Single, size As Single, Label As String, ORIEN As Integer, Theta As Single, PHI As Single)
'*********************************************************************************************************************************************
' Write a label on a left prism wall at world coordinates X,Z
  Dim I As Integer, DistanceAlongWall As Single, Y As Single

  T30 = Tan(30# * Pi / 180#)
  C30 = Cos(30# * Pi / 180#)
  Y = -X / T30 - 0.5 * size / C30
  'IF(THETA  >  1.5*PI or THETA  <  0.5*PI) THEN
    For I = 1 To 6
      LAB_CMT(I) = LPW_CMT(I)
    Next I
    LAB_CMT(5) = LAB_CMT(5) - UVECT(2) * 0.5 * size / C30
    LAB_CMT(6) = LAB_CMT(6) - VVECT(2) * 0.5 * size / C30
    Call PSBUFW(" gsave", 6)
    Call MG3DMOVE(X, Y, Z)
    Call PSCONCAT(LAB_CMT)
    If (ORIEN = 2) Then Call ROTATE(90#)
    If (ORIEN = 3) Then Call ROTATE(180#)
    If (ORIEN = 4) Then Call ROTATE(270#)
    DistanceAlongWall = -(Y - 0.5 * size * T30) / C30
    Call SETCUR(X, Z)
    Call MGLABEL(Label)
    If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
    Call PSBUFW(" grestore", 9)
  'END IF

  ' Return the label CMT to 2-D
  LAB_CMT(1) = 1#
  LAB_CMT(2) = 0#
  LAB_CMT(3) = 0#
  LAB_CMT(4) = 1#
  LAB_CMT(5) = 0#
  LAB_CMT(6) = 0#
End Sub
