Attribute VB_Name = "Mdl3DBarcht"
Option Explicit
Private Distance As Single
Private Theta As Single
Private PHI As Single
Sub BARCHART()
'*************
' Plots the 3-D barchart for a matrix of liberation spectra.
  Dim P(12, 51) As Single
  Dim GradeAxisTitle As String, SizeAxisTitle As String
  Dim SizeLabs(51) As String
  Dim LAYOUT  As String
  Dim NGC As Integer, NDC As Integer
  Dim Scalefactor As Single, Pmax As Single
  Dim I As Integer, J As Integer
  Dim NL As Integer, NL1 As Integer

  NGC = 12
 
  GradeAxisTitle = CURRMODELDATA(ID).TEXTSTRING(26)
  NL = Len(GradeAxisTitle)
  SizeAxisTitle = CURRMODELDATA(ID).TEXTSTRING(27)
  NL1 = Len(SizeAxisTitle)
  NDC = CURRMODELDATA(ID).PARAM(1)

  For I = 1 To NDC
    SizeLabs(I) = CURRMODELDATA(ID).TEXTSTRING(I)
  Next I
  Scalefactor = 1 / CURRMODELDATA(ID).PARAM(302)
  For J = 1 To NGC
    For I = 1 To NDC
      P(J, I) = CURRMODELDATA(ID).PARAM(12 * (I - 1) + J + 1)
    Next I
  Next J

' Scale the data
  Pmax = 0#
  For I = 1 To NGC
    For J = 1 To NDC
      P(I, J) = Scalefactor * P(I, J)
      'Get largest P value for the fence scale
      If (P(I, J) > Pmax) Then
        Pmax = P(I, J)
      End If
    Next J
  Next I
  Call THREE_D_BARCHART(P, Pmax, NGC, NDC, SizeLabs, GradeAxisTitle, NL, SizeAxisTitle, NL1)

End Sub

Sub THREE_D_BARCHART(P() As Single, Pmax As Single, NX As Integer, NY As Integer, SizeLabs() As String, GradeAxisTitle As String, NL As Integer, SizeAxisTitle As String, NL1 As Integer)
' ***************************************************************************************************************************************************************************************
' Plots a 3-D barchart.
  Dim StartX As Integer, EndX As Integer, StepX As Integer, StartY As Integer, EndY As Integer, StepY
  Dim xspan As Single, YSPAN As Single, XMargin As Single, Ymargin As Single, XGap As Single, YGAP As Single
  Dim width As Single, depth As Single, height As Single, R As Single
  Dim XORG As Single, YORG As Single, ZORG As Single
  Dim X As Single, Y As Single
  Dim XLABEL(12) As String
  Dim LABELTEXT  As String
  Dim LabelWidth As Single
  Dim NLab As Integer
  Dim I As Integer, J As Integer

  XLABEL(1) = "    0%   "
  XLABEL(2) = "  0 - 10 "
  XLABEL(3) = " 10 - 20 "
  XLABEL(4) = " 20 - 30 "
  XLABEL(5) = " 30 - 40 "
  XLABEL(6) = " 40 - 50 "
  XLABEL(7) = " 50 - 60 "
  XLABEL(8) = " 60 - 70 "
  XLABEL(9) = " 70 - 80 "
  XLABEL(10) = " 80 - 90 "
  XLABEL(11) = " 90 -100 "
  XLABEL(12) = "   100%  "

  Distance = 500#
  Theta = 55 '120 '200
  PHI = 120
  Theta = Theta * Pi / 180#
  PHI = PHI * Pi / 180#
  xspan = 500
  XMargin = 100
  Ymargin = 80
  YSPAN = NY * (xspan - 2 * XMargin) \ NX + 2 * Ymargin
  XGap = 0.02 * xspan
  YGAP = 1.1 * XGap
  width = (xspan - 2 * XMargin) / NX
  depth = (YSPAN - 2 * Ymargin) / NY
  width = width - XGap
  depth = depth - YGAP
  height = 260
  R = Sqr((0.5 * xspan) ^ 2 + (0.5 * YSPAN) ^ 2)
  XORG = 0.5 * R * Sin(Theta + 0.25 * Pi) - 140
  YORG = 0.5 * R * Cos(Theta - 0.25 * Pi) - 300
  ZORG = -300 '-120 '0
  Page(PageNumber).XORG = XORG
  Page(PageNumber).YORG = YORG
  Page(PageNumber).SPAN = xspan
  Page(PageNumber).RANGE = YSPAN
  Call Set3DOrigin(300, 400, 0)

'write(LABELTEXT,'(''XORG '',F5.0,'' YORG'', F5.0)')XORG,YORG
'  CALL MGMOVE(50,650)
'  CALL LABELC(LABELTEXT,20)

If CHPAGE() > 0 Then
  Call VIEWPLANE(Distance, Theta, PHI)
  Call PSLNWD(0.2)
  Call MGGRAYFILL(0.8)
  Call PEDESTAL(XORG, YORG, ZORG, NX, NY, width, depth, 60#, XMargin, Ymargin, XGap, YGAP)
  Call LBSIZE(14, 26)
  LabelWidth = 6# * NL
  Call MGTOPWLAB(XORG + 0.5 * xspan + LabelWidth, YORG + YSPAN, ZORG - 40, GradeAxisTitle, NL, 1, Theta, PHI)
  Call MGBOTTOMWLAB(XORG + 0.5 * xspan - LabelWidth, YORG, ZORG - 40, GradeAxisTitle, NL, 1, Theta, PHI)
  LabelWidth = 6# * NL1
  Call MGRIGHTWLAB(XORG + xspan, YORG + 0.5 * YSPAN - LabelWidth, ZORG - 40, SizeAxisTitle, NL1, 1, Theta, PHI)
  Call MGLEFTWLAB(XORG, YORG + 0.5 * YSPAN + LabelWidth, ZORG - 40, SizeAxisTitle, NL1, 1, Theta, PHI)
  Call MGGRAYFILL(0.95)
  Call LBSIZE(9, 16)

'Draw the axis labels
  X = XORG + XMargin + 0.5 * XGap
  For J = 1 To NY
    Y = YORG + Ymargin + 0.5 * YGAP + (NY - J) * (depth + YGAP)
    NLab = Len(SizeLabs(J))
    Call MGFLOORLAB(XORG + 5, Y - 0.3 * YGAP, ZORG, SizeLabs(J), NLab, 1, Theta, PHI)
    Call MGFLOORLAB(XORG + xspan - XMargin + 5, Y - 0.3 * YGAP, ZORG, SizeLabs(J), NLab, 1, Theta, PHI)
  Next J
  For I = 1 To NX
    X = XORG + XMargin + 0.8 * XGap + (I - 1) * (width + XGap)
    Call MGFLOORLAB(X + 0.9 * XGap, YORG + 10, ZORG, XLABEL(I), 9, 2, Theta, PHI)
    Call MGFLOORLAB(X + 0.9 * XGap, YORG + YSPAN - Ymargin, ZORG, XLABEL(I), 9, 2, Theta, PHI)
  Next I

  'Draw the scale posts
  If Pmax <= 0.2 Then
    Pmax = 0.2
  ElseIf Pmax <= 0.4 Then
    Pmax = 0.4
  ElseIf Pmax <= 0.6 Then
    Pmax = 0.6
  ElseIf Pmax <= 0.8 Then
    Pmax = 0.8
  Else
    Pmax = 1#
  End If
  Call ScalePost(XORG, YORG, ZORG, xspan, YSPAN, Pmax, height)

'Draw the bars
  If Theta > 0.5 * Pi And Theta <= 1.5 * Pi Then
    StartX = 1
    EndX = NX
    StepX = 1
  Else
    StartX = NX
    EndX = 1
    StepX = -1
  End If

  If (Theta > 0# And Theta <= Pi) Then
    StartY = 1
    EndY = NY
    StepY = 1
  Else
    StartY = NY
    EndY = 1
    StepY = -1
  End If

  For I = StartX To EndX Step StepX
    X = XORG + XMargin + 0.5 * XGap + (I - 1) * (width + XGap)
    For J = StartY To EndY Step StepY
      If J < 10 Then
        Call MGGRAYFILL(0.95)   'CALL MGGRAYFILL(1.0)
      Else
        Call MGGRAYFILL(1#)
      End If
      If P(I, J) <= 0# Then Call MGGRAYFILL(0.9)
      Y = YORG + Ymargin + 0.5 * YGAP + (NY - J) * (depth + YGAP)
      Call THREED_BAR(X, Y, ZORG, width, depth, P(I, J) * height)
    Next
  Next
  Call GROUT
  Call HDCOPY(1#)
End If
End Sub


Sub THREED_BAR(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
' *********************************************************************************************************
' Sets the 3-D bar at world coordinate X Y Z

' Draw the visible faces.
  If (Theta > 0.5 * Pi And Theta <= 1.5 * Pi) Then
    Call RIGHTFACE(X, Y, Z, width, depth, height)
  Else
    Call LEFTFACE(X, Y, Z, width, depth, height)
  End If
  If (Theta > 0# And Theta <= Pi) Then
    Call BOTTOMFACE(X, Y, Z, width, depth, height)
  Else
    Call TOPFACE(X, Y, Z, width, depth, height)
  End If
  Call BARTOP(X, Y, Z, width, depth, height)
End Sub


Sub TOPFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
' ******************************************************************************************************
' Construct the top face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X, Y + d, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
End Sub

Sub BOTTOMFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
' *********************************************************************************************************
' Construct the bottom face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
End Sub

Sub LEFTFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'     ***************************************************************************************************
' Construct the left hand face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
End Sub

Sub RIGHTFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
' ******************************************************************************************************
' Construct the right hand face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X + w, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
End Sub

Sub BARTOP(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
' ***************************************************************************************************
' Construct the roof face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
End Sub

Sub PEDESTAL(X As Single, Y As Single, Z As Single, NX As Integer, NY As Integer, w As Single, d As Single, H As Single, XMargin As Single, Ymargin As Single, XGap As Single, YGAP As Single)
' **********************************************************************************************************************************************************************************************
' Draw the pedestal for the 3-D bar chart
  Dim width As Single, depth As Single
  Dim XT As Single, YT As Single
  Dim I As Integer
  width = XMargin + NX * (w + XGap) + XMargin
  depth = Ymargin + NY * (d + YGAP) + Ymargin
  Call THREED_BAR(X, Y, Z - H, width, depth, H)
' Draw the tick marks
  Call MG3DMOVE(X + XMargin, Y + Ymargin, Z)
  Call MG3DPLOT(X + width - XMargin, Y + Ymargin, Z)
  Call MG3DPLOT(X + width - XMargin, Y + depth - Ymargin, Z)
  Call MG3DPLOT(X + XMargin, Y + depth - Ymargin, Z)
  Call MG3DPLOT(X + XMargin, Y + Ymargin, Z)
  For I = 1 To NX - 1
    XT = X + XMargin + I * (w + XGap)
    Call X_TICK("+", XT, Y + Ymargin, Z, 0.5 * YGAP)
    Call X_TICK("-", XT, Y + depth - Ymargin, Z, 0.5 * YGAP)
  Next I
  For I = 1 To NY - 1
    YT = Y + Ymargin + I * (d + YGAP)
    Call Y_TICK("+", X + XMargin, YT, Z, 0.5 * XGap)
    Call Y_TICK("-", X + width - XMargin, YT, Z, 0.5 * XGap)
  Next I
End Sub

Sub X_TICK(ORIENT As String, X As Single, Y As Single, Z As Single, Length As Single)
' ************************************************************************************
' Draws a tick on the x-axis
  Call MG3DMOVE(X, Y, Z)
  If ORIENT = "+" Then
    Call MG3DPLOT(X, Y + Length, Z)
  Else
    Call MG3DPLOT(X, Y - Length, Z)
  End If
End Sub

Sub Y_TICK(ORIENT As String, X As Single, Y As Single, Z As Single, Length As Single)
' *************************************************************************************
' Draws a tick on the y-axis
  Call MG3DMOVE(X, Y, Z)
  If ORIENT = "+" Then
    Call MG3DPLOT(X + Length, Y, Z)
  Else
    Call MG3DPLOT(X - Length, Y, Z)
  End If
End Sub

Sub ScalePost(XORG As Single, YORG As Single, ZORG As Single, xspan As Single, YSPAN As Single, Pmax As Single, height As Single)
'********************************************************************************************************************************
  Dim PostLabel As String
  Dim upper As Single, Fract As Single

  Call LNTYPE(0)
  Call LINEWD(0.1)
  Call COLOUR(7)

   upper = Pmax * height
  'Top thread and posts first.
  If Theta > Pi And Theta < 2 * Pi Then
    Call ThreeDLINE(XORG, YORG, ZORG + upper, XORG + xspan, YORG, ZORG + upper)
    Call ThreeDLINE(XORG, YORG, ZORG + 0#, XORG, YORG, ZORG + upper)
    Call MGTOPWLAB(XORG, YORG, ZORG + upper, "TOPW", 4, 1, Theta, PHI)
  End If
  If Theta > 1.25 * Pi Or Theta < 0.25 * Pi Then
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + upper, XORG + xspan, YORG + YSPAN, ZORG + upper)
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + 0#, XORG, YORG + YSPAN, ZORG + upper)
    Call MGRIGHTWLAB(XORG + xspan, YORG, ZORG + upper, "RIGW", 4, 1, Theta, PHI)
  End If
  If (Theta > 1.5 * Pi Or Theta < 0.5 * Pi) Then
    For Fract = 0.2 To Pmax + 0.0001 Step 0.2
      Call ThreeDLINE(XORG + xspan, YORG + YSPAN, ZORG + Fract * upper, XORG, YORG + YSPAN, ZORG + Fract * upper)
      PostLabel = Format(Fract, "#0.00")
      Call MGBOTTOMWLAB(XORG + 10, YORG + YSPAN + 5, ZORG + Fract * upper, PostLabel, 4, 1, Theta, PHI)
    Next
    Call MGBOTTOMWLAB(XORG - 5, YORG + YSPAN, ZORG + 10#, "Fraction in class", 17, 2, Theta, PHI)
    Call ThreeDLINE(XORG + xspan, YORG + YSPAN, ZORG + 0#, XORG + xspan, YORG + YSPAN, ZORG + Pmax * upper)
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + 0#, XORG, YORG + YSPAN, ZORG + Pmax * upper)
  End If
  If Theta > 1.75 * Pi Or Theta < 0.75 * Pi Then
    For Fract = 0.2 To Pmax + 0.0001 Step 0.2
      Call ThreeDLINE(XORG + xspan, YORG, ZORG + Fract * upper, XORG + xspan, YORG + YSPAN, ZORG + Fract * upper)
      PostLabel = Format(Fract, "#0.00")
      Call MGLEFTWLAB(XORG + xspan, YORG + YSPAN - 10, ZORG + Fract * upper, PostLabel, 4, 1, Theta, PHI)
    Next
    Call ThreeDLINE(XORG + xspan, YORG, ZORG + 0#, XORG + xspan, YORG, ZORG + Pmax * upper)
  End If
  Call RESLWD
  Call COLOUR(0)
  Call LNTYPE(0)

  End Sub
