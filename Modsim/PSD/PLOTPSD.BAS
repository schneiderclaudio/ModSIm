Attribute VB_Name = "PlotPsd"
Option Explicit
Public DATA_SET_NAME(50) As String
Public SET_ID(30) As Integer

Sub PlotPSDGraph()
'*****************
'Plot the data in the selected coordinate system.
Dim N_POINTS(30) As Integer, SELECTED_ID(30) As Integer
Dim SMESH(0 To 25, 30) As Single, FR(0 To 25, 30) As Single
Dim X(1 To 25) As Single, Y(1 To 25) As Single
Dim Label As String, SIZE_LABEL(0 To 6) As String
Dim i As Integer, N As Integer, NN As Integer, J As Integer
Dim WSMESH As Single, WFR As String
Dim SIZE_MIN As Single, SIZE_MAX As Single, CUM_MIN As Single, CUM_MAX As Single
Dim XOL As Single, XFL As Single, YOL As Single, YFL As Single
Dim XO As Single, XF As Single, NDECX As Integer
Dim YO As Single, YF As Single, NDECY As Integer
Dim IYPOS As Integer
Dim WSTRING As String

On Error GoTo ErrHandler
SIZE_LABEL(0) = "  m   "
SIZE_LABEL(1) = "  dm  "
SIZE_LABEL(2) = "  cm  "
SIZE_LABEL(3) = "  mm  "
SIZE_LABEL(4) = "      "
SIZE_LABEL(5) = "      "
SIZE_LABEL(6) = "micron"

' Get the parameters to plot the graphs
  Label = SIZE_LABEL(SetupData.SizeCode)
  For i = 1 To SetupData.Number
    SET_ID(i) = SetupData.GraphList(i)
    N_POINTS(i) = SetupData.GraphData(i).NDC - 1
    DATA_SET_NAME(i) = SetupData.GraphData(i).SET_NAME
    NN = 0
    For N = 1 To N_POINTS(i)
      NN = NN + 1
      WSMESH = SetupData.GraphData(i).CUMSIZE(N)
      WFR = SetupData.GraphData(i).fraction(N)
      'Dont record values larger than 0.999 for RR Log-normal or Logistic nor values <= 0
      If (SetupData.CoordinateCode >= 4 And WFR > 0.999) Or (WFR <= 0#) Then
        NN = NN - 1
      Else
        SMESH(NN, i) = WSMESH * 10 ^ SetupData.SizeCode
        FR(NN, i) = WFR * 100
      End If
    Next N
    N_POINTS(i) = NN
  Next i
  
  'Get the upper and lower values of the ranges
  SIZE_MIN = 1E+20
  SIZE_MAX = 0#
  CUM_MIN = 1E+20
  CUM_MAX = 0#
  For i = 1 To SetupData.Number
    If (SMESH(N_POINTS(i), i) < SIZE_MIN) Then
      SIZE_MIN = SMESH(N_POINTS(i), i)
    End If
    If (SMESH(1, i) > SIZE_MAX) Then
      SIZE_MAX = SMESH(1, i)
    End If
    If (FR(N_POINTS(i), i) < CUM_MIN) Then
      CUM_MIN = FR(N_POINTS(i), i)
    End If
    If (FR(1, i) > CUM_MAX) Then
      CUM_MAX = FR(1, i)
    End If
  Next i
  
  If SetupData.CoordinateCode = 1 Then
    XOL = 0#
    XFL = ULIM(SIZE_MAX)
    If (SetupData.TruncationCode = 1) Then
      If (XFL >= SetupData.TruncationSize) Then
        XFL = 0.99 * SetupData.TruncationSize
      End If
    End If
  Else
    Call LOGLIM(SIZE_MIN, SIZE_MAX, XO, XF, NDECX)
    If (SetupData.TruncationCode = 1) And (XF >= SetupData.TruncationSize) Then
      XF = 0.95 * SetupData.TruncationSize
    End If
  End If
  
  YOL = 0#
  YFL = 100#
  Call LOGLIM(CUM_MIN, CUM_MAX, YO, YF, NDECY)
  IYPOS = 590

  If CHPAGE() = 0 Then Exit Sub
  Select Case (SetupData.CoordinateCode)

    'DRAW ON LINEAR LINEAR COORDINATES
    Case (1)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LINAX(XOL, XFL, 10, 5.3, 1, "Y", "Y")
      Else
        Call TRUNCLINAX(XOL, XFL, 10, SetupData.TruncationSize, 5.2, 1, "Y", "Y")
      End If
    Call LABAX("Particle size", 13, Label, 6, 13, 1)
    Call LINAX(YOL, YFL, 10, 3#, 2, "Y", "Y")
    Call LABAX("Percent passing", 15, " ", 1, 13, 2)
    Call LBSIZE(10, 20)
    For i = 1 To SetupData.Number
      For J = 1 To N_POINTS(i)
        X(J) = SMESH(J, i)
        Y(J) = FR(J, i)
      Next J
      Call CLIPBOX
      Select Case (SetupData.LineCode)
        Case (1)
        If (SetupData.TruncationCode = 0) Then
          Call PLGRPH(X, Y, N_POINTS(i), "LINTR", "LINTR")
        Else
          Call PLGRPH(X, Y, N_POINTS(i), "TRUNCLIN", "LINTR")
        End If
        Case (2)
        If (SetupData.TruncationCode = 0) Then
          Call PLSMGR(X, Y, N_POINTS(i), 0, "LINTR", "LINTR")
        Else
          Call PLSMGR(X, Y, N_POINTS(i), 0, "TRUNCLIN", "LINTR")
        End If
      End Select
      If (SetupData.TruncationCode = 0) Then
        Call DISPNT(X, Y, N_POINTS(i), i + 20, "LINTR", "LINTR")
      Else
       Call DISPNT(X, Y, N_POINTS(i), i + 20, "TRUNCLIN", "LINTR")
      End If
      Call CLIPOFF
      Call IDENT(i, IYPOS)
    Next i
    Call MGMOVE(100, 650)
    If (SetupData.TruncationCode = 0) Then
      Call LABELC("Linear-linear coordinate system", 31)
    Else
      Call LABELC("Truncated linear-linear coordinate system", 41)
      Call MGMOVE(100, 630)
      Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
    End If
    Call GROUTCOORD
    Call HDCOPY(1#)

    'DRAW ON LINEAR-LOG SetUpData.CoordinateCode
    Case (2)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LGAXIS(XO, XF, 1, "Y", "Y")
      Else
        Call TRUNCLOGAX(XO, XF, SetupData.TruncationSize, 1, "Y", "Y")
      End If
      Call LABAX("Particle size", 13, Label, 6, 13, 1)
      Call LINAX(YOL, YFL, 10, 3#, 2, "Y", "Y")
      Call LABAX("Percent passing", 15, " ", 1, 13, 2)
      Call LBSIZE(10, 20)
      For i = 1 To SetupData.Number
        For J = 1 To N_POINTS(i)
          X(J) = SMESH(J, i)
          Y(J) = FR(J, i)
        Next J
        Call CLIPBOX
        Select Case (SetupData.LineCode)
          Case (1)
          If (SetupData.TruncationCode = 0) Then
            Call PLGRPH(X, Y, N_POINTS(i), "ALN", "LINTR")
          Else
            Call PLGRPH(X, Y, N_POINTS(i), "TRUNCLOG", "LINTR")
          End If
          Case (2)
          If (SetupData.TruncationCode = 0) Then
            Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "LINTR")
          Else
            Call PLSMGR(X, Y, N_POINTS(i), 0, "TRUNCLOG", "LINTR")
          End If
        End Select
        If (SetupData.TruncationCode = 0) Then
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "LINTR")
        Else
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "TRUNCLOG", "LINTR")
        End If
        Call CLIPOFF
        Call IDENT(i, IYPOS)
      Next i
      Call MGMOVE(100, 650)
      If (SetupData.TruncationCode = 0) Then
        Call LABELC("Linear-log coordinate system", 28)
      Else
        Call LABELC("Truncated linear-log coordinate system", 38)
        Call MGMOVE(100, 630)
        Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
      End If
      Call GROUTCOORD
      Call HDCOPY(1#)

    'DRAW ON LOG-LOG COORDINATES
    Case (3)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LGAXIS(XO, XF, 1, "Y", "Y")
      Else
        Call TRUNCLOGAX(XO, XF, SetupData.TruncationSize, 1, "Y", "Y")
      End If
      Call LABAX("Particle size", 13, Label, 6, 13, 1)
      Call LGAXIS(YO, YF, 2, "Y", "Y")
      Call LABAX("Percent passing", 15, " ", 1, 13, 2)
      Call LBSIZE(10, 20)
      For i = 1 To SetupData.Number
        For J = 1 To N_POINTS(i)
          X(J) = SMESH(J, i)
          Y(J) = FR(J, i)
        Next J
        Call CLIPBOX
        If SetupData.TruncationCode = 0 Then
          Select Case (SetupData.LineCode)
          Case (1)
          Call PLGRPH(X, Y, N_POINTS(i), "ALN", "ALN")
          Case (2)
          Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "ALN")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "ALN")
        Else
          Select Case (SetupData.LineCode)
          Case (1)
          Call PLGRPH(X, Y, N_POINTS(i), "TRUNCLOG", "ALN")
          Case (2)
          Call PLSMGR(X, Y, N_POINTS(i), 0, "TRUNCLOG", "ALN")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "TRUNCLOG", "ALN")
        End If
        Call CLIPOFF
        Call IDENT(i, IYPOS)
      Next i
      Call MGMOVE(100, 650)
      If SetupData.TruncationCode = 0 Then
        Call LABELC("Log-log coordinate system", 25)
      Else
        Call MGLABEL("Truncated log-log coordinate system")
        Call MGMOVE(100, 620)
        Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
      End If
      Call GROUTCOORD
      Call HDCOPY(1#)
      
    'DRAW ON ROSIN-RAMMLER COORDINATES
    Case (4)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LGAXIS(XO, XF, 1, "Y", "Y")
      Else
        Call TRUNCLOGAX(XO, XF, SetupData.TruncationSize, 1, "Y", "Y")
      End If
      Call LABAX("Particle size", 13, Label, 6, 13, 1)
      Call RRAXIS(YO, YF, 2, "Y", "Y")
      Call LABAX("Percent passing", 15, " ", 1, 13, 2)
      Call LBSIZE(10, 20)
      For i = 1 To SetupData.Number
        For J = 1 To N_POINTS(i)
          X(J) = SMESH(J, i)
          Y(J) = FR(J, i)
        Next J
        Call CLIPBOX
        If SetupData.TruncationCode = 0 Then
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "ALN", "LLRC")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "LLRC")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "LLRC")
        Else
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "TRUNCLOG", "LLRC")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "TRUNCLOG", "LLRC")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "TRUNCLOG", "LLRC")
        End If
        Call CLIPOFF
        Call IDENT(i, IYPOS)
      Next i
      Call MGMOVE(100, 650)
      If SetupData.TruncationCode = 0 Then
        Call LABELC("Rosin-Rammler coordinate system", 25)
      Else
        Call MGLABEL("Truncated Rosin-Rammler coordinate system")
        Call MGMOVE(100, 620)
        Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
      End If
      Call GROUTCOORD
      Call HDCOPY(1#)

    'DRAW ON LOG-NORMAL SetUpData.CoordinateCode
    Case (5)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LGAXIS(XO, XF, 1, "Y", "Y")
      Else
        Call TRUNCLOGAX(XO, XF, SetupData.TruncationSize, 1, "Y", "Y")
      End If
      Call LABAX("Particle size", 13, Label, 6, 13, 1)
      Call PROBAX(YO, YF, 2, "Y", "Y")
      Call LABAX("Percent passing", 15, " ", 1, 13, 2)
      Call LBSIZE(10, 20)
      For i = 1 To SetupData.Number
        For J = 1 To N_POINTS(i)
          X(J) = SMESH(J, i)
          Y(J) = FR(J, i)
        Next J
        Call CLIPBOX
        If SetupData.TruncationCode = 0 Then
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "ALN", "GAUINV")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "GAUINV")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "GAUINV")
        Else
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "TRUNCLOG", "GAUINV")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "TRUNCLOG", "GAUINV")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "TRUNCLOG", "GAUINV")
        End If
        Call CLIPOFF
        Call IDENT(i, IYPOS)
      Next i
      Call MGMOVE(100, 650)
      If SetupData.TruncationCode = 0 Then
        Call LABELC("Log-normal coordinate system", 25)
      Else
        Call MGLABEL("Truncated log-normal coordinate system")
        Call MGMOVE(100, 620)
        Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
      End If
      Call GROUTCOORD
      Call HDCOPY(1#)

    'DRAW ON LOGISTIC SetUpData.CoordinateCode
    Case (6)
      Call BBOX(150, 100, 500, 500)
      If (SetupData.TruncationCode = 0) Then
        Call LGAXIS(XO, XF, 1, "Y", "Y")
      Else
        Call TRUNCLOGAX(XO, XF, SetupData.TruncationSize, 1, "Y", "Y")
      End If
      Call LABAX("Particle size", 13, Label, 6, 13, 1)
      Call GISAXIS(YO, YF, 2, "Y", "Y")
      Call LABAX("Percent passing", 15, " ", 1, 13, 2)
      Call LBSIZE(10, 20)
      For i = 1 To SetupData.Number
        For J = 1 To N_POINTS(i)
          X(J) = SMESH(J, i)
          Y(J) = FR(J, i)
        Next J
        Call CLIPBOX
        If SetupData.TruncationCode Then
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "ALN", "LOGIST")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "LOGIST")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "LOGIST")
        Else
          Select Case (SetupData.LineCode)
            Case (1)
            Call PLGRPH(X, Y, N_POINTS(i), "ALN", "LOGIST")
            Case (2)
            Call PLSMGR(X, Y, N_POINTS(i), 0, "ALN", "LOGIST")
          End Select
          Call DISPNT(X, Y, N_POINTS(i), i + 20, "ALN", "LOGIST")
        End If
        Call CLIPOFF
        Call IDENT(i, IYPOS)
      Next i
      Call MGMOVE(100, 650)
      If SetupData.TruncationCode = 0 Then
        Call LABELC("Logistic coordinate system", 25)
      Else
        Call MGLABEL("Truncated logistic coordinate system")
        Call MGMOVE(100, 620)
        Call MGLABEL("Truncation size " & CStr(SetupData.TruncationSize) & Label)
      End If
      Call GROUTCOORD
      Call HDCOPY(1#)

    End Select
  Exit Sub
ErrHandler:
  MsgBox "Error when plotting size distributions", vbCritical, "ERROR"
  Exit Sub
  
End Sub

Sub IDENT(i As Integer, IYPOS As Integer)
'****************************************
' Set the identification labels
  Call SYMBOL(670, IYPOS, i + 20)
  Call MGMOVE(685, IYPOS - 5)
  Call MGNUMLAB(SET_ID(i))
  Call LABELC(DATA_SET_NAME(i), 30)
  IYPOS = IYPOS - 25
End Sub

