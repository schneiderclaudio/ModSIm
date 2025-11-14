Attribute VB_Name = "mdlBrFuncRegression"
Option Explicit
Public T10 As Single, T2 As Single, D50 As Single, D632 As Single, D90 As Single
Public FirstTime As Boolean, SecondTime As Boolean

Sub RightButtonProc(Which As String, X As Single, Y As Single)
'*************************************************************
  If Which = "Down" Then
  ElseIf Which = "Up" Then
    If REGModel = "Weibull" Then
      If FirstTime Then
        FirstTime = False
        MsgBox "Right click on the approximate 63.2% point"
        SecondTime = True
        D50 = X
      ElseIf SecondTime Then
        D632 = X
        SecondTime = False
        Unload MGPage(PageNumber)
        'Call ParameterEstimator
      End If
    ElseIf REGModel = "Logistic" Then
      If FirstTime Then
        FirstTime = False
        MsgBox "Right click on the approximate 90% point"
        D50 = X
        SecondTime = True
      ElseIf SecondTime Then
        D90 = X
        SecondTime = False
        Unload MGPage(PageNumber)
        'Call ParameterEstimator
      End If
    ElseIf REGModel = "LogNormal" Then
      If FirstTime Then
        FirstTime = False
        MsgBox "Right click on the approximate 90% point"
        D50 = X
        SecondTime = True
      ElseIf SecondTime Then
        D90 = X
        SecondTime = False
        Unload MGPage(PageNumber)
        'Call ParameterEstimator
      End If
    ElseIf REGModel = "RR-BrFunc" Or REGModel = "LogisticBrFunc" Then
      If FirstTime Then
        T10 = Y
        FirstTime = False
        SecondTime = False
        Unload MGPage(PageNumber)
        Call BrFuncParameterEstimator
      End If
    ElseIf REGModel = "LognormalBrFunc" Then
      If FirstTime Then
        FirstTime = False
        MsgBox "Right click on the approximate t2 point"
        T10 = Y
        SecondTime = True
      ElseIf SecondTime Then
        T2 = Y
        SecondTime = False
        Unload MGPage(PageNumber)
        Call BrFuncParameterEstimator
      End If
    End If
  End If
End Sub

Sub BrFuncParameterEstimator()
'*****************************
  Dim NumberOfParameters As Long
  Dim I As Integer, N1 As Integer, N2 As Integer, NN As Integer
  Dim NumberOfExpVars As Integer
  Dim Values1(1000) As Single, BrFunc(1000) As Single, Y(1000) As Single
  Dim Parameters(1 To 10) As Double
  Dim LowerConflimit(1 To 10) As Single, UpperConfLimit(1 To 10) As Single
  Dim ConfLevel As Single
  Dim SofSQ As Single
  Dim WS As Single
  
  On Error GoTo ErrHandler
  If REGModel = "RR-BrFunc" Then
    NumberOfParameters = 2
    Parameters(1) = T10
    Parameters(2) = 0.75
  ElseIf REGModel = "LogisticBrFunc" Then
    NumberOfParameters = 2
    Parameters(1) = T10
    Parameters(2) = 0.75
  ElseIf REGModel = "LognormalBrFunc" Then
    NumberOfParameters = 2
    Parameters(1) = T10
    Parameters(2) = Log(9) / (GAUINV(100 * T2) - GAUINV(100 * T10))
  End If
  'MsgBox "Parameters " & CStr(Parameters(1)) & "  " & CStr(Parameters(2))
  
  'Write the data file for the regression DLL
  N1 = Val(BrFuncRegression.txtNumber.text)
  For I = 1 To N1
    Values1(I) = Val(BrFuncRegression.txtMeshSize(I - 1).text) / Val(BrFuncRegression.txtParentSize)
  Next I
  For I = 1 To N1
    BrFunc(I) = 0.01 * Val(BrFuncRegression.txtPercentPassing(I - 1).text)
    Y(I) = Log(BrFunc(I))
  Next I
  
  Open RTrim(JobPath) & "LevMarREG.DAT" For Output As #32
  Print #32, BrFuncRegression.txtDataSetID.text
  Print #32, REGModel
  NumberOfExpVars = 1
  Print #32, NumberOfExpVars, N1
  For I = 1 To N1
    Print #32, Values1(I), Y(I)
  Next I
  Close (32)
  
  'Get required confidence level.
  ConfLevel = Val(BrFuncRegression.cboConfLevel.text)

  'Call the Levenberg-Marquard routine in the DLL.
  Screen.MousePointer = 11
  Call LevMarREG(JobPath, NumberOfParameters, Parameters(1), LowerConflimit(1), UpperConfLimit(1), ConfLevel)
  Screen.MousePointer = 0
  
  'Display the results
  For I = 1 To NumberOfParameters
    BrFuncRegression.txtParameterNumber(I - 1).text = Format(I, "#0")
    BrFuncRegression.txtParameterNumber(I - 1).Visible = True
    BrFuncRegression.txtEstimate(I - 1).text = Format(Parameters(I), "0.000E-#0")
    BrFuncRegression.txtEstimate(I - 1).Visible = True
    BrFuncRegression.txtLowerConfLim(I - 1).text = Format(LowerConflimit(I), "0.00E-#0")
    BrFuncRegression.txtLowerConfLim(I - 1).Visible = True
    BrFuncRegression.txtUpperConfLim(I - 1).text = Format(UpperConfLimit(I), "0.00E-#0")
    BrFuncRegression.txtUpperConfLim(I - 1).Visible = True
  Next I
'  For I = NumberOfParameters To 9
'    BrFuncRegression.txtParameterNumber(I).Visible = False
'    BrFuncRegression.txtEstimate(I).Visible = False
'    BrFuncRegression.txtLowerConfLim(I).Visible = False
'    BrFuncRegression.txtUpperConfLim(I).Visible = False
'  Next I

  'Calculate the model response
  SofSQ = 0
  NN = N1
  For I = 1 To N1
    If REGModel = "RR-BrFunc" Then
      If Values1(I) >= 1 Then
        Y(I) = 1
      Else
        Y(I) = 1 - (1 - Parameters(1)) ^ ((9 * Values1(I) / (1 - Values1(I))) ^ Parameters(2))
      End If
    ElseIf REGModel = "LogisticBrFunc" Then
      If Values1(I) >= 1 Then
        Y(I) = 1
      Else
        WS = ((1 - Values1(I)) / (9 * Values1(I))) ^ Parameters(2)
        Y(I) = 1 / (1 + WS * (1 - Parameters(1)) / Parameters(1))
      End If
    ElseIf REGModel = "LognormalBrFunc" Then
      If Values1(I) >= 1 Then
        Y(I) = 1
      Else
        WS = Log(9 * Values1(I) / (1 - Values1(I))) / Parameters(2) + GAUINV(100 * Parameters(1))
        Y(I) = 0.01 * GAUSPROB(WS)
      End If
    End If
    SofSQ = SofSQ + (BrFunc(I) - Y(I)) ^ 2
  Next I
  BrFuncRegression.txtSumOfSquares.text = Format(SofSQ, "0.000e-0")
  
  'Plot the model against the data
  If CHPAGE() = 0 Then Exit Sub
  Call BBOX(100, 100, 600, 500)
  Call LGAXIS(0.003, 1#, 1, "Y", "Y")
  Call MGLABAX("Relative particle size  D/d`p`'", 13, 1)
  Call LGAXIS(0.001, 1#, 2, "Y", "Y")
  Call MGLABAX("Breakage function", 13, 2)
  Call DISPNT(Values1, BrFunc, N1, 1, "ALN", "ALN")
  Call PLGRPH(Values1, Y, N1, "ALN", "ALN")
  
  Call MGMOVE(100, 650)
  Call MGLABEL(BrFuncRegression.txtDataSetID)
  Call GROUTCOORD

  Exit Sub
  
ErrHandler:
  MsgBox "Error during least squares calculation", vbCritical, "ERROR"
End Sub

