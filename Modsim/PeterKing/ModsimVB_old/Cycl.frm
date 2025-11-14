VERSION 5.00
Begin VB.Form Cycl 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Specify parameters for model CYCL"
   ClientHeight    =   5460
   ClientLeft      =   435
   ClientTop       =   1815
   ClientWidth     =   9480
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5460
   ScaleWidth      =   9480
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   12
      Left            =   8280
      TabIndex        =   44
      Top             =   3960
      Width           =   480
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   11
      Left            =   3000
      TabIndex        =   43
      Top             =   3960
      Width           =   480
   End
   Begin VB.Frame Frame6 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slurry viscosity"
      Height          =   720
      Left            =   3360
      TabIndex        =   41
      Top             =   3120
      Width           =   3735
      Begin VB.OptionButton OptViscosity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "c poise"
         Height          =   240
         Index           =   1
         Left            =   2640
         TabIndex        =   19
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Width           =   975
      End
      Begin VB.OptionButton OptViscosity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Pa s"
         Height          =   240
         Index           =   0
         Left            =   1560
         TabIndex        =   20
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   10
         Left            =   120
         TabIndex        =   18
         Top             =   360
         Width           =   1320
      End
      Begin VB.Label lblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Sluirry viscosity"
         Height          =   135
         Index           =   10
         Left            =   2160
         TabIndex        =   48
         Top             =   240
         Visible         =   0   'False
         Width           =   1455
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Head of feed slurry"
      Height          =   720
      Left            =   0
      TabIndex        =   30
      Top             =   3120
      Width           =   3015
      Begin VB.OptionButton optSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         Height          =   240
         Index           =   1
         Left            =   2160
         TabIndex        =   38
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Width           =   735
      End
      Begin VB.OptionButton optSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   0
         Left            =   1440
         TabIndex        =   37
         TabStop         =   0   'False
         ToolTipText     =   "C"
         Top             =   360
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   5
         Left            =   120
         TabIndex        =   36
         Top             =   360
         Width           =   1200
      End
      Begin VB.Label lblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Feed head"
         Height          =   135
         Index           =   5
         Left            =   1920
         TabIndex        =   47
         Top             =   240
         Visible         =   0   'False
         Width           =   735
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size specification by"
      Height          =   1680
      Left            =   0
      TabIndex        =   29
      Top             =   600
      Width           =   4560
      Begin VB.CommandButton CmdToggle 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "<- Convert to ->"
         Height          =   240
         Left            =   1080
         TabIndex        =   40
         ToolTipText     =   "Convert dimensions between two specifications"
         Top             =   240
         Width           =   1455
      End
      Begin VB.OptionButton optAbsolute 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Absolute"
         Height          =   240
         Left            =   2640
         TabIndex        =   2
         Tag             =   "Absolute"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
      Begin VB.OptionButton optRatio 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ratio"
         Height          =   240
         Left            =   120
         TabIndex        =   1
         TabStop         =   0   'False
         Tag             =   "Ratio"
         Top             =   240
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   2
         Left            =   2280
         TabIndex        =   4
         Top             =   840
         Width           =   1680
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   4
         Left            =   2280
         TabIndex        =   6
         Top             =   1320
         Width           =   1680
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   3
         Left            =   2280
         TabIndex        =   5
         Top             =   1080
         Width           =   1680
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   1
         Left            =   2280
         TabIndex        =   3
         Top             =   600
         Width           =   1680
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Vortex finder diameter"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   34
         Top             =   1080
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Spigot diameter"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   33
         Top             =   1320
         Width           =   1815
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Inlet diameter"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   32
         Top             =   840
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Vortex-spigot distance"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   31
         Top             =   600
         Width           =   2055
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   1560
      TabIndex        =   0
      Top             =   360
      Width           =   1080
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   6
      Left            =   2520
      TabIndex        =   14
      Top             =   4440
      Width           =   480
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Calibration factors"
      Height          =   1095
      Left            =   5040
      TabIndex        =   25
      Top             =   1920
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   9
         Left            =   2280
         TabIndex        =   17
         Top             =   720
         Width           =   1200
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   7
         Left            =   2280
         TabIndex        =   15
         Top             =   240
         Width           =   1200
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   220
         Index           =   8
         Left            =   2280
         TabIndex        =   16
         Top             =   480
         Width           =   1200
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "for the flow split"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   9
         Left            =   240
         TabIndex        =   28
         Top             =   720
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "for the sharpness index"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   8
         Left            =   240
         TabIndex        =   27
         Top             =   480
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "for d50"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   7
         Left            =   240
         TabIndex        =   26
         Top             =   240
         Width           =   1440
      End
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   7200
      TabIndex        =   22
      Top             =   4440
      Width           =   960
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   8400
      TabIndex        =   21
      Top             =   4440
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7560
      TabIndex        =   24
      Top             =   360
      Width           =   1680
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   13
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display the data set used inthe previous simulation"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display the new data that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Frame Frame5 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      Height          =   720
      Left            =   0
      TabIndex        =   35
      Top             =   2280
      Width           =   4560
      Begin VB.OptionButton optSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   7
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton optSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   3
         Left            =   3240
         TabIndex        =   10
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Width           =   1080
      End
      Begin VB.OptionButton optSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   2
         Left            =   2160
         TabIndex        =   9
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Width           =   720
      End
      Begin VB.OptionButton optSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   8
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   360
         Width           =   840
      End
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Factor for slurry density in separation zone (<1)"
      Height          =   255
      Index           =   12
      Left            =   4560
      TabIndex        =   46
      Top             =   3960
      Width           =   3615
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Index for variation of d50 with density"
      Height          =   255
      Index           =   11
      Left            =   120
      TabIndex        =   45
      Top             =   3960
      Width           =   2775
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   42
      Top             =   0
      Visible         =   0   'False
      Width           =   9495
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of cyclones in cluster"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   120
      TabIndex        =   39
      Top             =   4440
      Width           =   2295
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cyclone diameter"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   0
      TabIndex        =   23
      Top             =   360
      Width           =   1575
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "Cycl"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
  Call MakeNew(CURRMODELDATA(ID))
  Unload Cycl
End Sub

Private Sub CmdCancel_Click()
  Unload Cycl
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload Help  'In case help was called
  If FastParameterChange Then
    UnitMods.cmdQuit_Click
    FastParameterChange = False
  End If
End Sub

Private Sub CmdToggle_Click()
'****************************
  Dim Ws1 As Single, WS As Single
  Dim I As Integer
  Ws1 = Val(Parameter(0).text)
  If optRatio.Value = -1 Then
        For I = 1 To 4
          WS = Val(Parameter(I).text)
          Parameter(I).text = Str$(WS * Ws1)
        Next I
        optAbsolute.Value = -1
  ElseIf optAbsolute.Value = -1 Then
        For I = 1 To 4
          WS = Val(Parameter(I).text)
          Parameter(I).text = Str$(WS / Ws1)
        Next I
        optRatio.Value = -1
  End If
End Sub

Private Sub Form_Load()
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "CYCL" Then
        OptCurrent.Enabled = 0
        LoadDefault
        Call MakeNew(NEWDATA)
  Else
        Call LoadNew(CURRMODELDATA(ID))
        Call MakeNew(NEWDATA)
  End If
  Exit Sub

ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

Private Sub LoadDefault()
        Parameter(0).text = Str$(0.5)
        Parameter(1).text = Str$(3)
        Parameter(2).text = Str$(0.2)
        Parameter(3).text = Str$(0.167)
        Parameter(4).text = Str$(0.116)
        Parameter(5).text = Str$(3)
        Parameter(6).text = Str$(1)
        Parameter(7).text = Str$(1)
        Parameter(8).text = Str$(1)
        Parameter(9).text = Str$(1)
        Parameter(10).text = Str$(0.0012)
        Parameter(11).text = CStr(0.5)
        Parameter(12).text = CStr(0#)
                OptSize1(2).Value = -1
                optSize2(0).Value = -1
                OptViscosity(0).Value = -1
                optRatio.Value = -1
End Sub

Private Sub LoadNew(NDATA As ModelData)
  Dim I As Integer
  For I = 1 To NDATA.NOPAR
        Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  OptSize1(2).Value = -1
  optSize2(0).Value = -1
  optRatio.Value = -1
  OptViscosity(0).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
  Dim I As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "CYCL"
  NDATA.NOPAR = 13
  For I = 1 To NDATA.NOPAR
        NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the size to meters
  ScaleFactor = 1!
  If OptSize1(0).Value Then ScaleFactor = 0.001
  If OptSize1(1).Value Then ScaleFactor = 0.01
  If OptSize1(3).Value Then ScaleFactor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * ScaleFactor
  If optAbsolute.Value Then
        For I = 2 To 5
          NDATA.PARAM(I) = NDATA.PARAM(I) * ScaleFactor
          NDATA.PARAM(I) = NDATA.PARAM(I) / NDATA.PARAM(1)
        Next I
  End If
  If optSize2(1).Value Then NDATA.PARAM(6) = NDATA.PARAM(6) * 0.3048
  If OptViscosity(1).Value Then NDATA.PARAM(11) = NDATA.PARAM(11) * 0.001
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
PrintForm
End Sub

Private Sub optAbsolute_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        CHANGE_FLAG = -1
        If OptNew.Value = 0 Then OptNew.Value = -1
  End If
End Sub

Private Sub optAbsolute_GotFocus()
  Instruction.Visible = True
  Instruction.Caption = "Specify actual dimensions of the cyclone in appropriate units"
End Sub

Private Sub optAbsolute_LostFocus()
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptCurrent_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub optRatio_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optRatio_GotFocus()
  Instruction.Visible = True
  Instruction.Caption = "Specify the dimensions of the cyclone by ratio to the cyclone diameter"
End Sub

Private Sub optRatio_LostFocus()
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optSize1_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize1_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mm_cm_m_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize1(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "0.000E+#")
  If optAbsolute.Value Then
        For I = 1 To 4
          Parameter(I).text = Format(Val(Parameter(I).text) * Factor, "0.000E+#")
        Next I
  End If
  OptSize1(Index).Value = True
End If
End Sub

Private Sub optSize2_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize2_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize2_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
  Dim I As Integer
  Dim Factor As Double
  
  CFactor = m_feet
  If Button = vbRightButton Then
    'Find the currently set index
    For I = 0 To CFactor.Number - 1
      If optSize2(I).Value = True Then
        Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
      End If
    Next I
    Parameter(5).text = Format(Val(Parameter(5).text) * Factor, "0.000E+#")
    optSize2(Index).Value = True
  End If
End Sub

Private Sub OptViscosity_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optViscosity_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptViscosity_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = Pas_cp
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptViscosity(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(10).text = Format(Val(Parameter(10).text) * Factor, "0.000E+#")
  OptViscosity(Index).Value = True
End If
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
    'Check if this paprameter is already selected and display if necessary.
    For I = 1 To 5
      If UnitID(I) = ID Then
        If ParameterNumber(I) = Index + IDIFF Then
          Repeat.LstLevel.AddItem CStr(I), 0
          Repeat.LstLevel.ListIndex = 0
          Repeat.LstLevel.Enabled = False
          Repeat.TxtParameterName = ParameterName(I)
          Repeat.TxtValue(0) = CStr(StartValue(I))
          Repeat.TxtValue(1) = CStr(EndValue(I))
          Repeat.TxtValue(2) = CStr(StepValue(I))
        End If
      End If
    Next I
    Repeat.Show 1
    If Repeat.Level > 0 Then
      RepUnitNumber(Repeat.Level) = UnitMods.Unit
      UnitID(Repeat.Level) = ID
      ParameterNumber(Repeat.Level) = Index + IDIFF
    End If
End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub

