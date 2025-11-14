VERSION 5.00
Begin VB.Form GYRA 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model GYRA"
   ClientHeight    =   3585
   ClientLeft      =   315
   ClientTop       =   1815
   ClientWidth     =   8190
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
   ScaleHeight     =   3585
   ScaleWidth      =   8190
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   720
      Left            =   5760
      TabIndex        =   18
      Top             =   2280
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   720
      Left            =   6960
      TabIndex        =   17
      Top             =   2280
      Width           =   960
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Work index"
      Height          =   615
      Left            =   120
      TabIndex        =   21
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1080
      Width           =   4695
      Begin VB.OptionButton OptWI 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kWhr/shrt ton"
         Height          =   240
         Index           =   1
         Left            =   2880
         TabIndex        =   6
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   1695
      End
      Begin VB.OptionButton OptWI 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kWhr/tonne"
         Height          =   240
         Index           =   0
         Left            =   1440
         TabIndex        =   5
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   1455
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Width           =   1200
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   6360
      TabIndex        =   24
      Top             =   240
      Width           =   1695
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   16
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   15
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   14
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Open side setting"
      Height          =   615
      Left            =   120
      TabIndex        =   20
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   240
      Width           =   3495
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   2
         Left            =   2520
         TabIndex        =   3
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   0
         Left            =   1920
         TabIndex        =   2
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   1
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   120
         TabIndex        =   0
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.Frame FrameAS 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ore type"
      Height          =   855
      Left            =   1920
      TabIndex        =   22
      Top             =   1920
      Visible         =   0   'False
      Width           =   1560
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Slabby"
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   11
         Top             =   480
         Width           =   1095
      End
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Average"
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   10
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   1095
      End
   End
   Begin VB.Frame FrameASP 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ore type"
      Height          =   855
      Left            =   3720
      TabIndex        =   23
      Top             =   1920
      Visible         =   0   'False
      Width           =   1560
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Spongy"
         Height          =   240
         Index           =   6
         Left            =   120
         TabIndex        =   13
         Top             =   480
         Width           =   1095
      End
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Average"
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   1095
      End
   End
   Begin VB.Frame Framebts 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ore type"
      Height          =   1095
      Left            =   120
      TabIndex        =   19
      Top             =   1920
      Visible         =   0   'False
      Width           =   1575
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Slabby"
         Height          =   240
         Index           =   2
         Left            =   240
         TabIndex        =   9
         Top             =   720
         Width           =   1095
      End
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Tough"
         Height          =   240
         Index           =   1
         Left            =   240
         TabIndex        =   8
         TabStop         =   0   'False
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptOre_type 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Brittle"
         Height          =   240
         Index           =   0
         Left            =   240
         TabIndex        =   7
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Work index"
      Height          =   255
      Index           =   1
      Left            =   4920
      TabIndex        =   26
      Top             =   1320
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Open side setting"
      Height          =   255
      Index           =   0
      Left            =   3840
      TabIndex        =   25
      Top             =   480
      Visible         =   0   'False
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
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "GYRA"
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
  Unload GYRA
End Sub

Private Sub CmdCancel_Click()
  Unload GYRA
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

Private Sub Form_Load()
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "GYRA" Then
        OptCurrent.Enabled = 0
        LoadDefault
        Call MakeNew(NEWDATA)
        Call LoadNew(NEWDATA)
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
        Parameter(0).text = Str$(0.15)
        OptSize(0).Value = True
        OptOre_type(1).Value = -1
        Parameter(1).text = Str$(10.88)
        OptWI(0).Value = -1
        FrameASP.Visible = -1
        FrameAS.Visible = 0
        Framebts.Visible = 0
End Sub

Private Sub LoadNew(NDATA As ModelData)
          Parameter(0).text = CStr(NDATA.PARAM(1))
          OptSize(0).Value = True
          Parameter(1).text = CStr(NDATA.PARAM(3))
          OptWI(0).Value = -1
          If (NDATA.PARAM(3) < 11.03) Then
                FrameASP.Visible = -1
                FrameAS.Visible = 0
                Framebts.Visible = 0
                If Abs(NDATA.PARAM(2) - 0.85) <= 0.01 Then
                  OptOre_type(5).Value = -1
                Else
                  OptOre_type(6).Value = -1
                End If
          ElseIf NDATA.PARAM(3) < 14.33 Then
                FrameASP.Visible = 0
                FrameAS.Visible = -1
                Framebts.Visible = 0
                If Abs(NDATA.PARAM(2) - 0.82) <= 0.01 Then
                  OptOre_type(4).Value = -1
                Else
                  OptOre_type(3).Value = -1
                End If
          Else
                FrameASP.Visible = 0
                FrameAS.Visible = 0
                Framebts.Visible = -1
                If Abs(NDATA.PARAM(2) - 0.85) <= 0.01 Then
                  OptOre_type(0).Value = -1
                ElseIf Abs(NDATA.PARAM(2) - 0.75) <= 0.01 Then
                  OptOre_type(1).Value = -1
                Else
                  OptOre_type(2).Value = -1
                End If
          End If
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim Factor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "GYRA"
  NDATA.NOPAR = 3
  If OptSize(1).Value = True Then Factor = 0.01
  If OptSize(0).Value = True Then Factor = 1!
  If OptSize(2).Value = True Then Factor = 0.0254
  NDATA.PARAM(1) = Val(Parameter(0).text) * Factor
  If OptWI(0).Value = True Then Factor = 1!
  If OptWI(1).Value = True Then Factor = 1.1025
  NDATA.PARAM(3) = Val(Parameter(1).text) * Factor
  If NDATA.PARAM(3) < 11.03 Then
    If OptOre_type(6).Value = True Then NDATA.PARAM(2) = 0.82
    If OptOre_type(5).Value = True Then NDATA.PARAM(2) = 0.85
  ElseIf NDATA.PARAM(3) < 14.33 Then
    If OptOre_type(3).Value = True Then NDATA.PARAM(2) = 0.85
    If OptOre_type(4).Value = True Then NDATA.PARAM(2) = 0.82
  Else
    If OptOre_type(0).Value = True Then NDATA.PARAM(2) = 0.85
    If OptOre_type(1).Value = True Then NDATA.PARAM(2) = 0.75
    If OptOre_type(2).Value = True Then NDATA.PARAM(2) = 0.7
  End If
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

Private Sub optSize_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
  Dim I As Integer
  Dim Factor As Double
  
  CFactor = m_cm_inch
  If Button = vbRightButton Then
    'Find the currently set index
    For I = 0 To CFactor.Number - 1
      If OptSize(I).Value = True Then
        Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
      End If
    Next I
      Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "0.000E+#")
    OptSize(Index).Value = True
  End If
End Sub

Private Sub optWI_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optWI_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptWI_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
  Dim I As Integer
  Dim Factor As Double
  
  CFactor = kWhpert_kWhperst
  If Button = vbRightButton Then
    'Find the currently set index
    For I = 0 To CFactor.Number - 1
      If OptWI(I).Value = True Then
        Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
      End If
    Next I
      Parameter(1).text = Format(Val(Parameter(1).text) * Factor, "0.000E+#")
    OptWI(Index).Value = True
  End If
End Sub

Private Sub optOre_type_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optOre_type_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If KeyAscii = 13 Then
    If CHANGE_FLAG = -1 Then
      Call MakeNew(NEWDATA)
      If OptNew.Value = -1 Then Call LoadNew(NEWDATA)
    End If
    KeyAscii = 0
  End If
End Sub


Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    If OptNew.Value = -1 Then Call LoadNew(NEWDATA)
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
    If Index = 1 Then IDIFF = 2
    'Check if this parameter is already selected and display if necessary.
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


