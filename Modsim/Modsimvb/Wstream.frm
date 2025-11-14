VERSION 5.00
Begin VB.Form Wstream 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify water feeds"
   ClientHeight    =   3675
   ClientLeft      =   60
   ClientTop       =   1350
   ClientWidth     =   6795
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
   Icon            =   "Wstream.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3675
   ScaleWidth      =   6795
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   360
      Left            =   5760
      TabIndex        =   19
      Top             =   2880
      Width           =   960
   End
   Begin VB.TextBox StreamName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   1320
      TabIndex        =   0
      Top             =   600
      Width           =   4095
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   2040
      TabIndex        =   14
      Text            =   "Text2"
      Top             =   2520
      Width           =   360
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   1680
      TabIndex        =   11
      Text            =   "Text1"
      Top             =   2280
      Width           =   720
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   360
      Left            =   4680
      TabIndex        =   20
      Top             =   2880
      Width           =   960
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify water addition by"
      Height          =   855
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   2415
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Percent solids in unit"
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   6
         Tag             =   "WaterSpec"
         Top             =   480
         Value           =   -1  'True
         Width           =   2175
      End
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Water addition rate"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "WaterSpec"
         Top             =   240
         Width           =   2055
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1200
      Left            =   5520
      TabIndex        =   18
      Top             =   120
      Width           =   1200
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   17
         Tag             =   "Default"
         Top             =   720
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   16
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   15
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for water addition rate"
      Height          =   975
      Left            =   3120
      TabIndex        =   7
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1800
      Width           =   3135
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "L/min"
         Height          =   240
         Index           =   5
         Left            =   2280
         TabIndex        =   24
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   600
         Width           =   825
      End
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Impgal/min"
         Height          =   240
         Index           =   4
         Left            =   1200
         TabIndex        =   23
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   600
         Width           =   1185
      End
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "USgal/min"
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   22
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   600
         Value           =   -1  'True
         Width           =   1185
      End
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cfm"
         Height          =   240
         Index           =   2
         Left            =   1920
         TabIndex        =   10
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m^3/hr"
         Height          =   240
         Index           =   1
         Left            =   960
         TabIndex        =   9
         TabStop         =   0   'False
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   840
      End
      Begin VB.OptionButton OptRate 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg/s"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "Rate"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   960
      End
   End
   Begin VB.Label LblUnit 
      BackColor       =   &H00C0C0C0&
      Height          =   255
      Left            =   1680
      TabIndex        =   21
      Top             =   2520
      Width           =   255
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   5280
   End
   Begin VB.Label Nstream 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   840
      TabIndex        =   3
      Top             =   600
      Width           =   495
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Water addition rate"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   12
      Top             =   2280
      Width           =   1455
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Percent solids in unit"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   13
      Top             =   2520
      Width           =   1575
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   2
      Top             =   600
      Width           =   735
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
Attribute VB_Name = "Wstream"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NEWWDATA As WATERDATA
Private CHANGE_FLAG
Private NSTR As Integer
Private CurrentIndex

Private Sub CmdAccept_Click()
'****************************
  Dim FirstChar As Integer
  'Check for valid stream name
  FirstChar = Asc(Me.StreamName.text)
  If FirstChar < 65 Or FirstChar > 122 Or (FirstChar > 90 And FirstChar < 97) Then
    MsgBox "Stream names must start with an alphabetic character", vbExclamation, "WARNING"
    Exit Sub
  End If

  If CurrentIndex = 0 Then
    Sysdata.N_WATER_DATA_STREAMS = Sysdata.N_WATER_DATA_STREAMS + 1
    CurrentIndex = Sysdata.N_WATER_DATA_STREAMS
  End If
  Call MakeNew(CURRWDATA(CurrentIndex))
  Unload Me
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide
  If FastWaterStreamChange Then
    Sysdata.CmdReturn_Click
    FastWaterStreamChange = False
  End If
End Sub

Private Sub Form_Load()
'**********************
  Dim N As Integer
  
  CHANGE_FLAG = 0
  NSTR = Sysdata.NSTR
  Nstream.Caption = CStr(NSTR)
  LblUnit.Caption = Format(Sysdata.WUNIT, "####")
' Find the current data for this stream if any
  CurrentIndex = 0
  For N = 1 To Sysdata.N_WATER_DATA_STREAMS
    If CURRWDATA(N).ID = Sysdata.ID Then
      CurrentIndex = N
      Call LoadNew(CURRWDATA(N))
      Call MakeNew(NEWWDATA)
    End If
  Next N
  If CurrentIndex = 0 Then
    OptCurrent.Enabled = 0
    Call LoadDefault
    Call MakeNew(NEWWDATA)
  End If
  OptRate(0).Value = -1
  OptNew.Value = -1
End Sub

Private Sub LoadDefault()
'************************
  StreamName.text = "No name"
  Parameter(0).text = Str$(1#)
  Parameter(1).text = Str$(0!)
  OptDefault.Value = True
  OptRate(0).Value = True
    OptFeed(0).Value = True
    Label2.Visible = True
    Label3.Visible = False
    LblUnit.Visible = False
    Parameter(0).Visible = True
    Parameter(1).Visible = False
    Frame2.Visible = True
  
End Sub

Private Sub LoadNew(NWDATA As WATERDATA)
'**************************************
  Parameter(0).text = Format$(NWDATA.WATER_RATE)
  Parameter(1).text = Format$(NWDATA.PERCENT_SOLIDS)
  StreamName.text = NWDATA.STREAM_NAME
  If NWDATA.PERCENT_SOLIDS > 0! Then
    OptFeed(1).Value = True
    Label2.Visible = False
    Label3.Visible = True
    LblUnit.Visible = True
    Parameter(0).Visible = False
    Parameter(1).Visible = True
    Frame2.Visible = False
  Else
    OptFeed(0).Value = True
    Label2.Visible = True
    Label3.Visible = False
    LblUnit.Visible = False
    Parameter(0).Visible = True
    Parameter(1).Visible = False
    Frame2.Visible = True
  End If
  OptRate(0).Value = -1
End Sub

Private Sub MakeNew(NWDATA As WATERDATA)
'***************************************
  Dim Factor As Single
  CHANGE_FLAG = 0
  NWDATA.NSTR = NSTR
  NWDATA.ID = Sysdata.ID
  NWDATA.STREAM_NAME = StreamName.text

  'Convert the feedrate to kg/s
  CFactor = kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin_Lpermin

  If OptRate(0).Value = True Then Factor = 1!
  If OptRate(1).Value = True Then Factor = 1000 * CFactor.Factor(2)
  If OptRate(2).Value = True Then Factor = 1000 * CFactor.Factor(3)
  If OptRate(3).Value = True Then Factor = 1000 * CFactor.Factor(4)
  If OptRate(4).Value = True Then Factor = 1000 * CFactor.Factor(5)
  If OptRate(5).Value = True Then Factor = 1000 * CFactor.Factor(6)
  NWDATA.WATER_RATE = Val(Parameter(0).text) * Factor
  NWDATA.PERCENT_SOLIDS = Val(Parameter(1).text)
  If OptFeed(0).Value = True Then NWDATA.PERCENT_SOLIDS = 0#
  If OptFeed(1).Value = True Then NWDATA.WATER_RATE = 0#
End Sub

Private Sub MnuAccept_Click()
'****************************
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
'****************************
  Call CmdCancel_Click
End Sub

Private Sub OptCurrent_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRWDATA(CurrentIndex))
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptFeed_Click(Index As Integer)
'******************************************
  If Screen.ActiveControl.Tag = "Rate" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
  If Index = 0 Then
    Label2.Visible = -1
    Label3.Visible = 0
    LblUnit.Visible = 0
    Parameter(0).Visible = -1
    Parameter(1).Visible = 0
    Frame2.Visible = -1
  Else
    Label2.Visible = 0
    Label3.Visible = -1
    LblUnit.Visible = -1
    Parameter(0).Visible = 0
    Parameter(1).Visible = -1
    Frame2.Visible = 0
  End If
End Sub

  Private Sub OptFeed_GotFocus(Index As Integer)
    Instruction.Visible = -1
    Instruction.Caption = "Specify water rate or % solids in the unit that receives the water."
  End Sub

  Private Sub OptFeed_LostFocus(Index As Integer)
    Instruction.Visible = 0
  End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWWDATA)
  End If
End Sub

Private Sub optRate_Click(Index As Integer)
'******************************************
  If Screen.ActiveControl.Tag = "Rate" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optRate_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptRate_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin_Lpermin

If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptRate(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "#0.#####")
  OptRate(Index).Value = True
End If
End Sub
Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  Instruction.Visible = -1
  If Index = 0 Then
    Instruction.Caption = "Specify water feed rate"
  Else
    Instruction.Caption = "Specify % solids in the unit that receives the water"
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'************************************************
    If CHANGE_FLAG = -1 Then Call MakeNew(NEWWDATA)
    Instruction.Visible = 0
End Sub

Private Sub StreamName_Change()
'******************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub StreamName_GotFocus()
'********************************
  Instruction.Caption = "Specify a descriptive name for the stream"
  Instruction.Visible = -1
End Sub

Private Sub StreamName_Lostfocus()
'*********************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWWDATA)
    Sysdata.WStreamList.RemoveItem Sysdata.IND
    Sysdata.WStreamList.AddItem CStr(NSTR) & " " & NEWWDATA.STREAM_NAME, Sysdata.IND
  End If
    Instruction.Visible = 0
End Sub
