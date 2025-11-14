VERSION 5.00
Begin VB.Form DWSC 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model DWSC"
   ClientHeight    =   4350
   ClientLeft      =   60
   ClientTop       =   1500
   ClientWidth     =   8970
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4350
   ScaleWidth      =   8970
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   6240
      TabIndex        =   15
      Top             =   3360
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   7440
      TabIndex        =   14
      Top             =   3360
      Width           =   960
   End
   Begin VB.Frame Frame8 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for amplitude"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   4800
      TabIndex        =   30
      Top             =   1080
      Width           =   2295
      Begin VB.OptionButton Optinch 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   4
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton Optmm 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   3
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.Frame Frame5 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for mesh size"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   2520
      TabIndex        =   23
      Top             =   2400
      Width           =   2175
      Begin VB.OptionButton Optinch 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   0
         Left            =   1200
         TabIndex        =   9
         Top             =   240
         Width           =   840
      End
      Begin VB.OptionButton Optmm 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for screen dimensions"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   4800
      TabIndex        =   22
      Top             =   360
      Width           =   2295
      Begin VB.OptionButton Optfeet 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   1200
         TabIndex        =   1
         Top             =   240
         Width           =   960
      End
      Begin VB.OptionButton Optmeter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "meter"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   0
         TabStop         =   0   'False
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.Frame Frame9 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1080
      Left            =   7320
      TabIndex        =   31
      Top             =   120
      Width           =   1455
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   13
         Tag             =   "Default"
         Top             =   720
         Width           =   1080
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1200
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   960
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mesh characteristics"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   0
      TabIndex        =   16
      Top             =   2400
      Width           =   2295
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   285
         Index           =   1
         Left            =   1080
         TabIndex        =   7
         Top             =   240
         Width           =   960
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   17
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Screen characteristics"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1920
      Left            =   0
      TabIndex        =   18
      Top             =   240
      Width           =   4680
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   6
         Left            =   2760
         TabIndex        =   34
         Top             =   960
         Width           =   960
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   3
         Left            =   2760
         TabIndex        =   33
         Top             =   480
         Width           =   960
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   2
         Left            =   2760
         TabIndex        =   32
         Top             =   240
         Width           =   960
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   7
         Left            =   2760
         TabIndex        =   6
         Top             =   1440
         Width           =   960
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   2760
         TabIndex        =   5
         Top             =   1200
         Width           =   960
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   2760
         TabIndex        =   2
         Top             =   720
         Width           =   960
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Angle of vibration relative to screen"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   7
         Left            =   120
         TabIndex        =   29
         Top             =   1440
         Width           =   2655
      End
      Begin VB.Label Label8 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "degrees"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   3840
         TabIndex        =   20
         Top             =   1200
         Width           =   675
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Angle of inclination"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   19
         Top             =   1200
         Width           =   1815
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Vibration amplitude"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   6
         Left            =   120
         TabIndex        =   28
         Top             =   960
         Width           =   2295
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Vibration frequency (rpm)"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   27
         Top             =   720
         Width           =   2295
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Width of screen"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   21
         Top             =   480
         Width           =   2295
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length of screen"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   24
         Top             =   240
         Width           =   2295
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slurry characteristics"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   0
      TabIndex        =   25
      Top             =   3240
      Width           =   3255
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   285
         Index           =   0
         Left            =   2280
         TabIndex        =   10
         Top             =   240
         Width           =   720
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ultimate moisture content  %"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   26
         Top             =   240
         Width           =   2175
      End
   End
   Begin VB.Menu Mnufile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu Mnucancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "DWSC"
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
  Unload DWSC
End Sub

Private Sub CmdCancel_Click()
  Unload DWSC
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
'***************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "DWSC" Then
        
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
        Parameter(0).text = Str$(7.5)
        Parameter(1).text = Str$(0.001)
        Parameter(2).text = Str$(3.5)
        Parameter(3).text = Str$(1)
        Parameter(4).text = Str$(15)
        Parameter(5).text = Str$(300)
        Parameter(6).text = Str$(0.02)
        Parameter(7).text = Str$(45)
  Optmeter.Value = -1
  Optmm(0).Value = -1
  Parameter(1).text = Str$(Val(Parameter(1).text) * 1000)
  Optmm(1).Value = -1
  Parameter(6).text = Str$(Val(Parameter(6).text) * 1000)
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To NDATA.NOPAR
        Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  Optmeter.Value = -1
  Optmm(0).Value = -1
  Parameter(1).text = Str$(Val(Parameter(1).text) * 1000)
  Optmm(1).Value = -1
  Parameter(6).text = Str$(Val(Parameter(6).text) * 1000)
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim Factor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "DWSC"
  NDATA.NOPAR = 8
  For I = 1 To 8
        NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If (Optmm(0).Value = -1) Then Factor = 0.001
  If (Optinch(0).Value = -1) Then Factor = 0.0254
  NDATA.PARAM(2) = NDATA.PARAM(2) * Factor
  If (Optmm(1).Value = -1) Then Factor = 0.001
  If (Optinch(1).Value = -1) Then Factor = 0.0254
  NDATA.PARAM(7) = NDATA.PARAM(7) * Factor
  Factor = 1!
  If (Optfeet.Value = -1) Then Factor = 0.3048
  NDATA.PARAM(3) = NDATA.PARAM(3) * Factor
  NDATA.PARAM(4) = NDATA.PARAM(4) * Factor
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

Private Sub Optfeet_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optfeet_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optinch_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optinch_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub


Private Sub Optmeter_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optmeter_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optmm_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optmm_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
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

