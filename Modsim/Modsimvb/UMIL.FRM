VERSION 5.00
Begin VB.Form UMIL 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model UMIL"
   ClientHeight    =   5850
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   9735
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
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5850
   ScaleWidth      =   9735
   Begin VB.Frame Frame4 
      Caption         =   "Parameters for the breakage function"
      Height          =   1455
      Left            =   6000
      TabIndex        =   27
      Top             =   1440
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   10
         Left            =   2280
         TabIndex        =   34
         Top             =   1080
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   9
         Left            =   2280
         TabIndex        =   31
         Top             =   840
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   8
         Left            =   2280
         TabIndex        =   30
         Top             =   600
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   7
         Left            =   2280
         TabIndex        =   28
         Top             =   360
         Width           =   1215
      End
      Begin VB.Label LblParam 
         Caption         =   "Phi at 5mm"
         Height          =   255
         Index           =   10
         Left            =   120
         TabIndex        =   35
         Top             =   1080
         Width           =   2175
      End
      Begin VB.Label LblParam 
         Caption         =   "Delta"
         Height          =   255
         Index           =   9
         Left            =   120
         TabIndex        =   33
         Top             =   840
         Width           =   2175
      End
      Begin VB.Label LblParam 
         Caption         =   "Gamma"
         Height          =   255
         Index           =   8
         Left            =   120
         TabIndex        =   32
         Top             =   600
         Width           =   2175
      End
      Begin VB.Label LblParam 
         Caption         =   "Beta"
         Height          =   255
         Index           =   7
         Left            =   120
         TabIndex        =   29
         Top             =   360
         Width           =   2175
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Parameters for the selection function"
      Height          =   1455
      Left            =   120
      TabIndex        =   18
      Top             =   1440
      Width           =   5055
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   6
         Left            =   3720
         TabIndex        =   25
         Top             =   1080
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   5
         Left            =   3720
         TabIndex        =   22
         Top             =   840
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   4
         Left            =   3720
         TabIndex        =   21
         Top             =   600
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   3
         Left            =   3720
         TabIndex        =   19
         Top             =   360
         Width           =   1215
      End
      Begin VB.Label LblParam 
         Caption         =   "Lambda.  "
         Height          =   255
         Index           =   6
         Left            =   120
         TabIndex        =   26
         Top             =   1080
         Width           =   3375
      End
      Begin VB.Label LblParam 
         Caption         =   "Size at maximum rate of breakage  (mm)"
         Height          =   255
         Index           =   5
         Left            =   120
         TabIndex        =   24
         Top             =   840
         Width           =   3495
      End
      Begin VB.Label LblParam 
         Caption         =   "Alpha"
         Height          =   255
         Index           =   4
         Left            =   120
         TabIndex        =   23
         Top             =   600
         Width           =   2655
      End
      Begin VB.Label LblParam 
         Caption         =   "Selection function at 1 mm  (1/mins)"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   20
         Top             =   360
         Width           =   3495
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   3840
      TabIndex        =   16
      Top             =   840
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   3840
      TabIndex        =   13
      Top             =   600
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   3840
      TabIndex        =   1
      Top             =   360
      Width           =   1215
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6600
      TabIndex        =   11
      Top             =   4920
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7800
      TabIndex        =   10
      Top             =   4920
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7920
      TabIndex        =   0
      Top             =   120
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display data set that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display data set used in the previous simulation"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   3
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set for this unit model"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      ForeColor       =   &H00000000&
      Height          =   975
      Left            =   3480
      TabIndex        =   4
      Top             =   4560
      Width           =   2895
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "micron"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   1200
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   2160
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   615
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   1320
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   600
         Width           =   960
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   9
         Tag             =   "Size"
         Top             =   600
         Value           =   -1  'True
         Width           =   600
      End
   End
   Begin VB.Label LblParam 
      Caption         =   "Residence time in third section  (mins)"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   17
      Top             =   840
      Width           =   3615
   End
   Begin VB.Label LblParam 
      Caption         =   "Residence time in second section  (mins)"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   15
      Top             =   600
      Width           =   3615
   End
   Begin VB.Label LblParam 
      Caption         =   "Residence time in first section  (mins)"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   14
      Top             =   360
      Width           =   3615
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "UMIL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Sub CmdAccept_Click()
'*********************
  Call MakeNew(CURRMODELDATA(ID))
  UnitMods.Show
  Unload UMIL
End Sub

Sub CmdCancel_Click()
'*********************
  Unload UMIL
  UnitMods.Show
End Sub

Sub Form_Load()
'***************
  On Error GoTo ErrHandler
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "UMIL" Then
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

Sub LoadDefault()
'*****************
  Parameter(0).TEXT = CStr(???)
  OptSize(3).Value = -1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).TEXT = CStr(NDATA.PARAM(I))
  Next I
  OptSize(3).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "UMIL"
  NDATA.NOPAR = ??
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).TEXT)
  Next I
  'Convert the size to meters
  ScaleFactor = 1#
  If OptSize(0).Value Then ScaleFactor = 0.000001
  If OptSize(1).Value Then ScaleFactor = 0.001
  If OptSize(2).Value Then ScaleFactor = 0.01
  If OptSize(4).Value Then ScaleFactor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * ScaleFactor
End Sub

Private Sub MnuAccept_Click()
'****************************
   Call CmdAccept_Click
End Sub

Private Sub MnuClose_Click()
'***************************
  Call cmdClose_Click
End Sub

Sub OptCurrent_Click()
'**********************
  If Screen.ActiveControl.Tag = "Current" Then
        Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Sub OptDefault_Click()
'**********************
  If Screen.ActiveControl.Tag = "Default" Then
        Call LoadDefault
  End If
End Sub

Sub OptNew_Click()
'******************
  'Must trigger a load every time NEW is switched on except when changing a text field
  If Not TypeOf Screen.ActiveControl Is TextBox Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'*****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Sub optSize_Click(Index As Integer)
'***********************************
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optSize_LostFocus(Index As Integer)
'***************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub
