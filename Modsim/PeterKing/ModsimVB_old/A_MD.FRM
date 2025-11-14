VERSION 5.00
Begin VB.Form A_MD 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for models of the Andrews-Mika diagram"
   ClientHeight    =   3960
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   8235
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
   ScaleHeight     =   3960
   ScaleWidth      =   8235
   Begin VB.CheckBox ChkLibMatrix 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Calculate Andrews-Mika diagram for ""Ljubljana"" model on exit"
      Height          =   375
      Left            =   3120
      TabIndex        =   22
      Top             =   360
      Width           =   3135
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta function model"
      Height          =   2415
      Left            =   120
      TabIndex        =   9
      Top             =   1080
      Width           =   4335
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   7
         Left            =   3360
         TabIndex        =   23
         Top             =   2040
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   6
         Left            =   3360
         TabIndex        =   20
         Top             =   1680
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   5
         Left            =   3360
         TabIndex        =   18
         Top             =   1440
         Visible         =   0   'False
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   4
         Left            =   3360
         TabIndex        =   16
         Top             =   1200
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   3
         Left            =   3360
         TabIndex        =   14
         Top             =   960
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   2
         Left            =   3360
         TabIndex        =   12
         Top             =   600
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   1
         Left            =   3360
         TabIndex        =   10
         Top             =   360
         Width           =   855
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Asymmetry factor"
         Height          =   255
         Index           =   7
         Left            =   120
         TabIndex        =   24
         Top             =   2040
         Width           =   3255
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Variance sensitivity"
         Height          =   255
         Index           =   6
         Left            =   120
         TabIndex        =   21
         Top             =   1680
         Width           =   3255
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Variance exponent"
         Height          =   255
         Index           =   5
         Left            =   120
         TabIndex        =   19
         Top             =   1440
         Visible         =   0   'False
         Width           =   3255
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Andrews-Mika boundary sensitivity"
         Height          =   255
         Index           =   4
         Left            =   120
         TabIndex        =   17
         Top             =   1200
         Width           =   3135
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Andrews-Mika boundary exponent"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   15
         Top             =   960
         Width           =   3255
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Preferential breakage factor"
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   13
         Top             =   600
         Width           =   2535
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Liberation size  microns"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   11
         Top             =   360
         Width           =   2535
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   """Ljubljana"" model"
      Height          =   615
      Left            =   120
      TabIndex        =   6
      Top             =   240
      Width           =   2415
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   225
         Index           =   0
         Left            =   1440
         TabIndex        =   7
         Top             =   240
         Width           =   615
      End
      Begin VB.Label LblParam 
         BackColor       =   &H00C0C0C0&
         Caption         =   "PhiA (10 - 100)"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   8
         Top             =   240
         Width           =   1335
      End
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   5880
      TabIndex        =   4
      Top             =   2880
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7200
      TabIndex        =   3
      Top             =   2880
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   6480
      TabIndex        =   0
      Top             =   120
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   5
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
         TabIndex        =   1
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
         TabIndex        =   2
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set for this unit model"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "A_MD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private CHANGE_FLAG As Integer
Private NEWDATA As CLASSDATA
Private loadingForm As Boolean

Sub CmdAccept_Click()
'*********************
  Call MakeNew(CURRENT)
  Sysdata.LibMatrix_check = ChkLibMatrix.Value
  If ChangedA_MDiagramParameters Then
    MsgBox "The parameters for the beta function A-M digram have changed." & Chr(13) & Chr(10) & "The diagram must be remade from your mill model to take effect", vbInformation, "INFORMATION"
  End If
  A_MD.Hide
End Sub

Sub CmdCancel_Click()
'*********************
  Unload A_MD
End Sub

Sub Form_Load()
'**************
  On Error GoTo ErrHandler
  loadingForm = True
  CHANGE_FLAG = 0
  If CURRENT.SPECIFIED = 0 Or CURRENT.INDPP(2, 2) = 1 Then
    OptCurrent.Enabled = False
    LoadDefault
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  Else
    OptCurrent.Enabled = True
    Call LoadNew(CURRENT)
    Call MakeNew(NEWDATA)
  End If
  loadingForm = False
  Exit Sub
  
ErrHandler:
  loadingForm = False
  MsgBox "Error loading Andrews-Mika model data", 16, "ERROR"
  Exit Sub
End Sub

Sub LoadDefault()
'****************
  Parameter(0).text = CStr(50)
  Parameter(1).text = CStr(200)
  Parameter(2).text = CStr(0#)
  Parameter(3).text = CStr(1.5)
  Parameter(4).text = CStr(0.5)
  Parameter(5).text = CStr(1)
  Parameter(6).text = CStr(1#)
  Parameter(7).text = CStr(1#)
End Sub

Private Sub LoadNew(NDATA As CLASSDATA)
'**************************************
Dim I As Integer
  For I = 1 To 8
    Parameter(I - 1).text = CStr(NDATA.PPROP(NDATA.INDPP(2, 1) + I - 1))
  Next I
End Sub

Private Sub MakeNew(NDATA As CLASSDATA)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  Call ADD_TO_PPROP(NDATA, 2, 8)
  NDATA.PHIA = Val(Parameter(0).text)
  For I = 1 To 8
    NDATA.PPROP(NDATA.INDPP(2, 1) + I - 1) = Val(Parameter(I - 1).text)
  Next I
End Sub

Private Sub MnuAccept_Click()
'****************************
   Call CmdAccept_Click
End Sub

Private Sub MnuClose_Click()
'***************************
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm
End Sub

Sub OptCurrent_Click()
'**********************
  If Screen.ActiveControl.Tag = "Current" Then
        Call LoadNew(CURRENT)
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
  'Must trigger a load only when NEW is switched on with a click.
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If Not loadingForm Then
    If TypeOf Screen.ActiveControl Is TextBox Then
      If OptNew.Value = 0 Then OptNew.Value = -1
      CHANGE_FLAG = -1
      If Index > 0 Then ChangedA_MDiagramParameters = True
    End If
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'*****************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    If Index = 0 Then ChkLibMatrix.Value = 1
  End If
End Sub

