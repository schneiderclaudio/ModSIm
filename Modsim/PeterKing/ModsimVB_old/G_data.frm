VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form g_data 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify distribution of particle grades"
   ClientHeight    =   6225
   ClientLeft      =   105
   ClientTop       =   1350
   ClientWidth     =   8850
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
   ScaleHeight     =   6225
   ScaleWidth      =   8850
   Begin VB.CheckBox ChkDataAvailable 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Experimental data available"
      Height          =   375
      Left            =   6360
      TabIndex        =   61
      Tag             =   "DataAvailable"
      Top             =   4320
      Width           =   2295
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   720
      TabIndex        =   58
      Top             =   5520
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   720
      TabIndex        =   57
      Top             =   5280
      Width           =   900
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   4200
      TabIndex        =   55
      Top             =   720
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   5880
      TabIndex        =   54
      Top             =   720
      Width           =   1320
   End
   Begin VB.CommandButton CmdImport 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Import data from file"
      Height          =   480
      Left            =   2640
      TabIndex        =   56
      Tag             =   "Import"
      Top             =   5040
      Width           =   2055
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   6360
      TabIndex        =   24
      Top             =   4800
      Width           =   960
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   0
      Left            =   2880
      TabIndex        =   53
      Tag             =   "SizeRange"
      Top             =   720
      Value           =   -1  'True
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7560
      TabIndex        =   50
      Top             =   360
      Width           =   1215
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   22
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   21
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
         TabIndex        =   20
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   7800
      TabIndex        =   23
      Top             =   4800
      Width           =   960
   End
   Begin VB.CommandButton CmdClear 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      Caption         =   "Clear"
      Height          =   375
      Left            =   1680
      TabIndex        =   25
      Top             =   480
      Width           =   840
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   720
      TabIndex        =   0
      Top             =   480
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   720
      TabIndex        =   19
      Top             =   5040
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   720
      TabIndex        =   18
      Top             =   4800
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   720
      TabIndex        =   17
      Top             =   4560
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   720
      TabIndex        =   16
      Top             =   4320
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   720
      TabIndex        =   15
      Top             =   4080
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   720
      TabIndex        =   14
      Top             =   3840
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   720
      TabIndex        =   13
      Top             =   3600
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   720
      TabIndex        =   12
      Top             =   3360
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   720
      TabIndex        =   11
      Top             =   3120
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   720
      TabIndex        =   10
      Top             =   2880
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   720
      TabIndex        =   9
      Top             =   2640
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   720
      TabIndex        =   8
      Top             =   2400
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   720
      TabIndex        =   7
      Top             =   2160
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   720
      TabIndex        =   6
      Top             =   1920
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   720
      TabIndex        =   5
      Top             =   1680
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   720
      TabIndex        =   4
      Top             =   1440
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   720
      TabIndex        =   3
      Top             =   1200
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   720
      TabIndex        =   2
      Top             =   960
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   720
      TabIndex        =   1
      Top             =   720
      Width           =   900
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   7920
      Top             =   1920
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "22"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   21
      Left            =   0
      TabIndex        =   60
      Top             =   5520
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "21"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   20
      Left            =   0
      TabIndex        =   59
      Top             =   5280
      Width           =   600
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
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
      Height          =   240
      Index           =   0
      Left            =   5520
      TabIndex        =   51
      Top             =   720
      Width           =   375
   End
   Begin VB.Label LblStreamNo 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3840
      TabIndex        =   49
      Top             =   240
      Width           =   3480
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   52
      Top             =   0
      Visible         =   0   'False
      Width           =   8520
   End
   Begin VB.Label LblClass 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Class"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   26
      Top             =   240
      Width           =   480
   End
   Begin VB.Label Stream 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2880
      TabIndex        =   48
      Top             =   240
      Width           =   960
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "12"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   11
      Left            =   0
      TabIndex        =   32
      Top             =   3120
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "11"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   0
      TabIndex        =   38
      Top             =   2880
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "10"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   0
      TabIndex        =   37
      Top             =   2640
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "9"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   0
      TabIndex        =   36
      Top             =   2400
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "8"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   0
      TabIndex        =   35
      Top             =   2160
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "7"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   0
      TabIndex        =   34
      Top             =   1920
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "6"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   0
      TabIndex        =   33
      Top             =   1680
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "5"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   0
      TabIndex        =   31
      Top             =   1440
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "13"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   12
      Left            =   0
      TabIndex        =   39
      Top             =   3360
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "14"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   13
      Left            =   0
      TabIndex        =   40
      Top             =   3600
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "15"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   14
      Left            =   0
      TabIndex        =   41
      Top             =   3840
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "16"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   15
      Left            =   0
      TabIndex        =   42
      Top             =   4080
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "17"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   16
      Left            =   0
      TabIndex        =   43
      Top             =   4320
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "18"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   17
      Left            =   0
      TabIndex        =   44
      Top             =   4560
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "19"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   18
      Left            =   0
      TabIndex        =   45
      Top             =   4800
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "20"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   19
      Left            =   0
      TabIndex        =   46
      Top             =   5040
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "4"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   0
      TabIndex        =   30
      Top             =   1200
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "3"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   0
      TabIndex        =   29
      Top             =   960
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "2"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   0
      TabIndex        =   28
      Top             =   720
      Width           =   600
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "1"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   0
      TabIndex        =   27
      Top             =   480
      Width           =   600
   End
   Begin VB.Label LblMassFraction 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mass fraction"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   720
      TabIndex        =   47
      Top             =   240
      Width           =   1560
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
Attribute VB_Name = "g_data"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private NGC As Integer
Private SELECTIND As Integer
Private CHANGE_FLAG As Integer
Private NEWLDATA As GRADEDATA

Private Sub ChkDataAvailable_Click()
'***********************************
  If Screen.ActiveControl.Tag = "DataAvailable" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    Call MakeNew(NEWLDATA)
    Call LoadNew(NEWLDATA)
  End If
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim Cum As Single
  Dim N As Integer
  
  'If no data make sure that the data is cleared.
  If ChkDataAvailable = vbUnchecked Then
    NEWLDATA.NGC = 0
    Call LoadNew(NEWLDATA)
  Else
    If SizeRange1(0).text = "" Or SizeRange2(0).text = "" Then
      WSTRING = "Size range has not been specified"
      MsgBox WSTRING, 0, "ERROR"
      Exit Sub
    End If
    Cum = 0!
    For N = 1 To NGC
      Cum = Cum + NEWLDATA.Fraction(N)
    Next N
    If Abs(Cum - 1) > 0.0001 Then
      Call LoadNew(NEWLDATA)
      WSTRING = "Fractions sum to " + Str$(Cum)
      MsgBox WSTRING, 0, "ERROR"
      Exit Sub
    End If
    If OSTREAM.LibIndex = 0 Then
      Sysdata.N_O_DATA_STREAMS = Sysdata.N_O_DATA_STREAMS + 1
      OSTREAM.LibIndex = Sysdata.N_O_DATA_STREAMS
    End If
  End If
  Call MakeNew(CURRLDATA(OSTREAM.LibIndex))
  Unload g_data
End Sub

Private Sub CmdCancel_Click()
'*****************************
  Unload g_data
End Sub

Private Sub CmdClear_Click()
'****************************
  Dim N As Integer
  For N = 1 To NGC
    Parameter(N - 1).text = ""
  Next N
  Parameter(0).SetFocus
End Sub

Private Sub Form_Load()
'**********************
  On Error GoTo ErrHandler
  
  CHANGE_FLAG = 0
  SELECTIND = -1
  OptSizeRange(0).Value = -1

  LblStreamNo.Caption = OSTREAM.Stream.Caption + " " + OSTREAM.StreamName.text
  NGC = Val(Sysdata.NGCM.text)
  SizeRange1(0).text = Str$(0!)
  SizeRange2(0).text = Sysdata.D1.text

  'Find the current data for this stream if any
  If OSTREAM.LibIndex = 0 Or CURRLDATA(OSTREAM.LibIndex).ID = 0 Then
    OptCurrent.Enabled = 0
    LoadDefault
    SizeRange1(0).text = Str$(0)
    SizeRange2(0).text = Sysdata.D1.text
    Call MakeNew(NEWLDATA)
  Else
    NEWLDATA = CURRLDATA(OSTREAM.LibIndex)
    Call LoadNew(NEWLDATA)
  End If
  SizeRange1(0).Enabled = 0
  SizeRange2(0).Enabled = 0
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading liberation data", vbExclamation, "WARNING"
  Exit Sub
End Sub

Private Sub LoadDefault()
'*************************
  Dim N As Integer, Nomins As Integer
  Nomins = Val(Sysdata.NoMin.text)
  OptSizeRange(0).Value = -1
  SizeRange1(0).text = Str$(0!)
  SizeRange2(0).text = Sysdata.D1.text
  For N = 1 To 22
    LblClassNo(N - 1).Visible = False
    Parameter(N - 1).Visible = False
  Next N
  Me.ChkDataAvailable.Value = vbUnchecked
End Sub

Private Sub LoadNew(NDATA As GRADEDATA)
'**************************************
  Dim N As Integer
  
  NGC = NDATA.NGC
  If NGC = 0 Then
    ChkDataAvailable.Value = vbUnchecked
  Else
    ChkDataAvailable.Value = vbChecked
  End If
    
  For N = 1 To NGC
    LblClassNo(N - 1).Visible = -1
    Parameter(N - 1).Visible = -1
    Parameter(N - 1).text = Format$(NDATA.Fraction(N), "0.0000")
  Next N
  For N = NGC + 1 To 22
    LblClassNo(N - 1).Visible = 0
    Parameter(N - 1).Visible = 0
  Next N
End Sub

Private Sub MakeNew(NDATA As GRADEDATA)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  If ChkDataAvailable.Value = vbChecked Then
    NDATA.NGC = Val(Sysdata.NGCM)
    For I = 1 To NDATA.NGC
      NDATA.Fraction(I) = Val(Parameter(I - 1).text)
    Next I
  Else
    NDATA.NGC = 0
  End If
  NDATA.NSTR = Val(OSTREAM.Stream.Caption)
  NDATA.STREAM_NAME = OSTREAM.StreamName.text
  NGC = NDATA.NGC
  NDATA.ID = Sysdata.ID
End Sub

Private Sub MnuAccept_Click()
Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
'****************************
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm

End Sub

Private Sub OptCurrent_Click()
'******************************
  Dim TEMPDATA As GRADEDATA
  If Screen.ActiveControl.Tag = "Current" Then
    TEMPDATA = CURRLDATA(OSTREAM.LibIndex)
    Call LoadNew(TEMPDATA)
  End If
End Sub

Private Sub OptDefault_Click()
'******************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
'**************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWLDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'**********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'Parameter(Index).SetFocus
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
'************************************************
  Instruction.Visible = -1
  Instruction.Caption = "Specify fraction of the size class in this grade class"
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
'*********************************************************************
  If Index < NGC - 1 And KeyAscii = 13 Then
    Parameter(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'*************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWLDATA)
End Sub

Private Sub SizeRange1_Change(Index As Integer)
'***********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'SizeRange1(Index).SetFocus
  End If
End Sub

Private Sub SizeRange1_GotFocus(Index As Integer)
'*************************************************
  SELECTIND = Index
  Instruction.Visible = -1
  Instruction.Caption = "Grade distribution must be summed over all sizes"
End Sub


Private Sub CmdImport_GotFocus()
'*******************************
  Instruction.Visible = -1
  Instruction.Caption = "Get liberation data from an external ASCII file"
End Sub

Private Sub CmdImport_LostFocus()
'********************************
  Instruction.Visible = 0
End Sub


Private Sub CmdImport_Click()
'****************************
  'Open file  to get current data for size distributions
  Dim FileName As String
  Dim I As Integer, WS As Single
  On Error GoTo ErrHandler
  
  'Set filters
  CommonDialog1.Filter = "All files (*.*)|*.*)"
  'Default filter
  CommonDialog1.FilterIndex = 1
  'Set the cancel error flag
  CommonDialog1.CancelError = True
  CommonDialog1.Flags = cdlOFNHideReadOnly
  'Display OPEN dialog box
  CommonDialog1.ShowOpen
  FileName = Trim(CommonDialog1.FileName)
  
  On Error GoTo ErrHandler1
  
  Open FileName For Input As #15
  NEWLDATA.NSTR = Val(OSTREAM.Stream.Caption)
  NEWLDATA.NGC = Val(Sysdata.NGCM.text)
  WS = 0
  Do
    Input #15, NEWLDATA.SIZE_RANGE(1), NEWLDATA.SIZE_RANGE(2)
    For I = 1 To NEWLDATA.NGC
      Input #15, NEWLDATA.Fraction(I)
      WS = WS + NEWLDATA.Fraction(I)
    Next I
  Loop While Not EOF(15)
  For I = 1 To NEWLDATA.NGC
    NEWLDATA.Fraction(I) = NEWLDATA.Fraction(I) / WS
  Next I
  Call LoadNew(NEWLDATA)
  Close #15
  Exit Sub
  
ErrHandler:
  Exit Sub
    
ErrHandler1:
  MsgBox "Error reading liberation data file", 16, "ERROR"
  Exit Sub
  
End Sub
