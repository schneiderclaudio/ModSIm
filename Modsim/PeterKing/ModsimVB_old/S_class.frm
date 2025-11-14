VERSION 5.00
Begin VB.Form S_CLASS 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify the values attached to S-classes"
   ClientHeight    =   5100
   ClientLeft      =   105
   ClientTop       =   1665
   ClientWidth     =   8400
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
   ScaleHeight     =   5100
   ScaleWidth      =   8400
   Begin VB.Frame fraMagn 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for mag. susceptibility"
      Height          =   735
      Left            =   5880
      TabIndex        =   55
      Top             =   3000
      Width           =   2415
      Begin VB.OptionButton OptMagn 
         BackColor       =   &H00C0C0C0&
         Caption         =   "emu/g"
         Height          =   255
         Index           =   1
         Left            =   1320
         TabIndex        =   57
         Tag             =   "Magn"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton OptMagn 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m^3/kg"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   56
         Tag             =   "Magn"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   4200
      TabIndex        =   49
      Top             =   3240
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   4200
      TabIndex        =   48
      Top             =   3000
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   4200
      TabIndex        =   47
      Top             =   2760
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   4200
      TabIndex        =   46
      Top             =   2520
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   4200
      TabIndex        =   45
      Top             =   2280
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   2400
      TabIndex        =   44
      Tag             =   "MagnValue"
      Top             =   3240
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   2400
      TabIndex        =   43
      Tag             =   "MagnValue"
      Top             =   3000
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   2400
      TabIndex        =   42
      Tag             =   "MagnValue"
      Top             =   2760
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   2400
      TabIndex        =   41
      Tag             =   "MagnValue"
      Top             =   2520
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   2400
      TabIndex        =   40
      Tag             =   "MagnValue"
      Top             =   2280
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   600
      TabIndex        =   39
      Top             =   3240
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   600
      TabIndex        =   38
      Top             =   3000
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   600
      TabIndex        =   37
      Top             =   2760
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   600
      TabIndex        =   36
      Top             =   2520
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   600
      TabIndex        =   35
      Top             =   2280
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Distributed property"
      Height          =   1095
      Left            =   6000
      TabIndex        =   18
      Top             =   1680
      Width           =   2295
      Begin VB.CheckBox ChkDistrProp 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Other"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   21
         Tag             =   "Property"
         Top             =   720
         Width           =   1935
      End
      Begin VB.CheckBox ChkDistrProp 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Magnetic susceptibility"
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   20
         Tag             =   "Property"
         Top             =   480
         Width           =   1935
      End
      Begin VB.CheckBox ChkDistrProp 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Flotation rate constant"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   19
         Tag             =   "Property"
         Top             =   240
         Width           =   1935
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   600
      TabIndex        =   9
      Top             =   1560
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   4200
      TabIndex        =   29
      Top             =   1080
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   4200
      TabIndex        =   33
      Top             =   2040
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   4200
      TabIndex        =   32
      Top             =   1800
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   4200
      TabIndex        =   31
      Top             =   1560
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   4200
      TabIndex        =   30
      Top             =   1320
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   2400
      TabIndex        =   17
      Tag             =   "MagnValue"
      Top             =   2040
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   2400
      TabIndex        =   16
      Tag             =   "MagnValue"
      Top             =   1800
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   2400
      TabIndex        =   15
      Tag             =   "MagnValue"
      Top             =   1560
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   2400
      TabIndex        =   14
      Tag             =   "MagnValue"
      Top             =   1320
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox MagSuscept 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   2400
      TabIndex        =   13
      Tag             =   "MagnValue"
      Top             =   1080
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   600
      TabIndex        =   11
      Top             =   2040
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   600
      TabIndex        =   10
      Top             =   1800
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   600
      TabIndex        =   8
      Top             =   1320
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   600
      TabIndex        =   7
      Top             =   1080
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   7320
      TabIndex        =   26
      Top             =   4200
      Width           =   960
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   6000
      TabIndex        =   27
      Top             =   4200
      Width           =   960
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7080
      TabIndex        =   22
      Top             =   480
      Width           =   1215
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   25
         Tag             =   "Current"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   24
         TabStop         =   0   'False
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   23
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "10"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   120
      TabIndex        =   54
      Top             =   3240
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "9"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   120
      TabIndex        =   53
      Top             =   3000
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "8"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   120
      TabIndex        =   52
      Top             =   2760
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "7"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   120
      TabIndex        =   51
      Top             =   2520
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "6"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   120
      TabIndex        =   50
      Top             =   2280
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblKvalue 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Flotation rate constant"
      ForeColor       =   &H00000000&
      Height          =   480
      Left            =   600
      TabIndex        =   6
      Top             =   600
      Width           =   1680
   End
   Begin VB.Label LblAddProp 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " Additional property"
      ForeColor       =   &H00000000&
      Height          =   480
      Left            =   4200
      TabIndex        =   28
      Top             =   600
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.Label LblMagSuscept 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " Magnetic susceptibility"
      ForeColor       =   &H00000000&
      Height          =   480
      Left            =   2400
      TabIndex        =   12
      Top             =   600
      Visible         =   0   'False
      Width           =   1680
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Visible         =   0   'False
      Width           =   8160
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Class"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   34
      Top             =   840
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "4"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   120
      TabIndex        =   4
      Top             =   1800
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "3"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   120
      TabIndex        =   3
      Top             =   1560
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "2"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   120
      TabIndex        =   2
      Top             =   1320
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "1"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   120
      TabIndex        =   1
      Top             =   1080
      Visible         =   0   'False
      Width           =   480
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "5"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   120
      TabIndex        =   5
      Top             =   2040
      Visible         =   0   'False
      Width           =   480
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
Attribute VB_Name = "S_CLASS"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NNEW As CLASSDATA
Dim NSC As Integer
Dim CHANGE_FLAG As Integer

Private Sub AddProp_Change(Index As Integer)
'*******************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    AddProp(Index).SetFocus
  End If
End Sub

Private Sub AddProp_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < NNEW.NSC - 1 And KeyAscii = 13 Then
    AddProp(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub AddProp_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub ChkDistrProp_Click(Index As Integer)
'***********************************************
  Dim N As Integer
  If Screen.ActiveControl.Tag = "Property" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If ChkDistrProp(Index).Value = 0 Then
      If Index = 0 Then
        LblKvalue.Visible = 0
        For N = 1 To 10
          Parameter(N - 1).Visible = 0
        Next N
      End If
      If Index = 1 Then
        LblMagSuscept.Visible = 0
        fraMagn.Visible = False
        For N = 1 To 10
          MagSuscept(N - 1).Visible = 0
        Next N
      End If
      If Index = 2 Then
        LblAddProp.Visible = 0
        For N = 1 To 10
          AddProp(N - 1).Visible = 0
        Next N
      End If
    End If
    If ChkDistrProp(Index).Value = 1 Then
      If Index = 0 Then
        LblKvalue.Visible = -1
        For N = 1 To Val(Sysdata.NSCM.text)
          Parameter(N - 1).Visible = -1
        Next N
        Parameter(0).SetFocus
      End If
      If Index = 1 Then
        LblMagSuscept.Visible = -1
        Me.fraMagn.Visible = True
        For N = 1 To Val(Sysdata.NSCM.text)
          MagSuscept(N - 1).Visible = -1
        Next N
        MagSuscept(0).SetFocus
      End If
      If Index = 2 Then
        LblAddProp.Visible = -1
        For N = 1 To Val(Sysdata.NSCM.text)
          AddProp(N - 1).Visible = -1
        Next N
        AddProp(0).SetFocus
      End If
    End If
  End If
End Sub

Private Sub ChkDistrProp_Gotfocus(Index As Integer)
Instruction.Visible = -1
If Index = 0 Then Instruction.Caption = "Click to specify specific flotation rate constants"
If Index = 1 Then Instruction.Caption = "Click to specify magnetic susceptibilities for each S-class"
If Index = 2 Then Instruction.Caption = "Click to specify any other physical property for each S-class"
End Sub

Private Sub ChkDistrProp_Lostfocus(Index As Integer)
'***************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
  Instruction.Visible = 0
End Sub

Private Sub CmdAccept_Click()
'****************************
  Call MakeNew(CURRENT)
  CURRENT.SPECIFIED = -1
  Sysdata.NSCM.text = Str$(CURRENT.NSC)
  Unload S_CLASS
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload S_CLASS
End Sub

Private Sub Form_Load()
'**********************
  Dim N As Integer, I As Integer
  
  For N = 1 To Val(Sysdata.NSCM.text)
    LblClassNo(N - 1).Visible = -1
  Next N
  If CURRENT.SPECIFIED = 0 Or CURRENT.NSC <> Val(Sysdata.NSCM.text) Then
    OptCurrent.Enabled = 0
    LoadDefault
    NNEW.INDPPM = 1
    Call MakeNew(NNEW)
  Else
    Call LoadNew(CURRENT)
    NNEW.INDPPM = CURRENT.INDPPM
    NNEW.NPPROP = CURRENT.NPPROP
    For I = 1 To NNEW.NPPROP
      NNEW.LISTPP(I) = CURRENT.LISTPP(I)
      NNEW.INDPP(NNEW.LISTPP(I), 1) = CURRENT.INDPP(NNEW.LISTPP(I), 1)
      NNEW.INDPP(NNEW.LISTPP(I), 2) = CURRENT.INDPP(NNEW.LISTPP(I), 2)
    Next I
    Call MakeNew(NNEW)
  End If
End Sub

Private Sub LoadDefault()
'************************
  Dim N As Integer
  ChkDistrProp(0).Value = 1
  LblKvalue.Visible = -1
  ChkDistrProp(1).Value = 0
  LblMagSuscept.Visible = 0
  ChkDistrProp(2).Value = 0
  fraMagn.Visible = False
  LblAddProp.Visible = 0
  For N = 1 To Val(Sysdata.NSCM.text)
    Parameter(N - 1).Visible = -1
    Parameter(N - 1).text = ""
    MagSuscept(N - 1).Visible = 0
    AddProp(N - 1).Visible = 0
  Next N
End Sub

Private Sub LoadNew(NDATA As CLASSDATA)
'**************************************
  Dim N As Integer
  If NDATA.INDPP(5, 1) > 0 Then
    ChkDistrProp(0).Value = 1
    For N = 1 To NDATA.NSC
      Parameter(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(5, 1) + N - 1), "Scientific")
      Parameter(N - 1).Visible = -1
    Next N
  Else
    ChkDistrProp(0).Value = 0
    LblKvalue.Visible = 0
    For N = 1 To NDATA.NSC
      Parameter(N - 1).text = ""
      Parameter(N - 1).Visible = 0
    Next N
  End If
  If NDATA.INDPP(6, 1) > 0 Then
    ChkDistrProp(1).Value = 1
    LblMagSuscept.Visible = -1
    fraMagn.Visible = True
    For N = 1 To NDATA.NSC
      MagSuscept(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(6, 1) + N - 1), "Scientific")
      MagSuscept(N - 1).Visible = -1
    Next N
  Else
    ChkDistrProp(1).Value = 0
    LblMagSuscept.Visible = 0
    fraMagn.Visible = False
    For N = 1 To NDATA.NSC
      MagSuscept(N - 1).text = ""
      MagSuscept(N - 1).Visible = 0
    Next N
  End If
  If NDATA.INDPP(7, 1) > 0 Then
    ChkDistrProp(2).Value = 1
    LblAddProp.Visible = -1
    For N = 1 To NDATA.NSC
      AddProp(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(7, 1) + N - 1), "#.0000")
      AddProp(N - 1).Visible = -1
    Next N
  Else
    ChkDistrProp(2).Value = 0
    LblAddProp.Visible = 0
    For N = 1 To NDATA.NSC
      AddProp(N - 1).text = ""
      AddProp(N - 1).Visible = 0
    Next N
  End If
End Sub

Private Sub MagSuscept_Change(Index As Integer)
'**********************************************
  If Screen.ActiveControl.Tag = "MagnValue" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    MagSuscept(Index).SetFocus
  End If
End Sub

Private Sub MagSuscept_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < NNEW.NSC - 1 And KeyAscii = 13 Then
    MagSuscept(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub MagSuscept_LostFocus(Index As Integer)
'*************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub MakeNew(NDATA As CLASSDATA)
'**************************************
  Dim N As Integer
  Dim Factor As Single
  NDATA.SPECIFIED = -1
  NDATA.NSC = Val(Sysdata.NSCM.text)
  If ChkDistrProp(0).Value = 1 Then
    Call ADD_TO_PPROP(NDATA, 5, NDATA.NSC)
    For N = 1 To NDATA.NSC
      NDATA.PPROP(N + NDATA.INDPP(5, 1) - 1) = Val(Parameter(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 5, NDATA.NSC)
  End If
  If ChkDistrProp(1).Value = 1 And MagSuscept(0).text <> "" Then
    Call ADD_TO_PPROP(NDATA, 6, NDATA.NSC)
    Factor = 1#
    If OptMagn(1) = vbChecked Then
      Factor = 0.004 * Pi
    End If
    For N = 1 To NDATA.NSC
      NDATA.PPROP(N + NDATA.INDPP(6, 1) - 1) = Val(MagSuscept(N - 1).text) * Factor
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 6, NDATA.NSC)
  End If
  If ChkDistrProp(2).Value = 1 Then
    Call ADD_TO_PPROP(NDATA, 7, NDATA.NSC)
    For N = 1 To NDATA.NSC
      NDATA.PPROP(N + NDATA.INDPP(7, 1) - 1) = Val(AddProp(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 7, NDATA.NSC)
  End If
End Sub

Private Sub MnuAccept_Click()
'****************************
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
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRENT)
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub optMagn_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Magn" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optMagn_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
  CHANGE_FLAG = 0
End Sub

Private Sub OptMagn_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = cubmpkg_emupg
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptMagn(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  For I = 0 To Val(Sysdata.NGCM.text) - 1
    MagSuscept(I).text = Format(Val(MagSuscept(I).text) * Factor, "0.000E+#")
  Next I
  OptMagn(Index).Value = True
End If
End Sub
Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NNEW)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'Parameter(Index).SetFocus
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  Instruction.Visible = -1
  Instruction.Caption = "Specify the flotation rate constant of each S-class."
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If Index < NNEW.NSC - 1 And KeyAscii = 13 Then
    Parameter(Index + 1).text = ""
    Parameter(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub
