VERSION 5.00
Begin VB.Form OSTREAM 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Particle-size distribution"
   ClientHeight    =   6795
   ClientLeft      =   75
   ClientTop       =   1455
   ClientWidth     =   8385
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
   ScaleHeight     =   6795
   ScaleWidth      =   8385
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   7200
      TabIndex        =   66
      Top             =   5880
      Width           =   960
   End
   Begin VB.Frame fraSizeDistributions 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size distribution data"
      Enabled         =   0   'False
      Height          =   5415
      Left            =   120
      TabIndex        =   10
      Top             =   960
      Width           =   6735
      Begin VB.TextBox RRparameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   5280
         TabIndex        =   67
         Text            =   "Text1"
         Top             =   2400
         Width           =   720
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   6
         Left            =   1920
         TabIndex        =   63
         Top             =   1920
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   7
         Left            =   1920
         TabIndex        =   62
         Top             =   2160
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   8
         Left            =   1920
         TabIndex        =   61
         Top             =   2400
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   9
         Left            =   1920
         TabIndex        =   60
         Top             =   2640
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   10
         Left            =   1920
         TabIndex        =   59
         Top             =   2880
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   11
         Left            =   1920
         TabIndex        =   58
         Top             =   3120
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   12
         Left            =   1920
         TabIndex        =   57
         Top             =   3360
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   13
         Left            =   1920
         TabIndex        =   56
         Top             =   3600
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   14
         Left            =   1920
         TabIndex        =   55
         Top             =   3840
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   15
         Left            =   1920
         TabIndex        =   54
         Top             =   4080
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   16
         Left            =   1920
         TabIndex        =   53
         Top             =   4320
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   17
         Left            =   1920
         TabIndex        =   52
         Top             =   4560
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   18
         Left            =   1920
         TabIndex        =   51
         Top             =   4800
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   3
         Left            =   1920
         TabIndex        =   50
         Top             =   1200
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   1920
         TabIndex        =   49
         Top             =   1440
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   1920
         TabIndex        =   48
         Top             =   1680
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   2
         Left            =   1920
         TabIndex        =   47
         Top             =   960
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   1920
         TabIndex        =   46
         Top             =   480
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   1920
         TabIndex        =   45
         Top             =   720
         Width           =   800
      End
      Begin VB.TextBox Cum 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   19
         Left            =   1920
         TabIndex        =   44
         Top             =   5040
         Width           =   800
      End
      Begin VB.Frame Frame2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Units of size"
         Height          =   855
         Left            =   2880
         TabIndex        =   38
         Top             =   1080
         Width           =   2775
         Begin VB.OptionButton OptSize 
            BackColor       =   &H00C0C0C0&
            Caption         =   "micron"
            Height          =   240
            Index           =   0
            Left            =   120
            TabIndex        =   43
            TabStop         =   0   'False
            Tag             =   "Size"
            ToolTipText     =   "Left click to slect: right click to convert to"
            Top             =   240
            Width           =   975
         End
         Begin VB.OptionButton OptSize 
            BackColor       =   &H00C0C0C0&
            Caption         =   "mm"
            Height          =   240
            Index           =   1
            Left            =   1200
            TabIndex        =   42
            TabStop         =   0   'False
            Tag             =   "Size"
            ToolTipText     =   "Left click to slect: right click to convert to"
            Top             =   240
            Width           =   720
         End
         Begin VB.OptionButton OptSize 
            BackColor       =   &H00C0C0C0&
            Caption         =   "cm"
            Height          =   240
            Index           =   2
            Left            =   2040
            TabIndex        =   41
            TabStop         =   0   'False
            Tag             =   "Size"
            ToolTipText     =   "Left click to slect: right click to convert to"
            Top             =   240
            Width           =   615
         End
         Begin VB.OptionButton OptSize 
            BackColor       =   &H00C0C0C0&
            Caption         =   "m"
            Height          =   240
            Index           =   3
            Left            =   120
            TabIndex        =   40
            TabStop         =   0   'False
            Tag             =   "Size"
            ToolTipText     =   "Left click to slect: right click to convert to"
            Top             =   480
            Value           =   -1  'True
            Width           =   600
         End
         Begin VB.OptionButton OptSize 
            BackColor       =   &H00C0C0C0&
            Caption         =   "inch"
            Height          =   240
            Index           =   4
            Left            =   960
            TabIndex        =   39
            Tag             =   "Size"
            ToolTipText     =   "Left click to slect: right click to convert to"
            Top             =   480
            Width           =   735
         End
      End
      Begin VB.TextBox Nclasses 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Left            =   4200
         TabIndex        =   37
         Top             =   480
         Width           =   600
      End
      Begin VB.CheckBox ChkRR 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Use Rosin-Rammler distribution"
         Height          =   240
         Left            =   3000
         TabIndex        =   36
         Tag             =   "RRCheck"
         Top             =   2160
         Width           =   2640
      End
      Begin VB.TextBox RRparameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   3480
         TabIndex        =   35
         Text            =   "Text1"
         Top             =   2400
         Width           =   720
      End
      Begin VB.CommandButton CmdSpecGrades 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Specify distribution among grade classes"
         Height          =   360
         Left            =   2880
         TabIndex        =   34
         Top             =   3960
         Width           =   3360
      End
      Begin VB.CommandButton cmdExportSizeDistribution 
         Caption         =   "Export size distribution"
         Enabled         =   0   'False
         Height          =   375
         Left            =   2880
         TabIndex        =   33
         Top             =   2760
         Width           =   3375
      End
      Begin VB.CommandButton cmdImportSizeDistribution 
         Caption         =   "Import size distribution"
         Height          =   375
         Left            =   2880
         TabIndex        =   32
         Top             =   3240
         Width           =   3375
      End
      Begin VB.CommandButton CmdClear 
         Appearance      =   0  'Flat
         BackColor       =   &H000000FF&
         Caption         =   "Clear"
         Height          =   375
         Left            =   1080
         TabIndex        =   30
         Top             =   480
         Width           =   720
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   120
         TabIndex        =   29
         Top             =   1920
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   6
         Left            =   120
         TabIndex        =   28
         Top             =   2160
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   7
         Left            =   120
         TabIndex        =   27
         Top             =   2400
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   8
         Left            =   120
         TabIndex        =   26
         Top             =   2640
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   9
         Left            =   120
         TabIndex        =   25
         Top             =   2880
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   10
         Left            =   120
         TabIndex        =   24
         Top             =   3120
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   11
         Left            =   120
         TabIndex        =   23
         Top             =   3360
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   12
         Left            =   120
         TabIndex        =   22
         Top             =   3600
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   13
         Left            =   120
         TabIndex        =   21
         Top             =   3840
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   14
         Left            =   120
         TabIndex        =   20
         Top             =   4080
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   15
         Left            =   120
         TabIndex        =   19
         Top             =   4320
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   16
         Left            =   120
         TabIndex        =   18
         Top             =   4560
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   17
         Left            =   120
         TabIndex        =   17
         Top             =   4800
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   120
         TabIndex        =   16
         Top             =   960
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   2
         Left            =   120
         TabIndex        =   15
         Top             =   1200
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   3
         Left            =   120
         TabIndex        =   14
         Top             =   1440
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   120
         TabIndex        =   13
         Top             =   1680
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   120
         TabIndex        =   12
         Top             =   720
         Width           =   800
      End
      Begin VB.TextBox upper_size 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   18
         Left            =   120
         TabIndex        =   11
         Top             =   5040
         Width           =   800
      End
      Begin VB.Label Label4 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "% Passing"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   1920
         TabIndex        =   70
         Top             =   240
         Width           =   855
      End
      Begin VB.Label Label2 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   69
         Top             =   240
         Width           =   825
      End
      Begin VB.Label LblRR 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Lambda"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   4680
         TabIndex        =   68
         Top             =   2400
         Width           =   600
      End
      Begin VB.Label LblRR 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "D63.2"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   3000
         TabIndex        =   65
         Top             =   2400
         Width           =   480
      End
      Begin VB.Label Label3 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of sizes"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   2880
         TabIndex        =   64
         Top             =   480
         Width           =   1320
      End
      Begin VB.Label Label1 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         Caption         =   "Infinity"
         ForeColor       =   &H00000000&
         Height          =   225
         Left            =   120
         TabIndex        =   31
         Top             =   480
         Width           =   795
      End
   End
   Begin VB.CheckBox chkSpecifySizeDistributions 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify size distribution to compare against simulation"
      Height          =   255
      Left            =   120
      TabIndex        =   9
      Top             =   600
      Width           =   4335
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   7200
      TabIndex        =   3
      Top             =   5280
      Width           =   960
   End
   Begin VB.TextBox StreamName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   4080
      TabIndex        =   8
      Text            =   " "
      Top             =   240
      Width           =   4200
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7080
      TabIndex        =   4
      Top             =   600
      Width           =   1215
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   2
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
         TabIndex        =   1
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
         TabIndex        =   0
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Label Stream 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3720
      TabIndex        =   7
      Top             =   240
      Width           =   360
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream number"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3000
      TabIndex        =   5
      Top             =   240
      Width           =   600
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   6
      Top             =   0
      Visible         =   0   'False
      Width           =   8160
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
Attribute VB_Name = "OSTREAM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NEWODATA As STREAMDATA
Private CHANGE_FLAG As Integer
Private NSTR As Integer
Private CurrentIndex As Integer
Public LibIndex As Integer

Private Sub ChkRR_Click()
'************************
  If Screen.ActiveControl.Tag = "RRCheck" Then
    If ChkRR.Value = 1 Then
      Call SHOWRR
    Else
      Call HIDERR
    End If
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ChkRR_GotFocus()
  Instruction.Caption = "Check to choose Rosin-Rammmler distribution. Click <Clear> to calculate"
  Instruction.Visible = -1
End Sub

Private Sub ChkRR_LostFocus()
'*********************
  Instruction.Visible = 0
End Sub

Private Sub chkSpecifySizeDistributions_Click()
'**********************************************
  If Me.chkSpecifySizeDistributions.Value = vbChecked Then
     Me.fraSizeDistributions.Enabled = True
     Me.cmdExportSizeDistribution.Enabled = True
     Me.cmdImportSizeDistribution.Enabled = True
     Me.CmdSpecGrades.Enabled = True
  Else
     Me.fraSizeDistributions.Enabled = False
     Me.cmdExportSizeDistribution.Enabled = False
     Me.cmdImportSizeDistribution.Enabled = False
     Me.CmdSpecGrades.Enabled = False
  End If
End Sub

Private Sub CmdAccept_Click()
'*********************
  Dim NDC As Integer, I As Integer
  Dim FirstChar As Integer
  'Check for valid stream name
  FirstChar = Asc(Me.StreamName.text)
  If FirstChar < 65 Or FirstChar > 122 Or (FirstChar > 90 And FirstChar < 97) Then
    MsgBox "Stream names must start with an alphabetic character", vbExclamation, "WARNING"
    Exit Sub
  End If
  
  'Load new to register stream name
   Call LoadNew(NEWODATA)

  'Check that size distribution data is valid
  NDC = Val(Nclasses.text)
  For I = 0 To NDC - 2
    If Val(upper_size(I).text) < 0 Then
      MsgBox "Mesh sizes must be positive", 0, "ERROR"
      upper_size(I).SetFocus
      Exit Sub
    End If
  Next I
  For I = 1 To NDC - 2
    If Val(upper_size(I).text) > Val(upper_size(I - 1).text) Then
      MsgBox "Mesh sizes must decrease", 0, "ERROR"
      upper_size(I).SetFocus
      Exit Sub
    End If
  Next I
  For I = 0 To NDC - 1
    If Val(Cum(I).text) < 0 Then
      MsgBox "% passing must be positive", 0, "ERROR"
      Cum(I).SetFocus
      Exit Sub
    End If
  Next I
  For I = 1 To NDC - 1
    If Val(Cum(I).text) > Val(Cum(I - 1).text) Then
      MsgBox "% passing must not increase", 0, "ERROR"
      Cum(I).SetFocus
      Exit Sub
    End If
  Next I

  If CurrentIndex = 0 Then
    Sysdata.N_O_DATA_STREAMS = Sysdata.N_O_DATA_STREAMS + 1
    CurrentIndex = Sysdata.N_O_DATA_STREAMS
  End If
  Call MakeNew(CURRODATA(CurrentIndex))
  Unload OSTREAM
End Sub

Private Sub CmdCancel_Click()
'*********************
  Unload OSTREAM
End Sub

Private Sub CmdClear_Click()
'********************
  Dim LAMBDA As Single, Ratio As Single, D632 As Single, CSIZE As Single
  Dim NDC As Integer, I As Integer
  If OptNew.Value = -1 Then
    NDC = Val(Nclasses.text)
    Ratio = Sqr(2)
    D632 = Val(RRparameter(0).text)
    LAMBDA = Val(RRparameter(1).text)
    CSIZE = D632 * (4.605) ^ (1! / LAMBDA)
    For I = 1 To 19
      If ChkRR.Value = 1 Then
        upper_size(I - 1).text = Format$(CSIZE, "#.000E+#")
        Cum(I).text = Format$(ROSIN_RAMM(CSIZE, D632, LAMBDA) * 100, "####.00")
        CSIZE = CSIZE / Sqr(2!)
        If I <= NDC - 1 Then
          Call SHOW_CLASS(I)
        Else
          Call HIDE_CLASS(I)
        End If
      Else
        upper_size(I - 1).text = ""
        Cum(I).text = ""
      End If
    Next I
    If ChkRR.Value = 1 Then
      Call MakeNew(NEWODATA)
    Else
      If NDC > 1 Then upper_size(0).SetFocus
    End If
  End If
End Sub

Private Sub CmdClear_GotFocus()
'***********************
  Instruction.Visible = -1
  Instruction.Caption = "Click to clear all sizes or to generate Rosin-Rammler distribution"
End Sub

Private Sub CmdClear_LostFocus()
'************************
  Instruction.Visible = 0
End Sub

Private Sub cmdExportSizeDistribution_Click()
'********************************************
  Dim NDC As Integer
  Call MakeNew(NEWODATA)
  Call ExportSizeDistributionData(NEWODATA)
End Sub

Private Sub cmdImportSizeDistribution_Click()
'********************************************
Call ImportSizeDistributionData(NEWODATA)
Call LoadNew(NEWODATA)
End Sub

Private Sub CmdSpecGrades_Click()
'********************************
  Dim N As Integer
  'Find the index for the matching liberation data
  LibIndex = 0
  For N = 1 To Sysdata.N_LIB_DATA_STREAMS
    If CURRLDATA(N).ID = Sysdata.ID Then
      LibIndex = N
    End If
  Next N
  g_data.Top = DisplayOffsetY + 400
  g_data.Left = DisplayOffsetX + 400
  g_data.Show 1
End Sub

Private Sub CmdSpecGrades_GotFocus()
'****************************
  Instruction.Visible = -1
  Instruction.Caption = "Click to specify grade distribution for particles in the selected range"
  If Sysdata.FORM_CHANGE_FLAG = -1 Then
    Call MakeNew(NEWODATA)
    Sysdata.FORM_CHANGE_FLAG = 0
  End If
End Sub

Private Sub CmdSpecGrades_LostFocus()
'*****************************
  Instruction.Visible = 0
End Sub

Private Sub Cum_Change(Index As Integer)
'********************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Cum(Index).SetFocus
  End If
End Sub

Private Sub Cum_Gotfocus(Index As Integer)
'**********************************
  Instruction.Caption = "Specify the % passing the associated mesh size"
  Instruction.Visible = -1
End Sub

Private Sub Cum_KeyPress(Index As Integer, KeyAscii As Integer)
'*******************************************************
  If KeyAscii = 13 Then
    If Index > 0 Then
      If Val(Cum(Index).text) > Val(Cum(Index - 1).text) Then
        MsgBox "Percent passing values must decrease", 0, "ERROR"
        Cum(Index).SetFocus
      End If
    End If
    If Index < NEWODATA.NDC - 1 Then
      upper_size(Index).SetFocus
      KeyAscii = 0
    End If
  End If
End Sub

Private Sub Cum_LostFocus(Index As Integer)
'***********************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWODATA)
End Sub

Private Sub Form_Load()
'**********************
  Dim N As Integer
  Dim CSIZE As Single, D632 As Single, Ratio As Single
  
  CHANGE_FLAG = 0
  If Val(Sysdata.NGCM.text) = 1 Then CmdSpecGrades.Enabled = False
  NSTR = Sysdata.NSTR
  Stream.Caption = CStr(NSTR)
' Find the current data for this stream if any
  CurrentIndex = 0
  For N = 1 To Sysdata.N_O_DATA_STREAMS
    If CURRODATA(N).ID = Sysdata.ID Then
      CurrentIndex = N
      Call LoadNew(CURRODATA(N))
      Call MakeNew(NEWODATA)
    End If
  Next N
  If CurrentIndex = 0 Then
    OptCurrent.Enabled = 0
    Call LoadDefault
    Call MakeNew(NEWODATA)
  End If
  OptNew.Value = -1
  CSIZE = Val(Sysdata.D1.text)
  Ratio = Sqr(2)
  D632 = CSIZE / (Ratio ^ 5)
  RRparameter(0).text = Format$(D632, "#.000E+#")
  RRparameter(1).text = Str$(1.2)
  Call HIDERR
End Sub

Private Sub HIDERR()
'***********
    LblRR(0).Visible = 0
    RRparameter(0).Visible = 0
    LblRR(1).Visible = 0
    RRparameter(1).Visible = 0
End Sub

Private Sub HIDE_CLASS(I As Integer)
'****************************
    upper_size(I - 1).Visible = 0
    Cum(I).Visible = 0
End Sub

Private Sub LoadDefault()
'*****************
  Dim CSIZE As Single, Ratio As Single, D632 As Single
  Dim NDC As Integer, I As Integer
  'Load defaults
  StreamName.text = "No name"
  NDC = 1
  Nclasses.text = Str$(NDC)
  OptDefault.Value = -1
  OptSize(3).Value = -1
  Cum(0).text = Str$(100)
  For I = NDC To 19
    Call HIDE_CLASS(I)
  Next I
  Me.cmdImportSizeDistribution.Enabled = False
  Me.cmdExportSizeDistribution.Enabled = False
  Me.fraSizeDistributions.Enabled = False
  Me.chkSpecifySizeDistributions.Value = vbUnchecked
End Sub

Private Sub LoadNew(NODATA As STREAMDATA)
'*********************************
  Dim CUMUL As Single
  Dim I As Integer, N As Integer
  Nclasses.text = Str$(NODATA.NDC)
  StreamName.text = NODATA.STREAM_NAME
  CUMUL = 1!
  Cum(0).text = Format$(100!, "####.00")
  For I = 1 To NODATA.NDC - 1
    upper_size(I - 1).text = Format$(NODATA.CUMSIZE(I), "#.000E+#")
    Cum(I).text = Format$(NODATA.Fraction(I) * 100, "####.00")
    Call SHOW_CLASS(I)
  Next I
  For I = NODATA.NDC To 19
    Call HIDE_CLASS(I)
  Next I
  OptSize(3).Value = -1
  If NODATA.NDC <= 1 Then
    Me.cmdImportSizeDistribution.Enabled = False
    Me.cmdExportSizeDistribution.Enabled = False
    Me.fraSizeDistributions.Enabled = False
  Else
    Me.cmdImportSizeDistribution.Enabled = True
    Me.cmdExportSizeDistribution.Enabled = True
    Me.fraSizeDistributions.Enabled = True
  End If
  'Find the index for the matching liberation data
  LibIndex = 0
  For N = 1 To Sysdata.N_LIB_DATA_STREAMS
    If CURRLDATA(N).ID = Sysdata.ID Then
      LibIndex = N
    End If
  Next N
  If NODATA.NDC <= 1 Or LibIndex = 0 Then
    Me.fraSizeDistributions.Enabled = False
    Me.chkSpecifySizeDistributions.Value = vbUnchecked
  Else
    Me.fraSizeDistributions.Enabled = True
    Me.chkSpecifySizeDistributions.Value = vbChecked
  End If
End Sub

Private Sub MakeNew(NODATA As STREAMDATA)
'****************************************
  Dim Factor As Single
  Dim N As Integer
  CHANGE_FLAG = 0
  NODATA.NSTR = NSTR
  NODATA.NDC = Val(Nclasses.text)
  NODATA.ID = Sysdata.ID
  NODATA.STREAM_NAME = StreamName.text

  'Convert the size to meters
  Factor = 1
  If OptSize(0).Value = -1 Then Factor = 0.000001
  If OptSize(1).Value = -1 Then Factor = 0.001
  If OptSize(2).Value = -1 Then Factor = 0.01
  If OptSize(4).Value = -1 Then Factor = 0.0254
  For N = 1 To NODATA.NDC - 1
    NODATA.CUMSIZE(N) = Val(upper_size(N - 1).text) * Factor
    NODATA.Fraction(N) = Val(Cum(N).text) / 100
  Next N
  'Find the index for the matching liberation data
  LibIndex = 0
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

Private Sub Nclasses_Change()
'*********************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Nclasses.SetFocus
  End If
End Sub

Private Sub NClasses_GotFocus()
'***********************
  Instruction.Visible = -1
  Instruction.Caption = "Number of sizes in the data. Can differ from the number of size classes."
End Sub

Private Sub Nclasses_LostFocus()
'*******************************
  Dim I As Integer
  If Val(Nclasses.text) <= 0 Then
    MsgBox "The number of size classes must be specified" & vbCrLf & "Specify 1 if no data is available", 16, "ERROR"
    Nclasses.SetFocus
    CHANGE_FLAG = 0
    Exit Sub
  End If
  If Val(Nclasses.text) > 20 Then
    MsgBox "No more than 20 size classes can be specified as data", 16, "ERROR"
    Nclasses.SetFocus
    CHANGE_FLAG = 0
    Exit Sub
  End If
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWODATA)
    For I = 2 To NEWODATA.NDC
      Call SHOW_CLASS(I - 1)
    Next I
    For I = NEWODATA.NDC To 19
      Call HIDE_CLASS(I)
    Next I
  End If
  CHANGE_FLAG = 0
End Sub

Private Sub OptCurrent_Click()
'**********************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRODATA(CurrentIndex))
  End If
End Sub

Private Sub OptDefault_Click()
'**********************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
'******************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWODATA)
  End If
End Sub

Private Sub optSize_Click(Index As Integer)
'***********************************
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
'***************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWODATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = micron_mm_cm_m_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  For I = 0 To Nclasses - 1
    upper_size(I).text = Format(Val(upper_size(I).text) * Factor, "0.000E+#")
  Next I
  OptSize(Index).Value = True
End If
End Sub

Private Sub SHOWRR()
'************
    LblRR(0).Visible = -1
    RRparameter(0).Visible = -1
    LblRR(1).Visible = -1
    RRparameter(1).Visible = -1
End Sub

Private Sub SHOW_CLASS(I As Integer)
'****************************
    upper_size(I - 1).Visible = -1
    Cum(I).Visible = -1
End Sub

Private Sub StreamName_Change()
'******************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub StreamName_GotFocus()
'*************************
  Instruction.Caption = "Specify a descriptive name for the stream"
  Instruction.Visible = -1
End Sub

Private Sub StreamName_Lostfocus()
'*********************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWODATA)
    Sysdata.OStreamList.RemoveItem Sysdata.IND
    Sysdata.OStreamList.AddItem CStr(NSTR) & " " & NEWODATA.STREAM_NAME, Sysdata.IND
  End If
End Sub

Private Sub Upper_size_Change(Index As Integer)
'***************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Upper_size_GotFocus(Index As Integer)
'*****************************************
  Instruction.Caption = "Specify mesh size"
  Instruction.Visible = -1
End Sub

Private Sub upper_size_KeyPress(Index As Integer, KeyAscii As Integer)
'**************************************************************
  If KeyAscii = 13 Then
    If Index > 0 Then
      If Val(upper_size(Index).text) > Val(upper_size(Index - 1).text) Then
        MsgBox "Sizes must decrease", 0, "ERROR"
        upper_size(Index).SetFocus
      End If
    End If
    Cum(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Upper_size_LostFocus(Index As Integer)
'******************************************
   Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWODATA)
End Sub
