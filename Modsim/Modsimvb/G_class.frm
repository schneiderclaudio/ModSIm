VERSION 5.00
Begin VB.Form G_class 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Set up grade classes"
   ClientHeight    =   6360
   ClientLeft      =   30
   ClientTop       =   1740
   ClientWidth     =   9030
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
   ScaleHeight     =   6360
   ScaleWidth      =   9030
   Begin VB.Frame fraMagn 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for magnetic susceptibility"
      Height          =   615
      Left            =   6240
      TabIndex        =   129
      Top             =   3960
      Width           =   2655
      Begin VB.OptionButton optMagn 
         BackColor       =   &H00C0C0C0&
         Caption         =   "emu/g"
         Height          =   255
         Index           =   1
         Left            =   1440
         TabIndex        =   131
         Tag             =   "Magn"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton optMagn 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m^3/kg"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   130
         Tag             =   "Magn"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.CommandButton CmdA_MD 
      Caption         =   "Specify liberation model data"
      Height          =   615
      Left            =   6600
      TabIndex        =   128
      Top             =   4680
      Width           =   1935
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   5160
      TabIndex        =   127
      Top             =   5640
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   5160
      TabIndex        =   126
      Top             =   5400
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   3720
      TabIndex        =   125
      Top             =   5640
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   3720
      TabIndex        =   124
      Top             =   5400
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   2760
      TabIndex        =   123
      Tag             =   "SpGr"
      Top             =   5640
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   2760
      TabIndex        =   122
      Tag             =   "SpGr"
      Top             =   5400
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   600
      TabIndex        =   119
      Tag             =   "Parameter"
      Top             =   5640
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   600
      TabIndex        =   118
      Tag             =   "Parameter"
      Top             =   5400
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify composition by"
      Height          =   975
      Left            =   6600
      TabIndex        =   115
      Top             =   1800
      Width           =   1935
      Begin VB.OptionButton OptVol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Volume"
         Height          =   255
         Left            =   240
         TabIndex        =   117
         Tag             =   "Vol"
         Top             =   600
         Width           =   1575
      End
      Begin VB.OptionButton OptMass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mass"
         Height          =   255
         Left            =   240
         TabIndex        =   116
         Tag             =   "Mass"
         Top             =   240
         Value           =   -1  'True
         Width           =   1575
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Physical properties"
      Height          =   855
      Left            =   6600
      TabIndex        =   85
      Top             =   2880
      Width           =   1935
      Begin VB.CheckBox ChkPhyProp 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Other"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   87
         Tag             =   "PhysPropCheck"
         Top             =   480
         Width           =   975
      End
      Begin VB.CheckBox ChkPhyProp 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mag. susceptibility"
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   86
         Tag             =   "PhysPropCheck"
         Top             =   240
         Width           =   1695
      End
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   5160
      TabIndex        =   108
      Top             =   3720
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   5160
      TabIndex        =   107
      Top             =   3480
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   5160
      TabIndex        =   100
      Top             =   1800
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   5160
      TabIndex        =   114
      Top             =   5160
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   5160
      TabIndex        =   113
      Top             =   4920
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   5160
      TabIndex        =   112
      Top             =   4680
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   5160
      TabIndex        =   111
      Top             =   4440
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   5160
      TabIndex        =   110
      Top             =   4200
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   5160
      TabIndex        =   109
      Top             =   3960
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   5160
      TabIndex        =   106
      Top             =   3240
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   5160
      TabIndex        =   105
      Top             =   3000
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   5160
      TabIndex        =   104
      Top             =   2760
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   5160
      TabIndex        =   103
      Top             =   2520
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   5160
      TabIndex        =   102
      Top             =   2280
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   5160
      TabIndex        =   101
      Top             =   2040
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   5160
      TabIndex        =   99
      Top             =   1560
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   5160
      TabIndex        =   98
      Top             =   1320
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   5160
      TabIndex        =   97
      Top             =   1080
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   5160
      TabIndex        =   96
      Top             =   840
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.TextBox AddProp 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   5160
      TabIndex        =   95
      Top             =   600
      Visible         =   0   'False
      Width           =   900
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   7680
      TabIndex        =   92
      Top             =   5400
      Width           =   840
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   6600
      TabIndex        =   93
      Top             =   5400
      Width           =   960
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   3720
      TabIndex        =   84
      Top             =   5160
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   3720
      TabIndex        =   83
      Top             =   4920
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   3720
      TabIndex        =   82
      Top             =   4680
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   3720
      TabIndex        =   81
      Top             =   4440
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   3720
      TabIndex        =   80
      Top             =   4200
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   3720
      TabIndex        =   79
      Top             =   3960
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   3720
      TabIndex        =   78
      Top             =   3720
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   3720
      TabIndex        =   77
      Top             =   3480
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   3720
      TabIndex        =   76
      Top             =   3240
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   3720
      TabIndex        =   75
      Top             =   3000
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   3720
      TabIndex        =   74
      Top             =   2760
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   3720
      TabIndex        =   73
      Top             =   2520
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   3720
      TabIndex        =   72
      Top             =   2280
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   3720
      TabIndex        =   71
      Top             =   2040
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   3720
      TabIndex        =   70
      Top             =   1800
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   3720
      TabIndex        =   69
      Top             =   1560
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   3720
      TabIndex        =   68
      Top             =   1320
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   3720
      TabIndex        =   67
      Top             =   1080
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   3720
      TabIndex        =   66
      Top             =   840
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox MagSusc 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   3720
      TabIndex        =   65
      Top             =   600
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   2760
      TabIndex        =   63
      Tag             =   "SpGr"
      Top             =   5160
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   2760
      TabIndex        =   62
      Tag             =   "SpGr"
      Top             =   4920
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   2760
      TabIndex        =   61
      Tag             =   "SpGr"
      Top             =   4680
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   2760
      TabIndex        =   60
      Tag             =   "SpGr"
      Top             =   4440
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   2760
      TabIndex        =   59
      Tag             =   "SpGr"
      Top             =   4200
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   2760
      TabIndex        =   58
      Tag             =   "SpGr"
      Top             =   3960
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   2760
      TabIndex        =   57
      Tag             =   "SpGr"
      Top             =   3720
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   2760
      TabIndex        =   56
      Tag             =   "SpGr"
      Top             =   3480
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   2760
      TabIndex        =   55
      Tag             =   "SpGr"
      Top             =   3240
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   2760
      TabIndex        =   54
      Tag             =   "SpGr"
      Top             =   3000
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   2760
      TabIndex        =   53
      Tag             =   "SpGr"
      Top             =   2760
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   2760
      TabIndex        =   52
      Tag             =   "SpGr"
      Top             =   2520
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   2760
      TabIndex        =   51
      Tag             =   "SpGr"
      Top             =   2280
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   2760
      TabIndex        =   50
      Tag             =   "SpGr"
      Top             =   2040
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   2760
      TabIndex        =   49
      Tag             =   "SpGr"
      Top             =   1800
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   2760
      TabIndex        =   48
      Tag             =   "SpGr"
      Top             =   1560
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   2760
      TabIndex        =   47
      Tag             =   "SpGr"
      Top             =   1320
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   2760
      TabIndex        =   46
      Tag             =   "SpGr"
      Top             =   1080
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   2760
      TabIndex        =   45
      Tag             =   "SpGr"
      Top             =   840
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.TextBox Spgr 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   2760
      TabIndex        =   44
      Tag             =   "SpGr"
      Top             =   600
      Visible         =   0   'False
      Width           =   840
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7320
      TabIndex        =   88
      Top             =   360
      Width           =   1215
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   91
         Tag             =   "Current"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   90
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
         TabIndex        =   89
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   600
      TabIndex        =   42
      Tag             =   "Parameter"
      Top             =   5160
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   600
      TabIndex        =   41
      Tag             =   "Parameter"
      Top             =   4920
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   600
      TabIndex        =   40
      Tag             =   "Parameter"
      Top             =   4680
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   600
      TabIndex        =   39
      Tag             =   "Parameter"
      Top             =   4440
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   600
      TabIndex        =   38
      Tag             =   "Parameter"
      Top             =   4200
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   600
      TabIndex        =   37
      Tag             =   "Parameter"
      Top             =   3960
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   600
      TabIndex        =   36
      Tag             =   "Parameter"
      Top             =   3720
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   600
      TabIndex        =   35
      Tag             =   "Parameter"
      Top             =   3480
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   600
      TabIndex        =   34
      Tag             =   "Parameter"
      Top             =   3240
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   600
      TabIndex        =   33
      Tag             =   "Parameter"
      Top             =   3000
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   600
      TabIndex        =   32
      Tag             =   "Parameter"
      Top             =   2760
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   600
      TabIndex        =   31
      Tag             =   "Parameter"
      Top             =   2520
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   600
      TabIndex        =   30
      Tag             =   "Parameter"
      Top             =   2280
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   600
      TabIndex        =   29
      Tag             =   "Parameter"
      Top             =   2040
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   600
      TabIndex        =   28
      Tag             =   "Parameter"
      Top             =   1800
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   600
      TabIndex        =   27
      Tag             =   "Parameter"
      Top             =   1560
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   600
      TabIndex        =   26
      Tag             =   "Parameter"
      Top             =   1320
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   600
      TabIndex        =   24
      Tag             =   "Parameter"
      Top             =   840
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   600
      TabIndex        =   23
      Tag             =   "Parameter"
      Top             =   600
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   600
      TabIndex        =   25
      Tag             =   "Parameter"
      Top             =   1080
      Visible         =   0   'False
      Width           =   2040
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "22"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   21
      Left            =   120
      TabIndex        =   121
      Top             =   5640
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "21"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   20
      Left            =   120
      TabIndex        =   120
      Top             =   5400
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Class"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   0
      Top             =   360
      Width           =   480
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   195
      Left            =   120
      TabIndex        =   1
      Top             =   0
      Visible         =   0   'False
      Width           =   8280
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "20"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   19
      Left            =   120
      TabIndex        =   21
      Top             =   5160
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "19"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   18
      Left            =   120
      TabIndex        =   20
      Top             =   4920
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "18"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   17
      Left            =   120
      TabIndex        =   19
      Top             =   4680
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "17"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   16
      Left            =   120
      TabIndex        =   18
      Top             =   4440
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "16"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   15
      Left            =   120
      TabIndex        =   17
      Top             =   4200
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "15"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   14
      Left            =   120
      TabIndex        =   16
      Top             =   3960
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "14"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   13
      Left            =   120
      TabIndex        =   15
      Top             =   3720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "13"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   12
      Left            =   120
      TabIndex        =   14
      Top             =   3480
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "12"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   11
      Left            =   120
      TabIndex        =   13
      Top             =   3240
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "11"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   120
      TabIndex        =   12
      Top             =   3000
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   11
      Top             =   2760
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   10
      Top             =   2520
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   9
      Top             =   2280
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   8
      Top             =   2040
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   7
      Top             =   1800
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   6
      Top             =   1560
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   5
      Top             =   1320
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblClassNo 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "3"
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   3
      Top             =   840
      Visible         =   0   'False
      Width           =   495
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
      TabIndex        =   2
      Top             =   600
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label LblAddProp 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Additional property"
      ForeColor       =   &H00000000&
      Height          =   375
      Left            =   5160
      TabIndex        =   94
      Top             =   180
      Visible         =   0   'False
      Width           =   960
   End
   Begin VB.Label LblSpgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Sp. gr. of class"
      ForeColor       =   &H00000000&
      Height          =   375
      Left            =   2760
      TabIndex        =   43
      Top             =   180
      Width           =   855
   End
   Begin VB.Label LblMagSusc 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Magn. susc. of class"
      ForeColor       =   &H00000000&
      Height          =   375
      Left            =   3720
      TabIndex        =   64
      Top             =   180
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.Label Label22 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Composition"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   960
      TabIndex        =   22
      Top             =   360
      Width           =   1320
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
Attribute VB_Name = "G_class"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NNEW As CLASSDATA
Dim Nomins As Integer, NGC As Integer
Dim CHANGE_FLAG As Boolean

Private Sub AddProp_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub AddProp_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < NNEW.NGC - 1 And KeyAscii = 13 Then
    AddProp(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub AddProp_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub ChkPhyProp_Click(Index As Integer)
'*********************************************
Dim N As Integer
If Screen.ActiveControl.Tag = "PhysPropCheck" Then
  If OptNew.Value = False Then OptNew.Value = True
  CHANGE_FLAG = True
  If ChkPhyProp(Index).Value = 0 Then
    If Index = 1 Then
      LblMagSusc.Visible = 0
      Me.fraMagn.Visible = False
      For N = 1 To 20
        MagSusc(N - 1).Visible = 0
      Next N
    End If
    If Index = 2 Then
      LblAddProp.Visible = 0
      For N = 1 To 20
        AddProp(N - 1).Visible = 0
      Next N
    End If
  End If
  If ChkPhyProp(Index).Value = 1 Then
    If Index = 1 Then
      LblMagSusc.Visible = -1
      Me.fraMagn.Visible = True
      For N = 1 To Val(Sysdata.NGCM.text)
        MagSusc(N - 1).Visible = -1
      Next N
      MagSusc(0).SetFocus
    End If
    If Index = 2 Then
      LblAddProp.Visible = -1
      For N = 1 To Val(Sysdata.NGCM.text)
        AddProp(N - 1).Visible = -1
      Next N
      AddProp(0).SetFocus
    End If
  End If
End If
End Sub

Private Sub ChkPhyProp_Gotfocus(Index As Integer)
Instruction.Visible = -1
If Index = 0 Then Instruction.Caption = "Click to specify specific gravities for each grade class"
If Index = 1 Then Instruction.Caption = "Click to specify magnetic susceptibilities for each grade class"
If Index = 2 Then Instruction.Caption = "Click to specify any other physical property for each grade class"
End Sub

Private Sub ChkPhyProp_Lostfocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  Instruction.Caption = ""
  If CHANGE_FLAG Then Call MakeNew(NNEW)
End Sub

Private Sub CmdA_MD_Click()
'**************************
  Call MakeNew(CURRENT)
  A_MD.Top = DisplayOffsetY + 400
  A_MD.Left = DisplayOffsetX + 400
  A_MD.Show 1
End Sub

Public Sub CmdAccept_Click()
'****************************
  Dim N As Integer, M As Integer
  Dim sum As Single
  
  On Error GoTo ErrHandler
  
  Call MakeNew(CURRENT)
  CURRENT.SPECIFIED = -1
  Sysdata.NGCM.text = Str$(CURRENT.NGC)
  If OptMass Then CURRENT.MassVol = "GRDM"
  If OptVol Then CURRENT.MassVol = "GRDV"
  If A_MD.ChkLibMatrix.Value = 1 And CURRENT.NGC < 5 Then
    WSTRING = "Liberation model requires at least 5 particle types"
    MsgBox WSTRING, 0, "ERROR"
    Exit Sub
  End If
  For N = 1 To CURRENT.NGC
    sum = 0#
    For M = 1 To CURRENT.Nomins
      sum = sum + CURRENT.GRDMV(N, M)
    Next M
    If Abs(sum - 1#) >= 0.0001 Then
      Parameter(N - 1).SetFocus
      WSTRING = "Sum of grades is " + Str$(sum)
      MsgBox WSTRING, 0, "ERROR"
      Exit Sub
    End If
  Next N
  G_class.Hide
  Exit Sub
  
ErrHandler:
  MsgBox "Cannot accept this data", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub CmdCancel_Click()
  Unload G_class
End Sub

Private Sub Form_Load()
'**********************
  Dim N As Integer
  
  For N = 1 To Val(Sysdata.NGCM.text)
    LblClassNo(N - 1).Visible = True
    Spgr(N - 1).Visible = True
  Next N
  If CURRENT.SPECIFIED = 0 Or CURRENT.NGC <> Val(Sysdata.NGCM.text) Then
    OptCurrent.Enabled = False
    LoadDefault
    NNEW.INDPPM = 1
    NNEW.INDPP(1, 1) = 0
    Call MakeNew(NNEW)
    Call LoadNew(NNEW)
  Else
    Call LoadNew(CURRENT)
    NNEW.INDPPM = CURRENT.INDPPM
    NNEW.NPPROP = CURRENT.NPPROP
    For N = 1 To CURRENT.NPPROP
      NNEW.LISTPP(N) = CURRENT.LISTPP(N)
    Next N
    Call MakeNew(NNEW)
    Call LoadNew(NNEW)
  End If
  
End Sub

Private Sub LoadDefault()
'************************
  Dim N As Integer, M As Integer
  Dim Nomins As Integer
  Dim WS As Single
  Dim NGC As Integer
  
  OptMass.Value = True
  ChkPhyProp(1).Value = 0
  LblMagSusc.Visible = 0
  ChkPhyProp(2).Value = 0
  LblAddProp.Visible = 0
  Nomins = Val(Sysdata.NoMin.text)
  fraMagn.Visible = False
  NGC = Val(Sysdata.NGCM.text)
  If NGC > 2 Then WS = 1 / (NGC - 2)
  For N = 1 To Val(Sysdata.NGCM.text)
    MagSusc(N - 1).Visible = 0
    AddProp(N - 1).Visible = 0
    Parameter(N - 1).text = ""
    Parameter(N - 1).Visible = True
    If N = 1 Then
      For M = 1 To Nomins
        If M = 1 Then
          Parameter(N - 1).text = Parameter(N - 1).text + Str$(1!) + " "
        Else
          Parameter(N - 1).text = Parameter(N - 1).text + Str$(0!) + " "
        End If
      Next M
    ElseIf N = NGC Then
      For M = 1 To Nomins
        If M = 2 Then
          Parameter(N - 1).text = Parameter(N - 1).text + Str$(1!) + " "
        Else
          Parameter(N - 1).text = Parameter(N - 1).text + Str$(0!) + " "
        End If
      Next M
    Else
      For M = 1 To Nomins
        If M = 1 Then
          Parameter(N - 1).text = Parameter(N - 1).text + Format((NGC - N - 0.5) * WS, "0.000") + " "
        ElseIf M = 2 Then
          Parameter(N - 1).text = Parameter(N - 1).text + Format((N - 1.5) * WS, "0.##0") + " "
        Else
          Parameter(N - 1).text = Parameter(N - 1).text + Str$(0) + " "
        End If
      Next M
    End If
    If Sysdata.OptSGSpec(0).Value = True Then
      Spgr(N - 1).text = "2.7"
      Spgr(N - 1).Enabled = False
    Else
      Spgr(N - 1).text = "2.7"
    End If
  Next N
End Sub

Private Sub LoadNew(NDATA As CLASSDATA)
'**************************************
  Dim N As Integer, M As Integer
  If NDATA.MassVol = "GRDM" Then
    OptMass.Value = True
  Else
    OptVol.Value = True
  End If
  'If Sysdata.OptSGSpec(0) Then
  '  OptVol.Enabled = True
  'End If
  'If Sysdata.OptSGSpec(1) Then
  '  OptVol.Enabled = False
  'End If
  For N = 1 To NDATA.NGC
    Parameter(N - 1).text = ""
    For M = 1 To NDATA.Nomins
      Parameter(N - 1).text = Parameter(N - 1).text + Format$(NDATA.GRDMV(N, M), "0.0000") + " "
    Next M
    Parameter(N - 1).Visible = -1
  Next N
  If Sysdata.OptSGSpec(0).Value = True And NDATA.MineralSG(1) > 0 Then
    LblSpGr.Visible = True
    'Update the calculated SGs
    For N = 1 To NDATA.NGC
      Spgr(N - 1).text = CStr(CalcSG(NDATA, N))
      Spgr(N - 1).Enabled = False
    Next N
  ElseIf NDATA.PPROP(NDATA.INDPP(1, 1)) > 0 Then
    LblSpGr.Visible = True
    For N = 1 To NDATA.NGC
      Spgr(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(1, 1) + N - 1), "#.000")
      Spgr(N - 1).Visible = True
      Spgr(N - 1).Enabled = True
    Next N
  Else
    For N = 1 To NDATA.NGC
      Spgr(N - 1).Enabled = True
    Next N
  End If
  If NDATA.INDPP(3, 1) > 0 Then
    LblMagSusc.Visible = -1
    ChkPhyProp(1).Value = 1
    fraMagn.Visible = True
    OptMagn(0).Value = True
    For N = 1 To NDATA.NGC
      MagSusc(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(3, 1) + N - 1), "Scientific")
      MagSusc(N - 1).Visible = -1
    Next N
  Else
    LblMagSusc.Visible = 0
    ChkPhyProp(1).Value = 0
    fraMagn.Visible = False
    OptMagn(0).Value = True
    For N = 1 To NDATA.NGC
      MagSusc(N - 1).text = ""
      MagSusc(N - 1).Visible = 0
    Next N
  End If
  If NDATA.INDPP(4, 1) > 0 Then
    LblAddProp.Visible = -1
    ChkPhyProp(2).Value = 1
    For N = 1 To NDATA.NGC
      AddProp(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(4, 1) + N - 1), "#.000")
      AddProp(N - 1).Visible = -1
    Next N
  Else
    LblAddProp.Visible = 0
    ChkPhyProp(2).Value = 0
    For N = 1 To NDATA.NGC
      AddProp(N - 1).text = ""
      AddProp(N - 1).Visible = 0
    Next N
  End If
End Sub

Private Sub MagSusc_Change(Index As Integer)
'*******************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub MagSusc_KeyPress(Index As Integer, KeyAscii As Integer)
'******************************************************************
  If Index < NNEW.NGC - 1 And KeyAscii = 13 Then
    MagSusc(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub MagSusc_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub MakeNew(NDATA As CLASSDATA)
'**************************************
Dim I1 As Integer, I2 As Integer
Dim N As Integer, M As Integer
Dim Factor As Single

  NDATA.SPECIFIED = -1
  NDATA.NGC = Val(Sysdata.NGCM.text)
  NDATA.Nomins = Val(Sysdata.NoMin.text)
  If OptMass.Value Then NDATA.MassVol = "GRDM"
  If OptVol.Value Then NDATA.MassVol = "GRDV"
  For N = 1 To NDATA.NGC
    For M = 1 To NDATA.Nomins
      NDATA.GRDMV(N, M) = GetGRDMV(N, M)
    Next M
  Next N
  If Sysdata.OptSGSpec(0).Value = True Then
    For N = 1 To NDATA.Nomins
      NDATA.MineralSG(N) = Val(Sysdata.TxtSG(N - 1).text)
    Next N
  End If
  'If Sysdata.OptSGSpec(1).Value = True And Val(Spgr(0).TEXT) > 0 Then
  If Val(Spgr(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 1, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(1, 1) - 1) = Val(Spgr(N - 1).text)
    Next N
  End If
  If ChkPhyProp(1).Value = 1 And MagSusc(0).text <> "" Then
    Factor = 1#
    If OptMagn(1) = True Then
      Factor = 0.004 * Pi
    End If
    Call ADD_TO_PPROP(NDATA, 3, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(3, 1) - 1) = Val(MagSusc(N - 1).text) * Factor
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 3, NDATA.NGC)
  End If
  If ChkPhyProp(2).Value = 1 And Val(AddProp(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 4, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(4, 1) - 1) = Val(AddProp(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 4, NDATA.NGC)
  End If
End Sub

Private Sub MnuAccept_Click()
Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
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
    MagSusc(I).text = Format(Val(MagSusc(I).text) * Factor, "0.000E+#")
  Next I
  OptMagn(Index).Value = True
End If
End Sub
Private Sub OptMass_Click()
'**************************
  If Screen.ActiveControl.Tag = "Mass" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Call MakeNew(NNEW)
    Call LoadNew(NNEW)
  End If
End Sub

Private Sub OptMass_GotFocus()
'*****************************
  Instruction.Visible = -1
  Instruction.Caption = "Particle volume can be specified by mass or by volume"
End Sub

Private Sub OptMass_LostFocus()
'******************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub OptNew_Click()
'*************************
  'Must trigger a load every time NEW is switched on except when changing a text field
  If Not TypeOf Screen.ActiveControl Is TextBox Then
    Call LoadNew(NNEW)
  End If
End Sub

Private Sub OptVol_Click()
'**************************
  If Screen.ActiveControl.Tag = "Vol" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Call MakeNew(NNEW)
    Call LoadNew(NNEW)
  End If
End Sub

Private Sub OptVol_GotFocus()
'*****************************
  Instruction.Visible = -1
  Instruction.Caption = "Particle volume can be specified by mass or by volume"
End Sub

Private Sub OptVol_LostFocus()
'******************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If Screen.ActiveControl.Tag = "Parameter" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
'***********************************************
  Instruction.Visible = -1
  Instruction.Caption = "Specify the mineral content of each grade class. Separate values with a space"
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
  If Index < NNEW.NGC - 1 And KeyAscii = 13 Then
    Parameter(Index + 1).text = ""
    Parameter(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'************************************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NNEW)
    Call LoadNew(NNEW)
  End If
End Sub

Private Sub Spgr_Change(Index As Integer)
'****************************************
  If Screen.ActiveControl.Tag = "SpGr" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Spgr(Index).SetFocus
  End If
End Sub

Private Sub Spgr_KeyPress(Index As Integer, KeyAscii As Integer)
'***************************************************************
  If Index < NNEW.NGC - 1 And KeyAscii = 13 Then
    Spgr(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Spgr_LostFocus(Index As Integer)
'*******************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NNEW)
End Sub

Private Function CalcSG(NDATA As CLASSDATA, N As Integer) As Single
'******************************************************************
'Calculates the specific gravity of G-class N
  Dim WS As Single
  Dim M As Integer
  On Error GoTo ErrHandler
  If OptMass.Value = True Then
    CalcSG = 0
    For M = 1 To NDATA.Nomins
      CalcSG = CalcSG + GetGRDMV(N, M) / NDATA.MineralSG(M)
    Next M
    If CalcSG > 0 Then
      CalcSG = 1 / CalcSG
    End If
  Else
    CalcSG = 0#
    For M = 1 To NDATA.Nomins
       CalcSG = CalcSG + NDATA.MineralSG(M) * GetGRDMV(N, M)
    Next M
 End If
 Exit Function
 
ErrHandler:
  MsgBox "Error calculating particle specific gravities." & vbCrLf & "Check your data carefully", vbCritical, "ERROR"
  Exit Function
End Function

Private Function GetGRDMV(N As Integer, M As Integer) As Single
'**************************************************************
  'Decipher the list of GRDMV parameter values
    Dim resp As Integer
    Dim WSTRING As String
    Dim MM As Integer, I1 As Integer, I2 As Integer
    WSTRING = LTrim$(Parameter(N - 1).text)
    If WSTRING = "" Then
      GetGRDMV = 0#
    Else
      For MM = 1 To M
        ' Replace the next blank with a "," so that VAL works properly
        WSTRING = LTrim$(WSTRING)
        I1 = InStr(WSTRING, " ")
        If I1 > 0 Then Mid$(WSTRING, I1, 1) = ","
        GetGRDMV = Val(WSTRING)
        I2 = InStr(WSTRING, ",")
        If WSTRING <> "" Then
          WSTRING = Mid$(WSTRING, I2 + 1)
        Else
          resp = MsgBox("Distribution has not been completely specified", 0, "ERROR")
          'Parameter(N - 1).SetFocus
          Exit Function
        End If
      Next MM
    End If
End Function
