VERSION 5.00
Begin VB.Form Psd 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Particle-size distribution and feed rate in feed streams"
   ClientHeight    =   6075
   ClientLeft      =   540
   ClientTop       =   2490
   ClientWidth     =   8130
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
   ScaleHeight     =   6075
   ScaleWidth      =   8130
   Begin VB.CommandButton cmdImportSizeDistribution 
      Caption         =   "Import size distribution"
      Height          =   495
      Left            =   6600
      TabIndex        =   81
      Top             =   4200
      Width           =   1455
   End
   Begin VB.CommandButton cmdExportSizeDistribution 
      Caption         =   "Export size distribution"
      Enabled         =   0   'False
      Height          =   495
      Left            =   6600
      TabIndex        =   80
      Top             =   3600
      Width           =   1455
   End
   Begin VB.TextBox PercentSolids 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   6600
      TabIndex        =   53
      Top             =   3000
      Width           =   600
   End
   Begin VB.TextBox FeedRate 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   4080
      TabIndex        =   48
      Top             =   3000
      Width           =   855
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   1800
      TabIndex        =   9
      Top             =   1560
      Width           =   800
   End
   Begin VB.CommandButton CmdSpecGrades 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify grade distributions"
      Height          =   360
      Left            =   2760
      TabIndex        =   77
      Top             =   4560
      Width           =   2880
   End
   Begin VB.CommandButton CmdSClasses 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify distribution over S-classes"
      Height          =   360
      Left            =   2760
      TabIndex        =   78
      Tag             =   "S-Classes"
      Top             =   5040
      Width           =   2880
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of feed rate"
      Height          =   960
      Left            =   2760
      TabIndex        =   71
      Top             =   3480
      Width           =   2895
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Long tons/hr"
         Height          =   240
         Index           =   3
         Left            =   1320
         TabIndex        =   52
         Tag             =   "Feed"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   600
         Value           =   -1  'True
         Width           =   1335
      End
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Short tons/hr"
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   51
         TabStop         =   0   'False
         Tag             =   "Feed"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   1335
      End
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "tonnes/hr"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   50
         TabStop         =   0   'False
         Tag             =   "Feed"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   600
         Width           =   1095
      End
      Begin VB.OptionButton OptFeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg/s"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   49
         TabStop         =   0   'False
         Tag             =   "Feed"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   960
      End
   End
   Begin VB.TextBox StreamName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   5760
      TabIndex        =   0
      ToolTipText     =   "Must start with alpha char"
      Top             =   480
      Width           =   2295
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6240
      TabIndex        =   61
      Top             =   5040
      Width           =   840
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   615
      Left            =   7200
      TabIndex        =   60
      Top             =   5040
      Width           =   840
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   120
      TabIndex        =   38
      Top             =   5400
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
      TabIndex        =   36
      Top             =   4920
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
      TabIndex        =   40
      Top             =   5160
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
      TabIndex        =   34
      Top             =   4680
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
      TabIndex        =   32
      Top             =   4440
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
      TabIndex        =   30
      Top             =   4200
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
      TabIndex        =   28
      Top             =   3960
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
      TabIndex        =   26
      Top             =   3720
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
      TabIndex        =   24
      Top             =   3480
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
      TabIndex        =   22
      Top             =   3240
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
      TabIndex        =   20
      Top             =   3000
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
      TabIndex        =   18
      Top             =   2760
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
      TabIndex        =   16
      Top             =   2520
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
      TabIndex        =   14
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   120
      TabIndex        =   12
      Top             =   2040
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
      TabIndex        =   10
      Top             =   1800
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
      TabIndex        =   8
      Top             =   1560
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
      TabIndex        =   6
      Top             =   1320
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
      TabIndex        =   4
      Top             =   1080
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
      TabIndex        =   2
      Top             =   840
      Width           =   800
   End
   Begin VB.TextBox RRparameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   4920
      TabIndex        =   56
      Top             =   2520
      Width           =   600
   End
   Begin VB.TextBox RRparameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   3360
      TabIndex        =   55
      Top             =   2520
      Width           =   720
   End
   Begin VB.CheckBox ChkRR 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Use Rosin-Rammler distribution"
      Height          =   240
      Left            =   2760
      TabIndex        =   54
      Tag             =   "RRCheck"
      Top             =   2160
      Width           =   3120
   End
   Begin VB.TextBox Nclasses 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   5520
      TabIndex        =   1
      Top             =   840
      Width           =   480
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      Height          =   615
      Left            =   2760
      TabIndex        =   67
      Top             =   1320
      Width           =   3735
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   4
         Left            =   3000
         TabIndex        =   47
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   3
         Left            =   2400
         TabIndex        =   46
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Value           =   -1  'True
         Width           =   495
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   2
         Left            =   1800
         TabIndex        =   45
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   1
         Left            =   1080
         TabIndex        =   44
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "micron"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   43
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: right click to convert to"
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.CommandButton CmdClear 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      Caption         =   "Clear"
      Height          =   465
      Left            =   1080
      TabIndex        =   42
      Top             =   720
      Width           =   600
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   6960
      TabIndex        =   64
      Top             =   1080
      Width           =   1095
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   57
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   59
         TabStop         =   0   'False
         Tag             =   "Default"
         Top             =   720
         Width           =   855
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   58
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   855
      End
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   1800
      TabIndex        =   39
      Top             =   5400
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   1800
      TabIndex        =   37
      Top             =   4920
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   1800
      TabIndex        =   41
      Top             =   5160
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   1800
      TabIndex        =   5
      Top             =   1080
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   1800
      TabIndex        =   3
      Top             =   840
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   1800
      TabIndex        =   62
      Top             =   600
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   1800
      TabIndex        =   11
      Top             =   1800
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   1800
      TabIndex        =   7
      Top             =   1320
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   1800
      TabIndex        =   35
      Top             =   4680
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   1800
      TabIndex        =   33
      Top             =   4440
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   1800
      TabIndex        =   31
      Top             =   4200
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   1800
      TabIndex        =   29
      Top             =   3960
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   1800
      TabIndex        =   27
      Top             =   3720
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   1800
      TabIndex        =   25
      Top             =   3480
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   1800
      TabIndex        =   23
      Top             =   3240
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   1800
      TabIndex        =   21
      Top             =   3000
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   1800
      TabIndex        =   19
      Top             =   2760
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   1800
      TabIndex        =   17
      Top             =   2520
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   1800
      TabIndex        =   15
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   1800
      TabIndex        =   13
      Top             =   2040
      Width           =   800
   End
   Begin VB.Label LblStreamName 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream name"
      Height          =   255
      Left            =   4680
      TabIndex        =   79
      Top             =   480
      Width           =   975
   End
   Begin VB.Label Label7 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Percent solids"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   5400
      TabIndex        =   72
      Top             =   3000
      Width           =   1095
   End
   Begin VB.Label Label6 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Solids feed rate"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2760
      TabIndex        =   70
      Top             =   3000
      Width           =   1215
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   73
      Top             =   120
      Visible         =   0   'False
      Width           =   8040
   End
   Begin VB.Label LblInfinity 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Infinity"
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   120
      TabIndex        =   65
      Top             =   600
      Width           =   795
   End
   Begin VB.Label Stream 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3960
      TabIndex        =   74
      Top             =   480
      Width           =   675
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream number"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2760
      TabIndex        =   69
      Top             =   480
      Width           =   1200
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of mesh sizes in your data"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2760
      TabIndex        =   68
      Top             =   840
      Width           =   2655
   End
   Begin VB.Label LblRR 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Lambda"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   4320
      TabIndex        =   76
      Top             =   2520
      Width           =   600
   End
   Begin VB.Label LblRR 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "D63.2"
      ForeColor       =   &H00000000&
      Height          =   255
      Index           =   0
      Left            =   2760
      TabIndex        =   75
      Top             =   2520
      Width           =   495
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "% Passing"
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   1800
      TabIndex        =   63
      Top             =   360
      Width           =   795
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mesh size"
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   120
      TabIndex        =   66
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
Attribute VB_Name = "Psd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public CURR_INDEX As Integer
Private CHANGE_FLAG As Integer
Public FORM_CHANGE_FLAG As Integer
Private NEWDATA As SIZEDATA
Private NEWODATA As STREAMDATA
Private NSTR As Integer
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
'***************************
  Instruction.Caption = "Check to choose Rosin-Rammmler distribution. Click <Clear> to calculate"
  Instruction.Visible = -1
End Sub

Private Sub ChkRR_LostFocus()
'****************************
  Instruction.Visible = 0
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim I As Integer, NDC As Integer, Factor As Single
  Dim FirstChar As Integer
  Dim K As Integer
  Dim response As Integer, MaxSize As String, NDCM As String
  
  'Check for valid stream name
  FirstChar = Asc(Me.StreamName.text)
  If FirstChar < 65 Or FirstChar > 122 Or (FirstChar > 90 And FirstChar < 97) Then
    MsgBox "Stream names must start with an alphabetic character", vbExclamation, "WARNING"
    Exit Sub
  End If
  
  'Generate the Rosin-Rammler distribution if required
  If ChkRR.Value = vbChecked Then
  response = MsgBox("Rosin-Rammler box is checked." & Chr(13) & Chr(10) & "Do you want to generate a Rosin-Rammler distribution?", 36, "CAUTION")
    If response = vbYes Then
      Call CmdClear_Click
    End If
  End If
  
  'Check that size distribution data is valid
  NDC = Val(Nclasses.text)
  Factor = 1#
  If OptSize(0).Value Then Factor = 0.000001
  If OptSize(1).Value Then Factor = 0.001
  If OptSize(2).Value Then Factor = 0.01
  If OptSize(4).Value Then Factor = 0.0254
  
  If Val(upper_size(0).text) * Factor > Val(Sysdata.D1.text) Then
    response = MsgBox("Top mesh size is larger than the largest size specified on System data form", 17, "ERROR")
    If response = vbCancel Then
      upper_size(0).SetFocus
      Exit Sub
    Else
      MaxSize = InputBox("Specify a new value for the maximum size to be used by MODSIM." & Chr(13) & Chr(10) & "Specify the size in meters", "Change data", Sysdata.D1.text)
      If MaxSize = "" Then
        upper_size(0).SetFocus
        Exit Sub
      ElseIf Val(MaxSize) < Val(upper_size(0).text) * Factor Then
        response = MsgBox("This size is still too small.  Is this what you want?", vbYesNo, "WARNING")
        If response = vbNo Then
          upper_size(0).SetFocus
          Exit Sub
        Else
          Sysdata.D1.text = MaxSize
        End If
      Else
        Sysdata.D1.text = MaxSize
      End If
    End If
  End If
  
  If NDC > Val(Sysdata.NDCM.text) Then
    response = MsgBox("Number of mesh sizes exceeds number of size classes to be used by MODSIM", 17, "ERROR")
    If response = vbCancel Then
      Nclasses.SetFocus
      Exit Sub
    Else
      NDCM = InputBox("Change the number of size classes to be used by MODSIM.", "Change data", Sysdata.NDCM.text)
      If NDCM = "" Then
        Nclasses.SetFocus
        Exit Sub
      ElseIf Val(NDCM) < NDC Then
        response = MsgBox("This size is still too small.  Is this what you want?", vbYesNo + vbQuestion, "WARNING")
        If response = vbNo Then
          Nclasses.SetFocus
          Exit Sub
        Else
          Sysdata.NDCM.text = NDCM
        End If
      Else
        Sysdata.NDCM.text = NDCM
      End If
    End If
  End If
  For I = 0 To NDC - 2
    If Val(upper_size(I).text) <= 0 Then
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

  If CURR_INDEX = 0 Then
    Sysdata.N_FEED_DATA_STREAMS = Sysdata.N_FEED_DATA_STREAMS + 1
    CURR_INDEX = Sysdata.N_FEED_DATA_STREAMS
    CURRDATA(CURR_INDEX).NO_GRADE_RANGES = CURRDATA(0).NO_GRADE_RANGES
    CURRDATA(CURR_INDEX).NO_S_RANGES = CURRDATA(0).NO_S_RANGES
    For K = 1 To CURRDATA(0).NO_GRADE_RANGES
      CURRGDATA(CURR_INDEX, K) = CURRGDATA(0, K)
    Next K
    For K = 1 To CURRDATA(0).NO_S_RANGES
      CURRSDATA(K, CURR_INDEX) = CURRSDATA(K, 0)
    Next K
  End If
  Call MakeNew(CURRDATA(CURR_INDEX))
  Unload Psd
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload Psd
End Sub

Private Sub cmdExportSizeDistribution_Click()
'********************************************
  Dim NDC As Integer
  Call MakeNewStream(NEWODATA)
  Call ExportSizeDistributionData(NEWODATA)
End Sub

Private Sub cmdImportSizeDistribution_Click()
'********************************************
Call ImportSizeDistributionData(NEWODATA)
Call LoadNewStream(NEWODATA)
End Sub
Private Sub Form_Unload(Cancel As Integer)
  Me.Hide
  If FastFeedStreamChange Then
    Sysdata.CmdReturn_Click
    Unload Sysdata
    FastFeedStreamChange = False
  End If
End Sub
Private Sub CmdClear_Click()
'***************************
  Dim I As Integer
  Dim LAMBDA As Single
  Dim NDC As Integer
  Dim Ratio As Single
  Dim D632 As Single
  Dim CSIZE As Single, PercPassing As Single
  If OptNew.Value = -1 Then
    NDC = Val(Nclasses.text)
    Ratio = Sqr(2)
    D632 = Val(RRparameter(0).text)
    LAMBDA = Val(RRparameter(1).text)
    CSIZE = D632 * (4.605) ^ (1! / LAMBDA)
    For I = 1 To 19
      If ChkRR.Value = 1 Then
        upper_size(I - 1).text = Format$(CSIZE, "#.000E+#")
        PercPassing = ROSIN_RAMM(CSIZE, D632, LAMBDA) * 100
        Cum(I).text = Format$(PercPassing, "####.00")
        If PercPassing < 0.015 Then
          'Precision will be lost otherwise
          NDC = I
          Nclasses.text = CStr(NDC)
          Exit For
        End If
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
      Call MakeNew(NEWDATA)
      ChkRR.Value = vbUnchecked
      LblRR(0).Enabled = False
      RRparameter(0).Enabled = False
      LblRR(1).Enabled = False
      RRparameter(1).Enabled = False
    ElseIf NDC > 1 Then
      upper_size(0).SetFocus
    End If
  End If
End Sub

Private Sub CmdClear_GotFocus()
'******************************
  Instruction.Visible = -1
  Instruction.Caption = "Click to clear all sizes or to generate Rosin-Rammler distribution"
End Sub

Private Sub CmdClear_LostFocus()
'*******************************
  Instruction.Visible = 0
End Sub

Private Sub CmdSClasses_Click()
'******************************
  Sc_data.Top = DisplayOffsetY + 400
  Sc_data.Left = DisplayOffsetX + 400
  Sc_data.Show 1
End Sub

Private Sub CmdSClasses_GotFocus()
'*********************************
  Instruction.Visible = -1
  Instruction.Caption = "Click to specify distribution of S-classes"
  If FORM_CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    FORM_CHANGE_FLAG = 0
  End If
End Sub

Private Sub CmdSClasses_LostFocus()
'**********************************
  Instruction.Visible = 0
End Sub

Private Sub CmdSpecGrades_Click()
'********************************
  Dim Factor As Single, MaxSize As String
  Dim response As Integer
'Check the largest size first
  Factor = 1#
  If OptSize(0).Value Then Factor = 0.000001
  If OptSize(1).Value Then Factor = 0.001
  If OptSize(2).Value Then Factor = 0.01
  If OptSize(4).Value Then Factor = 0.0254
  
  If Val(upper_size(0).text) * Factor > Val(Sysdata.D1.text) Then
    response = MsgBox("Top mesh size is larger than the largest size specified on System data form", 17, "ERROR")
    If response = vbCancel Then
      upper_size(0).SetFocus
      Exit Sub
    Else
      MaxSize = InputBox("Specify a new value for the maximum size to be used by MODSIM." & Chr(13) & Chr(10) & "Specify the size in meters", "Change data", Sysdata.D1.text)
      If MaxSize = "" Then
        upper_size(0).SetFocus
        Exit Sub
      ElseIf Val(MaxSize) < Val(upper_size(0).text) * Factor Then
        response = MsgBox("This size is still too small.  Is this what you want?", vbYesNo, "WARNING")
        If response = vbNo Then
          upper_size(0).SetFocus
          Exit Sub
        Else
          Sysdata.D1.text = MaxSize
        End If
      Else
        Sysdata.D1.text = MaxSize
      End If
    End If
  End If
  If Sysdata.OptMinProcPlant.Value = True Then
    Gcdata.Top = DisplayOffsetY + 600
    Gcdata.Left = DisplayOffsetX + 600
    Gcdata.Show 1
  Else
    Washdata.Top = DisplayOffsetY + 600
    Washdata.Left = DisplayOffsetX + 600
    Washdata.Show 1
  End If
End Sub

Private Sub CmdSpecGrades_GotFocus()
'***********************************
  Instruction.Visible = -1
  Instruction.Caption = "Click to specify grade distribution for particles in the selected range"
  If FORM_CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    FORM_CHANGE_FLAG = 0
  End If
End Sub

Private Sub CmdSpecGrades_LostFocus()
'************************************
  Instruction.Visible = 0
End Sub

Private Sub Cum_Change(Index As Integer)
'***************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Cum(Index).SetFocus
  End If
End Sub

Private Sub Cum_Gotfocus(Index As Integer)
'*****************************************
  Instruction.Caption = "Specify the % passing the associated mesh size"
  Instruction.Visible = -1
End Sub

Private Sub Cum_KeyPress(Index As Integer, KeyAscii As Integer)
'**************************************************************
  If KeyAscii = 13 And Index < NEWDATA.NDC - 1 Then
    If Index > 0 Then
      If Val(Cum(Index).text) > Val(Cum(Index - 1).text) Then
        MsgBox "% passing must decrease", 0, "ERROR"
        Cum(Index).SetFocus
        Exit Sub
      ElseIf Val(Cum(Index + 1).text) > Val(Cum(Index).text) Then
        MsgBox "% passing must decrease", 0, "ERROR"
        Cum(Index + 1).SetFocus
        Exit Sub
      End If
    Else
      If Val(Cum(Index).text) < 100# Then
        MsgBox "Must be 100%", 0, "ERROR"
        Cum(Index).SetFocus
        Exit Sub
      End If
    End If
    upper_size(Index).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Cum_LostFocus(Index As Integer)
'******************************************
  Instruction.Visible = 0
  If Index > 0 Then
  Else
    If Val(Cum(Index).text) < 100# Then
       MsgBox "Must be 100%", 0, "ERROR"
       Cum(Index).SetFocus
       Exit Sub
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Feedrate_Change()
'****************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Feedrate_Gotfocus()
'******************************
  Instruction.Caption = "Specify feed rate for this feed stream"
  Instruction.Visible = -1
End Sub

Private Sub Feedrate_LostFocus()
'*******************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
  Instruction.Visible = 0
End Sub

Private Sub Form_Load()
'**********************
  Dim N As Integer
  Dim CSIZE As Single, Ratio As Single, D632 As Single
  On Error GoTo ErrHandler
  
  FORM_CHANGE_FLAG = 0
  CHANGE_FLAG = 0
  If Val(Sysdata.NSCM.text) = 1 Then
    CmdSClasses.Enabled = False
  Else
    CmdSClasses.Enabled = True
  End If
  If Val(Sysdata.NGCM.text) = 1 And Not Sysdata.FORM_CHANGE_FLAG Then CmdSpecGrades.Enabled = False
  NSTR = Sysdata.NSTR
  Stream.Caption = CStr(NSTR)
  If NSTR = 0 Then
    MsgBox "Has loaded a stream with number 0" & vbCrLf & "Load will be cancelled", vbExclamation, "DIAGNOSTIC"
    Err.Raise vbObjectError + 1050, "PSD form", "My error trap"
    Exit Sub
  End If
  If Sysdata.OptMinProcPlant.Value Then
    CmdSpecGrades.Caption = "Specify grade distributions"
    CmdSpecGrades.Enabled = True
  End If
  If Sysdata.OptCoalProcPlant.Value Then
    CmdSpecGrades.Caption = "Specify washability data"
    CmdSpecGrades.Enabled = True
  End If
' Find the current data for this stream if any
  CURR_INDEX = 0
  For N = 1 To Sysdata.N_FEED_DATA_STREAMS
    If CURRDATA(N).ID = Sysdata.ID Then
      CURR_INDEX = N
      Call LoadNew(CURRDATA(N))
      Call MakeNew(NEWDATA)
      Exit For
    End If
  Next N
  For N = 1 To Sysdata.N_O_DATA_STREAMS
    If CURRODATA(N).ID = Sysdata.ID Then
      CURR_INDEX = N
      Call LoadNewStream(CURRODATA(N))
      Call MakeNewStream(NEWODATA)
      Exit For
    End If
  Next N
  If CURR_INDEX = 0 Then
    OptCurrent.Enabled = 0
    Call LoadDefault
    Call MakeNew(NEWDATA)
  End If
  CSIZE = Val(Sysdata.D1.text)
  Ratio = Sqr(2)
  D632 = CSIZE / (Ratio ^ 5)
  RRparameter(0).text = Format$(D632, "#.000E+#")
  RRparameter(1).text = Str$(1.2)
  Call HIDERR
  OptNew.Value = -1
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub

Private Sub HIDERR()
'*******************
    LblRR(0).Visible = 0
    RRparameter(0).Visible = 0
    LblRR(1).Visible = 0
    RRparameter(1).Visible = 0
End Sub

Private Sub HIDE_CLASS(I As Integer)
'***********************************
    upper_size(I - 1).Visible = 0
    Cum(I).Visible = 0
End Sub

Private Sub LoadDefault()
'************************
  'Load defaults
  Dim CSIZE As Single, Ratio As Single, D632 As Single
  Dim NDC As Integer, I As Integer
  StreamName.text = "No name"
  CSIZE = Val(Sysdata.D1.text)
  Ratio = Sqr(2)
  D632 = CSIZE / (Ratio ^ 5)
  
  'Display the RR data but not enabled
  ChkRR.Value = vbUnchecked
  LblRR(0).Visible = True
  LblRR(0).Enabled = False
  RRparameter(0).Visible = True
  RRparameter(0).Enabled = False
  LblRR(1).Visible = True
  LblRR(1).Enabled = False
  RRparameter(1).Visible = True
  RRparameter(1).Enabled = False
  Nclasses.text = Sysdata.NDCM.text
  
  NDC = Val(Nclasses.text)
  If (NDC > 20) Then
     NDC = 20
     Nclasses.text = Str$(NDC)
  End If
  FeedRate.text = Str$(100!)
  OptFeed(1).Value = -1
  PercentSolids.text = Str$(100)
  OptDefault.Value = -1
  OptSize(3).Value = -1
  Cum(0).text = Str$(100)
  For I = 2 To NDC
    upper_size(I - 2).text = Format$(CSIZE, "0.000E+#")
    Cum(I - 1).text = Format$(ROSIN_RAMM(CSIZE, D632, 1.2) * 100, "####.00")
    CSIZE = CSIZE / Sqr(2!)
    Call SHOW_CLASS(I - 1)
  Next I
  For I = NDC To 20
    Call HIDE_CLASS(I)
  Next I
  Me.cmdExportSizeDistribution.Enabled = False
End Sub

Private Sub LoadNew(NDATA As SIZEDATA)
'*************************************
  Dim CUMUL As Single
  Dim I As Integer
  Nclasses.text = Str$(NDATA.NDC)
  StreamName.text = NDATA.STREAM_NAME
  FeedRate.text = Format$(NDATA.FEED_RATE, "0.0000E+#")
  OptFeed(0).Value = -1
  PercentSolids.text = Format$(NDATA.PERCENT_SOLIDS, "###.00")
  CUMUL = 1!
  Cum(0).text = Format$(100!, "####.00")
  For I = 1 To NDATA.NDC - 1
    upper_size(I - 1).text = Format$(NDATA.CumSize(I), "0.000E+#")
    Cum(I).text = Format$(NDATA.Fraction(I) * 100, "####.00")
    Call SHOW_CLASS(I)
  Next I
  For I = NDATA.NDC To 20
    Call HIDE_CLASS(I)
  Next I
  OptSize(3).Value = -1
  ChkRR.Value = vbUnchecked
  Call HIDERR
  If NDATA.NDC > 1 Then
    Me.cmdExportSizeDistribution.Enabled = True
  Else
    Me.cmdExportSizeDistribution.Enabled = False
  End If
    
End Sub

Private Sub LoadNewStream(NDATA As STREAMDATA)
'*********************************************
  Dim CUMUL As Single
  Dim I As Integer
  Nclasses.text = Str$(NDATA.NDC)
  StreamName.text = NDATA.STREAM_NAME
  'Feedrate.Text = Format$(NDATA.FEED_RATE, "#.000E+#")
  'OptFeed(0).Value = -1
  'PercentSolids.Text = Format$(NDATA.PERCENT_SOLIDS, "###.00")
  CUMUL = 1!
  Cum(0).text = Format$(100!, "####.00")
  For I = 1 To NDATA.NDC - 1
    upper_size(I - 1).text = Format$(NDATA.CumSize(I), "0.000E+#")
    Cum(I).text = Format$(NDATA.Fraction(I) * 100, "####.00")
    Call SHOW_CLASS(I)
  Next I
  For I = NDATA.NDC To 20
    Call HIDE_CLASS(I)
  Next I
  OptSize(3).Value = -1
End Sub

Private Sub MakeNew(NDATA As SIZEDATA)
'*************************************
  Dim Factor As Single
  Dim N As Integer, I As Integer
  CHANGE_FLAG = 0
  NDATA.NSTR = NSTR
  NDATA.NDC = Val(Nclasses.text)
  'Only 20 size data points
  If NDATA.NDC > 20 Then NDATA.NDC = 20
  NDATA.ID = Sysdata.ID
  NDATA.STREAM_NAME = StreamName.text
  'Convert feed rate to kg/s
  If OptFeed(0).Value = -1 Then Factor = 1!
  If OptFeed(2).Value = -1 Then Factor = 0.27778
  If OptFeed(1).Value = -1 Then Factor = 2000 * 0.45359237 / 3600
  If OptFeed(3).Value = -1 Then Factor = 0.28224
  NDATA.FEED_RATE = Val(FeedRate.text) * Factor
  NDATA.PERCENT_SOLIDS = Val(PercentSolids.text)

  'Convert the size to meters
  Factor = 1
  If OptSize(0).Value = -1 Then Factor = 0.000001
  If OptSize(1).Value = -1 Then Factor = 0.001
  If OptSize(2).Value = -1 Then Factor = 0.01
  If OptSize(4).Value = -1 Then Factor = 0.0254
  NDATA.Fraction(0) = 1#
  NDATA.Fraction(NDATA.NDC) = 0#
  For N = 1 To NDATA.NDC - 1
    NDATA.CumSize(N) = Val(upper_size(N - 1).text) * Factor
    NDATA.Fraction(N) = Val(Cum(N).text) / 100
  Next N
End Sub

Private Sub MakeNewStream(NDATA As STREAMDATA)
'*********************************************
  Dim Factor As Single
  Dim N As Integer, I As Integer
  CHANGE_FLAG = 0
  NDATA.NSTR = NSTR
  NDATA.NDC = Val(Nclasses.text)
  'Only 20 size data points
  If NDATA.NDC > 20 Then NDATA.NDC = 20
  NDATA.ID = Sysdata.ID
  NDATA.STREAM_NAME = StreamName.text

  'Convert the size to meters
  Factor = 1
  If OptSize(0).Value = -1 Then Factor = 0.000001
  If OptSize(1).Value = -1 Then Factor = 0.001
  If OptSize(2).Value = -1 Then Factor = 0.01
  If OptSize(4).Value = -1 Then Factor = 0.0254
  NDATA.Fraction(0) = 1#
  NDATA.Fraction(NDATA.NDC) = 0#
  For N = 1 To NDATA.NDC - 1
    NDATA.CumSize(N) = Val(upper_size(N - 1).text) * Factor
    NDATA.Fraction(N) = Val(Cum(N).text) / 100
  Next N
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
'****************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Nclasses.SetFocus
  End If
End Sub

Private Sub NClasses_GotFocus()
'******************************
  Instruction.Visible = -1
  Instruction.Caption = "Number of sizes in the data. Can differ from the number of size classes."
End Sub

Private Sub Nclasses_LostFocus()
'*******************************
  Dim I As Integer
  If Val(Nclasses.text) <= 0 Then
    MsgBox "The number of size classes must be specified", 16, "ERROR"
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
    Call MakeNew(NEWDATA)
    For I = 2 To NEWDATA.NDC
      Call SHOW_CLASS(I - 1)
    Next I
    For I = NEWDATA.NDC To 19
      Call HIDE_CLASS(I)
    Next I
  End If
  CHANGE_FLAG = 0
End Sub

Private Sub OptCurrent_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRDATA(CURR_INDEX))
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

Private Sub PercentSolids_Change()
'*********************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub PercentSolids_Gotfocus()
'***********************************
  Instruction.Caption = "Specify percent solids for this feed stream"
  Instruction.Visible = -1
End Sub

Private Sub PercentSolids_LostFocus()
'************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
  Instruction.Visible = 0
End Sub

Private Sub SHOWRR()
'*******************
    LblRR(0).Visible = -1
    RRparameter(0).Visible = -1
    LblRR(1).Visible = -1
    RRparameter(1).Visible = -1
    LblRR(0).Enabled = True
    RRparameter(0).Enabled = True
    LblRR(1).Enabled = True
    RRparameter(1).Enabled = True
End Sub

Private Sub SHOW_CLASS(I As Integer)
'***********************************
    upper_size(I - 1).Visible = -1
    Cum(I).Visible = -1
End Sub

Private Sub RRparameter_Change(Index As Integer)
'***********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub
Private Sub RRparameter_lostFocus(Index As Integer)
'**************************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
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
  Instruction.Caption = "Specify a descriptive name for the stream. Must start with an alphabetic character"
  Instruction.Visible = -1
End Sub

Private Sub StreamName_Lostfocus()
'*********************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    Sysdata.StreamList.RemoveItem Sysdata.IND
    Sysdata.StreamList.AddItem CStr(NSTR) & " " & NEWDATA.STREAM_NAME, Sysdata.IND
  End If
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

Sub OptFeed_Click(Index As Integer)
'***********************************
  If Screen.ActiveControl.Tag = "Feed" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub OptFeed_LostFocus(Index As Integer)
'***************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptFeed_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = kgps_stonphr_tonnephr_ltonphr
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptFeed(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  FeedRate.text = Format(Val(FeedRate.text) * Factor, "0.0000E+#")
  OptFeed(Index).Value = True
End If
End Sub


Private Sub Upper_size_Change(Index As Integer)
'**********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Upper_size_GotFocus(Index As Integer)
'************************************************
  Instruction.Caption = "Specify mesh size"
  Instruction.Visible = -1
End Sub

Private Sub upper_size_KeyPress(Index As Integer, KeyAscii As Integer)
'*********************************************************************
  If KeyAscii = 13 Then
    KeyAscii = 0
    If Index > 0 Then
      If Val(upper_size(Index).text) > Val(upper_size(Index - 1).text) Then
        MsgBox "Passing sizes must decrease", 0, "ERROR"
        upper_size(Index).SetFocus
        Exit Sub
      ElseIf Val(upper_size(Index + 1).text) > Val(upper_size(Index).text) Then
        MsgBox "Passing size must decrease", 0, "ERROR"
        upper_size(Index + 1).SetFocus
        Exit Sub
      End If
    End If
    Cum(Index + 1).SetFocus
  End If
End Sub

Private Sub Upper_size_LostFocus(Index As Integer)
'*************************************************
  Instruction.Visible = 0
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub
