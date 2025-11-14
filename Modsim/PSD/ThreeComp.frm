VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form ThreeComp 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "METGRAPH Three-component bar chart"
   ClientHeight    =   7170
   ClientLeft      =   150
   ClientTop       =   810
   ClientWidth     =   11490
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7170
   ScaleWidth      =   11490
   Begin VB.CheckBox ChkSubHeading 
      Caption         =   "Show on graph"
      Height          =   255
      Left            =   8160
      TabIndex        =   116
      Top             =   480
      Width           =   1455
   End
   Begin VB.CheckBox ChkHeading 
      Caption         =   "Show on graph"
      Height          =   255
      Left            =   8160
      TabIndex        =   115
      Top             =   120
      Width           =   1455
   End
   Begin VB.TextBox TxtNames 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   255
      Index           =   2
      Left            =   6960
      TabIndex        =   5
      Top             =   960
      Width           =   1095
   End
   Begin VB.TextBox TxtNames 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   255
      Index           =   1
      Left            =   4800
      TabIndex        =   4
      Top             =   960
      Width           =   1095
   End
   Begin VB.TextBox TxtNames 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   255
      Index           =   0
      Left            =   2640
      TabIndex        =   3
      Top             =   960
      Width           =   1095
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   10800
      Top             =   120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.CommandButton CmdViewGraph 
      Caption         =   "View Graph"
      Height          =   615
      Left            =   9720
      TabIndex        =   52
      Top             =   840
      Width           =   1575
   End
   Begin VB.TextBox TxtParam 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   10440
      TabIndex        =   8
      Text            =   "60"
      Top             =   2760
      Width           =   855
   End
   Begin VB.TextBox TxtParam 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   10440
      TabIndex        =   7
      Text            =   "15"
      Top             =   2520
      Width           =   855
   End
   Begin VB.TextBox TxtParam 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   10440
      TabIndex        =   6
      Text            =   "500"
      Top             =   2280
      Width           =   855
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   10800
      TabIndex        =   51
      Top             =   5760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   10800
      TabIndex        =   50
      Top             =   5280
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   33
      Left            =   10800
      TabIndex        =   49
      Top             =   5040
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   10800
      TabIndex        =   48
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   7920
      TabIndex        =   47
      Top             =   5760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   34
      Left            =   7920
      TabIndex        =   46
      Top             =   5520
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   40
      Left            =   7920
      TabIndex        =   45
      Top             =   5280
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   32
      Left            =   7920
      TabIndex        =   44
      Top             =   5040
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   7920
      TabIndex        =   43
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   25
      Left            =   5040
      TabIndex        =   42
      Top             =   6240
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   35
      Left            =   5040
      TabIndex        =   41
      Top             =   6000
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   41
      Left            =   5040
      TabIndex        =   40
      Top             =   5760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   42
      Left            =   5040
      TabIndex        =   39
      Top             =   5520
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   39
      Left            =   5040
      TabIndex        =   38
      Top             =   5280
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   31
      Left            =   5040
      TabIndex        =   37
      Top             =   5040
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   5040
      TabIndex        =   36
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   26
      Left            =   2160
      TabIndex        =   35
      Top             =   6720
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   36
      Left            =   2160
      TabIndex        =   34
      Top             =   6480
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   27
      Left            =   2160
      TabIndex        =   33
      Top             =   6240
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   37
      Left            =   2160
      TabIndex        =   32
      Top             =   6000
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   28
      Left            =   2160
      TabIndex        =   31
      Top             =   5760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   38
      Left            =   2160
      TabIndex        =   30
      Top             =   5520
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   29
      Left            =   2160
      TabIndex        =   29
      Top             =   5280
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   30
      Left            =   2160
      TabIndex        =   28
      Top             =   5040
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   2160
      TabIndex        =   27
      Top             =   4800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   4320
      TabIndex        =   21
      Top             =   3720
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   4320
      TabIndex        =   20
      Top             =   3480
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   4320
      TabIndex        =   19
      Top             =   3240
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   4320
      TabIndex        =   18
      Top             =   3000
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   4320
      TabIndex        =   17
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   7200
      TabIndex        =   26
      Top             =   3720
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   7200
      TabIndex        =   25
      Top             =   3480
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   7200
      TabIndex        =   24
      Top             =   3240
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   7200
      TabIndex        =   23
      Top             =   3000
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   7200
      TabIndex        =   22
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   1440
      TabIndex        =   16
      Top             =   3720
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   1440
      TabIndex        =   15
      Top             =   3480
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   1440
      TabIndex        =   14
      Top             =   3240
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   1440
      TabIndex        =   13
      Top             =   3000
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   1440
      TabIndex        =   12
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   4200
      TabIndex        =   11
      Top             =   1800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   2520
      TabIndex        =   10
      Top             =   1800
      Width           =   615
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   840
      TabIndex        =   9
      Top             =   1800
      Width           =   615
   End
   Begin VB.TextBox TxtHeading 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   1800
      TabIndex        =   2
      Top             =   480
      Width           =   6135
   End
   Begin VB.TextBox TxtHeading 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   1800
      TabIndex        =   1
      Top             =   120
      Width           =   6135
   End
   Begin VB.Label Label9 
      Caption         =   "Phase 2"
      Height          =   255
      Index           =   2
      Left            =   6240
      TabIndex        =   114
      Top             =   960
      Width           =   615
   End
   Begin VB.Label Label9 
      Caption         =   "Phase 1"
      Height          =   255
      Index           =   1
      Left            =   4080
      TabIndex        =   113
      Top             =   960
      Width           =   615
   End
   Begin VB.Label Label9 
      Caption         =   "Phase 0"
      Height          =   255
      Index           =   0
      Left            =   1920
      TabIndex        =   112
      Top             =   960
      Width           =   615
   End
   Begin VB.Label Label8 
      Caption         =   "Component names"
      Height          =   255
      Left            =   120
      TabIndex        =   111
      Top             =   960
      Width           =   1455
   End
   Begin VB.Label LblParam 
      Caption         =   "Colatitude angle (0 - 90 deg)"
      Height          =   255
      Index           =   0
      Left            =   8280
      TabIndex        =   110
      Top             =   2760
      Width           =   2175
   End
   Begin VB.Label LblParam 
      Caption         =   "Azimuthal angle (0 - 360 deg)"
      Height          =   255
      Index           =   1
      Left            =   8280
      TabIndex        =   109
      Top             =   2520
      Width           =   2175
   End
   Begin VB.Label Label7 
      Caption         =   "Graph size (100 - 700)"
      Height          =   255
      Index           =   0
      Left            =   8280
      TabIndex        =   108
      Top             =   2280
      Width           =   1695
   End
   Begin VB.Label Label6 
      Caption         =   "VIEW PARAMETERS"
      Height          =   255
      Left            =   8280
      TabIndex        =   107
      Top             =   1920
      Width           =   1695
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     80 -100    0 - 20"
      Height          =   255
      Index           =   33
      Left            =   8760
      TabIndex        =   106
      Top             =   5760
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     60 - 80   20 - 40"
      Height          =   255
      Index           =   32
      Left            =   8760
      TabIndex        =   105
      Top             =   5280
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     60 - 80     0 - 20"
      Height          =   255
      Index           =   31
      Left            =   8760
      TabIndex        =   104
      Top             =   5040
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40     60 - 80     0 - 20"
      Height          =   255
      Index           =   30
      Left            =   8760
      TabIndex        =   103
      Top             =   4800
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     40 - 60   40 - 60"
      Height          =   255
      Index           =   29
      Left            =   5880
      TabIndex        =   102
      Top             =   5760
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     40 - 60   20 - 40"
      Height          =   255
      Index           =   28
      Left            =   5880
      TabIndex        =   101
      Top             =   5520
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40     40 - 60   20 - 40"
      Height          =   255
      Index           =   27
      Left            =   5880
      TabIndex        =   100
      Top             =   5280
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40     40 - 60     0 - 20"
      Height          =   255
      Index           =   26
      Left            =   5880
      TabIndex        =   99
      Top             =   5040
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  40 - 60     40 - 60     0 - 20"
      Height          =   255
      Index           =   25
      Left            =   5880
      TabIndex        =   98
      Top             =   4800
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     20 - 40   60 - 80"
      Height          =   255
      Index           =   40
      Left            =   3000
      TabIndex        =   97
      Top             =   6240
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20     20 - 40   40 - 60"
      Height          =   255
      Index           =   39
      Left            =   3000
      TabIndex        =   96
      Top             =   6000
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40     20 - 40   40 - 60"
      Height          =   255
      Index           =   24
      Left            =   3000
      TabIndex        =   95
      Top             =   5760
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40     20 - 40   20 - 40"
      Height          =   255
      Index           =   23
      Left            =   3000
      TabIndex        =   94
      Top             =   5520
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  40 - 60     20 - 40   20 - 40"
      Height          =   255
      Index           =   22
      Left            =   3000
      TabIndex        =   93
      Top             =   5280
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  40 - 60     20 - 40     0 - 20"
      Height          =   255
      Index           =   21
      Left            =   3000
      TabIndex        =   92
      Top             =   5040
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  60 - 80     20 - 40     0 - 20"
      Height          =   255
      Index           =   20
      Left            =   3000
      TabIndex        =   91
      Top             =   4800
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20      0 - 20  80 - 100"
      Height          =   255
      Index           =   38
      Left            =   120
      TabIndex        =   90
      Top             =   6720
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "    0 - 20      0 - 20   60 - 80"
      Height          =   255
      Index           =   37
      Left            =   120
      TabIndex        =   89
      Top             =   6480
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40      0 - 20   60 - 80"
      Height          =   255
      Index           =   36
      Left            =   120
      TabIndex        =   88
      Top             =   6240
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  20 - 40      0 - 20   40 - 60"
      Height          =   255
      Index           =   35
      Left            =   120
      TabIndex        =   87
      Top             =   6000
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  40 - 60      0 - 20   40 - 60"
      Height          =   255
      Index           =   19
      Left            =   120
      TabIndex        =   86
      Top             =   5760
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  40 - 60      0 - 20   20 - 40"
      Height          =   255
      Index           =   18
      Left            =   120
      TabIndex        =   85
      Top             =   5520
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  60 - 80      0 - 20   20 - 40"
      Height          =   255
      Index           =   17
      Left            =   120
      TabIndex        =   84
      Top             =   5280
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "  60 - 80      0 - 20     0 - 20"
      Height          =   255
      Index           =   16
      Left            =   120
      TabIndex        =   83
      Top             =   5040
      Width           =   1935
   End
   Begin VB.Label LblDesignation 
      Caption         =   "80 - 100      0 - 20     0 - 20"
      Height          =   255
      Index           =   15
      Left            =   120
      TabIndex        =   82
      Top             =   4800
      Width           =   1935
   End
   Begin VB.Label LblDesignation 
      Caption         =   " 0 - 20    80 - 100"
      Height          =   255
      Index           =   14
      Left            =   3000
      TabIndex        =   81
      Top             =   3720
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "20 - 40     60 - 80"
      Height          =   255
      Index           =   13
      Left            =   3000
      TabIndex        =   80
      Top             =   3480
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "40 - 60     40 - 60"
      Height          =   255
      Index           =   12
      Left            =   3000
      TabIndex        =   79
      Top             =   3240
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "60 - 80     20 - 40"
      Height          =   255
      Index           =   11
      Left            =   3000
      TabIndex        =   78
      Top             =   3000
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "80 - 100     0 - 20"
      Height          =   255
      Index           =   10
      Left            =   3000
      TabIndex        =   77
      Top             =   2760
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   " 0 - 20    80 - 100"
      Height          =   255
      Index           =   9
      Left            =   5880
      TabIndex        =   76
      Top             =   3720
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "20 - 40     60 - 80"
      Height          =   255
      Index           =   8
      Left            =   5880
      TabIndex        =   75
      Top             =   3480
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "40 - 60     40 - 60"
      Height          =   255
      Index           =   7
      Left            =   5880
      TabIndex        =   74
      Top             =   3240
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "60 - 80     20 - 40"
      Height          =   255
      Index           =   6
      Left            =   5880
      TabIndex        =   73
      Top             =   3000
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "80 - 100     0 - 20"
      Height          =   255
      Index           =   5
      Left            =   5880
      TabIndex        =   72
      Top             =   2760
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   " 0 - 20    80 - 100"
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   71
      Top             =   3720
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "20 - 40     60 - 80"
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   70
      Top             =   3480
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "40 - 60     40 - 60"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   69
      Top             =   3240
      Width           =   1335
   End
   Begin VB.Label LblDesignation 
      Caption         =   "60 - 80     20 - 40"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   68
      Top             =   3000
      Width           =   1335
   End
   Begin VB.Label Label5 
      Caption         =   "TERNARY MIXTURES"
      Height          =   255
      Left            =   120
      TabIndex        =   67
      Top             =   4200
      Width           =   1695
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0  Phase 1  Phase 2"
      Height          =   255
      Index           =   9
      Left            =   8760
      TabIndex        =   66
      Top             =   4440
      Width           =   1935
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0  Phase 1  Phase 2"
      Height          =   255
      Index           =   8
      Left            =   5880
      TabIndex        =   65
      Top             =   4440
      Width           =   2175
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0  Phase 1  Phase 2"
      Height          =   255
      Index           =   7
      Left            =   3000
      TabIndex        =   64
      Top             =   4440
      Width           =   1935
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0  Phase 1  Phase 2"
      Height          =   255
      Index           =   6
      Left            =   120
      TabIndex        =   63
      Top             =   4440
      Width           =   2055
   End
   Begin VB.Label LblDesignation 
      Caption         =   "80 - 100     0 - 20"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   62
      Top             =   2760
      Width           =   1335
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 2  Phase 0"
      Height          =   255
      Index           =   5
      Left            =   3000
      TabIndex        =   61
      Top             =   2400
      Width           =   1335
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 1  Phase 2"
      Height          =   255
      Index           =   4
      Left            =   5880
      TabIndex        =   60
      Top             =   2400
      Width           =   1335
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0  Phase 1"
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   59
      Top             =   2400
      Width           =   1335
   End
   Begin VB.Label Label4 
      Caption         =   "BINARY MIXTURES"
      Height          =   255
      Left            =   120
      TabIndex        =   58
      Top             =   2160
      Width           =   1575
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 2"
      Height          =   255
      Index           =   2
      Left            =   3480
      TabIndex        =   57
      Top             =   1800
      Width           =   735
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 1"
      Height          =   255
      Index           =   1
      Left            =   1800
      TabIndex        =   56
      Top             =   1800
      Width           =   735
   End
   Begin VB.Label LblPhaseHead 
      Caption         =   "Phase 0"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   55
      Top             =   1800
      Width           =   735
   End
   Begin VB.Label Label3 
      Caption         =   "PURE PHASES"
      Height          =   255
      Left            =   120
      TabIndex        =   54
      Top             =   1560
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Sample Identification"
      Height          =   255
      Left            =   240
      TabIndex        =   53
      Top             =   480
      Width           =   1575
   End
   Begin VB.Label Label1 
      Caption         =   "GRAPH HEADING"
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   120
      Width           =   1455
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuOpen 
         Caption         =   "Open"
      End
      Begin VB.Menu MnuOpenStereoSoft 
         Caption         =   "Open StereoSoft file"
         Begin VB.Menu MnuMeasuredLinear 
            Caption         =   "Measured linear"
         End
         Begin VB.Menu MnuEstLinear 
            Caption         =   "Estimated linear"
         End
         Begin VB.Menu MnuVolByVol 
            Caption         =   "Volumetric by volume"
         End
         Begin VB.Menu MnuVolByMass 
            Caption         =   "Volumetric by mass"
         End
      End
      Begin VB.Menu MnuSave 
         Caption         =   "Save"
      End
      Begin VB.Menu MnuSaveAs 
         Caption         =   "Save as..."
      End
      Begin VB.Menu MnuPrintForm 
         Caption         =   "Print form"
      End
      Begin VB.Menu MnuExit 
         Caption         =   "Close"
      End
   End
   Begin VB.Menu MnuView 
      Caption         =   "View"
      Begin VB.Menu MnuViewGraph 
         Caption         =   "View graph"
      End
   End
End
Attribute VB_Name = "ThreeComp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NEWDATA As ThreeComponent
Private SavedFile As String

Private Sub StereoSoft(K As Integer)
'***********************************
Dim I As Integer, J As Integer
Dim DataFile As String
On Error GoTo ErrHandler
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files (*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowOpen
  DataFile = CommonDialog1.FileName
  Open DataFile For Input As 31
  Line Input #31, WSTRING
  Line Input #31, WSTRING
  Line Input #31, WSTRING
  NEWDATA.SUBHEADING = Mid(WSTRING, 10)
  For I = 1 To 10
    Line Input #31, WSTRING
  Next I
  For I = 0 To 42
    Line Input #31, WSTRING
    NEWDATA.Value(MMIAIndex(I) + 1) = Val(Mid(WSTRING, 28 + 14 * K, 13))
  Next I
   Call LoadNew(NEWDATA)
   SavedFile = DataFile
  Caption = "METGRAPH three-component barchart  " & Trim(DataFile)
  Close #31
  Exit Sub
ErrHandler:
  MsgBox "Error reading data file", vbCritical, "ERROR"
  Close #31
  Exit Sub

End Sub

Private Sub CmdViewGraph_Click()
'*******************************
  'Set up the file to transmit data to the graphing program
  Dim FileFound As String
  Call MakeNew(CURR3COMPDATA)
  Call ThreeComponentBarChart(CURR3COMPDATA)
End Sub

Private Sub Form_Load()
'**********************
'Set up the order for MMIA output
Dim I As Integer
MMIAIndex(0) = 0
MMIAIndex(1) = 1
MMIAIndex(2) = 2
MMIAIndex(3) = 3
MMIAIndex(4) = 4
MMIAIndex(5) = 5
MMIAIndex(6) = 6
MMIAIndex(7) = 7
MMIAIndex(8) = 17
MMIAIndex(9) = 16
MMIAIndex(10) = 15
MMIAIndex(11) = 14
MMIAIndex(12) = 13
MMIAIndex(13) = 8
MMIAIndex(14) = 9
MMIAIndex(15) = 10
MMIAIndex(16) = 11
MMIAIndex(17) = 12
MMIAIndex(18) = 18
MMIAIndex(19) = 30
MMIAIndex(20) = 29
MMIAIndex(21) = 38
MMIAIndex(22) = 28
MMIAIndex(23) = 37
MMIAIndex(24) = 27
MMIAIndex(25) = 36
MMIAIndex(26) = 26
MMIAIndex(27) = 19
MMIAIndex(28) = 31
MMIAIndex(29) = 39
MMIAIndex(30) = 42
MMIAIndex(31) = 41
MMIAIndex(32) = 35
MMIAIndex(33) = 25
MMIAIndex(34) = 20
MMIAIndex(35) = 32
MMIAIndex(36) = 40
MMIAIndex(37) = 34
MMIAIndex(38) = 24
MMIAIndex(39) = 21
MMIAIndex(40) = 33
MMIAIndex(41) = 23
MMIAIndex(42) = 22
For I = 0 To 42
  ValIndex(MMIAIndex(I)) = I
Next I
If CURR3COMPDATA.Defined Then
  Call LoadNew(CURR3COMPDATA)
Else
  Call LoadDefault
  Call LoadNew(NEWDATA)
  Call MakeNew(NEWDATA)
  
End If

End Sub


Private Sub MnuEstLinear_Click()
Call StereoSoft(1)
End Sub

Private Sub MnuExit_Click()
'**************************
Me.Hide
End Sub

Private Sub MnuMeasuredLinear_Click()
Call StereoSoft(0)
End Sub

Private Sub MnuOpen_Click()
'**************************
Dim I As Integer
Dim SavedFile As String
On Error GoTo ErrHandler
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files (*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowOpen
  SavedFile = CommonDialog1.FileName
  Call ReadFile(SavedFile)
  Exit Sub
ErrHandler:
  MsgBox "Error reading data file", vbCritical, "ERROR"
  Exit Sub
End Sub


Private Sub MnuPrintForm_Click()
'*******************************
On Error GoTo ErrHandler
PrintForm
ErrHandler:
Exit Sub
End Sub

Private Sub MnuSave_Click()
'**************************
Dim IFile As Integer, I As Integer

  On Error GoTo ErrHandler
  If SavedFile <> "" Then
    Call SaveFile(SavedFile)
  Else
    Call MnuSaveAs_Click
  End If
  Exit Sub

ErrHandler:
  MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub MnuSaveAs_Click()
'****************************
  Dim ReturnFile As String, SavedFile As String
  Dim resp As Integer, IFile As Integer
  Dim I As Integer
  On Error GoTo ErrHandler
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files (*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = 4
  CommonDialog1.ShowSave

  SavedFile = CommonDialog1.FileName
  If CommonDialog1.FilterIndex = 1 Then
    'Strip off any extensions
    I = InStr(SavedFile, ".")
    If I > 0 Then
     SavedFile = Mid(SavedFile, 1, I - 1) & ".dat"
    End If
  End If
  ReturnFile = Dir(CommonDialog1.FileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A job with this name already exists.  Overwrite?", vbOKCancel, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  Caption = "METGRAPH three-component barchart  " & Trim(SavedFile)
  Call SaveFile(SavedFile)
ErrHandler:
  Exit Sub
End Sub

Private Sub LoadDefault()
'************************
Dim I As Integer
NEWDATA.HEADING = "Three Component Liberation Distribution"
NEWDATA.ChkHeading = True
NEWDATA.SUBHEADING = "Default data set"
NEWDATA.ChkSubHeading = True
NEWDATA.Azimuth = 15
NEWDATA.Colatitude = 60
NEWDATA.size = 500
NEWDATA.Component(1) = "Mineral 1"
NEWDATA.Component(2) = "Mineral 2"
NEWDATA.Component(3) = "Mineral 3"
For I = 1 To 43
  NEWDATA.Value(I) = 44 - I
Next I

End Sub
Private Sub LoadNew(NDATA As ThreeComponent)
'*******************************************
Dim I As Integer
TxtHeading(0).text = NDATA.HEADING
If NDATA.ChkHeading Then ChkHeading.Value = vbChecked
TxtHeading(1).text = NDATA.SUBHEADING
If NDATA.ChkSubHeading Then ChkSubHeading.Value = vbChecked
TxtParam(0).text = NDATA.size
TxtParam(1).text = NDATA.Azimuth
TxtParam(2).text = NDATA.Colatitude
For I = 1 To 3
  TxtNames(I - 1).text = NDATA.Component(I)
Next I
For I = 1 To 43
  TxtValue(I - 1).text = NDATA.Value(I)
Next I
End Sub
Private Sub MakeNew(NDATA As ThreeComponent)
'******************************************
Dim I As Integer, MaxValue As Single
NDATA.HEADING = TxtHeading(0).text
If ChkHeading.Value = vbChecked Then
  NDATA.ChkHeading = True
Else
  NDATA.ChkHeading = False
End If
NDATA.SUBHEADING = TxtHeading(1).text
If ChkSubHeading.Value = vbChecked Then
  NDATA.ChkSubHeading = True
Else
  NDATA.ChkSubHeading = False
End If
NDATA.size = Val(TxtParam(0).text)
NDATA.Azimuth = Val(TxtParam(1).text)
NDATA.Colatitude = Val(TxtParam(2).text)
NDATA.SCALEValue = 1#
For I = 1 To 3
  NDATA.Component(I) = TxtNames(I - 1)
Next I
NDATA.MaxValue = 0
For I = 1 To 43
  NDATA.Value(I) = Val(TxtValue(I - 1).text)
  If NDATA.Value(I) > NDATA.MaxValue Then NDATA.MaxValue = NDATA.Value(I)
Next I
If NDATA.MaxValue > 80 Then
  NDATA.SCALEValue = 100
ElseIf NDATA.MaxValue > 60 Then
  NDATA.SCALEValue = 80
ElseIf NDATA.MaxValue > 40 Then
  NDATA.SCALEValue = 60
ElseIf NDATA.MaxValue > 20 Then
  NDATA.SCALEValue = 40
ElseIf NDATA.MaxValue > 1 Then
  NDATA.SCALEValue = 20
ElseIf NDATA.MaxValue > 0.8 Then
  NDATA.SCALEValue = 1#
ElseIf NDATA.MaxValue > 0.6 Then
  NDATA.SCALEValue = 0.8
ElseIf NDATA.MaxValue > 0.4 Then
  NDATA.SCALEValue = 0.6
ElseIf NDATA.MaxValue > 0.2 Then
  NDATA.SCALEValue = 0.4
Else
  NDATA.SCALEValue = 0.2
End If
End Sub

Sub SaveFile(FileName As String)
'*******************************
Dim I As Integer, IFile As Integer
On Error GoTo ErrHandler
  Call MakeNew(NEWDATA)
  IFile = FreeFile
  Open Trim(FileName) For Output As #IFile
  If ChkHeading = vbChecked Then
    Print #IFile, "T"
  Else
    Print #IFile, "F"
  End If
  Print #IFile, NEWDATA.HEADING
  If ChkSubHeading = vbChecked Then
    Print #IFile, "T"
  Else
    Print #IFile, "F"
  End If
  Print #IFile, NEWDATA.SUBHEADING
  Print #IFile, NEWDATA.size;
  Print #IFile, NEWDATA.Azimuth;
  Print #IFile, NEWDATA.Colatitude;
  Print #IFile, NEWDATA.SCALEValue
  For I = 1 To 3
    Print #IFile, NEWDATA.Component(I)
  Next I
  For I = 1 To 43
    Print #IFile, NEWDATA.Value(I)
  Next I
  Close (IFile)
  Me.Caption = FileName
Exit Sub

ErrHandler:
MsgBox "Error saving file", vbCritical, "ERROR"
Exit Sub
End Sub

Sub ReadFile(FileName As String)
'*******************************
Dim I As Integer, IFile As Integer
Dim WSTRING As String
On Error GoTo ErrHandler
  IFile = FreeFile
  Open Trim(FileName) For Input As #IFile
  Line Input #IFile, WSTRING
  If WSTRING = "T" Then
    ChkHeading.Value = vbChecked
  Else
    ChkHeading.Value = vbUnchecked
  End If
  Line Input #IFile, NEWDATA.HEADING
  Line Input #IFile, WSTRING
  If WSTRING = "T" Then
    ChkSubHeading = vbChecked
  Else
    ChkSubHeading = vbUnchecked
  End If
  Line Input #IFile, NEWDATA.SUBHEADING
  Input #IFile, NEWDATA.size, NEWDATA.Azimuth, NEWDATA.Colatitude, NEWDATA.SCALEValue
  For I = 1 To 3
    Input #IFile, NEWDATA.Component(I)
  Next I
  For I = 1 To 43
    Input #IFile, NEWDATA.Value(I)
  Next I
  Close (IFile)
  Call LoadNew(NEWDATA)
  Me.Caption = "METGRAPH three-component barchart  " & Trim(FileName)
Exit Sub

ErrHandler:
MsgBox "Error reading file", vbCritical, "ERROR"
Exit Sub
End Sub

Private Sub MnuViewGraph_Click()
'*******************************
Call CmdViewGraph_Click
End Sub


Private Sub MnuVolByMass_Click()
Call StereoSoft(3)
End Sub

Private Sub MnuVolByVol_Click()
Call StereoSoft(2)
End Sub

Private Sub TxtValue_KeyPress(Index As Integer, keyAscii As Integer)
'*******************************************************************
If ValIndex(Index) < 42 Then
  If keyAscii = 13 Then
    TxtValue(MMIAIndex(ValIndex(Index) + 1)).SetFocus
    keyAscii = 0
  End If
End If
End Sub
