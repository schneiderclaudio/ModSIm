VERSION 5.00
Begin VB.Form RajM 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   5190
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   10185
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
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5190
   ScaleWidth      =   10185
   Begin VB.CommandButton cmdExportSBvaluesbutton 
      Caption         =   "Export to file"
      Enabled         =   0   'False
      Height          =   375
      Left            =   5160
      TabIndex        =   162
      Top             =   720
      Width           =   2535
   End
   Begin VB.CommandButton cmdImportSBvaluesbutton 
      Caption         =   "Import from file"
      Enabled         =   0   'False
      Height          =   375
      Left            =   5160
      TabIndex        =   146
      Top             =   240
      Width           =   2535
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   136
      Left            =   3960
      TabIndex        =   142
      Top             =   4440
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Enabled         =   0   'False
      Height          =   210
      Index           =   135
      Left            =   9600
      TabIndex        =   141
      Text            =   "1.000"
      ToolTipText     =   "This should always be one (1)"
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   134
      Left            =   9000
      TabIndex        =   140
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   133
      Left            =   9000
      TabIndex        =   139
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   132
      Left            =   8400
      TabIndex        =   138
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   131
      Left            =   8400
      TabIndex        =   137
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   130
      Left            =   8400
      TabIndex        =   136
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   129
      Left            =   7800
      TabIndex        =   135
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   128
      Left            =   7800
      TabIndex        =   134
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   127
      Left            =   7800
      TabIndex        =   133
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   126
      Left            =   7800
      TabIndex        =   132
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   125
      Left            =   7200
      TabIndex        =   131
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   124
      Left            =   7200
      TabIndex        =   130
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   123
      Left            =   7200
      TabIndex        =   129
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   122
      Left            =   7200
      TabIndex        =   128
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   121
      Left            =   7200
      TabIndex        =   127
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   120
      Left            =   6600
      TabIndex        =   126
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   119
      Left            =   6600
      TabIndex        =   125
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   118
      Left            =   6600
      TabIndex        =   124
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   117
      Left            =   6600
      TabIndex        =   123
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   116
      Left            =   6600
      TabIndex        =   122
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   115
      Left            =   6600
      TabIndex        =   121
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   114
      Left            =   6000
      TabIndex        =   120
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   113
      Left            =   6000
      TabIndex        =   119
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   112
      Left            =   6000
      TabIndex        =   118
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   111
      Left            =   6000
      TabIndex        =   117
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   110
      Left            =   6000
      TabIndex        =   116
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   109
      Left            =   6000
      TabIndex        =   115
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   108
      Left            =   6000
      TabIndex        =   114
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   107
      Left            =   5400
      TabIndex        =   113
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   106
      Left            =   5400
      TabIndex        =   112
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   105
      Left            =   5400
      TabIndex        =   111
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   104
      Left            =   5400
      TabIndex        =   110
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   103
      Left            =   5400
      TabIndex        =   109
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   102
      Left            =   5400
      TabIndex        =   108
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   101
      Left            =   5400
      TabIndex        =   107
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   100
      Left            =   5400
      TabIndex        =   106
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   99
      Left            =   4800
      TabIndex        =   105
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   98
      Left            =   4800
      TabIndex        =   104
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   97
      Left            =   4800
      TabIndex        =   103
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   96
      Left            =   4800
      TabIndex        =   102
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   95
      Left            =   4800
      TabIndex        =   101
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   94
      Left            =   4800
      TabIndex        =   100
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   93
      Left            =   4800
      TabIndex        =   99
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   92
      Left            =   4800
      TabIndex        =   98
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   91
      Left            =   4800
      TabIndex        =   97
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   90
      Left            =   4200
      TabIndex        =   96
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   89
      Left            =   4200
      TabIndex        =   95
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   88
      Left            =   4200
      TabIndex        =   94
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   87
      Left            =   4200
      TabIndex        =   93
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   86
      Left            =   4200
      TabIndex        =   92
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   85
      Left            =   4200
      TabIndex        =   91
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   84
      Left            =   4200
      TabIndex        =   90
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   83
      Left            =   4200
      TabIndex        =   89
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   82
      Left            =   4200
      TabIndex        =   88
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   81
      Left            =   4200
      TabIndex        =   87
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   80
      Left            =   3600
      TabIndex        =   86
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   79
      Left            =   3600
      TabIndex        =   85
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   78
      Left            =   3600
      TabIndex        =   84
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   77
      Left            =   3600
      TabIndex        =   83
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   76
      Left            =   3600
      TabIndex        =   82
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   75
      Left            =   3600
      TabIndex        =   81
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   74
      Left            =   3600
      TabIndex        =   80
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   73
      Left            =   3600
      TabIndex        =   79
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   72
      Left            =   3600
      TabIndex        =   78
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   71
      Left            =   3600
      TabIndex        =   77
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   70
      Left            =   3600
      TabIndex        =   76
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   69
      Left            =   3000
      TabIndex        =   75
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   68
      Left            =   3000
      TabIndex        =   74
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   67
      Left            =   3000
      TabIndex        =   73
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   66
      Left            =   3000
      TabIndex        =   72
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   65
      Left            =   3000
      TabIndex        =   71
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   64
      Left            =   3000
      TabIndex        =   70
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   63
      Left            =   3000
      TabIndex        =   69
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   62
      Left            =   3000
      TabIndex        =   68
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   61
      Left            =   3000
      TabIndex        =   67
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   60
      Left            =   3000
      TabIndex        =   66
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   59
      Left            =   3000
      TabIndex        =   65
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   58
      Left            =   3000
      TabIndex        =   64
      Top             =   1200
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   57
      Left            =   2400
      TabIndex        =   63
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   56
      Left            =   2400
      TabIndex        =   62
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   55
      Left            =   2400
      TabIndex        =   61
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   54
      Left            =   2400
      TabIndex        =   60
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   53
      Left            =   2400
      TabIndex        =   59
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   52
      Left            =   2400
      TabIndex        =   58
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   51
      Left            =   2400
      TabIndex        =   57
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   50
      Left            =   2400
      TabIndex        =   56
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   49
      Left            =   2400
      TabIndex        =   55
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   48
      Left            =   2400
      TabIndex        =   54
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   47
      Left            =   2400
      TabIndex        =   53
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   46
      Left            =   2400
      TabIndex        =   52
      Top             =   1200
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   45
      Left            =   2400
      TabIndex        =   51
      Top             =   960
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   44
      Left            =   1800
      TabIndex        =   50
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   43
      Left            =   1800
      TabIndex        =   49
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   42
      Left            =   1800
      TabIndex        =   48
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   41
      Left            =   1800
      TabIndex        =   47
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   40
      Left            =   1800
      TabIndex        =   46
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   39
      Left            =   1800
      TabIndex        =   45
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   38
      Left            =   1800
      TabIndex        =   44
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   37
      Left            =   1800
      TabIndex        =   43
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   36
      Left            =   1800
      TabIndex        =   42
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   35
      Left            =   1800
      TabIndex        =   41
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   34
      Left            =   1800
      TabIndex        =   40
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   33
      Left            =   1800
      TabIndex        =   39
      Top             =   1200
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   32
      Left            =   1800
      TabIndex        =   38
      Top             =   960
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   31
      Left            =   1800
      TabIndex        =   37
      Top             =   720
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   30
      Left            =   1200
      TabIndex        =   36
      Top             =   3840
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   29
      Left            =   1200
      TabIndex        =   35
      Top             =   3600
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   28
      Left            =   1200
      TabIndex        =   34
      Top             =   3360
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   27
      Left            =   1200
      TabIndex        =   33
      Top             =   3120
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   26
      Left            =   1200
      TabIndex        =   32
      Top             =   2880
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   25
      Left            =   1200
      TabIndex        =   31
      Top             =   2640
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   1200
      TabIndex        =   30
      Top             =   2400
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   23
      Left            =   1200
      TabIndex        =   29
      Top             =   2160
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   1200
      TabIndex        =   28
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   1200
      TabIndex        =   27
      Top             =   1680
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   1200
      TabIndex        =   26
      Top             =   1440
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   1200
      TabIndex        =   25
      Top             =   1200
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   1200
      TabIndex        =   24
      Top             =   960
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   1200
      TabIndex        =   23
      Top             =   720
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   1200
      TabIndex        =   22
      Top             =   480
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Enabled         =   0   'False
      Height          =   225
      Index           =   15
      Left            =   120
      TabIndex        =   21
      Text            =   "0.00000"
      ToolTipText     =   "This should always be zero"
      Top             =   3840
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   120
      TabIndex        =   20
      Top             =   3600
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   120
      TabIndex        =   19
      Top             =   3360
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   120
      TabIndex        =   18
      Top             =   3120
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   120
      TabIndex        =   17
      Top             =   2880
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   120
      TabIndex        =   16
      Top             =   2640
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   120
      TabIndex        =   15
      Top             =   2400
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   120
      TabIndex        =   14
      Top             =   2160
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   120
      TabIndex        =   13
      Top             =   1920
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   120
      TabIndex        =   12
      Top             =   1680
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   120
      TabIndex        =   11
      Top             =   1440
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   120
      TabIndex        =   10
      Top             =   1200
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   120
      TabIndex        =   9
      Top             =   960
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   120
      TabIndex        =   8
      Top             =   720
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   120
      TabIndex        =   7
      Top             =   480
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   210
      Index           =   0
      Left            =   120
      TabIndex        =   1
      Top             =   240
      Width           =   735
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   7920
      TabIndex        =   5
      Top             =   4440
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   9120
      TabIndex        =   4
      Top             =   4440
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   8400
      TabIndex        =   0
      Top             =   120
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display data set that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   1185
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
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "15"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   17
      Left            =   9600
      TabIndex        =   161
      Top             =   3600
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "14"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   16
      Left            =   9000
      TabIndex        =   160
      Top             =   3360
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "13"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   15
      Left            =   8400
      TabIndex        =   159
      Top             =   3120
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "12"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   14
      Left            =   7800
      TabIndex        =   158
      Top             =   2880
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "11"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   13
      Left            =   7200
      TabIndex        =   157
      Top             =   2640
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "10"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   12
      Left            =   6600
      TabIndex        =   156
      Top             =   2400
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "9"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   11
      Left            =   6000
      TabIndex        =   155
      Top             =   2160
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "8"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   10
      Left            =   5400
      TabIndex        =   154
      Top             =   1920
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "7"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   9
      Left            =   4800
      TabIndex        =   153
      Top             =   1680
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "6"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   8
      Left            =   4200
      TabIndex        =   152
      Top             =   1440
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "5"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   7
      Left            =   3600
      TabIndex        =   151
      Top             =   1200
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "4"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   6
      Left            =   3000
      TabIndex        =   150
      Top             =   960
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "3"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   5
      Left            =   2400
      TabIndex        =   149
      Top             =   720
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "2"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   4
      Left            =   1800
      TabIndex        =   148
      Top             =   480
      Width           =   495
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      BackColor       =   &H00FF8080&
      Caption         =   "1"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   3
      Left            =   1200
      TabIndex        =   147
      Top             =   240
      Width           =   495
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Av. residence time, min."
      Height          =   255
      Index           =   2
      Left            =   2040
      TabIndex        =   145
      Top             =   4440
      Width           =   1935
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "b values"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   1
      Left            =   5280
      TabIndex        =   144
      Top             =   1320
      Width           =   735
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "S values 1/min"
      Height          =   255
      Index           =   0
      Left            =   0
      TabIndex        =   143
      Top             =   0
      Width           =   1095
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
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "RajM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
'*********************
Dim Sum As Single
Dim I As Integer

  'Makes a new copy of the current data for this model when the accept button is clicked.
  Call MakeNew(CURRMODELDATA(ID))
  
  'Check is breakage function adds to 1.0000
   Sum = 0#
   For I = 17 To 31
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 1 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 32 To 45
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 2 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 46 To 58
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 3 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 59 To 70
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 4 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 71 To 81
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 5 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 82 To 91
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 6 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 92 To 100
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 7 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 101 To 108
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 8 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 109 To 115
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 9 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 116 To 121
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 10 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 122 To 126
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 11 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 127 To 130
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 12 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 131 To 133
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 13 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
   Sum = 0#
   For I = 134 To 135
      Sum = Sum + CURRMODELDATA(ID).PARAM(I)
   Next I
   If Sum > 1.00009 Or Sum < 0.99991 Then
      MsgBox "Column 14 of BF adds to " & CStr(Sum), 15, "DATA INCONSISTENCY"
      Exit Sub
   End If
 
  Unload Me
End Sub

Private Sub CmdCancel_Click()
'*********************
  Unload Me
End Sub

Private Sub cmdExportSBvaluesbutton_Click()
   Call ExportSBData
End Sub
Private Sub ExportSBData()
'**************************************************
  Dim FileName As String, ReturnFile As String, JOBNAME As String
  Dim I As Integer, resp As Integer
  
  On Error GoTo Errhandler1
  ' Set filters
  'MODSIM.CommonDialog1.Filter = "Text files (*.txt)|*.txt"
  'MODSIM.CommonDialog1.FilterIndex = 1
  'MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  'MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  'MODSIM.CommonDialog1.ShowSave
  
  On Error GoTo ErrHandler

  'Strip off any file type extension
  'STRING = Trim(MODSIM.CommonDialog1.FileName)
  'I = InStr(WSTRING, ".")
  'If I > 0 Then
    'FileName = Mid(WSTRING, 1, I - 1)
  'Else
    'FileName = WSTRING
  'End If
  'FileName = FileName & ".txt"
  'ReturnFile = Dir(FileName)
  'If ReturnFile <> "" Then
    'resp = MsgBox("A file with name " & FileName & " already exists.  Overwrite?", 33, "WARNING")
    'If resp = vbCancel Then Exit Sub
  'End If
  
  'Open FileName For Output As #31

  'For I = 0 To 135
    'Print #31, Parameter(I)
  'Next I
  'Close #31
  Exit Sub
  
Errhandler1:
  Exit Sub
  
ErrHandler:
  'Close #31
  'MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub cmdImportSBvaluesbutton_Click()
   Call ImportSBData
End Sub
Private Sub ImportSBData()
'**************************************************
  Dim FileName As String
  Dim I As Integer, resp As Integer, N As Integer
  Dim SBValues(136) As Single
  Dim DataSetFound As Boolean
  
  On Error GoTo Errhandler1
  ' Set filters
  'MODSIM.CommonDialog1.Filter = "Text files (*.txt)|*.txt"
  'MODSIM.CommonDialog1.FilterIndex = 1
  'MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  'MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  'MODSIM.CommonDialog1.ShowOpen
  
  'FileName = MODSIM.CommonDialog1.FileName
  On Error GoTo ErrHandler
  'Open FileName For Input As #31
      'For I = 1 To 136
        'Line Input #31, WSTRING
        'SBValues(I) = Val(WSTRING)
        'N = N + 1
      'Next I
  'Close #31
  ' If I'm here then I've read the damn file
  'If N = 136 Then
     'For I = 1 To 136
       'Parameter(I - 1).text = CStr(SBValues(I))
     'Next I
  'End If
  Exit Sub
Errhandler1:
  Exit Sub
ErrHandler:
  'Close #31
  'MsgBox "Error reading file", vbCritical, "ERROR"
  Exit Sub
  
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
  Caption = Caption & " SB16 for unit " & CStr(UnitMods.Unit) 'Replace ???? with the 4-character model name
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "SB16" Then  'Replace ???? with the 4-character model name
    OptCurrent.Enabled = 0
    Call LoadDefault
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

Private Sub LoadDefault()
'*****************
  'Define the default parameter values here
  Parameter(0).text = CStr(0.012922)
  Parameter(1).text = CStr(0.026698)
  Parameter(2).text = CStr(0.054951)
  Parameter(3).text = CStr(0.111894)
  Parameter(4).text = CStr(0.221318)
  Parameter(5).text = CStr(0.406789)
  Parameter(6).text = CStr(0.639239)
  Parameter(7).text = CStr(0.786996)
  Parameter(8).text = CStr(0.759095)
  Parameter(9).text = CStr(0.625983)
  Parameter(10).text = CStr(0.4797)
  Parameter(11).text = CStr(0.357071)
  Parameter(12).text = CStr(0.262956)
  Parameter(13).text = CStr(0.192904)
  Parameter(14).text = CStr(0.14132)
  Parameter(15).text = CStr(0#)
  Parameter(16).text = CStr(0.3764)
  Parameter(17).text = CStr(0.1337)
  Parameter(18).text = CStr(0.0602)
  Parameter(19).text = CStr(0.0369)
  Parameter(20).text = CStr(0.0287)
  Parameter(21).text = CStr(0.0251)
  Parameter(22).text = CStr(0.0229)
  Parameter(23).text = CStr(0.0212)
  Parameter(24).text = CStr(0.0198)
  Parameter(25).text = CStr(0.0184)
  Parameter(26).text = CStr(0.0172)
  Parameter(27).text = CStr(0.016)
  Parameter(28).text = CStr(0.015)
  Parameter(29).text = CStr(0.014)
  Parameter(30).text = CStr(0.1945)
  Parameter(31).text = CStr(0.3648)
  Parameter(32).text = CStr(0.1311)
  Parameter(33).text = CStr(0.0601)
  Parameter(34).text = CStr(0.0376)
  Parameter(35).text = CStr(0.0295)
  Parameter(36).text = CStr(0.0259)
  Parameter(37).text = CStr(0.0237)
  Parameter(38).text = CStr(0.022)
  Parameter(39).text = CStr(0.0205)
  Parameter(40).text = CStr(0.0191)
  Parameter(41).text = CStr(0.0178)
  Parameter(42).text = CStr(0.0166)
  Parameter(43).text = CStr(0.0155)
  Parameter(44).text = CStr(0.2158)
  Parameter(45).text = CStr(0.353)
  Parameter(46).text = CStr(0.1283)
  Parameter(47).text = CStr(0.06)
  Parameter(48).text = CStr(0.0382)
  Parameter(49).text = CStr(0.0304)
  Parameter(50).text = CStr(0.0268)
  Parameter(51).text = CStr(0.0245)
  Parameter(52).text = CStr(0.0227)
  Parameter(53).text = CStr(0.0212)
  Parameter(54).text = CStr(0.0198)
  Parameter(55).text = CStr(0.0184)
  Parameter(56).text = CStr(0.0172)
  Parameter(57).text = CStr(0.2395)
  Parameter(58).text = CStr(0.3407)
  Parameter(59).text = CStr(0.1255)
  Parameter(60).text = CStr(0.0599)
  Parameter(61).text = CStr(0.0389)
  Parameter(62).text = CStr(0.0313)
  Parameter(63).text = CStr(0.0277)
  Parameter(64).text = CStr(0.0254)
  Parameter(65).text = CStr(0.0235)
  Parameter(66).text = CStr(0.0219)
  Parameter(67).text = CStr(0.0204)
  Parameter(68).text = CStr(0.0191)
  Parameter(69).text = CStr(0.2657)
  Parameter(70).text = CStr(0.3276)
  Parameter(71).text = CStr(0.1226)
  Parameter(72).text = CStr(0.0599)
  Parameter(73).text = CStr(0.0397)
  Parameter(74).text = CStr(0.0322)
  Parameter(75).text = CStr(0.0286)
  Parameter(76).text = CStr(0.0263)
  Parameter(77).text = CStr(0.0244)
  Parameter(78).text = CStr(0.0227)
  Parameter(79).text = CStr(0.0212)
  Parameter(80).text = CStr(0.2948)
  Parameter(81).text = CStr(0.3146)
  Parameter(82).text = CStr(0.1195)
  Parameter(83).text = CStr(0.0598)
  Parameter(84).text = CStr(0.0404)
  Parameter(85).text = CStr(0.0331)
  Parameter(86).text = CStr(0.0295)
  Parameter(87).text = CStr(0.0272)
  Parameter(88).text = CStr(0.0252)
  Parameter(89).text = CStr(0.0235)
  Parameter(90).text = CStr(0.3272)
  Parameter(91).text = CStr(0.3009)
  Parameter(92).text = CStr(0.1164)
  Parameter(93).text = CStr(0.0597)
  Parameter(94).text = CStr(0.0412)
  Parameter(95).text = CStr(0.0341)
  Parameter(96).text = CStr(0.0305)
  Parameter(97).text = CStr(0.0281)
  Parameter(98).text = CStr(0.0261)
  Parameter(99).text = CStr(0.363)
  Parameter(100).text = CStr(0.2868)
  Parameter(101).text = CStr(0.1131)
  Parameter(102).text = CStr(0.0596)
  Parameter(103).text = CStr(0.042)
  Parameter(104).text = CStr(0.0351)
  Parameter(105).text = CStr(0.0315)
  Parameter(106).text = CStr(0.0291)
  Parameter(107).text = CStr(0.4028)
  Parameter(108).text = CStr(0.2722)
  Parameter(109).text = CStr(0.1097)
  Parameter(110).text = CStr(0.0595)
  Parameter(111).text = CStr(0.0428)
  Parameter(112).text = CStr(0.0361)
  Parameter(113).text = CStr(0.0326)
  Parameter(114).text = CStr(0.4471)
  Parameter(115).text = CStr(0.2569)
  Parameter(116).text = CStr(0.1062)
  Parameter(117).text = CStr(0.0594)
  Parameter(118).text = CStr(0.0437)
  Parameter(119).text = CStr(0.0372)
  Parameter(120).text = CStr(0.4966)
  Parameter(121).text = CStr(0.2413)
  Parameter(122).text = CStr(0.1026)
  Parameter(123).text = CStr(0.0593)
  Parameter(124).text = CStr(0.0445)
  Parameter(125).text = CStr(0.5523)
  Parameter(126).text = CStr(0.2249)
  Parameter(127).text = CStr(0.0989)
  Parameter(128).text = CStr(0.0592)
  Parameter(129).text = CStr(0.617)
  Parameter(130).text = CStr(0.208)
  Parameter(131).text = CStr(0.095)
  Parameter(132).text = CStr(0.697)
  Parameter(133).text = CStr(0.1906)
  Parameter(134).text = CStr(0.8094)
  Parameter(135).text = CStr(1#)
  'And the residence time
  Parameter(136).text = CStr(2#)
   

End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  'Makes a new copy of the data for this model whenever the form is changed
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.Model = "SB16" 'Replace ???? with the 4-character model name
  NDATA.NOPAR = 137    'Specify the number of parameters in your model here
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
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
'Print the form
PrintForm

End Sub

Private Sub OptCurrent_Click()
'**********************
  If Screen.ActiveControl.Tag = "Current" Then
        Call LoadNew(CURRMODELDATA(ID))
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
  'Must trigger a load only when NEW is switched on with a click.
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'**************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'*****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation or for curve fitting.
Dim I As Integer, IDIFF As Integer
Dim ParameterCanBeUsed As Boolean
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  ParameterCanBeUsed = False
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.LblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  'If the parameter set up on the form is not sequential, deal with it here
  If Index < 15 Then           'Just let the user optimize the selection function values, except the last one of course
    ParameterCanBeUsed = True
    IDIFF = 1
  End If
  If Index = 136 Then  'in case the user wants to adjust the average residence time
      ParameterCanBeUsed = True
      IDIFF = 1
  End If
  
  If ParameterCanBeUsed Then
    'Check if this parameter is already selected and display if necessary.
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
End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub
