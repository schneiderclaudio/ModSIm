VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Gcdata 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify distribution of particle grades"
   ClientHeight    =   6255
   ClientLeft      =   120
   ClientTop       =   1350
   ClientWidth     =   8490
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
   ScaleHeight     =   6255
   ScaleWidth      =   8490
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   600
      TabIndex        =   135
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
      Left            =   600
      TabIndex        =   134
      Top             =   5280
      Width           =   900
   End
   Begin VB.CommandButton CmdImport 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Import data from file"
      Height          =   615
      Left            =   7080
      TabIndex        =   133
      Tag             =   "Import"
      Top             =   2640
      Width           =   1335
   End
   Begin VB.CommandButton CmdDelete 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Delete"
      Height          =   360
      Left            =   7200
      TabIndex        =   132
      Tag             =   "Delete"
      Top             =   1560
      Width           =   1200
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   7440
      TabIndex        =   26
      Top             =   4200
      Width           =   960
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   5520
      TabIndex        =   131
      Top             =   5280
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   5520
      TabIndex        =   130
      Top             =   5040
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   5520
      TabIndex        =   129
      Top             =   4800
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   5520
      TabIndex        =   128
      Top             =   4560
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   5520
      TabIndex        =   127
      Top             =   4320
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   5520
      TabIndex        =   111
      Top             =   4080
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   5520
      TabIndex        =   110
      Top             =   3840
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   5520
      TabIndex        =   109
      Top             =   3600
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   5520
      TabIndex        =   108
      Top             =   3360
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   5520
      TabIndex        =   107
      Top             =   3120
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   5520
      TabIndex        =   106
      Top             =   2880
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   5520
      TabIndex        =   105
      Top             =   2640
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   5520
      TabIndex        =   104
      Top             =   2400
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   5520
      TabIndex        =   103
      Top             =   2160
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   5520
      TabIndex        =   102
      Top             =   1920
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   5520
      TabIndex        =   101
      Top             =   1680
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   5520
      TabIndex        =   100
      Top             =   1440
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   5520
      TabIndex        =   99
      Top             =   1200
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   5520
      TabIndex        =   98
      Top             =   960
      Width           =   1320
   End
   Begin VB.TextBox SizeRange2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   5520
      TabIndex        =   1
      Top             =   720
      Width           =   1320
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   0
      Left            =   2640
      TabIndex        =   55
      Tag             =   "SizeRange"
      Top             =   720
      Value           =   -1  'True
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   19
      Left            =   2640
      TabIndex        =   116
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   5280
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   18
      Left            =   2640
      TabIndex        =   115
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   5040
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   17
      Left            =   2640
      TabIndex        =   114
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   4800
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   16
      Left            =   2640
      TabIndex        =   113
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   4560
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   15
      Left            =   2640
      TabIndex        =   112
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   4320
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   14
      Left            =   2640
      TabIndex        =   69
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   4080
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   13
      Left            =   2640
      TabIndex        =   68
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   3840
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   12
      Left            =   2640
      TabIndex        =   67
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   3600
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   11
      Left            =   2640
      TabIndex        =   66
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   3360
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   10
      Left            =   2640
      TabIndex        =   65
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   3120
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   9
      Left            =   2640
      TabIndex        =   64
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   2880
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   8
      Left            =   2640
      TabIndex        =   63
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   2640
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   7
      Left            =   2640
      TabIndex        =   62
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   2400
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   6
      Left            =   2640
      TabIndex        =   61
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   2160
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   5
      Left            =   2640
      TabIndex        =   60
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   1920
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   4
      Left            =   2640
      TabIndex        =   59
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   1680
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   3
      Left            =   2640
      TabIndex        =   58
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   1440
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   2
      Left            =   2640
      TabIndex        =   57
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   1200
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.OptionButton OptSizeRange 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
      Height          =   240
      Index           =   1
      Left            =   2640
      TabIndex        =   56
      TabStop         =   0   'False
      Tag             =   "SizeRange"
      Top             =   960
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   3960
      TabIndex        =   121
      Top             =   5280
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   3960
      TabIndex        =   120
      Top             =   5040
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   3960
      TabIndex        =   119
      Top             =   4800
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   3960
      TabIndex        =   118
      Top             =   4560
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   3960
      TabIndex        =   117
      Top             =   4320
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   3960
      TabIndex        =   83
      Top             =   4080
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   3960
      TabIndex        =   82
      Top             =   3840
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   3960
      TabIndex        =   81
      Top             =   3600
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   3960
      TabIndex        =   80
      Top             =   3360
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   3960
      TabIndex        =   79
      Top             =   3120
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   3960
      TabIndex        =   78
      Top             =   2880
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   3960
      TabIndex        =   77
      Top             =   2640
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   3960
      TabIndex        =   76
      Top             =   2400
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   3960
      TabIndex        =   75
      Top             =   2160
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   3960
      TabIndex        =   74
      Top             =   1920
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   3960
      TabIndex        =   73
      Top             =   1680
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   3960
      TabIndex        =   72
      Top             =   1440
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   3960
      TabIndex        =   71
      Top             =   1200
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   3960
      TabIndex        =   70
      Top             =   960
      Visible         =   0   'False
      Width           =   1320
   End
   Begin VB.TextBox SizeRange1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   3960
      TabIndex        =   0
      Top             =   720
      Width           =   1320
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7200
      TabIndex        =   52
      Top             =   360
      Width           =   1215
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   24
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current "
         Height          =   240
         Left            =   120
         TabIndex        =   23
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
         TabIndex        =   22
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
      Height          =   600
      Left            =   7440
      TabIndex        =   25
      Top             =   4920
      Width           =   960
   End
   Begin VB.CommandButton CmdClear 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      Caption         =   "Clear"
      Height          =   375
      Left            =   1680
      TabIndex        =   27
      Top             =   600
      Width           =   840
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   600
      TabIndex        =   21
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
      Left            =   600
      TabIndex        =   20
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
      Left            =   600
      TabIndex        =   19
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
      Left            =   600
      TabIndex        =   18
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
      Left            =   600
      TabIndex        =   17
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
      Left            =   600
      TabIndex        =   16
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
      Left            =   600
      TabIndex        =   15
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
      Left            =   600
      TabIndex        =   14
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
      Left            =   600
      TabIndex        =   13
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
      Left            =   600
      TabIndex        =   12
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
      Left            =   600
      TabIndex        =   11
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
      Left            =   600
      TabIndex        =   10
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
      Left            =   600
      TabIndex        =   9
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
      Left            =   600
      TabIndex        =   8
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
      Left            =   600
      TabIndex        =   7
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
      Left            =   600
      TabIndex        =   6
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
      Left            =   600
      TabIndex        =   5
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
      Left            =   600
      TabIndex        =   4
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
      Left            =   600
      TabIndex        =   3
      Top             =   720
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   600
      TabIndex        =   2
      Top             =   480
      Width           =   900
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   7680
      Top             =   2040
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
      TabIndex        =   137
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
      TabIndex        =   136
      Top             =   5280
      Width           =   600
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   19
      Left            =   5280
      TabIndex        =   126
      Top             =   5280
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   18
      Left            =   5280
      TabIndex        =   125
      Top             =   5040
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   17
      Left            =   5280
      TabIndex        =   124
      Top             =   4800
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   16
      Left            =   5280
      TabIndex        =   123
      Top             =   4560
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   15
      Left            =   5280
      TabIndex        =   122
      Top             =   4320
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   14
      Left            =   5280
      TabIndex        =   97
      Top             =   4080
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   13
      Left            =   5280
      TabIndex        =   96
      Top             =   3840
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   12
      Left            =   5280
      TabIndex        =   95
      Top             =   3600
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   11
      Left            =   5280
      TabIndex        =   94
      Top             =   3360
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   5280
      TabIndex        =   93
      Top             =   3120
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   5280
      TabIndex        =   92
      Top             =   2880
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   5280
      TabIndex        =   91
      Top             =   2640
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   5280
      TabIndex        =   90
      Top             =   2400
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   5280
      TabIndex        =   89
      Top             =   2160
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   5280
      TabIndex        =   88
      Top             =   1920
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   5280
      TabIndex        =   87
      Top             =   1680
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   5280
      TabIndex        =   86
      Top             =   1440
      Visible         =   0   'False
      Width           =   240
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   5280
      TabIndex        =   85
      Top             =   1200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   5280
      TabIndex        =   84
      Top             =   960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " -"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   5280
      TabIndex        =   53
      Top             =   720
      Width           =   255
   End
   Begin VB.Label LblStreamNo 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3480
      TabIndex        =   51
      Top             =   240
      Width           =   3480
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   54
      Top             =   0
      Visible         =   0   'False
      Width           =   8280
   End
   Begin VB.Label LblClass 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Class"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   28
      Top             =   240
      Width           =   480
   End
   Begin VB.Label Stream 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stream:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2640
      TabIndex        =   50
      Top             =   240
      Width           =   855
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
      TabIndex        =   34
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
      TabIndex        =   40
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
      TabIndex        =   39
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
      TabIndex        =   38
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
      TabIndex        =   37
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
      TabIndex        =   36
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
      TabIndex        =   35
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
      TabIndex        =   33
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
      TabIndex        =   41
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
      TabIndex        =   42
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
      TabIndex        =   43
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
      TabIndex        =   44
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
      TabIndex        =   45
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
      TabIndex        =   46
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
      TabIndex        =   47
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
      TabIndex        =   48
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
      TabIndex        =   32
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
      TabIndex        =   31
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
      TabIndex        =   30
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
      TabIndex        =   29
      Top             =   480
      Width           =   600
   End
   Begin VB.Label LblMassFraction 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mass fraction"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   600
      TabIndex        =   49
      Top             =   240
      Width           =   1080
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
Attribute VB_Name = "Gcdata"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private NO_GRADE_RANGES As Integer, NGC As Integer, NEW_GRADE_RANGES As Integer
Private SELECTIND As Integer, ACTIVE_K As Integer
'Only 20 size ranges to group grade distributions or washabilities.
Private NEWGDATA(1 To 20) As GRADEDATA
Private CHANGE_FLAG As Integer
Private Sub AddSizeRange(Index As Integer)
'*****************************************
  Dim K As Integer
  OptNew.Value = -1
  'Make the next row visible
  OptSizeRange(NO_GRADE_RANGES).Visible = -1
  SizeRange1(NO_GRADE_RANGES).Visible = -1
  Label1(NO_GRADE_RANGES).Visible = -1
  SizeRange2(NO_GRADE_RANGES).Visible = -1

  'Bump data from the existing last row
  SizeRange1(NO_GRADE_RANGES).text = SizeRange2(NO_GRADE_RANGES - 1).text
  If Val(SizeRange2(NO_GRADE_RANGES - 1).text) >= Val(Sysdata.D1.text) Then
    SizeRange2(NO_GRADE_RANGES).text = SizeRange2(NO_GRADE_RANGES - 1).text
  Else
    SizeRange2(NO_GRADE_RANGES).text = Sysdata.D1.text
  End If
  NEWGDATA(NO_GRADE_RANGES + 1) = NEWGDATA(NO_GRADE_RANGES)

  'Increment the row count
  NO_GRADE_RANGES = NO_GRADE_RANGES + 1
  NEW_GRADE_RANGES = NO_GRADE_RANGES
  If Index < NO_GRADE_RANGES - 2 Then
    'Bump data from the intermediate rows
    For K = NO_GRADE_RANGES - 1 To Index + 2 Step -1
      SizeRange1(K).text = SizeRange1(K - 1).text
      SizeRange2(K).text = SizeRange2(K - 1).text
      NEWGDATA(K + 1) = NEWGDATA(K)
    Next K
  'End If
  'If Index > 0 Then
    'Set data in the row above the altered row
    SizeRange2(Index + 1).text = SizeRange1(Index + 1).text
    SizeRange1(Index + 1).text = SizeRange2(Index).text
    NEWGDATA(Index + 2) = NEWGDATA(Index + 1)
    OptSizeRange(Index).Value = -1
  End If
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim Cum As Single
  Dim K As Integer, N As Integer
  If SizeRange1(0).text = "" Or SizeRange2(0).text = "" Then
    WSTRING = "Size range has not been specified"
    MsgBox WSTRING, 0, "ERROR"
    Exit Sub
  End If
  CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES = NO_GRADE_RANGES
  For K = 1 To NO_GRADE_RANGES
    ACTIVE_K = K
    Cum = 0!
    For N = 1 To NGC
      Cum = Cum + NEWGDATA(K).Fraction(N)
    Next N
    If Abs(Cum - 1) > 0.000001 Then
      OptSizeRange(K - 1).Value = -1
      Call LoadNew(NEWGDATA())
      WSTRING = "Fractions sum to " + CStr(Cum)
      MsgBox WSTRING, vbCritical, "ERROR"
      Exit Sub
    End If
    CURRGDATA(Psd.CURR_INDEX, K) = NEWGDATA(K)
  Next K

  Unload Gcdata
End Sub

Private Sub CmdCancel_Click()
  Unload Gcdata
End Sub

Private Sub CmdClear_Click()
  Dim N As Integer
  For N = 1 To NGC
    Parameter(N - 1).text = ""
  Next N
  Parameter(0).SetFocus
End Sub

Private Sub CmdDelete_Click()
'****************************
  Dim Index As Integer
  If NO_GRADE_RANGES = 1 Then
    Exit Sub
  End If
  If SELECTIND > -1 Then
    OptNew.Value = -1
    NO_GRADE_RANGES = NO_GRADE_RANGES - 1
    NEW_GRADE_RANGES = NO_GRADE_RANGES
    'Hide the last row.
    OptSizeRange(NO_GRADE_RANGES).Visible = 0
    SizeRange1(NO_GRADE_RANGES).Visible = 0
    Label1(NO_GRADE_RANGES).Visible = 0
    SizeRange2(NO_GRADE_RANGES).Visible = 0
    If SELECTIND < NO_GRADE_RANGES Then
      SizeRange2(SELECTIND).text = SizeRange2(SELECTIND + 1).text
      For Index = SELECTIND + 1 To NO_GRADE_RANGES
        SizeRange1(Index).text = SizeRange1(Index + 1).text
        SizeRange2(Index).text = SizeRange2(Index + 1).text
      Next Index
    End If
    Call MakeNew(NEWGDATA())
    SELECTIND = -1
  End If
End Sub

Private Sub CmdDelete_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Delete the indicated size range"
End Sub

Private Sub CmdDelete_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub Form_Load()
'**********************
  Dim K As Integer
  
  CHANGE_FLAG = 0
  SELECTIND = -1
  OptSizeRange(0).Value = -1
  ACTIVE_K = 1

  LblStreamNo.Caption = Psd.Stream.Caption + " " + Psd.StreamName.text
  NGC = Val(Sysdata.NGCM.text)
  SizeRange1(0).text = Str$(0!)
  SizeRange2(0).text = Sysdata.D1.text
  For K = 1 To 19
    SizeRange2(K).text = Sysdata.D1.text
    SizeRange1(K).Visible = 0
    SizeRange2(K).Visible = 0
  Next K

  'Find the current data for this stream if any
  If Psd.CURR_INDEX = 0 Or CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES = 0 Then
    OptCurrent.Enabled = 0
    LoadDefault
    SizeRange1(0).text = Str$(0)
    SizeRange2(0).text = Sysdata.D1.text
    NO_GRADE_RANGES = 1
    NEW_GRADE_RANGES = NO_GRADE_RANGES
    Call MakeNew(NEWGDATA())
  Else
    NO_GRADE_RANGES = CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES
    NEW_GRADE_RANGES = NO_GRADE_RANGES
    For K = 1 To NO_GRADE_RANGES
      NEWGDATA(K) = CURRGDATA(Psd.CURR_INDEX, K)
      SizeRange1(K - 1).text = Str$(NEWGDATA(K).SIZE_RANGE(1))
      SizeRange2(K - 1).text = Str$(NEWGDATA(K).SIZE_RANGE(2))
      OptSizeRange(K - 1).Visible = -1
      SizeRange1(K - 1).Visible = -1
      Label1(K - 1).Visible = -1
      SizeRange2(K - 1).Visible = -1
    Next K
    Call LoadNew(NEWGDATA())
    Call MakeNew(NEWGDATA())
  End If
  SizeRange1(0).Enabled = 0
End Sub

Private Sub LoadDefault()
'************************
  Dim K As Integer, N As Integer, Nomins As Integer
  Nomins = Val(Sysdata.NoMin.text)
  OptSizeRange(0).Value = -1
  SizeRange1(0).text = Str$(0!)
  SizeRange2(0).text = Sysdata.D1.text
  For K = 1 To 19
    SizeRange2(K).text = Sysdata.D1.text
    OptSizeRange(K).Visible = 0
    SizeRange1(K).Visible = 0
    Label1(K).Visible = 0
    SizeRange2(K).Visible = 0
  Next K
  For N = 1 To NGC
    LblClassNo(N - 1).Visible = -1
    Parameter(N - 1).Visible = -1
    If N <= Nomins Then
      If Nomins <= NGC Then
        Parameter(N - 1).text = Format$(1! / Nomins, "#.0000")
      Else
        Parameter(N - 1).text = Format$(1! / NGC, "#.0000")
      End If
    Else
      Parameter(N - 1).text = Str$(0)
    End If
  Next N
  For N = NGC + 1 To 22
    LblClassNo(N - 1).Visible = 0
    Parameter(N - 1).Visible = 0
  Next N
End Sub

Private Sub LoadNew(NDATA() As GRADEDATA)
'****************************************
  Dim N As Integer, K As Integer
  For N = 1 To NGC
    LblClassNo(N - 1).Visible = -1
    Parameter(N - 1).Visible = -1
    Parameter(N - 1).text = Format$(NDATA(ACTIVE_K).Fraction(N), "0.000000")
  Next N
  For N = NGC + 1 To 22
    LblClassNo(N - 1).Visible = 0
    Parameter(N - 1).Visible = 0
  Next N
  For K = 1 To NO_GRADE_RANGES
    SizeRange1(K - 1).text = Str$(NDATA(K).SIZE_RANGE(1))
    SizeRange2(K - 1).text = Str$(NDATA(K).SIZE_RANGE(2))
    OptSizeRange(K - 1).Visible = -1
    SizeRange1(K - 1).Visible = -1
    Label1(K - 1).Visible = -1
    SizeRange2(K - 1).Visible = -1
  Next K
  For K = NO_GRADE_RANGES + 1 To 20
    OptSizeRange(K - 1).Visible = 0
    SizeRange1(K - 1).Visible = 0
    Label1(K - 1).Visible = 0
    SizeRange2(K - 1).Visible = 0
  Next K
End Sub

Private Sub MakeNew(NDATA() As GRADEDATA)
'****************************************
Dim K As Integer, I As Integer
  CHANGE_FLAG = 0
  NDATA(ACTIVE_K).STREAM_NAME = Psd.StreamName.text
  For K = 1 To NO_GRADE_RANGES
    NDATA(K).SIZE_RANGE(1) = Val(SizeRange1(K - 1).text)
    NDATA(K).SIZE_RANGE(2) = Val(SizeRange2(K - 1).text)
    NDATA(K).NGC = Val(Sysdata.NGCM.text)
  Next K
  NDATA(NO_GRADE_RANGES).SIZE_RANGE(2) = Val(Sysdata.D1)
  NDATA(ACTIVE_K).NSTR = Val(LblStreamNo.Caption)
  For I = 1 To NGC
    NDATA(ACTIVE_K).Fraction(I) = Val(Parameter(I - 1).text)
  Next I
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
'Print the form
PrintForm
End Sub

Private Sub OptCurrent_Click()
'*****************************
  Dim K As Integer
  Dim TEMPDATA(1 To 20) As GRADEDATA
  If Screen.ActiveControl.Tag = "Current" Then
    NO_GRADE_RANGES = CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES
    For K = 1 To NO_GRADE_RANGES
      TEMPDATA(K) = CURRGDATA(Psd.CURR_INDEX, K)
    Next K
    Call LoadNew(TEMPDATA())
  End If
End Sub

Private Sub OptDefault_Click()
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
  If Screen.ActiveControl.Tag = "New" Then
    NO_GRADE_RANGES = NEW_GRADE_RANGES
    Call LoadNew(NEWGDATA())
  End If
End Sub

Private Sub OptSizeRange_Click(Index As Integer)
'***********************************************
  Dim N As Integer
  ACTIVE_K = Index + 1
  If Screen.ActiveControl.Tag = "SizeRange" Then
    If OptNew.Value = -1 Then
      If NEWGDATA(ACTIVE_K).NSTR > 0 Then
        Call LoadNew(NEWGDATA())
      Else
        Call LoadDefault
      End If
      Call MakeNew(NEWGDATA())
    End If
    If OptCurrent.Value = -1 Then
      For N = 1 To NGC
        LblClassNo(N - 1).Visible = -1
        Parameter(N - 1).Visible = -1
        Parameter(N - 1).text = Format$(CURRGDATA(Psd.CURR_INDEX, ACTIVE_K).Fraction(N), "0.0000")
      Next N
    End If
  End If
End Sub

Private Sub OptSizeRange_GotFocus(Index As Integer)
  Instruction.Visible = -1
  Instruction.Caption = "Choose the size range over which this distribution applies"
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'Parameter(Index).SetFocus
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  Instruction.Visible = -1
  Instruction.Caption = "Specify fraction of the size class in this grade class"
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < NGC - 1 And KeyAscii = 13 Then
    Parameter(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWGDATA())
End Sub

Private Sub SizeRange1_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'SizeRange1(Index).SetFocus
  End If
End Sub

Private Sub SizeRange1_GotFocus(Index As Integer)
  SELECTIND = Index
  Instruction.Visible = -1
  Instruction.Caption = "Specify the size range over which this distribution applies."
End Sub

Private Sub SizeRange1_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    Call SizeRange1_LostFocus(Index)
    SizeRange2(Index).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub SizeRange1_LostFocus(Index As Integer)
'*************************************************
  If Screen.ActiveControl.Tag = "Delete" Then
    Exit Sub
  Else
    SELECTIND = -1
  End If
  If Val(SizeRange1(Index).text) <= Val(SizeRange1(Index - 1).text) Or Val(SizeRange1(Index).text) >= Val(SizeRange2(Index).text) Then
    MsgBox "Impermissible size range has been specified", 0, "ERROR"
    Exit Sub
  Else
    SizeRange2(Index - 1).text = SizeRange1(Index).text
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWGDATA())
  Instruction.Visible = 0
End Sub

Private Sub SizeRange2_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    'SizeRange2(Index).SetFocus
  End If
End Sub

Private Sub SizeRange2_GotFocus(Index As Integer)
  SELECTIND = Index
  Instruction.Visible = -1
  Instruction.Caption = "Specify the size range over which this distribution applies."
End Sub

Private Sub SizeRange2_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    Call SizeRange2_LostFocus(Index)
    SizeRange2(Index).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub SizeRange2_LostFocus(Index As Integer)
  If Screen.ActiveControl.Tag = "Delete" Then
    Exit Sub
  Else
    SELECTIND = -1
  End If
  If Index < NO_GRADE_RANGES - 1 Then
    If Val(SizeRange2(Index).text) <= Val(SizeRange1(Index).text) Or Val(SizeRange2(Index).text) >= Val(SizeRange2(Index + 1).text) Then
      MsgBox "Impermissible size range has been specified", 0, "ERROR"
      Exit Sub
    End If
    If Val(SizeRange2(Index).text) < Val(SizeRange1(Index + 1).text) Then
      Call AddSizeRange(Index)
    Else
      SizeRange1(Index + 1).text = SizeRange2(Index).text
    End If
  Else
    If Val(SizeRange2(Index).text) < Val(Sysdata.D1.text) Then
      Call AddSizeRange(Index)
    End If
  End If
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWGDATA())
  Instruction.Visible = 0
End Sub

Private Sub CmdImport_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Get liberation data from an external ASCII file"
End Sub

Private Sub CmdImport_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub CmdImport_Click()
'****************************
  Dim NGCFile As Integer
  Dim WS As Single
  Dim Ws1 As Single, WS2 As Single
  Dim FileName As String
  Dim I As Integer, K As Integer
  On Error GoTo ErrHandler
  
  'Open file to get current data for size distributions
  'Set filters
  CommonDialog1.Filter = "All files (*.*)|*.*|Data files (*.DAT)|*.dat"
  'Default filter
  CommonDialog1.FilterIndex = 2
  'Set the cancel error flag
  CommonDialog1.CancelError = True
  CommonDialog1.Flags = cdlOFNHideReadOnly
  'Display OPEN dialog box
  CommonDialog1.ShowOpen
  FileName = Trim(CommonDialog1.FileName)
  
  Open FileName For Input As #38
  On Error GoTo ErrHandler1
  NGC = Val(Sysdata.NGCM.text)
  
  'Get a file identifier
  Line Input #38, WSTRING
  If Mid(WSTRING, 1, 10) <> "MODSIM_LIB" Then
    MsgBox "Incorrect file type", vbOKOnly, "ERROR"
    Close #38
    Exit Sub
  End If
  'Check the format version
  If Mid(WSTRING, 11, 3) = "1.1" Then
    Input #38, NGCFile
    If NGCFile <> NGC Then
      MsgBox "Number of grade classes in the file do not match number specified in system data", vbOKOnly, "ERROR"
      Close #38
      Exit Sub
    End If
    K = 0
    Do
      K = K + 1
      NEWGDATA(K).NSTR = Val(Psd.Stream.Caption)
      NEWGDATA(K).NGC = NGC
      Input #38, Ws1, WS2
      WS = 0
      For I = 1 To NEWGDATA(K).NGC
        Input #38, NEWGDATA(K).Fraction(I)
        WS = WS + NEWGDATA(K).Fraction(I)
      Next I
      If Ws1 <= WS2 Then
        NEWGDATA(K).SIZE_RANGE(1) = Ws1
        NEWGDATA(K).SIZE_RANGE(2) = WS2
      Else
        NEWGDATA(K).SIZE_RANGE(1) = WS2
        NEWGDATA(K).SIZE_RANGE(2) = Ws1
      End If
      For I = 1 To NEWGDATA(K).NGC
        NEWGDATA(K).Fraction(I) = NEWGDATA(K).Fraction(I) / WS
      Next I
    Loop While Not EOF(38)
  Else
    MsgBox "Unknown data format", vbOKOnly, "ERROR"
    Close #38
    Exit Sub
  End If
  
  NO_GRADE_RANGES = K
  NEW_GRADE_RANGES = NO_GRADE_RANGES
  NEWGDATA(1).SIZE_RANGE(1) = 0
  NEWGDATA(K).SIZE_RANGE(2) = Val(Sysdata.D1)
  
  Call LoadNew(NEWGDATA())
  Close #38
  Exit Sub
  
ErrHandler1:
  MsgBox "Error reading file for G-class data.", 16, "ERROR"
  Close #38
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub
