VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form LibData 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify liberation data"
   ClientHeight    =   8025
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   10875
   ForeColor       =   &H00000000&
   LinkMode        =   1  'Source
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   8025
   ScaleWidth      =   10875
   Begin VB.TextBox Parameter 
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   301
      Left            =   2880
      TabIndex        =   360
      Text            =   "Text1"
      Top             =   6960
      Width           =   735
   End
   Begin VB.TextBox txtSizeAxisTitle 
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   7560
      TabIndex        =   358
      Top             =   120
      Width           =   2175
   End
   Begin VB.TextBox txtGradeAxisTitle 
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   4080
      TabIndex        =   355
      Top             =   120
      Width           =   2055
   End
   Begin VB.CommandButton cmdSave 
      Caption         =   "Save data to file"
      Height          =   435
      Left            =   9840
      TabIndex        =   354
      Top             =   1320
      Width           =   975
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5520
      Top             =   7080
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.CommandButton cmdPlot 
      Caption         =   "Plot graph"
      Height          =   495
      Left            =   9840
      TabIndex        =   353
      Top             =   1920
      Width           =   975
   End
   Begin VB.CommandButton cmdGetData 
      Caption         =   "Get data from file"
      Height          =   495
      Left            =   9840
      TabIndex        =   352
      Top             =   720
      Width           =   975
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   120
      TabIndex        =   350
      Top             =   6240
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   120
      TabIndex        =   349
      Top             =   6000
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   120
      TabIndex        =   348
      Top             =   5760
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   25
      Left            =   120
      TabIndex        =   347
      Top             =   6480
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   120
      TabIndex        =   346
      Top             =   5280
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   120
      TabIndex        =   345
      Top             =   5040
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   120
      TabIndex        =   344
      Top             =   4800
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   120
      TabIndex        =   343
      Top             =   5520
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   120
      TabIndex        =   342
      Top             =   4560
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   120
      TabIndex        =   341
      Top             =   4080
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   120
      TabIndex        =   340
      Top             =   3840
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   120
      TabIndex        =   339
      Top             =   3600
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   120
      TabIndex        =   338
      Top             =   4320
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   120
      TabIndex        =   337
      Top             =   3120
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   120
      TabIndex        =   336
      Top             =   2880
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   120
      TabIndex        =   335
      Top             =   2640
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   120
      TabIndex        =   334
      Top             =   3360
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   120
      TabIndex        =   333
      Top             =   2160
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   120
      TabIndex        =   332
      Top             =   1920
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   120
      TabIndex        =   331
      Top             =   1680
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   120
      TabIndex        =   330
      Top             =   2400
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   120
      TabIndex        =   329
      Top             =   1200
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   120
      TabIndex        =   328
      Top             =   960
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   120
      TabIndex        =   327
      Top             =   720
      Width           =   950
   End
   Begin VB.TextBox txtSizeClass 
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   120
      TabIndex        =   326
      Top             =   1440
      Width           =   950
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   300
      Left            =   9000
      TabIndex        =   325
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   299
      Left            =   8280
      TabIndex        =   324
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   298
      Left            =   7560
      TabIndex        =   323
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   297
      Left            =   6840
      TabIndex        =   322
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   296
      Left            =   6120
      TabIndex        =   321
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   295
      Left            =   5400
      TabIndex        =   320
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   294
      Left            =   4680
      TabIndex        =   319
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   293
      Left            =   3960
      TabIndex        =   318
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   292
      Left            =   3240
      TabIndex        =   317
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   291
      Left            =   2520
      TabIndex        =   316
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   290
      Left            =   1800
      TabIndex        =   315
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   289
      Left            =   1080
      TabIndex        =   314
      Tag             =   "Parameter"
      Top             =   6480
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   0
      Left            =   2160
      TabIndex        =   1
      Tag             =   "Parameter"
      Top             =   120
      Width           =   375
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   480
      Left            =   9840
      TabIndex        =   11
      Top             =   3120
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   465
      Left            =   9840
      TabIndex        =   10
      Top             =   2520
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
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
      Height          =   1095
      Left            =   9240
      TabIndex        =   0
      Top             =   6840
      Width           =   1455
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
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
         Height          =   240
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display data set that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
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
         Height          =   240
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display data set used in the previous simulation"
         Top             =   480
         Width           =   1200
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
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
      Height          =   975
      Left            =   6120
      TabIndex        =   4
      Top             =   6960
      Visible         =   0   'False
      Width           =   2895
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "micron"
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
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   60
      Left            =   9000
      TabIndex        =   97
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   59
      Left            =   8280
      TabIndex        =   96
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   58
      Left            =   7560
      TabIndex        =   95
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   57
      Left            =   6840
      TabIndex        =   94
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   56
      Left            =   6120
      TabIndex        =   93
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   55
      Left            =   5400
      TabIndex        =   92
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   54
      Left            =   4680
      TabIndex        =   91
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   53
      Left            =   3960
      TabIndex        =   90
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   52
      Left            =   3240
      TabIndex        =   89
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   51
      Left            =   2520
      TabIndex        =   88
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   50
      Left            =   1800
      TabIndex        =   87
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   49
      Left            =   1080
      TabIndex        =   86
      Tag             =   "Parameter"
      Top             =   1680
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   48
      Left            =   9000
      TabIndex        =   73
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   47
      Left            =   8280
      TabIndex        =   72
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   46
      Left            =   7560
      TabIndex        =   71
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   45
      Left            =   6840
      TabIndex        =   70
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   44
      Left            =   6120
      TabIndex        =   69
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   43
      Left            =   5400
      TabIndex        =   68
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   42
      Left            =   4680
      TabIndex        =   67
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   41
      Left            =   3960
      TabIndex        =   66
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   40
      Left            =   3240
      TabIndex        =   65
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   39
      Left            =   2520
      TabIndex        =   64
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   38
      Left            =   1800
      TabIndex        =   63
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   37
      Left            =   1080
      TabIndex        =   62
      Tag             =   "Parameter"
      Top             =   1440
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   36
      Left            =   9000
      TabIndex        =   61
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   35
      Left            =   8280
      TabIndex        =   60
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   34
      Left            =   7560
      TabIndex        =   59
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   33
      Left            =   6840
      TabIndex        =   58
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   32
      Left            =   6120
      TabIndex        =   57
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   31
      Left            =   5400
      TabIndex        =   56
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   30
      Left            =   4680
      TabIndex        =   55
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   29
      Left            =   3960
      TabIndex        =   54
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   28
      Left            =   3240
      TabIndex        =   53
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   27
      Left            =   2520
      TabIndex        =   52
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   26
      Left            =   1800
      TabIndex        =   51
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   25
      Left            =   1080
      TabIndex        =   50
      Tag             =   "Parameter"
      Top             =   1200
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   24
      Left            =   9000
      TabIndex        =   49
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   23
      Left            =   8280
      TabIndex        =   48
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   22
      Left            =   7560
      TabIndex        =   47
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   21
      Left            =   6840
      TabIndex        =   46
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   20
      Left            =   6120
      TabIndex        =   45
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   19
      Left            =   5400
      TabIndex        =   44
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   18
      Left            =   4680
      TabIndex        =   43
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   17
      Left            =   3960
      TabIndex        =   42
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   16
      Left            =   3240
      TabIndex        =   41
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   15
      Left            =   2520
      TabIndex        =   40
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   14
      Left            =   1800
      TabIndex        =   39
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   13
      Left            =   1080
      TabIndex        =   38
      Tag             =   "Parameter"
      Top             =   960
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   12
      Left            =   9000
      TabIndex        =   25
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   11
      Left            =   8280
      TabIndex        =   24
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   10
      Left            =   7560
      TabIndex        =   23
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   9
      Left            =   6840
      TabIndex        =   22
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   8
      Left            =   6120
      TabIndex        =   21
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   7
      Left            =   5400
      TabIndex        =   20
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   6
      Left            =   4680
      TabIndex        =   19
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   5
      Left            =   3960
      TabIndex        =   18
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   4
      Left            =   3240
      TabIndex        =   17
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   3
      Left            =   2520
      TabIndex        =   16
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   2
      Left            =   1800
      TabIndex        =   15
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   1
      Left            =   1080
      TabIndex        =   14
      Tag             =   "Parameter"
      Top             =   720
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   121
      Left            =   1080
      TabIndex        =   85
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   122
      Left            =   1800
      TabIndex        =   84
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   123
      Left            =   2520
      TabIndex        =   83
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   124
      Left            =   3240
      TabIndex        =   82
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   125
      Left            =   3960
      TabIndex        =   81
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   126
      Left            =   4680
      TabIndex        =   80
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   127
      Left            =   5400
      TabIndex        =   79
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   128
      Left            =   6120
      TabIndex        =   78
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   129
      Left            =   6840
      TabIndex        =   77
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   130
      Left            =   7560
      TabIndex        =   76
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   131
      Left            =   8280
      TabIndex        =   75
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   132
      Left            =   9000
      TabIndex        =   74
      Tag             =   "Parameter"
      Top             =   3120
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   61
      Left            =   1080
      TabIndex        =   98
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   62
      Left            =   1800
      TabIndex        =   99
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   63
      Left            =   2520
      TabIndex        =   100
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   64
      Left            =   3240
      TabIndex        =   101
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   65
      Left            =   3960
      TabIndex        =   102
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   66
      Left            =   4680
      TabIndex        =   103
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   67
      Left            =   5400
      TabIndex        =   104
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   68
      Left            =   6120
      TabIndex        =   105
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   69
      Left            =   6840
      TabIndex        =   106
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   70
      Left            =   7560
      TabIndex        =   107
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   71
      Left            =   8280
      TabIndex        =   108
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   72
      Left            =   9000
      TabIndex        =   109
      Tag             =   "Parameter"
      Top             =   1920
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   73
      Left            =   1080
      TabIndex        =   110
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   74
      Left            =   1800
      TabIndex        =   111
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   75
      Left            =   2520
      TabIndex        =   112
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   76
      Left            =   3240
      TabIndex        =   113
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   77
      Left            =   3960
      TabIndex        =   114
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   78
      Left            =   4680
      TabIndex        =   115
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   79
      Left            =   5400
      TabIndex        =   116
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   80
      Left            =   6120
      TabIndex        =   117
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   81
      Left            =   6840
      TabIndex        =   118
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   82
      Left            =   7560
      TabIndex        =   119
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   83
      Left            =   8280
      TabIndex        =   120
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   84
      Left            =   9000
      TabIndex        =   121
      Tag             =   "Parameter"
      Top             =   2160
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   85
      Left            =   1080
      TabIndex        =   122
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   86
      Left            =   1800
      TabIndex        =   123
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   87
      Left            =   2520
      TabIndex        =   124
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   88
      Left            =   3240
      TabIndex        =   125
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   89
      Left            =   3960
      TabIndex        =   126
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   90
      Left            =   4680
      TabIndex        =   127
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   91
      Left            =   5400
      TabIndex        =   128
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   92
      Left            =   6120
      TabIndex        =   129
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   93
      Left            =   6840
      TabIndex        =   130
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   94
      Left            =   7560
      TabIndex        =   131
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   95
      Left            =   8280
      TabIndex        =   132
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   96
      Left            =   9000
      TabIndex        =   133
      Tag             =   "Parameter"
      Top             =   2400
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   97
      Left            =   1080
      TabIndex        =   134
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   98
      Left            =   1800
      TabIndex        =   135
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   99
      Left            =   2520
      TabIndex        =   136
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   100
      Left            =   3240
      TabIndex        =   137
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   101
      Left            =   3960
      TabIndex        =   138
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   102
      Left            =   4680
      TabIndex        =   139
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   103
      Left            =   5400
      TabIndex        =   140
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   104
      Left            =   6120
      TabIndex        =   141
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   105
      Left            =   6840
      TabIndex        =   142
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   106
      Left            =   7560
      TabIndex        =   143
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   107
      Left            =   8280
      TabIndex        =   144
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   108
      Left            =   9000
      TabIndex        =   145
      Tag             =   "Parameter"
      Top             =   2640
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   109
      Left            =   1080
      TabIndex        =   146
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   110
      Left            =   1800
      TabIndex        =   147
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   111
      Left            =   2520
      TabIndex        =   148
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   112
      Left            =   3240
      TabIndex        =   149
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   113
      Left            =   3960
      TabIndex        =   150
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   114
      Left            =   4680
      TabIndex        =   151
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   115
      Left            =   5400
      TabIndex        =   152
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   116
      Left            =   6120
      TabIndex        =   153
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   117
      Left            =   6840
      TabIndex        =   154
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   118
      Left            =   7560
      TabIndex        =   155
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   119
      Left            =   8280
      TabIndex        =   156
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   120
      Left            =   9000
      TabIndex        =   157
      Tag             =   "Parameter"
      Top             =   2880
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   133
      Left            =   1080
      TabIndex        =   158
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   134
      Left            =   1800
      TabIndex        =   159
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   135
      Left            =   2520
      TabIndex        =   160
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   136
      Left            =   3240
      TabIndex        =   161
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   137
      Left            =   3960
      TabIndex        =   162
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   138
      Left            =   4680
      TabIndex        =   163
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   139
      Left            =   5400
      TabIndex        =   164
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   140
      Left            =   6120
      TabIndex        =   165
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   141
      Left            =   6840
      TabIndex        =   166
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   142
      Left            =   7560
      TabIndex        =   167
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   143
      Left            =   8280
      TabIndex        =   168
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   144
      Left            =   9000
      TabIndex        =   169
      Tag             =   "Parameter"
      Top             =   3360
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   145
      Left            =   1080
      TabIndex        =   170
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   146
      Left            =   1800
      TabIndex        =   171
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   147
      Left            =   2520
      TabIndex        =   172
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   148
      Left            =   3240
      TabIndex        =   173
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   149
      Left            =   3960
      TabIndex        =   174
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   150
      Left            =   4680
      TabIndex        =   175
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   151
      Left            =   5400
      TabIndex        =   176
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   152
      Left            =   6120
      TabIndex        =   177
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   153
      Left            =   6840
      TabIndex        =   178
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   154
      Left            =   7560
      TabIndex        =   179
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   155
      Left            =   8280
      TabIndex        =   180
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   156
      Left            =   9000
      TabIndex        =   181
      Tag             =   "Parameter"
      Top             =   3600
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   157
      Left            =   1080
      TabIndex        =   182
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   158
      Left            =   1800
      TabIndex        =   183
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   159
      Left            =   2520
      TabIndex        =   184
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   160
      Left            =   3240
      TabIndex        =   185
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   161
      Left            =   3960
      TabIndex        =   186
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   162
      Left            =   4680
      TabIndex        =   187
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   163
      Left            =   5400
      TabIndex        =   188
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   164
      Left            =   6120
      TabIndex        =   189
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   165
      Left            =   6840
      TabIndex        =   190
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   166
      Left            =   7560
      TabIndex        =   191
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   167
      Left            =   8280
      TabIndex        =   192
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   168
      Left            =   9000
      TabIndex        =   193
      Tag             =   "Parameter"
      Top             =   3840
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   169
      Left            =   1080
      TabIndex        =   194
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   170
      Left            =   1800
      TabIndex        =   195
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   171
      Left            =   2520
      TabIndex        =   196
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   172
      Left            =   3240
      TabIndex        =   197
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   173
      Left            =   3960
      TabIndex        =   198
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   174
      Left            =   4680
      TabIndex        =   199
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   175
      Left            =   5400
      TabIndex        =   200
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   176
      Left            =   6120
      TabIndex        =   201
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   177
      Left            =   6840
      TabIndex        =   202
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   178
      Left            =   7560
      TabIndex        =   203
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   179
      Left            =   8280
      TabIndex        =   204
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   180
      Left            =   9000
      TabIndex        =   205
      Tag             =   "Parameter"
      Top             =   4080
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   181
      Left            =   1080
      TabIndex        =   206
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   182
      Left            =   1800
      TabIndex        =   207
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   183
      Left            =   2520
      TabIndex        =   208
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   184
      Left            =   3240
      TabIndex        =   209
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   185
      Left            =   3960
      TabIndex        =   210
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   186
      Left            =   4680
      TabIndex        =   211
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   187
      Left            =   5400
      TabIndex        =   212
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   188
      Left            =   6120
      TabIndex        =   213
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   189
      Left            =   6840
      TabIndex        =   214
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   190
      Left            =   7560
      TabIndex        =   215
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   191
      Left            =   8280
      TabIndex        =   216
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   192
      Left            =   9000
      TabIndex        =   217
      Tag             =   "Parameter"
      Top             =   4320
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   193
      Left            =   1080
      TabIndex        =   218
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   194
      Left            =   1800
      TabIndex        =   219
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   195
      Left            =   2520
      TabIndex        =   220
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   196
      Left            =   3240
      TabIndex        =   221
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   197
      Left            =   3960
      TabIndex        =   222
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   198
      Left            =   4680
      TabIndex        =   223
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   199
      Left            =   5400
      TabIndex        =   224
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   200
      Left            =   6120
      TabIndex        =   225
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   201
      Left            =   6840
      TabIndex        =   226
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   202
      Left            =   7560
      TabIndex        =   227
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   203
      Left            =   8280
      TabIndex        =   228
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   204
      Left            =   9000
      TabIndex        =   229
      Tag             =   "Parameter"
      Top             =   4560
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   205
      Left            =   1080
      TabIndex        =   230
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   206
      Left            =   1800
      TabIndex        =   231
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   207
      Left            =   2520
      TabIndex        =   232
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   208
      Left            =   3240
      TabIndex        =   233
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   209
      Left            =   3960
      TabIndex        =   234
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   210
      Left            =   4680
      TabIndex        =   235
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   211
      Left            =   5400
      TabIndex        =   236
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   212
      Left            =   6120
      TabIndex        =   237
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   213
      Left            =   6840
      TabIndex        =   238
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   214
      Left            =   7560
      TabIndex        =   239
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   215
      Left            =   8280
      TabIndex        =   240
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   216
      Left            =   9000
      TabIndex        =   241
      Tag             =   "Parameter"
      Top             =   4800
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   217
      Left            =   1080
      TabIndex        =   242
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   218
      Left            =   1800
      TabIndex        =   243
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   219
      Left            =   2520
      TabIndex        =   244
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   220
      Left            =   3240
      TabIndex        =   245
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   221
      Left            =   3960
      TabIndex        =   246
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   222
      Left            =   4680
      TabIndex        =   247
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   223
      Left            =   5400
      TabIndex        =   248
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   224
      Left            =   6120
      TabIndex        =   249
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   225
      Left            =   6840
      TabIndex        =   250
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   226
      Left            =   7560
      TabIndex        =   251
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   227
      Left            =   8280
      TabIndex        =   252
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   228
      Left            =   9000
      TabIndex        =   253
      Tag             =   "Parameter"
      Top             =   5040
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   229
      Left            =   1080
      TabIndex        =   254
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   230
      Left            =   1800
      TabIndex        =   255
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   231
      Left            =   2520
      TabIndex        =   256
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   232
      Left            =   3240
      TabIndex        =   257
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   233
      Left            =   3960
      TabIndex        =   258
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   234
      Left            =   4680
      TabIndex        =   259
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   235
      Left            =   5400
      TabIndex        =   260
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   236
      Left            =   6120
      TabIndex        =   261
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   237
      Left            =   6840
      TabIndex        =   262
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   238
      Left            =   7560
      TabIndex        =   263
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   239
      Left            =   8280
      TabIndex        =   264
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   240
      Left            =   9000
      TabIndex        =   265
      Tag             =   "Parameter"
      Top             =   5280
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   241
      Left            =   1080
      TabIndex        =   266
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   242
      Left            =   1800
      TabIndex        =   267
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   243
      Left            =   2520
      TabIndex        =   268
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   244
      Left            =   3240
      TabIndex        =   269
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   245
      Left            =   3960
      TabIndex        =   270
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   246
      Left            =   4680
      TabIndex        =   271
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   247
      Left            =   5400
      TabIndex        =   272
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   248
      Left            =   6120
      TabIndex        =   273
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   249
      Left            =   6840
      TabIndex        =   274
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   250
      Left            =   7560
      TabIndex        =   275
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   251
      Left            =   8280
      TabIndex        =   276
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   252
      Left            =   9000
      TabIndex        =   277
      Tag             =   "Parameter"
      Top             =   5520
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   253
      Left            =   1080
      TabIndex        =   278
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   254
      Left            =   1800
      TabIndex        =   279
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   255
      Left            =   2520
      TabIndex        =   280
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   256
      Left            =   3240
      TabIndex        =   281
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   257
      Left            =   3960
      TabIndex        =   282
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   258
      Left            =   4680
      TabIndex        =   283
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   259
      Left            =   5400
      TabIndex        =   284
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   260
      Left            =   6120
      TabIndex        =   285
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   261
      Left            =   6840
      TabIndex        =   286
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   262
      Left            =   7560
      TabIndex        =   287
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   263
      Left            =   8280
      TabIndex        =   288
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   264
      Left            =   9000
      TabIndex        =   289
      Tag             =   "Parameter"
      Top             =   5760
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   265
      Left            =   1080
      TabIndex        =   290
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   266
      Left            =   1800
      TabIndex        =   291
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   267
      Left            =   2520
      TabIndex        =   292
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   268
      Left            =   3240
      TabIndex        =   293
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   269
      Left            =   3960
      TabIndex        =   294
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   270
      Left            =   4680
      TabIndex        =   295
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   271
      Left            =   5400
      TabIndex        =   296
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   272
      Left            =   6120
      TabIndex        =   297
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   273
      Left            =   6840
      TabIndex        =   298
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   274
      Left            =   7560
      TabIndex        =   299
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   275
      Left            =   8280
      TabIndex        =   300
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   276
      Left            =   9000
      TabIndex        =   301
      Tag             =   "Parameter"
      Top             =   6000
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   277
      Left            =   1080
      TabIndex        =   302
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   278
      Left            =   1800
      TabIndex        =   303
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   279
      Left            =   2520
      TabIndex        =   304
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   280
      Left            =   3240
      TabIndex        =   305
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   281
      Left            =   3960
      TabIndex        =   306
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   282
      Left            =   4680
      TabIndex        =   307
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   283
      Left            =   5400
      TabIndex        =   308
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   284
      Left            =   6120
      TabIndex        =   309
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   285
      Left            =   6840
      TabIndex        =   310
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   286
      Left            =   7560
      TabIndex        =   311
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   287
      Left            =   8280
      TabIndex        =   312
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   288
      Left            =   9000
      TabIndex        =   313
      Tag             =   "Parameter"
      Top             =   6240
      Width           =   700
   End
   Begin VB.Label Label4 
      Caption         =   "Value for 100% probability"
      Height          =   255
      Left            =   720
      TabIndex        =   359
      Top             =   6960
      Width           =   1935
   End
   Begin VB.Label Label3 
      Caption         =   "Size axis title"
      Height          =   255
      Left            =   6600
      TabIndex        =   357
      Top             =   120
      Width           =   975
   End
   Begin VB.Label Label2 
      Caption         =   "Grade axis title"
      Height          =   255
      Left            =   3000
      TabIndex        =   356
      Top             =   120
      Width           =   1095
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H80000016&
      Caption         =   "Size class"
      Height          =   225
      Left            =   120
      TabIndex        =   351
      Top             =   480
      Width           =   950
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   11
      Left            =   8280
      TabIndex        =   37
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   10
      Left            =   7560
      TabIndex        =   36
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   9
      Left            =   6840
      TabIndex        =   35
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   8
      Left            =   6120
      TabIndex        =   34
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   7
      Left            =   5400
      TabIndex        =   33
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   6
      Left            =   4680
      TabIndex        =   32
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   5
      Left            =   3960
      TabIndex        =   31
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   4
      Left            =   3240
      TabIndex        =   30
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   3
      Left            =   2520
      TabIndex        =   29
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   2
      Left            =   1800
      TabIndex        =   28
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   1
      Left            =   1080
      TabIndex        =   27
      Top             =   480
      Width           =   705
   End
   Begin VB.Label lblColHeads 
      Alignment       =   2  'Center
      Height          =   225
      Index           =   12
      Left            =   9000
      TabIndex        =   26
      Top             =   480
      Width           =   705
   End
   Begin VB.Label LblParam 
      Caption         =   "Number of size  classes"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   13
      Top             =   120
      Width           =   1935
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
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuCut 
         Caption         =   "Cut"
         Shortcut        =   ^X
      End
      Begin VB.Menu mnuPaste 
         Caption         =   "Paste"
         Shortcut        =   ^V
      End
   End
End
Attribute VB_Name = "LibData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private LoadingForm As Boolean
Private WithFocus As Integer

Sub CmdAccept_Click()
'*********************
  Call MakeNew(CURRMODELDATA(ID))
  Unload Me
End Sub

Sub CmdCancel_Click()
'*********************
  Unload Me
End Sub

Private Sub cmdGetData_Click()
'*****************************
  Dim P(12, 51) As Single
  Dim FileName As String, GradeAxisTitle As String, SizeAxisTitle As String
  Dim SizeLabs(51) As String
  Dim LAYOUT  As String
  Dim NGC As Integer, NDC As Integer
  Dim Scalefactor As Single, Pmax As Single
  Dim I As Integer, J As Integer
  Dim NL As Integer, NL1 As Integer

  On Error GoTo ErrHandler
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|Text files (*.txt)|*.txt|All files(*.*)|*.*"
  CommonDialog1.Flags = 6
  CommonDialog1.ShowOpen
  Open CommonDialog1.FileName For Input As #31
  ' Specify the name of the file that contains the data as Unit 31
  ' This could be measured spectra or an A-M diagram
  ' Titles for the axes and labels for the size axis must be on the top of the file.
  
  NGC = 12
  Line Input #31, GradeAxisTitle
  NL = Len(GradeAxisTitle)
  NEWDATA.TEXTSTRING(26) = GradeAxisTitle
  Line Input #31, SizeAxisTitle
  NEWDATA.TEXTSTRING(27) = SizeAxisTitle
  NL1 = Len(SizeAxisTitle)
  Input #31, NDC
  NEWDATA.PARAM(1) = NDC
  NEWDATA.NOPAR = NGC * NDC + 1

  For I = 1 To NDC
    Input #31, NEWDATA.TEXTSTRING(I)
  Next I
  Input #31, LAYOUT
  Input #31, Scalefactor
  NEWDATA.PARAM(302) = Scalefactor
  If LAYOUT = "Grade by size" Then
    For J = 1 To NGC
      For I = 1 To NDC
        Input #31, NEWDATA.PARAM(12 * (I - 1) + J + 1)
      Next I
    Next J
  ElseIf LAYOUT = "Size by grade" Then
    For I = 1 To NDC
      For J = 1 To NGC
        Input #31, NEWDATA.PARAM(12 * (I - 1) + J + 1)
      Next J
    Next I
  Else
    MsgBox "You must specify the layout in the data file", vbCritical, "ERROR"
    Exit Sub
  End If
  Close #31
  Call LoadNew(NEWDATA)
  Exit Sub

ErrHandler:
  Exit Sub
End Sub

Private Sub cmdPlot_Click()
'**************************
Call MakeNew(CURRMODELDATA(ID))
Call BARCHART
End Sub

Private Sub cmdSave_Click()
'*****************************
  Dim P(12, 51) As Single
  Dim FileName As String, GradeAxisTitle As String, SizeAxisTitle As String
  Dim SizeLabs(51) As String
  Dim LAYOUT  As String
  Dim NGC As Integer, NDC As Integer
  Dim Scalefactor As Single, Pmax As Single
  Dim I As Integer, J As Integer
  Dim NL As Integer, NL1 As Integer
  Dim response As VbMsgBoxResult
  
  On Error GoTo ErrHandler
  Call MakeNew(NEWDATA)
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|Text files (*.txt)|*.txt"
  CommonDialog1.Flags = 4
  CommonDialog1.ShowSave
  FileName = Trim(CommonDialog1.FileName)
  'Strip off any file extensions
  I = InStr(CommonDialog1.FileName, ".")
  FileName = Trim(Mid(CommonDialog1.FileName, 1, I - 1))
  'Add the chosen extension
  If CommonDialog1.FilterIndex = 1 Then
    FileName = FileName & ".dat"
  Else
    FileName = FileName & ".txt"
  End If
  'Check if file exists
  If Dir(FileName) <> "" Then
    response = MsgBox("File " & FileName & " already exists. OverWrite?", vbOKCancel, "WARNING")
    If response = vbCancel Then Exit Sub
  End If
  Open FileName For Output As #31
  ' Specify the name of the file to contain the data as Unit 31
  ' This could be measured spectra or an A-M diagram
  ' Titles for the axes and labels for the size axis must be on the top of the file.
  
  NGC = 12
  GradeAxisTitle = NEWDATA.TEXTSTRING(26)
  Print #31, GradeAxisTitle
  SizeAxisTitle = NEWDATA.TEXTSTRING(27)
  Print #31, SizeAxisTitle
  NDC = NEWDATA.PARAM(1)
  Print #31, NDC

  For I = 1 To NDC
    Print #31, NEWDATA.TEXTSTRING(I)
  Next I
  LAYOUT = "Size by grade"
  Print #31, LAYOUT
  Scalefactor = NEWDATA.PARAM(302)
  Print #31, Scalefactor
  For I = 1 To NDC
    For J = 1 To NGC
      Print #31, NEWDATA.PARAM(12 * (I - 1) + J + 1)
    Next J
  Next I
  Close #31
  Exit Sub

ErrHandler:
  Exit Sub

End Sub

Sub Form_Load()
'***************
  Dim I As Integer, J As Integer
  Dim XLABEL(12) As String
  On Error GoTo ErrHandler
  LoadingForm = True
  XLABEL(1) = "    0%   "
  XLABEL(2) = "  0 - 10 "
  XLABEL(3) = " 10 - 20 "
  XLABEL(4) = " 20 - 30 "
  XLABEL(5) = " 30 - 40 "
  XLABEL(6) = " 40 - 50 "
  XLABEL(7) = " 50 - 60 "
  XLABEL(8) = " 60 - 70 "
  XLABEL(9) = " 70 - 80 "
  XLABEL(10) = " 80 - 90 "
  XLABEL(11) = " 90 -100 "
  XLABEL(12) = "   100%  "

  For J = 1 To 12
    lblColHeads(J) = XLABEL(J)
  Next J
  For I = 2 To 25
    txtSizeClass(I).Visible = False
    For J = 1 To 12
      Parameter(12 * (I - 1) + J).Visible = False
    Next J
  Next I
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "LIBE" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  LoadingForm = False
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  LoadingForm = False
  Exit Sub
End Sub

Sub LoadDefault()
'*****************
  Dim J As Integer, I As Integer
  Parameter(0).text = CStr(1)
  txtGradeAxisTitle = "Particle composition"
  txtSizeAxisTitle = "Size class"
  txtSizeClass(1) = "710 - 1000"
  For J = 1 To 12
    Parameter(J) = CStr(0)
  Next J
  Parameter(3) = CStr(35)
  Parameter(4) = CStr(50)
  Parameter(5) = CStr(15)
  For I = 2 To 25
    txtSizeClass(I).Visible = False
    txtSizeClass(I).text = ""
    For J = 1 To 12
      Parameter(12 * (I - 1) + J).text = ""
      Parameter(12 * (I - 1) + J).Visible = False
    Next J
  Next I
  Parameter(301).text = CStr(100)
  OptSize(3).Value = -1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer, J As Integer
  txtGradeAxisTitle = NDATA.TEXTSTRING(26)
  txtSizeAxisTitle = NDATA.TEXTSTRING(27)
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
  Parameter(301).text = CStr(NDATA.PARAM(302))
  For I = 1 To NDATA.PARAM(1)
    txtSizeClass(I).Visible = True
    txtSizeClass(I).text = NDATA.TEXTSTRING(I)
    For J = 1 To 12
      Parameter(12 * (I - 1) + J).Visible = True
    Next J
  Next I
  For I = NDATA.PARAM(1) + 1 To 25
    txtSizeClass(I).Visible = False
    For J = 1 To 12
      Parameter(12 * (I - 1) + J).Visible = False
    Next J
  Next I
  OptSize(3).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer, NDC As Integer
  Dim Scalefactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "LIBE"
  NDATA.NOPAR = 1 + 12 * Val(Parameter(0).text) + 1
  NDATA.TEXTSTRING(26) = txtGradeAxisTitle.text
  NDATA.TEXTSTRING(27) = txtSizeAxisTitle.text
  
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  NDATA.PARAM(302) = Val(Parameter(301).text)
  NDC = NDATA.PARAM(1)
  For I = 1 To NDC
    NDATA.TEXTSTRING(I) = txtSizeClass(I).text
  Next I
  'Convert the size to meters
  Scalefactor = 1#
  If OptSize(0).Value Then Scalefactor = 0.000001
  If OptSize(1).Value Then Scalefactor = 0.001
  If OptSize(2).Value Then Scalefactor = 0.01
  If OptSize(4).Value Then Scalefactor = 0.0254
End Sub


Private Sub MnuAccept_Click()
'****************************
   Call CmdAccept_Click
End Sub

Private Sub mnuClose_Click()
'***************************
  Call CmdCancel_Click
End Sub

Private Sub mnuCopy_Click()
'**************************
Clipboard.Clear
Clipboard.SetText Parameter(WithFocus).SelText
End Sub

Private Sub mnuCut_Click()
'************************
Clipboard.Clear
Clipboard.SetText Parameter(WithFocus).SelText
Parameter(WithFocus).text = ""
End Sub

Private Sub MnuPaste_Click()
'***************************
Dim text As String, I As Integer, N As Integer

text = Clipboard.GetText
For I = 1 To Len(text)
  If Mid(text, I, 1) = Chr(13) Then Mid(text, I, 1) = Chr(9)
  If Mid(text, I, 1) = Chr(10) Then Mid(text, I, 1) = " "
Next I
I = WithFocus
Do While text <> ""
  N = InStr(text, Chr(9))
  If N > 0 Then
    Parameter(I).text = Mid(text, 1, N - 1)
  Else
    Parameter(I).text = text
    Exit Sub
  End If
  text = Mid(text, N + 1)
  I = I + 1
Loop
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form
PrintForm
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
  'Must trigger a load only when NEW is switched on with a click.
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If LoadingForm Then Exit Sub
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
'***********************************************
WithFocus = Index
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
'********************************************************************
Dim I As Integer, J As Integer, NDC As Integer
If KeyAscii = 13 Then
  If Index = 0 Then
    NDC = NEWDATA.PARAM(1)
    For I = NDC + 1 To Val(Parameter(0).text)
      txtSizeClass(I).Visible = True
      txtSizeClass(NDC + 1).SetFocus
      For J = 1 To 12
        Parameter(12 * (I - 1) + J).Visible = True
      Next J
    Next I
  ElseIf Index - (Index \ 12) * 12 <> 0 Then
    Parameter(Index + 1).SetFocus
    Parameter(Index + 1).text = ""
  End If
  KeyAscii = 0
End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  If Index = 0 Then
    Call LoadNew(NEWDATA)
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

Private Sub txtSizeClass_Change(Index As Integer)
'***********************************************
  If LoadingForm Then Exit Sub
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub txtSizeClass_KeyPress(Index As Integer, KeyAscii As Integer)
'***********************************************************************
Dim J As Integer
If Index < 25 And KeyAscii = 13 Then
  If Index >= Val(Parameter(0).text) Then Parameter(0).text = CStr(Index + 1)
  txtSizeClass(Index + 1).Visible = True
  For J = 1 To 12
    Parameter(12 * (Index) + J).Visible = True
  Next J
  txtSizeClass(Index + 1).SetFocus
  KeyAscii = 0
End If
End Sub
