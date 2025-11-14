VERSION 5.00
Begin VB.Form Psd 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Particle-size distribution data sheet"
   ClientHeight    =   6705
   ClientLeft      =   420
   ClientTop       =   1545
   ClientWidth     =   9480
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
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6705
   ScaleWidth      =   9480
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   25
      Left            =   2520
      TabIndex        =   75
      Top             =   6360
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   24
      Left            =   2520
      TabIndex        =   74
      Top             =   6120
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   23
      Left            =   2520
      TabIndex        =   73
      Top             =   5880
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   22
      Left            =   2520
      TabIndex        =   72
      Top             =   5640
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   21
      Left            =   2520
      TabIndex        =   71
      Top             =   5400
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   24
      Left            =   120
      TabIndex        =   70
      Top             =   6360
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   23
      Left            =   120
      TabIndex        =   69
      Top             =   6120
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   22
      Left            =   120
      TabIndex        =   68
      Top             =   5880
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   21
      Left            =   120
      TabIndex        =   67
      Top             =   5640
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   20
      Left            =   120
      TabIndex        =   66
      Top             =   5400
      Width           =   1320
   End
   Begin VB.TextBox SetName 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   285
      Left            =   5040
      TabIndex        =   0
      ToolTipText     =   "Specify a name for this data set"
      Top             =   120
      Width           =   4320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   20
      Left            =   2520
      TabIndex        =   39
      Top             =   5160
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   19
      Left            =   2520
      TabIndex        =   41
      Top             =   4920
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   18
      Left            =   2520
      TabIndex        =   37
      Top             =   4680
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   17
      Left            =   2520
      TabIndex        =   35
      Top             =   4440
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   16
      Left            =   2520
      TabIndex        =   33
      Top             =   4200
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   15
      Left            =   2520
      TabIndex        =   31
      Top             =   3960
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   14
      Left            =   2520
      TabIndex        =   29
      Top             =   3720
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   13
      Left            =   2520
      TabIndex        =   27
      Top             =   3480
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   12
      Left            =   2520
      TabIndex        =   25
      Top             =   3240
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   11
      Left            =   2520
      TabIndex        =   23
      Top             =   3000
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   10
      Left            =   2520
      TabIndex        =   21
      Top             =   2760
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   9
      Left            =   2520
      TabIndex        =   19
      Top             =   2520
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   8
      Left            =   2520
      TabIndex        =   17
      Top             =   2280
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   7
      Left            =   2520
      TabIndex        =   15
      Top             =   2040
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   6
      Left            =   2520
      TabIndex        =   13
      Top             =   1800
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   5
      Left            =   2520
      TabIndex        =   11
      Top             =   1560
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   4
      Left            =   2520
      TabIndex        =   9
      Top             =   1320
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   3
      Left            =   2520
      TabIndex        =   7
      Top             =   1080
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   2
      Left            =   2520
      TabIndex        =   5
      Top             =   840
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   1
      Left            =   2520
      TabIndex        =   3
      Top             =   600
      Width           =   1320
   End
   Begin VB.TextBox Cum 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   2520
      TabIndex        =   55
      Text            =   "100"
      Top             =   360
      Width           =   1320
   End
   Begin VB.TextBox RRparameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   4560
      TabIndex        =   49
      Top             =   2520
      Width           =   1320
   End
   Begin VB.TextBox RRparameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   1
      Left            =   6960
      TabIndex        =   50
      Top             =   2520
      Width           =   1320
   End
   Begin VB.CheckBox ChkRR 
      Caption         =   "Use Rosin-Rammler distribution"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   3960
      TabIndex        =   48
      ToolTipText     =   "Check to generate a Rosin-Rammler distribution"
      Top             =   2280
      Width           =   4320
   End
   Begin VB.Frame Frame2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   855
      Left            =   3960
      TabIndex        =   60
      ToolTipText     =   "Left click to select  Right click to change to."
      Top             =   1080
      Width           =   2655
      Begin VB.OptionButton OptSize 
         Caption         =   "inch"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   4
         Left            =   1080
         TabIndex        =   47
         Tag             =   "Size"
         ToolTipText     =   "Left click to select  Right click to change to."
         Top             =   480
         Width           =   960
      End
      Begin VB.OptionButton OptSize 
         Caption         =   "m"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   46
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select  Right click to change to."
         Top             =   480
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         Caption         =   "cm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   2
         Left            =   1800
         TabIndex        =   45
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select  Right click to change to."
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         Caption         =   "mm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   1080
         TabIndex        =   44
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select  Right click to change to."
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         Caption         =   "micron"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   43
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select  Right click to change to."
         Top             =   240
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.TextBox Nclasses 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   285
      Left            =   5760
      TabIndex        =   1
      Text            =   "2"
      ToolTipText     =   "Specify number of size classes in your data"
      Top             =   600
      Width           =   960
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   6960
      TabIndex        =   54
      Top             =   6000
      Width           =   960
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   8400
      TabIndex        =   53
      Top             =   6000
      Width           =   960
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   120
      TabIndex        =   2
      Top             =   600
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   19
      Left            =   120
      TabIndex        =   38
      Top             =   5160
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   17
      Left            =   120
      TabIndex        =   36
      Top             =   4680
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   18
      Left            =   120
      TabIndex        =   40
      Top             =   4920
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   16
      Left            =   120
      TabIndex        =   34
      Top             =   4440
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   15
      Left            =   120
      TabIndex        =   32
      Top             =   4200
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   14
      Left            =   120
      TabIndex        =   30
      Top             =   3960
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   13
      Left            =   120
      TabIndex        =   28
      Top             =   3720
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   12
      Left            =   120
      TabIndex        =   26
      Top             =   3480
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   11
      Left            =   120
      TabIndex        =   24
      Top             =   3240
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   10
      Left            =   120
      TabIndex        =   22
      Top             =   3000
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   9
      Left            =   120
      TabIndex        =   20
      Top             =   2760
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   8
      Left            =   120
      TabIndex        =   18
      Top             =   2520
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   7
      Left            =   120
      TabIndex        =   16
      Top             =   2280
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   6
      Left            =   120
      TabIndex        =   14
      Top             =   2040
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   5
      Left            =   120
      TabIndex        =   12
      Top             =   1800
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   4
      Left            =   120
      TabIndex        =   10
      Top             =   1560
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   3
      Left            =   120
      TabIndex        =   8
      Top             =   1320
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   2
      Left            =   120
      TabIndex        =   6
      Top             =   1080
      Width           =   1320
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   1
      Left            =   120
      TabIndex        =   4
      Top             =   840
      Width           =   1320
   End
   Begin VB.CommandButton CmdClear 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Clear"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   1560
      TabIndex        =   42
      ToolTipText     =   "Click to clear or generate RR  distribution"
      Top             =   600
      Width           =   840
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   855
      Left            =   7680
      TabIndex        =   57
      Top             =   600
      Width           =   1215
      Begin VB.OptionButton OptNew 
         Caption         =   "New"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   51
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
      Begin VB.OptionButton OptCurrent 
         Caption         =   "Current"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   52
         TabStop         =   0   'False
         Top             =   480
         Width           =   975
      End
   End
   Begin VB.Label LblSetNumber 
      Alignment       =   2  'Center
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   4440
      TabIndex        =   65
      Top             =   120
      Width           =   615
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Name"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3960
      TabIndex        =   62
      Top             =   120
      Width           =   480
   End
   Begin VB.Label LblRR 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "D63.2"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
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
      Left            =   3960
      TabIndex        =   63
      Top             =   2520
      Width           =   615
   End
   Begin VB.Label LblRR 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Lambda"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
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
      Left            =   6240
      TabIndex        =   64
      Top             =   2520
      Width           =   720
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of sizes"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3960
      TabIndex        =   61
      Top             =   600
      Width           =   1800
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "% Passing"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2520
      TabIndex        =   56
      Top             =   120
      Width           =   1320
   End
   Begin VB.Label LblInfinity 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Infinity"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   120
      TabIndex        =   58
      Top             =   360
      Width           =   1320
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mesh size"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
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
      TabIndex        =   59
      Top             =   120
      Width           =   1320
   End
End
Attribute VB_Name = "Psd"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public CHANGE_FLAG  As Integer
Private NEWDATA As SIZEDATA
Private CFactor As ConversionFactor
Private Factor As Double

Private Sub ChkRR_Click()
  If ChkRR.Value = 1 Then
   Call SHOWRR
  Else
    Call HIDERR
  End If
End Sub

Private Sub CmdAccept_Click()
'****************************
  If Val(Nclasses) < 3 Then
    MsgBox "The number of size classes must be greater than 2", vbCritical, "ERROR"
    Nclasses.SetFocus
    Exit Sub
  End If
  
  If CurrentIndex = 0 Then
    NumberOfDataSets = NumberOfDataSets + 1
    CurrentIndex = NumberOfDataSets
    Call MakeNew(CURRDATA(NumberOfDataSets))
  Else
    Call MakeNew(CURRDATA(CurrentIndex))
  End If
  Unload Psd
  WSTRING = Str$(CURRDATA(CurrentIndex).ID) + " " + CURRDATA(CurrentIndex).SET_NAME
  Sizgraph.StreamList.AddItem WSTRING
  Sizgraph.CmdEditData.Enabled = -1
  Sizgraph.CmdSaveData.Enabled = -1
  Sizgraph.CmdSaveDataAs.Enabled = -1
  Sizgraph.Show
End Sub

Private Sub CmdCancel_Click()
  Unload Psd
  Sizgraph.Show
  If CurrentIndex > 0 Then
    WSTRING = Str$(CURRDATA(CurrentIndex).ID) + " " + CURRDATA(CurrentIndex).SET_NAME
    Sizgraph.StreamList.AddItem WSTRING
  End If
End Sub

Private Sub CmdClear_Click()
'***************************
  Dim Lambda As Single
  Dim CSIZE As Single, D632 As Single, RATIO As Single
  Dim NDC As Integer
  Dim I As Integer
  If OptNew.Value = -1 Then
    NDC = Val(Nclasses.text)
    RATIO = Sqr(2)
    D632 = Val(RRparameter(0).text)
    Lambda = Val(RRparameter(1).text)
    CSIZE = D632 * (4.605) ^ (1! / Lambda)
    For I = 1 To 24
      If ChkRR.Value = 1 Then
        upper_size(I - 1).text = Format$(CSIZE, "#.000E+#")
        Cum(I).text = Format$(ROSIN_RAMM(CSIZE, D632, Lambda) * 100, "####.00")
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
    Else
      upper_size(0).SetFocus
    End If
  End If
End Sub

Private Sub Cum_Change(Index As Integer)
'***************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Cum(Index).SetFocus
  End If
End Sub

Private Sub Cum_KeyPress(Index As Integer, keyAscii As Integer)
  If keyAscii = 13 And Index < NEWDATA.NDC - 1 Then
    If Index > 0 And Val(Cum(Index).text) > Val(Cum(Index - 1).text) Then
      WSTRING = "% passing must decrease"
      MsgBox WSTRING, 0, "ERROR"
    Else
      Cum(Index + 1).SetFocus
      keyAscii = 0
    End If
  End If
End Sub

Private Sub Cum_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Form_Load()
'**********************
  Dim I As Integer
  Psd.Top = 0
  Psd.Left = 0

  CHANGE_FLAG = 0
  If CurrentIndex = 0 Then
    OptCurrent.Enabled = 0
    NEWDATA.ID = 0
    NEWDATA.SET_NAME = ""
    NEWDATA.NDC = 2
    LblSetNumber = NumberOfDataSets + 1
    For I = 1 To 24
      Me.upper_size(I).Visible = False
      Me.Cum(I + 1).Visible = False
    Next I
  Else
    'Load the data set pointed to by CurrentIndex
    Call LoadNew(CURRDATA(CurrentIndex))
    Call MakeNew(NEWDATA)
  End If
  OptNew.Value = -1
  RRparameter(0).text = Str$(0.001)
  RRparameter(1).text = Str$(1.2)
  Call HIDERR
End Sub

Private Sub HIDERR()
    LblRR(0).Visible = 0
    RRparameter(0).Visible = 0
    LblRR(1).Visible = 0
    RRparameter(1).Visible = 0
End Sub

Private Sub HIDE_CLASS(I As Integer)
    upper_size(I - 1).Visible = 0
    Cum(I).Visible = 0
End Sub

Private Sub ID_KeyPress(keyAscii As Integer)
  If keyAscii = 13 Then
    SetName.SetFocus
    keyAscii = 0
  End If
End Sub

Private Sub ID_LostFocus()
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
  CHANGE_FLAG = 0
End Sub

Private Sub LoadNew(NDATA As SIZEDATA)
'************************************
  Dim I As Integer
  Dim Factor As Single
  Nclasses.text = Str$(NDATA.NDC)
  LblSetNumber.Caption = Str$(NDATA.ID)
  SetName.text = NDATA.SET_NAME
  Cum(0).text = Format$(100!, "####.00")
  Factor = 100
  For I = 1 To NDATA.NDC - 1
    upper_size(I - 1).text = Format$(NDATA.CUMSIZE(I), "0.000E+#")
    Cum(I).text = Format$(NDATA.fraction(I) * Factor, "####.00")
    Call SHOW_CLASS(I)
  Next I
  For I = NDATA.NDC To 25
    Call HIDE_CLASS(I)
  Next I
  OptSize(3).Value = -1
End Sub

Private Sub MakeNew(NDATA As SIZEDATA)
'*************************************
  Dim size(25) As Single, Factor As Single
  Dim N As Integer
  CHANGE_FLAG = 0
  NDATA.NDC = Val(Nclasses.text)
  NDATA.ID = Val(LblSetNumber.Caption)
  NDATA.SET_NAME = SetName.text

  'Convert the size to meters
  Factor = 1
  If OptSize(0).Value = -1 Then Factor = 0.000001
  If OptSize(1).Value = -1 Then Factor = 0.001
  If OptSize(2).Value = -1 Then Factor = 0.01
  If OptSize(4).Value = -1 Then Factor = 0.0254
  For N = 1 To NDATA.NDC - 1
    NDATA.CUMSIZE(N) = Val(upper_size(N - 1).text) * Factor
    NDATA.fraction(N) = Val(Cum(N).text) / 100
  Next N
  For N = NDATA.NDC To 25
    NDATA.CUMSIZE(N) = 0
    NDATA.fraction(N) = 0
  Next N
End Sub

Private Sub Nclasses_Change()
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Nclasses.SetFocus
  End If
End Sub

Private Sub Nclasses_LostFocus()
'*******************************
  Dim I As Integer
  If Val(Nclasses) > 25 Then
    MsgBox "No more than 25 size classes may be used", 48, "ERROR"
    Nclasses.text = "25"
    Exit Sub
  End If
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    For I = 2 To NEWDATA.NDC
      Call SHOW_CLASS(I - 1)
    Next I
    For I = NEWDATA.NDC To 25
      Call HIDE_CLASS(I)
    Next I
  End If
  CHANGE_FLAG = 0
End Sub

Private Sub OptCurrent_Click()
  Call LoadNew(CURRDATA(CurrentIndex))
End Sub

Private Sub OptNew_Click()
  Call LoadNew(NEWDATA)
End Sub

Private Sub optSize_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub SHOWRR()
    LblRR(0).Visible = -1
    RRparameter(0).Visible = -1
    LblRR(1).Visible = -1
    RRparameter(1).Visible = -1
End Sub

Private Sub SHOW_CLASS(I As Integer)
    upper_size(I - 1).Visible = -1
    Cum(I).Visible = -1
End Sub

Private Sub Upper_size_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub upper_size_KeyPress(Index As Integer, keyAscii As Integer)
  If keyAscii = 13 Then
    If Index < NEWDATA.NDC - 2 Then upper_size(Index + 1).SetFocus
    keyAscii = 0
    If Index > 0 Then
      If Val(upper_size(Index).text) > Val(upper_size(Index - 1).text) Then
        WSTRING = "Passing sizes must decrease"
        MsgBox WSTRING, 0, "ERROR"
        upper_size(Index).SetFocus
      End If
    End If
  End If
End Sub

Private Sub Upper_size_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer, NDC As Integer

NDC = Val(Nclasses.text)
CFactor = micron_mm_cm_m_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  For I = 0 To NDC - 1
    upper_size(I).text = Format(Val(upper_size(I).text) * Factor, "###.0000####")
  Next I
  OptSize(Index).Value = True
End If
End Sub
