VERSION 5.00
Begin VB.Form GMSU 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model GMSU"
   ClientHeight    =   7650
   ClientLeft      =   240
   ClientTop       =   1770
   ClientWidth     =   9570
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
   Icon            =   "gmsu.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7650
   ScaleWidth      =   9570
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   38
      Left            =   7440
      TabIndex        =   113
      Top             =   5040
      Width           =   615
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters based on"
      Height          =   1095
      Left            =   5640
      TabIndex        =   110
      Top             =   120
      Width           =   2175
      Begin VB.OptionButton optSelFuncSizeBase 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Upper mesh size"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   112
         Top             =   600
         Width           =   1935
      End
      Begin VB.OptionButton optSelFuncSizeBase 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Representative size"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   111
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
      Index           =   37
      Left            =   1920
      TabIndex        =   91
      Top             =   5400
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   36
      Left            =   1920
      TabIndex        =   90
      Top             =   5160
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   35
      Left            =   1920
      TabIndex        =   89
      Top             =   4920
      Width           =   615
   End
   Begin VB.ComboBox ComboFunction2 
      Height          =   330
      Left            =   120
      TabIndex        =   83
      Tag             =   "TransportModel"
      Top             =   6840
      Width           =   2535
   End
   Begin VB.CheckBox chkUseBondBallSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Use standard ball size distribution"
      Height          =   255
      Left            =   5520
      TabIndex        =   82
      Tag             =   "BallSize"
      Top             =   2160
      Width           =   3495
   End
   Begin VB.Frame frameBallSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size distribution"
      Height          =   2415
      Left            =   5520
      TabIndex        =   61
      Top             =   2520
      Width           =   3495
      Begin VB.OptionButton optBallSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   195
         Index           =   2
         Left            =   2520
         TabIndex        =   81
         Tag             =   "BallSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   1560
         Width           =   615
      End
      Begin VB.OptionButton optBallSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   195
         Index           =   1
         Left            =   2520
         TabIndex        =   80
         Tag             =   "BallSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   1200
         Width           =   615
      End
      Begin VB.OptionButton optBallSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   195
         Index           =   3
         Left            =   2520
         TabIndex        =   79
         Tag             =   "BallSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   1920
         Width           =   615
      End
      Begin VB.OptionButton optBallSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   195
         Index           =   0
         Left            =   2520
         TabIndex        =   78
         Tag             =   "BallSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   840
         Value           =   -1  'True
         Width           =   615
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   34
         Left            =   1560
         TabIndex        =   75
         Top             =   2040
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   33
         Left            =   360
         TabIndex        =   73
         Top             =   2040
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   32
         Left            =   1560
         TabIndex        =   72
         Top             =   1800
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   31
         Left            =   360
         TabIndex        =   71
         Top             =   1800
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   30
         Left            =   1560
         TabIndex        =   70
         Top             =   1560
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   29
         Left            =   360
         TabIndex        =   69
         Top             =   1560
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   28
         Left            =   1560
         TabIndex        =   68
         Top             =   1320
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   27
         Left            =   360
         TabIndex        =   67
         Top             =   1320
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   26
         Left            =   1560
         TabIndex        =   66
         Top             =   1080
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   25
         Left            =   360
         TabIndex        =   65
         Top             =   1080
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   24
         Left            =   1560
         TabIndex        =   64
         Top             =   840
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   23
         Left            =   360
         TabIndex        =   63
         Top             =   840
         Width           =   735
      End
      Begin VB.TextBox Parameter 
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
         Height          =   225
         Index           =   22
         Left            =   1800
         TabIndex        =   62
         Text            =   "6"
         Top             =   240
         Width           =   735
      End
      Begin VB.Label Label22 
         BackColor       =   &H00C0C0C0&
         Caption         =   "% in charge"
         Height          =   255
         Left            =   1440
         TabIndex        =   77
         Top             =   600
         Width           =   975
      End
      Begin VB.Label Label21 
         Alignment       =   2  'Center
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ball size"
         Height          =   255
         Left            =   360
         TabIndex        =   76
         Top             =   600
         Width           =   735
      End
      Begin VB.Label lblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of sizes"
         Height          =   255
         Index           =   22
         Left            =   240
         TabIndex        =   74
         Top             =   240
         Width           =   1335
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   3720
      TabIndex        =   59
      Top             =   3000
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   4560
      TabIndex        =   58
      Top             =   3000
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   4560
      TabIndex        =   55
      Top             =   2760
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   4560
      TabIndex        =   54
      Top             =   1800
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   4560
      TabIndex        =   53
      Top             =   2040
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   4560
      TabIndex        =   52
      Top             =   2520
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   4560
      TabIndex        =   51
      Top             =   2280
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   3720
      TabIndex        =   49
      Top             =   2760
      Width           =   615
   End
   Begin VB.CheckBox chkOverfill 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for overfilling?"
      Height          =   240
      Left            =   2880
      TabIndex        =   45
      Tag             =   "Over"
      Top             =   5760
      Width           =   1935
   End
   Begin VB.CheckBox ChkClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for post classification"
      Height          =   255
      Left            =   120
      TabIndex        =   44
      Tag             =   "ClassificationFunction"
      Top             =   5760
      Width           =   2775
   End
   Begin VB.ComboBox ComboFunction 
      BackColor       =   &H00FFFFFF&
      Height          =   330
      Left            =   120
      TabIndex        =   43
      Tag             =   "ClassificationFunction"
      Top             =   6240
      Width           =   1935
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   3480
      TabIndex        =   42
      Top             =   6120
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   21
      Left            =   3480
      TabIndex        =   41
      Top             =   6360
      Width           =   735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   4920
      TabIndex        =   39
      Top             =   1080
      Width           =   615
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      Height          =   615
      Left            =   5520
      TabIndex        =   36
      Tag             =   "Size"
      Top             =   1440
      Width           =   2175
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Feet"
         Height          =   255
         Index           =   1
         Left            =   1320
         TabIndex        =   38
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   615
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "meters"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   37
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   855
      End
   End
   Begin VB.CheckBox ChkMakeAMDiagram 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make Andrews-Mika diagram on exit using the beta function model."
      Height          =   495
      Left            =   5280
      TabIndex        =   35
      Top             =   6120
      Width           =   3735
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   3720
      TabIndex        =   32
      Top             =   2520
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   3720
      TabIndex        =   30
      Top             =   2280
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   3720
      TabIndex        =   28
      Top             =   2040
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   1800
      TabIndex        =   26
      Top             =   3600
      Width           =   615
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Choose a liberation model"
      Height          =   720
      Left            =   5280
      TabIndex        =   23
      Top             =   5400
      Width           =   3735
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
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
         TabIndex        =   34
         TabStop         =   0   'False
         Tag             =   "Liberation"
         Top             =   360
         Value           =   -1  'True
         Width           =   855
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Beta function"
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
         Left            =   2280
         TabIndex        =   25
         Tag             =   "Liberation"
         Top             =   360
         Width           =   1335
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ljubljana"
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
         TabIndex        =   24
         TabStop         =   0   'False
         Tag             =   "Liberation"
         Top             =   360
         Width           =   1095
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   3720
      TabIndex        =   0
      Top             =   1800
      Width           =   615
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   5400
      TabIndex        =   11
      Top             =   6720
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7200
      TabIndex        =   10
      Top             =   6720
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   8040
      TabIndex        =   22
      Top             =   0
      Width           =   1455
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   9
         Tag             =   "Default"
         Top             =   720
         Width           =   960
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1305
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   1800
      TabIndex        =   6
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   1800
      TabIndex        =   5
      Top             =   4080
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   1800
      TabIndex        =   4
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   4920
      TabIndex        =   3
      Top             =   840
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   4920
      TabIndex        =   2
      Top             =   600
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   4920
      TabIndex        =   1
      Top             =   360
      Width           =   615
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specific gravity of media:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   38
      Left            =   5400
      TabIndex        =   114
      Top             =   5040
      Width           =   1920
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball distribution 6"
      Height          =   255
      Index           =   34
      Left            =   4320
      TabIndex        =   109
      Top             =   4560
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 6"
      Height          =   255
      Index           =   33
      Left            =   3720
      TabIndex        =   108
      Top             =   4560
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball distribution 5"
      Height          =   255
      Index           =   32
      Left            =   4320
      TabIndex        =   107
      Top             =   4320
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 5"
      Height          =   255
      Index           =   31
      Left            =   3720
      TabIndex        =   106
      Top             =   4320
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball distribution 4"
      Height          =   255
      Index           =   30
      Left            =   4320
      TabIndex        =   105
      Top             =   4080
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 4"
      Height          =   255
      Index           =   29
      Left            =   3720
      TabIndex        =   104
      Top             =   4080
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball distribution 3"
      Height          =   255
      Index           =   28
      Left            =   4320
      TabIndex        =   103
      Top             =   3840
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 3"
      Height          =   255
      Index           =   27
      Left            =   3720
      TabIndex        =   102
      Top             =   3840
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball distribution 2"
      Height          =   255
      Index           =   26
      Left            =   4320
      TabIndex        =   101
      Top             =   3600
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 2"
      Height          =   255
      Index           =   25
      Left            =   3720
      TabIndex        =   100
      Top             =   3600
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Baal distribution 1"
      Height          =   255
      Index           =   24
      Left            =   4320
      TabIndex        =   99
      Top             =   3360
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size 1"
      Height          =   255
      Index           =   23
      Left            =   3720
      TabIndex        =   98
      Top             =   3360
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make up ball size"
      Height          =   255
      Index           =   19
      Left            =   5280
      TabIndex        =   97
      Top             =   3000
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fuul scale miil speed"
      Height          =   255
      Index           =   18
      Left            =   5280
      TabIndex        =   96
      Top             =   2760
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Label1"
      Height          =   255
      Index           =   17
      Left            =   5280
      TabIndex        =   95
      Top             =   2520
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Full scale ball load"
      Height          =   255
      Index           =   16
      Left            =   5280
      TabIndex        =   94
      Top             =   2280
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Full scale length"
      Height          =   255
      Index           =   15
      Left            =   5280
      TabIndex        =   93
      Top             =   2040
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Full scale diameter"
      Height          =   255
      Index           =   14
      Left            =   5280
      TabIndex        =   92
      Top             =   1800
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 3:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   37
      Left            =   240
      TabIndex        =   88
      Top             =   5400
      Width           =   1680
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 2:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   36
      Left            =   240
      TabIndex        =   87
      Top             =   5160
      Width           =   1680
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 1:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   35
      Left            =   240
      TabIndex        =   86
      Top             =   4920
      Width           =   1680
   End
   Begin VB.Label Label23 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fractional residence times:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   85
      Top             =   4680
      Width           =   2415
   End
   Begin VB.Label LblClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Transport model"
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   84
      Top             =   6600
      Width           =   1935
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make-up ball size  mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   13
      Left            =   240
      TabIndex        =   60
      Top             =   3000
      Width           =   2280
   End
   Begin VB.Label Label18 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Full scale mill"
      Height          =   375
      Left            =   4560
      TabIndex        =   57
      Top             =   1320
      Width           =   735
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label17 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill"
      Height          =   375
      Left            =   3840
      TabIndex        =   56
      Top             =   1320
      Width           =   495
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fraction of media filled by solids  U (Dry basis)"
      Height          =   255
      Index           =   11
      Left            =   240
      TabIndex        =   50
      Top             =   2520
      Width           =   3495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "D50  mm"
      Height          =   255
      Index           =   20
      Left            =   2160
      TabIndex        =   48
      Top             =   6120
      Width           =   1335
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Sharpness index"
      Height          =   255
      Index           =   21
      Left            =   2160
      TabIndex        =   47
      Top             =   6360
      Width           =   1335
   End
   Begin VB.Label LblClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Classification function"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   46
      Top             =   6000
      Width           =   1935
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Exponent for rate of decrease of selection function   Lambda"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   360
      TabIndex        =   40
      Top             =   1080
      Width           =   4455
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill speed as percentage of critical"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   12
      Left            =   240
      TabIndex        =   33
      Top             =   2760
      Width           =   2520
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball load  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   240
      TabIndex        =   31
      Top             =   2280
      Width           =   2160
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill length"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   240
      TabIndex        =   29
      Top             =   2040
      Width           =   1440
   End
   Begin VB.Label Label11 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill dimensions:"
      Height          =   255
      Left            =   120
      TabIndex        =   27
      Top             =   1560
      Width           =   1575
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for breakage function:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   15
      Top             =   3360
      Width           =   3015
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Particle size exponent alpha"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   360
      TabIndex        =   13
      Top             =   600
      Width           =   4455
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for selection function in test mill:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   12
      Top             =   120
      Width           =   3360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill diameter"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   240
      TabIndex        =   19
      Top             =   1800
      Width           =   2175
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specific rate of breakage at 1mm  S1    1/min"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   360
      TabIndex        =   14
      Top             =   360
      Width           =   4455
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size coefficient for maximum breakage rate mu    mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   360
      TabIndex        =   21
      Top             =   840
      Width           =   4455
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   360
      TabIndex        =   16
      Top             =   3600
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Gamma"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   360
      TabIndex        =   17
      Top             =   3840
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Delta"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   360
      TabIndex        =   20
      Top             =   4080
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Phi at 5mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   360
      TabIndex        =   18
      Top             =   4320
      Width           =   1440
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
Attribute VB_Name = "GMSU"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub MakeBondBallSizeDistribution()
'*****************************************
  Dim Mk(6) As Single, Dk(6) As Single, K As Integer, D As Single
  Dim DMax As Single, Dmin As Single, Mclas As Integer, Factor As Single
  
  Mclas = Val(Me.Parameter(22).text)
  If Mclas > 6 Then
    MsgBox "No more than 6 ball size classes are allowed", vbCritical, "ERROR"
    Me.Parameter(22).SetFocus
    Exit Sub
  End If
  DMax = Val(Me.Parameter(19).text)
  Dmin = 0.2 * DMax
  Factor = (Dmin / DMax) ^ (1 / Mclas)
  D = DMax
  For K = 1 To Mclas
    Mk(K) = BallSizeDistribution(D, DMax, Dmin)
    D = DMax * Factor ^ K
    If K = Mclas Then D = Dmin
    Mk(K) = Mk(K) - BallSizeDistribution(D, DMax, Dmin)
    Dk(K) = DMax * Factor ^ (K - 0.5)
    Me.Parameter(23 + 2 * (K - 1)).text = Format(Dk(K), "##.0")
    Me.Parameter(24 + 2 * (K - 1)).text = Format(Mk(K) * 100, "###.00")
  Next K
  optBallSize(0) = True
End Sub

Private Function BallSizeDistribution(D As Single, DMax As Single, Dmin As Single) As Single
'*******************************************************************************************
  Dim Delta As Single
  Delta = 0
  BallSizeDistribution = (D ^ (4 - Delta) - Dmin ^ (4 - Delta)) / (DMax ^ (4 - Delta) - Dmin ^ (4 - Delta))
End Function

Private Sub Check1_Click()

End Sub

Private Sub ChkClassification_Click()
'************************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If ChkClassification.Value = 1 Then
      ComboFunction.Visible = True
      ComboFunction2.Visible = True
      LblParameter(20).Visible = True
      LblParameter(21).Visible = True
      LblClassification(2).Visible = True
      LblClassification(3).Visible = True
      Parameter(20).Visible = True
      Parameter(21).Visible = True
    Else
      ComboFunction.Visible = False
      ComboFunction2.Visible = False
      LblParameter(20).Visible = False
      LblParameter(21).Visible = False
      LblClassification(2).Visible = False
      LblClassification(3).Visible = False
      Parameter(20).Visible = False
      Parameter(21).Visible = False
    End If
  End If
End Sub

Private Sub ChkClassification_LostFocus()
'****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub chkOverfill_Click()
'******************************
  If TypeOf Screen.ActiveControl Is CheckBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub
Private Sub chkOverfill_LostFocus()
'**********************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub chkUseBondBallSize_Click()
'*************************************
  If Screen.ActiveControl.Tag = "BallSize" Then
    If chkUseBondBallSize = vbChecked Then
      Call MakeBondBallSizeDistribution
    End If
  End If
End Sub

Private Sub ChkUsesRepSizes_Click()

End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim response As Integer, I As Integer
  Dim RetVal As Long
  Dim BrFuncParameters(1 To 4) As Single
  Dim sum As Single, Mclas As Integer
  
  'Check ball size distribution
  Mclas = Me.Parameter(22)
  sum = 0#
  For I = 1 To Mclas
    sum = sum + Val(Me.Parameter(24 + 2 * (I - 1)))
  Next I
  If Abs(sum - 100) > 0.1 Then
    MsgBox "Distribution of ball sizes must add to 100%" & Chr(13) & Chr(10) & " Your sum is " & Format(sum, "###.00"), vbCritical, "ERRROR"
    Me.Parameter(24).SetFocus
    Exit Sub
  End If
  
  'Check fractional residence times
  sum = Val(Parameter(35)) + Val(Parameter(36)) + Val(Parameter(37))
  If Abs(sum - 1#) > 0.001 Then
    MsgBox "Fractional residence times must add to 1" & Chr(13) & Chr(10) & " Your sum is " & Format(sum, "###.00"), vbCritical, "ERRROR"
    Me.Parameter(35).SetFocus
    Exit Sub
  End If
  
  'Check media density
  If Val(Parameter(38)) < 1# Then
    MsgBox "Media density is too low.", vbExclamation, "INFORMATION"
    Me.Parameter(38).SetFocus
    Exit Sub
  End If
  Call MakeNew(CURRMODELDATA(ID))
  For I = 1 To 4
    BrFuncParameters(I) = CURRMODELDATA(ID).PARAM(I + 3)
  Next I
  If ChkMakeAMDiagram.Value = 1 And OptLibModel(2).Value Then
    response = MsgBox("Make Andrews-Mika diagram?", 65, "INFORMATION")
    If response = vbOK Then
      Me.MousePointer = 11
      NameOfJob = MODSIM.JOBNAME
      RetVal = BetaAMD(JobPath, NameOfJob, BrFuncParameters(1))
      Me.MousePointer = 0
      If RetVal > -1 Then
        MsgBox " Error in Andrews-Mika diagram calculation " & CStr(RetVal), 64, "INFORMATION"
        JobBetaAMD = False
      Else
        JobBetaAMD = True
      End If
    End If
  End If
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload GMSU
End Sub

Private Sub CmdCancel_Click()
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload GMSU
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  'Unload Help  'In case help was called
  If FastParameterChange Then
    UnitMods.cmdQuit_Click
    FastParameterChange = False
  End If
End Sub

Private Sub ComboFunction_Change()
'*********************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction_Click()
'*******************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction_LostFocus()
'************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub ComboFunction2_Change()
'*********************************
  If Screen.ActiveControl.Tag = "TransportModel" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction2_Click()
  If Screen.ActiveControl.Tag = "TransportModel" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction2_LostFocus()
'************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub Form_Load()
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  ComboFunction.AddItem "Logistic"
  ComboFunction.AddItem "Rosin Rammler"
  ComboFunction.AddItem "Exponential sum"
  ComboFunction2.AddItem "PM PM (PM,CL)"
  ComboFunction2.AddItem "(PM PM PM),CL"
  ComboFunction2.AddItem "(PM,CL) (PM,CL) (PM,CL)"

  'Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "GMSU" Then
    
    OptCurrent.Enabled = 0
    Call LoadDefault
    Call MakeNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  If Val(Sysdata.NGCM.text) < 5 Then
    OptLibModel(1).Enabled = False
    OptLibModel(2).Enabled = False
  End If
  'Must unload Sysdata now otherwise it remains loaded and will not initialize properly when next needed.
  Unload Sysdata
  Exit Sub

ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

Private Sub LoadDefault()
'************************
  OptDefault.Value = -1
  'Use default data for quartz
  Parameter(0).text = Str$(1.3)
  Parameter(1).text = Str$(0.491)
  Parameter(2).text = Str$(2)
  Parameter(3).text = Str$(2)
  Parameter(4).text = Str$(3.2)
  Parameter(5).text = Str$(0.52)
  Parameter(6).text = Str$(0)
  Parameter(7).text = Str$(0.158)
  Parameter(8).text = Str$(0.38)
  Parameter(9).text = Str$(0.38)
  Parameter(10).text = Str$(40)
  Parameter(11).text = Str$(0.4)
  Parameter(12).text = Str$(74)
  Parameter(13).text = Str$(18.75)
  Parameter(14).text = Str$(5#)
  Parameter(15).text = Str$(5.5)
  Parameter(16).text = Str$(40)
  Parameter(17).text = Str$(0.4)
  Parameter(18).text = Str$(74)
  Parameter(19).text = Str$(50)
  Parameter(20).text = Str$(2)
  Parameter(21).text = Str$(0.75)
  Parameter(22).text = Str$(1)
  Parameter(35).text = Str$(0.0137)
  Parameter(36).text = Str$(0.2123)
  Parameter(37).text = Str$(0.774)
  Parameter(38).text = Str$(7.8)
  OptLibModel(0).Value = True
  OptSize(0).Value = True
  chkOverfill.Value = 0
  ChkClassification.Value = 0
  ComboFunction.Visible = False
  ComboFunction2.Visible = False
  LblParameter(20).Visible = False
  Parameter(20).Visible = False
  LblParameter(21).Visible = False
  Parameter(21).Visible = False
  ChkMakeAMDiagram.Value = 0
  ComboFunction.ListIndex = 0
  ComboFunction2.ListIndex = 0
  chkUseBondBallSize = vbChecked
  Me.optSelFuncSizeBase(0).Value = True
  Call MakeBondBallSizeDistribution
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
    For I = 1 To 35
      Parameter(I - 1).text = CStr(NDATA.PARAM(I))
    Next I
    Parameter(35).text = CStr(NDATA.PARAM(40))
    Parameter(36).text = CStr(NDATA.PARAM(41))
    Parameter(37).text = CStr(1# - NDATA.PARAM(40) - NDATA.PARAM(41))
    Parameter(38).text = CStr(NDATA.PARAM(43))

    If NDATA.PARAM(36) = 0 Then OptLibModel(0).Value = True
    If NDATA.PARAM(36) = 1 Then OptLibModel(1).Value = True
    If NDATA.PARAM(36) = 2 Then OptLibModel(2).Value = True
    OptSize(0).Value = True
    chkOverfill.Value = NDATA.PARAM(37)
    If NDATA.PARAM(42) = 0 Then
      Me.optSelFuncSizeBase(0).Value = True
    Else
      Me.optSelFuncSizeBase(1).Value = True
    End If
    If NDATA.PARAM(38) = 0 Then
      ChkClassification.Value = 0
      ComboFunction.Visible = False
      ComboFunction2.Visible = False
      LblParameter(20).Visible = False
      LblParameter(21).Visible = False
      LblClassification(2).Visible = False
      LblClassification(3).Visible = False
      Parameter(20).Visible = False
      Parameter(21).Visible = False
    Else
      ChkClassification.Value = 1
      ComboFunction.Visible = True
      ComboFunction2.Visible = True
      LblParameter(20).Visible = True
      LblParameter(21).Visible = True
      LblClassification(2).Visible = True
      LblClassification(3).Visible = True
      ComboFunction.ListIndex = NDATA.PARAM(38) - 1
      ComboFunction2.ListIndex = NDATA.PARAM(39)
      Parameter(20).Visible = True
      Parameter(21).Visible = True
    End If
    For I = 1 To NDATA.PARAM(23)
      Me.Parameter(23 + 2 * (I - 1)).Visible = True
      Me.Parameter(24 + 2 * (I - 1)).Visible = True
    Next I
    For I = NDATA.PARAM(23) + 1 To 6
      Me.Parameter(23 + 2 * (I - 1)).Visible = False
      Me.Parameter(24 + 2 * (I - 1)).Visible = False
    Next I
    optBallSize(2) = True

  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer, Mclas As Integer
  Dim ScaleFactor As Single
  
  CHANGE_FLAG = 0
  NDATA.Model = "GMSU"
  Mclas = Val(Me.Parameter(22).text)
  If Mclas < 1 Or Mclas > 6 Then
    MsgBox "Number of ball sizes must be between 1 and 6", vbCritical, "ERROR"
'    Me.Parameter(22).SetFocus
    Exit Sub
  End If
  If chkUseBondBallSize = vbChecked Then
    Call MakeBondBallSizeDistribution
  End If
  NDATA.NOPAR = 43
  For I = 1 To 35
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If OptLibModel(0).Value Then NDATA.PARAM(36) = 0
  If OptLibModel(1).Value Then NDATA.PARAM(36) = 1
  If OptLibModel(2).Value Then NDATA.PARAM(36) = 2
  If OptSize(0).Value Then ScaleFactor = 1#
  If OptSize(1).Value Then ScaleFactor = 0.3048
  NDATA.PARAM(9) = NDATA.PARAM(9) * ScaleFactor
  NDATA.PARAM(10) = NDATA.PARAM(10) * ScaleFactor
  NDATA.PARAM(15) = NDATA.PARAM(15) * ScaleFactor
  NDATA.PARAM(16) = NDATA.PARAM(16) * ScaleFactor
  NDATA.PARAM(37) = chkOverfill.Value
  If ChkClassification.Value = 0 Then
    NDATA.PARAM(38) = 0
    NDATA.PARAM(39) = 0
  Else
    NDATA.PARAM(38) = ComboFunction.ListIndex + 1
    NDATA.PARAM(39) = ComboFunction2.ListIndex
  End If
  NDATA.PARAM(40) = Val(Parameter(35).text)
  NDATA.PARAM(41) = Val(Parameter(36).text)
  If Me.optSelFuncSizeBase(0) Then
    NDATA.PARAM(42) = 0
  Else
    NDATA.PARAM(42) = 1
  End If
  'Added here density of media (usually steel balls)
  NDATA.PARAM(43) = Val(Parameter(38).text)
  ScaleFactor = 1#
  If optBallSize(0) Then
    ScaleFactor = mm_cm_m_inch.Factor(1)
  ElseIf optBallSize(1) Then
    ScaleFactor = mm_cm_m_inch.Factor(2)
  ElseIf optBallSize(3) Then
    ScaleFactor = mm_cm_m_inch.Factor(4)
  End If
  NDATA.PARAM(24) = NDATA.PARAM(24) * ScaleFactor
  NDATA.PARAM(26) = NDATA.PARAM(26) * ScaleFactor
  NDATA.PARAM(28) = NDATA.PARAM(28) * ScaleFactor
  NDATA.PARAM(30) = NDATA.PARAM(30) * ScaleFactor
  NDATA.PARAM(32) = NDATA.PARAM(32) * ScaleFactor
  NDATA.PARAM(34) = NDATA.PARAM(34) * ScaleFactor
  
End Sub

Private Sub MnuAccept_Click()
'****************************
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
PrintForm
End Sub

Private Sub optBallSize_Click(Index As Integer)
'**********************************************
  If Screen.ActiveControl.Tag = "BallSize" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub
Private Sub optBallSize_LostFocus(Index As Integer)
'**************************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub OptBallSize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
  Dim I As Integer
  Dim Factor As Double
  
  CFactor = mm_cm_m_inch
  If Button = vbRightButton Then
    'Find the currently set index
    For I = 0 To CFactor.Number - 1
      If optBallSize(I).Value = True Then
        Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
      End If
    Next I
    For I = 23 To 33 Step 2
      Parameter(I).text = Format(Val(Parameter(I).text) * Factor, "##.00000")
      optBallSize(Index).Value = True
    Next I
  End If
End Sub

Private Sub OptCurrent_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptLibModel_Click(Index As Integer)
'**********************************************
  If Screen.ActiveControl.Tag = "Liberation" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If OptLibModel(2).Value Then
      ChkMakeAMDiagram.Value = 1
    Else
      ChkMakeAMDiagram.Value = 0
    End If
  End If
End Sub

Private Sub OptLibModel_LostFocus(Index As Integer)
'**************************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub optSize_Click(Index As Integer)
'*****************************************
  If Screen.ActiveControl.Tag = "Size" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
  Dim I As Integer
  Dim Factor As Double
  
  CFactor = m_feet
  If Button = vbRightButton Then
    'Find the currently set index
    For I = 0 To CFactor.Number - 1
      If OptSize(I).Value = True Then
        Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
      End If
    Next I
    For I = 8 To 9
      Parameter(I).text = Format(Val(Parameter(I).text) * Factor, "#0.000")
      OptSize(Index).Value = True
    Next I
    For I = 14 To 15
      Parameter(I).text = Format(Val(Parameter(I).text) * Factor, "#0.000")
      OptSize(Index).Value = True
    Next I
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'************************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    If OptNew.Value = -1 Then Call LoadNew(NEWDATA)
    If Index >= 3 And Index <= 6 Then
      ChkMakeAMDiagram.Value = 1
    End If
  End If
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = LblParameter(Index).Caption
  Repeat.TxtValue(0) = Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
  If Index >= 39 Then IDIFF = 5
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
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub

