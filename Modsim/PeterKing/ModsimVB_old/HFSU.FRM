VERSION 5.00
Begin VB.Form HFSU 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model HFSU"
   ClientHeight    =   6735
   ClientLeft      =   240
   ClientTop       =   1770
   ClientWidth     =   8880
   BeginProperty Font 
      Name            =   "Arial"
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
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6735
   ScaleWidth      =   8880
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   16
      Left            =   1920
      TabIndex        =   61
      Top             =   3120
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   17
      Left            =   1920
      TabIndex        =   60
      Top             =   3360
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00E0E0E0&
      BorderStyle     =   0  'None
      Enabled         =   0   'False
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
      Height          =   225
      Index           =   18
      Left            =   1920
      TabIndex        =   59
      Top             =   3600
      Width           =   615
   End
   Begin VB.ComboBox ComboFunction2 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   330
      Left            =   360
      TabIndex        =   57
      Tag             =   "TransportModel"
      Top             =   5640
      Width           =   2535
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   15
      Left            =   6000
      TabIndex        =   55
      Top             =   4320
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   14
      Left            =   6000
      TabIndex        =   52
      Top             =   4560
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   13
      Left            =   6000
      TabIndex        =   50
      Top             =   4080
      Width           =   615
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Choose a model for net mill power"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   705
      Left            =   4320
      TabIndex        =   46
      Top             =   2040
      Width           =   4335
      Begin VB.OptionButton OptPowerModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Austin"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   3
         Left            =   3240
         TabIndex        =   56
         Tag             =   "Power"
         Top             =   360
         Width           =   975
      End
      Begin VB.OptionButton OptPowerModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Bond"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   49
         TabStop         =   0   'False
         Tag             =   "Power"
         Top             =   360
         Width           =   735
      End
      Begin VB.OptionButton OptPowerModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Morrel"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   2
         Left            =   2160
         TabIndex        =   48
         Tag             =   "Power"
         Top             =   360
         Width           =   975
      End
      Begin VB.OptionButton OptPowerModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
         BeginProperty Font 
            Name            =   "Arial"
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
         TabIndex        =   47
         TabStop         =   0   'False
         Tag             =   "Power"
         Top             =   360
         Width           =   855
      End
   End
   Begin VB.CheckBox ChkClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for post classification"
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
      Left            =   120
      TabIndex        =   42
      Tag             =   "Classification"
      Top             =   4320
      Width           =   2775
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   11
      Left            =   4680
      TabIndex        =   41
      Top             =   4920
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   12
      Left            =   4680
      TabIndex        =   40
      Top             =   5160
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.ComboBox CombFunction 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   330
      Left            =   360
      TabIndex        =   39
      Tag             =   "ClassificationFunction"
      Top             =   4920
      Visible         =   0   'False
      Width           =   2535
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units fo size"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   6720
      TabIndex        =   36
      Tag             =   "Size"
      Top             =   3120
      Width           =   2055
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Feet"
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
         Index           =   1
         Left            =   1200
         TabIndex        =   38
         Tag             =   "Size"
         Top             =   240
         Width           =   735
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "meters"
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
         TabIndex        =   37
         Tag             =   "Size"
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.CheckBox ChkMakeAMDiagram 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make Andrews-Mika diagram on exit using the beta function model"
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
      Left            =   240
      TabIndex        =   35
      Top             =   6120
      Width           =   5175
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   10
      Left            =   6000
      TabIndex        =   32
      Top             =   3840
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   9
      Left            =   6000
      TabIndex        =   30
      Top             =   3600
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   8
      Left            =   6000
      TabIndex        =   28
      Top             =   3360
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   3
      Left            =   1800
      TabIndex        =   26
      Top             =   1440
      Width           =   615
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Choose a liberation model"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   720
      Left            =   4320
      TabIndex        =   23
      Top             =   1200
      Width           =   4335
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
         BeginProperty Font 
            Name            =   "Arial"
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
         Width           =   1095
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Beta function"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   2
         Left            =   2640
         TabIndex        =   25
         Tag             =   "Liberation"
         Top             =   360
         Width           =   1575
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ljubljana"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   1320
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
      Height          =   225
      Index           =   7
      Left            =   6000
      TabIndex        =   0
      Top             =   3120
      Width           =   615
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
      Height          =   600
      Left            =   6120
      TabIndex        =   11
      Top             =   5520
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
      Height          =   600
      Left            =   7560
      TabIndex        =   10
      Top             =   5520
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
      Height          =   1095
      Left            =   7080
      TabIndex        =   22
      Top             =   0
      Width           =   1695
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
         Height          =   240
         Left            =   120
         TabIndex        =   9
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
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
         Height          =   240
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
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
         Height          =   240
         Left            =   120
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   6
      Left            =   1800
      TabIndex        =   6
      Top             =   2160
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   5
      Left            =   1800
      TabIndex        =   5
      Top             =   1920
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   4
      Left            =   1800
      TabIndex        =   4
      Top             =   1680
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   2
      Left            =   4560
      TabIndex        =   3
      Top             =   840
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   1
      Left            =   4560
      TabIndex        =   2
      Top             =   600
      Width           =   615
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   0
      Left            =   4560
      TabIndex        =   1
      Top             =   360
      Width           =   615
   End
   Begin VB.Label Label23 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fractional residence times:"
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
      TabIndex        =   65
      Top             =   2880
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 1:"
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
      Index           =   16
      Left            =   240
      TabIndex        =   64
      Top             =   3120
      Width           =   1680
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 2:"
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
      Index           =   17
      Left            =   240
      TabIndex        =   63
      Top             =   3360
      Width           =   1680
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Perf. Mixed Region 3:"
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
      Index           =   37
      Left            =   240
      TabIndex        =   62
      Top             =   3600
      Width           =   1680
   End
   Begin VB.Label LblTransportModel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Transport model"
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
      Left            =   240
      TabIndex        =   58
      Top             =   5400
      Width           =   1935
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Makeup ball size (mm)"
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
      Index           =   15
      Left            =   3840
      TabIndex        =   54
      Top             =   4320
      Width           =   2160
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Net grinding power (kW)"
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
      Index           =   14
      Left            =   3840
      TabIndex        =   53
      Top             =   4560
      Width           =   2160
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specific gravity of media"
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
      Index           =   13
      Left            =   3840
      TabIndex        =   51
      Top             =   4080
      Width           =   2160
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "D50  mm"
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
      Index           =   11
      Left            =   3240
      TabIndex        =   45
      Top             =   4920
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Sharpness index"
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
      Index           =   12
      Left            =   3240
      TabIndex        =   44
      Top             =   5160
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Label LblClassificationFunction 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Classification function"
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
      Left            =   240
      TabIndex        =   43
      Top             =   4680
      Visible         =   0   'False
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill speed  % of critical speed"
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
      Index           =   10
      Left            =   3840
      TabIndex        =   33
      Top             =   3840
      Width           =   2160
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball load  %"
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
      Index           =   9
      Left            =   3840
      TabIndex        =   31
      Top             =   3600
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill length"
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
      Index           =   8
      Left            =   3840
      TabIndex        =   29
      Top             =   3360
      Width           =   1440
   End
   Begin VB.Label Label11 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill dimensions:"
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
      Left            =   3720
      TabIndex        =   27
      Top             =   2880
      Width           =   1215
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for breakage function:"
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
      TabIndex        =   15
      Top             =   1200
      Width           =   3015
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Zeta 1"
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
      Left            =   360
      TabIndex        =   13
      Top             =   600
      Width           =   4095
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for enegy-specific selection function:"
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
      Top             =   120
      Width           =   4200
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill diameter"
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
      Index           =   7
      Left            =   3840
      TabIndex        =   19
      Top             =   3120
      Width           =   1455
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Energy-specific selection function at 1mm  tonnes/kWhr"
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
      Left            =   360
      TabIndex        =   14
      Top             =   360
      Width           =   4095
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Zeta 2"
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
      Left            =   360
      TabIndex        =   21
      Top             =   840
      Width           =   4095
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta"
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
      Left            =   360
      TabIndex        =   16
      Top             =   1440
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Gamma"
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
      Left            =   360
      TabIndex        =   17
      Top             =   1680
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Delta"
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
      Index           =   5
      Left            =   360
      TabIndex        =   20
      Top             =   1920
      Width           =   1440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Phi at 5mm"
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
      Index           =   6
      Left            =   360
      TabIndex        =   18
      Top             =   2160
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
Attribute VB_Name = "HFSU"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub ChkClassification_Click()
'************************************
  If Screen.ActiveControl.Tag = "Classification" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If ChkClassification.Value = 1 Then
      CombFunction.Visible = True
      ComboFunction2.Enabled = True
      ComboFunction2.text = ComboFunction2.List(0)
      ComboFunction2.ListIndex = 0
      lblParameter(11).Visible = True
      lblParameter(12).Visible = True
      LblClassificationFunction.Visible = True
      Parameter(11).Visible = True
      Parameter(12).Visible = True
    Else
      CombFunction.Visible = False
      ComboFunction2.Enabled = False
      ComboFunction2.text = ComboFunction2.List(3)
      ComboFunction2.ListIndex = 3
      lblParameter(11).Visible = False
      lblParameter(12).Visible = False
      LblClassificationFunction.Visible = False
      Parameter(11).Visible = False
      Parameter(12).Visible = False
    End If
  End If
End Sub

Private Sub ChkClassification_LostFocus()
'****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)

End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim response As Integer, I As Integer
  Dim RetVal As Long
  Dim BrFuncParameters(1 To 4) As Single
  Dim sum As Single
  
  'Check fractional residence times
  sum = Val(Parameter(16)) + Val(Parameter(17)) + Val(Parameter(18))
  If Abs(sum - 1#) > 0.001 Then
    MsgBox "Fractional residence times must add to 1" & Chr(13) & Chr(10) & " Your sum is " & Format(sum, "###.00"), vbCritical, "ERRROR"
    Me.Parameter(16).SetFocus
    Exit Sub
  End If
  
  'Check the transport model
  If Me.ChkClassification.Value > 0 Then
    If Me.ComboFunction2.ListIndex = 3 Then
      MsgBox "Transport model must include at least one classifier", vbCritical, "ERROR"
      Me.ComboFunction2.SetFocus
      Exit Sub
    End If
  Else
    If Me.ComboFunction2.ListIndex < 3 Then
      MsgBox "Transport model cannot include a classifier", vbCritical, "ERROR"
      Me.ComboFunction2.SetFocus
      Exit Sub
    End If
  End If
  
  'Check that classification model has been selected
  If Me.ChkClassification.Value > 0 Then
    If Me.CombFunction.text = "" Then
      MsgBox "A classification model must be selected", vbCritical, "ERROR"
      Me.CombFunction.SetFocus
      Exit Sub
    End If
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
      Me.MousePointer = 1
      If RetVal > -1 Then
        MsgBox " Error in Anrews-Mika diagram calculation " & CStr(RetVal), 64, "INFORMATION"
        JobBetaAMD = False
      Else
        JobBetaAMD = True
      End If
    End If
  End If
  Unload HFSU
End Sub

Private Sub CmdCancel_Click()
  Unload HFSU
End Sub

Private Sub ComboFunction2_Change()
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
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
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

Private Sub CombFunction_Change()
'********************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub CombFunction_Click()
'*******************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub CombFunction_LostFocus()
'***********************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Form_Load()
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  CombFunction.AddItem "Logistic"
  CombFunction.AddItem "Rosin Rammler"
  CombFunction.AddItem "Exponential sum"
  ComboFunction2.AddItem "PM PM (PM,CL)"
  ComboFunction2.AddItem "(PM PM PM),CL"
  ComboFunction2.AddItem "(PM,CL) (PM,CL) (PM,CL)"
  ComboFunction2.AddItem "PM, PM, PM"
  'Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "HFSU" Then
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
  Dim I As Integer
    OptDefault.Value = -1
    'Use default data from Lo and Herbst, Adv Mineral Process SME1986 p33
    Parameter(0).text = Str$(0.8)
    Parameter(1).text = Str$(0.185)
    Parameter(2).text = Str$(-0.312)
    Parameter(3).text = Str$(3.723)
    Parameter(4).text = Str$(0.748)
    Parameter(5).text = Str$(0)
    Parameter(6).text = Str$(0.72)
    Parameter(7).text = Str$(5)
    Parameter(8).text = Str$(6)
    Parameter(9).text = Str$(40)
    Parameter(10).text = Str$(74)
    Parameter(11).text = Str$(3)
    Parameter(12).text = Str$(0.8)
    Parameter(13).text = Str$(7.8)
    Parameter(14).text = Str$(2000)
    Parameter(15).text = Str$(50)
    Parameter(16).text = Str$(0.0137)
    Parameter(17).text = Str$(0.2123)
    Parameter(18).text = Str$(0.774)
    OptLibModel(0).Value = True
    OptPowerModel(3).Value = True
    ChkClassification.Value = 0
    CombFunction.Visible = False
    lblParameter(11).Visible = False
    lblParameter(12).Visible = False
    Parameter(11).Visible = False
    Parameter(12).Visible = False
    CombFunction.text = CombFunction.List(0)
    CombFunction.ListIndex = 0
    ComboFunction2.text = ComboFunction2.List(3)
    ComboFunction2.ListIndex = 3
    ComboFunction2.Visible = True
    ComboFunction2.Enabled = False
    LblClassificationFunction.Visible = False
    ChkMakeAMDiagram.Value = 0
    OptSize(0).Value = True
    
    For I = 7 To 10
       Parameter(I).Visible = True
       lblParameter(I).Visible = True
    Next I
    Parameter(13).Visible = True
    lblParameter(13).Visible = True
    Parameter(14).Visible = False
    lblParameter(14).Visible = False
    Label11.Visible = True
    Frame3.Visible = True
    lblParameter(15).Visible = False
    Parameter(15).Visible = False
       
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
    For I = 1 To 11
      Parameter(I - 1).text = CStr(NDATA.PARAM(I))
    Next I
    Parameter(13) = NDATA.PARAM(16)
    If NDATA.PARAM(12) = 0 Then OptLibModel(0).Value = True
    If NDATA.PARAM(12) = 1 Then OptLibModel(1).Value = True
    If NDATA.PARAM(12) = 2 Then OptLibModel(2).Value = True
    OptSize(0).Value = True
    
  If NDATA.PARAM(13) > 0 Then
    ChkClassification.Value = 1
    CombFunction.Visible = True
    lblParameter(11).Visible = True
    lblParameter(12).Visible = True
    Parameter(11).Visible = True
    Parameter(12).Visible = True
    CombFunction.ListIndex = NDATA.PARAM(13) - 1
    LblClassificationFunction.Visible = True
    ComboFunction2.text = ComboFunction2.List(NDATA.PARAM(20))
    ComboFunction2.ListIndex = NDATA.PARAM(20)
    ComboFunction2.Enabled = True
  Else
    ChkClassification.Value = 0
    CombFunction.Visible = False
    lblParameter(11).Visible = False
    lblParameter(12).Visible = False
    Parameter(11).Visible = False
    Parameter(12).Visible = False
    LblClassificationFunction.Visible = False
    ComboFunction2.text = ComboFunction2.List(3)
    ComboFunction2.ListIndex = 3
    ComboFunction2.Enabled = False
  End If
  Parameter(11).text = CStr(NDATA.PARAM(14))
  Parameter(12).text = CStr(NDATA.PARAM(15))
  
  OptPowerModel(NDATA.PARAM(17)).Value = True
  
  If NDATA.PARAM(17) < 1 Then
    For I = 7 To 10
      Parameter(I).Visible = False
      lblParameter(I).Visible = False
    Next I
    Parameter(13).Visible = False
    lblParameter(13).Visible = False
    Parameter(14).Visible = True
    lblParameter(14).Visible = True
    Label11.Visible = False
    Frame3.Visible = False
    lblParameter(15).Visible = False
    Parameter(15).Visible = False
  Else
    For I = 7 To 10
      Parameter(I).Visible = True
      lblParameter(I).Visible = True
    Next I
    Parameter(13).Visible = True
    lblParameter(13).Visible = True
    Parameter(14).Visible = False
    lblParameter(14).Visible = False
    Label11.Visible = True
    Frame3.Visible = True
    If NDATA.PARAM(17) = 1 Then
       lblParameter(15).Visible = True
       Parameter(15).Visible = True
    Else
       lblParameter(15).Visible = False
       Parameter(15).Visible = False
    End If
  End If
  Parameter(14).text = CStr(NDATA.PARAM(18))
  Parameter(15).text = CStr(NDATA.PARAM(19))
  Parameter(16).text = CStr(NDATA.PARAM(21))
  Parameter(17).text = CStr(NDATA.PARAM(22))
  Parameter(18).text = CStr(1# - NDATA.PARAM(21) - NDATA.PARAM(22))
  
  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "HFSU"
  NDATA.NOPAR = 22
  For I = 1 To 11
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If OptLibModel(0).Value Then NDATA.PARAM(12) = 0
  If OptLibModel(1).Value Then NDATA.PARAM(12) = 1
  If OptLibModel(2).Value Then NDATA.PARAM(12) = 2
  If OptSize(0).Value Then ScaleFactor = 1#
  If OptSize(1).Value Then ScaleFactor = 0.3048
  NDATA.PARAM(8) = NDATA.PARAM(8) * ScaleFactor
  NDATA.PARAM(9) = NDATA.PARAM(9) * ScaleFactor
  If ChkClassification.Value = 0 Then
    NDATA.PARAM(13) = 0
  Else
    NDATA.PARAM(13) = CombFunction.ListIndex + 1
  End If
  NDATA.PARAM(14) = Val(Parameter(11).text)
  NDATA.PARAM(15) = Val(Parameter(12).text)
  NDATA.PARAM(16) = Val(Parameter(13).text)
  
  If OptPowerModel(0).Value Then NDATA.PARAM(17) = 0
  If OptPowerModel(1).Value Then NDATA.PARAM(17) = 1
  If OptPowerModel(2).Value Then NDATA.PARAM(17) = 2
  If OptPowerModel(3).Value Then NDATA.PARAM(17) = 3
  
  NDATA.PARAM(18) = Val(Parameter(14).text)
  NDATA.PARAM(19) = Val(Parameter(15).text)
  NDATA.PARAM(20) = ComboFunction2.ListIndex
  NDATA.PARAM(21) = Val(Parameter(16).text)
  NDATA.PARAM(22) = Val(Parameter(17).text)
  
   
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
PrintForm
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

Private Sub OptPowerModel_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Power" Then
  If OptNew.Value = 0 Then OptNew.Value = -1
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub optSize_Click(Index As Integer)
'******************************************
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub
Private Sub optSize_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
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
    Repeat.TxtParameterName = lblParameter(Index).Caption
    Repeat.TxtValue(0) = Parameter(Index).text
    Repeat.TxtValue(1) = ""
    Repeat.TxtValue(2) = ""
    If Index < 11 Then
      IDIFF = 1
    ElseIf Index < 14 Then
      IDIFF = 3
    ElseIf Index < 16 Then
      IDIFF = 4
    Else
      IDIFF = 5
    End If
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

