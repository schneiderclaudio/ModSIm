VERSION 5.00
Begin VB.Form SAGP 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   5685
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   8640
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
   Icon            =   "SAGP.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5685
   ScaleWidth      =   8640
   Begin VB.CheckBox FixedFill 
      Caption         =   "Fix"
      Height          =   210
      Left            =   8400
      TabIndex        =   64
      Top             =   2880
      Value           =   1  'Checked
      Width           =   255
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   26
      Left            =   7200
      TabIndex        =   62
      Text            =   "Text7"
      Top             =   4560
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   25
      Left            =   7200
      TabIndex        =   60
      Text            =   "Text7"
      Top             =   4320
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   24
      Left            =   7200
      TabIndex        =   58
      Top             =   4080
      Width           =   855
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for fracture energy"
      Height          =   1455
      Left            =   240
      TabIndex        =   51
      Top             =   3240
      Width           =   3135
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   21
         Left            =   2400
         TabIndex        =   54
         Top             =   360
         Width           =   615
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   22
         Left            =   2400
         TabIndex        =   53
         Top             =   720
         Width           =   615
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   23
         Left            =   2400
         TabIndex        =   52
         Top             =   1080
         Width           =   615
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Particle fracture energy for large particles  J/kg "
         Height          =   495
         Index           =   21
         Left            =   240
         TabIndex        =   57
         Top             =   240
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Reference size d0  mm "
         Height          =   255
         Index           =   22
         Left            =   240
         TabIndex        =   56
         Top             =   720
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Exponent phi "
         Height          =   255
         Index           =   23
         Left            =   240
         TabIndex        =   55
         Top             =   1080
         Width           =   2055
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   18
      Left            =   7200
      TabIndex        =   50
      Text            =   "Text7"
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   17
      Left            =   7200
      TabIndex        =   48
      Text            =   "Text7"
      Top             =   3120
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   14
      Left            =   7200
      TabIndex        =   45
      Text            =   "Text7"
      Top             =   2400
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   13
      Left            =   7200
      TabIndex        =   43
      Text            =   "Text7"
      Top             =   2160
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   6000
      TabIndex        =   41
      Text            =   "Text7"
      Top             =   1320
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   6000
      TabIndex        =   36
      Text            =   "Text7"
      Top             =   600
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   6000
      TabIndex        =   35
      Text            =   "Text7"
      Top             =   840
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   6000
      TabIndex        =   34
      Text            =   "Text7"
      Top             =   1080
      Width           =   855
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   7080
      TabIndex        =   0
      Top             =   4920
      Width           =   960
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   5880
      TabIndex        =   1
      Top             =   4920
      Width           =   960
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   7200
      TabIndex        =   9
      Text            =   "Text7"
      Top             =   3840
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   19
      Left            =   7200
      TabIndex        =   26
      Text            =   "Text7"
      Top             =   3600
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   7200
      TabIndex        =   29
      Text            =   "Text7"
      Top             =   2880
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   12
      Left            =   7200
      TabIndex        =   31
      Text            =   "Text7"
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   15
      Left            =   7200
      TabIndex        =   6
      Text            =   "Text7"
      Top             =   2640
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   3000
      TabIndex        =   8
      Text            =   "Text7"
      Top             =   2880
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   6
      Left            =   3000
      TabIndex        =   7
      Text            =   "Text7"
      Top             =   2640
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   5
      Left            =   2640
      TabIndex        =   25
      Text            =   "Text7"
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   2640
      TabIndex        =   24
      Text            =   "Text7"
      Top             =   1680
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   2640
      TabIndex        =   5
      Text            =   "Text2"
      Top             =   1080
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   2640
      TabIndex        =   4
      Text            =   "Text1"
      Top             =   840
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   2640
      TabIndex        =   2
      Text            =   "Text5"
      Top             =   360
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   2640
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   600
      Width           =   855
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7080
      TabIndex        =   18
      Top             =   120
      Width           =   1455
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
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
         TabIndex        =   12
         Tag             =   "Default"
         Top             =   720
         Width           =   840
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
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
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1200
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
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
         TabIndex        =   10
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fix"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   27
      Left            =   8160
      TabIndex        =   65
      Top             =   2880
      Width           =   255
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Trommel mesh size  mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   26
      Left            =   5280
      TabIndex        =   63
      Top             =   4560
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Pebble port/grate open area ratio"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   25
      Left            =   4680
      TabIndex        =   61
      Top             =   4320
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Pebble port openning mm"
      Height          =   255
      Index           =   24
      Left            =   5160
      TabIndex        =   59
      Top             =   4080
      Width           =   1935
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball size   mm"
      Height          =   240
      Index           =   18
      Left            =   5280
      TabIndex        =   49
      Top             =   3360
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Trunnion diameter  m"
      Height          =   240
      Index           =   15
      Left            =   5280
      TabIndex        =   47
      Top             =   2640
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball volume  %"
      Height          =   240
      Index           =   17
      Left            =   5280
      TabIndex        =   46
      Top             =   3120
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Belly length  m"
      Height          =   240
      Index           =   14
      Left            =   5280
      TabIndex        =   44
      Top             =   2400
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Center line length  m"
      Height          =   240
      Index           =   13
      Left            =   5280
      TabIndex        =   42
      Top             =   2160
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball diameter  mm"
      Height          =   240
      Index           =   11
      Left            =   4080
      TabIndex        =   40
      Top             =   1320
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill diameter  m"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   4080
      TabIndex        =   39
      Top             =   600
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Load volume  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   4080
      TabIndex        =   38
      Top             =   840
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill speed  %critical"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   4080
      TabIndex        =   37
      Top             =   1080
      Width           =   1815
   End
   Begin VB.Label Label6 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill dimensions:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Left            =   4080
      TabIndex        =   33
      Top             =   360
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Grate opening   mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   20
      Left            =   5280
      TabIndex        =   21
      Top             =   3840
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill speed  %critical"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   19
      Left            =   5280
      TabIndex        =   27
      Top             =   3600
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Initial load volume  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   16
      Left            =   5280
      TabIndex        =   28
      Top             =   2880
      Width           =   1815
   End
   Begin VB.Label Label20 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Simulated mill dimensions (inside liners):"
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
      Height          =   240
      Left            =   5280
      TabIndex        =   32
      Top             =   1680
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill diameter  m"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   12
      Left            =   5280
      TabIndex        =   30
      Top             =   1920
      Width           =   1815
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "T10 parameter b  tonnes/kW hr"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   120
      TabIndex        =   23
      Top             =   1920
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "T10 parameter A  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   120
      TabIndex        =   22
      Top             =   1680
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Largest size for attrition products  mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   120
      TabIndex        =   19
      Top             =   2640
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Attrition parameter Ta"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   120
      TabIndex        =   20
      Top             =   2880
      Width           =   2775
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for selection function measured in test mill:"
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
      Height          =   240
      Left            =   0
      TabIndex        =   13
      Top             =   120
      Width           =   4935
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Lambda"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   120
      TabIndex        =   17
      Top             =   1080
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Selection function at 1mm    1/min"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   120
      TabIndex        =   15
      Top             =   360
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   120
      TabIndex        =   14
      Top             =   600
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mu in mm"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   120
      TabIndex        =   16
      Top             =   840
      Width           =   2415
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
Attribute VB_Name = "SAGP"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
'Check if Parameter 25 is > 0 and < 1
Dim Vm As Single, BellyLength As Single, CenterLineLength As Single, Dt As Single, Dm As Single
Dim WS As Single, Pi As Single, VFill As Single, MaxFill As Single

'Programming in VB is not so bad
    BellyLength = Val(Parameter(14).text)
    CenterLineLength = Val(Parameter(13).text)
    Dt = Val(Parameter(15).text)
    Dm = Val(Parameter(12).text)
    
    If BellyLength > 0# And Dm > 0# Then
       Pi = 3.1415927
       WS = CenterLineLength / BellyLength
       Vm = 0.25 * Pi * Dm * Dm * BellyLength
       Vm = Vm * (1# + 0.6667 * (WS - 1) * (1 - (Dt / Dm) ^ 3)) / (1 - Dt / Dm)
       VFill = (Vm - CenterLineLength * Dt * Dt * Pi * 0.25) / 2#
       MaxFill = 100# * VFill / Vm
    Else
        MaxFill = 0#
    End If
    
   If Val(Parameter(25).text) < 0 Then
       MsgBox "Pebble Port/Grate Open Area ratio must be > 0", vbExclamation, "Parameter handling"
   Else
       If Val(Parameter(24).text) < Val(Parameter(20).text) Then
           MsgBox "Grate size must be < than Pebble Port size", vbExclamation, "Parameter handling"
       Else
          If (Val(Parameter(16).text) > MaxFill Or Val(Parameter(16).text) <= Val(Parameter(17).text)) Then
              If (Val(Parameter(16).text) > MaxFill) Then MsgBox "Initial load volume exceeds maximum for this mill: " + CStr(MaxFill) + "%", vbExclamation, "Parameter handling"
              If (Val(Parameter(16).text) <= Val(Parameter(17).text)) Then MsgBox "Initial load volume cannot be <= Ball load", vbExclamation, "Parameter handling"
          Else
              Call MakeNew(CURRMODELDATA(ID))
              Unload SAGP
          End If
       End If
  End If
End Sub

Private Sub CmdCancel_Click()
  Unload SAGP
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
  On Error GoTo ErrHandler
  Caption = Caption & " " & UnitMods.Model & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  If UnitMods.Model = "SAGL" Then
    lblParameter(26).Visible = True
    Parameter(26).Visible = True
  Else
    lblParameter(26).Visible = False
    Parameter(26).Visible = False
  End If
  'Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> UnitMods.Model Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  Exit Sub

ErrHandler:
  Exit Sub
End Sub

Private Sub LoadDefault()
    OptDefault.Value = -1
    'Use default data for Austin functions from APCOM 87 p107
    'Determined using steel medium
    Parameter(0).text = CStr(1#)
    Parameter(1).text = CStr(0.95)
    Parameter(2).text = CStr(1.65)
    Parameter(3).text = CStr(3.3)
    Parameter(4).text = CStr(50)
    Parameter(5).text = CStr(1#)
    Parameter(6).text = CStr(2)
    Parameter(7).text = CStr(1)
    Parameter(8).text = CStr(0.2)
    Parameter(9).text = CStr(20)
    Parameter(10).text = CStr(75)
    Parameter(11).text = CStr(25)
    Parameter(12).text = CStr(8)
    Parameter(13).text = CStr(6)
    Parameter(14).text = CStr(4)
    Parameter(15).text = CStr(2)
    Parameter(16).text = CStr(35)
    Parameter(17).text = CStr(10)
    Parameter(18).text = CStr(50)
    Parameter(19).text = CStr(72)
    Parameter(20).text = CStr(40)
    Parameter(21).text = CStr(96)
    Parameter(22).text = CStr(1.17)
    Parameter(23).text = CStr(1.26)
    Parameter(24).text = CStr(100)
    Parameter(25).text = CStr(0.1)
    Parameter(26).text = CStr(50)
    FixedFill.Value = 1
  End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To NDATA.NOPAR - 1
    Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  FixedFill.Value = NDATA.PARAM(NDATA.NOPAR)
  
  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  If UnitMods.Model = "SAGL" Then
    NDATA.Model = "SAGL"
    NDATA.NOPAR = 28
  Else
    NDATA.Model = "SAGP"
    NDATA.NOPAR = 27
  End If
  For I = 1 To NDATA.NOPAR - 1
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  NDATA.PARAM(NDATA.NOPAR) = FixedFill.Value
  
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

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
29  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
    'Check if this parameter is already selected and display if necessary.
    For I = 1 To 5
      If UnitID(I) = ID Then
        If ParameterNumber(I) = Index + IDIFF Then
          Repeat.LstLevel.AddItem CStr(I), 0
          Repeat.LstLevel.ListIndex = 0
          Repeat.LstLevel.Enabled = True
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


