VERSION 5.00
Begin VB.Form Fltk 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model FLTK"
   ClientHeight    =   5175
   ClientLeft      =   135
   ClientTop       =   1815
   ClientWidth     =   7800
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
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5175
   ScaleWidth      =   7800
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
      Index           =   20
      Left            =   2280
      TabIndex        =   55
      Top             =   4440
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   19
      Left            =   2280
      TabIndex        =   54
      Top             =   4200
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.TextBox Parameter 
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
      Index           =   5
      Left            =   2880
      TabIndex        =   53
      Top             =   1560
      Width           =   855
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
      Index           =   13
      Left            =   600
      TabIndex        =   52
      Top             =   3960
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
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
      Left            =   6720
      TabIndex        =   22
      Top             =   4080
      Width           =   960
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
      Index           =   18
      Left            =   2280
      TabIndex        =   16
      Top             =   3960
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   17
      Left            =   2280
      TabIndex        =   15
      Top             =   3720
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   16
      Left            =   2280
      TabIndex        =   14
      Top             =   3480
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   15
      Left            =   600
      TabIndex        =   13
      Top             =   4440
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   14
      Left            =   600
      TabIndex        =   12
      Top             =   4200
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   12
      Left            =   600
      TabIndex        =   11
      Top             =   3720
      Visible         =   0   'False
      Width           =   1095
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
      Index           =   11
      Left            =   600
      TabIndex        =   10
      Top             =   3480
      Visible         =   0   'False
      Width           =   1095
   End
   Begin VB.TextBox Parameter 
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
      Index           =   10
      Left            =   2880
      TabIndex        =   9
      Top             =   2760
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   9
      Left            =   2880
      TabIndex        =   8
      Top             =   2520
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   8
      Left            =   2880
      TabIndex        =   7
      Top             =   2280
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   7
      Left            =   2880
      TabIndex        =   6
      Top             =   2040
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   6
      Left            =   2880
      TabIndex        =   5
      Top             =   1800
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   4
      Left            =   2880
      TabIndex        =   4
      Top             =   1320
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   3
      Left            =   2880
      TabIndex        =   3
      Top             =   1080
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   2
      Left            =   2880
      TabIndex        =   2
      Top             =   840
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   1
      Left            =   2880
      TabIndex        =   1
      Top             =   600
      Width           =   855
   End
   Begin VB.TextBox Parameter 
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
      Index           =   0
      Left            =   2880
      TabIndex        =   0
      Top             =   360
      Width           =   855
   End
   Begin VB.OptionButton optCubft 
      BackColor       =   &H00C0C0C0&
      Caption         =   "ft^3"
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
      Left            =   4680
      TabIndex        =   18
      Top             =   600
      Value           =   -1  'True
      Width           =   855
   End
   Begin VB.OptionButton optCubmeter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "m^3"
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
      Left            =   3840
      TabIndex        =   17
      TabStop         =   0   'False
      Top             =   600
      Width           =   735
   End
   Begin VB.Frame Frame1 
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
      Height          =   1095
      Left            =   6000
      TabIndex        =   36
      Top             =   720
      Width           =   1695
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
         TabIndex        =   21
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
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
         TabIndex        =   20
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
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
         TabIndex        =   19
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
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
      Left            =   5520
      TabIndex        =   23
      Top             =   4080
      Width           =   960
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k10"
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
      Index           =   20
      Left            =   1920
      TabIndex        =   57
      Top             =   4440
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k9"
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
      Index           =   19
      Left            =   1920
      TabIndex        =   56
      Top             =   4200
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   46
      Top             =   0
      Width           =   7680
   End
   Begin VB.Label Label17 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
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
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3840
      TabIndex        =   51
      Top             =   2760
      Width           =   720
   End
   Begin VB.Label Label16 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
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
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3840
      TabIndex        =   50
      Top             =   2520
      Width           =   720
   End
   Begin VB.Label Label12 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "sec"
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
      Left            =   3840
      TabIndex        =   34
      Top             =   1800
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Largest floatable particle"
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
      Index           =   10
      Left            =   120
      TabIndex        =   49
      Top             =   2760
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Particle size at maximum recovery"
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
      Index           =   9
      Left            =   120
      TabIndex        =   48
      Top             =   2520
      Width           =   2655
   End
   Begin VB.Label LblSpecificConstants 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specific flotation rate constants"
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
      TabIndex        =   37
      Top             =   3240
      Width           =   2895
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k1"
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
      Index           =   11
      Left            =   240
      TabIndex        =   38
      Top             =   3480
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k2"
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
      Index           =   12
      Left            =   240
      TabIndex        =   39
      Top             =   3720
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k3"
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
      Index           =   13
      Left            =   240
      TabIndex        =   40
      Top             =   3960
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k4"
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
      Index           =   14
      Left            =   240
      TabIndex        =   41
      Top             =   4200
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k5"
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
      Index           =   15
      Left            =   240
      TabIndex        =   42
      Top             =   4440
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k6"
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
      Index           =   16
      Left            =   1920
      TabIndex        =   43
      Top             =   3480
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k7"
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
      Index           =   17
      Left            =   1920
      TabIndex        =   44
      Top             =   3720
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "k8"
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
      Index           =   18
      Left            =   1920
      TabIndex        =   45
      Top             =   3960
      Visible         =   0   'False
      Width           =   360
   End
   Begin VB.Label Label10 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "sec"
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
      Left            =   3840
      TabIndex        =   32
      Top             =   1560
      Width           =   360
   End
   Begin VB.Label Label8 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
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
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   3840
      TabIndex        =   30
      Top             =   1320
      Width           =   360
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "1/min"
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
      Left            =   3840
      TabIndex        =   27
      Top             =   840
      Width           =   600
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of banks in parallel"
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
      Index           =   8
      Left            =   120
      TabIndex        =   47
      Top             =   2280
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Percent solids in the concentrate"
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
      Index           =   7
      Left            =   120
      TabIndex        =   35
      Top             =   2040
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Estimated cell residence time"
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
      Index           =   6
      Left            =   120
      TabIndex        =   33
      Top             =   1800
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Bubble residence time"
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
      Index           =   5
      Left            =   120
      TabIndex        =   31
      Top             =   1560
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Average bubble size"
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
      Index           =   4
      Left            =   120
      TabIndex        =   29
      Top             =   1320
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Froth transmission coefficient"
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
      Index           =   3
      Left            =   120
      TabIndex        =   28
      Top             =   1080
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Aeration rate per unit volume of cell"
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
      Index           =   2
      Left            =   120
      TabIndex        =   26
      Top             =   840
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Volume of each cell"
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
      Left            =   120
      TabIndex        =   25
      Top             =   600
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of cells in this bank"
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
      Left            =   120
      TabIndex        =   24
      Top             =   360
      Width           =   2655
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
Attribute VB_Name = "Fltk"
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
  Call MakeNew(CURRMODELDATA(ID))
  Unload Fltk
End Sub

Private Sub CmdCancel_Click()
  Unload Fltk
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
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "FLTK" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
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
'************************
   Dim I As Integer
   Dim J2 As Integer
    J2 = Match(UnitMods.Unit, 6)
    If J2 > 20 Then
      MsgBox "The number of cells in the bank could not be determined from the flowsheet." & Chr(13) & Chr(13) & "Edit the flowsheet to get this information if necessary.", 64, "INFORMATION"
      Parameter(0).text = CStr(1)
    Else
      Parameter(0).text = CStr(NOCELL(J2))
    End If
    Parameter(1).text = Str$(3)
    Parameter(2).text = Str$(1)
    Parameter(3).text = Str$(1)
    Parameter(4).text = Str$(2)
    Parameter(5).text = Str$(10)
    Parameter(6).text = Str$(60)
    Parameter(7).text = Str$(40)
    Parameter(8).text = Str$(1)
    Parameter(9).text = Str$(50)
    Parameter(10).text = Str$(1000)
    optCubmeter.Value = -1
    For I = 1 To UnitMods.NKVALS
      lblParameter(10 + I).Visible = -1
      Parameter(10 + I).Visible = -1
      Parameter(10 + I).text = Str$(KK(I))
    Next I
    For I = UnitMods.NKVALS + 1 To 10
      lblParameter(10 + I).Visible = 0
      Parameter(I + 10).Visible = 0
    Next I
End Sub

Private Sub LoadNew(NDATA As ModelData)
  Dim I As Integer
    For I = 1 To 11
      Parameter(I - 1).text = Str$(NDATA.PARAM(I))
    Next I
    Parameter(4).text = Str$(NDATA.PARAM(5) * 1000)
    For I = 12 To NDATA.NOPAR
      lblParameter(I - 1).Visible = -1
      Parameter(I - 1).Visible = -1
      Parameter(I - 1).text = Str$(NDATA.PARAM(I))
    Next I
    For I = NDATA.NOPAR + 1 To 21
      lblParameter(I - 1).Visible = 0
      Parameter(I - 1).Visible = 0
    Next I
    optCubmeter.Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.MODEL = "FLTK"
  NDATA.NOPAR = 11 + UnitMods.NKVALS
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the size to meters
  If optCubft.Value Then NDATA.PARAM(2) = NDATA.PARAM(2) * 0.028317
  'Convert the bubble size to meters
  NDATA.PARAM(5) = NDATA.PARAM(5) * 0.001
End Sub

Private Sub MnuAccept_Click()
   Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'**************************
'Print the form
PrintForm
End Sub

Private Sub optCubft_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optCubft_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optCubmeter_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optCubmeter_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
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
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
'****************************************
  Dim I As Integer
  If Index = 0 Then Instruction.Caption = "Specify the number of individual cells in this bank"
  If Index = 1 Then Instruction.Caption = "Specify the volume of pulp in each cell"
  If Index = 2 Then Instruction.Caption = "Specify the solids holdup in the pulp"
  If Index = 8 Then Instruction.Caption = "Specify the number of banks of cells in parallel"
  For I = 1 To UnitMods.NKVALS
    If Index = 8 + I Then Instruction.Caption = "Specify the specific rate constant for S-class " + Str$(I)
  Next I
  Instruction.Visible = -1
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < UnitMods.NKVALS + 8 And KeyAscii = 13 Then
    Parameter(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  Instruction.Visible = 0
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
Dim I As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
    For I = 1 To 5
      If UnitID(I) = ID Then
        If ParameterNumber(I) = Index + 1 Then
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
      ParameterNumber(Repeat.Level) = Index + 1
    End If
  End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub

