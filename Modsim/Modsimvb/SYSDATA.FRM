VERSION 5.00
Begin VB.Form Sysdata 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Specify system and plant data"
   ClientHeight    =   6225
   ClientLeft      =   195
   ClientTop       =   1320
   ClientWidth     =   9045
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
   ScaleHeight     =   6225
   ScaleWidth      =   9045
   Begin VB.CommandButton CmdConvergence 
      Appearance      =   0  'Flat
      Caption         =   "Set convergence properties"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   5280
      TabIndex        =   44
      ToolTipText     =   "Set parameters that control the converge of the simulation calculation"
      Top             =   5160
      Width           =   1695
   End
   Begin VB.ListBox StreamList 
      Appearance      =   0  'Flat
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
      Height          =   870
      Left            =   5280
      TabIndex        =   26
      Top             =   720
      Width           =   3615
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "ORE CHARACTERISTICS"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   5055
      Left            =   120
      TabIndex        =   31
      Top             =   480
      Width           =   5055
      Begin VB.Frame Frame3 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Select type of mineral to be processed"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   975
         Left            =   120
         TabIndex        =   43
         Top             =   240
         Width           =   3855
         Begin VB.CheckBox ChkProximate 
            BackColor       =   &H00C0C0C0&
            Caption         =   "Proximate analysis"
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
            Left            =   2040
            TabIndex        =   2
            Top             =   600
            Visible         =   0   'False
            Width           =   1695
         End
         Begin VB.OptionButton OptCoalProcPlant 
            BackColor       =   &H00C0C0C0&
            Caption         =   "Coal processing plant"
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
            TabIndex        =   1
            Top             =   600
            Width           =   1935
         End
         Begin VB.OptionButton OptMinProcPlant 
            BackColor       =   &H00C0C0C0&
            Caption         =   "Conventional minerals"
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
            TabIndex        =   0
            Top             =   240
            Value           =   -1  'True
            Width           =   2295
         End
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   6
         Left            =   2040
         TabIndex        =   19
         Top             =   3480
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   5
         Left            =   2040
         TabIndex        =   17
         Top             =   3240
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   4
         Left            =   2040
         TabIndex        =   15
         Top             =   3000
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   3
         Left            =   2040
         TabIndex        =   13
         Top             =   2760
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   2
         Left            =   2040
         TabIndex        =   11
         Top             =   2520
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   1
         Left            =   2040
         TabIndex        =   9
         Top             =   2280
         Visible         =   0   'False
         Width           =   650
      End
      Begin VB.TextBox TxtSG 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   210
         Index           =   0
         Left            =   2040
         TabIndex        =   7
         Top             =   2040
         Width           =   650
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   6
         Left            =   120
         TabIndex        =   18
         Top             =   3480
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   5
         Left            =   120
         TabIndex        =   16
         Top             =   3240
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   4
         Left            =   120
         TabIndex        =   14
         Top             =   3000
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   3
         Left            =   120
         TabIndex        =   12
         Top             =   2760
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   2
         Left            =   120
         TabIndex        =   10
         Top             =   2520
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   1
         Left            =   120
         TabIndex        =   8
         Top             =   2280
         Visible         =   0   'False
         Width           =   1800
      End
      Begin VB.Frame Frame2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Specify sp.gr by"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   855
         Left            =   2880
         TabIndex        =   41
         Top             =   1320
         Width           =   1575
         Begin VB.OptionButton OptSGSpec 
            BackColor       =   &H00C0C0C0&
            Caption         =   "Particle type"
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
            Left            =   120
            TabIndex        =   5
            Tag             =   "OptSG"
            Top             =   480
            Width           =   1215
         End
         Begin VB.OptionButton OptSGSpec 
            BackColor       =   &H00C0C0C0&
            Caption         =   "Mineral"
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
            TabIndex        =   4
            Tag             =   "OptSG"
            Top             =   240
            Value           =   -1  'True
            Width           =   855
         End
      End
      Begin VB.TextBox NSCM 
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
         Left            =   4680
         TabIndex        =   23
         Tag             =   "NSCM"
         Text            =   "1"
         Top             =   4200
         Width           =   255
      End
      Begin VB.TextBox D1 
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
         Left            =   4150
         TabIndex        =   21
         Text            =   "0.005"
         ToolTipText     =   "Specify the largest size for this simulation"
         Top             =   3840
         Width           =   615
      End
      Begin VB.TextBox NGCM 
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
         Left            =   2160
         TabIndex        =   22
         Text            =   "1"
         Top             =   4200
         Width           =   255
      End
      Begin VB.TextBox NDCM 
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
         Left            =   2160
         TabIndex        =   20
         Text            =   "25"
         Top             =   3840
         Width           =   255
      End
      Begin VB.CommandButton CmdGradeClasses 
         Appearance      =   0  'Flat
         Caption         =   "Set up grade classes"
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
         Height          =   375
         Left            =   120
         TabIndex        =   24
         Top             =   4560
         Width           =   1695
      End
      Begin VB.CommandButton CmdSClasses 
         Appearance      =   0  'Flat
         Caption         =   "Set up S-classes"
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
         Height          =   375
         Left            =   2640
         TabIndex        =   25
         Top             =   4560
         Width           =   1695
      End
      Begin VB.TextBox MinName 
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
         Height          =   210
         Index           =   0
         Left            =   120
         TabIndex        =   6
         Top             =   2040
         Width           =   1800
      End
      Begin VB.TextBox NoMin 
         Alignment       =   2  'Center
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
         Left            =   1560
         TabIndex        =   3
         Tag             =   "NumberOfMinerals"
         Top             =   1440
         Width           =   375
      End
      Begin VB.Label Label2 
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
         Height          =   255
         Left            =   4800
         TabIndex        =   45
         Top             =   3840
         Width           =   135
      End
      Begin VB.Label LblSGSpec 
         Alignment       =   2  'Center
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mineral specific gravities"
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
         Left            =   2040
         TabIndex        =   42
         Top             =   1400
         Width           =   735
      End
      Begin VB.Label Label6 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Largest particle size"
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
         Left            =   2640
         TabIndex        =   37
         Top             =   3840
         Width           =   1575
      End
      Begin VB.Label Label9 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of S-classes"
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
         Left            =   3000
         TabIndex        =   40
         Top             =   4200
         Width           =   1575
      End
      Begin VB.Label LblMinNames 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mineral names"
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
         TabIndex        =   36
         Top             =   1800
         Width           =   1695
      End
      Begin VB.Label Lblngcm 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of grade classes"
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
         Height          =   240
         Left            =   120
         TabIndex        =   39
         Top             =   4200
         Width           =   1935
      End
      Begin VB.Label Label7 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of size classes"
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
         TabIndex        =   38
         Top             =   3840
         Width           =   1815
      End
      Begin VB.Label Label1 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of minerals"
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
         TabIndex        =   32
         Top             =   1440
         Width           =   1455
      End
   End
   Begin VB.CommandButton CmdReturn 
      Appearance      =   0  'Flat
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
      Height          =   495
      Left            =   8160
      TabIndex        =   29
      Top             =   5160
      Width           =   720
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
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
      Height          =   495
      Left            =   7200
      TabIndex        =   30
      Top             =   5160
      Width           =   735
   End
   Begin VB.ListBox OStreamList 
      Appearance      =   0  'Flat
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
      Height          =   1575
      IntegralHeight  =   0   'False
      Left            =   5280
      TabIndex        =   28
      Top             =   3480
      Width           =   3615
   End
   Begin VB.ListBox WStreamList 
      Appearance      =   0  'Flat
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
      Height          =   1290
      Left            =   5280
      TabIndex        =   27
      Top             =   1920
      Width           =   3615
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
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
      TabIndex        =   46
      Top             =   120
      Visible         =   0   'False
      Width           =   8775
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Feed streams"
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
      Left            =   5280
      TabIndex        =   33
      Top             =   480
      Width           =   1815
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Internal and product streams"
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
      Left            =   5280
      TabIndex        =   35
      Top             =   3240
      Width           =   2775
   End
   Begin VB.Label Label4 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Water addition streams"
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
      Left            =   5280
      TabIndex        =   34
      Top             =   1680
      Width           =   2175
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
Attribute VB_Name = "Sysdata"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public LibMatrix_check As Integer
Public MAX_O_DATA_SETS As Integer, MAX_FEED_DATA_SETS As Integer
Public MAX_WATER_DATA_SETS As Integer, MAX_LIB_DATA_SETS As Integer
Public N_WATER_DATA_STREAMS As Integer, N_FEED_DATA_STREAMS As Integer
Public N_O_DATA_STREAMS As Integer, N_LIB_DATA_STREAMS As Integer
Public FORM_CHANGE_FLAG As Boolean
Public NSTR As Integer, ID As Integer, IND As Integer
Public WUNIT As Integer

  Private WSTRING As String
  Public Nstream As Integer
  Private Stream(100) As Integer
  Private From_unit(100) As Integer, To_unit(100) As Integer
  Private STREAM_TYPE(100) As String * 1
  Private JOBNAMEVAR As String
  Private Flags As String * 6
  Private loadingForm As Boolean
  
Private Sub ChkProximate_Click()
'*******************************
  Dim I As Integer
  If ChkProximate.Value = 1 Then
    NoMin.text = CStr(4)
    For I = 0 To 3
      MinName(I).Visible = True
    Next I
    MinName(0).text = "Ash "
    MinName(1).text = "Fixed carbon"
    MinName(2).text = "Volatiles"
    MinName(3).text = "Moisture"
    For I = 4 To 6
      MinName(I).Visible = False
    Next I
    For I = 0 To 6
      Me.TxtSG(I).Visible = False
    Next I
    Me.LblSGSpec.Visible = False
    Me.OptSGSpec(0).Enabled = False
  Else
    NoMin.text = CStr(2)
    For I = 0 To 1
      MinName(I).Visible = True
    Next I
    MinName(0).text = "Ash "
    MinName(1).text = "Combustibles"
    For I = 2 To 6
      MinName(I).Visible = False
    Next
    Me.OptSGSpec(0).Enabled = True
  End If
 End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload Sysdata
End Sub

Private Sub CmdConvergence_Click()
'*********************************
  Converge.Top = DisplayOffsetY + 200
  Converge.Left = DisplayOffsetX + 200
  Converge.Show 1
End Sub

Private Sub CmdSClasses_Click()
  S_CLASS.Top = DisplayOffsetY
  S_CLASS.Left = DisplayOffsetX
  S_CLASS.Show 1
End Sub

Private Sub CmdGradeClasses_Click()
'**********************************
  Dim N As Integer
  Dim resp As Integer
  If Val(NGCM.text) < Val(NoMin.text) Then
    MsgBox "Number of grade clases must be >= number of minerals", 0, "ERROR"
    NGCM.Enabled = True
    NGCM.SetFocus
    Exit Sub
  End If
  'Check that mineral names and SGs have been specified
  If Val(NoMin.text) <= 0 Or Val(NoMin.text) > 7 Then
    resp = MsgBox("Invalid number of minerals", 0, "ERROR")
    NoMin.SetFocus
    Exit Sub
  End If
  For N = 1 To CURRENT.Nomins
    If MinName(N - 1).text = "" Then
      resp = MsgBox("Mineral name is not specified", 0, "ERROR")
      MinName(N - 1).SetFocus
      Exit Sub
    End If
    If OptSGSpec(0).Value = True Then
      If Val(TxtSG(N - 1).text) <= 0 Or Val(TxtSG(N - 1).text) > 20 Then
        resp = MsgBox("Invalid specific gravity has been specified", 0, "ERROR")
        TxtSG(N - 1).SetFocus
        Exit Sub
      End If
    End If
  Next N
  G_class.Top = DisplayOffsetY + 200
  G_class.Left = DisplayOffsetX + 200
  G_class.Show 1
End Sub

Public Sub CmdReturn_Click()
'****************************
  Dim FRACT(MaximumSizeClasses) As Single
  Dim OUTFORM As String * 10
  Dim NL As String * 2
  Dim Fraction As Single
  Dim Cum As Single
  Dim P As Single, P1 As Single, P2 As Single, PM As Single, PSIZE As Single
  Dim hit As Integer
  Dim I As Integer, N As Integer, M As Integer, J As Integer, K As Integer
  Dim I1 As Integer, I2 As Integer
  Dim response As Integer
  Dim RetVal As Long
 

  'Check that class dimensions are within bounds
  If Val(NDCM.text) > MaximumSizeClasses Then
    MsgBox "The maximum number of size classes allowed is " & Format(MaximumSizeClasses, "#00"), 16, "ERROR"
    NDCM.SetFocus
    Exit Sub
  End If
  If Val(NGCM.text) > MaximumGClasses Then
    MsgBox "The maximum number of grade classes allowed is " & Format(MaximumGClasses, "#00"), 16, "ERROR"
    NGCM.SetFocus
    Exit Sub
  End If
  If Val(NSCM.text) > MaximumSClasses Then
    MsgBox "The maximum number of S classes allowed is " & Format(MaximumSClasses, "#00"), 16, "ERROR"
    NSCM.SetFocus
    Exit Sub
  End If
  

  'Check for changes on the form
  If FORM_CHANGE_FLAG Then
    Call MakeNew
    FORM_CHANGE_FLAG = False
  End If
   
  'Check the validity of the data
  If Val(NoMin.text) < 1 Then
    MsgBox "Specify the number of minerals", 0, "ERROR"
    NoMin.SetFocus
    Exit Sub
  End If
  If Val(NDCM.text) < 1 Then
    MsgBox "Specify the number of size clases", 0, "ERROR"
    NDCM.SetFocus
    Exit Sub
  End If
  If Val(NSCM.text) < 1 Then
    MsgBox "Specify the number of S-clases", 0, "ERROR"
    NSCM.SetFocus
    Exit Sub
  End If
  If Val(D1.text) <= 0 Then
    WSTRING = "The maximum particle size has not been specified"
    MsgBox WSTRING, 0, "ERROR"
    D1.SetFocus
    Exit Sub
  End If
  If Val(NGCM.text) < Val(NoMin.text) And OptMinProcPlant Then
    MsgBox "Number of grade clases must be >= number of minerals", 0, "ERROR"
    NGCM.Enabled = True
    NGCM.SetFocus
    Exit Sub
  End If
  If Me.OptSGSpec(0).Value Then
    'Check that the Sgs are within range
    For I = 1 To Val(NoMin.text)
      If Val(TxtSG(I - 1).text) > 500 Then
        MsgBox "Mineral density has been specified when specific gravity is required", vbExclamation, "ERROR"
        TxtSG(I - 1).SetFocus
        Exit Sub
      ElseIf Val(TxtSG(I - 1).text) <= 0 Then
        MsgBox "Specific gravity of all minerals must be specified", vbExclamation, "ERROR"
        TxtSG(I - 1).SetFocus
        Exit Sub
      End If
    Next I
  End If
  If MODSIM.JOBNAME = "" Then
    MODSIM.JOBNAME = InputBox("A jobname must be specified to store data", "WARNING")
    Exit Sub
  End If
  
  'Change the mouse pointer to hour glass for the duration of this procedure
  'Me.MousePointer = 11

  'Open a file to set up the Systems and Plant data components of DATT.DAT
  'This file will also be used to re-enter this module.
  Open RTrim(JobPath) + "CURRDATA.SYD" For Output As #34
  'Open a file to receive the S-class distributions for use with the liberation model
  Open RTrim(JobPath) + "S_CLASS.OUT" For Output As #35
  CURRENT.NDC = Val(NDCM.text)
  CURRENT.NGC = Val(NGCM.text)
  CURRENT.NSC = Val(NSCM.text)
  CURRENT.Nomins = Val(NoMin.text)
  NL = Chr$(13) + Chr$(10)
  
  'First the system data
  Print #34, "SYSTEM DATA"
  Print #34, MODSIM.JOBNAME

  Print #34, "NDGS   1";
  Print #34, Format(CURRENT.NDC, " 000");
  Print #34, Format(CURRENT.NGC, " 000");
  Print #34, Format(CURRENT.NSC, " 000")
  
  If OptCoalProcPlant.Value = True Then
    Print #34, "COAL";
    If ChkProximate.Value = 1 Then
      Print #34, "  -1"
    Else
      Print #34, "   0"
    End If
  End If
  Print #34, "MINS";
  Print #34, Format(Val(NoMin.text), " 000")
  For N = 1 To CURRENT.Nomins
    CURRENT.MineralNames(N) = Mid$(MinName(N - 1), 1, 16)
    Print #34, CURRENT.MineralNames(N)
    If OptSGSpec(0).Value = True Then CURRENT.MineralSG(N) = Val(TxtSG(N - 1).text)
  Next N
  response% = 0
  Print #34, "SIZE"
  CURRENT.size(1) = Val(D1.text)
  For I = 2 To CURRENT.NDC
    CURRENT.size(I) = CURRENT.size(I - 1) / Sqr(2)
  Next I
  Call PRNT_8G104(CURRENT.size(), 1, CURRENT.NDC, 34)
  
  'Check that all sp. grs are specified
  If OptSGSpec(0).Value Or G_class.OptVol Then
    For I = 1 To CURRENT.Nomins
      If CURRENT.MineralSG(I) <= 0 Then
        MsgBox "All mineral specific gravities must be specified", vbOKOnly, "ERROR"
        TxtSG(I - 1).Visible = True
        TxtSG(I - 1).SetFocus
        Close #34
        Close #35
        Me.MousePointer = 1
        Exit Sub
      End If
    Next I
  End If
  If CURRENT.MineralSG(1) > 0# Then
    Print #34, "SPGR"
    Call PRNT_8G104(CURRENT.MineralSG(), 1, CURRENT.Nomins, 34)
  End If
  Print #34, CURRENT.MassVol
  For N = 1 To CURRENT.NGC
    For M = 1 To CURRENT.Nomins - 1
      Print #34, Format$(CURRENT.GRDMV(N, M), "#.0000E+0") + " ";
    Next M
    Print #34, Format$(CURRENT.GRDMV(N, CURRENT.Nomins), "#.0000E+0")
  Next N
  'If CURRENT.NPPROP = 0 Then
    Me.Hide  'I am modal and G_Class is now higher in the Z-order
    Call G_class.CmdAccept_Click 'This will fill physical property 1 with the G-class sgs
    Me.Show
  'End If
    
  If CURRENT.PHIA > 0 And CURRENT.NPPROP < 2 Then
    CURRENT.NPPROP = 2
    CURRENT.LISTPP(2) = 2
    CURRENT.INDPP(2, 1) = CURRENT.INDPP(1, 1) + CURRENT.INDPP(1, 2)
    CURRENT.INDPP(2, 2) = 1
  End If
  If CURRENT.NPPROP > 0 Then
    For I = 1 To CURRENT.NPPROP
      Print #34, "PHYP";
      I1 = CURRENT.LISTPP(I)
      I2 = CURRENT.INDPP(I1, 2)
      Print #34, Format(I1, " 000");
      Print #34, Format(I2, " 000")
      Call PRNT_8G104(CURRENT.PPROP(), CURRENT.INDPP(I1, 1), CURRENT.INDPP(I1, 2), 34)
    Next I
  End If
  Print #34, ""

  'Now the plant data
  'Open a file to pass stream names to SIMOP
  Open RTrim(JobPath) & "StreamNames.dat" For Output As #44
  Print #44, "Stream names"
  
  Print #34, "PLANT DATA"
  Print #34, "PLNT   1"
  For N = 1 To Nstream
    If From_unit(N) <> -1 Then
      Print #34, "SCMA";
      Print #34, Format(Stream(N), " 000");
      Print #34, Format(From_unit(N), " 000");
      Print #34, Format(To_unit(N), " 000")
      Print #44, Stream(N)
    End If
  Next N
  
  Print #44, "Water feed streams"
  For N = 1 To Nstream
    If From_unit(N) = -1 Then
      hit = 0
      For I = 1 To N_WATER_DATA_STREAMS
        If CURRWDATA(I).ID = STREAM_ID(N) Then
          If CURRWDATA(I).WATER_RATE = 0! And CURRWDATA(I).PERCENT_SOLIDS = 0 Then
            hit = 0
          Else
            hit = -1
            Print #34, "WTRF";
            Print #34, Format(To_unit(N), " 000");
            Print #34, Format(Stream(N), " 000")
            Print #34, Format$(CURRWDATA(I).WATER_RATE, "#.00000E+0") + " ";
            Print #34, Format$(CURRWDATA(I).PERCENT_SOLIDS, "#0.00")
            Print #44, CStr(CURRWDATA(I).NSTR), CURRWDATA(I).STREAM_NAME
          End If
        End If
      Next I
      If hit = 0 Then
        WSTRING = "Water rate has not been specified for stream" + Str$(Stream(N))
        response% = MsgBox(WSTRING, 257, "WARNING")
        If response% = 2 Then
          Close #34
          Close #35
          Close #44
          Me.MousePointer = 1
          Exit Sub
        End If
      End If
    End If
  Next N
  For N = 1 To Nstream
    If From_unit(N) = 0 Then
      hit = 0
      For I = 1 To N_FEED_DATA_STREAMS
        If CURRDATA(I).ID = STREAM_ID(N) Then
          If CURRDATA(I).FEED_RATE = 0! Then
            hit = 0
          Else
            If CURRGDATA(I, 1).NSTR = 0 And CURRENT.NGC > 1 Then
              WSTRING = "Grade distributions have not been specified for stream" + Str$(Stream(N))
              response% = MsgBox(WSTRING, 257, "WARNING")
              If response% = 2 Then
                Close #34
                Close #35
                Close #44
                Me.MousePointer = 1
              Exit Sub
              End If
            End If
            hit = -1
            Print #34, "STRM";
            Print #35, "STRM";
            Print #34, Format(Stream(N), " 000");
            Print #35, Format(Stream(N), " 000");
            Print #34, "   2"
            Print #34, Format$(CURRDATA(I).FEED_RATE, "#.00000E+0") + " ";
            Print #34, Format$(CURRDATA(I).PERCENT_SOLIDS, "##.00")
            ' Interpolate the size distribution
            PM = 1!
            For J = 1 To CURRENT.NDC - 1
              PSIZE = Sqr(CURRENT.size(J) * CURRENT.size(J + 1))
              Call FRPASS(CURRDATA(I).CumSize(), CURRDATA(I).Fraction(), CURRDATA(I).NDC, P, PSIZE)
              FRACT(J) = PM - P
              PM = P
            Next J
            FRACT(CURRENT.NDC) = PM
            If CURRENT.NDC > 1 Then
              Call PRNT_8G104(FRACT(), 1, CURRENT.NDC, 34)
            End If
            ' Find the grade distributions for each of the simulation sizes.
            For J = 1 To CURRENT.NDC
              Print #35, "Size class ", Format$(J, "####")
              For K = 1 To CURRDATA(I).NO_GRADE_RANGES
                  If CURRENT.size(J) > CURRGDATA(I, K).SIZE_RANGE(1) And CURRENT.size(J) <= CURRGDATA(I, K).SIZE_RANGE(2) Then
                    If CURRGDATA(I, K).NGC = 0 Then
                      WSTRING = "No grade classes have been specified for size "
                      WSTRING = WSTRING + Str$(J) + " in stream " + Str$(Stream(N))
                      response% = MsgBox(WSTRING, 289, "WARNING")
                      If response% = 2 Then
                        Close #34
                        Close #35
                        Close #44
                        Me.MousePointer = 1
                        Exit Sub
                      End If
                    End If
                    If CURRGDATA(I, K).NGC > 1 Then
                      Call PRNT_8G104(CURRGDATA(I, K).Fraction(), 1, CURRGDATA(I, K).NGC, 34)
                    End If
                    Exit For
                  End If
              Next K
              'If K > CURRDATA(I).NO_GRADE_RANGES Then
              '  WSTRING = "Grade distribution data missing for size "
              '  WSTRING = WSTRING + Str$(J) + " in stream " + Str$(Stream(N))
              '  Response% = MsgBox(WSTRING, 289, "WARNING")
              '  If Response% = 2 Then
              '    Close #34
              '    Close #35
              '    Me.MousePointer = 1
              '    Exit Sub
              '  End If
              'End If

              If CURRENT.NSC > 1 And CURRSDATA(1, I).NSC = 0 Then
                WSTRING = "S-distribution data missing in stream " + Str$(Stream(N))
                response% = MsgBox(WSTRING, 257, "WARNING")
                If response% = 2 Then
                  Close #34
                  Close #35
                  Close #44
                  Me.MousePointer = 1
                  Exit Sub
                End If
              End If
              
              For M = 1 To CURRDATA(I).NO_S_RANGES
                Cum = 0#
                For K = 1 To CURRSDATA(M, I).NSC
                  Cum = Cum + CURRSDATA(M, I).Fraction(K)
                Next K
                If Abs(Cum - 1) > 0.00001 Then
                  WSTRING = "Error in S-class distribution data in grade-class "
                  WSTRING = WSTRING + Str$(M) + " in stream " + Str$(Stream(N))
                  response% = MsgBox(WSTRING, 257, "WARNING")
                  If response% = 2 Then
                    Close #34
                    Close #35
                    Me.MousePointer = 1
                    Exit Sub
                  End If
                End If
                If CURRENT.NSC > 1 Then
                'If CURRSDATA(1, I).NSC > 1 Then
                  Call PRNT_8G104(CURRSDATA(M, I).Fraction(), 1, CURRSDATA(M, I).NSC, 34)
                  Call PRNT_8G104(CURRSDATA(M, I).Fraction(), 1, CURRSDATA(M, I).NSC, 35)
                End If
              Next M
              
            Next J
          End If
        End If
      Next I
      If hit = 0 Then
        WSTRING = "Feed rate has not been specified for stream" + Str$(Stream(N))
        response% = MsgBox(WSTRING, 257, "WARNING")
        If response% = 2 Then
          Close #34
          Close #35
          Close #44
          Me.MousePointer = 1
          Exit Sub
        End If
      End If
    End If
  Next N
  Close #44
  
  If Converge.ChkTears.Value = 1 Then
    Print #34, "TEAR 0 0 0 0"
  End If
  Print #34,
  Print #34, "RUN DATA"
  Print #34, "CONV";
  If CURRENT.Conv <> 1 Then CURRENT.Conv = 3
  Print #34, Format(CURRENT.Conv, " 000")
  If CURRENT.ITER <= 0 Then CURRENT.ITER = 30
  Print #34, "ITER";
  Print #34, Format(CURRENT.ITER, " 000")
  Print #34, "TOLM ";
  If CURRENT.TOLM > -1 Or CURRENT.TOLM < -5 Then CURRENT.TOLM = -3
  Print #34, CURRENT.TOLM
  Close #34
  JobSystemInputData = True
  Close #35
  
  'Open a file to receive the stream numbers and names to go to SIMOP.FOR

  'Open a file to accept the experimental size distribution for re-entry and
  'later use by OPGRAPH.FOR. This file will also be used for re-entry to this module.
  Open RTrim(JobPath) + "SIZECOMP.OUT" For Output As #36
  Print #36, MODSIM.JOBNAME

  If N_FEED_DATA_STREAMS <= 0 Then
    MsgBox "There are no feed streams. Check your flowsheet", vbCritical, "ERROR"
    Close 36
    Me.MousePointer = 1
    Exit Sub
  End If
  For I = 1 To N_FEED_DATA_STREAMS
    If CURRDATA(I).NSTR <= 0 Then
      MsgBox "CURRDATA(I).NSTR <= 0"
      Close 36
      Exit Sub
    End If
    If (CURRDATA(I).NSTR > 0) Then
      Print #36, "Stream    ";
      Print #36, Format(CURRDATA(I).NSTR, " 000");
      Print #36, Format(CURRDATA(I).ID, " 000");
      If CURRDATA(I).STREAM_NAME = "" Then CURRDATA(I).STREAM_NAME = "No name"
      Print #36, " " + CURRDATA(I).STREAM_NAME
      If CURRDATA(I).FEED_RATE > 0 Then
        Print #36, "Feedrate  ";
        Print #36, Format$(CURRDATA(I).FEED_RATE, "#.0000E+0") + " ";
        Print #36, Format(CURRDATA(I).PERCENT_SOLIDS, "0.0000")
      End If
      If CURRDATA(I).NDC > 0 Then
        Print #36, "Size dist ";
        Print #36, Format(CURRDATA(I).NDC - 1, " 000")
        For J = 1 To CURRDATA(I).NDC - 1
          Print #36, Format$(CURRDATA(I).CumSize(J), "#.0000E+0") + " ";
          Print #36, Format$(CURRDATA(I).Fraction(J), "0.0000")
        Next J
      End If
    End If
  Next I
  For I = 1 To N_O_DATA_STREAMS
    If (CURRODATA(I).NSTR > 0) Then
      Print #36, "Stream    ";
      Print #36, Format(CURRODATA(I).NSTR, " 000");
      Print #36, Format(CURRODATA(I).ID, " 000");
      If CURRODATA(I).STREAM_NAME = "" Then CURRODATA(I).STREAM_NAME = "No name"
      Print #36, " " + CURRODATA(I).STREAM_NAME
      If CURRODATA(I).NDC > 0 Then
        Print #36, "Size dist ";
        Print #36, Format(CURRODATA(I).NDC - 1, " 000")
        For J = 1 To CURRODATA(I).NDC - 1
          Print #36, Format$(CURRODATA(I).CumSize(J), "#.0000E+0") + " ";
          Print #36, Format$(CURRODATA(I).Fraction(J), "0.0000")
        Next J
      End If
    End If
  Next I
  For I = 1 To N_WATER_DATA_STREAMS
    If (CURRWDATA(I).NSTR > 0) Then
      Print #36, "Stream    ";
      Print #36, Format(CURRWDATA(I).NSTR, " 000");
      Print #36, Format(CURRWDATA(I).ID, " 000");
      If CURRWDATA(I).STREAM_NAME = "" Then CURRWDATA(I).STREAM_NAME = "No name"
      Print #36, " " + CURRWDATA(I).STREAM_NAME
      Print #36, "Water feed ";
      Print #36, Format$(CURRWDATA(I).WATER_RATE, "#.0000E+0") + " ";
      Print #36, Format(CURRWDATA(I).PERCENT_SOLIDS, "0.0000")
    End If
  Next I
  Print #36, "END OF FILE"
  Close #36
  JobSizeData = True

  'Open a file to receive the grade distribution data for re-entry.
  Open RTrim(JobPath) + "GC_DISTR.OUT" For Output As #37
  Print #37, "Number of data streams ", N_FEED_DATA_STREAMS
  For I = 1 To N_FEED_DATA_STREAMS
    Print #37, "Stream    ";
    Print #37, Format(CURRDATA(I).NSTR, " 000");
    Print #37, Format(CURRDATA(I).ID, " 000");
    If CURRDATA(I).STREAM_NAME = "" Then CURRDATA(I).STREAM_NAME = "No name"
    Print #37, " " + CURRDATA(I).STREAM_NAME
    Print #37, "Number of grade ranges ", CURRDATA(I).NO_GRADE_RANGES
    For K = 1 To CURRDATA(I).NO_GRADE_RANGES
      Print #37, Format(K, " 000")
      Call PRNT_8G104(CURRGDATA(I, K).SIZE_RANGE(), 1, 2, 37)
      Print #37, Format(CURRGDATA(I, K).NGC, " 000")
      Call PRNT_8G104(CURRGDATA(I, K).Fraction(), 1, CURRGDATA(I, K).NGC, 37)
    Next K
  Next I
  Print #37, "END OF FILE"
  Close #37
  JobGCDistribution = True

  'Open a file to receive the grade distribution data for re-entry and for use
  'by LIBGRAPH.
  Open RTrim(JobPath) + "LIBCOMP.OUT" For Output As #37
  Print #37, MODSIM.JOBNAME

  'First the internal streams
  For I = 1 To N_O_DATA_STREAMS
    Print #37, "Stream    ";
    'Note that the stream number is available only in CURRODATA until form G_DATA
    'has been entered.
    Print #37, Format(CURRODATA(I).NSTR, " 000");
    Print #37, Format(CURRODATA(I).ID, " 000");
    If CURRODATA(I).STREAM_NAME = "" Then CURRODATA(I).STREAM_NAME = "No name"
    Print #37, " " + CURRODATA(I).STREAM_NAME
    Print #37, "Lib dist  ";
    Print #37, Format(CURRLDATA(I).NGC, " 000")
    For M = 1 To CURRLDATA(I).NGC
      Print #37, Format(CURRLDATA(I).Fraction(M), "0.####")
    Next M
  Next I
  'Now the feed streams. This data will not be used for re-entry only to plot
  'liberation spectra graphs.
  For I = 1 To N_FEED_DATA_STREAMS
    Print #37, "Stream    ";
    Print #37, Format(CURRDATA(I).NSTR, " 000");
    Print #37, Format(CURRDATA(I).ID, " 000");
    If CURRDATA(I).STREAM_NAME = "" Then CURRDATA(I).STREAM_NAME = "No name"
    Print #37, " " + CURRDATA(I).STREAM_NAME
    Print #37, "Lib dist  ";
    Print #37, Format(CURRGDATA(I, 1).NGC, " 000")
    For J = 1 To CURRGDATA(I, 1).NGC
      Fraction = 0#
      For K = 1 To CURRDATA(I).NO_GRADE_RANGES
        PSIZE = CURRGDATA(I, K).SIZE_RANGE(1)
        Call FRPASS(CURRDATA(I).CumSize(), CURRDATA(I).Fraction(), CURRDATA(I).NDC, P1, PSIZE)
        PSIZE = CURRGDATA(I, K).SIZE_RANGE(2)
        Call FRPASS(CURRDATA(I).CumSize(), CURRDATA(I).Fraction(), CURRDATA(I).NDC, P2, PSIZE)
        FRACT(K) = P2 - P1
        Fraction = CURRGDATA(I, K).Fraction(J) * FRACT(K) + Fraction
      Next K
      Print #37, Format(Fraction, "0.####")
    Next J
  Next I
  Print #37, "END OF FILE"
  JobLibcomp = True
  Close #37

  'Open a file to receive the s-class distribution data for re-entry.
  Open RTrim(JobPath) + "SC_DISTR.OUT" For Output As #39
  Print #39, "Number of data streams ", N_FEED_DATA_STREAMS
  For I = 1 To N_FEED_DATA_STREAMS
    Print #39, "Stream    ";
    Print #39, Format(CURRDATA(I).NSTR, " 000");
    Print #39, Format(CURRDATA(I).ID, " 000");
    If CURRDATA(I).STREAM_NAME = "" Then CURRDATA(I).STREAM_NAME = "No name"
    Print #39, " " + CURRDATA(I).STREAM_NAME
    Print #39, "Number of S-ranges", CURRDATA(I).NO_S_RANGES
    For K = 1 To CURRDATA(I).NO_S_RANGES
      Print #39, Format(K, " 000")
      Print #39, Format(CURRSDATA(K, I).NSC, " 000")
      Call PRNT_8G104(CURRSDATA(K, I).Fraction(), 1, CURRSDATA(K, I).NSC, 39)
    Next K
  Next I
  Print #39, "END OF FILE"
  Close #39
  JobSCDistribution = True
  
  'Open a file to transmit information directly to SIMOP.FOR
  Open RTrim(JobPath) & "SIMOP.DAT" For Output As #39
  'Write flags in the order COALFL,PROXFL,CVFLAG,SULPFL,PYRFL,GOLDFL
  Print #39, CURRENT.Flags
  
  If Mid(CURRENT.Flags, 3, 1) = "T" Then
    Print #39, "1"
    Call PRNT_8G104(CURRENT.PPROP(), CURRENT.INDPP(8, 1), CURRENT.INDPP(8, 2), 39)
  End If
  If Mid(CURRENT.Flags, 4, 1) = "T" Then
    Call PRNT_8G104(CURRENT.PPROP(), CURRENT.INDPP(9, 1), CURRENT.INDPP(9, 2), 39)
  End If
  If Mid(CURRENT.Flags, 5, 1) = "T" Then
    Call PRNT_8G104(CURRENT.PPROP(), CURRENT.INDPP(10, 1), CURRENT.INDPP(10, 2), 39)
  End If
  Close #39
  JobSimopData = True

  'Open a file to receive the information to calculate the liberation models
  Open RTrim(JobPath) + "AM_COEFF.DAT" For Output As 38
  Print #38, Val(NDCM.text);
  Print #38, Val(NGCM.text);
  Print #38, "1"
  Call PRNT_8G104(CURRENT.size(), 1, CURRENT.NDC, 38)
  Print #38, CURRENT.MassVol
  For N = 1 To CURRENT.NGC
    For M = 1 To CURRENT.Nomins - 1
      Print #38, Format$(CURRENT.GRDMV(N, M), "#.0000E+0") + " ";
    Next M
    Print #38, Format$(CURRENT.GRDMV(N, CURRENT.Nomins), "#.0000E+0")
  Next N
  Call PRNT_8G104(CURRENT.MineralSG(), 1, CURRENT.Nomins, 38)
  Print #38, CURRENT.PHIA
  Close #38
  
  'Recompute the Andrews-Mika coefficient matrix if necessary.
  If LibMatrix_check = 1 Then
    MsgBox "Calculating the Ljubljana Andrews-Mika diagram", 64, "INFORMATION"
    Me.MousePointer = 11
    NameOfJob = MODSIM.JOBNAME
    RetVal = LJUBAMD(JobPath, NameOfJob)
    Me.MousePointer = 1
    If RetVal > -1 Then
      MsgBox " Error in Andrews-Mika diagram calculation " & CStr(RetVal), 64, "INFORMATION"
    Else
      JobLjubAMD = True
    End If
  End If
  
  JobSystemDataNeeded = False
  ChangedJobname = False
  ChangedFlowsheet = False
  ChangedSystemInputData = True
  SavedJob = False

  'Change the mouse pointer back to arrow
  Me.MousePointer = 1
  Unload G_class
  Unload A_MD
  Unload Me
End Sub

Private Sub D1_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Specify the size largest particle in the feed.  Units must be meters"
End Sub

Private Sub D1_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then
    If NGCM.Enabled = -1 Then
      NGCM.SetFocus
      KeyAscii = 0
    End If
  End If
End Sub

Private Sub D1_LostFocus()
  Instruction.Visible = 0
End Sub

Public Function FindFeedStreamIndexNumber(Stream As Integer) As Boolean
'**********************************************************************
'Finds the list index for feed stream number
  Dim IND As Integer, I As Integer
  FindFeedStreamIndexNumber = False
  IND = Me.StreamList.ListCount
  For I = 0 To IND - 1
    If Val(Me.StreamList.List(I)) = Stream Then
      FindFeedStreamIndexNumber = True
      Me.StreamList.ListIndex = I
    End If
  Next I
End Function

Public Function FindWaterStreamIndexNumber(Stream As Integer) As Boolean
'**********************************************************************
'Finds the list index for water stream number
  Dim IND As Integer, I As Integer
  FindWaterStreamIndexNumber = False
  IND = Me.WStreamList.ListCount
  For I = 0 To IND - 1
    If Val(Me.WStreamList.List(I)) = Stream Then
      FindWaterStreamIndexNumber = True
      Me.WStreamList.ListIndex = I
    End If
  Next I
End Function

Public Function FindOtherStreamIndexNumber(Stream As Integer) As Boolean
'**********************************************************************
'Finds the list index for an internal stream number
  Dim IND As Integer, I As Integer
  FindOtherStreamIndexNumber = False
  IND = Me.OStreamList.ListCount
  For I = 0 To IND - 1
    If Val(Me.OStreamList.List(I)) = Stream Then
      FindOtherStreamIndexNumber = True
      Me.OStreamList.ListIndex = I
    End If
  Next I
End Function

Public Function FindSequentialStreamNumber(StreamNumber As Integer) As Integer
'*****************************************************************************
'Finds the sequential number that is attached to this stream (index of vectorStream(100))
  Dim I As Integer
  For I = 1 To Nstream
    If Stream(I) = StreamNumber Then
      FindSequentialStreamNumber = I
      Exit For
    End If
  Next I
End Function
Private Sub Form_Load()
'**********************
  Dim STREAM_NAME As String * 30
  Dim NL As String * 2
  Dim KEYY As String * 4
  Dim KK As Integer
  Dim NSTR As Integer
  Dim N As Integer, M As Integer, K As Integer, J As Integer
  Dim NPLNT As Integer
  Dim N_FEED_STREAMS As Integer, N_WATER_STREAMS As Integer, N_O_STREAMS As Integer
  Dim NF As Integer, NO As Integer, NW As Integer
  Dim NDS As Integer
  Dim Feed As Single, Percent As Single
  Dim ProxFlag As Boolean
  Dim WS As Single
  Dim I1 As Integer, I2 As Integer, I As Integer
  Dim Dummy As String
  Dim Dummy1 As Single, Dummy2 As Single
  Dim ndummy As Integer, response As Integer
  
  On Error GoTo ErrHandler
  
  loadingForm = True
  FastFeedStreamChange = False
  FastWaterStreamChange = False
  FastOtherStreamChange = False
  FORM_CHANGE_FLAG = False
  Sysdata.MAX_FEED_DATA_SETS = 6
  Sysdata.MAX_WATER_DATA_SETS = 30
  Sysdata.MAX_O_DATA_SETS = 30
  If ProfessionalVersion Or AcademicVersion Then
    Sysdata.MAX_O_DATA_SETS = 100
  End If
  Sysdata.MAX_LIB_DATA_SETS = 36
  NGCM.text = Str$(1)
  NGCM.Enabled = False
  
  'Set up any required defaults
  CURRENT.MassVol = "GRDM"
  CURRENT.GRDMV(1, 1) = 1#

'Open file to get the stream data coming from the flowsheet
  If JobSystemData Then
    Open RTrim(JobPath) + "SYSDATA.OUT" For Input As #31
  Else
    loadingForm = False
    Exit Sub
  End If
  Line Input #31, JOBNAMEVAR
  If Trim(JOBNAMEVAR) <> Trim(MODSIM.JOBNAME) Then
    response = MsgBox("Flowsheet data file originates from job " & JOBNAMEVAR & "." & Chr(13) & Chr(10) & "Select cancel if you want to load a blank form.", 33, "WARNING")
    If response = vbCancel Then
      loadingForm = False
      Close #31
      Exit Sub
    End If
  End If
  Sysdata.Caption = "MODSIM - Specify ore characteristics and stream data for job " + Trim(JOBNAMEVAR)
  N_FEED_STREAMS = 0
  N_WATER_STREAMS = 0
  N_O_STREAMS = 0
  Input #31, Nstream
  For N = 1 To Nstream
    Input #31, Stream(N)
    Input #31, STREAM_ID(N)
    STREAM_TYPE(N) = Input(1, #31)
    Input #31, From_unit(N)
    Input #31, To_unit(N)
    WSTRING = CStr(Stream(N))
    If STREAM_TYPE(N) = "F" Then
      N_FEED_STREAMS = N_FEED_STREAMS + 1
      If N_FEED_STREAMS > MAX_FEED_DATA_SETS Then
        MsgBox "Too many feed streams have been included on the flowsheet", 0, "ERROR"
        loadingForm = False
        Exit Sub
      End If
    End If
    If STREAM_TYPE(N) = "W" Then
      N_WATER_STREAMS = N_WATER_STREAMS + 1
      If N_WATER_STREAMS > MAX_WATER_DATA_SETS Then
        MsgBox "Too many water addition streams have been included on the flowsheet", 0, "ERROR"
        loadingForm = False
        Exit Sub
      End If
    End If
    If STREAM_TYPE(N) = "O" Then
      N_O_STREAMS = N_O_STREAMS + 1
    End If
  Next N
  Close #31
  
  'Open file SIZECOMP.OUT to get CURRENT data for size distributions
  N_FEED_DATA_STREAMS = 0
  N_O_DATA_STREAMS = 0
  N_WATER_DATA_STREAMS = 0
  If JobSizeData Then
    Open RTrim(JobPath) + "SIZECOMP.OUT" For Input As 35
    For N = 1 To MAX_FEED_DATA_SETS
      CURRDATA(N).ID = 0
    Next N
    For N = 1 To MAX_O_DATA_SETS
      CURRODATA(N).ID = 0
    Next N
    For N = 1 To MAX_WATER_DATA_SETS
      CURRWDATA(N).ID = 0
    Next N
    Line Input #35, JOBNAMEVAR
    If Mid$(JOBNAMEVAR, 1, 8) = "END OF F" Then
      Close #35
    Else
      NF = 0
      NO = 0
      NW = 0
      WSTRING = Input$(10, #35)
      Do While Not EOF(35)
        If WSTRING = "END OF FIL" Then Exit Do
        If NF >= MAX_FEED_DATA_SETS Then
          MsgBox "Too many feed data sets in file SIZECOMP.OUT", 0, "WARNING"
          Exit Do
        End If
        If NO >= MAX_O_DATA_SETS Then
          MsgBox "Too many data sets in file SIZECOMP.OUT", 0, "WARNING"
          Exit Do
        End If
        If NW >= MAX_WATER_DATA_SETS Then
          MsgBox "Too many water feed data sets in file SIZECOMP.OUT", 0, "WARNING"
          Exit Do
        End If
        If WSTRING = "Stream    " Then
          Input #35, NSTR, ID
          For N = 1 To Nstream
            If STREAM_ID(N) = ID Then
              Exit For
            End If
          Next N
          If N <= Nstream Then
            'Stream with matching ID has been found
            If STREAM_TYPE(N) = "F" Then
              NF = NF + 1
              Line Input #35, STREAM_NAME
              CURRDATA(NF).ID = ID
              CURRDATA(NF).STREAM_NAME = STREAM_NAME
              WSTRING = Input$(10, #35)
              If WSTRING = "Feedrate  " Then
                Input #35, Feed, Percent
                CURRDATA(NF).FEED_RATE = Feed
                CURRDATA(NF).PERCENT_SOLIDS = Percent
                WSTRING = Input$(10, #35)
              End If
              If WSTRING = "Size dist " Then
                Input #35, CURRDATA(NF).NDC
                For M = 1 To CURRDATA(NF).NDC
                  Input #35, CURRDATA(NF).CumSize(M), CURRDATA(NF).Fraction(M)
                Next M
                CURRDATA(NF).NDC = CURRDATA(NF).NDC + 1
                WSTRING = Input$(10, #35)
              End If
            ElseIf STREAM_TYPE(N) = "O" Then
              NO = NO + 1
              Line Input #35, STREAM_NAME
              CURRODATA(NO).ID = ID
              CURRODATA(NO).STREAM_NAME = STREAM_NAME
              WSTRING = Input$(10, #35)
              If WSTRING = "Size dist " Then
                Input #35, CURRODATA(NO).NDC
                For M = 1 To CURRODATA(NO).NDC
                  Input #35, CURRODATA(NO).CumSize(M), CURRODATA(NO).Fraction(M)
                Next M
                CURRODATA(NO).NDC = CURRODATA(NO).NDC + 1
                WSTRING = Input$(10, #35)
              End If
            ElseIf STREAM_TYPE(N) = "W" Then
              NW = NW + 1
              Line Input #35, STREAM_NAME
              CURRWDATA(NW).ID = ID
              CURRWDATA(NW).STREAM_NAME = STREAM_NAME
              WSTRING = Input$(10, #35)
              If WSTRING = "Water feed" Then
                Input #35, Feed, Percent
                CURRWDATA(NW).WATER_RATE = Feed
                CURRWDATA(NW).PERCENT_SOLIDS = Percent
                WSTRING = Input$(10, #35)
              End If
            Else
              WSTRING = Input$(10, #35)
            End If
          Else
            WSTRING = "The system data does not match your flowsheet in the selected job."
            WSTRING = WSTRING + Chr(13) + "Please check the system data carefully."
            response = MsgBox(WSTRING, 48, "WARNING")
           ' For N = 1 To NSTREAM
           '   WSTRING = Str$(Stream(N))
           '   If STREAM_TYPE(N) = "F" Then StreamList.AddItem WSTRING
           '   If STREAM_TYPE(N) = "W" Then WStreamList.AddItem WSTRING
           '   If STREAM_TYPE(N) = "O" Then OStreamList.AddItem WSTRING
           ' Next N
           ' JobSystemData = False
           ' JobSystemInputData = False
           ' JobSizeData = False
           ' Close #35
           ' Exit Sub
            Line Input #35, STREAM_NAME
            WSTRING = Input$(10, #35)
            If WSTRING = "Feedrate  " Then
              Input #35, Feed, Percent
              WSTRING = Input$(10, #35)
            End If
            If WSTRING = "Size dist " Then
              Input #35, ndummy
              For M = 1 To ndummy
                Input #35, Dummy1, Dummy2
              Next M
              WSTRING = Input$(10, #35)
            End If
            If WSTRING = "Water feed" Then
              Input #35, Feed, Percent
              WSTRING = Input$(10, #35)
            End If
          End If
        Else
          WSTRING = Input$(10, #35)
          If WSTRING = "Feedrate  " Then
            Input #35, Feed, Percent
            WSTRING = Input$(10, #35)
          End If
          If WSTRING = "Size dist " Then
            Input #35, ndummy
            For M = 1 To ndummy
              Input #35, Dummy1, Dummy2
            Next M
            WSTRING = Input$(10, #35)
          End If
          If WSTRING = "Water feed" Then
            Line Input #35, STREAM_NAME
            WSTRING = Input$(10, #35)
            If WSTRING = "Water feed" Then
              Input #35, Feed, Percent
              WSTRING = Input$(10, #35)
            End If
          End If
        End If
      Loop
      N_FEED_DATA_STREAMS = NF
      N_WATER_DATA_STREAMS = NW
      N_O_DATA_STREAMS = NO
    End If
    Close #35
  End If
  
  'Enter the streams in the stream list box
  For N = 1 To Nstream
    WSTRING = CStr(Stream(N))
    If Stream(N) = 0 Then
      MsgBox "Attempting to load a stream with number 0", vbInformation, "WARNING"
      Exit Sub
    End If
    For M = 1 To N_FEED_DATA_STREAMS
      If CURRDATA(M).ID = STREAM_ID(N) Then
        WSTRING = WSTRING + " " + CURRDATA(M).STREAM_NAME
        CURRDATA(M).NSTR = Stream(N)
      End If
    Next M
    For M = 1 To N_WATER_DATA_STREAMS
      If CURRWDATA(M).ID = STREAM_ID(N) Then
        WSTRING = WSTRING + " " + CURRWDATA(M).STREAM_NAME
        CURRWDATA(M).NSTR = Stream(N)
      End If
    Next M
    For M = 1 To N_O_DATA_STREAMS
      If CURRODATA(M).ID = STREAM_ID(N) Then
        WSTRING = WSTRING + " " + CURRODATA(M).STREAM_NAME
        CURRODATA(M).NSTR = Stream(N)
      End If
    Next M
    If STREAM_TYPE(N) = "F" Then StreamList.AddItem Trim(WSTRING)
    If STREAM_TYPE(N) = "W" Then WStreamList.AddItem Trim(WSTRING)
    If STREAM_TYPE(N) = "O" Then OStreamList.AddItem Trim(WSTRING)
  Next N
  

  If JobSystemInputData Then
    'Open file to get the current System data
    Open RTrim(JobPath) + "CURRDATA.SYD" For Input As #32
    CURRENT.NPPROP = 0
    CURRENT.INDPPM = 1
    CURRENT.INDPP(1, 1) = 1
    CURRENT.GRDMV(1, 1) = 1!
    MinName(0).text = ""
    NL = Chr$(13) + Chr$(10)
    Do While Not EOF(32)
      KEYY = Input$(4, #32)
      If (KEYY <> "STOP") Then
        If KEYY = "SYST" Then
          Input #32, WSTRING
          Line Input #32, JOBNAMEVAR
          If Trim(JOBNAMEVAR) <> Trim(MODSIM.JOBNAME) Then
            response = MsgBox("System input data file originates from job " & JOBNAMEVAR & "." & Chr(13) & Chr(10) & "Select cancel if you want to load the form without current data.", 33, "WARNING")
            If response = vbCancel Then
              Close #32
              loadingForm = False
              Exit Sub
            End If
          End If
        ElseIf KEYY = "NDGS" Then
          Input #32, NPLNT, CURRENT.NDC, CURRENT.NGC, CURRENT.NSC
          NDCM.text = Str$(CURRENT.NDC)
          NGCM.text = Str$(CURRENT.NGC)
          Me.NGCM.Enabled = True
          Me.NGCM.Visible = True
          'If CURRENT.NGC > 1 Then
            CmdGradeClasses.Visible = -1
            CmdGradeClasses.Enabled = True
          'End If
          NSCM.text = Str$(CURRENT.NSC)
          If CURRENT.NSC > 1 Then
            CmdSClasses.Visible = -1
            CmdSClasses.Enabled = True
          End If
          CURRENT.INDPPM = CURRENT.NGC + 1
        ElseIf KEYY = "COAL" Then
          ProxFlag = False
          Input #32, ndummy
          If ndummy <> 0 Then
            ProxFlag = True
            ChkProximate.Value = 1
          End If
          CmdGradeClasses.Enabled = False
          OptCoalProcPlant.Value = True
        ElseIf KEYY = "MINS" Then
          Input #32, CURRENT.Nomins
          If CURRENT.Nomins > 1 Then
            NGCM.Enabled = -1
            Lblngcm.Enabled = True
            If Not OptCoalProcPlant.Value Then CmdGradeClasses.Enabled = True
          Else
            NGCM.text = Str$(1)
            NGCM.Enabled = 0
            CmdGradeClasses.Enabled = 0
          End If
          For N = 1 To CURRENT.Nomins
            MinName(N - 1).Visible = -1
            TxtSG(N - 1).Visible = True
            Input #32, CURRENT.MineralNames(N)
            MinName(N - 1).text = CURRENT.MineralNames(N)
          Next N
          NoMin.text = Str$(CURRENT.Nomins)
        ElseIf KEYY = "SIZE" Then
          Line Input #32, Dummy$
          Input #32, WS
          D1.text = Format$(WS, "#.00E+#")
        ElseIf KEYY = "SPGR" Then
          Line Input #32, Dummy$
          LblSGSpec.Visible = True
          OptSGSpec(0).Value = -1
          For N = 1 To CURRENT.Nomins
            TxtSG(N - 1).Visible = True
            TxtSG(N - 1).Enabled = True
            Input #32, CURRENT.MineralSG(N)
            TxtSG(N - 1).text = Str$(CURRENT.MineralSG(N))
          Next N
        ElseIf KEYY = "GRDV" Or KEYY = "GRDM" Then
          Line Input #32, Dummy$
          CURRENT.MassVol = KEYY
          CURRENT.SPECIFIED = -1
          For N = 1 To CURRENT.NGC
            For M = 1 To CURRENT.Nomins
              Input #32, CURRENT.GRDMV(N, M)
            Next M
          Next N
        ElseIf KEYY = "PHYP" Then
          Input #32, I1, I2
          CURRENT.NPPROP = CURRENT.NPPROP + 1
          CURRENT.LISTPP(CURRENT.NPPROP) = I1
          If I1 = 1 Then
            OptSGSpec(1).Value = True
            'For M = 1 To CURRENT.Nomins
            '  TxtSG(M - 1).Visible = False
            'Next M
          End If
          CURRENT.INDPP(I1, 1) = CURRENT.INDPPM
          CURRENT.INDPP(I1, 2) = I2
          For I = 1 To I2
            Input #32, CURRENT.PPROP(CURRENT.INDPPM)
            If I1 = 2 Then CURRENT.PHIA = CURRENT.PPROP(CURRENT.INDPPM)
            CURRENT.INDPPM = CURRENT.INDPPM + 1
          Next I
        ElseIf KEYY = "CONV" Then
          Input #32, CURRENT.Conv
        ElseIf KEYY = "ITER" Then
          Input #32, CURRENT.ITER
        ElseIf KEYY = "TOLM" Then
          Input #32, CURRENT.TOLM
        Else
          Line Input #32, Dummy$
        End If
      End If
      If KEYY = "STOP" Then Exit Do
    Loop
    Close #32
  End If

  If JobGCDistribution Then
    'Open file GC_DISTR.DAT to read any CURRENT grade distribution data
    Open RTrim(JobPath) + "GC_DISTR.OUT" For Input As #38
    WSTRING = Input$(22, #38)
    If WSTRING = "Number of data streams" Then
      Input #38, NDS
      N_LIB_DATA_STREAMS = NDS
    Else
      MsgBox "Error in grade distribution data", 0, "ERROR"
      Close #38
      loadingForm = False
      Exit Sub
    End If
    For I = 1 To NDS
      WSTRING = Input$(10, #38)
      If WSTRING = "Stream    " Then
        Input #38, NSTR, ID
        Line Input #38, STREAM_NAME
        'Find the index of CURRDATA that corresponds to this ID
        For N = 1 To N_FEED_DATA_STREAMS
          If CURRDATA(N).ID = ID Then
            Exit For
          End If
        Next N
        If N > MAX_FEED_DATA_SETS Then
          MsgBox "Too many grade data sets", 0, "WARNING"
          Exit For
        End If
        WSTRING = Input$(22, #38)
        If WSTRING = "Number of grade ranges" Then
          Input #38, CURRDATA(N).NO_GRADE_RANGES
          For K = 1 To CURRDATA(N).NO_GRADE_RANGES
            CURRGDATA(N, K).NSTR = NSTR
            Input #38, KK
            Input #38, CURRGDATA(N, K).SIZE_RANGE(1)
            Input #38, CURRGDATA(N, K).SIZE_RANGE(2)
            Input #38, CURRGDATA(N, K).NGC
            For J = 1 To CURRGDATA(N, K).NGC
              Input #38, CURRGDATA(N, K).Fraction(J)
            Next J
          Next K
        Else
          MsgBox "Error in grade distribution data", 16, "ERROR"
          Close #38
          loadingForm = False
          Exit Sub
        End If
      End If
    Next I
    Close #38
  End If

  If JobLibcomp Then
    'Open file LIBCOMP.OUT to read experimental liberation spectra
    For I = 1 To N_LIB_DATA_STREAMS
      CURRLDATA(I).ID = 0
    Next I
    Open RTrim(JobPath) + "LIBCOMP.OUT" For Input As #36
    Line Input #36, JOBNAMEVAR
    If Mid$(JOBNAMEVAR, 1, 8) <> "END OF F" Then
      N = 0
      WSTRING = Input$(10, #36)
      Do While Not EOF(36)
        If WSTRING = "END OF FIL" Then Exit Do
        If N >= MAX_LIB_DATA_SETS Then
          MsgBox "Too many data sets in file LIBCOMP.OUT", 0, "WARNING"
          Exit Do
        End If
        If WSTRING = "Stream    " Then
          N = N + 1
          Input #36, NSTR, ID
          Line Input #36, STREAM_NAME
        '  IF RTRIM$(STREAM_NAME) = "No name" THEN STREAM_NAME = " "
          CURRLDATA(N).ID = ID
          CURRLDATA(N).STREAM_NAME = STREAM_NAME
          WSTRING = Input$(10, #36)
          If WSTRING = "Lib dist  " Then
            Input #36, CURRLDATA(N).NGC
            For M = 1 To CURRLDATA(N).NGC
              Input #36, CURRLDATA(N).Fraction(M)
            Next M
          End If
        End If
        WSTRING = Input$(10, #36)
      Loop
      N_LIB_DATA_STREAMS = N
    End If
    Close #36
  End If

  If JobSCDistribution Then
    'Open file SC_DISTR.OUT to read any CURRENT S-CLASS distribution data
    Open RTrim(JobPath) + "SC_DISTR.OUT" For Input As #39
    WSTRING = Input$(22, #39)
    If WSTRING = "Number of data streams" Then
      Input #39, NDS
    Else
      MsgBox "Error in S-distribution data", 0, "ERROR"
      loadingForm = False
      Exit Sub
    End If
    For I = 1 To NDS
      WSTRING = Input$(10, #39)
      If WSTRING = "Stream    " Then
        Input #39, NSTR, ID
        Line Input #39, STREAM_NAME
        'Find the index of CURRDATA that corresponds to this ID
        For N = 1 To N_FEED_DATA_STREAMS
          If CURRDATA(N).ID = ID Then
            If N > MAX_FEED_DATA_SETS Then
              MsgBox "Too many S-class data sets", 0, "WARNING"
              loadingForm = False
              Exit Sub
            End If
            Exit For
          End If
        Next N
        WSTRING = Input$(18, #39)
        If WSTRING = "Number of S-ranges" Then
          Input #39, CURRDATA(N).NO_S_RANGES
          For K = 1 To CURRDATA(N).NO_S_RANGES
            CURRSDATA(K, N).NSTR = NSTR
            Input #39, KK
            Input #39, CURRSDATA(K, N).NSC
            For J = 1 To CURRSDATA(K, N).NSC
              Input #39, CURRSDATA(K, N).Fraction(J)
            Next J
          Next K
        Else
          MsgBox "Error in S-distribution data", 0, "ERROR"
          loadingForm = False
          Exit Sub
        End If
      End If
    Next I
    Close #39
  End If
  loadingForm = False
  Exit Sub
  
ErrHandler:
  MsgBox "Error in reading system data", 16, "ERROR"
  loadingForm = False
  Close
  Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  If Not MODSIM.ViewFlowsheetFlag Then
  End If
End Sub

Private Sub MinName_GotFocus(Index As Integer)
'*********************************************
  Instruction.Visible = -1
  Instruction.Caption = "Specify mineral names."
End Sub

Private Sub MinName_KeyPress(Index As Integer, KeyAscii As Integer)
'******************************************************************
  If KeyAscii = 13 Then
    If OptSGSpec(0).Value = True Then
      TxtSG(Index).SetFocus
    ElseIf Index < Val(NoMin.text) - 1 Then
      MinName(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub


Private Sub MinName_LostFocus(Index As Integer)
  Instruction.Visible = 0
  Instruction.Caption = ""
End Sub

Private Sub MnuAccept_Click()
'****************************
  Call CmdReturn_Click
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

Private Sub NDCM_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Specify the number of size classes to be used in the simulation"
End Sub

Private Sub NDCM_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then
    KeyAscii = 0
    D1.SetFocus
  End If
End Sub

Private Sub NDCM_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub NGCM_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 And Val(NGCM.text) > 1 Then
    CmdGradeClasses.Enabled = -1
    CmdGradeClasses.SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub NGCM_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Number of grade clases. Must be >4 to simulate liberation during grinding."
End Sub

Private Sub NGCM_LostFocus()
  If Val(NGCM.text) > 1 Then
    CmdGradeClasses.Visible = -1
  End If
  Instruction.Visible = 0
End Sub

Private Sub NoMin_Change()
'*************************
  If Not loadingForm Then
    If Screen.ActiveControl.Tag = "NumberOfMinerals" Then
      FORM_CHANGE_FLAG = True
      Me.OptSGSpec(0).Value = True
    End If
  End If
End Sub

Private Sub NoMin_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Specify the number of minerals of interest in the ore"
End Sub

Private Sub NoMin_KeyPress(KeyAscii As Integer)
'**********************************************
  Dim N As Integer
  If KeyAscii = 13 Then
    MinName(0).Visible = True
    MinName(0).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub NoMin_LostFocus()
'****************************
  Dim N As Integer
  CURRENT.Nomins = Val(NoMin.text)
  If Val(NoMin.text) > 1 Then
    Lblngcm.Enabled = True
    NGCM.Enabled = True
    CmdGradeClasses.Enabled = True
    For N = 1 To Val(NoMin.text)
      MinName(N - 1).Visible = True
      If OptSGSpec(0).Value = True Then TxtSG(N - 1).Visible = True
    Next N
  Else
    NGCM.text = CStr(1)
    Lblngcm.Enabled = False
    NGCM.Enabled = False
    If Not FORM_CHANGE_FLAG Then CmdGradeClasses.Enabled = 0
  End If
  For N = Val(NoMin.text) + 1 To 7
    MinName(N - 1).Visible = False
    TxtSG(N - 1).Visible = False
  Next N
Instruction.Visible = 0
End Sub

Private Sub NSCM_KeyPress(KeyAscii As Integer)
'*********************************************
  If KeyAscii = 13 And Val(NSCM.text) > 1 Then
    CmdSClasses.Visible = -1
    CmdSClasses.Enabled = True
    CmdSClasses.SetFocus
    KeyAscii = 0
  End If
  If Val(NSCM.text) <= 1 Then
    CmdSClasses.Enabled = False
  End If
End Sub

Private Sub NSCM_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Number of S-clases."
End Sub

Private Sub NSCM_LostFocus()
  If Val(NSCM.text) > 1 Then
    CmdSClasses.Visible = -1
    CmdSClasses.Enabled = True
  End If
  Instruction.Visible = 0
End Sub

Private Sub OptCoalProcPlant_Click()
'***********************************
  Dim I As Integer
  ChkProximate.Visible = True
  Me.OptSGSpec(1).Value = True
  'Lblngcm.Caption = "Number of washability classes"
  Lblngcm.Caption = ""
  Lblngcm.Enabled = True
  NoMin.Enabled = False
  'NGCM.Enabled = False
  NGCM.Visible = False
  'CmdGradeClasses.Enabled = False
  CmdGradeClasses.Visible = False
  If ChkProximate.Value = 1 Then
    NoMin.text = CStr(4)
    For I = 0 To 3
      MinName(I).Visible = True
    Next I
    MinName(0).text = "Ash "
    MinName(1).text = "Fixed carbon"
    MinName(2).text = "Volatiles"
    MinName(3).text = "Moisture"
    For I = 4 To 6
      MinName(I).Visible = False
    Next I
    Me.OptSGSpec(0).Enabled = False
  Else
    NoMin.text = CStr(2)
    For I = 0 To 1
      MinName(I).Visible = True
    Next I
    MinName(0).text = "Ash "
    MinName(1).text = "Combustibles"
    For I = 2 To 6
      MinName(I).Visible = False
    Next
  End If
End Sub

Private Sub OptMinProcPlant_Click()
'**********************************
  Dim I As Integer
  ChkProximate.Visible = False
  Me.OptSGSpec(0).Enabled = True
  OptSGSpec(0).Value = True
  NGCM.Enabled = True
  Me.NGCM.Visible = True
  Lblngcm.Caption = "Number of grade classes"
  Lblngcm.Enabled = True
  NoMin.Enabled = True
  CmdGradeClasses.Enabled = True
  CmdGradeClasses.Visible = True
    NoMin.text = CStr(CURRENT.Nomins)
    If CURRENT.MineralSG(1) > 0 Then
      OptSGSpec(0).Value = True
    End If
    MinName(0).Visible = True
    For I = 0 To CURRENT.Nomins - 1
      MinName(I).Visible = True
      MinName(I).text = CURRENT.MineralNames(I + 1)
      TxtSG(I).text = CURRENT.MineralSG(I + 1)
    Next I
    For I = CURRENT.Nomins To 6
      MinName(I).Visible = False
      TxtSG(I).Visible = False
    Next
End Sub

Private Sub OptSGSpec_Click(Index As Integer)
'********************************************
  Dim N As Integer
  If Not loadingForm Then
      If Index = 0 Then
        LblSGSpec.Visible = True
        LblSGSpec.Enabled = True
        TxtSG(0).Visible = True         'In case NoMin = 0
        If Not Me.OptCoalProcPlant.Value Then CmdGradeClasses.Enabled = True   'To enable access to class sp.gr if necessary
        For N = 1 To Val(NoMin.text)
          TxtSG(N - 1).Visible = True
          TxtSG(N - 1).text = CStr(CURRENT.MineralSG(N))
        Next N
      Else
        LblSGSpec.Visible = False
        TxtSG(0).Visible = False   'In case NoMin =0
        For N = 1 To 7
          TxtSG(N - 1).Visible = False
          TxtSG(N - 1).text = ""
        Next N
      End If
  End If
End Sub

Private Sub OptSGSpec_GotFocus(Index As Integer)
'***********************************************
  Instruction.Visible = True
  Instruction.Caption = "Specify the specific gravity of the minerals or of the particle types"
End Sub

Private Sub OptSGSpec_LostFocus(Index As Integer)
'************************************************
  Instruction.Visible = False
  Instruction.Caption = ""
End Sub


Private Sub OStreamList_DblClick()
'*********************************
  Dim N As Single
  IND = OStreamList.ListIndex
  If IND >= 0 Then
    NSTR = Val(OStreamList.text)
    For N = 1 To Nstream
      If Stream(N) = NSTR Then
        ID = STREAM_ID(N)
        Exit For
      End If
    Next N
    OSTREAM.Top = DisplayOffsetY + 200
    OSTREAM.Left = DisplayOffsetX + 200
    OSTREAM.Show 1
  End If
End Sub

Private Sub OStreamList_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Double click to specify size distribution for comparison on output"
End Sub

Private Sub OStreamList_KeyDown(KeyCode As Integer, Shift As Integer)
'********************************************************************
  Dim KEY_SPACE As Integer, KEY_RETURN As Integer
  KEY_SPACE = 32
  KEY_RETURN = 13
  If (KeyCode = KEY_SPACE Or KeyCode = KEY_RETURN) Then
    Call OStreamList_DblClick
  End If
End Sub

Private Sub OStreamList_LostFocus()
  Instruction.Visible = 0
End Sub

Public Sub StreamList_DblClick()
'********************************
  'Dim NSTR As Integer
  Dim N As Integer
  'Check that class dimesions are within bounds
  If Val(NDCM.text) > MaximumSizeClasses Then
    MsgBox "The maximum number of size classes allowed is " & Format(MaximumSizeClasses, "#00"), 16, "ERROR"
    NDCM.SetFocus
    Exit Sub
  End If
  If Val(NGCM.text) > MaximumGClasses Then
    MsgBox "The maximum number of grade classes allowed is " & Format(MaximumGClasses, "#00"), 16, "ERROR"
    NGCM.SetFocus
    Exit Sub
  End If
  If Val(NSCM.text) > MaximumSClasses Then
    MsgBox "The maximum number of S classes allowed is " & Format(MaximumSClasses, "#00"), 16, "ERROR"
    NSCM.SetFocus
    Exit Sub
  End If
  
  'Check that mineral data has been specified
  If Val(NoMin.text) <= 0 Or MinName(0).text = "" Then
    MsgBox "Mineral data must be specified before attempting to define the feed stream", vbExclamation, "ERROR"
    Exit Sub
  End If
  
  IND = StreamList.ListIndex
  If IND >= 0 Then
    NSTR = Val(StreamList.text)
    If NSTR = 0 Then
      MsgBox "Attempting to load a stream with number 0" & vbCrLf & StreamList.text, vbExclamation, "DIAGNOSTIC"
      Exit Sub
    End If
    For N = 1 To Nstream
      If Stream(N) = NSTR Then
        ID = STREAM_ID(N)
        Exit For
      End If
    Next N
    Psd.Top = DisplayOffsetY + 200
    Psd.Left = DisplayOffsetX + 200
    Psd.Show 1
  End If
End Sub

Private Sub StreamList_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Double click to specify flowrate and size distribution"
End Sub

Private Sub StreamList_KeyDown(KeyCode As Integer, Shift As Integer)
'*******************************************************************
  Dim KEY_SPACE As Integer, KEY_RETURN As Integer
  KEY_SPACE = 32
  KEY_RETURN = 13
  If (KeyCode = KEY_SPACE Or KeyCode = KEY_RETURN) Then
    Call StreamList_DblClick
  End If
End Sub

Private Sub StreamList_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub TxtSG_GotFocus(Index As Integer)
'*******************************************
  Instruction.Visible = True
  Instruction.Caption = "Specify the specific gravity of this mineral"

End Sub

Private Sub TxtSG_KeyPress(Index As Integer, KeyAscii As Integer)
'****************************************************************
FORM_CHANGE_FLAG = True
If KeyAscii = 13 Then
  If Index < Val(NoMin.text) - 1 Then
    If MinName(Index + 1).text = "" Then
      MinName(Index + 1).SetFocus
    Else
      TxtSG(Index + 1).Visible = True
      TxtSG(Index + 1).SetFocus
    End If
  End If
  KeyAscii = 0
End If
End Sub


Private Sub TxtSG_LostFocus(Index As Integer)
'********************************************
  Instruction.Caption = ""
  Instruction.Visible = False
  Me.OptSGSpec(0).Value = True
End Sub

Public Sub WStreamList_DblClick()
'*********************************
  Dim N As Integer
  IND = WStreamList.ListIndex
  ID = 0
  If IND >= 0 Then
    NSTR = Val(WStreamList.text)
    For N = 1 To Nstream
      If Stream(N) = NSTR Then
        ID = STREAM_ID(N)
        Exit For
      End If
    Next N
    WUNIT = To_unit(N)
  End If
  Wstream.Top = DisplayOffsetY + 200
  Wstream.Left = DisplayOffsetY + 200
  Wstream.Show 1
End Sub

Private Sub WStreamList_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "Double click to select water feed stream to specify water rate or % solids in unit product"
End Sub


Private Sub WStreamList_KeyDown(KeyCode As Integer, Shift As Integer)
'********************************************************************
  Dim KEY_SPACE As Integer, KEY_RETURN As Integer
  KEY_SPACE = 32
  KEY_RETURN = 13
  If (KeyCode = KEY_SPACE Or KeyCode = KEY_RETURN) Then
    Call WStreamList_DblClick
  End If
End Sub

Private Sub WStreamList_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub MakeNew()
'********************
  Dim N As Integer
    For N = 1 To CURRENT.Nomins
      CURRENT.MineralSG(N) = Val(TxtSG(N - 1).text)
    Next N
End Sub
