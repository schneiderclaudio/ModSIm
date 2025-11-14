VERSION 5.00
Begin VB.Form Washdata 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Washability data for a single size fraction"
   ClientHeight    =   7275
   ClientLeft      =   75
   ClientTop       =   1995
   ClientWidth     =   9090
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
   KeyPreview      =   -1  'True
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7275
   ScaleWidth      =   9090
   Begin VB.CommandButton cmdAM_D 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify liberation model"
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
      Left            =   3840
      TabIndex        =   226
      Top             =   6720
      Width           =   2535
   End
   Begin VB.CommandButton CmdPreviousSizeRange 
      Caption         =   "Previous size range"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   300
      Left            =   240
      TabIndex        =   224
      Top             =   1125
      Width           =   1935
   End
   Begin VB.CommandButton CmdNextSizeRange 
      Caption         =   "Next size range"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   300
      Left            =   2400
      TabIndex        =   223
      Top             =   1125
      Width           =   1935
   End
   Begin VB.Frame FraUnitsCalValue 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for calorific value"
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
      Left            =   3600
      TabIndex        =   219
      Top             =   5880
      Width           =   2865
      Begin VB.CommandButton CmdCVConvert 
         Caption         =   ">"
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
         Left            =   1200
         TabIndex        =   225
         Top             =   240
         Width           =   375
      End
      Begin VB.OptionButton OptUnitsCalValue 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Btu/lb"
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
         Left            =   1680
         TabIndex        =   221
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton OptUnitsCalValue 
         BackColor       =   &H00C0C0C0&
         Caption         =   "MJ/kg"
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
         TabIndex        =   220
         Top             =   240
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   14
      Left            =   7995
      TabIndex        =   217
      Top             =   5640
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   13
      Left            =   7995
      TabIndex        =   216
      Top             =   5400
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   12
      Left            =   7995
      TabIndex        =   215
      Top             =   5160
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   11
      Left            =   7995
      TabIndex        =   214
      Top             =   4920
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   10
      Left            =   7995
      TabIndex        =   213
      Top             =   4680
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   9
      Left            =   7995
      TabIndex        =   212
      Top             =   4440
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   8
      Left            =   7995
      TabIndex        =   211
      Top             =   4200
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   7
      Left            =   7995
      TabIndex        =   210
      Top             =   3960
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   6
      Left            =   7995
      TabIndex        =   209
      Top             =   3720
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   5
      Left            =   7995
      TabIndex        =   208
      Top             =   3480
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   4
      Left            =   7995
      TabIndex        =   207
      Top             =   3240
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   3
      Left            =   7995
      TabIndex        =   206
      Top             =   3000
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   2
      Left            =   7995
      TabIndex        =   205
      Top             =   2760
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   1
      Left            =   7995
      TabIndex        =   204
      Top             =   2520
      Width           =   950
   End
   Begin VB.TextBox PyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   0
      Left            =   7995
      TabIndex        =   203
      Top             =   2280
      Width           =   950
   End
   Begin VB.CheckBox ChkPyrSulfur 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Pyritic Sulfur"
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
      Left            =   7560
      TabIndex        =   202
      Top             =   1680
      Width           =   1455
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   14
      Left            =   1575
      TabIndex        =   200
      Top             =   5640
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   13
      Left            =   1575
      TabIndex        =   199
      Top             =   5400
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   12
      Left            =   1575
      TabIndex        =   198
      Top             =   5160
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   11
      Left            =   1575
      TabIndex        =   197
      Top             =   4920
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   10
      Left            =   1575
      TabIndex        =   196
      Top             =   4680
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   9
      Left            =   1575
      TabIndex        =   195
      Top             =   4440
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   8
      Left            =   1575
      TabIndex        =   194
      Top             =   4200
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   7
      Left            =   1575
      TabIndex        =   193
      Top             =   3960
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   6
      Left            =   1575
      TabIndex        =   192
      Top             =   3720
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   5
      Left            =   1575
      TabIndex        =   191
      Top             =   3480
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   4
      Left            =   1575
      TabIndex        =   190
      Top             =   3240
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   3
      Left            =   1575
      TabIndex        =   189
      Top             =   3000
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   2
      Left            =   1575
      TabIndex        =   188
      Top             =   2760
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   1
      Left            =   1575
      TabIndex        =   187
      Top             =   2520
      Width           =   700
   End
   Begin VB.TextBox Spgr 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   230
      Index           =   0
      Left            =   1575
      TabIndex        =   186
      Top             =   2280
      Width           =   700
   End
   Begin VB.CheckBox ChkSulfur 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Total Sulfur"
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
      Left            =   6000
      TabIndex        =   70
      Top             =   1680
      Width           =   1215
   End
   Begin VB.CheckBox ChkCalVal 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Calorific value"
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
      Left            =   6000
      TabIndex        =   69
      Top             =   1440
      Width           =   1455
   End
   Begin VB.CheckBox ChkMoist 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Moisture"
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
      Left            =   4920
      TabIndex        =   68
      Top             =   1680
      Width           =   1080
   End
   Begin VB.CheckBox ChkVolatiles 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Volatiles"
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
      Left            =   4920
      TabIndex        =   67
      Top             =   1440
      Width           =   1080
   End
   Begin VB.CheckBox ChkFixedC 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fixed C"
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
      Left            =   3960
      TabIndex        =   66
      Top             =   1560
      Width           =   960
   End
   Begin VB.CommandButton CmdAccept 
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
      Height          =   495
      Left            =   7920
      TabIndex        =   8
      Top             =   6000
      Width           =   960
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data specified as"
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
      Left            =   120
      TabIndex        =   183
      Top             =   5880
      Width           =   2745
      Begin VB.OptionButton OptFract 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Fractional"
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
         TabIndex        =   184
         Top             =   240
         Value           =   -1  'True
         Width           =   1080
      End
      Begin VB.OptionButton OptCumulative 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Cumulative"
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
         Height          =   240
         Left            =   1440
         TabIndex        =   185
         TabStop         =   0   'False
         Top             =   240
         Width           =   1215
      End
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   2325
      TabIndex        =   90
      Top             =   5640
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   2325
      TabIndex        =   89
      Top             =   5400
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   2325
      TabIndex        =   88
      Top             =   5160
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   2325
      TabIndex        =   87
      Top             =   4920
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   2325
      TabIndex        =   86
      Top             =   4680
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   2325
      TabIndex        =   85
      Top             =   4440
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   2325
      TabIndex        =   84
      Top             =   4200
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   2325
      TabIndex        =   83
      Top             =   3960
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   2325
      TabIndex        =   82
      Top             =   3720
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   2325
      TabIndex        =   81
      Top             =   3480
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   2325
      TabIndex        =   80
      Top             =   3240
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   2325
      TabIndex        =   79
      Top             =   3000
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   2325
      TabIndex        =   78
      Top             =   2760
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   2325
      TabIndex        =   77
      Top             =   2520
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Fraction 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   2325
      TabIndex        =   76
      Top             =   2280
      Width           =   700
   End
   Begin VB.TextBox DataFile 
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
      Height          =   240
      Left            =   3360
      TabIndex        =   12
      Top             =   480
      Width           =   3975
   End
   Begin VB.TextBox Number 
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
      Height          =   240
      Left            =   2520
      TabIndex        =   182
      Top             =   1560
      Width           =   600
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   960
      TabIndex        =   15
      Tag             =   "Density"
      Top             =   2280
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   960
      TabIndex        =   30
      Tag             =   "Density"
      Top             =   5640
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   960
      TabIndex        =   29
      Tag             =   "Density"
      Top             =   5400
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   960
      TabIndex        =   28
      Tag             =   "Density"
      Top             =   5160
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   960
      TabIndex        =   27
      Tag             =   "Density"
      Top             =   4920
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   960
      TabIndex        =   26
      Tag             =   "Density"
      Top             =   4680
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   960
      TabIndex        =   25
      Tag             =   "Density"
      Top             =   4440
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   960
      TabIndex        =   24
      Tag             =   "Density"
      Top             =   4200
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   960
      TabIndex        =   23
      Tag             =   "Density"
      Top             =   3960
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   960
      TabIndex        =   22
      Tag             =   "Density"
      Top             =   3720
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   960
      TabIndex        =   21
      Tag             =   "Density"
      Top             =   3480
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   960
      TabIndex        =   20
      Tag             =   "Density"
      Top             =   3240
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   960
      TabIndex        =   19
      Tag             =   "Density"
      Top             =   3000
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   960
      TabIndex        =   18
      Tag             =   "Density"
      Top             =   2760
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Density 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   960
      TabIndex        =   17
      Tag             =   "Density"
      Top             =   2520
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   3075
      TabIndex        =   95
      Top             =   3240
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   3075
      TabIndex        =   91
      Top             =   2280
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   3075
      TabIndex        =   105
      Top             =   5640
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   3075
      TabIndex        =   104
      Top             =   5400
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   3075
      TabIndex        =   103
      Top             =   5160
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   3075
      TabIndex        =   102
      Top             =   4920
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   3075
      TabIndex        =   101
      Top             =   4680
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   3075
      TabIndex        =   100
      Top             =   4440
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   3075
      TabIndex        =   99
      Top             =   4200
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   3075
      TabIndex        =   98
      Top             =   3960
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   3075
      TabIndex        =   97
      Top             =   3720
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   3075
      TabIndex        =   96
      Top             =   3480
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   3075
      TabIndex        =   94
      Top             =   3000
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   3075
      TabIndex        =   93
      Top             =   2760
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Ash 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   3075
      TabIndex        =   92
      Top             =   2520
      Visible         =   0   'False
      Width           =   650
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   240
      TabIndex        =   44
      Tag             =   "Density"
      Top             =   5640
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   240
      TabIndex        =   43
      Tag             =   "Density"
      Top             =   5400
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   240
      TabIndex        =   42
      Tag             =   "Density"
      Top             =   5160
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   240
      TabIndex        =   41
      Tag             =   "Density"
      Top             =   4920
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   240
      TabIndex        =   40
      Tag             =   "Density"
      Top             =   4680
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   240
      TabIndex        =   39
      Tag             =   "Density"
      Top             =   4440
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   240
      TabIndex        =   38
      Tag             =   "Density"
      Top             =   4200
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   240
      TabIndex        =   37
      Tag             =   "Density"
      Top             =   3960
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   240
      TabIndex        =   36
      Tag             =   "Density"
      Top             =   3720
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   240
      TabIndex        =   35
      Tag             =   "Density"
      Top             =   3480
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   240
      TabIndex        =   34
      Tag             =   "Density"
      Top             =   3240
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   240
      TabIndex        =   33
      Tag             =   "Density"
      Top             =   3000
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   240
      TabIndex        =   32
      Tag             =   "Density"
      Top             =   2760
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Densu 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   240
      TabIndex        =   31
      Tag             =   "Density"
      Top             =   2520
      Visible         =   0   'False
      Width           =   550
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   7245
      TabIndex        =   180
      Tag             =   "Sulfur"
      Top             =   5640
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   7245
      TabIndex        =   179
      Tag             =   "Sulfur"
      Top             =   5400
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   7245
      TabIndex        =   178
      Tag             =   "Sulfur"
      Top             =   5160
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   7245
      TabIndex        =   177
      Tag             =   "Sulfur"
      Top             =   4920
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   7245
      TabIndex        =   176
      Tag             =   "Sulfur"
      Top             =   4680
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   7245
      TabIndex        =   175
      Tag             =   "Sulfur"
      Top             =   4440
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   7245
      TabIndex        =   174
      Tag             =   "Sulfur"
      Top             =   4200
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   7245
      TabIndex        =   173
      Tag             =   "Sulfur"
      Top             =   3960
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   7245
      TabIndex        =   172
      Tag             =   "Sulfur"
      Top             =   3720
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   7245
      TabIndex        =   171
      Tag             =   "Sulfur"
      Top             =   3480
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   7245
      TabIndex        =   170
      Tag             =   "Sulfur"
      Top             =   3240
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   7245
      TabIndex        =   169
      Tag             =   "Sulfur"
      Top             =   3000
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   7245
      TabIndex        =   168
      Tag             =   "Sulfur"
      Top             =   2760
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   7245
      TabIndex        =   167
      Tag             =   "Sulfur"
      Top             =   2520
      Visible         =   0   'False
      Width           =   700
   End
   Begin VB.TextBox Sulfur 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   7245
      TabIndex        =   166
      Tag             =   "Sulfur"
      Top             =   2280
      Width           =   700
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   6405
      TabIndex        =   165
      Tag             =   "CValue"
      Top             =   5640
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   6405
      TabIndex        =   164
      Tag             =   "CValue"
      Top             =   5400
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   6405
      TabIndex        =   163
      Tag             =   "CValue"
      Top             =   5160
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   6405
      TabIndex        =   162
      Tag             =   "CValue"
      Top             =   4920
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   6405
      TabIndex        =   161
      Tag             =   "CValue"
      Top             =   4680
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   6405
      TabIndex        =   160
      Tag             =   "CValue"
      Top             =   4440
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   6405
      TabIndex        =   159
      Tag             =   "CValue"
      Top             =   4200
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   6405
      TabIndex        =   158
      Tag             =   "CValue"
      Top             =   3960
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   6405
      TabIndex        =   157
      Tag             =   "CValue"
      Top             =   3720
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   6405
      TabIndex        =   156
      Tag             =   "CValue"
      Top             =   3480
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   6405
      TabIndex        =   155
      Tag             =   "CValue"
      Top             =   3240
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   6405
      TabIndex        =   154
      Tag             =   "CValue"
      Top             =   3000
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   6405
      TabIndex        =   153
      Tag             =   "CValue"
      Top             =   2760
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   6405
      TabIndex        =   152
      Tag             =   "CValue"
      Top             =   2520
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox CValue 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   6405
      TabIndex        =   151
      Tag             =   "CValue"
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   5505
      TabIndex        =   150
      Tag             =   "Moisture"
      Top             =   5640
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   5505
      TabIndex        =   149
      Tag             =   "Moisture"
      Top             =   5400
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   5505
      TabIndex        =   148
      Tag             =   "Moisture"
      Top             =   5160
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   5505
      TabIndex        =   147
      Tag             =   "Moisture"
      Top             =   4920
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   5505
      TabIndex        =   146
      Tag             =   "Moisture"
      Top             =   4680
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   5505
      TabIndex        =   145
      Tag             =   "Moisture"
      Top             =   4440
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   5505
      TabIndex        =   144
      Tag             =   "Moisture"
      Top             =   4200
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   5505
      TabIndex        =   143
      Tag             =   "Moisture"
      Top             =   3960
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   5505
      TabIndex        =   142
      Tag             =   "Moisture"
      Top             =   3720
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   5505
      TabIndex        =   141
      Tag             =   "Moisture"
      Top             =   3480
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   5505
      TabIndex        =   140
      Tag             =   "Moisture"
      Top             =   3240
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   5505
      TabIndex        =   139
      Tag             =   "Moisture"
      Top             =   3000
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   5505
      TabIndex        =   138
      Tag             =   "Moisture"
      Top             =   2760
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   5505
      TabIndex        =   137
      Tag             =   "Moisture"
      Top             =   2520
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Moisture 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   5505
      TabIndex        =   136
      Tag             =   "Moisture"
      Top             =   2280
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   4605
      TabIndex        =   135
      Tag             =   "Volatiles"
      Top             =   5640
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   4605
      TabIndex        =   134
      Tag             =   "Volatiles"
      Top             =   5400
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   4605
      TabIndex        =   133
      Tag             =   "Volatiles"
      Top             =   5160
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   4605
      TabIndex        =   132
      Tag             =   "Volatiles"
      Top             =   4920
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   4605
      TabIndex        =   131
      Tag             =   "Volatiles"
      Top             =   4680
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   4605
      TabIndex        =   130
      Tag             =   "Volatiles"
      Top             =   4440
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   4605
      TabIndex        =   129
      Tag             =   "Volatiles"
      Top             =   4200
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   4605
      TabIndex        =   128
      Tag             =   "Volatiles"
      Top             =   3960
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   4605
      TabIndex        =   127
      Tag             =   "Volatiles"
      Top             =   3720
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   4605
      TabIndex        =   126
      Tag             =   "Volatiles"
      Top             =   3480
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   4605
      TabIndex        =   125
      Tag             =   "Volatiles"
      Top             =   3240
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   4605
      TabIndex        =   124
      Tag             =   "Volatiles"
      Top             =   3000
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   4605
      TabIndex        =   123
      Tag             =   "Volatiles"
      Top             =   2760
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   4605
      TabIndex        =   122
      Tag             =   "Volatiles"
      Top             =   2520
      Visible         =   0   'False
      Width           =   850
   End
   Begin VB.TextBox Volatiles 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   4605
      TabIndex        =   121
      Tag             =   "Volatiles"
      Top             =   2280
      Width           =   850
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   1
      Left            =   3765
      TabIndex        =   107
      Tag             =   "FixedC"
      Top             =   2520
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   0
      Left            =   3765
      TabIndex        =   106
      Tag             =   "FixedC"
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   13
      Left            =   3765
      TabIndex        =   119
      Tag             =   "FixedC"
      Top             =   5400
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   14
      Left            =   3765
      TabIndex        =   120
      Tag             =   "FixedC"
      Top             =   5640
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   12
      Left            =   3765
      TabIndex        =   118
      Tag             =   "FixedC"
      Top             =   5160
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   11
      Left            =   3765
      TabIndex        =   117
      Tag             =   "FixedC"
      Top             =   4920
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   10
      Left            =   3765
      TabIndex        =   116
      Tag             =   "FixedC"
      Top             =   4680
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   9
      Left            =   3765
      TabIndex        =   115
      Tag             =   "FixedC"
      Top             =   4440
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   8
      Left            =   3765
      TabIndex        =   114
      Tag             =   "FixedC"
      Top             =   4200
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   7
      Left            =   3765
      TabIndex        =   113
      Tag             =   "FixedC"
      Top             =   3960
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   6
      Left            =   3765
      TabIndex        =   112
      Tag             =   "FixedC"
      Top             =   3720
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   5
      Left            =   3765
      TabIndex        =   111
      Tag             =   "FixedC"
      Top             =   3480
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   4
      Left            =   3765
      TabIndex        =   110
      Tag             =   "FixedC"
      Top             =   3240
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   3
      Left            =   3765
      TabIndex        =   109
      Tag             =   "FixedC"
      Top             =   3000
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox FixedC 
      Alignment       =   1  'Right Justify
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
      Height          =   230
      Index           =   2
      Left            =   3765
      TabIndex        =   108
      Tag             =   "FixedC"
      Top             =   2760
      Visible         =   0   'False
      Width           =   800
   End
   Begin VB.TextBox SizeU 
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
      Height          =   240
      Left            =   1320
      TabIndex        =   1
      Top             =   840
      Width           =   840
   End
   Begin VB.TextBox SizeL 
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
      Height          =   240
      Left            =   2400
      TabIndex        =   3
      Top             =   840
      Width           =   840
   End
   Begin VB.OptionButton OptSizeUnits 
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
      Height          =   240
      Index           =   0
      Left            =   3480
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   840
      Value           =   -1  'True
      Width           =   615
   End
   Begin VB.OptionButton OptSizeUnits 
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
      Height          =   240
      Index           =   1
      Left            =   4200
      TabIndex        =   10
      TabStop         =   0   'False
      Top             =   840
      Width           =   615
   End
   Begin VB.OptionButton OptSizeUnits 
      BackColor       =   &H00C0C0C0&
      Caption         =   "microns"
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
      Left            =   5040
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   840
      Width           =   975
   End
   Begin VB.OptionButton OptSizeUnits 
      BackColor       =   &H00C0C0C0&
      Caption         =   "mesh"
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
      Height          =   240
      Index           =   3
      Left            =   6240
      TabIndex        =   6
      Top             =   840
      Width           =   720
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
      Left            =   7440
      TabIndex        =   75
      Top             =   360
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
         Height          =   240
         Left            =   120
         TabIndex        =   72
         Tag             =   "New"
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
         Height          =   240
         Left            =   120
         TabIndex        =   73
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1215
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
         Height          =   240
         Left            =   120
         TabIndex        =   74
         TabStop         =   0   'False
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.CommandButton CmdCancel 
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
      Height          =   495
      Left            =   6600
      TabIndex        =   9
      Top             =   6000
      Width           =   960
   End
   Begin VB.Label LblJobName 
      BackColor       =   &H00C0C0C0&
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
      Left            =   600
      TabIndex        =   222
      Top             =   480
      Width           =   1575
   End
   Begin VB.Label LblPyrSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Pyr Sulfur %"
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
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   7995
      TabIndex        =   218
      Top             =   2040
      Width           =   945
   End
   Begin VB.Label LblSpGr 
      Alignment       =   2  'Center
      BackColor       =   &H00C0C0C0&
      Caption         =   "Av. density"
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
      Left            =   1575
      TabIndex        =   201
      Top             =   1800
      Width           =   705
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0FFFF&
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
      TabIndex        =   71
      Top             =   120
      Visible         =   0   'False
      Width           =   8895
   End
   Begin VB.Label LblPercent 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Weight %"
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
      Left            =   2325
      TabIndex        =   45
      Top             =   2040
      Width           =   705
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size range"
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
      TabIndex        =   0
      Top             =   840
      Width           =   1200
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Coal sample"
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
      Left            =   2280
      TabIndex        =   11
      Top             =   480
      Width           =   1095
   End
   Begin VB.Label LblVolatiles 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Volatiles %"
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
      Left            =   4605
      TabIndex        =   48
      Top             =   2040
      Width           =   855
   End
   Begin VB.Label LblFixedC 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fixed C %"
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
      Left            =   3765
      TabIndex        =   47
      Top             =   2040
      Width           =   795
   End
   Begin VB.Label LblAsh 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ash %"
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
      Left            =   3075
      TabIndex        =   46
      Top             =   2040
      Width           =   645
   End
   Begin VB.Label LblSulfur 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Tot S %"
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
      Left            =   7245
      TabIndex        =   51
      Top             =   2040
      Width           =   705
   End
   Begin VB.Label LblCalVal 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cal. value"
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
      Left            =   6405
      TabIndex        =   50
      Top             =   2040
      Width           =   795
   End
   Begin VB.Label LblMoist 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Moisture %"
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
      Left            =   5505
      TabIndex        =   49
      Top             =   2040
      Width           =   855
   End
   Begin VB.Label Label16 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of washability classes"
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
      TabIndex        =   181
      Top             =   1560
      Width           =   2295
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Float"
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
      Left            =   240
      TabIndex        =   14
      Top             =   2280
      Width           =   555
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " x"
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
      Left            =   2160
      TabIndex        =   2
      Top             =   840
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   16
      Top             =   2280
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   61
      Top             =   4680
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   57
      Top             =   3720
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   65
      Top             =   5640
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   64
      Top             =   5400
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Index           =   12
      Left            =   720
      TabIndex        =   63
      Top             =   5160
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Index           =   11
      Left            =   720
      TabIndex        =   62
      Top             =   4920
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   60
      Top             =   4440
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   59
      Top             =   4200
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   58
      Top             =   3960
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   56
      Top             =   3480
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   55
      Top             =   3240
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   54
      Top             =   3000
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   53
      Top             =   2760
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label Label8 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   " - "
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
      Left            =   720
      TabIndex        =   52
      Top             =   2520
      Visible         =   0   'False
      Width           =   255
   End
   Begin VB.Label LblDensity 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Density"
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
      Left            =   360
      TabIndex        =   13
      Top             =   2040
      Width           =   1065
   End
   Begin VB.Label Label4 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Job:"
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
      TabIndex        =   7
      Top             =   480
      Width           =   495
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
Attribute VB_Name = "Washdata"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private NO_GRADE_RANGES As Integer, NGC As Integer
Private SELECTIND As Integer, ACTIVE_K As Integer
'Only 20 size ranges to group grade distributions or washabilities.
Private NEWGDATA(1 To 20) As GRADEDATA
Private AncilliaryData(1 To 20) As WashabilityData
Private CHANGE_FLAG As Integer
Private NEWCDATA As CLASSDATA
Private Nomins As Integer

Private Sub MakeNew(NNDATA As GRADEDATA, NDATA As CLASSDATA, NWDATA As WashabilityData)
'***********************************************************
Dim I As Integer, I1 As Integer, I2 As Integer
Dim N As Integer, M As Integer
Dim K As Integer
Dim SizeFactor As Single
  CHANGE_FLAG = 0
  NDATA.SPECIFIED = -1
  NDATA.NGC = Val(Number.text)
  Sysdata.NGCM.text = Number.text
  NDATA.Nomins = Val(Sysdata.NoMin.text)
  NDATA.MassVol = "GRDM"
  For N = 1 To NDATA.NGC
    For M = 1 To NDATA.Nomins
      If M = 1 Then NDATA.GRDMV(N, M) = 0.01 * Val(Ash(N - 1).text)
      If M = 2 Then
        If Sysdata.ChkProximate.Value = vbUnchecked Then
          NDATA.GRDMV(N, M) = 1# - 0.01 * Val(Ash(N - 1).text)
        Else
          NDATA.GRDMV(N, M) = 0.01 * Val(FixedC(N - 1).text)
        End If
      End If
      If M = 3 Then NDATA.GRDMV(N, M) = 0.01 * Val(Volatiles(N - 1).text)
      If M = 4 Then NDATA.GRDMV(N, M) = 0.01 * Val(Moisture(N - 1).text)
    Next M
  Next N
  If Sysdata.OptSGSpec(0).Value = True Then
    For N = 1 To NDATA.Nomins
      NDATA.MineralSG(N) = Val(Sysdata.TxtSG(N - 1).text)
    Next N
  End If
  If Sysdata.OptSGSpec(1).Value = True And Val(Spgr(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 1, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(1, 1) - 1) = Val(Spgr(N - 1).text)
    Next N
  End If
  
  'Set the coal flags to go to SIMOP
  Mid(CURRENT.Flags, 1, 1) = "T"
  If Sysdata.ChkProximate.Value = 1 Then
    Mid(CURRENT.Flags, 2, 1) = "T"
  Else
    Mid(CURRENT.Flags, 2, 1) = "F"
  End If
  If ChkCalVal.Value = 1 Then
    Mid(CURRENT.Flags, 3, 1) = "T"
  Else
    Mid(CURRENT.Flags, 3, 1) = "F"
  End If
  If ChkSulfur.Value = 1 Then
    Mid(CURRENT.Flags, 4, 1) = "T"
  Else
    Mid(CURRENT.Flags, 4, 1) = "F"
  End If
  If ChkPyrSulfur.Value = 1 Then
    Mid(CURRENT.Flags, 5, 1) = "T"
  Else
    Mid(CURRENT.Flags, 5, 1) = "F"
  End If
  
  If ChkCalVal.Value = 1 And Val(CValue(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 8, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(8, 1) - 1) = Val(CValue(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 8, NDATA.NGC)
  End If
  If ChkSulfur.Value = 1 And Val(Sulfur(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 9, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(9, 1) - 1) = Val(Sulfur(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 9, NDATA.NGC)
  End If
  If ChkPyrSulfur.Value = 1 And Val(PyrSulfur(0).text) > 0 Then
    Call ADD_TO_PPROP(NDATA, 10, NDATA.NGC)
    For N = 1 To NDATA.NGC
      NDATA.PPROP(N + NDATA.INDPP(10, 1) - 1) = Val(PyrSulfur(N - 1).text)
    Next N
  Else
    Call REMOVE_FROM_PPROP(NDATA, 10, NDATA.NGC)
  End If
  
'Now the Grade data
  NNDATA.NGC = Val(Number.text)
  NNDATA.STREAM_NAME = Psd.StreamName.text
  SizeFactor = 0.001
  If OptSizeUnits(1) Then SizeFactor = 0.0254
  If OptSizeUnits(2) Then SizeFactor = 0.000001
  NNDATA.SIZE_RANGE(2) = SizeFactor * Val(SizeU.text)
  NNDATA.SIZE_RANGE(1) = SizeFactor * Val(SizeL.text)
  NNDATA.NSTR = Val(Psd.Stream.Caption)
  For I = 1 To NNDATA.NGC
    NNDATA.Fraction(I) = 0.01 * Val(Fraction(I - 1).text)
  Next I

  'Record the density ranges as ancilliary data
  NWDATA.NGC = Val(Number.text)
  NWDATA.LowerDensity(1) = Label7.Caption
  For I = 1 To NWDATA.NGC - 1
    NWDATA.LowerDensity(I + 1) = Densu(I - 1).text
    NWDATA.UpperDensity(I) = Density(I - 1).text
  Next I
  NWDATA.UpperDensity(NWDATA.NGC) = Density(NWDATA.NGC - 1).text
End Sub

Private Sub Ash_Change(Index As Integer)
'***************************************
  CHANGE_FLAG = -1
End Sub

Private Sub Ash_KeyPress(Index As Integer, KeyAscii As Integer)
'**************************************************************
  If KeyAscii = 13 Then
    If Index > 0 Then
      If Val(Ash(Index).text) < Val(Ash(Index - 1).text) Then
        MsgBox "Ash should increase", 16, "ERROR"
        Ash(Index).SetFocus
        KeyAscii = 0
        Exit Sub
      End If
    End If
    If Index < Val(Number.text) - 1 Then
      Ash(Index + 1).text = ""
      Ash(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub Ash_LostFocus(Index As Integer)
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkCalVal_Click()
'****************************
  Dim I As Integer
  CHANGE_FLAG = -1
  If ChkCalVal.Value = 1 Then
    FraUnitsCalValue.Enabled = True
    LblCalVal.Enabled = True
    For I = 1 To Val(Number.text)
      CValue(I - 1).Visible = True
      CValue(I - 1).Enabled = True
    Next I
  Else
    LblCalVal.Enabled = False
    FraUnitsCalValue.Enabled = False
    For I = 1 To 15
      CValue(I - 1).Visible = False
      CValue(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkCalVal_LostFocus()
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkFixedC_Click()
'****************************
Dim I As Integer
  CHANGE_FLAG = -1
  If ChkFixedC.Value = 1 Then
    LblFixedC.Enabled = True
    For I = 1 To Val(Number.text)
      FixedC(I - 1).Visible = True
      FixedC(I - 1).Enabled = True
    Next I
  Else
    LblFixedC.Enabled = False
    For I = 1 To 15
      FixedC(I - 1).Visible = False
      FixedC(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkFixedC_LostFocus()
'********************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkMoist_Click()
'***************************
  Dim I As Integer
  CHANGE_FLAG = -1
  If ChkMoist.Value = 1 Then
    LblMoist.Enabled = True
    For I = 1 To Val(Number.text)
      Moisture(I - 1).Visible = True
      Moisture(I - 1).Enabled = True
    Next I
  Else
    LblMoist.Enabled = False
    For I = 1 To 15
      Moisture(I - 1).Visible = False
      Moisture(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkMoist_LostFocus()
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkPyrSulfur_Click()
'****************************
  Dim I As Integer
  CHANGE_FLAG = -1
  If ChkPyrSulfur.Value = 1 Then
    LblPyrSulfur.Enabled = True
    For I = 1 To Val(Number.text)
      PyrSulfur(I - 1).Visible = True
      PyrSulfur(I - 1).Enabled = True
    Next I
  Else
    LblPyrSulfur.Enabled = False
    For I = 1 To 15
      PyrSulfur(I - 1).Visible = False
      PyrSulfur(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkPyrSulfur_LostFocus()
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkSulfur_Click()
'****************************
  Dim I As Integer
  CHANGE_FLAG = -1
  If ChkSulfur.Value = 1 Then
    LblSulfur.Enabled = True
    For I = 1 To Val(Number.text)
      Sulfur(I - 1).Visible = True
      Sulfur(I - 1).Enabled = True
    Next I
  Else
    LblSulfur.Enabled = False
    For I = 1 To 15
      Sulfur(I - 1).Visible = False
      Sulfur(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkSulfur_LostFocus()
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub ChkVolatiles_Click()
'*******************************
  Dim I As Integer
  CHANGE_FLAG = -1
  If ChkVolatiles.Value = 1 Then
    LblVolatiles.Enabled = True
    For I = 1 To Val(Number.text)
      Volatiles(I - 1).Visible = True
      Volatiles(I - 1).Enabled = True
    Next I
  Else
    LblVolatiles.Enabled = False
    For I = 1 To 15
      Volatiles(I - 1).Visible = False
      Volatiles(I - 1).Enabled = False
    Next I
  End If
End Sub

Private Sub ChkVolatiles_LostFocus()
'******************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim NGC As Integer
  Dim I As Integer, K As Integer, N As Integer, M As Integer
  Dim sum As Single, Cum As Single
  
  NGC = CURRENT.NGC
  If OptUnitsCalValue(1) Then
    Call ConvertCV(2.326 * 0.001)
    OptUnitsCalValue(0) = True
  End If
    
  Call MakeNew(NEWGDATA(ACTIVE_K), CURRENT, AncilliaryData(ACTIVE_K))
  CURRENT.SPECIFIED = -1
  Sysdata.NGCM.text = CStr(CURRENT.NGC)
  
  'Check the GRDM matrix
  For N = 1 To CURRENT.NGC
    sum = 0#
    For M = 1 To CURRENT.Nomins
      sum = sum + CURRENT.GRDMV(N, M)
    Next M
    If Abs(sum - 1#) >= 0.0001 Then
      Ash(N - 1).SetFocus
      WSTRING = "Sum of grades is " + CStr(sum)
      MsgBox WSTRING, 32, "ERROR"
      Exit Sub
    End If
  Next N
  
  If SizeU.text = "" Or SizeL.text = "" Then
    WSTRING = "Size range has not been specified"
    MsgBox WSTRING, 0, "ERROR"
    Exit Sub
  End If
  CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES = NO_GRADE_RANGES
  For K = 1 To NO_GRADE_RANGES
    ACTIVE_K = K
    Cum = 0!
    For N = 1 To NEWGDATA(K).NGC
      Cum = Cum + NEWGDATA(K).Fraction(N)
    Next N
    If Abs(Cum - 1#) > 0.0001 Then
      Call LoadNew(NEWGDATA(K), NEWCDATA, AncilliaryData(K))
      WSTRING = "Weight % sums to " + CStr(100 * Cum)
      MsgBox WSTRING, 0, "ERROR"
      Exit Sub
    End If
    CURRGDATA(Psd.CURR_INDEX, K) = NEWGDATA(K)
  Next K

  'Save the ancilliary data to file
  JobWashabilityData = True
  Open RTrim(JobPath) & "WASHDATA.OUT" For Output As #31
  Print #31, MODSIM.JOBNAME
  Print #31, NO_GRADE_RANGES
  For K = 1 To NO_GRADE_RANGES
    Print #31, NGC
    For I = 1 To NGC
      Print #31, AncilliaryData(K).LowerDensity(I); "  ";
      Print #31, AncilliaryData(K).UpperDensity(I)
    Next I
  Next K
  Close #31

  Unload Washdata
End Sub

Private Sub cmdAM_D_Click()
'**************************
  'Call MakeNew(CURRENT)
  A_MD.Top = DisplayOffsetY + 400
  A_MD.Left = DisplayOffsetX + 400
  A_MD.Show 1

End Sub

Private Sub CmdCancel_Click()
  Unload Me
End Sub

Private Sub CmdCVConvert_Click()
'*******************************
CHANGE_FLAG = -1
Select Case CmdCVConvert.Caption
  Case ">"
    Call ConvertCV(1000 / 2.326)
    OptUnitsCalValue(1).Value = True
  Case "<"
    Call ConvertCV(2.326 * 0.001)
    OptUnitsCalValue(0).Value = True
End Select
End Sub

Private Sub CmdCVConvert_LostFocus()
'***********************************
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If

End Sub

Private Sub CmdNextSizeRange_Click()
'***********************************
Dim K As Integer
'Don't go past the end
If Val(SizeL.text) = 0 Then Exit Sub
ACTIVE_K = ACTIVE_K + 1
CmdPreviousSizeRange.Enabled = True
If ACTIVE_K > NO_GRADE_RANGES Then
  NO_GRADE_RANGES = ACTIVE_K
  SizeU.text = SizeL.text
  SizeL.text = ""
  For K = 1 To Val(Number)
    Fraction(K - 1).text = ""
  Next K
Else
  Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  If ACTIVE_K > 1 Then
    SizeU.text = CStr(NEWGDATA(ACTIVE_K - 1).SIZE_RANGE(1) * 1000)
  End If
End If
End Sub

Private Sub CmdPreviousSizeRange_Click()
'***************************************
If Val(SizeL.text) <= 0 Then
End If
If ACTIVE_K > 1 Then
  ACTIVE_K = ACTIVE_K - 1
  Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  If ACTIVE_K = 1 Then
    CmdPreviousSizeRange.Enabled = False
  End If
End If
End Sub

Private Sub CValue_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub CValue_KeyPress(Index As Integer, KeyAscii As Integer)
'**************************************************************
  If KeyAscii = 13 Then
    If Index > 0 Then
      If Val(CValue(Index).text) > Val(CValue(Index - 1).text) Then
        MsgBox "Calorific value should decrease", 0, "ERROR"
        CValue(Index).SetFocus
        KeyAscii = 0
        Exit Sub
      End If
    End If
    If Index < Val(Number.text) - 1 Then
      CValue(Index + 1).text = ""
      CValue(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub CValue_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub DataFile_Change()
'****************************
  CHANGE_FLAG = -1
End Sub

Private Sub DataFile_LostFocus()
'*******************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
    Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Density_Change(Index As Integer)
'*******************************************
  If Screen.ActiveControl.Tag = "Density" Then
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Density_KeyPress(Index As Integer, KeyAscii As Integer)
'******************************************************************
  If KeyAscii = 13 Then
    Densu(Index).text = Density(Index).text
    If Index < Val(Number.text) - 2 Then
      Density(Index + 1).text = ""
      Density(Index + 1).SetFocus
    End If
    KeyAscii = 0
    If Index > 0 Then
      If Val(Density(Index).text) < Val(Density(Index - 1).text) Then
        MsgBox "Densities must increase", 16, "ERROR"
        Density(Index).SetFocus
      End If
    End If
  End If
End Sub

Private Sub Density_LostFocus(Index As Integer)
'**********************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Densu_Change(Index As Integer)
'*****************************************
  If Screen.ActiveControl.Tag = "Density" Then
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Densu_KeyPress(Index As Integer, KeyAscii As Integer)
If KeyAscii = 13 Then
  If Index < Val(Number.text) - 1 Then
    Density(Index).text = ""
    Density(Index).SetFocus
  End If
  KeyAscii = 0
End If
End Sub

Private Sub Densu_LostFocus(Index As Integer)
'********************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub FixedC_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub FixedC_KeyPress(Index As Integer, KeyAscii As Integer)
'**************************************************************
  If KeyAscii = 13 Then
    If Index < Val(Number.text) - 1 Then
      FixedC(Index + 1).text = ""
      FixedC(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub FixedC_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Form_Load()
'**********************
Dim K As Integer, I As Integer, Ib As Integer
On Error GoTo ErrorMessage

  CHANGE_FLAG = 0
  ACTIVE_K = 1
  NO_GRADE_RANGES = 1
  LblJobName.Caption = MODSIM.JOBNAME
  SizeU.text = CStr(1000 * Val(Sysdata.D1.text))
  CmdPreviousSizeRange.Enabled = False

  DataFile.text = Psd.Stream.Caption & " " & Psd.StreamName.text
  NGC = Val(Sysdata.NGCM.text)
  
  'Find the current G-class data if any
  If CURRENT.SPECIFIED = 0 Or CURRENT.NGC <> Val(Sysdata.NGCM.text) Then
    OptCurrent.Enabled = False
    LoadDefault
    NEWCDATA.INDPPM = 1
    NEWCDATA.INDPP(1, 1) = 0
    OptNew.Value = -1
  Else
    NEWCDATA = CURRENT
  End If

  If JobWashabilityData Then
    Open RTrim(JobPath) & "WASHDATA.OUT" For Input As 31
    Line Input #31, WSTRING
    If WSTRING <> MODSIM.JOBNAME Then
      MsgBox "Washability data originates from job " & WSTRING, vbExclamation, "WARNING"
    End If
    Input #31, NO_GRADE_RANGES
    For K = 1 To NO_GRADE_RANGES
      Input #31, NGC
      CurrentAncData(K).NGC = NGC
      For I = 1 To NGC
        Input #31, WSTRING
        Ib = InStr(WSTRING, "  ")
        CurrentAncData(K).LowerDensity(I) = Mid(WSTRING, 1, Ib - 1)
        CurrentAncData(K).UpperDensity(I) = Mid(WSTRING, Ib + 2)
      Next I
    Next K
    Close #31
    For K = 1 To NO_GRADE_RANGES
      AncilliaryData(K) = CurrentAncData(K)
    Next K
  End If

  'Find the current G-distribution data for this stream if any
  If Psd.CURR_INDEX = 0 Or CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES = 0 Then
    OptCurrent.Enabled = 0
    LoadDefault
    NO_GRADE_RANGES = 1
    Call MakeNew(NEWGDATA(1), NEWCDATA, AncilliaryData(1))
  Else
    NO_GRADE_RANGES = CURRDATA(Psd.CURR_INDEX).NO_GRADE_RANGES
    For K = 1 To NO_GRADE_RANGES
      NEWGDATA(K) = CURRGDATA(Psd.CURR_INDEX, K)
    Next K
  End If
  SizeU.Enabled = False
  OptNew.Value = -1
  If Sysdata.ChkProximate Then
    Me.ChkFixedC.Value = 1
    Me.ChkFixedC.Enabled = False
    Me.ChkMoist.Value = 1
    Me.ChkMoist.Enabled = False
    Me.ChkVolatiles.Value = 1
    Me.ChkVolatiles.Enabled = False
  Else
    Me.ChkFixedC.Enabled = True
    Me.ChkMoist.Enabled = True
    Me.ChkVolatiles.Enabled = True
  End If
  
  Call LoadNew(NEWGDATA(1), NEWCDATA, AncilliaryData(1))

Exit Sub

ErrorMessage:
  MsgBox "An error has occurred when loading washability data", 32, "WARNING"
End Sub

Private Sub LoadDefault()
'************************
  Dim I As Integer
  Number.text = CStr(1)
  Label7.Caption = "Float"
  Density(0).text = CStr(2.7)
  Spgr(0).text = CStr(1.3)
  Fraction(0).text = CStr(100)
  Ash(0).text = CStr(10)
  If Sysdata.ChkProximate.Value = 1 Then
    ChkFixedC.Value = 1
    FixedC(0).Visible = True
    FixedC(0).Enabled = True
    FixedC(0).text = CStr(60)
    ChkVolatiles.Value = 1
    Volatiles(0).Visible = True
    Volatiles(0).Enabled = True
    Volatiles(0).text = CStr(25)
    ChkMoist.Value = 1
    Moisture(0).Visible = True
    Moisture(0).Enabled = True
    Moisture(0).text = CStr(5)
    ChkCalVal.Value = 1
    CValue(0).text = CStr(26)
    ChkSulfur.Value = 1
    Sulfur(0).text = CStr(1.5)
    ChkPyrSulfur.Value = 1
    PyrSulfur(0).text = CStr(1.2)
  Else
    ChkFixedC.Value = 0
    FixedC(0).Visible = False
    FixedC(0).Enabled = False
    ChkVolatiles.Value = 0
    Volatiles(0).Visible = False
    Volatiles(0).Enabled = False
    ChkMoist.Value = 0
    Moisture(0).Visible = False
    Moisture(0).Enabled = False
    ChkCalVal.Value = 0
    ChkSulfur.Value = 0
    ChkPyrSulfur.Value = 0
  End If
  For I = 1 To 14
    Densu(I - 1).Visible = False
    Density(I).Visible = False
    Spgr(I).Visible = False
    Fraction(I).Visible = False
    Ash(I).Visible = False
    FixedC(I).Visible = False
    Volatiles(I).Visible = False
    Moisture(I).Visible = False
    CValue(I).Visible = False
    Sulfur(I).Visible = False
    PyrSulfur(I).Visible = False
  Next I
End Sub

Private Sub LoadNew(NNDATA As GRADEDATA, NDATA As CLASSDATA, NWDATA As WashabilityData)
'***********************************************************
  Dim N As Integer, I As Integer
  Dim NGC As Integer

  NGC = NNDATA.NGC
  Number.text = CStr(NNDATA.NGC)
  Sysdata.NGCM.text = Number.text
  For N = 1 To NNDATA.NGC
    Fraction(N - 1).text = CStr(NNDATA.Fraction(N))
    Fraction(N - 1).Visible = -1
  Next N
  If NDATA.PPROP(NDATA.INDPP(1, 1)) > 0 Then
    LblSpGr.Visible = True
    For N = 1 To NDATA.NGC
      Spgr(N - 1).text = Format(NDATA.PPROP(NDATA.INDPP(1, 1) + N - 1), "#.000")
      Spgr(N - 1).Visible = True
      Spgr(N - 1).Enabled = True
    Next N
  Else
    For N = 1 To NDATA.NGC
      Spgr(N - 1).Enabled = True
    Next N
  End If
  If NDATA.INDPP(8, 1) > 0 Then
    ChkCalVal.Value = 1
    For N = 1 To NDATA.NGC
      CValue(N - 1).text = Format(NDATA.PPROP(NDATA.INDPP(8, 1) + N - 1), "#.000")
      CValue(N - 1).Visible = -1
    Next N
  Else
    ChkCalVal.Value = 0
    For N = 1 To NDATA.NGC
      CValue(N - 1).text = ""
      CValue(N - 1).Visible = 0
    Next N
  End If
  If NDATA.INDPP(9, 1) > 0 Then
    ChkSulfur.Value = 1
    For N = 1 To NDATA.NGC
      Sulfur(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(9, 1) + N - 1), "#.000")
      Sulfur(N - 1).Visible = -1
    Next N
  Else
    ChkSulfur.Value = 0
    For N = 1 To NDATA.NGC
      Sulfur(N - 1).text = ""
      Sulfur(N - 1).Visible = 0
    Next N
  End If
  If NDATA.INDPP(10, 1) > 0 Then
    ChkPyrSulfur.Value = 1
    For N = 1 To NDATA.NGC
      PyrSulfur(N - 1).text = Format$(NDATA.PPROP(NDATA.INDPP(10, 1) + N - 1), "#.000")
      PyrSulfur(N - 1).Visible = -1
    Next N
  Else
    ChkPyrSulfur.Value = 0
    For N = 1 To NDATA.NGC
      PyrSulfur(N - 1).text = ""
      PyrSulfur(N - 1).Visible = 0
    Next N
  End If
  For N = 1 To NDATA.NGC
    Ash(N - 1).text = CStr(100 * NDATA.GRDMV(N, 1))
    If Sysdata.ChkProximate.Value = vbChecked Then
      FixedC(N - 1).text = CStr(100 * NDATA.GRDMV(N, 2))
      Volatiles(N - 1).text = CStr(100 * NDATA.GRDMV(N, 3))
      Moisture(N - 1).text = CStr(100 * NDATA.GRDMV(N, 4))
      FixedC(N - 1).Visible = True
      Volatiles(N - 1).Visible = True
      Moisture(N - 1).Visible = True
    Else
      FixedC(N - 1).Visible = False
      Volatiles(N - 1).Visible = False
      Moisture(N - 1).Visible = False
    End If
  Next N
    
  For I = 0 To NDATA.NGC - 1
    Ash(I).Visible = True
    Densu(I).Visible = -1
    Label8(I).Visible = -1
    Density(I).Visible = -1
  Next I
  For I = NDATA.NGC To 14
    Densu(I - 1).Visible = False
    Density(I).Visible = False
    Spgr(I).Visible = False
    Fraction(I).Visible = False
    Ash(I).Visible = False
    FixedC(I).Visible = False
    Volatiles(I).Visible = False
    Moisture(I).Visible = False
    CValue(I).Visible = False
    Sulfur(I).Visible = False
    PyrSulfur(I).Visible = False
  Next I
  
  'Now the Grade data
  Number.text = CStr(NNDATA.NGC)
  SizeU.text = CStr(1000 * NNDATA.SIZE_RANGE(2))
  SizeL.text = CStr(1000 * NNDATA.SIZE_RANGE(1))
  OptSizeUnits(0) = True
  Number.text = CStr(NNDATA.NGC)
  For I = 1 To NNDATA.NGC
    Fraction(I - 1).text = CStr(100 * NNDATA.Fraction(I))
  Next I

  'and now the ancilliary data
  Label7.Caption = "Float"
  For I = 1 To NGC - 1
    Densu(I - 1).text = NWDATA.LowerDensity(I + 1)
    Density(I - 1).text = NWDATA.UpperDensity(I)
  Next I
  Density(NGC - 1).text = "Sink"
End Sub

Private Sub fraction_Change(Index As Integer)
'********************************************
  CHANGE_FLAG = -1
End Sub

Private Sub fraction_KeyPress(Index As Integer, KeyAscii As Integer)
'*******************************************************************
  If KeyAscii = 13 Then
    If Index > 0 And OptCumulative.Value = True Then
      If Val(Fraction(Index).text) < Val(Fraction(Index - 1).text) Then
        MsgBox "Weights must increase", 16, "ERROR"
        Fraction(Index).SetFocus
        KeyAscii = 0
        Exit Sub
      End If
    End If
    If Index < Val(Number.text) - 1 Then
      Fraction(Index + 1).text = ""
      Fraction(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub fraction_LostFocus(Index As Integer)
'***********************************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
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
'Print the form.
PrintForm
End Sub

Private Sub Moisture_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub Moisture_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    If Index < Val(Number.text) - 1 Then
      Moisture(Index + 1).text = ""
      Moisture(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub Moisture_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Number_Change()
'**************************
  CHANGE_FLAG = -1
End Sub

Private Sub Number_KeyPress(KeyAscii As Integer)
If KeyAscii = 13 Then
  Density(0).text = ""
  Density(0).SetFocus
  KeyAscii = 0
End If
End Sub

Private Sub Number_LostFocus()
'*****************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
    Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub OptCurrent_Click()
  If Screen.ActiveControl.Tag = "Current" Then
  Call LoadNew(CURRGDATA(Psd.CURR_INDEX, ACTIVE_K), CURRENT, CurrentAncData(ACTIVE_K))
  End If
End Sub

Private Sub OptDefault_Click()
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub OptUnitsCalValue_Click(Index As Integer)
'***************************************************
If OptUnitsCalValue(0) Then
  CmdCVConvert.Caption = ">"
Else
  CmdCVConvert.Caption = "<"
End If
End Sub

Private Sub PyrSulfur_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub PyrSulfur_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    If Index < Val(Number.text) - 1 Then
      PyrSulfur(Index + 1).text = ""
      PyrSulfur(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub PyrSulfur_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub SizeL_Change()
'*************************
  CHANGE_FLAG = -1
End Sub

Private Sub SizeL_LostFocus()
'****************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
    Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub SizeU_Change()
'*************************
  CHANGE_FLAG = -1
End Sub

Private Sub SizeU_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then
    SizeL.SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub SizeU_LostFocus()
'****************************
  If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
    Call LoadNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Spgr_Change(Index As Integer)
'****************************************
CHANGE_FLAG = 1
End Sub

Private Sub Spgr_KeyPress(Index As Integer, KeyAscii As Integer)
'***************************************************************
  If KeyAscii = 13 Then
    If Index > 0 Then
      If Val(Spgr(Index).text) < Val(Spgr(Index - 1).text) Then
        MsgBox "Density should increase", 16, "ERROR"
        Spgr(Index).SetFocus
        KeyAscii = 0
        Exit Sub
      End If
    End If
    If Index < Val(Number.text) - 1 Then
      Spgr(Index + 1).text = ""
      Spgr(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub Sulfur_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub Sulfur_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    If Index < Val(Number.text) - 1 Then
      Sulfur(Index + 1).SetFocus
      Sulfur(Index + 1).text = ""
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub Sulfur_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Private Sub Volatiles_Change(Index As Integer)
  CHANGE_FLAG = -1
End Sub

Private Sub Volatiles_KeyPress(Index As Integer, KeyAscii As Integer)
  If KeyAscii = 13 Then
    If Index < Val(Number.text) - 1 Then
      Volatiles(Index + 1).text = ""
      Volatiles(Index + 1).SetFocus
    End If
    KeyAscii = 0
  End If
End Sub

Private Sub Volatiles_LostFocus(Index As Integer)
 If CHANGE_FLAG Then
    OptNew.Value = -1
    Call MakeNew(NEWGDATA(ACTIVE_K), NEWCDATA, AncilliaryData(ACTIVE_K))
  End If
End Sub

Public Sub ConvertCV(Factor As Single)
'*************************************
Dim N As Integer
For N = 0 To Number - 1
  CValue(N).text = CStr(Factor * Val(CValue(N).text))
Next N
End Sub
