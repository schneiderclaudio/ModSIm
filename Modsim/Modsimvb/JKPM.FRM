VERSION 5.00
Begin VB.Form JKPM 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "JKPM"
   ClientHeight    =   4800
   ClientLeft      =   375
   ClientTop       =   1200
   ClientWidth     =   8235
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
   Icon            =   "JKPM.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4800
   ScaleWidth      =   8235
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty DataFormat 
         Type            =   1
         Format          =   "0"
         HaveTrueFalseNull=   0
         FirstDayOfWeek  =   0
         FirstWeekOfYear =   0
         LCID            =   1033
         SubFormatType   =   1
      EndProperty
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
      Index           =   23
      Left            =   1440
      TabIndex        =   49
      Top             =   360
      Width           =   840
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      BeginProperty DataFormat 
         Type            =   1
         Format          =   "0"
         HaveTrueFalseNull=   0
         FirstDayOfWeek  =   0
         FirstWeekOfYear =   0
         LCID            =   1033
         SubFormatType   =   1
      EndProperty
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
      Index           =   22
      Left            =   1920
      TabIndex        =   38
      Top             =   4200
      Width           =   840
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
      Index           =   21
      Left            =   4920
      TabIndex        =   37
      Top             =   3840
      Width           =   840
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
      Index           =   20
      Left            =   4920
      TabIndex        =   36
      Top             =   3600
      Width           =   840
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
      Index           =   19
      Left            =   4920
      TabIndex        =   35
      Top             =   3360
      Width           =   840
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
      Index           =   18
      Left            =   4920
      TabIndex        =   33
      Top             =   3120
      Width           =   840
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
      Left            =   4920
      TabIndex        =   32
      Top             =   2880
      Width           =   840
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
      Index           =   16
      Left            =   4920
      TabIndex        =   31
      Top             =   2400
      Width           =   840
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
      Left            =   1920
      TabIndex        =   30
      Top             =   3840
      Width           =   840
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
      Left            =   1920
      TabIndex        =   29
      Top             =   3360
      Width           =   840
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
      Left            =   1920
      TabIndex        =   28
      Top             =   3120
      Width           =   840
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
      Left            =   1920
      TabIndex        =   27
      Top             =   2880
      Width           =   840
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
      Left            =   1920
      TabIndex        =   26
      Top             =   2640
      Width           =   840
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
      Left            =   1920
      TabIndex        =   25
      Top             =   2400
      Width           =   840
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
      Height          =   375
      Left            =   7080
      TabIndex        =   13
      Top             =   4200
      Width           =   960
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
      Height          =   375
      Left            =   5880
      TabIndex        =   14
      Top             =   4200
      Width           =   960
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
      Left            =   1920
      TabIndex        =   9
      Top             =   1800
      Width           =   840
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
      Left            =   1920
      TabIndex        =   8
      Top             =   1560
      Width           =   840
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
      Left            =   1920
      TabIndex        =   7
      Top             =   1320
      Width           =   840
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
      Left            =   1920
      TabIndex        =   6
      Top             =   1080
      Width           =   840
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
      Left            =   1920
      TabIndex        =   5
      Top             =   840
      Width           =   840
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
      Left            =   960
      TabIndex        =   4
      Top             =   1800
      Width           =   825
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
      Left            =   960
      TabIndex        =   3
      Top             =   1560
      Width           =   840
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
      Left            =   960
      TabIndex        =   2
      Top             =   1320
      Width           =   840
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
      Left            =   960
      TabIndex        =   1
      Top             =   1080
      Width           =   840
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
      Left            =   960
      TabIndex        =   0
      Top             =   840
      Width           =   840
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
      Left            =   6960
      TabIndex        =   24
      Top             =   240
      Width           =   1215
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
         TabIndex        =   12
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current"
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
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   975
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
         TabIndex        =   10
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   735
      End
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Size (mm)"
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   8.25
         Charset         =   0
         Weight          =   900
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   27
      Left            =   840
      TabIndex        =   59
      Top             =   600
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D*"
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   8.25
         Charset         =   0
         Weight          =   900
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   26
      Left            =   1920
      TabIndex        =   58
      Top             =   600
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Industrial mill"
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
      Index           =   24
      Left            =   1440
      TabIndex        =   57
      Top             =   2160
      Width           =   1320
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Factor K"
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
      Index           =   20
      Left            =   3480
      TabIndex        =   56
      Top             =   3600
      Width           =   1320
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill WI kWh/t"
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
      Index           =   19
      Left            =   3480
      TabIndex        =   55
      Top             =   3360
      Width           =   1320
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill charge J %"
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
      Index           =   18
      Left            =   3120
      TabIndex        =   54
      Top             =   3120
      Width           =   1680
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill crit. speed %"
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
      Left            =   3120
      TabIndex        =   53
      Top             =   2880
      Width           =   1680
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill diam. (m)"
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
      Left            =   3240
      TabIndex        =   52
      Top             =   2400
      Width           =   1560
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D* knot 5"
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
      Left            =   1800
      TabIndex        =   51
      Top             =   1800
      Visible         =   0   'False
      Width           =   1080
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D* knot 4"
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
      Left            =   1800
      TabIndex        =   50
      Top             =   1560
      Visible         =   0   'False
      Width           =   1080
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of knots:"
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
      Index           =   23
      Left            =   120
      TabIndex        =   48
      Top             =   360
      Width           =   1320
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of mills:"
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
      Index           =   22
      Left            =   600
      TabIndex        =   47
      Top             =   4200
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "WI kWh/t"
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
      Left            =   480
      TabIndex        =   46
      Top             =   3360
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Charge J %"
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
      TabIndex        =   45
      Top             =   3120
      Width           =   1065
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Critical speed %"
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
      Left            =   600
      TabIndex        =   44
      Top             =   2880
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill Length (m)"
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
      TabIndex        =   43
      Top             =   2640
      Width           =   1080
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill Diameter (m)"
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
      Left            =   600
      TabIndex        =   42
      Top             =   2400
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Knot 5"
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
      Left            =   0
      TabIndex        =   41
      Top             =   1800
      Width           =   840
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill ball top size (mm)"
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
      Index           =   21
      Left            =   2880
      TabIndex        =   40
      Top             =   3840
      Width           =   1920
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Test mill"
      BeginProperty Font 
         Name            =   "Arial Black"
         Size            =   8.25
         Charset         =   0
         Weight          =   900
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   25
      Left            =   4560
      TabIndex        =   39
      Top             =   2160
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ball top size (mm)"
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
      Left            =   360
      TabIndex        =   34
      Top             =   3840
      Width           =   1440
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D* knot 3"
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
      Left            =   1800
      TabIndex        =   22
      Top             =   1320
      Visible         =   0   'False
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D* knot 2"
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
      Left            =   1800
      TabIndex        =   21
      Top             =   1080
      Visible         =   0   'False
      Width           =   1185
   End
   Begin VB.Label LblParameter 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ln R/D* knot 1"
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
      Left            =   1800
      TabIndex        =   20
      Top             =   840
      Visible         =   0   'False
      Width           =   1200
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Knot 4"
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
      Left            =   0
      TabIndex        =   19
      Top             =   1560
      Width           =   840
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Knot 3"
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
      Left            =   0
      TabIndex        =   18
      Top             =   1320
      Width           =   840
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Knot 2"
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
      Left            =   0
      TabIndex        =   17
      Top             =   1080
      Width           =   840
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Knot 1"
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
      Left            =   0
      TabIndex        =   16
      Top             =   840
      Width           =   840
   End
   Begin VB.Label LblMessage 
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
      TabIndex        =   23
      Top             =   0
      Visible         =   0   'False
      Width           =   8655
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
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
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   2640
      TabIndex        =   15
      Top             =   0
      Width           =   3960
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "JKPM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NEWDATA As ModelData
Private CHANGE_FLAG As Integer
Private ID As Integer

Private Sub CmdAccept_Click()
'*****************************
  Call MakeNew(CURRMODELDATA(ID))
  Unload JKPM
End Sub

Private Sub CmdCancel_Click()
'*****************************
  Unload JKPM
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload Help 'In case it is still visible
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
  'Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "JKPM" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading parameter specification form", 16, "ERROR"
  Exit Sub
End Sub
Private Sub LoadDefault()
'*************************
OptDefault.Value = -1
'Number of knots
Parameter(23).text = Str$(5)
'x values
Parameter(0).text = Str$(0.15)
Parameter(1).text = Str$(0.7)
Parameter(2).text = Str$(1.83)
Parameter(3).text = Str$(3.35)
Parameter(4).text = Str$(9.47)
'y values
Parameter(5).text = Str$(0.955)
Parameter(6).text = Str$(2.3)
Parameter(7).text = Str$(2.8)
Parameter(8).text = Str$(3.69)
Parameter(9).text = Str$(3.67)
'Mill diameter
Parameter(10).text = Str$(5.36)
'Mill length
Parameter(11).text = Str$(7.32)
'Fraction critical speed %
Parameter(12).text = Str$(75)
'J (%)
Parameter(13).text = Str$(40#)
'WI kWh/t
Parameter(14).text = Str$(15#)
'Ball top size (mm)
Parameter(15).text = Str$(50)
'Test mill diameter
Parameter(16).text = Str$(0.3048)
'Test mill fraction critical speed %
Parameter(17).text = Str$(75)
'Test mill J (%)
Parameter(18).text = Str$(40#)
'Test mill WI kWh/t
Parameter(19).text = Str$(15#)
'Test mill factor K
Parameter(20).text = Str$(0.00044)
'Test mill ball top size (mm)
Parameter(21).text = Str$(30)
'Number of mills in parallel
Parameter(22).text = Str$(1)
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
    For I = 1 To NDATA.NOPAR
      Parameter(I - 1).text = Str$(NDATA.PARAM(I))
    Next I
  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim Factor As Single
  Dim I As Integer
  NDATA.Model = "JKPM"
  NDATA.NOPAR = 24
  For I = 1 To 24
      NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub OptCurrent_Click()
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Private Sub OptDefault_Click()
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'**********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    CHANGE_FLAG = -1
  End If
  If Val(Parameter(23).text) < 3 Then
     Parameter(23).text = Str$(3)
  End If
  If Val(Parameter(23).text) > 5 Then
     Parameter(23).text = Str$(5)
  End If
  If Val(Parameter(23).text) > 3 Then
     Parameter(3).Visible = True
     Parameter(8).Visible = True
     If Val(Parameter(23).text) > 4 Then
        Parameter(4).Visible = True
        Parameter(9).Visible = True
    Else
        Parameter(4).Visible = False
        Parameter(9).Visible = False
    End If
  Else
     Parameter(3).Visible = False
     Parameter(8).Visible = False
     Parameter(4).Visible = False
     Parameter(9).Visible = False
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'*************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub optSize_Click(Index As Integer)
'*******************************************
  If TypeOf Screen.ActiveControl Is OptionButton Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
'***********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
  Dim I As Integer, IDIFF As Integer
  On Error GoTo ErrHandler
  
  If Shift = 2 And ProfessionalVersion Then
    Call MakeNew(NEWDATA)
    Repeat.TxtParameterName = Me.LblParameter(Index).Caption
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
          Repeat.LstLevel.Enabled = False
          Repeat.TxtParameterName = ParameterName(I)
          Repeat.TxtValue(0) = CStr(StartValue(I))
          Repeat.TxtValue(1) = CStr(EndValue(I))
          Repeat.TxtValue(2) = CStr(StepValue(I))
        End If
      End If
    Next I
    Repeat.Show vbModal
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
