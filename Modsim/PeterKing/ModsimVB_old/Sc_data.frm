VERSION 5.00
Begin VB.Form Sc_data 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify the distribution of rate constants"
   ClientHeight    =   5535
   ClientLeft      =   75
   ClientTop       =   1695
   ClientWidth     =   8655
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
   ScaleHeight     =   5535
   ScaleWidth      =   8655
   Tag             =   "Grade class"
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
      TabIndex        =   47
      Top             =   2760
      Width           =   1440
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
      TabIndex        =   46
      Top             =   2520
      Width           =   1440
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
      TabIndex        =   45
      Top             =   2280
      Width           =   1440
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
      TabIndex        =   44
      Top             =   2040
      Width           =   1440
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
      TabIndex        =   43
      Top             =   1800
      Width           =   1440
   End
   Begin VB.CommandButton CmdClear 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      Caption         =   "Clear"
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
      Left            =   3480
      TabIndex        =   40
      Top             =   600
      Width           =   840
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
      Height          =   480
      Left            =   6360
      TabIndex        =   29
      Top             =   4560
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
      Height          =   480
      Left            =   7560
      TabIndex        =   28
      Top             =   4560
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
      Index           =   4
      Left            =   1920
      TabIndex        =   4
      Top             =   1560
      Width           =   1440
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
      Left            =   1920
      TabIndex        =   3
      Top             =   1320
      Width           =   1440
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
      Left            =   1920
      TabIndex        =   2
      Top             =   1080
      Width           =   1440
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
      Left            =   1920
      TabIndex        =   1
      Top             =   840
      Width           =   1440
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
      Left            =   1920
      TabIndex        =   0
      Top             =   600
      Width           =   1440
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
      Left            =   7200
      TabIndex        =   33
      Top             =   360
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
         TabIndex        =   27
         Tag             =   "Default"
         Top             =   720
         Value           =   -1  'True
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
         TabIndex        =   26
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
         TabIndex        =   25
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Width           =   735
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Grade class for this distribution"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1335
      Left            =   120
      TabIndex        =   39
      Top             =   3120
      Width           =   8415
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "22"
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
         Index           =   21
         Left            =   960
         TabIndex        =   42
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   960
         Width           =   615
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "21"
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
         Index           =   20
         Left            =   120
         TabIndex        =   41
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   960
         Width           =   615
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "1"
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
         TabIndex        =   5
         Tag             =   "GradeClass"
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "20"
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
         Index           =   19
         Left            =   7680
         TabIndex        =   24
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   615
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "19"
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
         Index           =   18
         Left            =   6840
         TabIndex        =   23
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "18"
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
         Index           =   17
         Left            =   6000
         TabIndex        =   22
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "17"
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
         Index           =   16
         Left            =   5160
         TabIndex        =   21
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "16"
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
         Index           =   15
         Left            =   4320
         TabIndex        =   20
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "15"
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
         Index           =   14
         Left            =   3480
         TabIndex        =   19
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "14"
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
         Index           =   13
         Left            =   2640
         TabIndex        =   18
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "13"
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
         Index           =   12
         Left            =   1800
         TabIndex        =   17
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "12"
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
         Index           =   11
         Left            =   960
         TabIndex        =   16
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "11"
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
         Index           =   10
         Left            =   120
         TabIndex        =   15
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "10"
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
         Index           =   9
         Left            =   7680
         TabIndex        =   14
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   615
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "9"
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
         Index           =   8
         Left            =   6840
         TabIndex        =   13
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "8"
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
         Index           =   7
         Left            =   6000
         TabIndex        =   12
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "7"
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
         Index           =   6
         Left            =   5160
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "6"
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
         Index           =   5
         Left            =   4320
         TabIndex        =   10
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "5"
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
         Index           =   4
         Left            =   3480
         TabIndex        =   9
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "4"
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
         Left            =   2640
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "3"
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
         Left            =   1800
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptGradeClass 
         BackColor       =   &H00C0C0C0&
         Caption         =   "2"
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
         Left            =   960
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "GradeClass"
         Top             =   240
         Width           =   720
      End
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "10"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   52
      Top             =   2760
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "9"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   51
      Top             =   2520
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "8"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   50
      Top             =   2280
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "7"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   49
      Top             =   2040
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "6"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   48
      Top             =   1800
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "5"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   38
      Top             =   1560
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "4"
      BeginProperty Font 
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
      TabIndex        =   37
      Top             =   1320
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "3"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   36
      Top             =   1080
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "2"
      BeginProperty Font 
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
      Left            =   120
      TabIndex        =   35
      Top             =   840
      Width           =   1680
   End
   Begin VB.Label LblClass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "1"
      BeginProperty Font 
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
      TabIndex        =   34
      Top             =   600
      Width           =   1680
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "S-Class number"
      BeginProperty Font 
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
      TabIndex        =   31
      Top             =   360
      Width           =   1680
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Fraction"
      BeginProperty Font 
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
      Left            =   1920
      TabIndex        =   32
      Top             =   360
      Width           =   1440
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
      Left            =   0
      TabIndex        =   30
      Top             =   0
      Visible         =   0   'False
      Width           =   8400
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
Attribute VB_Name = "Sc_data"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private NO_S_RANGES As Integer, NGC As Integer, NSC As Integer
Private ACTIVE_K As Integer
Private CHANGE_FLAG  As Integer
Private NEWSDATA(1 To 15) As SDATA
Private TEMPSDATA(1 To 15) As SDATA

Private Sub CmdAccept_Click()
'****************************
Dim K As Integer, N As Integer
Dim Cum As Single
  CURRDATA(Psd.CURR_INDEX).NO_S_RANGES = NO_S_RANGES
  For K = 1 To NO_S_RANGES
        ACTIVE_K = K
        Cum = 0!
        For N = 1 To NSC
          Cum = Cum + NEWSDATA(K).Fraction(N)
        Next N
        If Abs(Cum - 1) > 0.00001 Then
          OptGradeClass(K - 1).Value = -1
          Call LoadNew(NEWSDATA())
          WSTRING = "Fractions sum to " + Str$(Cum)
          MsgBox WSTRING, 0, "ERROR"
          Exit Sub
        End If
        CURRSDATA(K, Psd.CURR_INDEX) = NEWSDATA(K)
  Next K

  Unload Sc_data
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload Sc_data
End Sub

Private Sub CmdClear_Click()
'***************************
  Dim N As Integer
  For N = 1 To 10
        Parameter(N - 1).text = ""
  Next N
  Parameter(0).SetFocus
End Sub

Private Sub Form_Load()
'**********************
  Dim K As Integer, I As Integer
  
  CHANGE_FLAG = 0
  NGC = Val(Sysdata.NGCM.text)
  NSC = Val(Sysdata.NSCM.text)
  For K = 1 To 22
        OptGradeClass(K - 1).Visible = -1
        If K > NGC Then OptGradeClass(K - 1).Visible = 0
  Next K
  For I = NSC + 1 To 10
        LblClass(I - 1).Visible = 0
        Parameter(I - 1).Visible = 0
  Next I
  ACTIVE_K = 1

  'Find the current data for this stream if any
  If Psd.CURR_INDEX = 0 Or CURRDATA(Psd.CURR_INDEX).NO_S_RANGES = 0 Then
        OptCurrent.Enabled = 0
        OptDefault.Value = -1
        LoadDefault
        NO_S_RANGES = NGC
        Call MakeNew(NEWSDATA())
  Else
        NO_S_RANGES = CURRDATA(Psd.CURR_INDEX).NO_S_RANGES
        For K = 1 To NO_S_RANGES
          NEWSDATA(K) = CURRSDATA(K, Psd.CURR_INDEX)
          TEMPSDATA(K) = CURRSDATA(K, Psd.CURR_INDEX)
        Next K
        For K = 1 To NSC
          LblClass(K - 1).Visible = -1
        Next K
        Call LoadNew(NEWSDATA())
        Call MakeNew(NEWSDATA())
  End If
  OptNew.Value = -1
End Sub

Private Sub LoadDefault()
'************************
  Dim K As Integer
  For K = 1 To NSC
        Parameter(K - 1).text = Str$(1 / NSC)
  Next K
  'OptGradeClass(ACTIVE_K - 1).Value = -1
End Sub

Private Sub LoadNew(NDATA() As SDATA)
'************************************
Dim K As Integer
  For K = 1 To NSC
        Parameter(K - 1).text = Str$(NDATA(ACTIVE_K).Fraction(K))
  Next K
  OptGradeClass(ACTIVE_K - 1).Value = -1
End Sub

Private Sub MakeNew(NDATA() As SDATA)
'************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA(ACTIVE_K).NSTR = Val(Psd.Stream.Caption)
  NDATA(ACTIVE_K).NGC = Val(Sysdata.NGCM.text)
  NO_S_RANGES = Val(Sysdata.NGCM.text)
  NDATA(ACTIVE_K).NSC = Val(Sysdata.NSCM.text)
  For I = 1 To NSC
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

Private Sub OptCurrent_Click()
'*****************************
  Dim K As Integer
  Dim TEMPDATA(1 To 20) As SDATA
  If Screen.ActiveControl.Tag = "Current" Then
        For K = 1 To NO_S_RANGES
          TEMPDATA(K) = CURRSDATA(K, Psd.CURR_INDEX)
        Next K
        Call LoadNew(TEMPDATA())
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
    'CHANGE_FLAG = -1
  End If
End Sub

Private Sub OptGradeClass_CLICK(Index As Integer)
'************************************************
  If Screen.ActiveControl.Tag = "GradeClass" Then
    ACTIVE_K = Index + 1
    If OptNew.Value = -1 Then
      Call LoadNew(NEWSDATA())
    End If
    If OptCurrent.Value = -1 Then
      Call LoadNew(TEMPSDATA())
    End If
    If OptDefault.Value = -1 Then
      Call LoadDefault
    End If
  End If
End Sub

Private Sub OptGradeClass_GotFocus(Index As Integer)
  Instruction.Caption = "Select the Grade class for this distribution"
  Instruction.Visible = -1
End Sub

Private Sub OptGradeClass_LostFocus(Index As Integer)
'****************************************************
  If CHANGE_FLAG Then Call MakeNew(NEWSDATA())
  Instruction.Visible = 0
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
        Call LoadNew(NEWSDATA())
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
        'Parameter(Index).SetFocus
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  Instruction.Caption = "Specify the fractional distribution of rate constants"
  Instruction.Visible = -1
End Sub

Private Sub Parameter_KeyPress(Index As Integer, KeyAscii As Integer)
  If Index < NSC - 1 And KeyAscii = 13 Then
        Parameter(Index + 1).SetFocus
        KeyAscii = 0
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWSDATA())
  Instruction.Visible = 0
End Sub
