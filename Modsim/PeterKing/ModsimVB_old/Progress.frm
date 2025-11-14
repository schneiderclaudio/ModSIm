VERSION 5.00
Begin VB.Form FrmShowProgress 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Progress of repetitive simulations"
   ClientHeight    =   2385
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   6645
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   2385
   ScaleWidth      =   6645
   Begin VB.Label LblComplete 
      ForeColor       =   &H000000FF&
      Height          =   255
      Left            =   3120
      TabIndex        =   25
      Top             =   240
      Width           =   3255
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   5
      Left            =   5400
      TabIndex        =   24
      Top             =   2040
      Width           =   1095
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   4
      Left            =   5400
      TabIndex        =   23
      Top             =   1800
      Width           =   1095
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   3
      Left            =   5400
      TabIndex        =   22
      Top             =   1560
      Width           =   1095
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   2
      Left            =   5400
      TabIndex        =   21
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   1
      Left            =   5400
      TabIndex        =   20
      Top             =   1080
      Width           =   1095
   End
   Begin VB.Label LblParamValue 
      Caption         =   "Value"
      Height          =   225
      Index           =   0
      Left            =   5400
      TabIndex        =   19
      Top             =   840
      Width           =   1095
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   5
      Left            =   2160
      TabIndex        =   18
      Top             =   2040
      Width           =   3135
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   4
      Left            =   2160
      TabIndex        =   17
      Top             =   1800
      Width           =   3135
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   3
      Left            =   2160
      TabIndex        =   16
      Top             =   1560
      Width           =   3135
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   2
      Left            =   2160
      TabIndex        =   15
      Top             =   1320
      Width           =   3135
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   1
      Left            =   2160
      TabIndex        =   14
      Top             =   1080
      Width           =   3135
   End
   Begin VB.Label LblParameterName 
      Caption         =   "Parameter"
      Height          =   225
      Index           =   0
      Left            =   2160
      TabIndex        =   13
      Top             =   840
      Width           =   3135
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   5
      Left            =   840
      TabIndex        =   12
      Top             =   2040
      Width           =   1095
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   4
      Left            =   840
      TabIndex        =   11
      Top             =   1800
      Width           =   1095
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   3
      Left            =   840
      TabIndex        =   10
      Top             =   1560
      Width           =   1095
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   2
      Left            =   840
      TabIndex        =   9
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   1
      Left            =   840
      TabIndex        =   8
      Top             =   1080
      Width           =   1095
   End
   Begin VB.Label LblLevel 
      Caption         =   "5"
      Height          =   225
      Index           =   5
      Left            =   120
      TabIndex        =   7
      Top             =   2040
      Width           =   615
   End
   Begin VB.Label LblLevel 
      Caption         =   "4"
      Height          =   225
      Index           =   4
      Left            =   120
      TabIndex        =   6
      Top             =   1800
      Width           =   615
   End
   Begin VB.Label LblLevel 
      Caption         =   "3"
      Height          =   225
      Index           =   3
      Left            =   120
      TabIndex        =   5
      Top             =   1560
      Width           =   615
   End
   Begin VB.Label LblLevel 
      Caption         =   "2"
      Height          =   225
      Index           =   2
      Left            =   120
      TabIndex        =   4
      Top             =   1320
      Width           =   615
   End
   Begin VB.Label LblLevel 
      Caption         =   "1"
      Height          =   225
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   1080
      Width           =   615
   End
   Begin VB.Label LblUnitNumber 
      Caption         =   "Unit number"
      Height          =   225
      Index           =   0
      Left            =   840
      TabIndex        =   2
      Top             =   840
      Width           =   1095
   End
   Begin VB.Label LblLevel 
      Caption         =   "Level"
      Height          =   225
      Index           =   0
      Left            =   120
      TabIndex        =   1
      Top             =   840
      Width           =   615
   End
   Begin VB.Label LblProgress 
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   2655
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "FrmShowProgress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub MnuClose_Click()
'***************************
Me.Hide
End Sub
