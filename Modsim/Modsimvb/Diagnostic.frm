VERSION 5.00
Begin VB.Form Diagnostic 
   BackColor       =   &H00C0C0C0&
   Caption         =   "Diagnostic"
   ClientHeight    =   8550
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   3015
   Icon            =   "Diagnostic.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   8550
   ScaleWidth      =   3015
   StartUpPosition =   3  'Windows Default
   Begin VB.ListBox lstDiagnostic 
      Height          =   7080
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   2775
   End
   Begin VB.Label lblDiagnostic 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Label"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   1
      Top             =   8160
      Width           =   3375
   End
   Begin VB.Label lblDiagnostic 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Label"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   0
      Top             =   7680
      Width           =   3375
   End
End
Attribute VB_Name = "Diagnostic"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
