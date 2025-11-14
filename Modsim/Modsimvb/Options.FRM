VERSION 5.00
Begin VB.Form Options 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Options"
   ClientHeight    =   1200
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4680
   Icon            =   "Options.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1200
   ScaleWidth      =   4680
   StartUpPosition =   3  'Windows Default
   Begin VB.CheckBox chkFullscreen 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Display fullscreen"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   1695
   End
   Begin VB.CommandButton CmdClose 
      Caption         =   "Close"
      Height          =   375
      Left            =   3720
      TabIndex        =   1
      Top             =   720
      Width           =   735
   End
   Begin VB.CheckBox ChkDisplay 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Display flowsheet when job is opened"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   600
      Width           =   3135
   End
End
Attribute VB_Name = "Options"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CmdClose_Click()
'***************************
  If Me.chkFullscreen.Value = vbChecked Then
    OptionShowFullScreen = True
  Else
    OptionShowFullScreen = False
  End If
  If Me.ChkDisplay.Value = vbChecked Then
    OptionShowFlowsheet = True
  Else
    OptionShowFlowsheet = False
  End If
  Unload Me
End Sub

Private Sub Form_Load()
'**********************
If OptionShowFullScreen Then
  Me.chkFullscreen.Value = vbChecked
Else
  Me.chkFullscreen.Value = vbUnchecked
End If
If OptionShowFlowsheet Then
  Me.ChkDisplay.Value = vbChecked
Else
  Me.ChkDisplay.Value = vbUnchecked
End If
End Sub
