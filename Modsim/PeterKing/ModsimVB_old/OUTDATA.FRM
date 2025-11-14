VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form OutData 
   Caption         =   "Simulation results"
   ClientHeight    =   6300
   ClientLeft      =   450
   ClientTop       =   1140
   ClientWidth     =   11625
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   6300
   ScaleWidth      =   11625
   WindowState     =   2  'Maximized
   Begin MSFlexGridLib.MSFlexGrid Grid 
      Height          =   6135
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   11415
      _ExtentX        =   20135
      _ExtentY        =   10821
      _Version        =   393216
      Rows            =   1
      WordWrap        =   -1  'True
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
         Enabled         =   0   'False
         Visible         =   0   'False
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
   Begin VB.Menu MnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu MnuCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
   End
End
Attribute VB_Name = "OutData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
'*********************
Me.WindowState = 2   'Maximized
End Sub

Private Sub MnuClose_Click()
'***************************
Unload Me
End Sub

Private Sub MnuCopy_Click()
'**************************
'Copies selected text to the clipboard
  Clipboard.Clear
  Clipboard.SetText Grid.Clip
End Sub

Private Sub MnuPrint_Click()
'***************************
PrintForm
End Sub
