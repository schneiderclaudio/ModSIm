VERSION 5.00
Begin VB.MDIForm MDIPSD 
   BackColor       =   &H00E0E0E0&
   Caption         =   "Graphics for Mineral Processors"
   ClientHeight    =   7920
   ClientLeft      =   1140
   ClientTop       =   1800
   ClientWidth     =   10530
   LinkTopic       =   "MDIForm1"
   Picture         =   "MDIPSD.frx":0000
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuExit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu MnuSelect 
      Caption         =   "Select"
      Begin VB.Menu MnuPSD 
         Caption         =   "Size distribution"
      End
      Begin VB.Menu Mnu2CompLib 
         Caption         =   "2 component liberation distribution"
      End
      Begin VB.Menu Mnu3CompLib 
         Caption         =   "3 component liberation distribution"
      End
      Begin VB.Menu minuDivider1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuBrFunc 
         Caption         =   "Breakage functions"
      End
      Begin VB.Menu mnuDivider3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuBetaFuction 
         Caption         =   "Incomplete beta function"
      End
      Begin VB.Menu mnuDivider2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuBatchMill 
         Caption         =   "Batch mill simulation"
      End
      Begin VB.Menu mnuDivider4 
         Caption         =   "-"
      End
      Begin VB.Menu mnuHypoTest 
         Caption         =   "Hypothesis tests for distributions"
      End
   End
End
Attribute VB_Name = "MDIPSD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub MDIForm_Load()
'*************************
End Sub

Private Sub Mnu2CompLib_Click()
'******************************
ID = 2
LibData.Show
End Sub

Private Sub Mnu3CompLib_Click()
'******************************
'MsgBox "This function is not yet implemented", vbInformation, "SORRY"
ThreeComp.Show
End Sub

Private Sub mnuBatchMill_Click()
'*******************************
ID = 1
BatchMill.Top = 0
BatchMill.Left = 0
BatchMill.Show
End Sub

Private Sub mnuBetaFuction_Click()
'*********************************
BetaFunctionParams.Show
End Sub

Private Sub mnuBrFunc_Click()
'****************************
BrFuncRegression.Show
End Sub

Private Sub MnuExit_Click()
Call MGCLOSE
End
End Sub

Private Sub mnuHypoTest_Click()
'******************************
  Dim ReturnFile As String
  ' Clear the output file
  ReturnFile = Dir(RTrim(JobPath) & "HypoTest.out")
  If ReturnFile <> "" Then
    Kill RTrim(JobPath) & "HypoTest.out"
  End If
  HYPOTEST.Show
End Sub

Private Sub MnuPSD_Click()
Sizgraph.Left = 0
Sizgraph.Top = 0
Sizgraph.Show
End Sub

