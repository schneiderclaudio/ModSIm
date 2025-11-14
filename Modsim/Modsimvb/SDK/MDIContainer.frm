VERSION 5.00
Begin VB.MDIForm MDIContainer 
   BackColor       =   &H8000000C&
   Caption         =   "MODSIM Model Software Development Kit"
   ClientHeight    =   8715
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   11700
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   3  'Windows Default
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuSave 
         Caption         =   "Save"
      End
      Begin VB.Menu mnuCancel 
         Caption         =   "Cancel"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuModelNames 
         Caption         =   "Models"
      End
   End
End
Attribute VB_Name = "MDIContainer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub mnuExit_Click()
'**************************
  End
End Sub

Private Sub mnuModelNames_Click()
'********************************
Models.Show
End Sub

