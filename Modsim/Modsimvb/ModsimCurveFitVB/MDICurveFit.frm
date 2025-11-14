VERSION 5.00
Begin VB.MDIForm MODSIM 
   BackColor       =   &H8000000C&
   Caption         =   "MODSIM curve fit"
   ClientHeight    =   8280
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   10590
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "MODSIM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'Stands in place of the main MODSIM form
Public JOBNAME As String

