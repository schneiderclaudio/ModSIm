VERSION 5.00
Begin VB.Form Mixr 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Parameter setup for model MIXR"
   ClientHeight    =   1350
   ClientLeft      =   2100
   ClientTop       =   1350
   ClientWidth     =   7200
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
   Icon            =   "Mixr.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1350
   ScaleWidth      =   7200
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "ACCEPT"
      Height          =   360
      Left            =   6120
      TabIndex        =   1
      Top             =   600
      Width           =   960
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   "This model requires no parameters.  Click ACCEPT to continue."
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Width           =   5535
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
Attribute VB_Name = "Mixr"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
'****************************
  CURRMODELDATA(ID).Model = "MIXR"
  CURRMODELDATA(ID).NOPAR = 0
  Unload Mixr
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload Help  'In case help was called
  If FastParameterChange Then
    UnitMods.cmdQuit_Click
    FastParameterChange = False
  End If
End Sub

Private Sub Form_Load()
'**********************
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  ' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "MIXR" Then
    ' Use default data
    CURRMODELDATA(ID).Model = "MIXR"
    CURRMODELDATA(ID).NOPAR = 0
  End If
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub
