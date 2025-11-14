VERSION 5.00
Begin VB.Form Blbx 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   Caption         =   "MODSIM - Parameter selection for model BLBX"
   ClientHeight    =   1635
   ClientLeft      =   1860
   ClientTop       =   1065
   ClientWidth     =   7440
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
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1635
   ScaleWidth      =   7440
   Begin VB.CommandButton cmdAccept 
      Caption         =   "Accept"
      Height          =   495
      Left            =   4920
      TabIndex        =   2
      Top             =   720
      Width           =   855
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Cancel"
      Height          =   495
      Left            =   6240
      TabIndex        =   1
      Top             =   720
      Width           =   960
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   " This model requires no parameters"
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
      Left            =   360
      TabIndex        =   0
      Top             =   240
      Width           =   6600
   End
End
Attribute VB_Name = "Blbx"
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
  Call MakeNew(CURRMODELDATA(ID))
  Unload Me
End Sub

Private Sub CmdCancel_Click()
  Unload Blbx
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

Private Sub LoadDefault()
'************************
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  CHANGE_FLAG = 0
  NDATA.MODEL = UnitMods.MODEL
  NDATA.NOPAR = 0
End Sub

Private Sub Form_Load()
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "BLBX" Then
        'OptCurrent.Enabled = 0
        LoadDefault
        Call MakeNew(NEWDATA)
  Else
        'Call LoadNew(CURRMODELDATA(ID))
        Call MakeNew(NEWDATA)
  End If
  Exit Sub

ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

