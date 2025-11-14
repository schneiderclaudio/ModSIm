VERSION 5.00
Begin VB.Form Splt 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Parameters for model SPLT"
   ClientHeight    =   2415
   ClientLeft      =   1845
   ClientTop       =   1335
   ClientWidth     =   7440
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
   Icon            =   "Splt.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2415
   ScaleWidth      =   7440
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   615
      Left            =   4800
      TabIndex        =   1
      Top             =   1200
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   615
      Left            =   6240
      TabIndex        =   0
      Top             =   1200
      Width           =   960
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   "This splitter has no outlet stream!"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   1560
      TabIndex        =   3
      Top             =   720
      Visible         =   0   'False
      Width           =   4200
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   "SPLT is a uniform splitter. Parameters are set automatically"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   2
      Top             =   480
      Width           =   7320
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
Attribute VB_Name = "Splt"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
    CURRMODELDATA(ID).Model = "SPLT"
    CURRMODELDATA(ID).NOPAR = 3
    CURRMODELDATA(ID).PARAM(1) = NOSPLIT(UnitMods.Unit)
    CURRMODELDATA(ID).PARAM(2) = 1 / NOSPLIT(UnitMods.Unit)
    CURRMODELDATA(ID).PARAM(3) = 1 / NOSPLIT(UnitMods.Unit)

  Unload Splt
End Sub

Private Sub CmdCancel_Click()
  Unload Splt
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
  On Error GoTo ErrHandler
  Caption = Caption & " on unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  If NOSPLIT(UnitMods.Unit) = 0 Then
    Label1.Visible = -1
    Label2.Visible = 0
    cmdAccept.Visible = 0
  End If
  Exit Sub

ErrHandler:
  Exit Sub
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
PrintForm
End Sub
