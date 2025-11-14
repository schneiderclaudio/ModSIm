VERSION 5.00
Begin VB.Form blank 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model ????"
   ClientHeight    =   5265
   ClientLeft      =   45
   ClientTop       =   1440
   ClientWidth     =   9480
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
   LinkMode        =   1  'Source
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5265
   ScaleWidth      =   9480
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   4320
      TabIndex        =   7
      Text            =   "Text1"
      Top             =   960
      Width           =   2535
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   8400
      TabIndex        =   0
      Top             =   4440
      Width           =   960
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   7200
      TabIndex        =   1
      Top             =   4440
      Width           =   960
   End
   Begin VB.Frame Frame8 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   975
      Left            =   8160
      TabIndex        =   2
      Top             =   360
      Width           =   1215
      Begin VB.OptionButton OptDefault 
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   3
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set for this unit model"
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptCurrent 
         Caption         =   "Current"
         Height          =   240
         Left            =   120
         TabIndex        =   4
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display the data set set used in the previous simulation"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton OptNew 
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display the data set that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.Label Label1 
      Caption         =   "Label1"
      Height          =   255
      Left            =   2040
      TabIndex        =   6
      Top             =   960
      Width           =   2295
   End
End
Attribute VB_Name = "blank"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
'*****************************
  Call MakeNew(CURRMODELDATA(ID))
  UnitMods.Show
  Unload blank
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload blank
  UnitMods.Show
End Sub

Private Sub Form_Load()
'**********************
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  UnitMods.Hide
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "blank" Then
        OptCurrent.Enabled = 0
        LoadDefault
        Call MakeNew(NEWDATA)
  Else
        Call LoadNew(CURRMODELDATA(ID))
        Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub LoadDefault()
'************************
End Sub

Private Sub LoadNew(NDATA As ModelData)
'***************************************
  For I = 1 To NDATA.NOPAR
        Parameter(I - 1).TEXT = Str$(NDATA.PARAM(I))
  Next I
End Sub

Private Sub MakeNew(NDATA As ModelData)
'***************************************
  CHANGE_FLAG = 0
  NDATA.MODEL = "blank"
  NDATA.NOPAR = 11
  For I = 1 To 11
        NDATA.PARAM(I) = Val(Parameter(I - 1).TEXT)
  Next I
End Sub

Private Sub OptCurrent_Click()
'******************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Private Sub OptDefault_Click()
'******************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
'**************************
  'Must trigger a load every time NEW is switched on except when changing a text field
  If Not TypeOf Screen.ActiveControl Is TextBox Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'**********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'*************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub
