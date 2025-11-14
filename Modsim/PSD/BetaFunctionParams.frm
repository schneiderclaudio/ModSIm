VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form BetaFunctionParams 
   Caption         =   "Incomplete beta function"
   ClientHeight    =   3195
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   9390
   LinkTopic       =   "Form1"
   ScaleHeight     =   3195
   ScaleWidth      =   9390
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtAnswer 
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   4440
      TabIndex        =   12
      Top             =   840
      Width           =   1215
   End
   Begin VB.TextBox txtXValue 
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   4440
      TabIndex        =   11
      Top             =   600
      Width           =   1215
   End
   Begin VB.CheckBox chkSpecificValue 
      Caption         =   "Calculate a specific value"
      Height          =   255
      Left            =   3240
      TabIndex        =   8
      Top             =   360
      Width           =   2295
   End
   Begin MSFlexGridLib.MSFlexGrid MSFlexGrid1 
      Height          =   615
      Left            =   240
      TabIndex        =   7
      Top             =   1440
      Width           =   8895
      _ExtentX        =   15690
      _ExtentY        =   1085
      _Version        =   393216
      Cols            =   12
      BorderStyle     =   0
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      Height          =   615
      Left            =   8280
      TabIndex        =   6
      Top             =   2400
      Width           =   855
   End
   Begin VB.CommandButton cmdCalculate 
      Caption         =   "Show values"
      Height          =   615
      Left            =   6720
      TabIndex        =   5
      Top             =   2400
      Width           =   1095
   End
   Begin VB.Frame Frame1 
      Caption         =   "Parameters for the beta function"
      Height          =   975
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   2535
      Begin VB.TextBox Parameter 
         Height          =   285
         Index           =   1
         Left            =   960
         TabIndex        =   4
         Top             =   600
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Height          =   285
         Index           =   0
         Left            =   960
         TabIndex        =   2
         Top             =   240
         Width           =   1215
      End
      Begin VB.Label Label1 
         Caption         =   "beta"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   3
         Top             =   600
         Width           =   615
      End
      Begin VB.Label Label1 
         Caption         =   "alpha"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   1
         Top             =   240
         Width           =   615
      End
   End
   Begin VB.Label Label3 
      Caption         =   "Beta function"
      Height          =   225
      Left            =   3240
      TabIndex        =   10
      Top             =   840
      Width           =   975
   End
   Begin VB.Label Label2 
      Caption         =   "x"
      Height          =   225
      Left            =   3240
      TabIndex        =   9
      Top             =   600
      Width           =   975
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuClose 
         Caption         =   "Close"
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
   End
End
Attribute VB_Name = "BetaFunctionParams"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdCalculate_Click()
'*******************************
  Dim I As Integer
  Dim Alpha As Double, beta As Double, P(11) As Double, X(11) As Double
  Dim XX As Double
  
  On Error GoTo ErrHandler
  
  If Parameter(0).text = "" Then
    MsgBox "Value for alpha is missing", vbCritical, "ERROR"
    Exit Sub
  Else
    Alpha = Val(Parameter(0).text)
  End If
  If Parameter(1).text = "" Then
    MsgBox "Value for beta is missing", vbCritical, "ERROR"
    Exit Sub
  Else
    beta = Val(Parameter(1).text)
  End If
  
  If Alpha <= 0 Then
    MsgBox "alpha must be greater than zero", vbCritical, "ERROR"
    Exit Sub
  End If
  If beta <= 0 Then
    MsgBox "beta must be greater than zero", vbCritical, "ERROR"
    Exit Sub
  End If
  
MSFlexGrid1.Col = 0
MSFlexGrid1.Row = 0
MSFlexGrid1.text = "x"
MSFlexGrid1.Row = 1
MSFlexGrid1.text = "Beta function"
  For I = 0 To 10
    X(I + 1) = 0.1 * I
    P(I + 1) = BetaFunction(X(I + 1), Alpha, beta)
    MSFlexGrid1.Col = I + 1
    MSFlexGrid1.Row = 0
    MSFlexGrid1.text = Format(X(I + 1), "0.000")
    MSFlexGrid1.Row = 1
    MSFlexGrid1.text = Format(P(I + 1), "0.00000")
  Next I
  
  If chkSpecificValue = vbChecked Then
    XX = Val(txtXValue.text)
    If XX < 0# Or XX > 1# Then
      MsgBox " X must be between 0 and 1", vbCritical, "WARNING"
      txtXValue.SetFocus
      Exit Sub
    Else
      txtAnswer.text = Format(BetaFunction(XX, Alpha, beta), "0.00000")
    End If
  End If
  Exit Sub
ErrHandler:
  MsgBox "Error when attempting to evaluate the beta function." & vbCrLf & "Check your parameters", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub cmdClose_Click()
'***************************
Unload Me
End Sub

Private Sub Form_Load()
'**********************
Dim I As Integer
MSFlexGrid1.FixedRows = 0
MSFlexGrid1.ColWidth(0) = 1100
For I = 0 To 10
  MSFlexGrid1.ColWidth(I + 1) = 700
Next I
End Sub

Private Sub mnuClose_Click()
'***************************
Unload Me
End Sub

Private Sub mnuCopy_Click()
'**************************
'Copies selected text to the clipboard
  Clipboard.Clear
  Clipboard.SetText MSFlexGrid1.Clip
End Sub

Sub Parameter_Change(Index As Integer)
'*************************************
  MSFlexGrid1.Clear
End Sub
