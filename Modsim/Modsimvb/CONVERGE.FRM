VERSION 5.00
Begin VB.Form Converge 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify convergence parameters"
   ClientHeight    =   2700
   ClientLeft      =   1980
   ClientTop       =   2310
   ClientWidth     =   7035
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
   Icon            =   "Converge.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2700
   ScaleWidth      =   7035
   Begin VB.CheckBox ChkTears 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Start simulation from previous end point"
      Height          =   255
      Left            =   120
      TabIndex        =   11
      Top             =   1800
      Value           =   1  'Checked
      Width           =   3735
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   4680
      TabIndex        =   1
      Top             =   1680
      Width           =   960
   End
   Begin VB.TextBox ITER 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Left            =   2520
      TabIndex        =   7
      Text            =   "50"
      Top             =   1440
      Width           =   720
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Accept"
      Height          =   480
      Left            =   6000
      TabIndex        =   0
      Top             =   1680
      Width           =   960
   End
   Begin VB.Frame Frame2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Tolerance required"
      ForeColor       =   &H00000000&
      Height          =   1320
      Left            =   4560
      TabIndex        =   9
      Top             =   240
      Width           =   2400
      Begin VB.OptionButton OptTol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "0.001%"
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   15
         Top             =   960
         Value           =   -1  'True
         Width           =   1065
      End
      Begin VB.OptionButton OptTol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "0.01%"
         Height          =   240
         Index           =   3
         Left            =   1320
         TabIndex        =   14
         Top             =   600
         Width           =   840
      End
      Begin VB.OptionButton OptTol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "0.1%"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   6
         Top             =   600
         Width           =   720
      End
      Begin VB.OptionButton OptTol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "1%"
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   5
         TabStop         =   0   'False
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptTol 
         BackColor       =   &H00C0C0C0&
         Caption         =   "10%"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   4
         TabStop         =   0   'False
         Top             =   240
         Width           =   720
      End
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Convergence method"
      ForeColor       =   &H00000000&
      Height          =   960
      Left            =   120
      TabIndex        =   8
      Top             =   240
      Width           =   4320
      Begin VB.OptionButton OptMidpoint 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Midpoint"
         Height          =   255
         Left            =   2160
         TabIndex        =   13
         Top             =   600
         Width           =   1935
      End
      Begin VB.OptionButton OptBoundedWegstein 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Bounded Wegstein"
         Height          =   255
         Left            =   2160
         TabIndex        =   12
         Top             =   240
         Width           =   2055
      End
      Begin VB.OptionButton OptNewton 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Modified Newton"
         Height          =   240
         Left            =   120
         TabIndex        =   3
         Top             =   600
         Value           =   -1  'True
         Width           =   1560
      End
      Begin VB.OptionButton OptDirect 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Direct substitution"
         Height          =   240
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Top             =   240
         Width           =   1635
      End
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Maximum iterations allowed"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   10
      Top             =   1440
      Width           =   2415
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
Attribute VB_Name = "Converge"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Sub CmdAccept_Click()
'****************************
  Dim I As Integer
  If OptNewton.Value Then Sysdat.CURRENT.Conv = 3
  If OptDirect.Value = -1 Then Sysdat.CURRENT.Conv = 1
  If OptBoundedWegstein.Value Then Sysdat.CURRENT.Conv = 2
  If OptMidpoint.Value Then Sysdat.CURRENT.Conv = 4
  For I = 1 To 5
    If OptTol(I - 1).Value Then Sysdat.CURRENT.TOLM = -I
  Next I
  Sysdat.CURRENT.ITER = Val(ITER.text)
  Converge.Hide
End Sub

Private Sub CmdCancel_Click()
  Unload Converge
End Sub

Private Sub Form_Load()
'**********************
  Dim Index As Integer
  If Sysdat.CURRENT.Conv = 1 Then
    OptDirect.Value = -1
  Else
    OptNewton.Value = -1
  End If
  If Sysdat.CURRENT.TOLM < 0 And Sysdat.CURRENT.TOLM > -6 Then
    Index = -Sysdat.CURRENT.TOLM - 1
    OptTol(Index).Value = -1
  Else
    OptTol(2).Value = -1
  End If
  If Sysdat.CURRENT.ITER = 0 Then
    ITER.text = Str$(30)
  Else
    ITER.text = Str$(Sysdat.CURRENT.ITER)
  End If
End Sub

Private Sub MnuAccept_Click()
Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm
End Sub
