VERSION 5.00
Begin VB.Form PSD 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Size distribution"
   ClientHeight    =   7890
   ClientLeft      =   150
   ClientTop       =   720
   ClientWidth     =   4020
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7890
   ScaleWidth      =   4020
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox SetName 
      Height          =   375
      Left            =   120
      TabIndex        =   58
      Text            =   "Name"
      Top             =   480
      Width           =   3855
   End
   Begin VB.CommandButton cmdAccept 
      Caption         =   "Accept"
      Height          =   375
      Left            =   2880
      TabIndex        =   56
      Top             =   7440
      Width           =   975
   End
   Begin VB.CommandButton CmdCancel 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   2880
      TabIndex        =   55
      Top             =   6960
      Width           =   975
   End
   Begin VB.TextBox NClasses 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   2280
      TabIndex        =   53
      Top             =   1200
      Width           =   855
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   25
      Left            =   1080
      TabIndex        =   52
      Top             =   7320
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   1080
      TabIndex        =   51
      Top             =   7080
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   1080
      TabIndex        =   50
      Top             =   6840
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   1080
      TabIndex        =   49
      Top             =   6600
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   1080
      TabIndex        =   48
      Top             =   6360
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   1080
      TabIndex        =   47
      Top             =   6120
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   1080
      TabIndex        =   46
      Top             =   5880
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   1080
      TabIndex        =   45
      Top             =   5640
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   1080
      TabIndex        =   44
      Top             =   5400
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   1080
      TabIndex        =   43
      Top             =   5160
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   1080
      TabIndex        =   42
      Top             =   4920
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   1080
      TabIndex        =   41
      Top             =   4680
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   1080
      TabIndex        =   40
      Top             =   4440
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   1080
      TabIndex        =   39
      Top             =   4200
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   1080
      TabIndex        =   38
      Top             =   3960
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   1080
      TabIndex        =   37
      Top             =   3720
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   1080
      TabIndex        =   36
      Top             =   3480
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   1080
      TabIndex        =   35
      Top             =   3240
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   1080
      TabIndex        =   34
      Top             =   3000
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   1080
      TabIndex        =   33
      Top             =   2760
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   1080
      TabIndex        =   32
      Top             =   2520
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   1080
      TabIndex        =   31
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   1080
      TabIndex        =   30
      Top             =   2040
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   1080
      TabIndex        =   29
      Top             =   1800
      Width           =   800
   End
   Begin VB.TextBox cum 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   1080
      TabIndex        =   28
      Top             =   1560
      Width           =   800
   End
   Begin VB.TextBox txtPercPass 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   465
      Index           =   0
      Left            =   1080
      MultiLine       =   -1  'True
      TabIndex        =   27
      Text            =   "PSDCF.frx":0000
      Top             =   1080
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   25
      Left            =   240
      TabIndex        =   26
      Top             =   7320
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   240
      TabIndex        =   25
      Top             =   7080
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   240
      TabIndex        =   24
      Top             =   6840
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   240
      TabIndex        =   23
      Top             =   6600
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   240
      TabIndex        =   22
      Top             =   6360
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   240
      TabIndex        =   21
      Top             =   6120
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   240
      TabIndex        =   20
      Top             =   5880
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   240
      TabIndex        =   19
      Top             =   5640
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   240
      TabIndex        =   18
      Top             =   5400
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   240
      TabIndex        =   17
      Top             =   5160
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   240
      TabIndex        =   16
      Top             =   4920
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   240
      TabIndex        =   15
      Top             =   4680
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   240
      TabIndex        =   14
      Top             =   4440
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   240
      TabIndex        =   13
      Top             =   4200
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   240
      TabIndex        =   12
      Top             =   3960
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   240
      TabIndex        =   11
      Top             =   3720
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   240
      TabIndex        =   10
      Top             =   3480
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   240
      TabIndex        =   9
      Top             =   3240
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   240
      TabIndex        =   8
      Top             =   3000
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   240
      TabIndex        =   7
      Top             =   2760
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   240
      TabIndex        =   6
      Top             =   2520
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   240
      TabIndex        =   5
      Top             =   2280
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   240
      TabIndex        =   4
      Top             =   2040
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   240
      TabIndex        =   3
      Top             =   1800
      Width           =   800
   End
   Begin VB.TextBox upper_size 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   240
      TabIndex        =   2
      Top             =   1560
      Width           =   800
   End
   Begin VB.TextBox txtMeshSize 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000016&
      BorderStyle     =   0  'None
      Height          =   465
      Index           =   0
      Left            =   240
      MultiLine       =   -1  'True
      TabIndex        =   1
      Text            =   "PSDCF.frx":0010
      Top             =   1080
      Width           =   800
   End
   Begin VB.Label lblStreamNumber 
      Height          =   255
      Left            =   240
      TabIndex        =   57
      Top             =   120
      Width           =   855
   End
   Begin VB.Label Label2 
      Caption         =   "Number of sizes"
      Height          =   255
      Left            =   2280
      TabIndex        =   54
      Top             =   960
      Width           =   1335
   End
   Begin VB.Label lblStreamName 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   1200
      TabIndex        =   0
      Top             =   120
      Width           =   2535
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuClose 
         Caption         =   "Close"
         Checked         =   -1  'True
      End
   End
End
Attribute VB_Name = "PSD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CmdAccept_Click()
'****************************
  Dim I As Integer
  Open RTrim(JobPath) & "LevMarParEst.DAT" For Output As 31
  Print #31, Trim(JobFileName)
  Print #31, "FAGM"
  Print #31, "1 " & NClasses
  For I = 1 To Val(NClasses)
    Print #31, upper_size(I) & "  " & cum(I)
  Next I
  Close 31
  Me.Hide
End Sub

Private Sub CmdCancel_Click()
'****************************
Unload Me
End Sub

Private Sub Form_Load()
'**********************
  'Get the size distribution data to fit from the job file
    Dim StreamNumber As Integer, NumSizes As Integer
    Dim I As Integer
    Dim COMPSIZE As Single, Compare As Single
    Open RTrim(JobFileName) & ".siz" For Input As #34
    Line Input #34, WSTRING
    If Mid(WSTRING, 1, 8) = "END OF F" Then GoTo 9001
10:
    Line Input #34, WSTRING
    If Mid(WSTRING, 1, 10) = "END OF FIL" Then GoTo 9001
    If Mid(WSTRING, 1, 10) = "Stream    " And RTrim(Mid(WSTRING, 20)) = "Mill discharge" Then
      Me.lblStreamNumber.Caption = Mid(WSTRING, 11, 4)
      Me.lblStreamName.Caption = RTrim(Mid(WSTRING, 20))
20:
      Line Input #34, WSTRING
      If Mid(WSTRING, 1, 10) = "Size dist " Then
        PSD.NClasses = Mid(WSTRING, 11, 4)
        NumSizes = Val(Mid(WSTRING, 11, 4))
        If NumSizes > 0 Then
          For I = 1 To NumSizes
            Input #34, COMPSIZE, Compare
            Me.upper_size(I) = CStr(COMPSIZE)
            Me.cum(I) = CStr(Compare)
          Next I
        End If
      ElseIf Mid(WSTRING, 1, 10) = "Water feed" Then
        GoTo 10
      Else
        GoTo 20
      End If
    End If
    GoTo 10
9001:
  Close 34
  Exit Sub

End Sub

Private Sub mnuClose_Click()
  Me.Hide
End Sub

Private Sub upper_size_KeyPress(Index As Integer, KeyAscii As Integer)
'**********************************************************************
  If KeyAscii = 13 Then
    upper_size(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub

Private Sub Cum_KeyPress(Index As Integer, KeyAscii As Integer)
'**********************************************************************
  If KeyAscii = 13 Then
    cum(Index + 1).SetFocus
    KeyAscii = 0
  End If
End Sub
