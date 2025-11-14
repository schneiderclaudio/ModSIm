VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form Help 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM Help"
   ClientHeight    =   7305
   ClientLeft      =   795
   ClientTop       =   1455
   ClientWidth     =   11520
   BeginProperty Font 
      Name            =   "Courier New"
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
   ScaleHeight     =   7305
   ScaleWidth      =   11520
   Begin VB.TextBox Text1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "Courier"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   6135
      HideSelection   =   0   'False
      Left            =   0
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Top             =   120
      Width           =   11175
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   9120
      TabIndex        =   1
      Top             =   6360
      Width           =   960
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   7680
      TabIndex        =   2
      Top             =   6360
      Width           =   960
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5160
      Top             =   6360
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
   End
End
Attribute VB_Name = "Help"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CmdAccept_Click()
'****************************
  'Turn the help check off in form UnitMods before calling the model parameter form again
  UnitMods.ChkHelp.Value = 0
  'Now call the appropriate form
  Call UnitMods.Models_DblClick
  'Will execute the next statement only when the modal parameter form is hidden or unloaded
  Unload Me
End Sub

Private Sub CmdCancel_Click()
'****************************
  Unload Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  'Clear the printer
  'Printer.KillDoc
End Sub

Private Sub Form_Load()
  Dim ALINE As String, NL As String * 2
  Dim MODEL As String * 4
  Dim IN_CONTEXT As Boolean, FOUND_CONTEXT As Boolean
  
  On Error GoTo ErrHandler
  CommonDialog1.CancelError = True
  
  Open RTrim(FileReadPath) & "MODELS.HLP" For Input As #36
  IN_CONTEXT = False
  FOUND_CONTEXT = False
  MODEL = UnitMods.MODEL
  NL = Chr$(13) + Chr$(10)
  Do While Not FOUND_CONTEXT And Not EOF(36)
    Line Input #36, ALINE$
    If ((InStr(ALINE$, ".context") = 1) And (InStr(ALINE$, MODEL) = 10)) Then
      IN_CONTEXT = True
      FOUND_CONTEXT = True
      DontShowHelp = False
      Text1.text = MODEL + NL$
      Do While IN_CONTEXT And Not EOF(36)
        Line Input #36, ALINE$
        If (InStr(ALINE$, ".context") = 1) Then
          IN_CONTEXT = False
        Else
          Text1.text = Text1.text + ALINE & NL
        End If
      Loop
    ElseIf ((InStr(ALINE$, ".context") = 1) And (InStr(ALINE$, MODEL) = 11)) Then
      MsgBox " See item " & MODEL & " in the Help menu", vbInformation, "MODSIM help"
      Close #36
      DontShowHelp = True
      Exit Sub
    End If
  Loop
  If (Not IN_CONTEXT And EOF(36)) Then
    Close #36
    Open RTrim(FileReadPath) & "UserModels.hlp" For Input As #37
    Do While Not FOUND_CONTEXT And Not EOF(37)
      Line Input #37, ALINE$
      If ((InStr(ALINE$, ".context") = 1) And (InStr(ALINE$, MODEL) = 10)) Then
        IN_CONTEXT = True
        FOUND_CONTEXT = True
        Text1.text = MODEL + NL$
        Do While IN_CONTEXT And Not EOF(37)
          Line Input #37, ALINE$
          If (InStr(ALINE$, ".context") = 1) Then
            IN_CONTEXT = False
          Else
            Text1.text = Text1.text + ALINE & NL
          End If
        Loop
      End If
    Loop
    If (Not IN_CONTEXT And EOF(37)) Then
      Text1.text = "No help is available for model " + MODEL
      Printer.Print "No help is available for model " + MODEL
    End If
  Else
    Close #36
  End If
  Close #37
  
ErrHandler:
  Exit Sub
End Sub

Private Sub mnuHelp_Click()
'**************************
  Dim RetVal As Long
  RetVal = Shell("HH " & RTrim(FileReadPath) & "MODSIM.chm", vbNormalFocus)
End Sub

Private Sub MnuPrint_Click()
'***************************
  CommonDialog1.CancelError = True
  On Error GoTo ErrHandler
  CommonDialog1.ShowPrinter
  Printer.Print Text1.text
  Printer.EndDoc
  Exit Sub

ErrHandler:
  Exit Sub
End Sub
