VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form ViewFile 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "View file"
   ClientHeight    =   7680
   ClientLeft      =   435
   ClientTop       =   1125
   ClientWidth     =   11235
   BeginProperty Font 
      Name            =   "Courier"
      Size            =   9.75
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "ViewFile.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7680
   ScaleWidth      =   11235
   Begin VB.CommandButton CmdClose 
      Caption         =   "Close"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   495
      Left            =   5160
      TabIndex        =   1
      Top             =   6720
      Width           =   855
   End
   Begin VB.TextBox Text1 
      Height          =   6615
      Left            =   0
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   3  'Both
      TabIndex        =   0
      Top             =   0
      Width           =   11175
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   6840
      Top             =   6720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "File created on"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   120
      TabIndex        =   3
      Top             =   6720
      Width           =   1335
   End
   Begin VB.Label LblDate 
      BackColor       =   &H00FFFFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   1560
      TabIndex        =   2
      Top             =   6720
      Width           =   3255
   End
   Begin VB.Menu File 
      Caption         =   "File"
      Begin VB.Menu SaveAs 
         Caption         =   "Save As ....."
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
   Begin VB.Menu MnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu MnuCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu MnuCut 
         Caption         =   "Cut"
         Shortcut        =   ^X
      End
      Begin VB.Menu MnuPaste 
         Caption         =   "Paste"
         Shortcut        =   ^V
      End
   End
   Begin VB.Menu MnuClearTears 
      Caption         =   "Clear tear stream data"
      Visible         =   0   'False
   End
End
Attribute VB_Name = "ViewFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CmdClose_Click()
'**************************
  ViewPageNumberList(ViewPageNumber) = False
  Unload Me
End Sub

Private Sub Form_Activate()
'**************************
ViewPageNumber = Val(Me.Caption)
End Sub

Private Sub Form_Load()
'**********************
CommonDialog1.CancelError = True
End Sub

Private Sub MnuClearTears_Click()
'********************************
  On Error GoTo ErrHandler
  Open RTrim(JobPath) + EchoFileName For Output As #36
  Print #36, "LAST 0 0 0 0"
  Close #36
  ViewPageNumberList(ViewPageNumber) = False
  Unload Me
  Exit Sub
  
ErrHandler:
  MsgBox "Cannot open TEARS file", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub MnuClose_Click()
  ViewPageNumberList(ViewPageNumber) = False
  Unload Me
End Sub

Private Sub MnuCopy_Click()
'**************************
'Copies selected text to the clipboard
  Clipboard.Clear
  Clipboard.SetText Text1.SelText
End Sub

Private Sub MnuCut_Click()
'*************************
'Cuts selected text to the clipboard
  Clipboard.Clear
  Clipboard.SetText Text1.SelText
  Text1.SelText = ""
End Sub


Private Sub MnuPaste_Click()
'***************************
'Paste from the clipboard
  Text1.SelText = Clipboard.GetText()
End Sub


Private Sub MnuPrint_Click()
'***************************
  Dim NumCopies As Integer, I As Integer
  CommonDialog1.CancelError = True
 ' CommonDialog1.UseDevModeCopies = False
  On Error GoTo ErrHandler
  CommonDialog1.ShowPrinter
  Printer.FontName = Me.Text1.FontName
'  NumCopies = CommonDialog1.Copies
'  For I = 1 To NumCopies
'    Printer.EndDoc
'  Next I

  Printer.Print Text1.text
  TextHeight (Text1.text)
  Printer.EndDoc
  Exit Sub

ErrHandler:
  Exit Sub

End Sub

Private Sub SaveAs_Click()
'*************************
' Save file that is displayed
  Dim Ifile As Integer, ReturnFile As String, resp As Integer
  
  On Error GoTo ErrHandler
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "Text files (*.txt) | *.txt | All files | *.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.FileName = "*.txt"
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  ReturnFile = Dir(CommonDialog1.FileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A job with this name already exists.  Overwrite?", 17, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  Ifile = FreeFile
  Open Trim(CommonDialog1.FileName) For Output As #Ifile
  Print #Ifile, Text1.text
  Close (Ifile)
Exit Sub

ErrHandler:
  'User pressed Cancel button
  Exit Sub

End Sub


