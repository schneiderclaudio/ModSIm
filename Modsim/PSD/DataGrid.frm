VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form DataGrid 
   Caption         =   "Import data"
   ClientHeight    =   7230
   ClientLeft      =   165
   ClientTop       =   735
   ClientWidth     =   3255
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
   ScaleHeight     =   7230
   ScaleWidth      =   3255
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtDataSetName 
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Tag             =   "DataName"
      Top             =   360
      Width           =   2655
   End
   Begin MSFlexGridLib.MSFlexGrid grdSizeData 
      Height          =   6255
      Left            =   120
      TabIndex        =   0
      Top             =   840
      Width           =   2415
      _ExtentX        =   4260
      _ExtentY        =   11033
      _Version        =   393216
      Rows            =   25
      FixedCols       =   0
   End
   Begin VB.Label Label1 
      Caption         =   "Data set name"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   1215
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuAccept 
         Caption         =   "Accept data"
      End
      Begin VB.Menu mnuClose 
         Caption         =   "Close"
      End
      Begin VB.Menu mnuPrint 
         Caption         =   "Print"
         Shortcut        =   ^P
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuCut 
         Caption         =   "Cut"
         Shortcut        =   ^X
      End
      Begin VB.Menu mnuPaste 
         Caption         =   "Paste"
         Shortcut        =   ^V
      End
   End
End
Attribute VB_Name = "DataGrid"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub MnuAccept_Click()
'****************************
PSD.SetName.text = txtDataSetName.text
I = 1
Do
  grdSizeData.Row = I
  grdSizeData.Col = 0
  PSD.upper_size(I - 1) = grdSizeData.text
  grdSizeData.Col = 1
  PSD.cum(I) = grdSizeData.text
  I = I + 1
Loop While grdSizeData.text <> ""
PSD.NClasses = CStr(I - 1)
Unload Me
PSD.Show
End Sub

Private Sub mnuClose_Click()
'***************************
Unload Me
End Sub

Private Sub mnuCopy_Click()
'**************************
'Copies selected text to the clipboard
  Clipboard.Clear
  Clipboard.SetText grdSizeData.Clip
End Sub

Private Sub MnuPaste_Click()
'**************************
'Paste text from the clipboard
  Dim text As String, newText As String, CHAR As String * 1
  text = Clipboard.GetText
  newText = ""
  For I = 1 To Len(text)
    CHAR = Mid(text, I, 1)
    If CHAR <> vbLf Then
      newText = newText & CHAR
    End If
  Next I
  grdSizeData.Clip = newText
End Sub

Private Sub MnuPrint_Click()
'***************************
PrintForm
End Sub

