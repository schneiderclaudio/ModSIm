VERSION 5.00
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "MSINET.OCX"
Begin VB.Form FTP 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Internet access "
   ClientHeight    =   8415
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6885
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   8415
   ScaleWidth      =   6885
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Height          =   1695
      Left            =   120
      TabIndex        =   8
      Top             =   120
      Width           =   6615
      Begin VB.CommandButton cmdConnect 
         Caption         =   "Connect"
         Height          =   495
         Left            =   5640
         TabIndex        =   3
         Top             =   840
         Width           =   855
      End
      Begin VB.TextBox txtURL 
         Height          =   285
         IMEMode         =   3  'DISABLE
         Index           =   2
         Left            =   1200
         MaxLength       =   128
         PasswordChar    =   "*"
         TabIndex        =   2
         Top             =   1200
         Width           =   3615
      End
      Begin VB.TextBox txtURL 
         Height          =   285
         Index           =   1
         Left            =   1200
         TabIndex        =   1
         Top             =   840
         Width           =   3615
      End
      Begin VB.TextBox txtURL 
         Height          =   285
         Index           =   0
         Left            =   1200
         TabIndex        =   0
         Top             =   240
         Width           =   5055
      End
      Begin VB.Label lblURL 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Password"
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   11
         Top             =   1200
         Width           =   855
      End
      Begin VB.Label lblURL 
         BackColor       =   &H00C0C0C0&
         Caption         =   "User name"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   10
         Top             =   840
         Width           =   855
      End
      Begin VB.Label lblURL 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Remote host"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   9
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Cancel"
      Height          =   495
      Left            =   5760
      TabIndex        =   6
      Top             =   3000
      Width           =   975
   End
   Begin VB.CommandButton cmdLoad 
      Caption         =   "Load job"
      Height          =   495
      Left            =   5760
      TabIndex        =   5
      Top             =   2280
      Width           =   975
   End
   Begin VB.ListBox lstDir 
      Height          =   4545
      Left            =   120
      TabIndex        =   4
      ToolTipText     =   "Double click to select"
      Top             =   1920
      Width           =   5055
   End
   Begin InetCtlsObjects.Inet itcMain 
      Left            =   6120
      Top             =   5640
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
   End
   Begin VB.Label lblDiag 
      Height          =   1575
      Left            =   120
      TabIndex        =   7
      Top             =   6600
      Width           =   6495
   End
End
Attribute VB_Name = "FTP"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private sCurrentDir As String
Private sFileName As String
Private RequestedFile As String
Dim mstrTempDir As String
'API function
Private Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" _
    (ByVal nBufferLength As Long, _
    ByVal lpBuffer As String) As Long
Private Sub CmdCancel_Click()
'****************************
  On Error GoTo ErrHandler:
  Screen.MousePointer = vbDefault
  Me.itcMain.Execute , "QUIT"
  Unload Me
  Exit Sub
  
ErrHandler:
  Unload Me
  Exit Sub
End Sub

Private Sub cmdConnect_Click()
'*****************************
  On Error GoTo ErrHandler
  Me.itcMain.RemoteHost = txtURL(0).text
  'Me.itcMain.URL = txtURL(0).TEXT
  Me.itcMain.UserName = txtURL(1).text
  Me.itcMain.Password = txtURL(2).text
  Screen.MousePointer = vbHourglass
  Me.itcMain.Execute , "DIR "
  PreviousURL = txtURL(0).text
  PreviousUsername = txtURL(1).text
  
  Exit Sub
    
ErrHandler:
  lblDiag.Caption = "Could not connect to the Internet"
  Screen.MousePointer = vbDefault
  Exit Sub

End Sub

Private Sub Form_Load()
'**********************
  Dim I As Integer
  
  On Error GoTo ErrHandler
  'We want this to run only once.

  If Me.itcMain.StillExecuting Then
    MsgBox "Previous transfer still executing", vbInformation, "Information"
    Exit Sub
  End If
  
  'Check for spaces in the job path
  I = InStr(1, JobPath, " ", 1)
  If I = 0 Then
    mstrTempDir = JobPath
  Else
    'Get Windows temporary file path
    Dim lngLen As Long
    lngLen = 144
    mstrTempDir = SPACE(lngLen)
    lngLen = GetTempPath(lngLen, mstrTempDir)
    mstrTempDir = Left(mstrTempDir, lngLen)
    'Check for spaces in the temporary directory
    I = InStr(1, mstrTempDir, " ", 1)
    If I <> 0 Then
      MsgBox "A suitable temporary directory is not available. Please contact an instructor", vbExclamation, "You have a problem"
      Exit Sub
    End If
  End If

  sFileName = ""
  RequestedFile = ""
  DoEvents
  cmdLoad.Enabled = False
  Me.itcMain.Protocol = icFTP
  Me.itcMain.AccessType = icUseDefault
  
      
  If StudentVersion Or DemoVersion Then
    For I = 0 To 2
      lblURL(I).Enabled = False
      txtURL(I).Enabled = False
    Next I
    cmdConnect.Enabled = False
    Frame1.Enabled = False
    'Me.itcMain.RemoteHost = "ftp.mines.utah.edu"
    'Me.itcMain.UserName = "ftp"
    'Me.itcMain.Password = "mete6010@webct"
    Screen.MousePointer = vbHourglass
    
    On Error GoTo errOpenURL
    RequestedFile = "/pub/cmt/modsim/"
    'Me.itcMain.Execute , "DIR " & RequestedFile
    Me.itcMain.URL = "ftp://ftp.mines.utah.edu"
    Me.itcMain.Execute , "DIR /pub/cmt/modsim/*.PAK"
    txtURL(0).text = Me.itcMain.URL
  ElseIf ProfessionalVersion Or AcademicVersion Then
    For I = 0 To 2
      lblURL(I).Enabled = True
      txtURL(I).Enabled = True
    Next I
    txtURL(0) = PreviousURL
    txtURL(1) = PreviousUsername
    cmdConnect.Enabled = True
  End If
  
  Exit Sub
  
errOpenURL:
  Select Case Err.Number
        Case icBadUrl
            MsgBox "Bad address. Please reenter."
        Case icConnectFailed, icConnectionAborted, icCannotConnect
            MsgBox "Unable to connect to network."
        Case icInetTimeout
            MsgBox "Connection timed out."
        Case icExecuting
            'Cancel previous request
            itcMain.Cancel
            'Check whether cancel worked
            If itcMain.StillExecuting Then
                lblDiag.Caption = "Couldn't cancel request."
            'Resubmit current request
            Else
                Resume
            End If
        Case Else
            lblDiag.Caption = CStr(Err.Number) & " " & Err.Description
  End Select
  Exit Sub
ErrHandler:
  lblDiag.Caption = "Could not connect to the Internet"
  Screen.MousePointer = vbDefault
  Exit Sub
End Sub


Private Sub itcMain_StateChanged(ByVal State As Integer)
'*******************************************************
  Dim sChunk As String
  Dim sText As String

  Select Case State
    Case icResolvingHost, icRequesting, icRequestSent
      lblDiag.Caption = "Searching ....."
      
    Case icHostResolved
      lblDiag.Caption = "Found host"
      
    Case icResponseCompleted
      sText = ""
      Do
        sChunk = Me.itcMain.GetChunk(1024, icString)
        If Len(sChunk) = 0 Then Exit Do
        sText = sText & sChunk
      Loop
      If sText <> "" Then
        lblDiag.Caption = "Transfer completed"
        loadlist sText
        cmdLoad.Enabled = True
      Else
        Call MODSIM.UnpackJobFile(sFileName)
        lblDiag.Caption = "Packed job has been loaded."
        Screen.MousePointer = vbDefault
        Unload Me
      End If
        
    Case icConnecting, icConnected
      lblDiag.Caption = "Connecting"
      
    Case icDisconnecting, icDisconnected
      lblDiag.Caption = "disconnected"
      
    Case icError
      lblDiag.Caption = "ERROR " & CStr(itcMain.ResponseCode) & " " & itcMain.ResponseInfo
      Screen.MousePointer = vbDefault
      
  End Select
End Sub


Private Sub loadlist(sDir As String)
'***********************************
  Dim lctr As Long, lPos As Long
  Dim sContents() As String
  
  sContents() = Split(sDir, vbCrLf)
  lPos = 0
  For lctr = 0 To UBound(sContents) - 1
    If sContents(lctr) <> "" Then
      If StudentVersion And Right(sContents(lctr), 1) = "/" Then
      Else
        Me.lstDir.AddItem sContents(lctr), lPos
        lPos = lPos + 1
      End If
    End If
  Next lctr
  Screen.MousePointer = vbDefault

  If Me.lstDir.ListCount = 0 Then
    MsgBox "No packed job files were found", vbExclamation
  End If
  
End Sub

Private Sub lstDir_DblClick()
'****************************
  Call cmdLoad_click
  
End Sub

Private Sub cmdLoad_click()
'**************************
  Dim I As Integer
  On Error GoTo ErrHandler

  If Me.itcMain.StillExecuting Then
    MsgBox "A request is still processing", vbExclamation
    Exit Sub
  End If
  RequestedFile = RequestedFile & Trim(Me.lstDir.text)
  Me.lstDir.Clear
  Screen.MousePointer = vbHourglass
  If Right(RequestedFile, 1) = "/" Then
    Me.itcMain.RemoteHost = txtURL(0).text
    Me.itcMain.UserName = txtURL(1).text
    Me.itcMain.Password = txtURL(2).text
    Me.itcMain.Execute , "DIR " & RequestedFile
  ElseIf Right(RequestedFile, 4) = ".PAK" Then
    sFileName = Trim(mstrTempDir) & "Temp.PAK"
    If Dir(sFileName) <> "" Then Kill sFileName
    Me.itcMain.URL = "ftp.mines.utah.edu"
    Me.itcMain.Execute , "GET " & RequestedFile & " " & sFileName
    cmdLoad.Enabled = False
  Else
    MsgBox "Please select a packed MODSIM file or another directory", vbCritical, "ERROR"
  End If
  Exit Sub
  
ErrHandler:
  Exit Sub

End Sub
