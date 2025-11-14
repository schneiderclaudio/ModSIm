Attribute VB_Name = "mdlViewFile"
Option Explicit

Public EchoFileName As String
Public ViewPage(0 To 9) As Form
Public ViewPageNumber As Integer
Public ViewPageNumberList(0 To 9) As Boolean

Public Function SetUpViewFile() As Integer
'*****************************************
  Dim LinesFromFile As String
  Dim ReturnFile As String
  Dim LengthOfFile As Long
  Dim I As Integer
  
  On Error GoTo ErrHandler
  'Look for the first vacant page.
  ViewPageNumber = 0
  For I = 9 To 1 Step -1
    If ViewPageNumberList(I) = False Then
      ViewPageNumber = I
    End If
  Next I
  SetUpViewFile = 0
  If ViewPageNumber = 0 Then
    MESSAGE = "No more than 9 files can be open at once.  Delete one or more files before continuing"
    MsgBox MESSAGE, vbExclamation, "ERROR"
    Exit Function
  End If
  Set ViewPage(ViewPageNumber) = New ViewFile
  ViewPageNumberList(ViewPageNumber) = True

  ViewPage(ViewPageNumber).Caption = CStr(ViewPageNumber) & " " & EchoFileName
  
  ReturnFile = Dir(RTrim(JobPath) + EchoFileName)
  If ReturnFile = "" Then
    MsgBox "Could not find the requested file", 16, "ERROR"
    Exit Function
  End If
 
  Open RTrim(JobPath) + EchoFileName For Input As #36
  ViewPage(ViewPageNumber).Text1.text = ""
  LengthOfFile = LOF(36)
  If LengthOfFile > 60000 Then
    MsgBox "The requested file is too long for this window" & vbCrLf & "Open file " & RTrim(JobPath) + EchoFileName & " in an external text editor such as WordPad", vbExclamation, "Suggestion"
    Close #36
    Exit Function
  End If
  
  LinesFromFile = Input(LOF(36), #36)
  Close #36
  ViewPage(ViewPageNumber).Text1.text = LinesFromFile
  ViewPage(ViewPageNumber).LblDate.Caption = FileDateTime(RTrim(JobPath) + EchoFileName)
  SetUpViewFile = ViewPageNumber
  
  If EchoFileName = "TEARS.OUT" Then
    ViewPage(ViewPageNumber).MnuClearTears.Visible = True
  End If
  Exit Function
  
ErrHandler:
  If Err.Number = 7 Then
    MsgBox "The requested file is too long for this window" & vbCrLf & "Open file " & RTrim(JobPath) + EchoFileName & " in an external text editor such as WordPad", vbExclamation, "Suggestion"
  Else
    MsgBox "Error when setting up file to view", 16, "ERROR"
    End If
  Close #36
  Exit Function
End Function

