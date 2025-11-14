VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form MGSheet 
   AutoRedraw      =   -1  'True
   Caption         =   "METGRAPH"
   ClientHeight    =   8010
   ClientLeft      =   480
   ClientTop       =   1530
   ClientWidth     =   11400
   FillColor       =   &H00C0C0C0&
   FillStyle       =   0  'Solid
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
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   534
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   760
   Begin VB.CheckBox chkScreenCoordinates 
      Caption         =   "Show screen coordinates"
      Height          =   255
      Left            =   2040
      TabIndex        =   7
      Top             =   7320
      Width           =   2295
   End
   Begin VB.PictureBox pctMG 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00E0E0E0&
      FillColor       =   &H00FFFFFF&
      Height          =   7260
      Left            =   120
      MousePointer    =   2  'Cross
      ScaleHeight     =   480
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   740
      TabIndex        =   0
      Top             =   0
      Width           =   11160
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   10800
      Top             =   7320
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label lblGraphCoordY 
      Height          =   255
      Left            =   960
      TabIndex        =   6
      Top             =   7560
      Width           =   855
   End
   Begin VB.Label lblGraphCoordX 
      Height          =   255
      Left            =   120
      TabIndex        =   5
      Top             =   7560
      Width           =   735
   End
   Begin VB.Label Labelgr 
      Caption         =   "Graph coordinates"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   7320
      Width           =   1455
   End
   Begin VB.Label LblScreenCoordinateY 
      Height          =   255
      Left            =   5400
      TabIndex        =   3
      Top             =   7560
      Visible         =   0   'False
      Width           =   615
   End
   Begin VB.Label LblScreenCoordinateX 
      Height          =   255
      Left            =   4560
      TabIndex        =   2
      Top             =   7560
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblScreenCoordinates 
      Caption         =   "Screen coordinates"
      Height          =   255
      Left            =   4560
      TabIndex        =   1
      Top             =   7320
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.Menu File 
      Caption         =   "File"
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu PSSave 
         Caption         =   "Save as PostScript file"
      End
      Begin VB.Menu MakeEPSIFile 
         Caption         =   "Save as Encapsulated PostScript file"
      End
      Begin VB.Menu mnuEnhancedmetafile 
         Caption         =   "Save as enhanced metafile"
         Visible         =   0   'False
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "MGSheet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim RBCursorOn As Boolean
Dim RBCursorOrigin As Boolean
Dim RectCursorOn As Boolean
Dim RectCursorOrigin As Boolean
Dim OriginX As Single
Dim OriginY As Single
Dim MouseLocationX As Single
Dim MouseLocationY As Single
Dim OldMouseLocationX As Single
Dim OldMouseLocationY As Single
Dim CornerX As Single
Dim CornerY As Single
Dim OldCornerX As Single
Dim OldCornerY As Single
Public ActualBackColor As Long

Public DisplayCoordinates As Boolean
Public RightButtonEvent As Boolean

Private Sub Form_Activate()
'**************************
'Reset the pagenumber information when the page is activated
PageNumber = Val(Mid(Me.Caption, 15, 2))
If PagePSBufferSaved(PageNumber) Then
  Call ReadPSBuffer
  PagePSBufferSaved(PageNumber) = False
End If
'Allow the PostScript image to build continuously
PSFlagOn = True
End Sub

Private Sub Form_Deactivate()
'****************************
'Save the PostScript buffer for this page.
PageNumber = Val(Mid(Me.Caption, 15, 2))
Call SavePSBuffer
End Sub

Private Sub Form_Load()
'**********************
  RBCursorOn = False
  RBCursorOrigin = False
  RectCursorOn = False
  RectCursorOrigin = False
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'****************************************************************************************
  Dim MESSAGE As String
  If RBCursorOn Or RectCursorOn Then
    OriginX = X
    OriginY = Y
    OldMouseLocationX = OriginX
    OldMouseLocationY = OriginY
  End If
  If RBCursorOn Then
    RBCursorOrigin = True
  End If
  If RectCursorOn Then
    RectCursorOrigin = True
    OldCornerX = OriginX
    OldCornerY = OriginY
  End If
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
'****************************************************************************************
  PageNumber = Val(Mid(Me.Caption, 15, 2))
  If RBCursorOn Or RectCursorOn Then
    MouseLocationX = X
    MouseLocationY = Y
    DrawMode = 7
    ForeColor = RGB(0, 128, 128)
    If RBCursorOrigin Then
      Line (OriginX, OriginY)-(OldMouseLocationX, OldMouseLocationY)
      Line (OriginX, OriginY)-(MouseLocationX, MouseLocationY)
    End If
    If RectCursorOrigin Then
      Line (OriginX, OriginY)-(OldCornerX, OldCornerY)
      Line (OldCornerX, OldCornerY)-(OldMouseLocationX, OldMouseLocationY)
      If Button And 1 Then
        CornerX = OriginX
        CornerY = MouseLocationY
      End If
      If Button And 2 Then
        CornerX = MouseLocationX
        CornerY = OriginY
      End If
      Line (OriginX, OriginY)-(CornerX, CornerY)
      Line (CornerX, CornerY)-(MouseLocationX, MouseLocationY)
      OldCornerX = CornerX
      OldCornerY = CornerY
    End If
    OldMouseLocationX = MouseLocationX
    OldMouseLocationY = MouseLocationY
  End If
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
'**************************************************************************************
  If RBCursorOrigin Then
    RBCursorOn = False
    RBCursorOrigin = False
  End If
  If RectCursorOrigin Then
    RectCursorOn = False
    RectCursorOrigin = False
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
Dim dl As Long
Dim ReturnFile As String, FileName As String
  'Get the page number of the displayed page
  PageNumber = Mid(Me.Caption, 15, 2)
  'Close the associated metafile on disk
  'Check for existing file
'  FileName = RTrim(JobPath) & "Metafile" & Format(PageNumber, "00") & ".emf"
'  ReturnFile = Dir(FileName)
'  If ReturnFile <> "" Then
'    MsgBox "Metafile " & FileName & " exists.", vbExclamation, "Information"
'  End If
  
  'Close the metafile if it is open
  EnhMetafileHnd(PageNumber) = CloseEnhMetaFile(EnhMetafileDC(PageNumber))
  If EnhMetafileHnd(PageNumber) <> 0 Then
    dl = DeleteEnhMetaFile(EnhMetafileHnd(PageNumber))
    If dl <> 0 Then emfsdeleted = emfsdeleted + 1
  End If
  Set MGPage(PageNumber) = Nothing
  PageNumberList(PageNumber) = False
  PagePSBufferSaved(PageNumber) = False
  PageNumber = 0
  'Unload Me
End Sub

Private Sub MakeEPSIFile_Click()
'*******************************
'Save graphics as a PostScript file
  Dim I As Integer, PgNumber As Integer
  Dim FileName As String, ReturnFile As String
  Dim resp As Integer
  Dim PSFileName As String
  On Error GoTo ErrHandler

  'Get the page number of the displayed page
  PageNumber = Mid(Me.Caption, 15, 2)
  If MAKE_EPSI() <> 0 Then
    CommonDialog1.Filter = "Encapsulated postScript files (.eps)|*.eps"
    CommonDialog1.FilterIndex = 1
    CommonDialog1.Flags = 4
    CommonDialog1.ShowSave
    PSFileName = CommonDialog1.FileName
    'Strip any file extensions and add the .eps
    I = InStr(PSFileName, ".")
    If I > 0 Then
       PSFileName = Mid(PSFileName, 1, I - 1) & ".eps"
    Else
       PSFileName = PSFileName & ".eps"
    End If
    'Check for existing file
    ReturnFile = Dir(PSFileName)
    If ReturnFile <> "" Then
      resp = MsgBox("A file with name " & PSFileName & " already exists.  Overwrite?", vbExclamation + vbOKCancel, "WARNING")
      If resp = vbCancel Then Exit Sub
    End If
    FileCopy RTrim(JobPath) & "EPSI" & Format(PageNumber, "00") & ".eps", PSFileName
  End If
  Exit Sub
ErrHandler:
  Exit Sub
End Sub

Private Sub mnuClose_Click()
'***************************
'Call Form_Unload(0)
Unload Me
End Sub

Private Sub mnuEnhancedmetafile_Click()
'**************************************
'Save graphics as an enhanced metafile
  Dim I As Integer
  Dim FileName As String, ReturnFile As String
  Dim resp As Integer
  Dim dl As Long
  On Error GoTo ErrHandlerOpen

  'Get the page number of the displayed page
  PageNumber = Mid(Me.Caption, 15, 2)
  'Close the metafile if it is open
  EnhMetafileHnd(PageNumber) = CloseEnhMetaFile(EnhMetafileDC(PageNumber))
  If EnhMetafileHnd(PageNumber) <> 0 Then
    dl = DeleteEnhMetaFile(EnhMetafileHnd(PageNumber))
    If dl <> 0 Then emfsdeleted = emfsdeleted + 1
  End If
  
  On Error GoTo ErrHandler
  CommonDialog1.Filter = "Enhanced metafiles (.emf)|*.emf"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  FileName = CommonDialog1.FileName
  'Strip any file extensions and add the .emf
  I = InStr(FileName, ".")
  If I > 0 Then
     FileName = Mid(FileName, 1, I - 1) & ".emf"
  Else
     FileName = FileName & ".emf"
  End If
  'Check for existing file
  ReturnFile = Dir(FileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A file with name " & FileName & " already exists.  Overwrite?", vbExclamation + vbOKCancel, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  FileCopy RTrim(JobPath) & "metafile" & Format(PageNumber, "00") & ".emf", FileName
  Exit Sub
  
ErrHandler:
  Exit Sub
ErrHandlerOpen:
  MsgBox "Could not save metafile", vbCritical, "ERROR"
  Exit Sub

End Sub

Private Sub MnuPrint_Click()
PrintForm
End Sub

Private Sub pctMG_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************
Dim SX As Single, SY As Single, ScX As Single, ScY As Single
If Button = vbKeyRButton Then
  If RightButtonEvent Then
    On Error GoTo Errhandler1
    ScX = (X - Me.pctMG.ScaleLeft) * 1024 / Me.pctMG.ScaleWidth
    ScY = (Me.pctMG.ScaleHeight - Y) * 780 / Me.pctMG.ScaleHeight
    Call CONVERTSCRN_TRANS(ScX, ScY, SX, SY)
    Call RightButtonProc("Down", SX, SY)
  End If
ElseIf DisplayCoordinates Then
  On Error GoTo ErrHandler2
  ScX = (X - Me.pctMG.ScaleLeft) * 1024 / Me.pctMG.ScaleWidth
  ScY = (Me.pctMG.ScaleHeight - Y) * 780 / Me.pctMG.ScaleHeight
  If chkScreenCoordinates Then
    lblScreenCoordinates.Visible = True
    LblScreenCoordinateX.Visible = True
    LblScreenCoordinateY.Visible = True
    LblScreenCoordinateX.Caption = Format(ScX, "####.0")
    LblScreenCoordinateY.Caption = Format(ScY, "###.0")
  Else
    lblScreenCoordinates.Visible = False
    LblScreenCoordinateX.Visible = False
    LblScreenCoordinateY.Visible = False
  End If
  Call CONVERTSCRN_TRANS(ScX, ScY, SX, SY)
  lblGraphCoordX.Caption = Format(SX, "#.00E+00")
  lblGraphCoordY.Caption = Format(SY, "#.00E+00")
End If
Exit Sub

Errhandler1:
  MsgBox "Right button event is not active in this application", vbExclamation, "WARNING"
  Exit Sub
ErrHandler2:
  MsgBox "Could not determine coordinates", vbExclamation, "WARNING"
  Exit Sub
End Sub

Private Sub pctMG_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************
Dim SX As Single, SY As Single, ScX As Single, ScY As Single
If DisplayCoordinates Then
  PageNumber = Val(Mid(Me.Caption, 15, 2))
  ScX = (X - Me.pctMG.ScaleLeft) * 1024 / Me.pctMG.ScaleWidth
  ScY = (Me.pctMG.ScaleHeight - Y) * 780 / Me.pctMG.ScaleHeight
  If chkScreenCoordinates Then
    lblScreenCoordinates.Visible = True
    LblScreenCoordinateX.Visible = True
    LblScreenCoordinateY.Visible = True
    LblScreenCoordinateX.Caption = Format(ScX, "####.0")
    LblScreenCoordinateY.Caption = Format(ScY, "###.0")
  Else
    lblScreenCoordinates.Visible = False
    LblScreenCoordinateX.Visible = False
    LblScreenCoordinateY.Visible = False
  End If
  Call CONVERTSCRN_TRANS(ScX, ScY, SX, SY)
  lblGraphCoordX.Caption = Format(SX, "#.00E+00")
  lblGraphCoordY.Caption = Format(SY, "#.00E+00")
End If
End Sub

Private Sub pctMG_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************
Dim SX As Single, SY As Single, ScX As Single, ScY As Single
If Button = vbKeyRButton Then
  If RightButtonEvent Then
    ScX = (X - Me.pctMG.ScaleLeft) * 1024 / Me.pctMG.ScaleWidth
    ScY = (Me.pctMG.ScaleHeight - Y) * 780 / Me.pctMG.ScaleHeight
    Call CONVERTSCRN_TRANS(ScX, ScY, SX, SY)
    Call RightButtonProc("Up", SX, SY)
  End If
End If
End Sub

Private Sub PSSave_Click()
'*************************
'Save graphics as a PostScript file
  Dim I As Integer, PgNumber As Integer
  Dim FileName As String, ReturnFile As String
  Dim resp As Integer
  Dim PSFileName As String
  Dim sLLX As Single, sLLY As Single, sURX As Single, sURY As Single, IMAGE_SIZE As Long
  On Error GoTo ErrHandler

  'Get the page number of the displayed page
  PgNumber = Mid(Me.Caption, 15, 2)
  CommonDialog1.Filter = "PostScript files (.ps)|*.ps"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  PSFileName = CommonDialog1.FileName
  'Strip any file extensions and add the .ps
  I = InStr(PSFileName, ".")
  If I > 0 Then
     PSFileName = Mid(PSFileName, 1, I - 1) & ".ps"
  Else
     PSFileName = PSFileName & ".ps"
  End If
  'Check for existing file
  ReturnFile = Dir(PSFileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A file with name " & PSFileName & " already exists.  Overwrite?", vbExclamation + vbOKCancel, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  
  'FileCopy RTrim(JobPath) & "PSFile" & Format(PgNumber, "00") & ".ps", PSFileName
  PageNumber = PgNumber
  MGPage(PageNumber).MousePointer = 11
  MGPage(PageNumber).pctMG.MousePointer = 11
  Call GET_BOUND_BOX(sLLX, sLLY, sURX, sURY, IMAGE_SIZE)
  'Adjust the bounding box for the scale factor and the translated origin.
  If ROT = 90# Then
    IBBOX(1) = TRANS(1) * 72# / 25.4 - ScalePS(2) * sURY
    IBBOX(2) = TRANS(2) * 72# / 25.4 + ScalePS(1) * sLLX
    IBBOX(3) = TRANS(1) * 72# / 25.4 - ScalePS(2) * sLLY
    IBBOX(4) = TRANS(2) * 72# / 25.4 + ScalePS(1) * sURX
  Else
    IBBOX(1) = ScalePS(1) * sLLX + TRANS(1) * 72# / 25.4
    IBBOX(2) = ScalePS(2) * sLLY + TRANS(2) * 72# / 25.4
    IBBOX(3) = ScalePS(1) * sURX + TRANS(1) * 72# / 25.4
    IBBOX(4) = ScalePS(2) * sURY + TRANS(2) * 72# / 25.4
  End If
  Call WritePSFile(PSFileName)
  MGPage(PageNumber).MousePointer = 0
  MGPage(PageNumber).pctMG.MousePointer = 2
  Exit Sub
  
ErrHandler:
  MGPage(PgNumber).MousePointer = 0
  MGPage(PageNumber).pctMG.MousePointer = 2
  Exit Sub
End Sub




