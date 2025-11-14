Attribute VB_Name = "PCMON"
Option Explicit

Private MESSAGE As String
Public pensCreated As Integer, solidbrushesCreated As Integer
Public regionsCreated As Integer, fontsCreated As Integer, emfsCreated As Integer
Public pensdeleted As Integer, brushesdeleted As Integer, regionsdeleted As Integer
Public fontsDeleted As Integer, emfsdeleted As Integer
Public objectscreated As Integer, objectsdeleted As Integer

Public MGPage(0 To 25) As Form
Public PageNumber As Integer
Public PageNumberList(0 To 25) As Boolean
Public PagePSBufferSaved(0 To 25) As Boolean
Public FirstPage As Boolean

'METGRAPH variables
Public METGRAPH_OPEN As Boolean
Public CURX As Single, CURY As Single
Public Isize As Integer        'Size of graph plotting symbols
Private LineWidth As Integer   'Width of line in pixels
Public TABLE(128) As Integer
Public Const ITOT As Integer = 1540
Public Const INUM As Integer = 42
Public START(128, 3) As Integer
Public SYMTAB(ITOT, 3) As Integer
Public SCALEX As Single, SCALEY As Single
Public GSSCALEX As Single, GSSCALEY As Single
Public TXTSZ As Single, COSRAD As Single, SINRAD As Single, Theta As Single
Public SUBLEVEL As Integer, SUPLEVEL As Integer
Public IFONT As Integer, ICHARSET As Integer

Public PSFlagOn As Boolean
Public PrinterFlagOn As Boolean
Public MetaFileFlagOn As Boolean
Public EnhMetafileDC(0 To 25) As Long
Public EnhMetafileHnd(0 To 25) As Long
Public hndMetafile As Long
Public hndPen As Long
Public OldPen As Long

Public ClipRegion As Long
Public SavedPreClipDC As Long, SavedPreClipMetafileDC As Long

Type PageFont
  Name As String
  Size As Single
  italic As Boolean
  Bold As Boolean
End Type
Public LabelFont As PageFont, Flyoutfont As PageFont
Public Const LF_FACESIZE = 32
Public Type LOGFONT
        lfHeight As Long
        lfWidth As Long
        lfEscapement As Long
        lfOrientation As Long
        lfWeight As Long
        lfItalic As Byte
        lfUnderline As Byte
        lfStrikeOut As Byte
        lfCharSet As Byte
        lfOutPrecision As Byte
        lfClipPrecision As Byte
        lfQuality As Byte
        lfPitchAndFamily As Byte
        lfFaceName(1 To LF_FACESIZE) As Byte
End Type

Public Type RECT
        Left As Long
        Top As Long
        Right As Long
        Bottom As Long
End Type

Public Type xyrcoord
  xcoord As Single
  ycoord As Single
End Type

Public Type POINTAPI
        X As Long
        Y As Long
End Type
Public Oldpoint As POINTAPI

Public Type Size
        cx As Long
        cy As Long
End Type

Public Const BLACK_BRUSH = 4
Public Const BLACK_PEN = 7
Public Const MM_TEXT = 1
Public Const MM_TWIPS = 6
Public Const MM_ISOTROPIC = 7
Public Const MM_ANISOTROPIC = 8
Public Const HORZRES = 8            '  Horizontal width in pixels
Public Const HORZSIZE = 4           '  Horizontal size in millimeters
Public Const VERTRES = 10           '  Vertical width in pixels
Public Const VERTSIZE = 6           '  Vertical size in millimeters
Public Const CCHDEVICENAME = 32
Public Const CCHFORMNAME = 32
Public Type DEVMODE
        dmDeviceName As String * CCHDEVICENAME
        dmSpecVersion As Integer
        dmDriverVersion As Integer
        dmSize As Integer
        dmDriverExtra As Integer
        dmFields As Long
        dmOrientation As Integer
        dmPaperSize As Integer
        dmPaperLength As Integer
        dmPaperWidth As Integer
        dmScale As Integer
        dmCopies As Integer
        dmDefaultSource As Integer
        dmPrintQuality As Integer
        dmColor As Integer
        dmDuplex As Integer
        dmYResolution As Integer
        dmTTOption As Integer
        dmCollate As Integer
        dmFormName As String * CCHFORMNAME
        dmUnusedPadding As Integer
        dmBitsPerPel As Long
        dmPelsWidth As Long
        dmPelsHeight As Long
        dmDisplayFlags As Long
        dmDisplayFrequency As Long
End Type

Public Const PS_SOLID = 0
Public Declare Function SelectClipRgn Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long) As Long
Public Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Public Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Public Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Public Declare Function Polygon Lib "gdi32" (ByVal hdc As Long, lppoint As POINTAPI, ByVal nCount As Long) As Long
Public Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Public Declare Function CreateFontIndirect Lib "gdi32" Alias "CreateFontIndirectA" (lpLogFont As LOGFONT) As Long

Public Declare Function CloseEnhMetaFile Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function CreateEnhMetaFile Lib "gdi32" Alias "CreateEnhMetaFileA" (ByVal hdcRef As Long, ByVal lpFileName As String, lpRect As RECT, ByVal lpDescription As String) As Long
Public Declare Function CopyEnhMetaFile Lib "gdi32" Alias "CopyEnhMetaFileA" (ByVal hemfSrc As Long, ByVal lpszFile As String) As Long
Public Declare Function CreateDC Lib "gdi32" Alias "CreateDCA" (ByVal lpDriverName As String, ByVal lpDeviceName As String, ByVal lpOutput As String, lpInitData As DEVMODE) As Long
Public Declare Function SaveDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function RestoreDC Lib "gdi32" (ByVal hdc As Long, ByVal nSavedDC As Long) As Long
Public Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long
Public Declare Function DeleteEnhMetaFile Lib "gdi32" (ByVal hemf As Long) As Long
Public Declare Function SetWindowExtEx Lib "gdi32" (ByVal hdc As Long, ByVal NX As Long, ByVal NY As Long, lpsize As Size) As Long
Public Declare Function SetWindowOrgEx Lib "gdi32" (ByVal hdc As Long, ByVal NX As Long, ByVal NY As Long, lppoint As POINTAPI) As Long
Public Declare Function SetViewportExtEx Lib "gdi32" (ByVal hdc As Long, ByVal NX As Long, ByVal NY As Long, lpsize As Size) As Long
Public Declare Function SetViewportOrgEx Lib "gdi32" (ByVal hdc As Long, ByVal NX As Long, ByVal NY As Long, lppoint As POINTAPI) As Long
Public Declare Function GetWindowExtEx Lib "gdi32" (ByVal hdc As Long, lpsize As Size) As Long
Public Declare Function SetMapMode Lib "gdi32" (ByVal hdc As Long, ByVal nMapMode As Long) As Long
Public Declare Function GetDeviceCaps Lib "gdi32" (ByVal hdc As Long, ByVal nIndex As Long) As Long
Public Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long
Public Declare Function ReleaseDC Lib "user32" (ByVal hwnd As Long, ByVal hdc As Long) As Long

Public Declare Function GetPixel Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long) As Long
Public Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, lppoint As POINTAPI) As Long
Public Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long) As Long
Public Declare Function Ellipse Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Public Declare Function TextOut Lib "gdi32" Alias "TextOutA" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, ByVal lpstring As String, ByVal nCount As Long) As Long

Sub RTAB()
'*********
'INITIALISES METGRAPH TO RUN ON A PC under Visual Basic.
'  OPEN METGRAPH
   Dim I As Integer
   If METGRAPH_OPEN Then
     response = MsgBox("METGRAPH is already open. Do you want to open it again?", 308, "WARNING")
     If response = vbNo Then Exit Sub
   End If
   METGRAPH_OPEN = True
'  Create page 1 and initialise the page number and the page number list.
   FirstPage = True
   PageNumber = 1
   For I = 1 To 25
     PageNumberList(I) = False
     PagePSBufferSaved(I) = False
   Next I
   
   'Do not open a fresh page here for general use. Each graph will make a call to CHPAGE
   'Call CRPAGE(1)
      
'  Read the data to construct characters
   Call ReadPCCHAR
      
'  INITIALISE THE ASCII TABLE.
    For I = 1 To 128
     TABLE(I) = I
   Next I

End Sub

Sub MGCLOSE()
'************
'  CLOSE METGRAPH
  Dim I As Integer
  Dim dl As Long
  'Delete any metafiles that have not been deleted previously
  For I = 1 To 25
    If EnhMetafileHnd(I) <> 0 Then
      'EnhMetafileHnd(I) = CloseEnhMetaFile(EnhMetafileDC(I))
      dl = DeleteEnhMetaFile(EnhMetafileHnd(I))
      If dl <> 0 Then emfsdeleted = emfsdeleted + 1
    End If
  Next I
  METGRAPH_OPEN = False
End Sub
 
Sub HDCOPY(SF As Single)
'**********************
  'Close the metafile
  Dim dl As Long
'  EnhMetafileHnd(PageNumber) = CloseEnhMetaFile(EnhMetafileDC(PageNumber))
'  MetaFileFlagOn = False
'  dl = DeleteEnhMetaFile(EnhMetafileHnd(PageNumber))
'  If dl <> 0 Then emfsdeleted = emfsdeleted + 1
  'Write the PostScript file
  Call PSCOPY
End Sub
 
Sub CRPAGE(I As Integer, PageSize As String)
'*******************************************
  'Create a new page for drawing
  Dim xExt As Long, yExt As Long
  
  On Error GoTo ErrHandler
  If I > 25 Then
    MESSAGE = "Only 25 graphics pages can be open at once.  Delete one or more pages before continuing"
    response = MsgBox(MESSAGE, 0, "ERROR")
    Exit Sub
  End If
  If I = 0 Then

  ElseIf I < 26 Then
    'Load an additional instance of MGSheet
    Set MGPage(I) = New MGSheet
  
    If PageSize = "Normal" Then
      MGPage(I).height = 8850  'The new page is loaded here but not activated
      MGPage(I).width = 11520
      MGPage(I).pctMG.height = 484
      MGPage(I).pctMG.width = 744
      MGPage(I).Labelgr.Top = 488
      MGPage(I).lblGraphCoordX.Top = 504
      MGPage(I).lblGraphCoordY.Top = 504
      MGPage(I).chkScreenCoordinates.Top = 488
      MGPage(I).lblScreenCoordinates.Top = 488
      MGPage(I).LblScreenCoordinateX.Top = 504
      MGPage(I).LblScreenCoordinateY.Top = 504
    ElseIf PageSize = "Small" Then
      MGPage(I).height = 5000
      MGPage(I).width = 6100
      MGPage(I).pctMG.height = 240
      MGPage(I).pctMG.width = 390
      MGPage(I).Labelgr.Top = 247
      MGPage(I).lblGraphCoordX.Top = 263
      MGPage(I).lblGraphCoordY.Top = 263
      MGPage(I).chkScreenCoordinates.Top = 247
      MGPage(I).lblScreenCoordinates.Top = 247
      MGPage(I).LblScreenCoordinateX.Top = 263
      MGPage(I).LblScreenCoordinateY.Top = 263
    End If
    MGPage(I).pctMG.Cls
    'Get the actual background color
    MGPage(I).ActualBackColor = GetPixel(MGPage(I).pctMG.hdc, 10, 10)

    PageNumber = I
    PageNumberList(I) = True
    MGPage(I).Caption = "METGRAPH page " + Format(I, "#0")
      
    'INITIALISE THE SCALE FACTORS
    SCALEX = 1#
    SCALEY = 1#
      
    'INITIALISE SYMBOL SIZE.
    Isize = 5

    'INITIALISE TEXT SIZE.
    TXTSZ = 3#
    COSRAD = TXTSZ
    SINRAD = 0#

    'INITIALISE TEXT DIRECTION.
    Theta = 0#

    'INITIALISE THE CURRENT GRAPHIC COORDINATE POSITION.
    CURX = 0#
    CURY = 0#


    'INITIALISE THE CMT FOR 3-D LABELS SO THEY WRITE ON THE 2-D VIEWPLANE
    LAB_CMT(1) = 1#
    LAB_CMT(2) = 0#
    LAB_CMT(3) = 0#
    LAB_CMT(4) = 1#
    LAB_CMT(5) = 0#
    LAB_CMT(6) = 0#
    
    'INITIALISE THE SUB- AND SUPERSCRIPT LEVELS
    SUBLEVEL = 0
    SUPLEVEL = 0

    'INITIALISE THE FONT TO HELVETICA
    IFONT = 1

    'INITIALISE THE CHARACTER SET
    ICHARSET = 1
    
    'Disable the metafile feature
    MetaFileFlagOn = False
'    If Not OpenMetafileDC() Then
'      MsgBox "Metafile device context could not be opened", vbExclamation, "WARNING"
'    End If
  End If
  Exit Sub
ErrHandler:
  If Err.Number = 7 Then
    MESSAGE = "Error when creating a MGGRAPH page" & Chr(10) & Chr(13) & "Insufficient memory"
  Else
    MESSAGE = "Error when creating a MGGRAPH page"
  End If
  MsgBox MESSAGE, vbCritical, "ERROR"
  Exit Sub
End Sub

Function OpenMetafileDC() As Boolean
'***********************************
    'Open an enhanced metafile device context for this page
  Dim FileName As String, Description As String
  Dim lpRect As RECT, refDC As Long, response As Long
  Dim dl As Long, lx As Long, ly As Long, lppoint As POINTAPI, lpsize As Size
  MetaFileFlagOn = True
  refDC = GetDC(0)  'Use screen as the reference DC
  FileName = RTrim(JobPath) & "Metafile" & Format(PageNumber, "00") & ".emf"
  Description = "METGRAPH" & Chr(0) & "Page" & Format(PageNumber, "00") & Chr(0) & Chr(0)
  'Set the metafile playback size to 50% of the picture box size.
  lpRect.Top = CLng(0)
  lpRect.Left = CLng(0)
  'lprect.Bottom = CLng(12500)
  lpRect.Bottom = CLng(100 * GetDeviceCaps(MGPage(PageNumber).pctMG.hdc, VERTSIZE))
  'lprect.Right = CLng(12500)
  lpRect.Right = CLng(100 * GetDeviceCaps(MGPage(PageNumber).pctMG.hdc, HORZSIZE))
  'Use 1:1 aspect ratio
  'If lpRect.Right < lpRect.Bottom Then
    'lpRect.Right = lpRect.Bottom
  'Else
    'lpRect.Bottom = lpRect.Right
  'End If
   
  EnhMetafileDC(PageNumber) = CreateEnhMetaFile(refDC, FileName, lpRect, Description)
  response = ReleaseDC(0, refDC)
  If EnhMetafileDC(PageNumber) <> 0 Then
    emfsCreated = emfsCreated + 1
  Else
    MsgBox "Could not create a metafile" & vbCrLf & "emfscreated " & CStr(emfsCreated) & vbCrLf & "emfsdeleted " & CStr(emfsdeleted), vbExclamation, "WARNING"
    OpenMetafileDC = False
    Exit Function
  End If
  dl = SetMapMode(EnhMetafileDC(PageNumber), MM_TEXT)
  lx = 0
  ly = 0
  dl = SetWindowOrgEx(EnhMetafileDC(PageNumber), lx, ly, lppoint)
  lx = CLng(MGPage(PageNumber).pctMG.width)
  ly = CLng(MGPage(PageNumber).pctMG.height)
  dl = SetWindowExtEx(EnhMetafileDC(PageNumber), lx, ly, lpsize)
  lx = 0
  ly = 0
  dl = SetViewportOrgEx(EnhMetafileDC(PageNumber), lx, ly, lppoint)
  lx = CLng(MGPage(PageNumber).pctMG.ScaleWidth)
  ly = CLng(MGPage(PageNumber).pctMG.ScaleHeight)
  dl = SetViewportExtEx(EnhMetafileDC(PageNumber), lx, ly, lpsize)
  OpenMetafileDC = True
  Exit Function
ErrHandler:
  If Err.Number = 7 Then
    MESSAGE = "Error when opening metafile DC" & Chr(10) & Chr(13) & "Insufficient memory"
  Else
    MESSAGE = "Error when creating a MGGRAPH page"
  End If
  MsgBox MESSAGE, vbCritical, "ERROR"
  OpenMetafileDC = False
  Exit Function
   
End Function

Function CHPAGE() As Integer
'***************************
  CHPAGE = ChangePage("Normal")
End Function

Function ChangePage(PageSize As String) As Integer
'*************************************************
  'Creates the next METGRAPH graphics page in sequence.
  'PageSize can be "Normal" or "Small"
  'Returns the value of the pagenumber
  Dim I As Integer
  
  On Error GoTo ErrHandler
  
  If Not METGRAPH_OPEN Then
    Call RTAB
  End If

  'Look for the first vacant page.
  PageNumber = 0
  For I = 25 To 1 Step -1
    If PageNumberList(I) = False Then
      PageNumber = I
    End If
  Next I
  If PageNumber = 0 Then
    MESSAGE = "No more than 25 graphics pages can be open at once.  Delete one or more pages before continuing"
    response = MsgBox(MESSAGE, 0, "ERROR")
    ChangePage = 0
    Exit Function
  End If
  
  Call CRPAGE(PageNumber, PageSize)
  ChangePage = PageNumber
'
'  Initialize PostScript and create a PostScript page.
  Call PSINIT
  Call LINEWD(0.28)
  Exit Function
  
ErrHandler:
  If Err.Number = 7 Then
    MESSAGE = "Error when changing METGRAPH page" & Chr(13) & Chr(13) & "Insufficient memory"
  Else
    MESSAGE = "Error when changing METGRAPH page"
  End If
  MsgBox MESSAGE, vbCritical, "ERROR"
  Exit Function
  End Function

Public Function ReOpenMGPage(IP)
'*******************************
  'Re-open a previously used page
  If PageNumberList(IP) Then
    PageNumber = IP
    Call ReOpenPSPage
    ReOpenMGPage = PageNumber
  Else
    ReOpenMGPage = 0
  End If
End Function

Sub GROUT()
'**********
'Save the PostScript buffer to include all the new information on the graph
Call SavePSBuffer
'Display the current page without display of graph coordinates.
MGPage(PageNumber).DisplayCoordinates = False
MGPage(PageNumber).RightButtonEvent = False
MGPage(PageNumber).Labelgr.Visible = False
MGPage(PageNumber).lblGraphCoordX.Visible = False
MGPage(PageNumber).lblGraphCoordY.Visible = False
MGPage(PageNumber).chkScreenCoordinates.Visible = False
MGPage(PageNumber).LblScreenCoordinateX.Visible = False
MGPage(PageNumber).LblScreenCoordinateY.Visible = False
MGPage(PageNumber).Left = 500 * PageNumber
MGPage(PageNumber).Top = 500 * PageNumber
MGPage(PageNumber).Show
End Sub

Sub GROUTCOORD()
'**************
'Save the PostScript buffer to include all the new information on the graph
Call SavePSBuffer
'Display the current page with display of graph coordinates.
MGPage(PageNumber).DisplayCoordinates = True
MGPage(PageNumber).RightButtonEvent = False
MGPage(PageNumber).Labelgr.Visible = True
MGPage(PageNumber).lblGraphCoordX.Visible = True
MGPage(PageNumber).lblGraphCoordY.Visible = True
MGPage(PageNumber).chkScreenCoordinates.Visible = True
MGPage(PageNumber).LblScreenCoordinateX.Visible = True
MGPage(PageNumber).LblScreenCoordinateY.Visible = True
MGPage(PageNumber).Left = 500 * PageNumber
MGPage(PageNumber).Top = 500 * PageNumber
MGPage(PageNumber).Show
End Sub

Sub GROUTRightButtonEvent()
'*************************
'Save the PostScript buffer to include all the new information on the graph
Call SavePSBuffer
'Display the current page with display of graph coordinates.
MGPage(PageNumber).DisplayCoordinates = True
MGPage(PageNumber).RightButtonEvent = True
MGPage(PageNumber).Labelgr.Visible = True
MGPage(PageNumber).lblGraphCoordX.Visible = True
MGPage(PageNumber).lblGraphCoordY.Visible = True
MGPage(PageNumber).chkScreenCoordinates.Visible = True
MGPage(PageNumber).LblScreenCoordinateX.Visible = True
MGPage(PageNumber).LblScreenCoordinateY.Visible = True
MGPage(PageNumber).Left = 500 * PageNumber
MGPage(PageNumber).Top = 500 * PageNumber
MGPage(PageNumber).Show
End Sub

'Sub RightButtonProc(Which As String, X As Single, Y As Single)
''Activate this dummy procedure for applications that do not define a right button procedure
'End Sub

Sub MGRMVE(X As Single, Y As Single)
'************************************
  'The high-resolution move primitive.
  Call TRMOVE(X, Y)
  'Transmit this to PostScript if necessary
  If PSFlagOn Then Call PSMOVE(X, Y)
  If PrinterFlagOn Then
    Printer.CurrentX = X
    Printer.CurrentY = Y
  End If
End Sub

  Sub MGRELMVE(X As Single, Y As Single)
' **************************************
' The high-resolution relative move primitive.
' Rotate and scale if necessary
  Dim XS As Single, Ys As Single
  XS = (COSRAD * X - SINRAD * Y) * SCALEX
  Ys = (COSRAD * Y + SINRAD * X) * SCALEY
  Call TRMOVE(CURX + XS, CURY + Ys)
' TRANSMIT THIS TO POSTCRIPT
  Call PSRELMOVE(X, Y)
'
'...AND TO THE PLOTTER.
'  CALL HDMOVE(CURX+X,CURY+Y)
  End Sub

Sub RPLOT(X As Single, Y As Single)
'**********************************
  'The high-resolution plot primitive.
  Dim lx As Long, ly As Long, dl As Long
  Call TRPLOT(X, Y)
  If PSFlagOn Then Call PSPLOT(X, Y)
  If PrinterFlagOn Then Printer.Line -(X, Y)

  End Sub

  Sub TRPLOT(X As Single, Y As Single)
' ************************************
'  Plots to the screen without transmitting to the hard copy.
  Dim SX As Single, SY As Single
  Dim lx As Long, ly As Long, dl As Long
  Call CoordConv(X, Y, SX, SY)
  If PageNumber = 0 Then
    MGPage(PageNumber).Line -(SX, SY)
  Else
    MGPage(PageNumber).pctMG.Line -(SX, SY)
  End If

  If MetaFileFlagOn Then
    If PageNumber = 0 Then
      lx = CLng(SX)
      ly = CLng(SY)
      dl = LineTo(EnhMetafileDC(PageNumber), lx, ly)
    ElseIf PageNumber < 10 Then
      lx = CLng(SX)
      ly = CLng(SY)
      dl = LineTo(EnhMetafileDC(PageNumber), lx, ly)
    End If
  End If
  Call SETCUR(X, Y)
  End Sub

Sub TRMOVE(X As Single, Y As Single)
'***********************************
'  Moves on the screen without transmitting to the plotters.
  Dim SX As Single, SY As Single
  Dim lx As Long, ly As Long, dl As Long
  Call CoordConv(X, Y, SX, SY)
  If PageNumber = 0 Then
    MGPage(PageNumber).CurrentX = SX
    MGPage(PageNumber).CurrentY = SY
  Else
    MGPage(PageNumber).pctMG.CurrentX = SX
    MGPage(PageNumber).pctMG.CurrentY = SY
  End If
  If MetaFileFlagOn Then
    If PageNumber = 0 Then
      lx = CLng(SX)
      ly = CLng(SY)
      dl = MoveToEx(EnhMetafileDC(PageNumber), lx, ly, Oldpoint)
    ElseIf PageNumber < 10 Then
      lx = CLng(SX)
      ly = CLng(SY)
      dl = MoveToEx(EnhMetafileDC(PageNumber), lx, ly, Oldpoint)
    End If
  End If
  Call SETCUR(X, Y)
End Sub

Sub CoordConv(X As Single, Y As Single, SX As Single, SY As Single)
'******************************************************************
'Converts coordinates in the 1024x780 system to screen coordinates.
Dim lx As Long, lpsize As Size
Dim width As Single
If PageNumber = 0 Then
  width = 780 * MGPage(0).width / MGPage(0).height
  SX = X * MGPage(0).ScaleWidth / width
  SY = (780 - Y) * MGPage(0).ScaleHeight / 780
Else
  SX = X * MGPage(PageNumber).pctMG.ScaleWidth / 1024
  SY = (780 - Y) * MGPage(PageNumber).pctMG.ScaleHeight / 780
End If
End Sub

Sub ConvertCoordFromScreen(SX As Single, SY As Single, X As Single, Y As Single)
'*******************************************************************************
'Converts coordinates in the screen coordinates to 1024x780 system.
Dim lx As Long, lpsize As Size
Dim width As Single
If PageNumber = 0 Then
  width = 780 * MGPage(0).width / MGPage(0).height
  X = SX * width / MGPage(0).ScaleWidth
  Y = (MGPage(0).ScaleHeight - SY) * 780 / MGPage(0).ScaleHeight
Else
  X = SX * 1024 / MGPage(PageNumber).pctMG.ScaleWidth
  Y = 780 * (MGPage(PageNumber).pctMG.ScaleHeight - SY) / MGPage(PageNumber).pctMG.ScaleHeight
End If
End Sub

Sub MGCIRCLE(X As Single, Y As Single, Radius As Single)
'*******************************************************
  Dim di As Long, X1 As Long, Y1 As Long, X2 As Long, Y2 As Long
  Dim SX As Single, SY As Single
  Dim Xr As Single, Yr As Single
  Call CoordConv(X, Y, SX, SY)
  Call CoordConv(Radius, Radius, Xr, Yr)
  If PageNumber = 0 Then
    MGPage(PageNumber).FillStyle = 1
    MGPage(PageNumber).Circle (SX, SY), Xr
  Else
    MGPage(PageNumber).pctMG.FillStyle = 1
    MGPage(PageNumber).pctMG.Circle (SX, SY), Xr
  End If
  If MetaFileFlagOn Then
    X1 = SX - Radius
    Y1 = SY - Radius
    X2 = SX + Radius
    Y2 = SY + Radius
    di = Ellipse(EnhMetafileDC(PageNumber), X1, Y1, X2, Y2)
  End If
  If PSFlagOn Then Call PSCIRC(X, Y, Radius)
  If PrinterFlagOn Then Printer.Circle (X, Y), Radius
End Sub

Sub MGFilledCircle(X As Single, Y As Single, Radius As Single)
'*************************************************************
  Dim di As Long, X1 As Long, Y1 As Long, X2 As Long, Y2 As Long
  Dim SX As Single, SY As Single
  Dim Xr As Single, Yr As Single
  Call CoordConv(X, Y, SX, SY)
  Call CoordConv(Radius, Radius, Xr, Yr)
  If PageNumber = 0 Then
    MGPage(PageNumber).FillStyle = 0
    MGPage(PageNumber).Circle (SX, SY), Xr
  Else
    MGPage(PageNumber).pctMG.FillStyle = 0
    MGPage(PageNumber).pctMG.Circle (SX, SY), Xr
  End If
  If MetaFileFlagOn Then
    X1 = SX - Radius
    Y1 = SY - Radius
    X2 = SX + Radius
    Y2 = SY + Radius
    di = Ellipse(EnhMetafileDC(PageNumber), X1, Y1, X2, Y2)
  End If
  If PSFlagOn Then Call PSFCIRC(X, Y, Radius)
  If PrinterFlagOn Then Printer.Circle (X, Y), Radius
End Sub

Sub MGCOLOREDCIRCLE(X As Single, Y As Single, Radius As Single, color As Long)
'*****************************************************************************
  Dim di As Long, X1 As Long, Y1 As Long, X2 As Long, Y2 As Long
  Dim SX As Single, SY As Single
  Dim Xr As Single, Yr As Single
  Call CoordConv(X, Y, SX, SY)
  Call CoordConv(Radius, Radius, Xr, Yr)
  If PageNumber = 0 Then
    MGPage(PageNumber).FillColor = color
    MGPage(PageNumber).FillStyle = 0
    MGPage(PageNumber).Circle (SX, SY), Xr, color
    MGPage(PageNumber).FillStyle = 1
  ElseIf PageNumber < 10 Then
    MGPage(PageNumber).pctMG.FillColor = color
    MGPage(PageNumber).pctMG.FillStyle = 0
    MGPage(PageNumber).pctMG.Circle (SX, SY), Xr, color
    MGPage(PageNumber).pctMG.FillStyle = 1
  End If
  If PSFlagOn Then Call PSCIRC(X, Y, Radius)
  If MetaFileFlagOn Then
    X1 = X - Radius
    Y1 = Y - Radius
    X2 = X + Radius
    Y2 = Y + Radius
    di = Ellipse(EnhMetafileDC(PageNumber), X1, Y1, X2, Y2)
  End If
  If PrinterFlagOn Then Printer.Circle (X, Y), Radius, color
End Sub

Sub MGGRAYFILL(F As Single)
'**************************
'Sets the fill color to a shade of gray
  Dim Gray As Integer
  
  On Error GoTo ErrHandler
  Gray = F * 255
  If PageNumber = 0 Then
    MGPage(PageNumber).FillColor = RGB(Gray, Gray, Gray)
  ElseIf PageNumber > 0 Then
    MGPage(PageNumber).pctMG.FillColor = RGB(Gray, Gray, Gray)
  Else
    GoTo ErrHandler
  End If
  GRAYFILL = F
  Exit Sub
  
ErrHandler:
  MsgBox "Error setting gray level", vbExclamation, "WARNING"
  Exit Sub
End Sub

Sub COLOUR(I As Integer)
'***********************
'Set the color to be used in all graphics primitives
MGPage(PageNumber).pctMG.ForeColor = QBColor(I)
End Sub

Sub CHANGECOL(I As Integer)
'**************************
'Change the color of the graphic primitives
'Only colors that show well on the screen are used
I = I + 1
If I <= 0 Then I = 0
If I > 14 Then I = 0
Call COLOUR(I)
End Sub

 Sub MGClearCurrentPage()
'************************
MGPage(PageNumber).pctMG.Cls
End Sub

Sub MGCLIP(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single)
'*****************************************************************
'Defines a rectangular clip region
  Dim IX1 As Long, IY1 As Long, IX2 As Long, IY2 As Long
  Dim SX As Single, SY As Single
  Dim RetVal As Long
  Call CoordConv(X1, Y2, SX, SY)
  IX1 = CLng(SX)
  IY1 = CLng(SY)
  Call CoordConv(X2, Y1, SX, SY)
  IX2 = CLng(SX)
  IY2 = CLng(SY)
  ClipRegion = CreateRectRgn(IX1, IY1, IX2, IY2)
  regionsCreated = regionsCreated + 1
  SavedPreClipDC = SaveDC(MGPage(PageNumber).pctMG.hdc)
  RetVal = SelectClipRgn(MGPage(PageNumber).pctMG.hdc, ClipRegion)
  If MetaFileFlagOn Then
    SavedPreClipMetafileDC = SaveDC(EnhMetafileDC(PageNumber))
    RetVal = SelectClipRgn(EnhMetafileDC(PageNumber), ClipRegion)
  End If
  If PSFlagOn Then
    Call PSCLIP(X1, Y1, X2, Y2)
  End If
End Sub

Sub MGCLIPOFF()
'**************
  Dim dl As Long, RetVal As Long
  RetVal = DeleteObject(ClipRegion)
  regionsdeleted = regionsdeleted + 1
  dl = RestoreDC(MGPage(PageNumber).pctMG.hdc, SavedPreClipDC)
  If MetaFileFlagOn Then
    dl = RestoreDC(EnhMetafileDC(PageNumber), SavedPreClipMetafileDC)
  End If
  If PSFlagOn Then
    Call PSCLIPOFF
  End If
End Sub

Sub SETCUR(X As Single, Y As Single)
' ************************************
'  SETS THE CURRENT POSITION.
  CURX = X
  CURY = Y
End Sub

Public Sub LNTYPE(I As Integer)
'******************************
' Set the line type
Call MGLNTYPE(I)
End Sub
'
 Sub LINEWD(T As Single)
'***********************
'  SETS THE LINE WIDTH.  T is mm.
 Dim PTS As Single
 Dim pixels As Integer
 If PageNumber = 0 Then Exit Sub
 pixels = CInt(T / 0.28)
 If pixels < 1 Then pixels = 1
 LineWidth = pixels
 MGPage(PageNumber).pctMG.DrawWidth = pixels
 PTS = 2.835 * T
 Call PSLNWD(PTS)
 End Sub

Sub MGLNTYPE(ITP As Integer)
'***************************
'  Sets the line type.
  Dim IT As Integer
  Dim OldPen As Long, nPenStyle As Long, di As Long
  IT = ITP - ((ITP - 1) \ 5) * 5 - 1
  If (ITP < 1) Then IT = 0
  If PageNumber = 0 Then
    MGPage(PageNumber).DrawStyle = IT
  Else
    MGPage(PageNumber).pctMG.DrawStyle = IT
  End If
  nPenStyle = IT
  If MetaFileFlagOn Then
    If hndPen Then
      di = DeleteObject(hndPen)
      pensdeleted = pensdeleted + 1
    End If
    hndPen = CreatePen(nPenStyle, LineWidth, BLACK_PEN)
    pensCreated = pensCreated + 1
    OldPen = SelectObject(EnhMetafileDC(PageNumber), hndPen)
  End If
'  SET LINE TYPE ON THE PLOTTER AND IN POSTSCRIPT.
'      CALL HDLNTP(IT)
  IT = ITP - ((ITP - 1) \ 10) * 10
  If (ITP < 1) Then IT = 0
  If PSFlagOn Then Call PSLNTP(IT)
End Sub
 Sub MGLineWidth(T As Single)
'****************************
'  SETS THE LINE WIDTH.  T is mm.
 Dim PTS As Single
 Dim pixels As Integer
 Dim di As Long
 
 If PageNumber = 0 Then
   pixels = CInt(T / 0.26)
   If pixels < 1 Then pixels = 1
   LineWidth = pixels
   MGPage(PageNumber).DrawWidth = pixels
 Else
   Call LINEWD(T)
 End If
 If PSFlagOn Then
   PTS = 2.835 * T
   Call PSLNWD(PTS)
 End If
 If MetaFileFlagOn Then
    If hndPen Then
      di = DeleteObject(hndPen)
      If di = 0 Then
        MsgBox "Could not delete pen", vbExclamation, "Warning"
      Else
        pensdeleted = pensdeleted + 1
      End If
    End If
    hndPen = CreatePen(PS_SOLID, LineWidth, BLACK_PEN)
    pensCreated = pensCreated + 1
    OldPen = SelectObject(EnhMetafileDC(PageNumber), hndPen)
 End If
End Sub

 Sub RESLWD()
'************
'  RESETS LINE WIDTH TO DEFAULT VALUES
 Call LINEWD(0.3527)
 End Sub

  Sub TXSIZE(I As Integer, J As Integer)
' **************************************
' Sets the text size to I. J is not used.
  'I is the size in points on the PostScript output.
  'Factor is the scaling factor for the line drawn screen font to make screen fonts and Postscript fonts about equal in size,
  Dim Factor As Single, Size As Single
  Factor = 0.18
  Size = Factor * I
  Call SETTEXTSIZE(Size)
  PSFONTSIZE = I
  If PSFlagOn Then
    Call PSLBSZ
  End If
  End Sub
'
  Sub SETTEXTSIZE(Size As Single)
' *******************************
' Sets the text size for the monitor
  Dim RAD As Single
  TXTSZ = Size
  RAD = 2# * 3.1415927 * (Theta / 360#)
  COSRAD = Cos(RAD) * TXTSZ
  SINRAD = Sin(RAD) * TXTSZ
  End Sub
'
  Sub TXTDIR(I As Integer)
' ************************
'  Sets the text direction in integrals of 90 degees.
  Dim RAD As Single
  Theta = (I - 1) * 90#
  RAD = 2# * 3.1415927 * (Theta / 360#)
  COSRAD = Cos(RAD) * TXTSZ
  SINRAD = Sin(RAD) * TXTSZ
  End Sub

Sub RotateLabel(Angle As Single)
'*******************************
  'Rotate labels by ANGLE
  Dim RAD As Single
  Call PSROT(Angle)
  RAD = 2# * 3.1415927 * (Angle / 360#)
  COSRAD = Cos(RAD) * TXTSZ
  SINRAD = Sin(RAD) * TXTSZ
End Sub

Sub MGLABEL(Label As String)
'***************************
  Dim Length As Integer, I As Integer, LAB(81) As Integer
  'MGPage(0).ForeColor = vbBlack
  Length = Len(Label)
  If PageNumber = 0 Then
    MGPage(PageNumber).Print Label
    If PSFlagOn Then
      For I = 1 To Length
        LAB(I) = ICHAR(Mid(Label, I, 1))
      Next I
      Call PSLAB(LAB, Length)
    End If
  Else
    Call LABELC(Label, Length)
  End If
End Sub

Sub Label(IDENT() As Integer, N As Integer)
' *******************************************
  'WRITES A LABLE TO THE SCREEN AT THE CURRENT POSITION AND TRANSMIT TO
  'THE HARD COPY DEVICES.
  Dim LAB(256) As Integer, LabLen As Integer
  Dim istart As Integer, I As Integer
 
  istart = 0
  LabLen = 0
  For I = 1 To N
'
'   Check for font change
    If (I > 1) Then
      If (IDENT(I - 1) = ICHAR("\") And (IDENT(I) >= 49 And IDENT(I) <= 51)) Then GoTo Lbl10
    End If
    If (IDENT(I) = ICHAR("\") And (IDENT(I + 1) >= 49 And IDENT(I + 1) <= 51)) Then
      Call LABELOUT(LAB, LabLen)
      LabLen = 0
      Call MGCHARSET(IDENT(I + 1) - 48)
      istart = I + 1
      GoTo Lbl10
    End If

'   Ignore backslash
    If (IDENT(I) = ICHAR("\") And IDENT(I + 1) <> ICHAR("\")) Then
      istart = istart + 1
      GoTo Lbl10
    End If

'   Check for subscript
    If (I > 1 And IDENT(I - 1) <> ICHAR("\")) Then
      If (IDENT(I) = ICHAR("`")) Then
        If (SUBLEVEL Mod 2 = 0) Then
          Call LABELOUT(LAB, LabLen)
          LabLen = 0
          SUBLEVEL = SUBLEVEL + 1
          istart = I
          Call SUBSCRIPT
        Else
          Call LABELOUT(LAB, LabLen)
          LabLen = 0
          SUBLEVEL = SUBLEVEL - 1
          istart = I
          Call CANSUB
        End If
      GoTo Lbl10
      End If
    End If

'   Check for superscript
    If (I > 1 And IDENT(I - 1) <> ICHAR("\")) Then
      If (IDENT(I) = ICHAR("~")) Then
        If (SUPLEVEL Mod 2 = 0) Then
          Call LABELOUT(LAB, LabLen)
          LabLen = 0
          SUPLEVEL = SUPLEVEL + 1
          istart = I
          Call SUPSCRIPT
        Else
          Call LABELOUT(LAB, LabLen)
          LabLen = 0
          SUPLEVEL = SUPLEVEL - 1
          istart = I
          Call CANSUP
        End If
      GoTo Lbl10
      End If
    End If

    LAB(I - istart) = IDENT(I)
    LabLen = LabLen + 1

Lbl10:
  Next I
  If (LabLen > 0) Then Call LABELOUT(LAB, LabLen)
  If (SUBLEVEL > 0) Then
    For I = 1 To SUBLEVEL
      Call CANSUB
    Next I
    SUBLEVEL = 0
  End If
  If (SUPLEVEL > 0) Then
    For I = 1 To SUPLEVEL
      Call CANSUP
    Next I
    SUPLEVEL = 0
  End If
End Sub

  Sub LABELOUT(IDENT() As Integer, N As Integer)
' **********************************************
' Sends the label to the output devices
  Dim I As Integer, Label As String * 80

' Use line drawn characters
  Call TLABEL(IDENT, N)
  
' Use the Visual Basics fonts
  'For I = 1 To N
   ' Mid(LABEL, I, 1) = CHR(IDENT(I))
  'Next I
  'MGPage(PageNumber).pctMG.CurrentX = CURX
  'MGPage(PageNumber).pctMG.CurrentY = CURY
  'MGPage(PageNumber).pctMG.Print RTrim(LABEL)
  
  'CURX = CURX + MGPage(PageNumber).TextWidth(RTrim(LABEL))
'
'  WRITE THE LABEL TO THE HARDCOPY DEVICES.
   If PSFlagOn Then
     Call PSLAB(IDENT, N)
   End If
'  CALL HDLAB(IDENT,N)
  End Sub

Sub MGNEWFONT(NewFont As PageFont)
'*********************************
'Gets a new font for page 0 using the font dialog
  Dim N As Integer
  On Error GoTo ErrHandler
  MGPage(0).CommonDialog1.CancelError = True
  MGPage(0).CommonDialog1.Flags = &H3&
  MGPage(0).CommonDialog1.FontName = MGPage(0).FONT.Name
  MGPage(0).CommonDialog1.FONTSIZE = MGPage(0).FONT.Size
  MGPage(0).CommonDialog1.FontItalic = MGPage(0).FONT.italic
  MGPage(0).CommonDialog1.FontBold = MGPage(0).FONT.Bold
  MGPage(0).CommonDialog1.ShowFont
    NewFont.Name = MGPage(0).CommonDialog1.FontName
    NewFont.Size = MGPage(0).CommonDialog1.FONTSIZE
    NewFont.Bold = MGPage(0).CommonDialog1.FontBold
    NewFont.italic = MGPage(0).CommonDialog1.FontItalic
 'Printer font is set to the page font when print is selected on the menu
  With Printer.FONT
    .Name = MGPage(0).CommonDialog1.FontName
    .Size = MGPage(0).CommonDialog1.FONTSIZE
    .Bold = MGPage(0).CommonDialog1.FontBold
    .italic = MGPage(0).CommonDialog1.FontItalic
  End With
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub

Sub MGLABSEG(text As String, ISEG As Integer, IX As Single, IY As Single, OffsetX As Single, OffsetY As Single)
'**************************************************************************************************************
'Opens a segment to take a text string and write the label.
'Applies to page 0 only
  Dim dl As Long, nCount As Long
  Dim X As Long, Y As Long
  Dim ScX As Single, ScY As Single
  Dim I As Integer, LAB(81) As Integer
  
  Call CoordConv(IX - OffsetX, IY - OffsetY, ScX, ScY)
  MGPage(0).CurrentX = ScX
  MGPage(0).CurrentY = ScY - MGPage(0).TextHeight("M")
  MGPage(0).Print text
  If PSFlagOn = True Then
    Call PSMOVE(IX - OffsetX, IY - OffsetY + 3)
    'Call PSMOVE(IX - OffsetX, IY - OffsetY + MGPage(0).TextHeight("M") + 5)
    For I = 1 To Len(text)
      LAB(I) = ICHAR(Mid(text, I, 1))
    Next I
    Call PSLAB(LAB, Len(text))
  End If
  If MetaFileFlagOn Then
    Call CoordConv(IX - OffsetX, IY - OffsetY + MGPage(0).TextHeight("M") + 10, ScX, ScY)
    X = ScX
    Y = ScY
    nCount = Len(text)
    dl = TextOut(EnhMetafileDC(PageNumber), X, Y, text, nCount)
  End If
  '...and to the printer.
  If PrinterFlagOn Then
    Printer.CurrentX = IX - OffsetX
    Printer.CurrentY = IY - OffsetY + MGPage(0).TextHeight("M") + 5
    Printer.Print text
  End If
  End Sub

Sub MGNUMLAB(Number As Integer)
'******************************
' Draws an integer as a label
  If PageNumber = 0 Then
    MGPage(PageNumber).CurrentY = MGPage(PageNumber).CurrentY - MGPage(PageNumber).TextHeight("M")
    MGPage(PageNumber).Print Number
    'If PSFlagOn Then Call PSLAB(CStr(Number))
    If PrinterFlagOn Then
      Printer.CurrentY = Printer.CurrentY - Printer.TextHeight("M")
      Printer.Print Number
    End If
  End If
End Sub
  Sub TLABEL(IDENT() As Integer, N As Integer)
' ********************************************
'  WRITES A LABLE TO THE SCREEN AT THE CURRENT POSITION.
'
  Dim I As Integer
  Dim X As Single, Y As Single, AX As Single, AY As Single
  Dim XW As Single, YW As Single
  Dim DISPX As Single, DISPY As Single
  Dim ST As Integer
  Dim PENUP As Boolean
  MGPage(PageNumber).pctMG.DrawWidth = 1
  For I = 1 To N
    PENUP = True
    AX = CURX
    AY = CURY
    ST = START(IDENT(I), ICHARSET)

Lbl50:
    If (SYMTAB(ST, ICHARSET) = -9) Then GoTo Lbl60
    DISPX = SYMTAB(ST, ICHARSET) * 5# / 6#
    DISPY = SYMTAB(ST + 1, ICHARSET)
    XW = AX + (COSRAD * DISPX - SINRAD * DISPY) * SCALEX
    YW = AY + (COSRAD * DISPY + SINRAD * DISPX) * SCALEY
    Call VIEWC_CMT(XW, YW, X, Y, LAB_CMT)
    If (PENUP) Then
      Call TRMOVE(X, Y)
      'Draw the last pixel
      MGPage(PageNumber).pctMG.PSet Step(0, 0)

      PENUP = False
    Else
      Call TRPLOT(X, Y)
    End If
    ST = ST + 2
    If (SYMTAB(ST, ICHARSET) = -8) Then
      ST = ST + 1
      'Draw the last pixel
      MGPage(PageNumber).pctMG.PSet Step(0, 0)
      PENUP = True
    End If
    GoTo Lbl50

Lbl60:
'   FINISHED PLOTTING CHARACTER, UPDATE CURRENT POSITION
    CURX = AX + 5# * COSRAD * SCALEX
    CURY = AY + 5# * SINRAD * SCALEY
 Next I
 MGPage(PageNumber).pctMG.DrawWidth = LineWidth
 End Sub
'
  Sub SUBSCRIPT()
' ***************
' Sets the label in subscript mode
  Dim Y As Single
  Call MGSCALE(0.8, 0.8)
  MGPage(PageNumber).pctMG.FONTSIZE = 8
  Y = -2 * TXTSZ
  Call MGRELMVE(0#, Y)
  End Sub

  Sub CANSUB()
' ***********
' Cancels the subscript state
  Dim Y As Single
  Y = 2 * TXTSZ
  Call MGRELMVE(0#, Y)
  Call MGSCALE(1# / 0.8, 1# / 0.8)
  MGPage(PageNumber).pctMG.FONTSIZE = 10
  End Sub
'
  Sub SUPSCRIPT()
' ***************
' Sets the label in superscript mode
  Dim Y As Single
  Call MGSCALE(0.8, 0.8)
  MGPage(PageNumber).pctMG.FONTSIZE = 8

  Y = 2 * TXTSZ
  Call MGRELMVE(0#, Y)
  End Sub

  Sub CANSUP()
' ************
' Cancels the superscript state
  Dim Y As Single
  Y = -2 * TXTSZ
  Call MGRELMVE(0#, Y)
  Call MGSCALE(1# / 0.8, 1# / 0.8)
   MGPage(PageNumber).pctMG.FONTSIZE = 10
  End Sub

Sub MGFONT(NewFont As PageFont)
'******************************
'Sets the page font for page 0 to Newfont
Dim N As Integer
Dim Size As Single
With MGPage(0).FONT
  .Name = NewFont.Name
  .Size = NewFont.Size
  .Bold = NewFont.Bold
  .italic = NewFont.italic
End With
If PrinterFlagOn Then
  With Printer.FONT
    .Name = NewFont.Name
    .Size = NewFont.Size
    .Bold = NewFont.Bold
    .italic = NewFont.italic
  End With
End If
If PSFlagOn Then
  If NewFont.Name = "Helvetica" Then
    N = 1
  ElseIf NewFont.Name = "Times New Roman" Then
    N = 2
  ElseIf NewFont.Name = "Courier" Then
    N = 3
  ElseIf NewFont.Name = "Arial" Then
    N = 0
  Else
    N = 1
  End If
' Set the PostScript FONT to N
' Dont change unnecessarily
  If (N > 3) Then Return
'  If (N <> IFONT) Then
    IFONT = N
    ' Set the PostScript font
    PSFONTSIZE = NewFont.Size
    If N = 0 Then PSFONTSIZE = 0.95 * PSFONTSIZE ' To make Helvetica look more like Arial
    Call PSSETFONT(N)
    Call PSLBSZ
'  End If
End If
End Sub

  Sub MGSCALE(XSC As Single, YSC As Single)
' *****************************************
' Set the scale factors
  GSSCALEX = SCALEX
  GSSCALEY = SCALEY
  SCALEX = XSC * SCALEX
  SCALEY = YSC * SCALEY
  Call PSScale(XSC, YSC)
  End Sub
'
  Sub MGUNSCALE()
' ***************
' Resets the scale factors to the values immediately prceding the last
' call to MGSCALE
  Call PSScale(GSSCALEX / SCALEX, GSSCALEY / SCALEY)
  SCALEX = GSSCALEX
  SCALEY = GSSCALEY
  End Sub

  Sub MGCHARSET(N)
' ****************
' Sets the symbol set to N
' Dont change unnecessarily
  Dim Size As Single
  If (N > 3) Then Exit Sub
  If (N <> ICHARSET) Then
    ICHARSET = N

' Set the PostScript font
    If (N = 1) Then Call PSSETFONT(IFONT)
    If (N = 2) Then Call PSSETFONT(4)
    If (N = 3) Then Call PSSETFONT(5)
    Call PSLBSZ
  End If
  End Sub

Public Function ICHAR(Character As String) As Integer
'****************************************************
'Simulates the FORTRAN ICHAR function
  On Error GoTo ErrHandler
  If Character = "" Then Exit Function
  ICHAR = Asc(Character)
  Exit Function

ErrHandler:
  MsgBox "Error writing label", vbCritical, "ERROR"
  Exit Function
End Function

Public Sub ReadPCCHAR()
'**********************
'Reads the data for character generation from METGRAPH file PCCHAR.INC
Dim X(22) As Integer
Dim WSTRING As String
Dim I As Integer, J As Integer, K As Integer, L As Integer, M As Integer

On Error GoTo ErrHandler

Open RTrim(VGPath) & "PCCHAR.INC" For Input As #31
WSTRING = Input(188, #31)
L = 1
K = 0
For I = 1 To 32
  WSTRING = Input(8, #31)
  Input #31, X(1), X(2), X(3), X(4), X(5), X(6), X(7), X(8), X(9), X(10), X(11), X(12)
  For J = 1 To 12
    K = K + 1
    START(K, L) = X(J)
    If K = 128 Then
      L = L + 1
      K = 0
    End If
  Next J
Next I
L = 1
K = 0
For I = 1 To INUM
  WSTRING = Input(40, #31)
  For M = 1 To 5
    WSTRING = Input(8, #31)
    Input #31, X(1), X(2), X(3), X(4), X(5), X(6), X(7), X(8), X(9), X(10), X(11), X(12), X(13), X(14), X(15), X(16), X(17), X(18), X(19), X(20), X(21), X(22)
    For J = 1 To 22
      K = K + 1
      SYMTAB(K, L) = X(J)
      If K = ITOT Then
        L = L + 1
        K = 0
      End If
    Next J
  Next M
Next I
Close #31
Exit Sub

ErrHandler:
  MsgBox "Could not find METGRAPH character definitions" & vbCrLf & "Path is " & RTrim(VGPath), vbCritical, "ERROR"
  Exit Sub
End Sub

Sub MGPOLY(ITYPE As Integer, Verts() As xyrcoord, NVERTS As Integer)
' ******************************************************************
' Draws a polygon defined by the array of coordinate structures VERTS.
' NVERTS is the number of vertices (up to 32).
' ITYPE defines the filling type
'ITYPE = 1: No filling
'      = 2: Polygon is filled with the current color.
'      = 3: Polygon border drawn in current color filled with the
'           background color.
'      = 4: Border and polygon filled with background color.
'      = 5: Border drawn in current color polygon filled with current fill color.

' Note Visual Basic does not support a polygon construction. This is the best
' compromise.
  Dim SX As Single, SY As Single
  Dim Vertex(32) As POINTAPI
  Dim nCount As Long, hPen As Long, hBrush As Long, OldPen As Long, OldBrush As Long
  Dim PenColor As Long, FillColor As Long
  Dim SavedDC As Long, SavedMetafileDC As Long
  Dim RetVal As Long
  Dim PERI As Single
  Dim I As Integer

  On Error GoTo ErrHandler
  
  PERI = Abs(Verts(1).xcoord - Verts(NVERTS).xcoord)
  PERI = PERI + Abs(Verts(1).ycoord - Verts(NVERTS).ycoord)
  For I = 2 To NVERTS
    PERI = PERI + Abs(Verts(I).xcoord - Verts(I - 1).xcoord)
    PERI = PERI + Abs(Verts(I).ycoord - Verts(I - 1).ycoord)
  Next I
  If (PERI < NVERTS) Then
    response = MsgBox(" Polygon is too small to draw.", 0, "ERROR")
    Exit Sub
  End If
  nCount = CLng(NVERTS)
  For I = 1 To NVERTS
    Call CoordConv(Verts(I).xcoord, Verts(I).ycoord, SX, SY)
    Vertex(I).X = CLng(SX)
    Vertex(I).Y = CLng(SY)
  Next I
  
  If PageNumber = 0 Then
    SavedDC = SaveDC(MGPage(PageNumber).hdc)
  ElseIf PageNumber > 0 Then
    SavedDC = SaveDC(MGPage(PageNumber).pctMG.hdc)
  Else
    GoTo ErrHandler
  End If
  
  If MetaFileFlagOn Then SavedMetafileDC = SaveDC(EnhMetafileDC(PageNumber))
  
  Select Case ITYPE
    Case 1
      If PageNumber = 0 Then
        MGPage(PageNumber).FillStyle = 1    'Transparent
      ElseIf PageNumber > 0 Then
        MGPage(PageNumber).pctMG.FillStyle = 1    'Transparent
      Else
        GoTo ErrHandler
      End If
      Call TRMOVE(Verts(NVERTS).xcoord, Verts(NVERTS).ycoord)
      For I = 1 To NVERTS
        Call TRPLOT(Verts(I).xcoord, Verts(I).ycoord)
      Next I
    Case 2
      If PageNumber = 0 Then
        MGPage(PageNumber).FillStyle = 0    'Solid fill
        PenColor = MGPage(PageNumber).ForeColor
        FillColor = MGPage(PageNumber).ForeColor
      ElseIf PageNumber > 0 Then
        MGPage(PageNumber).pctMG.FillStyle = 0    'Solid fill
        PenColor = MGPage(PageNumber).pctMG.ForeColor
        FillColor = MGPage(PageNumber).pctMG.ForeColor
      Else
        GoTo ErrHandler
      End If
    Case 3
      If PageNumber = 0 Then
        MGPage(PageNumber).FillStyle = 0      'Solid fill
        PenColor = MGPage(PageNumber).ForeColor
        FillColor = MGPage(PageNumber).BackColor
      ElseIf PageNumber > 0 Then
        MGPage(PageNumber).pctMG.FillStyle = 0     'Solid fill
        PenColor = MGPage(PageNumber).pctMG.ForeColor
        FillColor = MGPage(PageNumber).pctMG.BackColor
      Else
        GoTo ErrHandler
      End If
    Case 4
      If PageNumber = 0 Then
        MGPage(PageNumber).FillStyle = 0
        PenColor = MGPage(PageNumber).BackColor
        FillColor = MGPage(PageNumber).BackColor
      ElseIf PageNumber > 0 Then
        MGPage(PageNumber).pctMG.FillStyle = 0
        PenColor = MGPage(PageNumber).pctMG.BackColor
        FillColor = MGPage(PageNumber).pctMG.BackColor
      Else
        GoTo ErrHandler
      End If
    Case 5
      If PageNumber = 0 Then
        MGPage(PageNumber).FillStyle = 0    'Solid fill
        PenColor = MGPage(PageNumber).ForeColor
        FillColor = MGPage(PageNumber).FillColor
      ElseIf PageNumber > 0 Then
        MGPage(PageNumber).pctMG.FillStyle = 0
        PenColor = MGPage(PageNumber).pctMG.ForeColor
        FillColor = MGPage(PageNumber).pctMG.FillColor
      Else
        GoTo ErrHandler
      End If
  End Select
  If ITYPE > 1 Then
    hBrush = CreateSolidBrush(FillColor)
    solidbrushesCreated = solidbrushesCreated + 1
    hPen = CreatePen(PS_SOLID, 1, PenColor)
    pensCreated = pensCreated + 1
   ' Debug.Print "Pens and brushes created " & CStr(pensCreated) & "  " & CStr(solidbrushesCreated)
    If PageNumber = 0 Then
      OldPen = SelectObject(MGPage(PageNumber).hdc, hPen)
      OldBrush = SelectObject(MGPage(PageNumber).hdc, hBrush)
      RetVal = Polygon(MGPage(PageNumber).hdc, Vertex(1), nCount)
      RetVal = RestoreDC(MGPage(PageNumber).hdc, SavedDC)
      RetVal = SelectObject(MGPage(PageNumber).hdc, OldPen)  'Deselect pen and brush before deleting them
      RetVal = SelectObject(MGPage(PageNumber).hdc, OldBrush)
    ElseIf PageNumber > 0 Then
      OldPen = SelectObject(MGPage(PageNumber).pctMG.hdc, hPen)
      OldBrush = SelectObject(MGPage(PageNumber).pctMG.hdc, hBrush)
      RetVal = Polygon(MGPage(PageNumber).pctMG.hdc, Vertex(1), nCount)
      RetVal = RestoreDC(MGPage(PageNumber).pctMG.hdc, SavedDC)
      RetVal = SelectObject(MGPage(PageNumber).pctMG.hdc, OldPen)  'Deselect pen and brush before deleting them
      RetVal = SelectObject(MGPage(PageNumber).pctMG.hdc, OldBrush)
    Else
      GoTo ErrHandler
    End If
    
    If MetaFileFlagOn Then
      OldPen = SelectObject(EnhMetafileDC(PageNumber), hPen)
      OldBrush = SelectObject(EnhMetafileDC(PageNumber), hBrush)
      RetVal = Polygon(EnhMetafileDC(PageNumber), Vertex(1), nCount)
      RetVal = RestoreDC(EnhMetafileDC(PageNumber), SavedMetafileDC)
      RetVal = SelectObject(EnhMetafileDC(PageNumber), OldPen)  'Deselect pen and brush before deleting them
      RetVal = SelectObject(EnhMetafileDC(PageNumber), OldBrush)
    End If
    RetVal = DeleteObject(hPen)
    pensdeleted = pensdeleted + 1
    'Debug.Print "Pensdeleted " & CStr(pensdeleted)
    RetVal = DeleteObject(hBrush)
    brushesdeleted = brushesdeleted + 1
    'Debug.Print "Brushesdeleted " & CStr(brushesdeleted)
  End If
    
' Write the polygon construction to the PostScript file.
  If PSFlagOn Then
    Call PSPOLY(ITYPE, Verts, NVERTS)
  End If
  
  If PrinterFlagOn Then
    'Draw the border only
    Call PrinterPoly(ITYPE, Verts, NVERTS)
  End If
  Exit Sub
  
ErrHandler:
  MsgBox "Error drawing polygon", vbExclamation, "WARNING"
  Exit Sub
End Sub

Sub PrinterPoly(ITYPE As Integer, Verts() As xyrcoord, NVERTS As Integer)
'************************************************************************
  Dim I As Integer
  Printer.DrawMode = 13
  Printer.DrawStyle = 0
  Printer.DrawWidth = 1
  Printer.CurrentX = Verts(NVERTS).xcoord
  Printer.CurrentY = Verts(NVERTS).ycoord
  For I = 1 To NVERTS
    Printer.Line -(Verts(I).xcoord, Verts(I).ycoord)
  Next I
End Sub
