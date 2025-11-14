Attribute VB_Name = "VBMetgraph"
Option Explicit
Public PageNumber As Integer
Public MGPage(0 To 9) As Form

Type POINTAPI
  X As Long
  Y As Long
End Type

Type RECTS
  Left As Integer
  Top As Integer
  Right As Integer
  Bottom As Integer
End Type

Type SIZE
  cx As Long
  cy As Long
End Type

Type METAFILEHEADER    '22 bytes
  key As Long
  hMF As Integer
  bbox As RECTS
  inch As Integer
  reserved As Long
  checksum As Integer
End Type

' Logical Font
Public Const LF_FACESIZE = 32
Public Const LF_FULLFACESIZE = 64

Type LOGFONT
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
        lfFaceName(LF_FACESIZE) As Byte
End Type

Type PageFont
  Name As String
  SIZE As Integer
  italic As Boolean
  Bold As Boolean
End Type

Public Const MM_ISOTROPIC = 7
Public Const MM_ANISOTROPIC = 8
Public Const GMEM_MOVEABLE = &H2
Public Const GMEM_ZEROINIT = &H40

Public Const PS_SOLID = 0
Public Const WHITE_PEN = 6
Public Const BLACK_PEN = 7
Public Const NULL_PEN = 8

Public oldpoint As POINTAPI
Public hndMetaFile As Long
Public hndMFFont As Long
Public hndPen As Long
Public MetaFileDC As Long
Public MetaFileInch As Long  'Metafile units per inch

Public LineWidth As Long
Public OldPen As Long
Public LineType As Long

Public PSFlagOn As Boolean
Public PrinterFlagOn As Boolean
Public MetaFileFlagOn As Boolean

Public LabelFont As PageFont, Flyoutfont As PageFont


Public Declare Function CreateMetaFile Lib "gdi32" Alias "CreateMetaFileA" (ByVal lpString As String) As Long
Public Declare Function DeleteMetaFile Lib "gdi32" (ByVal hMF As Long) As Long
Public Declare Function CloseMetaFile Lib "gdi32" (ByVal hMF As Long) As Long
Public Declare Function PlayMetaFile Lib "gdi32" (ByVal hdc As Long, ByVal hMF As Long) As Long
Public Declare Function GetMetaFileBitsEx Lib "gdi32" (ByVal hMF As Long, ByVal nSize As Long, lpvData As Any) As Long
Public Declare Function SetWindowExtEx Lib "gdi32" (ByVal hdc As Long, ByVal nX As Long, ByVal nY As Long, lpSize As SIZE) As Long
Public Declare Function SetMapMode Lib "gdi32" (ByVal hdc As Long, ByVal nMapMode As Long) As Long
Public Declare Function CopyMetaFile Lib "gdi32" Alias "CopyMetaFileA" (ByVal hMF As Long, ByVal lpFileName As String) As Long
Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Declare Function CreateFontIndirect Lib "gdi32" Alias "CreateFontIndirectA" (lpLogFont As LOGFONT) As Long
Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long
Declare Function SelectObject Lib "gdi32" (ByVal hdc As Long, ByVal hObject As Long) As Long
Declare Function TextOut Lib "gdi32" Alias "TextOutA" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, ByVal lpString As String, ByVal nCount As Long) As Long

Public Declare Function MoveToEx Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long, lpPoint As POINTAPI) As Long
Public Declare Function LineTo Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal Y As Long) As Long
Public Declare Function Ellipse Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long

Public Declare Function lopen Lib "kernel32" Alias "_lopen" (ByVal lpPathName As String, ByVal iReadWrite As Long) As Long
Public Declare Function lclose Lib "kernel32" Alias "_lclose" (ByVal hFile As Long) As Long
Public Declare Function lcreat Lib "kernel32" Alias "_lcreat" (ByVal lpPathName As String, ByVal iAttribute As Long) As Long
Public Declare Function lwrite Lib "kernel32" Alias "_lwrite" (ByVal hFile As Long, lpBuffer As Any, ByVal wBytes As Long) As Long

Public Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Public Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalSize Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long

Sub MGCIRCLE(X As Single, Y As Single, Radius As Single)
'*******************************************************
  Dim di As Long, X1 As Long, Y1 As Long, X2 As Long, Y2 As Long
  MGPage(PageNumber).Circle (X, Y), Radius
  If PSFlagOn Then Call PSCIRC(X, Y, Radius)
  If MetaFileFlagOn Then
    X1 = X - Radius
    Y1 = Y - Radius
    X2 = X + Radius
    Y2 = Y + Radius
    di = Ellipse(MetaFileDC, X1, Y1, X2, Y2)
  End If
  If PrinterFlagOn Then Printer.Circle (X, Y), Radius
End Sub

Sub MGCOLOREDCIRCLE(X As Single, Y As Single, Radius As Single, Color As Long)
'*****************************************************************************
  Dim di As Long, X1 As Long, Y1 As Long, X2 As Long, Y2 As Long
  MGPage(PageNumber).FillColor = Color
  MGPage(PageNumber).FillStyle = 0
  MGPage(PageNumber).Circle (X, Y), Radius, Color
  MGPage(PageNumber).FillStyle = 1
  If PSFlagOn Then Call PSCIRC(X, Y, Radius)
  If MetaFileFlagOn Then
    X1 = X - Radius
    Y1 = Y - Radius
    X2 = X + Radius
    Y2 = Y + Radius
    di = Ellipse(MetaFileDC, X1, Y1, X2, Y2)
  End If
  If PrinterFlagOn Then Printer.Circle (X, Y), Radius, Color
End Sub

Public Sub MGSaveMetaFile(Left As Long, Top As Long, Right As Long, Bottom As Long, FileName As String)
'******************************************************************************************************
  'Saves a placeable Windows metafile to a disk file.
  Dim fhnd As Long, newmf As Long, mfglbhnd As Long
  Dim dc As Long, dl As Long, di As Long
  Dim mfile As METAFILEHEADER
  Dim xExt As Long, yExt As Long
  Dim oldsize As SIZE
  Dim mfinfosize As Long
  Dim currentfileloc As Long
  Dim gptr As Long
  
  On Error GoTo ErrHandler
  
  'Open the file to write
  fhnd = lcreat(FileName, 0)
  If fhnd >= 0 Then Call lclose(fhnd)  'Closes the open handle
  fhnd = lopen(FileName, 2)
  If fhnd < 0 Then
    MsgBox "Could not open the requested file", 16, "ERROR"
    Exit Sub
  End If
  'Open FileName For Binary Access Write As #31
  
  If hndMetaFile = 0 Then
    MsgBox "No metafile exists", 16, "ERROR"
    Exit Sub
  End If
    
  'Write the placeable header
  mfile.key = &H9AC6CDD7
  mfile.hMF = 0
  mfile.bbox.Left = Left
  mfile.bbox.Top = Top
  mfile.bbox.Right = Right
  mfile.bbox.Bottom = Bottom
  mfile.inch = MetaFileInch
  mfile.reserved = 0
  'Build the checksum
  mfile.checksum = &H9AC6 Xor &HCDD7
  mfile.checksum = mfile.checksum Xor mfile.bbox.Right
  mfile.checksum = mfile.checksum Xor mfile.bbox.Bottom
  mfile.checksum = mfile.checksum Xor mfile.inch

  'Write the header to the file
  dl = lwrite(fhnd, mfile, Len(mfile))
  
  'Create a metafile DC with the proper extents,
  'play the MG metafile into it and close it to retrieve its handle.
  dc = CreateMetaFile(vbNullString)
  xExt = Right - Left
  yExt = Bottom - Top
  dl = SetWindowExtEx(dc, xExt, yExt, oldsize)
  di = SetMapMode(dc, MM_ANISOTROPIC)
  di = PlayMetaFile(dc, hndMetaFile)
  newmf = CloseMetaFile(dc)
  
  'Get the size of the buffer
  mfinfosize = GetMetaFileBitsEx(newmf, 0, ByVal 0)
  If mfinfosize = 0 Then
    di = lclose(fhnd)
    Exit Sub
  End If
  
  'Allocate the space for the buffer
  mfglbhnd = GlobalAlloc(GMEM_MOVEABLE Or GMEM_ZEROINIT, mfinfosize)
  gptr = GlobalLock(mfglbhnd)
  dl = GetMetaFileBitsEx(newmf, mfinfosize, ByVal gptr)
  
  'Write the buffer to the file
  dl = lwrite(fhnd, ByVal gptr, mfinfosize)
  
  'Release the buffer
  di = GlobalUnlock(mfglbhnd)
  di = GlobalFree(mfglbhnd)
  
  'Close the file
  dl = lclose(fhnd)
  Exit Sub
  
ErrHandler:
  MsgBox "Could not write the metafile.", 16, "ERROR"
  Exit Sub
End Sub

 Sub SQUARE(IX As Single, IY As Single, N As Single)
'***************************************************
'This draws a square with centre at (IX,IY) and side length of 2N
 Call MGMOVE(IX + N, IY + N)
 Call MGPLOT(IX - N, IY + N)
 Call MGPLOT(IX - N, IY - N)
 Call MGPLOT(IX + N, IY - N)
 Call MGPLOT(IX + N, IY + N)
 End Sub


Sub MGLABEL(LABEL As String)
'***************************
  Dim dl As Long
  
  'MGPage(0).ForeColor = vbBlack
  MGPage(PageNumber).Print LABEL
  If PSFlagOn Then Call PSLAB(LABEL)
  If PrinterFlagOn Then Printer.Print LABEL
End Sub
Sub MGFONT(NewFont As PageFont)
'***************************
'Sets the page font to Newfont
With MGPage(0).FONT
  .Name = NewFont.Name
  .SIZE = NewFont.SIZE
  .Bold = NewFont.Bold
  .italic = NewFont.italic
End With
If PrinterFlagOn Then
  With Printer.FONT
    .Name = NewFont.Name
    .SIZE = NewFont.SIZE
    .Bold = NewFont.Bold
    .italic = NewFont.italic
  End With
End If
If PSFlagOn Then
  Call PSLBSZ(NewFont.SIZE)
End If
End Sub
Sub MGNEWFONT(NewFont As PageFont)
'*********************************
'Gets a new font using the font dialog
  Dim N As Integer
  On Error GoTo ErrHandler
  MGPage(0).CommonDialog1.CancelError = True
  MGPage(0).CommonDialog1.Flags = &H3&
  MGPage(0).CommonDialog1.FontName = MGPage(0).FONT.Name
  MGPage(0).CommonDialog1.ShowFont
    NewFont.Name = MGPage(0).CommonDialog1.FontName
    NewFont.SIZE = MGPage(0).CommonDialog1.FontSize
    NewFont.Bold = MGPage(0).CommonDialog1.FontBold
    NewFont.italic = MGPage(0).CommonDialog1.FontItalic
  'Printer font is set to the page font when print is selected on the menu
'  With Printer.FONT
'    .Name = MGPage(0).CommonDialog1.FontName
'    .SIZE = MGPage(0).CommonDialog1.FontSize
'    .Bold = MGPage(0).CommonDialog1.FontBold
'    .Italic = MGPage(0).CommonDialog1.FontItalic
'  End With
  PSFONTSIZE = MGPage(0).CommonDialog1.FontSize
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub
Sub MGLABSEG(TEXT As String, ISEG As Integer, IX As Single, IY As Single, OffsetX As Single, OffsetY As Single)
'**************************************************************************************************************
'Opens a segment to take a text string and write the label.
'
  Dim dl As Long, nCount As Long
  Dim X As Long, Y As Long
  'Call MGONSG
  'Call MGQSEG(ISEG)
  Call MGMOVE(IX - OffsetX, IY - OffsetY)
  If PSFlagOn = True Then
    Call PSMOVE(IX - OffsetX, IY - OffsetY + MGPage(0).TextHeight("M") + 5)
  End If
  Call MGLABEL(TEXT)
  If MetaFileFlagOn Then
    X = IX - OffsetX
    Y = IY - OffsetY - 2
    nCount = Len(TEXT)
    dl = TextOut(MetaFileDC, X, Y, TEXT, nCount)
  End If
  'Call MGCSEG
  End Sub

Public Sub MGMOVE(X As Single, Y As Single)
'******************************************
'  The Metgraph  move primitive
  Dim di As Long, lx As Long, ly As Long
  
  MGPage(PageNumber).CurrentX = X
  MGPage(PageNumber).CurrentY = Y
  If MetaFileFlagOn Then
    lx = X
    ly = Y
    di = MoveToEx(MetaFileDC, lx, ly, oldpoint)
  End If
  If PSFlagOn Then Call PSMOVE(X, Y)
  If PrinterFlagOn Then
    Printer.CurrentX = X
    Printer.CurrentY = Y
  End If
End Sub
 
Public Sub MGPLOT(X As Single, Y As Single)
'******************************************
'  The Metgraph plot primitive
  Dim lx As Long, ly As Long, dl As Long
  MGPage(PageNumber).Line -(X, Y)
  If MetaFileFlagOn Then
    lx = X
    ly = Y
    dl = LineTo(MetaFileDC, lx, ly)
  End If
  If PSFlagOn Then Call PSPLOT(X, Y)
  If PrinterFlagOn Then Printer.Line -(X, Y)
End Sub

Sub MGNUMLAB(Number As Integer)
'******************************
' Draws an integer as a label
  MGPage(PageNumber).CurrentX = MGPage(PageNumber).CurrentX
  MGPage(PageNumber).CurrentY = MGPage(PageNumber).CurrentY - MGPage(PageNumber).TextHeight("M")
  MGPage(PageNumber).Print Number
  If PSFlagOn Then Call PSLAB(CStr(Number))
  If PrinterFlagOn Then
    Printer.CurrentY = Printer.CurrentY - Printer.TextHeight("M")
    Printer.Print Number
  End If
End Sub

Sub MGLNTYPE(ITP As Integer)
'***************************
'  Sets the line type.
  Dim IT As Integer
  Dim OldPen As Long, nPenStyle As Long, di As Long
  IT = ITP - ((ITP - 1) \ 5) * 5 - 1
  If (ITP < 1) Then IT = 0
  MGPage(PageNumber).DrawStyle = IT
  nPenStyle = IT
  If MetaFileFlagOn Then
    If hndPen Then di = DeleteObject(hndPen)
    hndPen = CreatePen(nPenStyle, LineWidth, BLACK_PEN)
    OldPen = SelectObject(MetaFileDC, hndPen)
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
 Dim Pixels As Integer
 Dim di As Long
 
 Pixels = CInt(T / 0.26)
 If Pixels < 1 Then Pixels = 1
 LineWidth = Pixels
 MGPage(PageNumber).DrawWidth = Pixels
 If PSFlagOn Then
   PTS = 2.835 * T
   Call PSLNWD(PTS)
 End If
 If MetaFileFlagOn Then
    If hndPen Then di = DeleteObject(hndPen)
    hndPen = CreatePen(PS_SOLID, LineWidth, BLACK_PEN)
    OldPen = SelectObject(MetaFileDC, hndPen)
  End If
 
 End Sub
