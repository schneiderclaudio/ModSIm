Attribute VB_Name = "Tiff"
Option Explicit

Private LLX As Long, LLY As Long, URX As Long, URY As Long

' TIFF file header data structure
  Type TIFF_FILE_HEADER
    byte_order  As Integer           ' byte order
    Version As Integer               ' version
    ifd_offset  As Long              ' offset to first IFD
  End Type

' TIFF tag data TYPE
  Type A_TIFF_TAG
    Tag As Integer                   ' tag number
    type  As Integer                 ' tag type
    count As Long                    ' number of items of length 'type'
    offset As Long                   ' offset or data value
  End Type

'EPSI file header
  Type EPSI_HEADER
    Tag As Long
    HeaderLength As Long
    PSFileLength As Long
    Unused1 As Long
    Unused2 As Long
    TiffOffset As Long
    TiffFileSize As Long
    Terminator As Integer
  End Type

Sub MAKE_TIFF()
'**************
  Dim FileName As String, FileExists As String
  Dim TIFF_FILE_SIZE As Long
  MGPage(PageNumber).MousePointer = 11
  MGPage(PageNumber).pctMG.MousePointer = 11
  
  FileName = RTrim(JobPath) & "TIFF" & Format(PageNumber, "00") & ".tif"
  FileExists = Dir(FileName)
  If FileExists <> "" Then
    Kill FileName
  End If
  
  'Open and close the file so that it exists to put the TIFF data
  Open FileName For Binary Access Write As #12
  Close #12
  Call MGC_GET_BOUND_BOX(LLX, LLY, URX, URY)
  Call MG_MAKE_TIFF(FileName, TIFF_FILE_SIZE)
  MGPage(PageNumber).MousePointer = 0
  MGPage(PageNumber).pctMG.MousePointer = 2

End Sub

Function MAKE_EPSI() As Integer
'******************************
'Copies the existing PostScript file, flushes the postscript buffer and and closes the epsi file.
'      Logical GRPFL, EPSI
'      INTEGER PSPOIN,STPSPN
'      COMMON/MGPSBU/PSPOIN,STPSPN
  Dim FileName As String, FileExists As String
  Dim LineOfText As String, WSTRING As String
  Dim sLLX As Single, sLLY As Single, sURX As Single, sURY As Single, IMAGE_SIZE As Long
  Dim BND_BOX(4) As Integer
  Dim Header As EPSI_HEADER
  Dim PS_FILE_LENGTH As Long, TIFF_FILE_SIZE As Long, LINE_NUMBER As Long
  Dim I As Integer
  
  On Error GoTo ErrHandler
  
  MAKE_EPSI = 0
  MGPage(PageNumber).MousePointer = 11
  MGPage(PageNumber).pctMG.MousePointer = 11
  Call PSCOPY
  
  FileName = RTrim(JobPath) & "EPSI" & Format(PageNumber, "00") & ".eps"
  FileExists = Dir(FileName)
  If FileExists <> "" Then
    Kill FileName
  End If
  Open FileName For Binary Access Write As #12

  'Write the index header to the file
  Header.Tag = &HC6D3D0C5
  Header.HeaderLength = &H1E
  Header.PSFileLength = 0
  Header.Unused1 = 0
  Header.Unused2 = 0
  Header.TiffOffset = 0
  Header.TiffFileSize = 0
  Header.Terminator = &HFFFF

  Call GET_BOUND_BOX(sLLX, sLLY, sURX, sURY, IMAGE_SIZE)
  'Adjust the bounding box for the scale factor and the translated origin.
  If ROT = 90# Then
    BND_BOX(1) = TRANS(1) * 72# / 25.4 - ScalePS(2) * sURY
    BND_BOX(2) = TRANS(2) * 72# / 25.4 + ScalePS(1) * sLLX
    BND_BOX(3) = TRANS(1) * 72# / 25.4 - ScalePS(2) * sLLY
    BND_BOX(4) = TRANS(2) * 72# / 25.4 + ScalePS(1) * sURX
  Else
    BND_BOX(1) = ScalePS(1) * sLLX + TRANS(1) * 72# / 25.4
    BND_BOX(2) = ScalePS(2) * sLLY + TRANS(2) * 72# / 25.4
    BND_BOX(3) = ScalePS(1) * sURX + TRANS(1) * 72# / 25.4
    BND_BOX(4) = ScalePS(2) * sURY + TRANS(2) * 72# / 25.4
  End If
'
'     This is replaced lower down
 '     IHead(7) = 194 + IMAGE_SIZE / 8
      
  'Write the header into the EPSI file
  Put #12, 1, Header
  Close #12
' Flush the PostScript buffer
'      Call FLPSBU
'      WRITE(LUM,1000)
' 1000 FORMAT('stroke')
' Close any open gsaves
'      DO N = 1,NGSAVE
'        WRITE(LUM,*)'grestore'
'      END DO
'      NGSAVE = 0

' Transfer the contents of the PSFILE, counting the number of bytes
  PS_FILE_LENGTH = 0 '

' Allow for the cr lf pair at the end of the header
'  PS_FILE_LENGTH = 2
  LINE_NUMBER = 0
  FileExists = Dir(RTrim(JobPath) & "PSFILE" & Format(PageNumber, "00") & ".ps")
  Open RTrim(JobPath) & "PSFILE" & Format(PageNumber, "00") & ".ps" For Input As #11
  Open FileName For Append As #12
  Do While Not EOF(11)
    Line Input #11, LineOfText
    LINE_NUMBER = LINE_NUMBER + 1
    If Mid(LineOfText, 1, 7) = "%%Bound" Then
      WSTRING = "%%BoundingBox: " & Format(BND_BOX(1), "###0") & " " & Format(BND_BOX(2), "###0") & " " & Format(BND_BOX(3), "###0") & " " & Format(BND_BOX(4), "###0")
      Print #12, WSTRING
      PS_FILE_LENGTH = PS_FILE_LENGTH + Len(WSTRING) + 2
    ElseIf LINE_NUMBER = 15 Then
      WSTRING = Format(TRANS(1), "#######0.0") & " mm " & Format(TRANS(2), "#######0.0") & " mm" & " translate"
      Print #12, WSTRING
      PS_FILE_LENGTH = PS_FILE_LENGTH + Len(WSTRING) + 2
    ElseIf LINE_NUMBER = 16 Then
      WSTRING = " 0 rotate"
      Print #12, WSTRING
      PS_FILE_LENGTH = PS_FILE_LENGTH + Len(WSTRING) + 2
    Else
      Print #12, LineOfText
      PS_FILE_LENGTH = PS_FILE_LENGTH + Len(LineOfText) + 2
    End If
  Loop
'      Else
'        BACKSPACE(LUM, ERR = 999)
'        EPSI =.TRUE.
'      End If

'      WRITE(LOGICAL_UNIT,1001)
' 1001 FORMAT('restore')
'      WRITE(LOGICAL_UNIT,1002)
' 1002 FORMAT('%%Trailer')
'      PS_FILE_LENGTH = PS_FILE_LENGTH + 16 + 4'

' The following lines are commented out because MS WORD can't recognize the
' atend keyword in the PostScript file
'     Write the bounding box data To the PostScript file.
'      WRITE(LOGICAL_UNIT, 1003) (BND_BOX(I),I = 1,4)
' 1003 FORMAT('%%BoundingBox: ',4I5)
'      PS_FILE_LENGTH = PS_FILE_LENGTH + 35 + 2

'      WRITE(LOGICAL_UNIT,1005)
' 1005 FORMAT('%EOF')
'      PS_FILE_LENGTH = PS_FILE_LENGTH + 4 !+ 2
'      Close (LOGICAL_UNIT)'
  Close #12
  Close #11
  Call MG_MAKE_TIFF(FileName, TIFF_FILE_SIZE)
' Re-open the EPSI file to rewrite the header with the correct PostScript and TIFF file lengths.
  Header.PSFileLength = PS_FILE_LENGTH
  Header.TiffOffset = Header.HeaderLength + Header.PSFileLength + 2
  Header.TiffFileSize = TIFF_FILE_SIZE
  Open FileName For Binary Access Write As #12
  Put #12, 1, Header
  Close (12)
  MAKE_EPSI = -1
  MGPage(PageNumber).MousePointer = 0
  MGPage(PageNumber).pctMG.MousePointer = 2
  Exit Function
  
ErrHandler:
  MsgBox "Error occured when making espi file", vbCritical, "ERROR"
  MAKE_EPSI = 0
  MGPage(PageNumber).MousePointer = 0
  MGPage(PageNumber).pctMG.MousePointer = 2
  
  Exit Function
End Function

Sub GET_BOUND_BOX(sLLX As Single, sLLY As Single, sURX As Single, sURY As Single, IMAGE_SIZE As Long)
'************************************************************************************************
' Finds the extent of the graphic image for the PostScript bounding box.
  Call MGC_GET_BOUND_BOX(LLX, LLY, URX, URY)
  IMAGE_SIZE = (URX - LLX + 1) * (LLY - URY + 1)
' Convert from logical coords to 780x1024 system.
  sLLX = LLX * 1024 / MGPage(PageNumber).pctMG.ScaleWidth
  sLLY = (MGPage(PageNumber).pctMG.ScaleHeight - LLY) * 780 / MGPage(PageNumber).pctMG.ScaleHeight
  sURX = URX * 1024 / MGPage(PageNumber).pctMG.ScaleWidth
  sURY = (MGPage(PageNumber).pctMG.ScaleHeight - URY) * 780 / MGPage(PageNumber).pctMG.ScaleHeight
End Sub

Sub MGC_GET_BOUND_BOX(LLX As Long, LLY As Long, URX As Long, URY As Long)
'************************************************************************
' Finds the extreme extent of the graphic image. The message line is excluded.
  Dim IX As Long, IY As Long, istart As Long, remainder As Integer, remc As Integer
  Dim itest As Long
  Dim X1 As Single, Y1 As Single, X2 As Single, Y2 As Single
  LLX = MGPage(PageNumber).pctMG.ScaleWidth - 1
  LLY = 0
  URY = 0
  URX = 0
  istart = 0
  
  For IY = MGPage(PageNumber).pctMG.ScaleHeight - 1 To 0 Step -4
    For IX = 0 To LLX
      itest = GetPixel(MGPage(PageNumber).pctMG.hdc, IX, IY)
      If (itest > -1 And itest <> MGPage(PageNumber).ActualBackColor) Then
        If LLY = 0 Then LLY = IY
        Exit For
      End If
    Next IX
    LLX = IX - 1
  Next IY
  For IY = 0 To MGPage(PageNumber).pctMG.ScaleHeight - 1 Step 4
    For IX = MGPage(PageNumber).pctMG.ScaleWidth - 1 To URX Step -4
      itest = GetPixel(MGPage(PageNumber).pctMG.hdc, IX, IY)
      If (itest > -1 And itest <> MGPage(PageNumber).ActualBackColor) Then
        If URY = 0 Then URY = IY
        Exit For
      End If
    Next IX
    URX = IX + 1
  Next IY
' Make the image width a multiple of 8 pixels...
  remainder = (URX - LLX + 1) Mod 8
  remc = 8 - remainder
  LLX = LLX - remc \ 2
  URX = URX + remc - remc \ 2

'... and then add a 8-pixel frame on each side
  LLX = LLX - 8
  URX = URX + 8
  LLY = LLY + 8
  URY = URY - 8
  X1 = LLX
  Y1 = LLY
  X2 = URX
  Y2 = URY
  MGPage(PageNumber).pctMG.FillStyle = 1
  MGPage(PageNumber).pctMG.Line (X1, Y1)-(X2, Y2), RGB(0, 255, 0), B
  End Sub

Sub MG_MAKE_TIFF(FILE_NAME As String, Size As Long)
'**************************************************
' Makes a tiff image of the screen

'  TIFF HEADER CONSTANTS
  Const qc_tiff_byte_order As Integer = &H4949   ' Intel byte ordering
  Const qc_tiff_version As Integer = 42          ' TIFF version number
  Const qc_tiff_ifd_offset As Integer = 8        ' offset to start of IFD
  Const qc_tiff_header_size As Integer = 8       ' TIFF header size
  Const qc_tiff_tag_size As Integer = 12         ' TIFF tag structure size
  Const qc_tiff_num_tags As Integer = 14         ' number of tags used
                                                         ' size of IFD
  Const qc_software As String * 8 = "METGRAPH"     ' software string

  Dim I As Integer, J As Integer, K As Integer
  Dim itest As Long
  Dim NextByte As Long
  Dim packed As Byte            ' Holds the packed bytes
  Dim ifd_offset As Long        ' running count for start of IFD offset data
  Dim software_len As Integer   ' length of software string
  Dim num_tags As Integer       ' number of TIFF tags used
  Dim null_ifd_ptr As Long      ' NULL IFD pointer
  Dim X As Long                 ' Used for resolution calculations
  Dim width As Long, height As Long  ' Width and height of image in pixels
  Dim qc_tiff_ifd_size As Integer          ' Length of the all IFD entries

  Dim file_header As TIFF_FILE_HEADER     ' TIFF file header structure
  Dim tiff_tag As A_TIFF_TAG              ' TIFF tag field structure

  ' Open the file to append the tiff image
  NextByte = FileLen(FILE_NAME) + 1
  Open FILE_NAME For Binary Access Write As #12
  If NextByte > 1 Then
    Put #12, NextByte, "  "
  End If

  ' Calculate the offset
  qc_tiff_ifd_size = qc_tiff_num_tags * qc_tiff_tag_size
  software_len = Len(qc_software)
  '  Header + Directory entries + All the tag entries + Offset to next IFD
  ifd_offset = qc_tiff_header_size + 2 + qc_tiff_ifd_size + 4

  ' Calculate the width and height of the image
   width = URX - LLX + 1
   height = -(URY - LLY) + 1

  ' Create the TIFF header
  file_header.byte_order = qc_tiff_byte_order
  file_header.Version = qc_tiff_version
  file_header.ifd_offset = qc_tiff_ifd_offset

  ' write the TIFF header
  Put #12, , file_header

  ' Start writing the IFD
  ' Write the number of tags
  num_tags = qc_tiff_num_tags
  Put #12, , num_tags

  ' SubfileType
  tiff_tag.Tag = &HFF
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = 1
  Put #12, , tiff_tag

  ' ImageWidth
  tiff_tag.Tag = &H100
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = width
  Put #12, , tiff_tag

  ' ImageHeight
  tiff_tag.Tag = &H101
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = height
  Put #12, , tiff_tag

  ' BitsPerSample
  tiff_tag.Tag = &H102
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = 1
  Put #12, , tiff_tag

  ' Compression
  tiff_tag.Tag = &H103
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = 1
  Put #12, , tiff_tag

  ' PhotometricInterpretation
  tiff_tag.Tag = &H106
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = 0
  Put #12, , tiff_tag


  ' StripOffsets
  tiff_tag.Tag = &H111
  tiff_tag.type = 4
  tiff_tag.count = 1
  tiff_tag.offset = ifd_offset + 2 * 4 + 2 * 4 + software_len
  Put #12, , tiff_tag

  ' SamplesPerPixel
  tiff_tag.Tag = &H115
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = 1
  Put #12, , tiff_tag

  ' RowsPerStrip
  tiff_tag.Tag = &H116
  tiff_tag.type = 4
  tiff_tag.count = 1
  tiff_tag.offset = height
  Put #12, , tiff_tag

  ' StripByteCounts
  tiff_tag.Tag = &H117
  tiff_tag.type = 4
  tiff_tag.count = 1
  tiff_tag.offset = height * width
Put #12, , tiff_tag

  ' XResolution
  tiff_tag.Tag = &H11A
  tiff_tag.type = 5
  tiff_tag.count = 1
  tiff_tag.offset = ifd_offset
Put #12, , tiff_tag
  ifd_offset = ifd_offset + 2 * 4

  ' YResolution
  tiff_tag.Tag = &H11B
  tiff_tag.type = 5
  tiff_tag.count = 1
  tiff_tag.offset = ifd_offset
Put #12, , tiff_tag
  ifd_offset = ifd_offset + 2 * 4

  ' ResolutionUnit
  tiff_tag.Tag = &H128
  tiff_tag.type = 3
  tiff_tag.count = 1
  tiff_tag.offset = &H2
Put #12, , tiff_tag

  ' Software
  tiff_tag.Tag = &H131
  tiff_tag.type = 2
  tiff_tag.count = software_len
  tiff_tag.offset = ifd_offset
Put #12, , tiff_tag
  ifd_offset = ifd_offset + software_len
  Size = ifd_offset

  ' Write the null ifd pointer
  null_ifd_ptr = 0
Put #12, , null_ifd_ptr

  ' Write the XResolution
  X = 72
Put #12, , X
  X = 1
Put #12, , X

  ' Write the YResolution
  X = 72
Put #12, , X
  X = 1
Put #12, , X

   ' Write the software string
Put #12, , qc_software

' Make a TIFF image of the graphics within the bounding box
  K = 0
  packed = 0
  For I = URY To LLY
    For J = LLX To URX
      itest = GetPixel(MGPage(PageNumber).pctMG.hdc, J, I)
      If itest > -1 And itest <> MGPage(PageNumber).pctMG.BackColor And itest <> MGPage(PageNumber).pctMG.FillColor Then
        packed = packed Or (7 - K)
      End If
      K = K + 1
      If K = 8 Then
        K = 0
        Put #12, , packed
        Size = Size + 1
        packed = 0
      End If
    Next J
  Next I
  ' close the TIFF FILE
  Close #12
End Sub
