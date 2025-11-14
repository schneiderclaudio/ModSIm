Attribute VB_Name = "VBPostScript"
'*************************  mgpostsc.bas  ****************************C
'                                                                     C
'                  METGRAPH INTERACTIVE GRAPHICS                      C
'                                                                     C
' This file holds the routines to create a PostScript program to      C
' display METGRAPH graphics on a PostScript printer.                  C
'                                                                     C
' (c) METGRAPH copyright RP KING  Johannesburg 1985                   C
' (c) Metgraph  copyright  R P King  Johannesburg 1995-1997           C
'                                                                     C
'*********************************************************************C
Option Explicit

 Private PSBUFF(200000) As String * 1
 Private PSPOIN As Long, STPSPN As Long
 Private FONT As String * 11
 Public PSFONTSIZE As Single
 Public GRAYFILL As Single
 Dim EPSI As Boolean
 Dim NGSAVE As Integer, ICHARSET As Integer
 Public IBBOX(4) As Integer, TRANS(2) As Single, ROT As Single, ScalePS(2) As Single
 Dim ICURR As Integer, MSEG As Integer
 
 Sub PSINIT()
 '***********
' Opens a buffer for PostScript graphics output.
' Set the PostScript flag on so that calls to MGPLOT
' will create an entry in the PostScript buffer.
'
'  Set the EPSI flag to false. PSCLOSE will produce an encapsulated
'  PostScript file for interchange when EPSI is true. There is a time penalty
'  over the straight PostScript file.
   EPSI = False
'
'  Create a new page for the PostScript output.
   Call PSCRPG(PageNumber)
   
'  Eliminate any previuosly saved PS buffer
   PagePSBufferSaved(PageNumber) = False
   
'  Initialise the buffer pointers.
   PSPOIN = 1
   STPSPN = 1
' Initialise the flags for the new page.
   FONT = "Helvetica"
   NGSAVE = 0
   GRAYFILL = 1#
   ICHARSET = 1
' Enable the PostScript flag for all pages. Pages that do not write the PostScript
' image continuously eg. Modsim's FLSheet, must control this separately in their
' activate event procedure.
   PSFlagOn = True
   
'  Set the initial label size to be used as a default
   PSFONTSIZE = 13
   Call PSLBSZ
End Sub

Sub PSCRPG(IP As Integer)
'************************
'  Creates a new PostScript page.
'  Set up the default bounding box
   IBBOX(1) = 0
   IBBOX(2) = 0
 '  IBBOX(3) = 577
   IBBOX(3) = 640
   IBBOX(4) = 430
   TRANS(1) = 12.5
   TRANS(2) = 12.5
   ScalePS(1) = 0.5
   ScalePS(2) = 0.5
   ROT = 0#
End Sub

Sub ReOpenPSPage()
'*****************
  'Re-opens, for further drawing, the PostScript buffer for a graphics page.  PageNumber = IP
  Call ReadPSBuffer
End Sub

Function BUFOFW(N As Integer) As Boolean
'****************************************
'  WARNS IF OVERFLOW IS IMMINENT.
  If PSPOIN + N >= 200000 Then
    MsgBox ("***WARNING...PostScript buffer is full. No further graphic elements will be added")
    BUFOFW = True
  Else
    BUFOFW = False
  End If
End Function

Sub PSBUFN(N As Integer)
'***********************
' Encodes integer N into the PostScript buffer.
  Dim WSTRING As String
  Dim I As Integer, IDIG As Integer
  WSTRING = " " & CStr(N)
  IDIG = Len(WSTRING)
  If BUFOFW(IDIG + 1) Then Exit Sub
  For I = 1 To IDIG
    PSBUFF(PSPOIN) = Mid(WSTRING, I, 1)
    PSPOIN = PSPOIN + 1
  Next I
 End Sub

Sub PSBUFR(R As Single)
'**********************
' Encodes real number R into the PostScript buffer.
  Dim WSTRING As String
  Dim I As Integer, IDIG As Integer
  WSTRING = Format(R, "###0.00")
  IDIG = Len(WSTRING)
  If BUFOFW(IDIG + 1) Then Exit Sub
  PSBUFF(PSPOIN) = " "
  PSPOIN = PSPOIN + 1
  For I = 1 To IDIG
    PSBUFF(PSPOIN) = Mid(WSTRING, I, 1)
    PSPOIN = PSPOIN + 1
  Next I
End Sub

Sub PSBUFW(WRD As String, IC As Integer)
'***************************************
' Encodes a word WRD having I' characters into the PostScript buffer.
  Dim I As Integer
  If BUFOFW(IC + 1) Then Exit Sub
  For I = 1 To IC
    PSBUFF(PSPOIN) = Mid(WRD, I, 1)
    PSPOIN = PSPOIN + 1
  Next I
End Sub


Sub PSPLOT(X As Single, Y As Single)
'**********************************
' The high resolution PLOT primitive for PostScript.
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFW(" lt", 3)
End Sub

Sub PSMOVE(X As Single, Y As Single)
'**********************************
' THE MOVE PRIMITIVE FOR POST SCRIPT.
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFW(" mt", 3)
End Sub

Sub PSRELMOVE(X As Single, Y As Single)
'*************************************
' THE RELATIVE MOVE PRIMITIVE FOR POST SCRIPT.
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFW(" rmt", 4)
End Sub
'
 Sub PSSTR()
'***********
'  Strokes a PostScript path.
 Call PSBUFW(" stroke", 7)
 End Sub
'
 Sub PSCLIP(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single)
'***************************************************************
' Clips the PostScript image
 Call PSBUFW(" stroke gsave", 13)
 Call PSBUFW(" newpath", 8)
 NGSAVE = NGSAVE + 1
 Call PSMOVE(X1, Y1)
 Call PSPLOT(X2, Y1)
 Call PSPLOT(X2, Y2)
 Call PSPLOT(X1, Y2)
 Call PSBUFW(" closepath", 10)
 Call PSBUFW(" clip", 5)
 Call PSBUFW(" newpath", 8)
 End Sub
'
 Sub PSCLIPOFF()
'***************
' Resets the clip path to the previous graphics state.
 Call PSBUFW(" stroke grestore", 16)
 NGSAVE = NGSAVE - 1
 End Sub

Sub PSCIRC(X As Single, Y As Single, FACT As Single)
'**************************************************
' DRAWS A SMOOTH CIRCLE OF RADIUS FACT CENTRED AT (X,Y).
' First stroke the current path and start a new path.
  Call PSBUFW(" stroke", 7)
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFR(FACT)
  Call PSBUFN(0)
  Call PSBUFN(360)
  Call PSBUFW(" arc", 4)
End Sub

Sub PSFCIRC(X As Single, Y As Single, FACT As Single)
'**************************************************
' DRAWS A SMOOTH FILLED CIRCLE OF RADIUS FACT CENTRED AT (X,Y).
' First stroke the current path and start a new path.
  Call PSBUFW(" stroke", 7)
  Call PSBUFW(" newpath", 8)
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFR(FACT)
  Call PSBUFN(0)
  Call PSBUFN(360)
  Call PSBUFW(" arc", 4)
  Call PSBUFW(" gsave", 6)
  Call PSBUFR(GRAYFILL)
  Call PSBUFW(" setgray", 8)
  Call PSBUFW(" fill", 5)
  Call PSBUFW(" grestore", 9)
End Sub
 Sub PSLAB(Label() As Integer, IC As Integer)
'*******************************************
'  Sends the string LABEL having IC characters to the PostScript buffer.
 Dim I As Integer
 If (ICHARSET = 3) Then
   Call PSOCTLAB(Label, IC)
   Exit Sub
 End If
 Call PSBUFW(" (", 2)
 For I = 1 To IC
   'Check for imbedded ( or )
   If Chr(Label(I)) = "(" Then
     PSBUFF(PSPOIN) = "\"
     PSPOIN = PSPOIN + 1
   End If
   If Chr(Label(I)) = ")" Then
     PSBUFF(PSPOIN) = "\"
     PSPOIN = PSPOIN + 1
   End If
   PSBUFF(PSPOIN) = Chr(Label(I))
   PSPOIN = PSPOIN + 1
 Next I
 Call PSBUFW(")", 1)
 Call PSBUFW(" show", 5)
 End Sub
'
 Sub PSOCTLAB(Label() As Integer, IC As Integer)
'**********************************************
' Writes a label to the Postscript buffer using octal codes.
' The label is broken into blocks of 10 characters to ensure that the
' PostScript file does not contain labels with a carriage return embedded.
  Dim ICC As Integer, istart As Integer, I As Integer
  Dim LabArray(1 To 10) As Integer
  ICC = IC
  istart = 0
Lbl10:
  If (ICC > 10) Then
    For I = 1 To 10
      LabArray(I) = Label(istart + I)
    Next I
    Call PSHIGHLAB(LabArray, 10)
    istart = istart + 10
    ICC = ICC - 10
  Else
    For I = 1 To ICC
      LabArray(I) = Label(istart + I)
    Next I
    Call PSHIGHLAB(LabArray, ICC)
    Exit Sub
  End If
GoTo Lbl10
End Sub
'
 Sub PSHIGHLAB(Label() As Integer, IC As Integer)
'***********************************************
' Writes a label to the Postscript buffer using octal codes
 Dim CODE As String * 4
 Dim I As Integer
 Dim IOCTAL As Integer, IOCTAL1 As Integer, IOCTAL2 As Integer, IOCTAL3 As Integer

 Call PSBUFW(" (", 2)
 For I = 1 To IC
   IOCTAL1 = (Label(I) + 128) \ 64
   IOCTAL = (Label(I) + 128) - IOCTAL1 * 64
   IOCTAL2 = IOCTAL \ 8
   IOCTAL3 = IOCTAL - IOCTAL2 * 8
   IOCTAL = 100 * IOCTAL1 + 10 * IOCTAL2 + IOCTAL3
   CODE = "\" & Format(IOCTAL, "000")
   Call PSBUFW(CODE, 4)
 Next I
 Call PSBUFW(")", 1)
 Call PSBUFW(" show", 5)
 End Sub

Sub PSSETFONT(N As Integer)
'**************************
' Selects a font
  ICHARSET = 1
  If N = 0 Then FONT = "Helvetica"
  If N = 1 Then FONT = "Helvetica"
  If N = 2 Then FONT = "Times-Roman"
  If N = 3 Then FONT = "Courier"
  If N = 4 Then
    FONT = "Symbol"
    ICHARSET = 2
  End If
  If N = 5 Then
    FONT = "Symbol"
    ICHARSET = 3
  End If
End Sub

Sub PSLBSZ()
'***********
  'Set the PostScript font size to PSFONTSIZE which is in points
  Dim FONTSIZE As Integer
  FONTSIZE = 2 * PSFONTSIZE   'Factor of 2 because PostScript output is scale by 0.5 to fit the page.
  If PSFlagOn Then
    Call PSBUFW(" /" & FONT & " findfont ", 23)
    Call PSBUFN(FONTSIZE)
    Call PSBUFW(" scalefont setfont", 18)
  End If
End Sub
'
 Sub PSROT(Angle As Single)
'**************************
'ROTATES THE COORDINATE SYSTEM ABOUT THE CURRENT POINT.
'
 Dim I As Integer
 I = Angle
 Call PSBUFW(" gsave currentpoint translate", 29)
 Call PSBUFN(I)
 Call PSBUFW(" rotate", 7)
 End Sub
'
 Sub PSCNRT()
'**********
'CANCELS THE PREVIOUS ROTATION.
'
 Call PSBUFW(" grestore", 9)
 End Sub

Sub PSLNTP(ITP As Integer)
'*************************
'  SETS UP THE LINE TYPE FOR PostScript.
'  Stroke the current path before changing to the new line type.
 Call PSBUFW(" stroke", 7)
 If (ITP < 2 Or ITP > 10) Then Call PSBUFW(" []", 3)
 If (ITP = 2) Then Call PSBUFW(" [3]", 4)
 If (ITP = 3) Then Call PSBUFW(" [5]", 4)
 If (ITP = 4) Then Call PSBUFW(" [7]", 4)
 If (ITP = 5) Then Call PSBUFW(" [14 7]", 7)
 If (ITP = 6) Then Call PSBUFW(" [10 5]", 7)
 If (ITP = 8) Then Call PSBUFW(" [1 4]", 6)
 If (ITP = 7) Then Call PSBUFW(" [18 5 1 5]", 11)
 If (ITP = 9) Then Call PSBUFW(" [11 5 3 3 3 5]", 15)
 If (ITP = 10) Then Call PSBUFW(" [6 5 3 3 3 5 3 5]", 18)
 Call PSBUFW(" 0 setdash", 10)
 End Sub

 Sub PSLNWD(w As Single)
'***********************
'  SETS THE LINE WIDTH
 Call PSBUFW(" stroke", 7)
 Call PSBUFR(w)
 Call PSBUFW(" setlinewidth", 13)
 End Sub
'
 Sub PSScale(X As Single, Y As Single)
'************************************
'  Scales the PostScript image.
 Call PSBUFR(X)
 Call PSBUFR(Y)
 Call PSBUFW(" scale", 6)
 End Sub
'
 Sub PSUNSCALE()
'*************
'  Unscales the PostScript image.
 End Sub
'
 Sub PSCURV(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single, X As Single, Y As Single)
'***************************************************************************************
'IMPLEMENTS THE curveto OPERATION IN PostScript.
 Call PSBUFR(X1)
 Call PSBUFR(Y1)
 Call PSBUFR(X2)
 Call PSBUFR(Y2)
 Call PSBUFR(X)
 Call PSBUFR(Y)
 Call PSBUFW(" curveto", 8)
 End Sub

Sub PSPOLY(ITYPE As Integer, Verts() As xyrcoord, NVERT As Integer)
'*****************************************************************
' Implements the polygon routine.
 Dim I As Integer
 Call PSBUFW(" stroke", 7)
 If (ITYPE = 4) Then Call PSBUFW(" gsave", 6)
 Call PSMOVE(Verts(1).xcoord, Verts(1).ycoord)
 For I = 2 To NVERT
   Call PSPLOT(Verts(I).xcoord, Verts(I).ycoord)
 Next I
 Call PSBUFW(" closepath", 10)
 If (ITYPE = 2) Then
   Call PSBUFW(" fill", 5)
 End If
 If (ITYPE = 3) Then
   Call PSBUFW(" gsave", 6)
   Call PSBUFR(GRAYFILL)
   Call PSBUFW(" setgray", 8)
   Call PSBUFW(" fill", 5)
   Call PSBUFW(" grestore", 9)
 End If
 If (ITYPE = 4) Then
   Call PSBUFR(GRAYFILL)
   Call PSBUFW(" setgray", 8)
   Call PSBUFW(" fill", 5)
   Call PSBUFW(" grestore", 9)
 End If
 If (ITYPE = 5) Then
   Call PSBUFW(" gsave", 6)
   Call PSBUFR(GRAYFILL)
   Call PSBUFW(" setgray", 8)
   Call PSBUFW(" fill", 5)
   Call PSBUFW(" grestore", 9)
 End If
End Sub

Sub PSCONCAT(CMT() As Single)
'****************************
' Concatenates the CMT matrix with matrix CMT
  Dim I As Integer
  Call PSBUFW(" [", 2)
  For I = 1 To 6
    Call PSBUFR(CMT(I))
  Next
  Call PSBUFW("] concat", 8)
End Sub

 Sub PSGRAYFILL(F As Single)
'***************************
' Sets the PostScript gray level
 GRAYFILL = F
 End Sub

Sub PSCOPY()
'***********
  'Write the PostScript file
  Dim FileName As String
  FileName = RTrim(JobPath) & "PSFILE" & Format(PageNumber, "00") & ".ps"
  Call WritePSFile(FileName)
End Sub

Sub WritePSFile(FileName As String)
'**********************************
  'Writes a PostScript file
   Dim N As Integer
   
   Open FileName For Output As #11
  'Print the program header.
   Print #11, "%!PS-Adobe-2.0 EPSF-1.2"
   Print #11, "%This is a PostScript program produced by METGRAPH."
   Print #11, "%%Creator: METGRAPH"
   Print #11, "%%BoundingBox: "; IBBOX(1); IBBOX(2); IBBOX(3); IBBOX(4)
   Print #11, "%%Pages: 1"
   Print #11, "%%EndComments"
   Print #11, "%%BeginProlog"
   Print #11, "/mm {2.835 mul} def"
   Print #11, "/lt /lineto load def"
   Print #11, "/mt /moveto load def"
   Print #11, "/rmt /rmoveto load def"
   Print #11, "%%EndProlog"
   Print #11, "%%Page: 1 1"
   Print #11, "save "
   Print #11, Format(TRANS(1), "Fixed"); " mm "; Format(TRANS(2), "Fixed"); " mm translate"
   Print #11, ROT; "rotate"
   Print #11, 0.5; 0.5; "scale"
   Print #11, "1 setlinewidth 2 setlinecap 1 setlinejoin"
  ' Flushes the postscript buffer, strokes the path, restores, adds the METGRAPH
  ' logo if necessary, adds the TIFF image to the end of the file if necessary
  ' then closes the file.

  Call FLPSBU
  Print #11, "stroke"
  ' Close any open gsaves
  For N = 1 To NGSAVE
    Print #11, "grestore"
  Next N
  NGSAVE = 0
  ' Print #11, "/Helvetica findfont 10 scalefont setfont"
  ' Print #11, "900 -20 mt ( graphics by METGRAPH) show')"

  Print #11, "restore"
  Print #11, "showpage"
  Print #11, "%%Trailer"

  ' The following are commented out because MS WORD cannot recognize the
  ' atend keyword in PostScript
  '      Print(LU, 1004) (IBBOX(I),I = 1,4)
  ' 1004 FORMAT('%%BoundingBox: ',4I5)
  Print #11, "%%EOF"
  Close #11
End Sub

Sub SavePSBuffer()
'*****************
' Saves the current PostScript buffer for subsequent additions.
 Dim I As Long
 Dim FileName As String
 FileName = RTrim(JobPath) & "PSBuffer" & Format(PageNumber, "00")
 Open FileName For Output As #11
 Print #11, PSPOIN, STPSPN
 For I = 1 To PSPOIN - 1
   Print #11, PSBUFF(I);
 Next I
 Print #11, vbCrLf
 Close #11
 PagePSBufferSaved(PageNumber) = True
End Sub

Public Sub ReadPSBuffer()
'************************
'Reads a PostScript buffer that was previously saved.
 Dim I As Long
 Dim FileName As String
 FileName = RTrim(JobPath) & "PSBuffer" & Format(PageNumber, "00")
 Open FileName For Input As #12
 Input #12, PSPOIN, STPSPN
 For I = 1 To PSPOIN - 1
   PSBUFF(I) = Input(1, #12)
 Next I
 Close #12

End Sub

Sub PSWIPE(I1 As Integer, I2 As Integer)
'**************************************
' WIPES OUT THE POSTSCRIPT BUFFER FROM LOCATION I1 TO I2 INCLUSIVE.
'
  Dim IDIFF As Long, J As Long
  IDIFF = I2 - I1 + 1
  If (I2 + 1 <= PSPOIN - 1) Then
    For J = I2 + 1 To PSPOIN - 1
      PSBUFF(J - IDIFF) = PSBUFF(J)
    Next J
  End If
  PSPOIN = PSPOIN - IDIFF
  If STPSPN <= I1 Then Exit Sub
  If STPSPN <= I2 Then STPSPN = I1
  If STPSPN > I2 Then STPSPN = STPSPN - IDIFF
  End Sub

Sub FLPSBU()
'***********
'  Flushes the PostScript buffer to the print file.
'  The records must end with white space.
Dim I1 As Long, I2 As Long, IM As Long, I As Long
Dim STPSPNM As Long

STPSPNM = STPSPN
While STPSPN < PSPOIN
  I2 = STPSPN + 79
  If I2 >= PSPOIN Then I2 = PSPOIN - 1
  'Start all strings on a new line
  For I = STPSPN + 1 To I2
    If PSBUFF(I) = "(" And PSBUFF(I - 1) <> "\" Then
      IM = I - 1
      GoTo 30
    End If
  Next I
  I1 = I2 - STPSPN - 2
  For I = 1 To I1
    IM = I2 - I + 1
    If PSBUFF(IM) = " " Then GoTo 30
  Next I
  IM = I2
30:
  For I = STPSPN To IM
    Print #11, PSBUFF(I);
  Next I
  Print #11,
  STPSPN = IM + 1
Wend
If (EPSI) Then
  STPSPN = STPSPNM
Else
  STPSPN = 1
  'PSPOIN = 1
End If
End Sub
