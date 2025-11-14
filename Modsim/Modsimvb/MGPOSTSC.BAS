Attribute VB_Name = "VBPostScript"
'*************************  mgpostsc.bas  ****************************C
'                                                                     C
'                  METGRAPH INTERACTIVE GRAPHICS                      C
'                                                                     C
' This file holds the routines to create a PostScript program to      C
' display METGRAPH graphics on a PostScript printer.                  C
'                                                                     C
' (c) METGRAPH copyright RP KING  Johannesburg 1985                   C
' (c) Metgraph  copyright  R P King  Johannesburg 1995-1997         C
'                                                                     C
'*********************************************************************C
Option Explicit

 Public PSPath As String
 Dim PSBUFF(100000) As String * 1
 Dim PSPOIN As Long, STPSPN As Long
 Dim GRPFL As Boolean, EPSI As Boolean
 Dim NGSAVE As Integer, GRAYFILL As Single, ICHARSET As Integer
 Dim IBBOX(4) As Integer, TRANS(2) As Single, ROT As Single
 Dim PSScale(2) As Single
 Dim FONT As String * 11
 Public PSFONTSIZE As Integer
 Dim ISEGST(500) As Long, ISEGEN(500) As Long
 Dim ICURR As Integer, MSEG As Integer
Sub PSINIT()
'***********
' Opens a buffer for PostScript graphics output.
' Set the PostScript flag on so that calls to MGPLOT
' will create an entry in the PostScript buffer.
 ' VBMetgraph.PSFlagOn = True
'
'  Set move/draw flag flag to FALSE
'  Calls to PLOT produce a line when GRPFL is true and a move otherwise.
   GRPFL = False
'
'  Set the EPSI flag to false. PSCLOSE will produced an encapsulated
'  PostScript file for interchange when EPSI is true. There is a time penalty
'  over the straight PostScript file.
   EPSI = False
'
'  Create a new page for the PostScript output.
   Call PSCRPG(1)
   
End Sub

Sub PSCRPG(IP As Integer)
'************************
'  Creates a new PostScript page.
   Dim FNAME As String * 8
   
   On Error GoTo ErrHandler

'  Initialise the buffer pointers.
   PSPOIN = 1
   STPSPN = 1

'  Set up the default bounding box
   IBBOX(1) = 0
   IBBOX(2) = 0
   IBBOX(3) = 577
   IBBOX(4) = 430
   TRANS(1) = 12.5
   TRANS(2) = 12.5
   PSScale(1) = 0.47
   PSScale(2) = 0.47
   ROT = 0#

   FNAME = "PSFILE"
   If (IP > 1) Then
     Call PCLOSE
   End If
'

'  Print the program header.
   Print #11, "%!PS-Adobe-2.0 EPSF-1.2"
   Print #11, "%This is a PostScript program produced by METGRAPH."
   Print #11, "%%Creator: METGRAPH"
   Print #11, "%%BoundingBox: "; IBBOX(1); IBBOX(2); IBBOX(3); IBBOX(4)
   Print #11, "%%Pages: 1"
   Print #11, "%%EndComments"
   Print #11, "/mm {2.835 mul} def"
   Print #11, "/lt /lineto load def"
   Print #11, "/mt /moveto load def"
   Print #11, "/rmt /rmoveto load def"
   Print #11, "%%EndProlog"
   Print #11, "save "
   Print #11, Format(TRANS(1), "Fixed"); " mm "; Format(TRANS(2), "Fixed"); " mm translate"
   Print #11, ROT; "rotate"
   Print #11, PSScale(1); PSScale(2); "scale"
   Print #11, "1 setlinewidth 2 setlinecap 1 setlinejoin"
' Initialise the flags for the new page.
   FONT = "Helvetica"
   NGSAVE = 0
   GRAYFILL = 1#
   ICHARSET = 1
' Enable the PostScript flag
   PSFlagOn = True
   Exit Sub
'  Set the label size
   Call PSLBSZ(PSFONTSIZE)
   
ErrHandler:
   MsgBox "Error when opening the PostScript buffer", 16, "ERROR"
   Exit Sub
End Sub

      Sub PCLOSE()
'     ************
' Flushes the postscript buffer, strokes the path, restores, adds the METGRAPH
' logo if necessary, adds the TIFF image to the end of the file if necessary
' then closes the file.
  Dim N As Integer
  On Error GoTo ErrHandler
  
  Call FLPSBU
  Print #11, "stroke"
' Close any open gsaves
  For N = 1 To NGSAVE
    Print #11, "grestore"
  Next N
  NGSAVE = 0

Print #11, "restore"
Print #11, "showpage"
Print #11, "%%Trailer"

' The following are commented out because MS WORD cannot recognize the
' atend keyword in PostScript
'      Print(LU, 1004) (IBBOX(I),I = 1,4)
' 1004 FORMAT('%%BoundingBox: ',4I5)
Print #11, "%%EOF"
Close #11
VBMetgraph.PSFlagOn = False
Exit Sub

ErrHandler:
  Exit Sub
End Sub

Sub FLPSBU()
'***********
'  Flushes the PostScript buffer to the print file.
'  The records must end with white space.
Dim I1 As Long, I2 As Long, IM As Long, I As Long
Dim STPSPNM As Integer
STPSPNM = STPSPN
While STPSPN < PSPOIN
  I2 = STPSPN + 79
  If I2 >= PSPOIN Then I2 = PSPOIN - 1
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
  PSPOIN = 1
End If
End Sub

Sub BUFOFW(N As Integer)
'***********************
'  WARNS IF OVERFLOW IS IMMINENT AND FLUSHES THE BUFFER.
  If PSPOIN + N >= 100000 Then
    MsgBox ("***WARNING...PostScript buffer is full and is being flushed")
    Call FLPSBU
  End If
End Sub

Sub PSBUFN(N As Integer)
'***********************
' Encodes integer N into the PostScript buffer.
  Dim WSTRING As String
  Dim I As Integer, IDIG As Integer
  WSTRING = Str(N)
  IDIG = Len(WSTRING)
  Call BUFOFW(IDIG + 1)
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
  WSTRING = Format(R, "####.##")
  IDIG = Len(WSTRING)
  Call BUFOFW(IDIG + 1)
  PSBUFF(PSPOIN) = " "
  PSPOIN = PSPOIN + 1
  For I = 1 To IDIG
    PSBUFF(PSPOIN) = Mid(WSTRING, I, 1)
    PSPOIN = PSPOIN + 1
  Next I
End Sub

Sub PSBUFW(WRD As String, IC As Integer)
'***************************************
' Encodes a word WRD having IC characters into the PostScript buffer.
  Dim I As Integer
  Call BUFOFW(IC + 1)
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

Sub PSCIRC(X As Single, Y As Single, Fact As Single)
'**************************************************
' DRAWS A SMOOTH CIRCLE OF RADIUS FACT CENTRED AT (X,Y).
' First stroke the current path and start a new path.
  Call PSBUFW(" stroke", 7)
  Call PSBUFR(X)
  Call PSBUFR(Y)
  Call PSBUFR(Fact)
  Call PSBUFN(0)
  Call PSBUFN(360)
  Call PSBUFW(" arc", 4)
End Sub

Sub PSLAB(LABEL As String)
'*************************
' Sends the string LABEL having IC characters to the PostScript buffer.
  If ICHARSET = 3 Then
    Call PSOCTLAB(LABEL, Len(LABEL))
    Exit Sub
  End If
  Call PSBUFW(" (", 2)
  Call PSBUFW(LABEL, Len(LABEL))
  Call PSBUFW(")", 1)
  Call PSBUFW(" show", 5)
End Sub

Sub PSOCTLAB(LABEL As String, IC As Integer)
'******************************************
' Writes a label to the Postscript buffer using octal codes.
' The label is broken into blocks of 10 characters to ensure that the
' PostScript file does not contain labels with a carriage return embedded.

  Dim ICC As Integer, ISTART As Integer
  ICC = IC
  ISTART = 1
  Do While ISTART <= IC
    If (ICC > 10) Then
      Call PSHIGHLAB(Mid(LABEL, ISTART, 10), 10)
      ISTART = ISTART + 10
      ICC = ICC - 10
    Else
      Call PSHIGHLAB(Mid(LABEL, ISTART, ICC), ICC)
      Exit Do
    End If
  Loop
  End Sub

Sub PSHIGHLAB(LABEL As String, IC As Integer)
'*******************************************
' Writes a label to the Postscript buffer using octal codes
  Dim I As Integer, J As Integer
  Dim CODE As String * 4
  Dim IOCTAL As Integer, IOCTAL1 As Integer, IOCTAL2 As Integer, IOCTAL3 As Integer


  Call PSBUFW(" (", 2)
  For I = 1 To IC
    J = Asc(Mid(LABEL, I, 1))
    IOCTAL1 = (J + 128) / 64
    IOCTAL = (J + 128) - IOCTAL1 * 64
    IOCTAL2 = IOCTAL / 8
    IOCTAL3 = IOCTAL - IOCTAL2 * 8
    IOCTAL = 100 * IOCTAL1 + 10 * IOCTAL2 + IOCTAL3
    CODE = "\" + CStr(IOCTAL)
    Call PSBUFW(CODE, 4)
  Next I
  Call PSBUFW(")", 1)
  Call PSBUFW(" show", 5)
End Sub

Sub PSSETFONT(N As Integer)
'**************************
' Selects a font
  ICHARSET = 1
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

Sub PSLBSZ(SIZE As Integer)
'**************************
' THIS SUB SETS THE LABEL SIZE.

  Dim FontSize As Integer
'Scale by a factor of 2 because of the 0.47 scale in the PS file
  FontSize = CInt(2 * SIZE)
  Call PSBUFW(" /" + FONT + " findfont ", 23)
  Call PSBUFN(FontSize)
  Call PSBUFW(" scalefont setfont", 18)
End Sub


Sub PSLNTP(ITP As Integer)
'*************************
'  SETS UP THE LINE TYPE FOR PostScript.
'  Matches the HP7475 line types.
'  Stroke the current path before changing to the new line type.
      Call PSBUFW(" stroke", 8)
      If (ITP < 2 Or ITP > 10) Then Call PSBUFW(" []", 3)
      If (ITP = 2) Then Call PSBUFW(" [1 15]", 7)
      If (ITP = 3) Then Call PSBUFW(" [1 8]", 7)
      If (ITP = 4) Then Call PSBUFW(" [16 5 3 5]", 11)
      If (ITP = 5) Then Call PSBUFW(" [18 7]", 7)
      If (ITP = 6) Then Call PSBUFW(" [13 12]", 8)
      If (ITP = 8) Then Call PSBUFW(" [1 4]", 6)
      If (ITP = 7) Then Call PSBUFW(" [18 5 1 5]", 11)
      If (ITP = 9) Then Call PSBUFW(" [11 5 3 3 3 5]", 15)
      If (ITP = 10) Then Call PSBUFW(" [6 5 3 3 3 5 3 5]", 18)
      Call PSBUFW(" 0 setdash", 10)
      End Sub

 Sub PSLNWD(W As Single)
'***********************
'  SETS THE LINE WIDTH
   Call PSBUFW(" stroke", 7)
   Call PSBUFR(W)
   Call PSBUFW(" setlinewidth", 13)
 End Sub

Sub PSWIPE(I1 As Long, I2 As Long)
'*********************************
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

Sub PSOSEG(I As Integer)
'***********************
' OPENS SEGMENT I IN HARD COPY BUFFER.
  ISEGST(I) = PSPOIN
End Sub

Sub PSCSEG()
'*********
' CLOSES THE CURRENT SEGMENT IN THE HARD COPY BUFFER.
  Call MGQSEG(ICURR)
  If ICURR > 0 Then ISEGEN(ICURR) = PSPOIN - 1
End Sub

Sub PSDSEG(I As Integer)
'***********************
' DELETES SEGMENT I IN THE HARD COPY BUFFER.
  Dim IDIFF2 As Integer, J As Integer
  ' WIPE IT FROM THE POSTSCRIPT BUFFER
  Call PSWIPE(ISEGST(I), ISEGEN(I))
  IDIFF2 = ISEGEN(I) - ISEGST(I) + 1
  Call MGQSGM(MSEG)
  For J = 1 To MSEG
    If (MGQSGX(J)) Then
      If ISEGST(J) > ISEGST(I) Then ISEGST(J) = ISEGST(J) - IDIFF2
      If ISEGEN(J) > ISEGEN(I) Then ISEGEN(J) = ISEGEN(J) - IDIFF2
    End If
  Next J
End Sub

Sub MGOSEG(I As Integer)
'***********************
' Opens segment number I
  If ICURR > 0 Then Call MGCSEG
  ICURR = I
  If I > MSEG Then MSEG = I
  If VBMetgraph.PSFlagOn Then Call PSOSEG(I)
End Sub

Sub MGCSEG()
'***********
' Closes the current segment.
  If VBMetgraph.PSFlagOn Then Call PSCSEG
  ICURR = 0
End Sub

Sub MGQSEG(I As Integer)
'***********************
' QUERY THE CURRENT SEGMENT NUMBER.
  I = ICURR
End Sub

Sub MGONSG()
'*********
' OPEN THE NEXT SEGMENT IN SEQUENCE.
  Call MGOSEG(MSEG + 1)
End Sub

Sub MGQSGM(I As Integer)
'***********************
' QUERY THE HIGHEST NUMBER SEGMENT THAT HAS BEEN USED.
  I = MSEG
End Sub
'
Function MGQSGX(I As Integer) As Boolean
'***************************************
' QUERY SEGMENT I EXISTS.
  MGQSGX = True
End Function

Sub MGDSEG(I As Integer)
'***********************
' DELETE SEGMENT I.
  Call MGQSEG(ICURR)
  If ICURR = I Then Call MGCSEG
  If VBMetgraph.PSFlagOn Then Call PSDSEG(I)
End Sub

Sub MGESEG(I As Integer)
'***********************
' ERASES SEGMENT I.
  Call MGDSEG(I)
End Sub
