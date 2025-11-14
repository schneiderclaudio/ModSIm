Attribute VB_Name = "MdlPlotsubs"
Option Explicit
'*************************** PLOTSUBS *********************************C
'                                                                      C
'            METGRAPH   INTERACTIVE GRAPHICS PACKAGE                   C
'                                                                      C
'     THIS FILE CONTAINS ALL THE GENERAL PLOTTING SUBROUTINES          C
'     This is the Visual Basic Version   1997                          C
'                                                                      C
'     METGRAPH COPYRIGHT (C) 1985-1997  RP KING                        C
'                                                                      C
'**********************************************************************C
'
Public MESSAGE As String, response  As Integer
Private IXORG As Integer, IYORG As Integer, ISPAN As Integer, IRANGE As Integer
Private Const NMAX As Integer = 151

 Sub MGMOVE(IX As Integer, IY As Integer)
'****************************************
'  PERFORMS A PEN-UP MOVE TO SCREEN COORDINATES IX,IY
Dim X As Single, Y As Single
'
 X = IX
 Y = IY
 Call MGRMVE(X, Y)
 End Sub
'
 Sub MGPLOT(X As Single, Y As Single)
'**************************************
 Call RPLOT(X, Y)
 End Sub
'
 Sub PLOT(IX As Integer, IY As Integer)
'**************************************
'  LOW RESOLUTION PLOT PRIMITIVE.
Dim X As Single, Y As Single
'
 X = IX
 Y = IY
 Call RPLOT(X, Y)
 End Sub

 Sub LABCON(LOGLAB() As String, NL As Integer, DECLAB() As Integer, ND As Integer)
'*******************************************************************************
'  CONVERTS A LABEL FROM CHARACTER (OR CHARACTER*1) ARRAY TO INTEGER ARR
  Dim I As Integer
 For I = 1 To NL
   If (LOGLAB(I) <> " ") Then ND = I
 Next I
 For I = 1 To ND
   DECLAB(I) = ICHAR(LOGLAB(I))
 Next I
 End Sub

 Sub LBSIZE(SPACE As Integer, Line As Integer)
'*********************************************
'  THIS SUB SETS THE LABEL SIZE.
'  SPACE IS THE LETTER SIZE AND LINE THE DISTANCE BETWEEN LINES.
'
 Call TXSIZE(SPACE, Line)
 End Sub
'
 Sub AXNUMB(IX As Integer, IY As Integer, IPOS As Integer, ORIEN As Integer, Val As Single, Form As Single, LBS As Integer)
'***********************************************
Dim iw As Integer
'  SETS THE NUMBER ALONG THE AXIS.
 iw = Fix(Abs(Form)) * LBS
 If (Form <= 0#) Then iw = iw + 3 * LBS
 If (ORIEN = 1) Then Call MGMOVE(IX + IPOS - iw / 2, IY - 2 * LBS)
 If (ORIEN = 2) Then Call MGMOVE(IX - iw - 5, IY + IPOS - LBS / 2)
 If (ORIEN = 3) Then Call MGMOVE(IX + IPOS - iw / 2, IY + 2 * LBS)
 If (ORIEN = 4) Then Call MGMOVE(IX + 5, IY + IPOS - LBS / 2)
 If (Form > 0) Then
   Call NUMLBR(Val, Form)
 Else
   Call NUMLBR_EXP(Val, Form)
 End If
 End Sub
'
 Sub NUMLBR(X As Single, F As Single)
'************************************
'   THIS SUB DRAWS THE REAL NUMBER AS A LABEL USING FORMAT F.
 Dim IDENT(20) As Integer, ICH As Integer
 Call FORMTR(X, F, IDENT, ICH)
 Call Label(IDENT, ICH)
 End Sub
'
 Sub NUMLBR_EXP(X As Single, e As Single)
'****************************************
'   THIS SUB DRAWS THE REAL NUMBER AS A LABEL USING EXPONENTIAL FORMAT E.
' E must be NONPOSITIVE to signal that exponential format is required.
 Dim WORD As String
 Dim F As Single, J As Integer, IEXP As Integer
 If (e > 0#) Then Exit Sub
 
 F = -e
 If (X < 0#) Then
   X = -X
   WORD = "-"
 End If
 J = Fix(F * 10 - Fix(F) * 10 + 0.001)
 If (J = 0) Then
   IEXP = CInt(0.4343 * Log(X))
   WORD = WORD & "10~" & Format(IEXP, "##0") & "~"
   Call LABELC(WORD, Len(WORD))
 End If
 End Sub
'
'
 Sub FORMTR(XX As Single, F As Single, IDENT() As Integer, ICHR As Integer)
'**************************************************************************
'   THIS SUB FORMATS REAL VARIABLE X ACCORDING TO  F  FORMAT
'   AND RETURNS THE ASCII EQUIVALENT IN  IDENT WHICH HAS ICHR CHARACTER
 Dim II As Long, IDIG As Integer, NUM(20) As Integer
 Dim X As Single
 Dim I As Integer, J As Integer, JJ As Integer
 Dim Xfract As Single
 ICHR = 0
 X = XX
 If (X < 0#) Then
   X = -X
   IDENT(1) = TABLE(45)
   ICHR = 1
 End If
 J = Fix(F * 10 - Fix(F) * 10 + 0.001)
 X = Fix(X * (10 ^ J) + 0.5)
 X = X / (10 ^ J)
'
'  INTEGRAL PART
'  *************
 II = Fix(X)
 Call FORMT(II, IDIG, NUM)
 For I = 1 To IDIG
   ICHR = ICHR + 1
   IDENT(ICHR) = NUM(I)
 Next I
 If (J = 0) Then GoTo Lbl41
'
'DECIMAL INDICATOR
'*****************
 ICHR = ICHR + 1
 IDENT(ICHR) = TABLE(46)
'
'FRACTIONAL PART
'***************
 Xfract = X - Fix(X)
 II = Fix(Xfract * (10 ^ J) + 0.5)
 Call FORMT(II, IDIG, NUM)
 If (IDIG >= J) Then GoTo Lbl45
 II = J - IDIG
 For I = 1 To II
   ICHR = ICHR + 1
   IDENT(ICHR) = TABLE(48)
 Next I
Lbl45:
 For I = 1 To IDIG
   ICHR = ICHR + 1
   IDENT(ICHR) = NUM(I)
 Next I
'
'  RIGHT JUSTIFY
'  *************
Lbl41:
J = Fix(F)
If (ICHR >= J) Then Exit Sub
For I = 1 To J
  JJ = J - I + 1
  II = ICHR - I + 1
  If (II >= 1) Then IDENT(JJ) = IDENT(II)
  If (II < 1) Then IDENT(JJ) = TABLE(32)
Next I
ICHR = J
End Sub
'
 Sub FORMT(I As Long, IDIG As Integer, NUM() As Integer)
'**********************************************************
'  THIS SUB FORMATS THE INTEGER I AND RETURNS
'  THE ASCII EQUIVALENT IN NUM WHICH HAS IDIG CHARACTERS
 
Dim K As Integer, KK As Integer, II As Long, N As Integer, ZI As Single
Dim J As Long
If (I = 0) Then IDIG = 1
 II = I
 If (I < 0) Then II = -I
 ZI = II
 If (II > 0) Then IDIG = Fix(ALOG10(ZI) + 0.00001) + 1
 For K = 1 To IDIG
   J = 10 ^ (IDIG - K)
   N = II \ J
   NUM(K) = TABLE(48 + N)
   II = II - N * J
 Next K
 If (I >= 0) Then Exit Sub
 IDIG = IDIG + 1
 For K = 2 To IDIG
   KK = IDIG + 1 - K
   NUM(KK + 1) = NUM(KK)
 Next K
 NUM(1) = TABLE(45)
 End Sub
'
'
 Sub FORMTC(INUM As Integer, IDIG As Integer, NUMC As String)
'**********************************************************
'
'  FORMATS THE INTEGER ID AND RETURNS THE ENCODED STRING IN NUMC WHICH
'  HAS IDIG CHARACTERS.
 Dim NUM(10) As Integer, I As Integer
 Dim CHAR As String * 1
' Call FORMT(INUM, IDIG, NUM)
 For I = 1 To IDIG
'   NUMC(I:I)=CHAR(NUM(I))
 Next I
 End Sub
 
 Function FindFormat(X As Single) As Single
 '*****************************************
 'Finds a suitable format for the number X to use with sub LINAX
 Dim A As Single
 
 A = Int(ALOG10(X))
 If A < 0 Then
   FindFormat = -A * 1.1 + 3
 Else
   FindFormat = A * 1.1 + 3
 End If
 End Function

Sub LABELC(STRNG As String, N As Integer)
'*****************************************
'  Converts a label string to integer array.
 Dim LAB(255) As Integer, I As Integer
 For I = 1 To N
      LAB(I) = ICHAR(Mid(STRNG, I, 1))
 Next I
 Call Label(LAB, N)
End Sub

Sub LEGENDBOX(SX As Single, SY As Single, width As Integer, height As Integer, ITYPE As Integer)
  '***********************************************************************************************
  Dim Vert(4) As xyrcoord
  Vert(1).xcoord = SX
  Vert(1).ycoord = SY
  Vert(2).xcoord = SX
  Vert(2).ycoord = SY - height
  Vert(3).xcoord = SX + width
  Vert(3).ycoord = SY - height
  Vert(4).xcoord = SX + width
  Vert(4).ycoord = SY
  Call MGPOLY(ITYPE, Vert, 4)
End Sub

Sub LEGENDTEXT(SX As Single, SY As Single, ISymbol As Integer, text As String, IPOS As Integer, LineSpace As Integer)
'**********************************************************************************************************************************
  Dim IX As Integer, IY As Integer
  IX = SX
  IY = SY
  Call SYMBOL(IX + 10, IY - (IPOS - 1 / 3) * LineSpace, ISymbol)
  Call MGMOVE(IX + 30, IY - IPOS * LineSpace)
  Call MGLABEL(text)
End Sub

 Sub LINID(IX As Integer, IY As Integer, LENGT As Integer, Lable() As Integer, N As Integer)
'*******************************************************************************************
'  THIS SUB PROVIDES A LINE IDENTIFICATION.
'  IT DRAWS A LINE OF LENGTH LENGT , STARTING AT (IX,IY) AND
'  IDENTIFIES IT WITH LABLE HAVING N CHARACTERS.
'  THE CURRENT PEN AND LINE TYPE ARE USED.
 Call MGMOVE(IX, IY)
 Call PLOT(IX + LENGT, IY)
 'Draw the lable with a solid line
 Call LNTYPE(0)
 Call MGMOVE(IX + LENGT + 5, IY)
 Call Label(Lable, N)
 End Sub

Sub LINIDC(IX As Integer, IY As Integer, LENGT As Integer, STRNG As String, N As Integer)
'*****************************************************************************************
'  PROVIDES AN IDENTIFIER FOR A PLOTTED LINE. A LINE OF LENGTH LENGT IS
'  DRAWN, STARTING AT (IX,IY) AND IT IS IDENTIFIED WITH A LABEL STRNG N
'  CHARACTERS LONG.

 Dim I As Integer
 Dim IDENT(80) As Integer
 For I = 1 To N
   IDENT(I) = ICHAR(Mid(STRNG, I, 1))
 Next I
 Call LINID(IX, IY, LENGT, IDENT, N)
End Sub

Sub MGRARRO(XM As Single, YM As Single, X As Single, Y As Single)
'****************************************************************
  ' DRAWS AN ARROW FROM (XM,YM) TO (X,Y).
  Dim X1 As Single, Y1 As Single
  If (XM = X And YM = Y) Then
    Exit Sub
  Else
    Call RARROW(XM, YM, X, Y, X1, Y1)
    Call MGRMVE(XM, YM)
    Call RPLOT(X1, Y1)
  End If
End Sub

Sub RARROW(XM As Single, YM As Single, X As Single, Y As Single, X1 As Single, Y1 As Single)
'*************************************************************************************
  ' Draws an arrowhead in real precision.
  ' Arrowhead is scaled to the length of the arrow.
  ' D1 is the relative head length and D2 is the relative head width.
  Dim d As Single, D1 As Single, D2 As Single
  Dim WX As Single, WY As Single
  Dim XB As Single, YB As Single
  Dim XT As Single, YT As Single
  If (XM = X And YM = Y) Then
    Exit Sub
  Else
    d = Sqr((XM - X) ^ 2 + (YM - Y) ^ 2)
    D1 = 10#
    D2 = 3.3
    WX = (XM - X) / d
    WY = (YM - Y) / d
    X1 = WX * D1 + X
    Y1 = WY * D1 + Y
    XB = X1 - WY * D2
    YB = Y1 + WX * D2
    XT = X1 + WY * D2
    YT = Y1 - WX * D2
    Call MGRMVE(XT, YT)
    Call RPLOT(X, Y)
    Call RPLOT(XB, YB)
    Call RPLOT(XT, YT)
  End If
End Sub

Sub GRAPHARROW(SX As Single, SY As Single, LR As String, TB As String, Size As Single)
'*************************************************************************************
  Dim X As Single, Y As Single
  Call MGRMVE(SX, SY)
  If LR = "L" Then
    X = SX - Size
  Else
    X = SX + Size
  End If
  Call MGRARRO(SX, SY, X, SY)
  If TB = "B" Then
    Y = SY - Size
  Else
    Y = SY + Size
  End If
  Call MGRARRO(SX, SY, SX, Y)
End Sub

Sub GRPHPL(XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer, X() As Single, Y() As Single, N As Integer, XO As Single, YO As Single, XF As Single, YF As Single)
'******************************************************
'This SUB plots a graph of array y against array x
 Dim XX As Single, YY As Single
 Dim I As Integer
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 For I = 1 To N
   XX = ((X(I) - XO) / (XF - XO)) * SPAN + XORG
   YY = ((Y(I) - YO) / (YF - YO)) * RANGE + YORG
   If (I = 1) Then Call MGRMVE(XX, YY)
   If (I > 1) Then Call RPLOT(XX, YY)
 Next I
 Call PSSTR
 End Sub
'
'
 Sub SPLHPL(XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer, X() As Single, Y() As Single, N As Integer, XO As Single, YO As Single, XF As Single, YF As Single, NS As Integer)
'**********************************************************************************************************************************************************************************************
'  PLOTS A SMOOTH GRAPH OF Y AGAINST X.
'
 Dim Y2(NMAX) As Single, YP1 As Single, YP2 As Single
 Dim XM As Single, YM As Single, YPM As Single, YP As Single
 Dim XS As Single, Ys As Single
 Dim IX As Integer, IY As Integer
 Dim I As Integer, J As Integer
 Dim Del As Single
 Call REGCOR(XORG, YORG, SPAN, RANGE)
 If (N > NMAX) Then
   MESSAGE = "Subroutine SPLHPL is limited to " + Format(NMAX, "####") + " Points"
   response = MsgBox(MESSAGE, 0, "ERROR")
   Call MGCLOSE
 End If
 Call MGSPLN(X, Y, N, YP1, YP2, Y2)
 XM = GRPHPT(X(1), XO, XF, SPAN, XORG)
 YM = GRPHPT(Y(1), YO, YF, RANGE, YORG)
 Call MGSPLI(X, Y, Y2, N, X(1), Y(1), YPM)
 YPM = GRPHSL(YPM, XO, YO, XF, YF, SPAN, RANGE)
 IX = XM
 IY = YM
 Call MGMOVE(IX, IY)
 For I = 1 To N - 1
   Del = 0.9999 * (X(I + 1) - X(I)) / CSng(NS + 1)
   For J = 1 To NS + 1
     XS = X(I) + Del * J
     Call MGSPLI(X, Y, Y2, N, XS, Ys, YP)
     XS = GRPHPT(XS, XO, XF, SPAN, XORG)
     Ys = GRPHPT(Ys, YO, YF, RANGE, YORG)
     YP = GRPHSL(YP, XO, YO, XF, YF, SPAN, RANGE)
     Call MGCURV(XS, Ys, YP, XM, YM, YPM)
     XM = XS
     YM = Ys
     YPM = YP
   Next J
 Next I
 Call PSSTR

 End Sub
'
 Sub EXPDIS(XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer, X() As Single, Y() As Single, N As Integer, XO As Single, YO As Single, XF As Single, YF As Single, ISYM As Integer)
'**********************************************************************
 Dim IX As Integer, IY As Integer
 Dim I As Integer
 Call LNTYPE(0)
 For I = 1 To N
   IX = CInt(((X(I) - XO) / (XF - XO)) * SPAN) + XORG
   IY = CInt(((Y(I) - YO) / (YF - YO)) * RANGE) + YORG
   Call SYMBOL(IX, IY, ISYM)
 Next I
 Call PSSTR
 End Sub
 
 Sub EXTRAC(X1() As Single, Y1() As Single, X2() As Single, Y2() As Single, N1 As Integer, Interval As Integer, N2 As Integer)
'*****************************************************************************************************************************
'This SUB extracts points at interval int from arrays x1 & y
'to form new arrays x2 & y2. n1= number of points in input arrays. n2 =
'number of points in output arrays.
 Dim N As Integer, J As Integer
 N2 = N1 / Interval
 For N = 1 To N2
    J = (N - 1) * Interval + 1
    X2(N) = X1(J)
    Y2(N) = Y1(J)
 Next N
'
'  KEEP THE SAME TERMINAL VALUES.
 If J < N1 Then
   N2 = N2 + 1
   X2(N2) = X1(N1)
   Y2(N2) = Y1(N1)
 End If
 End Sub
 
 Sub SYMSIZ(SymbolSize As Integer)
'*********************************
'  SETS THE SIZE OF THE SYMBOLS.
   Isize = SymbolSize
 End Sub
'
'
 Sub SYMBOL(IX As Integer, IY As Integer, N As Integer)
'******************************************************
'  PLOTS SYMBOL NUMBER N AT LOCATION IX,IY.
Dim Verts(9) As xyrcoord
Dim LocalSize As Integer
Dim IR As Integer, NN As Integer
 LocalSize = Isize
 IR = LocalSize / Sqr(2#)
 If (N < 1) Then N = -N
 NN = (N - 1 Mod 29) + 1
 Select Case NN
   Case 1, 21
     Verts(1).xcoord = IX - LocalSize
     Verts(1).ycoord = IY + LocalSize
     Verts(2).xcoord = IX + LocalSize
     Verts(2).ycoord = IY + LocalSize
     Verts(3).xcoord = IX + LocalSize
     Verts(3).ycoord = IY - LocalSize
     Verts(4).xcoord = IX - LocalSize
     Verts(4).ycoord = IY - LocalSize
     If (NN = 1) Then Call MGPOLY(3, Verts, 4)
     If (NN = 21) Then Call MGPOLY(2, Verts, 4)
   Case 2, 22
     Verts(1).xcoord = IX
     Verts(1).ycoord = IY + LocalSize
     Verts(2).xcoord = IX - LocalSize / 2
     Verts(2).ycoord = IY + LocalSize
     Verts(3).xcoord = IX - LocalSize
     Verts(3).ycoord = IY + LocalSize / 2
     Verts(4).xcoord = IX - LocalSize
     Verts(4).ycoord = IY - LocalSize / 2
     Verts(5).xcoord = IX - LocalSize / 2
     Verts(5).ycoord = IY - LocalSize
     Verts(6).xcoord = IX + LocalSize / 2
     Verts(6).ycoord = IY - LocalSize
     Verts(7).xcoord = IX + LocalSize
     Verts(7).ycoord = IY - LocalSize / 2
     Verts(8).xcoord = IX + LocalSize
     Verts(8).ycoord = IY + LocalSize / 2
     Verts(9).xcoord = IX + LocalSize / 2
     Verts(9).ycoord = IY + LocalSize
     If (NN = 2) Then Call MGPOLY(3, Verts, 9)
     If (NN = 22) Then Call MGPOLY(2, Verts, 9)
   Case 3, 23
     Verts(1).xcoord = IX
     Verts(1).ycoord = IY + LocalSize
     Verts(2).xcoord = IX - LocalSize
     Verts(2).ycoord = IY - LocalSize
     Verts(3).xcoord = IX + LocalSize
     Verts(3).ycoord = IY - LocalSize
     If (NN = 3) Then Call MGPOLY(3, Verts, 3)
     If (NN = 23) Then Call MGPOLY(2, Verts, 3)
  Case 4, 24
     Verts(1).xcoord = IX + LocalSize
     Verts(1).ycoord = IY + LocalSize
     Verts(2).xcoord = IX - LocalSize
     Verts(2).ycoord = IY + LocalSize
     Verts(3).xcoord = IX
     Verts(3).ycoord = IY - LocalSize
     If (NN = 4) Then Call MGPOLY(3, Verts, 3)
     If (NN = 24) Then Call MGPOLY(2, Verts, 3)
   Case 5, 25
     Verts(1).xcoord = IX - LocalSize
     Verts(1).ycoord = IY
     Verts(2).xcoord = IX
     Verts(2).ycoord = IY - LocalSize
     Verts(3).xcoord = IX + LocalSize
     Verts(3).ycoord = IY
     Verts(4).xcoord = IX
     Verts(4).ycoord = IY + LocalSize
     If (NN = 5) Then Call MGPOLY(3, Verts, 4)
     If NN = 25 Then Call MGPOLY(2, Verts, 4)
   Case 6, 7, 26, 27
     If NN = 6 Or NN = 26 Then LocalSize = -LocalSize
     Verts(1).xcoord = IX + LocalSize
     Verts(1).ycoord = IY - LocalSize
     Verts(2).xcoord = IX + LocalSize
     Verts(2).ycoord = IY + LocalSize
     Verts(3).xcoord = IX - LocalSize
     Verts(3).ycoord = IY
     If (NN = 6 Or NN = 7) Then Call MGPOLY(3, Verts, 3)
     If (NN = 26 Or NN = 27) Then Call MGPOLY(2, Verts, 3)
   Case 8, 28
       Verts(1).xcoord = IX - LocalSize
       Verts(1).ycoord = IY + LocalSize
       Verts(2).xcoord = IX + LocalSize
       Verts(2).ycoord = IY + LocalSize
       Verts(3).xcoord = IX - LocalSize
       Verts(3).ycoord = IY - LocalSize
       Verts(4).xcoord = IX + LocalSize
       Verts(4).ycoord = IY - LocalSize
       If (NN = 8) Then Call MGPOLY(3, Verts, 4)
       If (NN = 28) Then Call MGPOLY(2, Verts, 4)
    Case 9, 29
       Verts(1).xcoord = IX - LocalSize
       Verts(1).ycoord = IY + LocalSize
       Verts(2).xcoord = IX + LocalSize
       Verts(2).ycoord = IY - LocalSize
       Verts(3).xcoord = IX + LocalSize
       Verts(3).ycoord = IY + LocalSize
       Verts(4).xcoord = IX - LocalSize
       Verts(4).ycoord = IY - LocalSize
       If (NN = 9) Then Call MGPOLY(3, Verts, 4)
       If (NN = 29) Then Call MGPOLY(2, Verts, 4)
    Case 10
       Call CROSS(IX, IY, LocalSize)
    Case 11
       Call MGMOVE(IX + LocalSize, IY + LocalSize)
       Call PLOT(IX - LocalSize, IY - LocalSize)
       Call MGMOVE(IX + LocalSize, IY - LocalSize)
       Call PLOT(IX - LocalSize, IY + LocalSize)
    Case 12
       Call MGMOVE(IX, IY)
       Call PLOT(IX, IY + LocalSize)
       Call MGMOVE(IX + LocalSize, IY - LocalSize)
       Call PLOT(IX, IY)
       Call PLOT(IX - LocalSize, IY - LocalSize)
    Case 13
       Call MGMOVE(IX, IY)
       Call PLOT(IX + LocalSize, IY)
       Call MGMOVE(IX - LocalSize, IY + LocalSize)
       Call PLOT(IX, IY)
       Call PLOT(IX - LocalSize, IY - LocalSize)
    Case 14
       Call MGMOVE(IX, IY)
       Call PLOT(IX - LocalSize, IY)
       Call MGMOVE(IX + LocalSize, IY + LocalSize)
       Call PLOT(IX, IY)
       Call PLOT(IX + LocalSize, IY - LocalSize)
    Case 15
       Call MGMOVE(IX, IY)
       Call PLOT(IX, IY - LocalSize)
       Call MGMOVE(IX - LocalSize, IY + LocalSize)
       Call PLOT(IX, IY)
       Call PLOT(IX + LocalSize, IY + LocalSize)
    Case 16
       Call MGMOVE(IX - 3, IY + LocalSize)
       Call PLOT(IX + 3, IY + LocalSize)
       Call MGMOVE(IX, IY + LocalSize)
       Call PLOT(IX, IY - LocalSize)
       Call PLOT(IX, IY + LocalSize)
       Call MGMOVE(IX - 3, IY - LocalSize)
       Call PLOT(IX + 3, IY - LocalSize)
       Call MGMOVE(IX + 1, IY)
       Call PLOT(IX - 1, IY)
    Case 17
       Call MGMOVE(IX - LocalSize, IY + 3)
       Call PLOT(IX - LocalSize, IY - 3)
       Call MGMOVE(IX - LocalSize, IY)
       Call PLOT(IX + LocalSize, IY)
       Call MGMOVE(IX + LocalSize, IY - 3)
       Call PLOT(IX + LocalSize, IY + 3)
       Call MGMOVE(IX, IY + 1)
       Call PLOT(IX, IY - 1)
    Case 18
       Call CROSS(IX, IY, LocalSize)
       Call MGMOVE(IX + IR, IY + IR)
       Call PLOT(IX - IR, IY - IR)
       Call MGMOVE(IX + IR, IY - IR)
       Call PLOT(IX - IR, IY + IR)
    Case 19
       Call MGMOVE(IX - LocalSize, IY + LocalSize - 4)
       Call PLOT(IX - LocalSize, IY + LocalSize)
       Call PLOT(IX - LocalSize + 4, IY + LocalSize)
       Call MGMOVE(IX - LocalSize, IY + LocalSize)
       Call PLOT(IX + LocalSize, IY - LocalSize)
       Call MGMOVE(IX + LocalSize, IY - LocalSize + 4)
       Call PLOT(IX + LocalSize, IY - LocalSize)
       Call PLOT(IX + LocalSize - 4, IY - LocalSize)
       Call MGMOVE(IX + 1, IY + 1)
       Call PLOT(IX - 1, IY - 1)
    Case 20
       Call MGMOVE(IX + LocalSize, IY + LocalSize - 4)
       Call PLOT(IX + LocalSize, IY + LocalSize)
       Call PLOT(IX + LocalSize - 4, IY + LocalSize)
       Call MGMOVE(IX + LocalSize, IY + LocalSize)
       Call PLOT(IX - LocalSize, IY - LocalSize)
       Call MGMOVE(IX - LocalSize, IY - LocalSize + 4)
       Call PLOT(IX - LocalSize, IY - LocalSize)
       Call PLOT(IX - LocalSize + 4, IY - LocalSize)
       Call MGMOVE(IX - 1, IY + 1)
       Call PLOT(IX + 1, IY - 1)
    End Select
    End Sub
'
 Sub CROSS(IX As Integer, IY As Integer, N As Integer)
'*****************************************************
'This draws a cross with centre at (IX,IY) and armlength of N
 Call MGMOVE(IX + N, IY)
 Call PLOT(IX - N, IY)
 Call MGMOVE(IX, IY + N)
 Call PLOT(IX, IY - N)
 End Sub

Sub SQUARE(IX As Single, IY As Single, N As Single)
'***************************************************
'This draws a square with centre at (IX,IY) and side length of 2N
 Call MGRMVE(IX + N, IY + N)
 Call MGPLOT(IX - N, IY + N)
 Call MGPLOT(IX - N, IY - N)
 Call MGPLOT(IX + N, IY - N)
 Call MGPLOT(IX + N, IY + N)
End Sub

 Sub MGSPLN(X() As Single, Y() As Single, N As Integer, YP1 As Single, YPN As Single, Y2() As Single)
'****************************************************************************************************
' SPLINE FUNCTION FIT. ADAPTED FROM NUMERICAL RECIPES.
' NOTE: MAXIMUM DIMENSION IS NMAX.
' NOTE: NATURAL BOUNDARY CONDITIONS ARE USED.
Dim K As Integer, I As Integer
Dim U(NMAX) As Single
Dim H1 As Single, H2 As Single
Dim Sig As Single, P As Single
Dim WS As Single
Dim QN As Single
Dim UN As Single
 If N > NMAX Then
   MESSAGE = "Spline function is limited to " & Format(NMAX, "###") & " points"
   response = MsgBox(MESSAGE, 0, "ERROR")
   Call MGCLOSE
   Stop
 End If
 Y2(1) = 0#
 U(1) = 0#
' Y2(1)=-0.5
' U(1)=(3.0/(X(2)-X(1)))*((Y(2)-Y(1))/(X(2)-X(1))-YP1)
 For I = 2 To N - 1
   H1 = X(I) - X(I - 1)
   H2 = X(I + 1) - X(I)
   If (H1 * H2 <= 0#) Then
   MESSAGE = "X values for spline function must be a monotonic sequence"
   response = MsgBox(MESSAGE, 0, "ERROR")
   Exit Sub
'    Call MGCLOSE
'    Stop
   End If
   Sig = H1 / (X(I + 1) - X(I - 1))
   P = Sig * Y2(I - 1) + 2#
   Y2(I) = (Sig - 1#) / P
   WS = (Y(I + 1) - Y(I)) / H2 - (Y(I) - Y(I - 1)) / H1
   WS = 6# * WS / (X(I + 1) - X(I - 1)) - Sig * U(I - 1)
   U(I) = WS / P
 Next I
 QN = 0#
 UN = 0#
 Y2(N) = (UN - QN * U(N - 1)) / (QN * Y2(N - 1) + 1#)
 For K = N - 1 To 1 Step -1
   Y2(K) = Y2(K) * Y2(K + 1) + U(K)
 Next K
 End Sub
'
 Sub MGSPLI(XA() As Single, YA() As Single, Y2A() As Single, N As Integer, X As Single, Y As Single, YP As Single)
'*****************************************************************************************************************
' EVALUATES THE SPLINE FUNCTION.

  Dim I As Integer
  Dim A As Single, B As Single, c As Single, d As Single
  Dim KHI As Integer, KLO As Integer
  Dim H As Single
  Dim WS As Single
  For I = 1 To N - 1
    A = XA(I + 1) - X
    B = X - XA(I)
    If (A * B >= 0#) Then GoTo Labl30
  Next I
 MESSAGE = "X IS OUTSIDE THE RANGE OF SPLINE DATA"
 response = MsgBox(MESSAGE, 0, "ERROR")
 Call MGCLOSE
 Exit Sub
Labl30:
 KHI = I + 1
 KLO = I
 H = XA(KHI) - XA(KLO)
 A = A / H
 B = B / H
 c = (A ^ 3 - A)
 d = (B ^ 3 - B)
 Y = A * YA(KLO) + B * YA(KHI) + (c * Y2A(KLO) + d * Y2A(KHI)) * H * H / 6#
 WS = (YA(KHI) - YA(KLO)) / H
 YP = WS - ((3# * A * A - 1#) * Y2A(KLO) - (3# * B * B - 1#) * Y2A(KHI)) * H / 6#
 End Sub
'
'
 Sub MGCURV(X As Single, Y As Single, YP As Single, XM As Single, YM As Single, YPM As Single)
'*********************************************************************************************
'  DRAWS A BEZIER CURVE FROM THE CURRENT POINT (XM,YM) TO THE POINT
'  (X,Y). YP AND YPM ARE THE REQUIRED GRADIENTS AT (X,Y) AND (XM,YM).
'  XC AND YC ARE ARRAYS HOLDING THE COORDINATES OF THE 4 CONTROL POINTS
 Dim XC(4) As Single, YC(4) As Single
 Dim XC1 As Single, YC1 As Single, XC2 As Single, YC2 As Single
 Call MGBEZC(X, Y, YP, XM, YM, YPM, XC1, YC1, XC2, YC2)
 XC(1) = XM
 YC(1) = YM
 XC(2) = XC1
 YC(2) = YC1
 XC(3) = XC2
 YC(3) = YC2
 XC(4) = X
 YC(4) = Y
 Call MGBEZ(XC, YC)
 End Sub
'
'
 Sub MGBEZC(X As Single, Y As Single, YP As Single, XM As Single, YM As Single, YPM As Single, XC1 As Single, YC1 As Single, XC2 As Single, YC2 As Single)
'*********************************************************************************************************************************************************
'  CALCULATES THE COORDINATES OF THE CONTROL POINTS OF THE BEZIER
'  CURVE FROM POINT XM,YM TO X,Y.
'  YPM AND YP ARE THE GRADIENTS AT THE INITIAL AND FINAL POINTS.
Dim Del As Single, DELX As Single, DELY As Single
 Del = 0.33 * Sqr((X - XM) ^ 2 + (Y - YM) ^ 2) / 1.414
 If X >= XM Then
   DELX = Del
 Else
   DELX = -Del
 End If
 If Y >= YM Then
   DELY = Del
 Else
   DELY = -Del
 End If
 If (Abs(YPM) > 1#) Then
   YC1 = YM + DELY
   XC1 = XM + DELY / YPM
 Else
   XC1 = XM + DELX
   YC1 = DELX * YPM + YM
 End If
 If (Abs(YP) > 1#) Then
   YC2 = Y - DELY
   XC2 = X - DELY / YP
 Else
   XC2 = X - DELX
   YC2 = Y - DELX * YP
 End If
 End Sub
'
'
 Sub MGBEZ(X() As Single, Y() As Single)
'***************************************
'  PLOTS A BEZIER CURVE OVER A SINGLE INTERVAL.
'  X & Y ARE VECTORS OF THE X & Y COORDINATES OF THE FOUR CONTROL
'  POINTS.
 Dim Del As Single, T As Single
 Dim I As Integer
 Dim COEFF(4) As Single
 Const NPOINT As Integer = 10
 Dim PX(NPOINT) As Single, PY(NPOINT) As Single
'
 Del = 1# / CSng(NPOINT)
 For I = 1 To NPOINT
   T = I * Del
   Call BZCOEF(X, COEFF)
   PX(I) = CUBIC(COEFF, T)
   Call BZCOEF(Y, COEFF)
   PY(I) = CUBIC(COEFF, T)
   Call TRPLOT(PX(I), PY(I))
 Next I
 Call PSCURV(X(2), Y(2), X(3), Y(3), X(4), Y(4))
' IF(IPTYPE(IZ) = 2 OR IPTYPE(IZ) = 4)THEN
'   CALL HPCURV(PX,PY,NPOINT)
' END IF
 End Sub
'
'
 Function CUBIC(COEFF() As Single, T As Single)
'**********************************************
'  LOCATES A COORDINATE ALONG A CUBIC POLYNOMIAL.
'
 CUBIC = ((COEFF(1) * T + COEFF(2)) * T + COEFF(3)) * T + COEFF(4)
 End Function
'
'
 Sub BZCOEF(CPT() As Single, COEFF() As Single)
'**********************************************
'  COMPUTES THE COEFFICIENTS OF THE PARAMETRIC REPRESENTATION OF
'  THE BEZIER CURVE.
'  CPT IS THE VECTOR OF 4 CONTROL POINT COORDINATES.
'  COEFF IS THE VECTOR OF 4 CALCULATED BEZIER CURVE COEFFIENTS.
'
 COEFF(1) = -1# * CPT(1) + 3# * CPT(2) - 3# * CPT(3) + 1# * CPT(4)
 COEFF(2) = 3# * CPT(1) - 6# * CPT(2) + 3# * CPT(3)
 COEFF(3) = -3# * CPT(1) + 3# * CPT(2)
 COEFF(4) = 1# * CPT(1)
 End Sub
'
'
 Function GRPHPT(X As Single, XO As Single, XF As Single, SPAN As Integer, XORG As Integer)
'******************************************************************************************
'  SCALES A POINT TO SCREEN COORDINATES.
'
 GRPHPT = SPAN * ((X - XO) / (XF - XO)) + XORG
 End Function
'
 Function GRPHSL(YP As Single, XO As Single, YO As Single, XF As Single, YF As Single, SPAN As Integer, RANGE As Integer)
'***********************************************************************************************************************
'  SCALES THE SLOPE OF A CURVE TO SCREEN COORDINATES.
'
 GRPHSL = YP * RANGE * (XF - XO) / (SPAN * (YF - YO))
 End Function
'
 Sub REGCOR(XORG As Integer, YORG As Integer, SPAN As Integer, RANGE As Integer)
'*******************************************************************************
'
' PLOTS CORNERS AT BOTTOM LEFT AND TOP RIGHT TO CONFIRM REGISTRATION
' OF GRAPH IN BOX.
 Call MGMOVE(XORG, YORG + 10)
 Call PLOT(XORG, YORG)
 Call PLOT(XORG + 10, YORG)
 Call MGMOVE(XORG + SPAN - 10, YORG + RANGE)
 Call PLOT(XORG + SPAN, YORG + RANGE)
 Call PLOT(XORG + SPAN, YORG + RANGE - 10)
 End Sub
'
 Sub AXLAB(IX As Integer, IY As Integer, Length As Integer, iw As Integer, LBS As Integer, ORIEN As Integer, LAB As String, LabLen As Integer, LUNIT As String, LN As Integer)
'********************************************************************************************************************************
' SETS THE AXIS LABEL.
 Dim LABB As String
 Dim LOFF As Integer, N As Integer, N1 As Integer
 Dim Angle As Single
 Const SPACE As String = "    "
 LOFF = 0.75 * (LabLen + 4 + LN) * LBS
 If (ORIEN = 1) Then Call MGMOVE(IX + (Length - LOFF) / 2, IY - 4 * LBS)
 If (ORIEN = 2) Then Call MGMOVE(IX - iw - 15, IY + (Length - LOFF) / 2)
 If (ORIEN = 3) Then Call MGMOVE(IX + (Length - LOFF) / 2, IY + 4 * LBS)
 If (ORIEN = 4) Then Call MGMOVE(IX + iw + 15, IY + (Length + LOFF) / 2)
 Angle = (ORIEN - 1) * 90#
 If (ORIEN = 3) Then Angle = 0#
 Call ROTATE(Angle)
 Call LBSIZE(LBS, 2 * LBS)
 Call CONCAT(LAB, LabLen, SPACE, 4, LABB, N)
 Call CONCAT(LABB, N, LUNIT, LN, LABB, N1)
 Call LABELC(LABB, N1)
 Call CANROT
 End Sub

'
 Sub CONCAT(STR1 As String, N1 As Integer, STR2 As String, N2 As Integer, WSTRING As String, N As Integer)
'********************************************************************************************************
'  CONCATENATES TWO STRINGS STR1 AND STR2 OF LENGTH N1, N2 RESPECTIVELY
'  INTO A NEW STRING OF LENGTH N.
 WSTRING = STR1 & STR2
 N = N1 + N2
 End Sub
'
 Sub ROTATE(Angle As Single)
'***************************
 Dim I As Integer
 Call PSROT(Angle)
 I = Angle / 90# + 1.00001
 Call TXTDIR(I)
' CALL HPROTATE(ANGLE)
 End Sub

 Sub CANROT()
'*****************
'  CANCELS ROTATE INSTRUCTION
 Call PSCNRT
 Call TXTDIR(1)
' CALL HPCANROT
 End Sub
'
Sub RESET()
'****************
'  RESETS THE HARDCOPY BUFFERS.
' CALL HRESET
'Call PRESET
End Sub
'
 Public Function ALOG10(X As Single) As Double
 '********************************************
 ' Calculates log to the base 10
   ALOG10 = Log(X) / Log(10#)
 End Function
'
 Function ULIM(XMAX As Single)
'*****************************
'  ESTABLISHES A SUITABLE UPPER LIMIT FOR PLOTTING.
 Dim IP As Integer, WS As Single
 WS = ALOG10(XMAX) + 0.00001
 If (WS < 0#) Then WS = WS - 1
 IP = Fix(WS)
 WS = XMAX * 10# ^ (-IP)
' The first test has been known to fail when WS is exactly = 2.0
 If (WS <= 2#) Then
   ULIM = 2# * 10# ^ IP
 ElseIf (WS <= 5#) Then
   ULIM = 5# * 10# ^ IP
 Else
   ULIM = 10# ^ (IP + 1)
 End If
 End Function
 
 Function ULIMNarrow(XMAX As Single)
'***********************************
'  ESTABLISHES A SUITABLE UPPER LIMIT FOR PLOTTING.
 Dim IP As Integer, WS As Single
 WS = ALOG10(XMAX) + 0.00001
 If (WS < 0#) Then WS = WS - 1
 IP = Fix(WS)
 WS = XMAX * 10# ^ (-IP)
' The first test has been known to fail when WS is exactly = 2.0
 If (WS <= 2#) Then
ULIMNarrow = 2# * 10# ^ IP
 ElseIf (WS <= 3#) Then
ULIMNarrow = 3# * 10# ^ IP
 ElseIf (WS <= 4#) Then
ULIMNarrow = 4# * 10# ^ IP
 ElseIf (WS <= 5#) Then
ULIMNarrow = 5# * 10# ^ IP
 ElseIf (WS <= 6#) Then
ULIMNarrow = 6# * 10# ^ IP
 ElseIf (WS <= 7#) Then
ULIMNarrow = 7# * 10# ^ IP
 ElseIf (WS <= 8#) Then
ULIMNarrow = 8# * 10# ^ IP
 ElseIf (WS <= 9#) Then
ULIMNarrow = 9# * 10# ^ IP
 Else
ULIMNarrow = 10# ^ (IP + 1)
 End If
 End Function

 Function BLIM(XMIN As Single)
'*****************************
  '  ESTABLISHES A SUITABLE LOWER LIMIT FOR PLOTTING.
  Dim IP As Integer
  Dim WS As Single
  Dim XX As Single, Limit As Single
  
  On Error GoTo ErrHandler
  If XMIN > 0 Then
    WS = ALOG10(XMIN) + 0.00001
    'If (WS < 0#) Then WS = WS - 1
    IP = Int(WS)
    WS = XMIN / (10# ^ IP)
    If (WS < 2#) Then BLIM = 1# * 10# ^ IP
    If (WS >= 2# And WS < 5#) Then BLIM = 2# * 10# ^ IP
    If (WS >= 5#) Then BLIM = 5# * 10 ^ (IP)
  Else
    BLIM = -ULIM(-XMIN)
  End If
Exit Function

ErrHandler:
   MsgBox "Could not establish a lower limt for the abscissa", vbCritical, "ERROR"
   Exit Function
 End Function
'

 Function BLIMNarrow(XMIN As Single)
'*****************************
  '  ESTABLISHES A SUITABLE LOWER LIMIT FOR PLOTTING
  Dim IP As Integer
  Dim WS As Single
  Dim XX As Single, Limit As Single
  
  On Error GoTo ErrHandler
  If XMIN > 0 Then
    WS = ALOG10(XMIN) + 0.00001
    IP = Int(WS)
    WS = XMIN / (10# ^ IP)
    If WS < 2 Then
      BLIMNarrow = 1# * 10# ^ IP
    ElseIf WS < 3 Then
      BLIMNarrow = 2# * 10# ^ IP
    ElseIf WS < 4 Then
      BLIMNarrow = 3# * 10 ^ (IP)
    ElseIf WS < 5 Then
      BLIMNarrow = 4# * 10 ^ (IP)
    ElseIf WS < 6 Then
      BLIMNarrow = 5# * 10 ^ (IP)
    ElseIf WS < 7 Then
      BLIMNarrow = 6# * 10 ^ (IP)
    ElseIf WS < 8 Then
      BLIMNarrow = 7# * 10 ^ (IP)
    ElseIf WS < 9 Then
      BLIMNarrow = 8# * 10 ^ (IP)
    ElseIf WS < 10 Then
      BLIMNarrow = 9# * 10 ^ (IP)
    End If
  Else
    BLIMNarrow = -ULIMNarrow(-XMIN)
  End If
Exit Function

ErrHandler:
   MsgBox "Could not establish a lower limt for the abscissa", vbCritical, "ERROR"
   Exit Function
 End Function
'
 Function VMIN(X() As Single, N As Integer)
'******************************************
'  FINDS THE SMALLEST ELEMENT IN THE VECTOR X.
Dim I As Integer
 VMIN = X(1)
 For I = 2 To N
   If X(I) < VMIN Then VMIN = X(I)
 Next I
 End Function
'
'
 Function VMAX(X() As Single, N As Integer)
'******************************************
'  FINDS THE LARGEST ELEMENT OF VECTOR X.
Dim I As Integer
 VMAX = X(1)
 For I = 2 To N
   If X(I) > VMAX Then VMAX = X(I)
 Next I
 End Function
'
 Sub LOGLIM(XMIN As Single, XMAX As Single, XO As Single, XF As Single, NDEC As Integer)
'***************************************************************************************
'  ESTABLISHES THE DECADE RANGE FOR LOGARITHMIC PLOTS.
 Dim IP1 As Integer, IP2 As Integer
 Dim WS As Single
 If XMIN <= 0 Then
   MsgBox "Lower limit for log plot must be >= 0", vbCritical, "ERROR"
   Exit Sub
 End If
 WS = 0.4343 * Log(XMIN) + 0.000001
 If (WS < 0#) Then WS = WS - 1
 IP1 = Fix(WS)
 WS = 0.4342945 * Log(XMAX) - 0.000001
 If (WS < 0#) Then WS = WS - 1
 IP2 = Fix(WS) + 1
 NDEC = IP2 - IP1
 XO = 10# ^ IP1
 XF = 10# ^ IP2
 End Sub

 Sub LOGFMT(XO As Single, NDEC As Integer, FMT As Single)
'********************************************************
'  ESTABLISHES AN APPROPRIATE FORMAT FOR PLOTTING ON A LOG AXIS.
 Dim FRAC As Single
 FMT = NDEC + 1
 FRAC = 0.4343 * Log(XO)
 If (XO >= 1#) Then
   FMT = FMT + CInt(FRAC)
 Else
   FMT = FMT - 0.1 * CInt(FRAC) + 2#
 End If
 End Sub
