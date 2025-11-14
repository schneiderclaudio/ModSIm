Attribute VB_Name = "MdlPlant1"
Option Explicit
Private NVERTS As Integer
Private Verts(32) As xyrcoord
Sub DrawConveyorBelt(IX As Single, IY As Single, IX1 As Single, IY1 As Single)
'*****************************************************************************
  Dim FACT As Single, DELY As Single, DELX As Single
  Dim Slope As Single
  FACT = 2 * Abs(IR)
  'Draw the first drum
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX, IY, FACT)
  End If
  'Draw the second drum
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX1, IY1, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX1, IY1, FACT)
  End If
  'Draw the belt
  If IX1 = IX Then
    DELY = 0
  Else
    Slope = (IY1 - IY) / (IX1 - IX)
    DELY = FACT / Sqr(1# + Slope * Slope)
  End If
  DELX = -Slope * DELY
  Call MGRMVE(IX + DELX, IY + DELY)
  Call MGPLOT(IX1 + DELX, IY1 + DELY)
  If UnitNumberFlagOn Then
    Call Number(0.5 * (IX + IX1), 0.5 * (IY + IY1), 0, 2, UnitNumber)
  End If

End Sub

Sub FRECTAN(JX As Single, JY As Single, IX As Single, IY As Single)
'*****************************************************************
  NVERTS = 4
  SetVertex IX, IY - JY, Verts(1)
  SetVertex IX + JX, IY - JY, Verts(2)
  SetVertex IX + JX, IY, Verts(3)
  SetVertex IX, IY, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
End Sub

Sub RECTAN(JX As Single, JY As Single, IX As Single, IY As Single)
'*****************************************************************
  NVERTS = 4
  SetVertex IX, IY - JY, Verts(1)
  SetVertex IX + JX, IY - JY, Verts(2)
  SetVertex IX + JX, IY, Verts(3)
  SetVertex IX, IY, Verts(4)
  Call MGPOLY(1, Verts, NVERTS)
End Sub

Sub DrawJawCrusher(IX As Single, IY As Single)
'*************************************
' Draws the jaw crusher icon
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 4
  SetVertex IX - 2 * IR, IY - 6 * ISS, Verts(1)
  SetVertex IX - IR, IY - 6 * ISS, Verts(2)
  SetVertex IX - 2 * IR, IY + 5 * ISS, Verts(3)
  SetVertex IX - 5 * IR, IY + 5 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  SetVertex IX + IR, IY - 6 * ISS, Verts(1)
  SetVertex IX + 2 * IR, IY - 6 * ISS, Verts(2)
  SetVertex IX + 3 * IR, IY + 5 * ISS, Verts(3)
  SetVertex IX + IR, IY + 5 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX + 4 * IR, IY - 3 * ISS, 1.7 * ISS)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX + 4 * IR, IY - 3 * ISS, 1.7 * ISS)
  End If
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 0, UnitNumber)
End Sub

Sub DrawDoubleDeckScreen(IX As Single, IY As Single)
'***************************************************
'  Draws the icon for a double deck screen.
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 5
  SetVertex IX - 11 * IR, IY + 5 * ISS, Verts(1)
  SetVertex IX - 11 * IR, IY + 2 * ISS, Verts(2)
  SetVertex IX + 6 * IR, IY - 5 * ISS, Verts(3)
  SetVertex IX + 9 * IR, IY - ISS, Verts(4)
  SetVertex IX + 9 * IR, IY + 2 * ISS, Verts(5)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGLNTYPE(8)
  Call MGRMVE(IX + 9 * IR, IY + 2 * ISS)
  Call MGPLOT(IX - 11 * IR, IY + 5 * ISS)
  Call MGRMVE(IX - 11 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 9 * IR, IY - ISS)
  Call MGLNTYPE(1)
  If UnitNumberFlagOn Then Call Number(IX, IY, 0, 3, UnitNumber)
  End Sub

Sub DrawSpiralConcentrator(IX As Single, IY As Single)
'******************************************************
  Dim ISS As Single, I As Single
  ISS = Abs(IR)
  Call FRECTAN(2 * IR, 15 * ISS, IX - IR, IY + 8 * ISS)
  For I = 1 To 5
    Call SPIRAL(IX - IR, IY + 8 * ISS - 2 * I * ISS, ISS, IR)
  Next I
  Call FRECTAN(2 * IR, ISS, IX + 3 * IR, IY - 4 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -2, -5, UnitNumber)
 End Sub

Sub SPIRAL(IX As Single, IY As Single, ISS As Single, IR As Single)
'***************************************************************
'  DRAWS THE SPIRAL ICON
  Call MGRMVE(IX, IY)
  Call MGPLOT(IX - 4 * IR, IY - ISS)
  Call MGPLOT(IX + 6 * IR, IY - 2 * ISS)
  Call MGPLOT(IX + 3 * IR, IY - 2 * ISS)
End Sub

Sub DrawDrewboy(IX As Single, IY As Single)
'******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MGRMVE(IX - 6 * IR, IY + 5 * ISS)
  Call MGPLOT(IX - 5 * IR, IY + 5 * ISS)
  Call MGPLOT(IX - 5 * IR, IY + 4 * ISS)
  Call MGRMVE(IX + 6 * IR, IY + 5 * ISS)
  Call MGPLOT(IX + 5 * IR, IY + 5 * ISS)
  Call MGPLOT(IX + 5 * IR, IY + 3 * ISS)
  Call MGRMVE(IX, IY - 2 * ISS)
  Call MGPLOT(IX + 5 * IR, IY + 3 * ISS)
  
  NVERTS = 8
  SetVertex IX - 5 * IR, IY + 4 * ISS, Verts(1)
  SetVertex IX - 5 * IR, IY - ISS, Verts(2)
  SetVertex IX, IY - 6 * ISS, Verts(3)
  SetVertex IX + 11 * IR, IY + 5 * ISS, Verts(4)
  SetVertex IX + 11 * IR, IY + 7 * ISS, Verts(5)
  SetVertex IX + 9 * IR, IY + 7 * ISS, Verts(6)
  SetVertex IX + 5 * IR, IY + 3 * ISS, Verts(7)
  SetVertex IX + 5 * IR, IY + 4 * ISS, Verts(8)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 2, 1, UnitNumber)
End Sub

Sub DrawFlotationCell(IX As Single, IY As Single)
'************************************************
  Dim K As Integer, JJ As Integer
  Dim ISS As Single
  ISS = Abs(IR)
  'FLOTF = True
  If RPFLAG = 1 And Not MGPage(0).MoveFlag And Not MGPage(0).DeleteFlag Then
    NOCELL(NSEL) = Val(MGPage(0).Nocells)
  End If
  JJ = NOCELL(NSEL)
  Call FRECTAN(10 * IR * JJ, 10 * ISS, IX - 5 * IR, IY + 5 * ISS)
  JJ = NOCELL(NSEL) - 1
  Call MGRMVE(IX - 5 * IR, IY + 5 * ISS)
  Call MGPLOT(IX + JJ * 10 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * JJ * IR + 5 * IR, IY + 5 * ISS)
  For K = 1 To JJ
    Call MGRMVE(IX + (10 * K - 5) * IR, IY + 5 * ISS)
    Call MGPLOT(IX + (10 * K - 5) * IR, IY - 5 * ISS)
  Next K
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawHydrocyclone(IX As Single, IY As Single)
'***********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 6
  SetVertex IX - 3 * ISS, IY + 2 * ISS, Verts(1)
  SetVertex IX - 3 * ISS, IY - 2 * ISS, Verts(2)
  SetVertex IX - ISS, IY - 8 * ISS, Verts(3)
  SetVertex IX, IY - 8 * ISS, Verts(4)
  SetVertex IX + 2 * ISS, IY - 2 * ISS, Verts(5)
  SetVertex IX + 2 * ISS, IY + 2 * ISS, Verts(6)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGVertexMove(Verts(2))
  Call MGVertexPlot(Verts(5))
  
  If UnitNumberFlagOn Then Call Number(IX, IY, 2, 2, UnitNumber)
End Sub

Sub DrawShakingTable(IX As Single, IY As Single)
'***********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(20 * ISS, 7 * ISS, IX - 10 * ISS, IY + 3 * ISS)
  Call MGRMVE(IX - 10 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY + ISS)
  Call MGPLOT(IX - IR, IY + ISS)
  Call MGRMVE(IX - 3 * IR, IY)
  Call MGPLOT(IX + 10 * IR, IY)
  Call MGPLOT(IX + 10 * IR, IY - ISS)
  Call MGPLOT(IX - 5 * IR, IY - ISS)
  Call MGRMVE(IX - 7 * IR, IY - 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY - 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY - 3 * ISS)
  Call MGPLOT(IX - 9 * IR, IY - 3 * ISS)
  Call MGRMVE(IX + 6 * IR, IY + 3 * ISS)
  Call MGPLOT(IX + 6 * IR, IY + 2 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 3, 3, UnitNumber)
End Sub

Sub DrawWetDrumMagSeparator(IX As Single, IY As Single)
'******************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call DrawFilter(IX, IY)
End Sub

Sub DrawFilter(IX As Single, IY As Single)
'*****************************************
  Dim ISS As Single, FACT As Single
  ISS = Abs(IR)
  MGPage(0).FillStyle = 0
  FACT = ISS * 4
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX, IY, FACT)
  End If
  Call MGRMVE(IX - 6 * IR, IY - 4 * ISS)
  Call MGPLOT(IX - 5 * IR, IY - 3 * ISS)
  Call MGPLOT(IX - 5 * IR, IY - 6 * ISS)
  Call MGPLOT(IX + 6 * IR, IY - 6 * ISS)
  Call MGPLOT(IX + 6 * IR, IY - 3 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 3, UnitNumber)
End Sub

Sub DrawGrindingMill(IX As Single, IY As Single)
'***********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MillIcon(IX, IY, ISS, IR, 0#, 0#)
End Sub

Sub DrawScreen(IX As Single, IY As Single)
'*****************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 5
  SetVertex IX - 11 * IR, IY + 4 * ISS, Verts(1)
  SetVertex IX - 11 * IR, IY + 2 * ISS, Verts(2)
  SetVertex IX + 6 * IR, IY - 5 * ISS, Verts(3)
  SetVertex IX + 9 * IR, IY - ISS, Verts(4)
  SetVertex IX + 9 * IR, IY + ISS, Verts(5)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -2, 3, UnitNumber)
End Sub

Sub DrawCompoundScreen(IX As Single, IY As Single)
'*************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 7
  SetVertex IX - 11 * IR, IY + 4 * ISS, Verts(1)
  SetVertex IX - 11 * IR, IY + 1 * ISS, Verts(2)
  SetVertex IX - 4 * IR, IY - 5 * ISS, Verts(3)
  SetVertex IX - 0.5 * IR, IY - 1 * ISS, Verts(4)
  SetVertex IX + 6 * IR, IY - 5 * ISS, Verts(5)
  SetVertex IX + 9 * IR, IY - ISS, Verts(6)
  SetVertex IX + 9 * IR, IY + ISS, Verts(7)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -2, 3, UnitNumber)
End Sub

Sub DrawPuddlePan(IX As Single, IY As Single)
'********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(20 * ISS, 5 * ISS, IX - 10 * ISS, IY + 3 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY - 2 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX - 10 * ISS, IY - 2 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX + 6 * ISS, IY - 2 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX - 6 * ISS, IY - 2 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX + 3 * ISS, IY - 2 * ISS)
  Call MGRMVE(IX, IY + 3 * ISS)
  Call MGPLOT(IX - 3 * ISS, IY - 2 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 3, UnitNumber)
End Sub

Sub DrawStreamSplitter(IX As Single, IY As Single)
'*************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 4
  SetVertex IX + 5 * ISS, IY, Verts(1)
  SetVertex IX, IY - 3 * ISS, Verts(2)
  SetVertex IX - 5 * ISS, IY, Verts(3)
  SetVertex IX, IY + 3 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 2, UnitNumber)
End Sub

Sub DrawPump(IX As Single, IY As Single)
'***************************************
  Dim ISS As Single, FACT As Single
  Dim color As Long, fill As Single
  ISS = Abs(IR)
  FACT = ISS * 2.9
  NVERTS = 4
  SetVertex IX - 2 * ISS, IY - 2 * ISS, Verts(1)
  SetVertex IX - 3 * ISS, IY - 4 * ISS, Verts(2)
  SetVertex IX + 3 * ISS, IY - 4 * ISS, Verts(3)
  SetVertex IX + 2 * ISS, IY - 2 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX, IY, FACT)
  End If
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 2, UnitNumber)
End Sub

Sub DrawReichertCone(IX As Single, IY As Single)
'***********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 24
  SetVertex IX - ISS, IY + 9 * ISS, Verts(1)
  SetVertex IX - 10 * ISS, IY + 7 * ISS, Verts(2)
  SetVertex IX - 10 * ISS, IY + 6 * ISS, Verts(3)
  SetVertex IX - ISS, IY + 4 * ISS, Verts(4)
  SetVertex IX - ISS, IY + 3 * ISS, Verts(5)
  SetVertex IX - 10 * ISS, IY + ISS, Verts(6)
  SetVertex IX - 10 * ISS, IY, Verts(7)
  SetVertex IX - ISS, IY - 2 * ISS, Verts(8)
  SetVertex IX - ISS, IY - 3 * ISS, Verts(9)
  SetVertex IX - 10 * ISS, IY - 5 * ISS, Verts(10)
  SetVertex IX - 10 * ISS, IY - 6 * ISS, Verts(11)
  SetVertex IX - ISS, IY - 8 * ISS, Verts(12)
  SetVertex IX + ISS, IY + 9 * ISS, Verts(24)
  SetVertex IX + 10 * ISS, IY + 7 * ISS, Verts(23)
  SetVertex IX + 10 * ISS, IY + 6 * ISS, Verts(22)
  SetVertex IX + ISS, IY + 4 * ISS, Verts(21)
  SetVertex IX + ISS, IY + 3 * ISS, Verts(20)
  SetVertex IX + 10 * ISS, IY + ISS, Verts(19)
  SetVertex IX + 10 * ISS, IY, Verts(18)
  SetVertex IX + ISS, IY - 2 * ISS, Verts(17)
  SetVertex IX + ISS, IY - 3 * ISS, Verts(16)
  SetVertex IX + 10 * ISS, IY - 5 * ISS, Verts(15)
  SetVertex IX + 10 * ISS, IY - 6 * ISS, Verts(14)
  SetVertex IX + ISS, IY - 8 * ISS, Verts(13)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call FRECTAN(2 * ISS, 20 * ISS, IX - ISS, IY + 10 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawThickener(IX As Single, IY As Single)
'********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MGRMVE(IX - 11 * ISS, IY + 2 * ISS)
  Call MGPLOT(IX - 11 * ISS, IY + ISS)
  Call MGPLOT(IX - 10 * ISS, IY + ISS)
  NVERTS = 6
  SetVertex IX - 10 * ISS, IY + 2 * ISS, Verts(1)
  SetVertex IX + 10 * ISS, IY + 2 * ISS, Verts(2)
  SetVertex IX + 10 * ISS, IY, Verts(3)
  SetVertex IX, IY - 2 * ISS, Verts(4)
  SetVertex IX - 10 * ISS, IY, Verts(5)
  SetVertex IX - 10 * ISS, IY + ISS, Verts(6)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGRMVE(IX - ISS, IY + 3 * ISS)
  Call MGPLOT(IX - ISS, IY + ISS)
  Call MGRMVE(IX + ISS, IY + 3 * ISS)
  Call MGPLOT(IX + ISS, IY + ISS)
  Call MGRMVE(IX + 10 * ISS, IY + ISS)
  Call MGPLOT(IX + 11 * ISS, IY + ISS)
  Call MGPLOT(IX + 11 * ISS, IY + 2 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 2, UnitNumber)
End Sub

Sub DrawCrusher(IX As Single, IY As Single)
'******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MGRMVE(IX - 5 * ISS, IY + 7 * ISS)
  Call MGPLOT(IX - 4 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX - 8 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX - 2 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX + 2 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX + 8 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX + 4 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX + 5 * ISS, IY + 7 * ISS)
  NVERTS = 8
  SetVertex IX - ISS, IY + 7 * ISS, Verts(1)
  SetVertex IX - ISS, IY + 5 * ISS, Verts(2)
  SetVertex IX - 7 * ISS, IY - 3 * ISS, Verts(3)
  SetVertex IX, IY + ISS, Verts(4)
  SetVertex IX + 7 * ISS, IY - 3 * ISS, Verts(5)
  SetVertex IX + ISS, IY + 5 * ISS, Verts(6)
  SetVertex IX + ISS, IY + 7 * ISS, Verts(7)
  SetVertex IX - ISS, IY + 7 * ISS, Verts(8)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 5, UnitNumber)
End Sub

Sub DrawSump(IX As Single, IY As Single)
'***************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MGRMVE(IX - 4 * ISS, IY + 6 * ISS)
  Call MGPLOT(IX - 4 * ISS, IY + 5 * ISS)
  Call MGRMVE(IX + 4 * ISS, IY + 6 * ISS)
  Call MGPLOT(IX + 4 * ISS, IY + 5 * ISS)
  NVERTS = 5
  SetVertex IX - 4 * ISS, IY + 5 * ISS, Verts(1)
  SetVertex IX - 4 * ISS, IY - 4 * ISS, Verts(2)
  SetVertex IX, IY - 7 * ISS, Verts(3)
  SetVertex IX + 4 * ISS, IY - 4 * ISS, Verts(4)
  SetVertex IX + 4 * ISS, IY + 5 * ISS, Verts(5)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 0, UnitNumber)
End Sub

Sub DrawWhims(IX As Single, IY As Single)
'****************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(20 * ISS, 4 * ISS, IX - 10 * ISS, IY)
  Call FRECTAN(4 * ISS, 5 * ISS, IX - 2 * ISS, IY + 5 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 4, UnitNumber)
End Sub

Sub DrawShallowBath(IX As Single, IY As Single)
'**********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 3
  SetVertex IX - 8 * ISS, IY + ISS, Verts(1)
  SetVertex IX + 8 * ISS, IY + ISS, Verts(2)
  SetVertex IX, IY - 3 * ISS, Verts(3)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGRMVE(IX - 10 * ISS, IY + 2 * ISS)
  Call MGPLOT(IX, IY - 3 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY + 2 * ISS)
  Call MGRMVE(IX - 11 * ISS, IY - ISS)
  Call MGPLOT(IX - 10 * ISS, IY)
  Call MGPLOT(IX, IY - 5 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY)
  Call MGPLOT(IX + 11 * ISS, IY - ISS)
  Call MGRMVE(IX - 9 * ISS, IY + 3 * ISS)
  Call MGPLOT(IX - 3 * ISS, IY)
  Call MGPLOT(IX + 3 * ISS, IY)
  Call MGPLOT(IX + 11 * ISS, IY + 4 * ISS)
  
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 4, UnitNumber)
End Sub

Sub DrawDenseMediumCyclone(IX As Single, IY As Single)
'*****************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call CYCLONE(IX, IY, IR, 8)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawDynaWhirlpool(IX As Single, IY As Single)
'************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(20 * ISS, 6 * ISS, IX - 10 * ISS, IY + 3 * ISS)
  Call MGRMVE(IX - 10 * ISS, IY - ISS)
  Call MGPLOT(IX - 13 * ISS, IY - ISS)
  Call MGPLOT(IX - 13 * ISS, IY + ISS)
  Call MGPLOT(IX - 10 * ISS, IY + ISS)
  Call MGRMVE(IX + 10 * ISS, IY - ISS)
  Call MGPLOT(IX + 13 * ISS, IY - ISS)
  Call MGPLOT(IX + 13 * ISS, IY + ISS)
  Call MGPLOT(IX + 10 * ISS, IY + ISS)
  Call MGRMVE(IX - 10 * ISS, IY + 3 * ISS)
  Call MGPLOT(IX - 6 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX - 2 * ISS, IY + 3 * ISS)
  Call MGPLOT(IX + 2 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX + 6 * ISS, IY + 3 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY - 3 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -1, 2, UnitNumber)
End Sub

Sub DrawBaumJig(IX As Single, IY As Single)
'******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 10
  SetVertex IX - 5 * IR, IY - 3 * ISS, Verts(1)
  SetVertex IX - 5 * IR, IY - 5 * ISS, Verts(2)
  SetVertex IX - 4 * IR, IY - 8 * ISS, Verts(3)
  SetVertex IX - 2 * IR, IY - 10 * ISS, Verts(4)
  SetVertex IX + 2 * IR, IY - 10 * ISS, Verts(5)
  SetVertex IX + 4 * IR, IY - 8 * ISS, Verts(6)
  SetVertex IX + 5 * IR, IY - 5 * ISS, Verts(7)
  SetVertex IX + 5 * IR, IY + 4 * ISS, Verts(8)
  SetVertex IX - 2 * IR, IY + 4 * ISS, Verts(9)
  SetVertex IX - 2 * IR, IY - 3 * ISS, Verts(10)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGRMVE(IX - 2 * IR, IY - 4 * ISS)
  Call MGPLOT(IX - 2 * IR, IY + 5 * ISS)
  Call MGPLOT(IX - 5 * IR, IY + 5 * ISS)
  Call MGPLOT(IX - 5 * IR, IY - 3 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 3, UnitNumber)
End Sub

Sub DrawBatacJig(IX As Single, IY As Single)
'*******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 6
  SetVertex IX - 5 * ISS, IY + 5 * ISS, Verts(1)
  SetVertex IX - 5 * ISS, IY - 5 * ISS, Verts(2)
  SetVertex IX - ISS, IY - 10 * ISS, Verts(3)
  SetVertex IX + ISS, IY - 10 * ISS, Verts(4)
  SetVertex IX + 5 * ISS, IY - 5 * ISS, Verts(5)
  SetVertex IX + 5 * ISS, IY + 5 * ISS, Verts(6)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGRMVE(IX - 5 * ISS, IY + 4 * ISS)
  Call MGPLOT(IX - 4 * ISS, IY + 2 * ISS)
  Call MGPLOT(IX - 4 * ISS, IY)
  Call MGRMVE(IX, IY + 4 * ISS)
  Call MGPLOT(IX - ISS, IY + 2 * ISS)
  Call MGPLOT(IX - ISS, IY)
  Call MGRMVE(IX - 5 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX - 4 * ISS, IY - 5 * ISS)
  Call MGRMVE(IX, IY - 3 * ISS)
  Call MGPLOT(IX - ISS, IY - 5 * ISS)
  Call MGRMVE(IX, IY + 4 * ISS)
  Call MGPLOT(IX + ISS, IY + 2 * ISS)
  Call MGPLOT(IX + ISS, IY)
  Call MGRMVE(IX + 5 * ISS, IY + 4 * ISS)
  Call MGPLOT(IX + 4 * ISS, IY + 2 * ISS)
  Call MGPLOT(IX + 4 * ISS, IY)
  Call MGRMVE(IX, IY - 3 * ISS)
  Call MGPLOT(IX + ISS, IY - 5 * ISS)
  Call MGRMVE(IX + 5 * ISS, IY - 3 * ISS)
  Call MGPLOT(IX + 4 * ISS, IY - 5 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -2, 4, UnitNumber)
End Sub

Sub DrawNorwaltWasher(IX As Single, IY As Single)
'************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 5
  SetVertex IX - ISS, IY + 9 * ISS, Verts(1)
  SetVertex IX - ISS, IY - 6 * ISS, Verts(2)
  SetVertex IX + ISS, IY - 6 * ISS, Verts(3)
  SetVertex IX + ISS, IY + 9 * ISS, Verts(4)
  SetVertex IX, IY + 10 * ISS, Verts(5)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  
  Call MGMOVE(IX - ISS, IY + 9 * ISS)
  Call MGPLOT(IX - 2 * ISS, IY + 9 * ISS)
  Call MGPLOT(IX - 3 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX - 9 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX - 7 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX - 3 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX - ISS, IY + 2 * ISS)
  Call MGRMVE(IX + ISS, IY + 2 * ISS)
  Call MGPLOT(IX + 3 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX + 7 * ISS, IY - 6 * ISS)
  Call MGPLOT(IX + 9 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX + 3 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX + 2 * ISS, IY + 9 * ISS)
  Call MGPLOT(IX + ISS, IY + 9 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -5, 0, UnitNumber)
End Sub

Sub DrawWemcoDrum(IX As Single, IY As Single)
'********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(16 * IR, 10 * ISS, IX - 8 * IR, IY + 5 * ISS)
  Call MGRMVE(IX + 13 * IR, IY - 2 * ISS)
  Call MGPLOT(IX - 8 * IR, IY - 2 * ISS)
  Call MGRMVE(IX - 8 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * IR, IY + 3 * ISS)
  NVERTS = 4
  SetVertex IX - 8 * IR, IY - 5 * ISS, Verts(1)
  SetVertex IX - 10 * IR, IY - 3 * ISS, Verts(2)
  SetVertex IX - 10 * IR, IY + 3 * ISS, Verts(3)
  SetVertex IX - 8 * IR, IY + 5 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawTeskaDrum(IX As Single, IY As Single)
'********************************************
  Dim ISS As Single, FACT As Single
  ISS = Abs(IR)
  FACT = ISS * 6
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX, IY, FACT)
  End If
  Call MGRMVE(IX, IY)
  Call MGPLOT(IX - 10 * ISS, IY - 5 * ISS)
  Call MGRMVE(IX, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY + 2 * ISS)
  Call MGPLOT(IX + 10 * ISS, IY + 3 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 4, UnitNumber)
End Sub

Sub DrawChanceSandBath(IX As Single, IY As Single)
'*************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 14
  SetVertex IX - 5 * ISS, IY + 5 * ISS, Verts(1)
  SetVertex IX - 5 * ISS, IY + 2 * ISS, Verts(2)
  SetVertex IX - ISS, IY - 5 * ISS, Verts(3)
  SetVertex IX - ISS, IY - 9 * ISS, Verts(4)
  SetVertex IX - 2 * ISS, IY - 10 * ISS, Verts(5)
  SetVertex IX - 2 * ISS, IY - 13 * ISS, Verts(6)
  SetVertex IX - ISS, IY - 14 * ISS, Verts(7)
  SetVertex IX + ISS, IY - 14 * ISS, Verts(8)
  SetVertex IX + 2 * ISS, IY - 13 * ISS, Verts(9)
  SetVertex IX + 2 * ISS, IY - 10 * ISS, Verts(10)
  SetVertex IX + ISS, IY - 9 * ISS, Verts(11)
  SetVertex IX + ISS, IY - 5 * ISS, Verts(12)
  SetVertex IX + 5 * ISS, IY + 2 * ISS, Verts(13)
  SetVertex IX + 5 * ISS, IY + 5 * ISS, Verts(14)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawSegregatingBin(IX As Single, IY As Single)
'*************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 8
  SetVertex IX - 4 * IR, IY + 6 * ISS, Verts(1)
  SetVertex IX + 4 * IR, IY + 6 * ISS, Verts(2)
  SetVertex IX + 4 * IR, IY - 4 * ISS, Verts(3)
  SetVertex IX + 2 * IR, IY - 8 * ISS, Verts(4)
  SetVertex IX, IY - 4 * ISS, Verts(5)
  SetVertex IX - 2 * IR, IY - 8 * ISS, Verts(6)
  SetVertex IX - 4 * IR, IY - 4 * ISS, Verts(7)
  SetVertex IX - 4 * IR, IY + 6 * ISS, Verts(8)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawBlackBox(IX As Single, IY As Single)
'*******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call FRECTAN(14 * ISS, 8 * ISS, IX - 7 * ISS, IY + 4 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawDenseMediumVessel(IX As Single, IY As Single)
'****************************************************
  Dim ISS As Single, R As Single
  ISS = Abs(IR)
  R = 5 * ISS
  NVERTS = 4
  SetVertex IX - 5 * ISS, IY + 5 * ISS, Verts(1)
  SetVertex IX - 5 * ISS, IY - 5 * ISS, Verts(2)
  SetVertex IX + 5 * ISS, IY - 5 * ISS, Verts(3)
  SetVertex IX + 5 * ISS, IY + 5 * ISS, Verts(4)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGCIRCLE(IX, IY, R)
  Call MGRMVE(IX - 5 * ISS, IY)
  Call MGPLOT(IX + 5 * ISS, IY)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 2, UnitNumber)
End Sub

Sub DrawAutogenousMill(IX As Single, IY As Single)
'*************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MillIcon(IX, IY, ISS, IR, -2, 4)
End Sub

Sub DrawAutogenousMillWithTrommel(IX As Single, IY As Single)
'************************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MillIcon(IX, IY, ISS, IR, -2, 4)
  Call TrommelScreen(IX + (10 - 2) * IR, IY + 2 * ISS)
End Sub

Sub DrawRodMill(IX As Single, IY As Single)
'******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MillIcon(IX, IY, ISS, IR, 3, -1)
End Sub

Sub DrawGyratoryCrusher(IX As Single, IY As Single)
'**************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 12
  SetVertex IX, IY + 7 * ISS, Verts(1)
  SetVertex IX - ISS, IY + 7 * ISS, Verts(2)
  SetVertex IX - ISS, IY + 6 * ISS, Verts(3)
  SetVertex IX - 2 * ISS, IY + 6 * ISS, Verts(4)
  SetVertex IX - 4 * ISS, IY - 3 * ISS, Verts(5)
  SetVertex IX - 2 * ISS, IY - 3 * ISS, Verts(6)
  SetVertex IX - ISS, IY - 7 * ISS, Verts(7)
  SetVertex IX, IY - 7 * ISS, Verts(8)
  SetVertex IX + ISS, IY - 3 * ISS, Verts(9)
  SetVertex IX + 3 * ISS, IY - 3 * ISS, Verts(10)
  SetVertex IX + ISS, IY + 6 * ISS, Verts(11)
  SetVertex IX, IY + 6 * ISS, Verts(12)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  
  MGMOVE IX - 7 * ISS, IY + 6 * ISS
  MGPLOT IX - 5 * ISS, IY - 3 * ISS
  MGMOVE IX + 7 * ISS, IY + 6 * ISS
  MGPLOT IX + 5 * ISS, IY - 3 * ISS
  
  If UnitNumberFlagOn Then Call Number(IX, IY, 2, 1, UnitNumber)
End Sub

Sub DrawWateronlyCyclone(IX As Single, IY As Single)
'***************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call CYCLONE(IX, IY, IR, 5)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, -1, UnitNumber)
End Sub

Sub DrawDewateringScreen(IX As Single, IY As Single)
'***************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 5
  SetVertex IX - 10 * IR, IY + 2 * ISS, Verts(1)
  SetVertex IX - 10 * IR, IY, Verts(2)
  SetVertex IX, IY - 3 * ISS, Verts(3)
  SetVertex IX + 10 * IR, IY, Verts(4)
  SetVertex IX + 10 * IR, IY + 2 * ISS, Verts(5)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGLNTYPE(7)
  Call MGRMVE(IX + 10 * IR, IY + 2 * ISS)
  Call MGPLOT(IX - 10 * IR, IY + 2 * ISS)
  Call MGLNTYPE(1)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 3, UnitNumber)
End Sub

Sub DrawSieveBend(IX As Single, IY As Single)
'*******************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 11
  SetVertex IX - 7 * IR, IY + 11 * ISS, Verts(1)
  SetVertex IX - 7 * IR, IY + 4 * ISS, Verts(2)
  SetVertex IX, IY - 2 * ISS, Verts(3)
  SetVertex IX + 6 * IR, IY, Verts(4)
  SetVertex IX + 6 * IR, IY + 2 * ISS, Verts(5)
  SetVertex IX + 4 * IR, IY + 2 * ISS, Verts(6)
  SetVertex IX + IR, IY + 3 * ISS, Verts(7)
  SetVertex IX - 2 * IR, IY + 5 * ISS, Verts(8)
  SetVertex IX - 4 * IR, IY + 7 * ISS, Verts(9)
  SetVertex IX - 5 * IR, IY + 10 * ISS, Verts(10)
  SetVertex IX - 5 * IR, IY + 11 * ISS, Verts(11)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawFixedRollMill(IX As Single, IY As Single)
'************************************************
  Call ROLLMILL(IX, IY, IR)
End Sub

Sub DrawElutriator(IX As Single, IY As Single)
'*********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 12
  SetVertex IX - 3 * IR, IY + 10 * ISS, Verts(1)
  SetVertex IX + 3 * IR, IY + 10 * ISS, Verts(2)
  SetVertex IX + 3 * IR, IY + 5 * ISS, Verts(3)
  SetVertex IX + 6 * IR, IY + 4 * ISS, Verts(4)
  SetVertex IX + 6 * IR, IY - 10 * ISS, Verts(5)
  SetVertex IX + 1 * IR, IY - 13 * ISS, Verts(6)
  SetVertex IX - 1 * IR, IY - 13 * ISS, Verts(7)
  SetVertex IX - 6 * IR, IY - 10 * ISS, Verts(8)
  SetVertex IX - 6 * IR, IY + 3 * ISS, Verts(9)
  SetVertex IX - 7 * IR, IY + 3 * ISS, Verts(10)
  SetVertex IX - 7 * IR, IY + 5 * ISS, Verts(11)
  SetVertex IX - 3 * IR, IY + 5 * ISS, Verts(12)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)

  Call MGRMVE(IX - IR, IY + 9 * ISS)
  Call MGPLOT(IX + 7 * IR, IY + 9 * ISS)
  Call MGPLOT(IX + 7 * IR, IY + 8 * ISS)
  Call MGPLOT(IX + IR, IY + 8 * ISS)
  Call MGPLOT(IX + IR, IY - 7 * ISS)
  Call MGPLOT(IX + 2 * IR, IY - 8 * ISS)
  Call MGPLOT(IX - 2 * IR, IY - 8 * ISS)
  Call MGPLOT(IX - IR, IY - 7 * ISS)
  Call MGPLOT(IX - IR, IY + 9 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -1, 0, UnitNumber)
End Sub

Sub DrawConveyor(IX As Single, IY As Single)
'*******************************************
  Dim FACT As Single, IX1 As Single, IY1 As Single
  Dim Slope As Single
  Dim DELX As Single, DELY As Single
  If RPFLAG > 1 Then
    Call DrawConveyorBelt(IX, IY, CONVEY_X(NSEL), CONVEY_Y(NSEL))
  End If
  
  If MGPage(0).DeleteFlag Or MGPage(0).PaintFlag Or MGPage(0).MovingFlag1 Or MGPage(0).ReplaceFlag Then
    Call DrawConveyorBelt(IX, IY, CONVEY_X(NSEL), CONVEY_Y(NSEL))
  End If
  
  If MGPage(0).MovingFlag2 Then
    If MGPage(0).SecondDrumFlag Then
      Call DrawConveyorBelt(UnitLocX(62, NSEL), UnitLocY(62, NSEL), IX, IY)
      CONVEY_X(NSEL) = IX
      CONVEY_Y(NSEL) = IY
      IX = UnitLocX(62, NSEL)
      IY = UnitLocY(62, NSEL)
    ElseIf MGPage(0).FirstDrumFlag Then
      Call DrawConveyorBelt(IX, IY, CONVEY_X(NSEL), CONVEY_Y(NSEL))
    Else
      CONVEY_X(NSEL) = IX - ConveyorDel2X
      IX = IX - ConveyorDel1X
      CONVEY_Y(NSEL) = IY - ConveyorDel2Y
      IY = IY - ConveyorDel1Y
      Call DrawConveyorBelt(IX, IY, CONVEY_X(NSEL), CONVEY_Y(NSEL))
    End If
  End If
  
  If RPFLAG = 1 And Not MGPage(0).MoveFlag And Not MGPage(0).DeleteFlag And Not MGPage(0).PaintFlag And Not MGPage(0).ReplaceFlag Then
    'Drawing the first time must be interactive to position the second drum.
    MGPage(0).DrawMode = 7
    MGPage(0).ForeColor = MGPage(0).BackColor
    MGPage(0).ConveyorFlag = True
    MGPage(0).FirstDrumFlag = False
    MGPage(0).SecondDrumFlag = True
    FACT = 2 * Abs(IR)
    'Draw the first drum
    Call MGCIRCLE(IX, IY, FACT)
    'Draw the second drum
    IX1 = MGPage(0).MouseLocationX
    IY1 = MGPage(0).MouseLocationY
    Call MGCIRCLE(IX1, IY1, FACT)
    'Draw the belt
    If IX1 = IX Then
      DELY = 0
    Else
      Slope = (IY1 - IY) / (IX1 - IX)
      DELY = FACT / Sqr(1# + Slope * Slope)
    End If
    DELX = -Slope * DELY
    Call MGRMVE(IX + DELX, IY + DELY)
    Call MGPLOT(IX1 + DELX, IY1 + DELY)
    CONVEY_X(NSEL) = IX1
    CONVEY_Y(NSEL) = IY1
  End If
End Sub

Sub DrawSpiralClassifier(IX As Single, IY As Single)
'***************************************************
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 6
  SetVertex IX - 5 * IR, IY + 6 * ISS, Verts(1)
  SetVertex IX - 5 * IR, IY + 3 * ISS, Verts(2)
  SetVertex IX + 3 * IR, IY - 3 * ISS, Verts(3)
  SetVertex IX + 5 * IR, IY - 3 * ISS, Verts(4)
  SetVertex IX + 5 * IR, IY, Verts(5)
  SetVertex IX + 3 * IR, IY, Verts(6)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  If UnitNumberFlagOn Then Call Number(IX, IY, -1, 0, UnitNumber)
End Sub

Sub MillIcon(IX As Single, IY As Single, ISS As Single, IR As Single, lx As Single, ly As Single)
'************************************************************************************************
'  DRAW THE ICON FOR THE MILL
Dim IX1 As Single, IX2 As Single, IX3 As Single, IX4 As Single
Dim IX5 As Single, IX6 As Single, IX7 As Single, IX8 As Single
Dim IY1 As Single, IY2 As Single, IY3 As Single, IY4 As Single
  IX1 = IX - (11 + lx) * IR
  IX2 = IX1 + IR
  IX3 = IX2 + IR
  IX4 = IX3 + IR
  IX5 = IX + (6 + lx) * IR
  IX6 = IX5 + 2 * IR
  IX7 = IX6 + IR
  IX8 = IX7 + 1 * IR
  IY1 = (9 + ly) * ISS
  IY2 = IY1 - 2 * ISS
  IY3 = 3 * ISS
  IY4 = 2 * ISS
  NVERTS = 20
  SetVertex IX1, IY + IY3, Verts(1)
  SetVertex IX2, IY + IY4, Verts(2)
  SetVertex IX3, IY + IY4, Verts(3)
  SetVertex IX4, IY + IY2, Verts(4)
  SetVertex IX5, IY + IY2, Verts(5)
  SetVertex IX5, IY + IY1, Verts(6)
  SetVertex IX6, IY + IY1, Verts(7)
  SetVertex IX6, IY + IY2, Verts(8)
  SetVertex IX7, IY + IY4, Verts(9)
  SetVertex IX8, IY + IY4, Verts(10)
  SetVertex IX8, IY - IY4, Verts(11)
  SetVertex IX7, IY - IY4, Verts(12)
  SetVertex IX6, IY - IY2, Verts(13)
  SetVertex IX6, IY - IY1, Verts(14)
  SetVertex IX5, IY - IY1, Verts(15)
  SetVertex IX5, IY - IY2, Verts(16)
  SetVertex IX4, IY - IY2, Verts(17)
  SetVertex IX3, IY - IY4, Verts(18)
  SetVertex IX2, IY - IY4, Verts(19)
  SetVertex IX1, IY - IY3, Verts(20)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGVertexMove(Verts(2))
  Call MGVertexPlot(Verts(19))
  Call MGVertexMove(Verts(3))
  Call MGVertexPlot(Verts(18))
  Call MGVertexMove(Verts(5))
  Call MGVertexPlot(Verts(16))
  Call MGVertexMove(Verts(8))
  Call MGVertexPlot(Verts(13))
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 0, UnitNumber)
End Sub

Sub TrommelScreen(X As Single, Y As Single)
'******************************************
  'Draw a trommel screen
  Dim I As Integer, ISS As Single
  Dim GAP As Single, YGAP As Single, height As Single
  ISS = Abs(IR)
  height = 4 * ISS
  'Shade the icon
  NVERTS = 4
  Call MGGRAYFILL(0.9)
  Verts(1).xcoord = X
  Verts(1).ycoord = Y
  Verts(2).xcoord = X + 8 * IR
  Verts(2).ycoord = Y
  Verts(3).xcoord = X + 8 * IR
  Verts(3).ycoord = Y - height
  Verts(4).xcoord = X
  Verts(4).ycoord = Y - height
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  
  GAP = 4 * IR / 5
  YGAP = Abs(GAP)
  Call LINEWD(0.1)
  For I = 1 To 10
    Call MGRMVE(X + I * GAP, Y)
    Call MGPLOT(X + I * GAP, Y - height)
  Next I
  For I = 0 To 5
    Call MGRMVE(X, Y - I * YGAP)
    Call MGPLOT(X + 8 * IR, Y - I * YGAP)
  Next I
  Call RESLWD
  Call MGRMVE(X, Y - height - YGAP)
  Call MGPLOT(X, Y - height - 2 * YGAP)
  Call MGPLOT(X + 3 * GAP, Y - height - 4 * YGAP)
  Call MGPLOT(X + 8 * IR, Y - height - 2 * YGAP)
  Call MGPLOT(X + 8 * IR, Y - height - YGAP)
Exit Sub

End Sub

Sub DrawStockpile(IX As Single, IY As Single)
'********************************************
'  ICON FOR A STOCKPILE
  Dim I As Integer, LS As Single, ISS As Single
  ISS = Abs(IR)
  LS = ISS
  NVERTS = 4
  SetVertex IX + ISS, IY + 10 * ISS, Verts(1)
  SetVertex IX + 14 * ISS, IY - 10 * ISS, Verts(2)
  SetVertex IX - ISS, IY + 10 * ISS, Verts(4)
  SetVertex IX - 14 * ISS, IY - 10 * ISS, Verts(3)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  For I = 1 To 2
    Call MGRMVE(IX + LS, IY + 6 * ISS)
    Call MGPLOT(IX + 9 * LS, IY - 10 * ISS)
    Call MGRMVE(IX + LS, IY + 3 * ISS)
    Call MGPLOT(IX + 4 * LS, IY - 10 * ISS)
    LS = -LS
  Next I
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, 5, UnitNumber)
End Sub

Sub CYCLONE(IX As Single, IY As Single, IR As Single, ICONE As Integer)
'*********************************************************************
' Draws the icon for the cyclone. ICONE defines the cone angle.
  Dim ISS As Single
  ISS = Abs(IR)
  NVERTS = 14
  SetVertex IX + 3 * IR, IY + 5 * ISS, Verts(1)
  SetVertex IX + IR, IY + 5 * ISS, Verts(2)
  SetVertex IX + IR, IY + 7 * ISS, Verts(3)
  SetVertex IX - IR, IY + 7 * ISS, Verts(4)
  SetVertex IX - IR, IY + 5 * ISS, Verts(5)
  SetVertex IX - 3 * IR, IY + 5 * ISS, Verts(6)
  SetVertex IX - 3 * IR, IY + 4 * ISS, Verts(7)
  SetVertex IX - 5 * IR, IY + 4 * ISS, Verts(8)
  SetVertex IX - 5 * IR, IY + 2 * ISS, Verts(9)
  SetVertex IX - 3 * IR, IY + 2 * ISS, Verts(10)
  SetVertex IX - 3 * IR, IY - ISS * (8 - ICONE), Verts(11)
  SetVertex IX - IR, IY - 8 * ISS, Verts(12)
  SetVertex IX + IR, IY - 8 * ISS, Verts(13)
  SetVertex IX + 3 * IR, IY - ISS * (8 - ICONE), Verts(14)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  
  Call MGMOVE(IX - 3 * IR, IY + 4 * ISS)
  Call MGPLOT(IX - 2 * IR, IY + 4 * ISS)
  Call MGMOVE(IX - 3 * IR, IY + 2 * ISS)
  Call MGPLOT(IX - 2 * IR, IY + 2 * ISS)
End Sub

Sub ROLLMILL(IX As Single, IY As Single, IR As Single)
'*****************************************************
  Dim ISS As Single, FACT As Single
  ISS = Abs(IR)
  FACT = ISS * 3
  MGPage(0).FillStyle = 0
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX - 4 * ISS, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX - 4 * ISS, IY, FACT)
  End If
  If MGPage(0).PolygonType = 5 Then
    Call MGFilledCircle(IX + 4 * ISS, IY, FACT)
  ElseIf MGPage(0).PolygonType = 1 Then
    Call MGCIRCLE(IX + 4 * ISS, IY, FACT)
  End If
  Call MGRMVE(IX - 3 * ISS, IY + 5 * ISS)
  Call MGPLOT(IX - ISS, IY + ISS)
  Call MGRMVE(IX + ISS, IY + ISS)
  Call MGPLOT(IX + 3 * ISS, IY + 5 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, -1, UnitNumber)
End Sub

Sub DrawHPRollMill(IX As Single, IY As Single)
'*********************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call ROLLMILL(IX, IY, IR)
  Call SPRING(IX + 4 * IR, IY, ISS)
  Call SPRING(IX + 5 * IR, IY, ISS)
  Call SPRING(IX + 6 * IR, IY, ISS)
  Call SPRING(IX + 7 * IR, IY, ISS)
  Call SPRING(IX + 8 * IR, IY, ISS)
End Sub

Sub SPRING(IX As Single, IY As Single, ISS As Single)
'****************************************************
' Draws a spring leaf
  Call MGRMVE(IX, IY - ISS)
  Call MGPLOT(IX + ISS, IY + ISS)
End Sub

Sub DrawKNELSON(IX As Single, IY As Single)
'******************************************
' Icon for the Knelson concentrator
  Dim ISS As Single
      ISS = Abs(IR)
      Call FRECTAN(10 * IR, 12 * ISS, IX - 5 * IR, IY + 6 * ISS)
      Call MGRMVE(IX - 5 * IR, IY + 6 * ISS)
      Call MGPLOT(IX, IY - 4 * ISS)
      Call MGPLOT(IX + 5 * IR, IY + 6 * ISS)
      Call MGRMVE(IX - 5 * IR, IY - 4 * ISS)
      Call MGPLOT(IX + 5 * IR, IY - 4 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 1, -1, UnitNumber)
End Sub
  
Public Sub DrawWaterInjectionCyclone(IX As Single, IY As Single)
'***************************************************************
' Draws the icon for the water injection cyclone.
  Dim ISS As Single, IRS As Single
  ISS = 0.8 * Abs(IR)
  IRS = 0.8 * IR
  NVERTS = 26
  SetVertex IX + 4 * IRS, IY + 11 * ISS, Verts(1)
  SetVertex IX + 4 * IRS, IY + 9 * ISS, Verts(2)
  SetVertex IX + 6 * IRS, IY + 9 * ISS, Verts(3)
  SetVertex IX + 6 * IRS, IY + 7 * ISS, Verts(4)
  SetVertex IX + 4 * IRS, IY + 7 * ISS, Verts(5)
  SetVertex IX + 4 * IRS, IY + 6 * ISS, Verts(6)
  SetVertex IX + 3 * IRS, IY + 6 * ISS, Verts(7)
  SetVertex IX + 3 * IRS, IY - 6 * ISS, Verts(8)
  SetVertex IX + 4 * IRS, IY - 6 * ISS, Verts(9)
  SetVertex IX + 4 * IRS, IY - 10 * ISS, Verts(10)
  SetVertex IX + 2 * IRS, IY - 10 * ISS, Verts(11)
  SetVertex IX + 2 * IRS, IY - 12 * ISS, Verts(12)
  SetVertex IX + 1 * IRS, IY - 13 * ISS, Verts(13)
  SetVertex IX - 1 * IRS, IY - 13 * ISS, Verts(14)
  SetVertex IX - 2 * IRS, IY - 12 * ISS, Verts(15)
  SetVertex IX - 2 * IRS, IY - 10 * ISS, Verts(16)
  SetVertex IX - 4 * IRS, IY - 10 * ISS, Verts(17)
  SetVertex IX - 4 * IRS, IY - 9 * ISS, Verts(18)
  SetVertex IX - 7 * IRS, IY - 9 * ISS, Verts(19)
  SetVertex IX - 7 * IRS, IY - 7 * ISS, Verts(20)
  SetVertex IX - 4 * IRS, IY - 7 * ISS, Verts(21)
  SetVertex IX - 4 * IRS, IY - 6 * ISS, Verts(22)
  SetVertex IX - 3 * IRS, IY - 6 * ISS, Verts(23)
  SetVertex IX - 3 * IRS, IY + 6 * ISS, Verts(24)
  SetVertex IX - 4 * IRS, IY + 6 * ISS, Verts(25)
  SetVertex IX - 4 * IRS, IY + 11 * ISS, Verts(26)
  Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
  Call MGVertexMove(Verts(2))
  Call MGPLOT(IX, IY + 9 * ISS)
  Call MGVertexMove(Verts(4))
  Call MGPLOT(IX, IY + 7 * ISS)
  Call MGVertexMove(Verts(20))
  Call MGPLOT(IX - 2 * IRS, IY - 7 * ISS)
  Call MGVertexMove(Verts(19))
  Call MGPLOT(IX - 2 * IRS, IY - 9 * ISS)
  If UnitNumberFlagOn Then Call Number(IX, IY, 0, 1, UnitNumber)
End Sub

Sub DrawFlotationColumn(IX As Single, IY As Single)
'************************************************
   Dim ISS As Single
   ISS = Abs(IR)
   NVERTS = 9
   SetVertex IX - 3 * IR, IY + 8 * ISS, Verts(1)
   SetVertex IX - 3 * IR, IY - 8 * ISS, Verts(2)
   SetVertex IX, IY - 9 * ISS, Verts(3)
   SetVertex IX + 3 * IR, IY - 8 * ISS, Verts(4)
   SetVertex IX + 3 * IR, IY + 7 * ISS, Verts(5)
   SetVertex IX + 4 * IR, IY + 7 * ISS, Verts(6)
   SetVertex IX + 4 * IR, IY + 10 * ISS, Verts(7)
   SetVertex IX - 4 * IR, IY + 10 * ISS, Verts(8)
   SetVertex IX - 4 * IR, IY + 8 * ISS, Verts(9)
   Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)
   Call MGVertexMove(Verts(1))
   Call MGVertexPlot(Verts(5))

   If UnitNumberFlagOn Then Call Number(IX, IY, 1, 1, UnitNumber)
End Sub

Sub DrawJones(IX As Single, IY As Single)
'****************************************
   Dim ISS As Single, FACT As Single

   ISS = Abs(IR)
   FACT = ISS * 3.9
   NVERTS = 16
   SetVertex IX - 2 * ISS, IY, Verts(1)
   SetVertex IX - 4 * ISS, IY - 3 * ISS, Verts(2)
   SetVertex IX - 4 * ISS, IY - 4 * ISS, Verts(3)
   SetVertex IX - 3 * ISS, IY - 4 * ISS, Verts(4)
   SetVertex IX, IY - 2 * ISS, Verts(5)
   SetVertex IX + 3 * ISS, IY - 4 * ISS, Verts(6)
   SetVertex IX + 4 * ISS, IY - 4 * ISS, Verts(7)
   SetVertex IX + 4 * ISS, IY - 3 * ISS, Verts(8)
   SetVertex IX + 2 * ISS, IY, Verts(9)
   SetVertex IX + 4 * ISS, IY + 3 * ISS, Verts(10)
   SetVertex IX + 4 * ISS, IY + 4 * ISS, Verts(11)
   SetVertex IX + 3 * ISS, IY + 4 * ISS, Verts(12)
   SetVertex IX, IY + 2 * ISS, Verts(13)
   SetVertex IX - 3 * ISS, IY + 4 * ISS, Verts(14)
   SetVertex IX - 4 * ISS, IY + 4 * ISS, Verts(15)
   SetVertex IX - 4 * ISS, IY + 3 * ISS, Verts(16)
   Call MGPOLY(MGPage(0).PolygonType, Verts, NVERTS)

   Call MGCIRCLE(IX, IY, FACT)
   If UnitNumberFlagOn Then Call Number(IX, IY, 1, 4, UnitNumber)
End Sub



Public Sub DrawFlyout(IX As Single, IY As Single, AX As Single, AY As Single)
'***************************************************************************
'Draws a flyout on the flowsheet to contain mass balance data
'Upper left is at IX,IY and attachment to stream is at AX,AY
  Dim cx As Single, cy As Single
  Dim extra As Single
  If MGPage(0).ChkShowFlyouts.Value = vbChecked Then
    extra = 0
    If MGPage(0).SelectedFlyout = 0 Then
      'Drawing the reference flyout
      extra = 2.5 * FlyoutSize
    End If
    Call RECTAN(12.4 * FlyoutSize + extra, 5 * FlyoutSize, IX, IY)
    If Not MGPage(0).PaintFlag Then
      Call MGRMVE(IX, IY - 2.5 * FlyoutSize)
      Call MGPLOT(IX + 12.4 * FlyoutSize + extra, IY - 2.5 * FlyoutSize)
      Call MGRMVE(IX + 6.8 * FlyoutSize + 0.5 * extra, IY)
      Call MGPLOT(IX + 6.8 * FlyoutSize + 0.5 * extra, IY - 5 * FlyoutSize)
    End If
    If MGPage(0).SelectedFlyout = 0 Then Exit Sub
    Call MGRMVE(AX, AY)
    cx = IX
    cy = IY - 5 * FlyoutSize
    If AX > IX + 7 * FlyoutSize Then cx = IX + 12.4 * FlyoutSize
    If AY > IY - 2.5 * FlyoutSize Then cy = IY
    Call MGPLOT(cx, cy)
  End If
End Sub

