Attribute VB_Name = "Mdl3DSUBS"
'***********************       3DSUBS       ***************************C
'                                                                      C
'                METGRAPH INTERACTIVE PACKAGE                          C
'                                                                      C
'  THIS FILE HOLDS THE ROUTINES FOR 3-D GRAPHICS                       C
'                                                                      C
'  METGRAPH (C) COPYRIGHT  1985-1998  RP KING  Salt Lake City          C
'                                                                      C
'**********************************************************************C
Option Explicit

Public NVECT(3) As Single, RVECT(3) As Single, VVECT(3) As Single, UVECT(3) As Single
Public UP(3) As Single, THR_D_CURR_POS(3) As Single
Public Const Pi As Single = 3.1415927
Public FL_CMT(6) As Single, BW_CMT(6) As Single, TW_CMT(6) As Single
Public LW_CMT(6) As Single, RW_CMT(6)  As Single
Public LAB_CMT(6) As Single
Public RPW_CMT(6) As Single, LPW_CMT(6) As Single
Private XOrigin As Integer, YOrigin As Integer, ZOrigin As Integer

Sub Set3DOrigin(X As Integer, Y As Integer, Z As Integer)
'********************************************************
'Sets up the 3D origin to suit the application
  XOrigin = X
  YOrigin = Y
  ZOrigin = Z
End Sub

Sub VIEWPLANE(Distance As Single, Theta As Single, Phi As Single)
'***************************************************************
' Sets up the view plane for 3D views.
 Dim I As Integer
 UP(1) = 0#
 UP(2) = 0#
 UP(3) = 1#
 NVECT(1) = Sin(Phi) * Cos(Theta)
 NVECT(2) = Sin(Phi) * Sin(Theta)
 NVECT(3) = Cos(Phi)
 Call NORM(NVECT)
 For I = 1 To 3
   RVECT(I) = -Distance * NVECT(I)
 Next I
 VVECT(1) = UP(1) - DOTPROD(UP, NVECT) * NVECT(1)
 VVECT(2) = UP(2) - DOTPROD(UP, NVECT) * NVECT(2)
 VVECT(3) = UP(3) - DOTPROD(UP, NVECT) * NVECT(3)
 Call NORM(VVECT)
 Call CROSSPROD(NVECT, VVECT, UVECT)
' set up the CMT matrices
 Call LABEL_CMTS
End Sub

Sub VIEWC(XW As Single, YW As Single, ZW As Single, VX As Single, VY As Single, VZ As Single)
'*******************************************************************************************
' Get the coordinates in the view plane of a vertex at world
' coordinates xw, yw,zw)
 Dim DIFF(3) As Single
 DIFF(1) = XW - RVECT(1)
 DIFF(2) = YW - RVECT(2)
 DIFF(3) = ZW - RVECT(3)
 VX = DOTPROD(DIFF, UVECT) + XOrigin
 VY = DOTPROD(DIFF, VVECT) + YOrigin
 VZ = DOTPROD(DIFF, NVECT) + ZOrigin
End Sub

Sub VIEWC_CMT(WX As Single, WY As Single, VX As Single, VY As Single, CMT() As Single)
'*************************************************************************************
' Get the coordinates in the view plane of a vertex at world coordinates
' XW,YW,ZW using the CMT transformation matrix
  VX = CMT(1) * WX + CMT(3) * WY + CMT(5)
  VY = CMT(2) * WX + CMT(4) * WY + CMT(6)
End Sub

Sub LABEL_CMTS()
'****************
'  Sets up the PostScript CMTs for labels
Dim T30 As Single, C30 As Single
' Set up the CMT for the floor
 FL_CMT(1) = UVECT(1)
 FL_CMT(2) = VVECT(1)
 FL_CMT(3) = UVECT(2)
 FL_CMT(4) = VVECT(2)
 FL_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
 FL_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
' Set up the CMT for bottom wall
 BW_CMT(1) = UVECT(1)
 BW_CMT(2) = VVECT(1)
 BW_CMT(3) = UVECT(3)
 BW_CMT(4) = VVECT(3)
 BW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
 BW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
' Set up the CMT for right hand wall
 RW_CMT(1) = UVECT(2)
 RW_CMT(2) = VVECT(2)
 RW_CMT(3) = UVECT(3)
 RW_CMT(4) = VVECT(3)
 RW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
 RW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
' Set up the CMT for left hand wall
 LW_CMT(1) = -UVECT(2)
 LW_CMT(2) = -VVECT(2)
 LW_CMT(3) = UVECT(3)
 LW_CMT(4) = VVECT(3)
 LW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
 LW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
' Set up the CMT for top wall
 TW_CMT(1) = -UVECT(1)
 TW_CMT(2) = -VVECT(1)
 TW_CMT(3) = UVECT(3)
 TW_CMT(4) = VVECT(3)
 TW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
 TW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
  T30 = Tan(30# * Pi / 180#)
  C30 = Cos(30# * Pi / 180#)
  RPW_CMT(1) = UVECT(1) + UVECT(2) / T30
  RPW_CMT(2) = VVECT(1) + VVECT(2) / T30
  RPW_CMT(3) = UVECT(3)
  RPW_CMT(4) = VVECT(3)
  RPW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
  RPW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
' Set up the CMT for left prism wall
  LPW_CMT(1) = UVECT(1) - UVECT(2) / T30
  LPW_CMT(2) = VVECT(1) - VVECT(2) / T30
  LPW_CMT(3) = UVECT(3)
  LPW_CMT(4) = VVECT(3)
  LPW_CMT(5) = -DOTPROD(UVECT, RVECT) + XOrigin
  LPW_CMT(6) = -DOTPROD(VVECT, RVECT) + YOrigin
End Sub

Sub NORM(VECT() As Single)
'***************************
' Normalize the 3D vector VECT
  Dim I As Integer
  Dim Length As Single
  
  Length = Sqr(VECT(1) * VECT(1) + VECT(2) * VECT(2) + VECT(3) * VECT(3))
  For I = 1 To 3
    VECT(I) = VECT(I) / Length
  Next I
End Sub

Function DOTPROD(V1() As Single, V2() As Single)
'***********************************************
' Computes the dot product of two 3D vectors V1 and V2.
 Dim I As Integer
 DOTPROD = 0#
 For I = 1 To 3
   DOTPROD = DOTPROD + V1(I) * V2(I)
 Next I
End Function

 Sub CROSSPROD(V1() As Single, V2() As Single, VP() As Single)
'*************************************************************
' Generates the cross product of two 3D vectors V1 and V2. The result
' is returned in VP.
 VP(1) = V1(2) * V2(3) - V1(3) * V2(2)
 VP(2) = V1(3) * V2(1) - V1(1) * V2(3)
 VP(3) = V1(1) * V2(2) - V1(2) * V2(1)
 End Sub

Sub MG3DMOVE(X As Single, Y As Single, Z As Single)
'*************************************************
' This is the 3D move routine
  Dim VX As Single, VY As Single, VZ As Single
  Call VIEWC(X, Y, Z, VX, VY, VZ)
  Call MGRMVE(VX, VY)
  THR_D_CURR_POS(1) = X
  THR_D_CURR_POS(2) = Y
  THR_D_CURR_POS(3) = Z
End Sub

Sub MG3DPLOT(X As Single, Y As Single, Z As Single)
'*************************************************
' This is the 3D plot routine
  Dim VX As Single, VY As Single, VZ As Single
  Call VIEWC(X, Y, Z, VX, VY, VZ)
  Call RPLOT(VX, VY)
  THR_D_CURR_POS(1) = X
  THR_D_CURR_POS(2) = Y
  THR_D_CURR_POS(3) = Z
End Sub

Sub ThreeDLINE(X1 As Single, Y1 As Single, Z1 As Single, X2 As Single, Y2 As Single, Z2 As Single)
' **************************************************************************************************
' Draws a line from world coordinates x1,y1,z1 to x2,y2,z2.
  Call MG3DMOVE(X1, Y1, Z1)
  Call MG3DPLOT(X2, Y2, Z2)
End Sub

Sub MGFLOORLAB(X As Single, Y As Single, Z As Single, Label As String, N As Integer, ORIEN As Integer, Theta As Single, Phi As Single)
'*************************************************************************************************************************************
' Write a label on the floor at world coordinates X,Y,Z
  Dim I As Integer
  For I = 1 To 6
    LAB_CMT(I) = FL_CMT(I)
  Next I
  LAB_CMT(5) = LAB_CMT(5) + UVECT(3) * Z
  LAB_CMT(6) = LAB_CMT(6) + VVECT(3) * Z
  Call PSBUFW(" gsave", 6)
  Call MG3DMOVE(X, Y, Z)
  Call PSCONCAT(LAB_CMT)
  If (ORIEN = 2) Then Call ROTATE(90#)
  If (ORIEN = 3) Then Call ROTATE(180#)
  If (ORIEN = 4) Then Call ROTATE(270#)
  Call SETCUR(X, Y)
  Call LABELC(Label, N)
  If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
  Call PSBUFW(" grestore", 9)
End Sub

Sub MGFLOORLABEL(X As Single, Y As Single, Z As Single, Label As String, N As Integer, Angle As Single)
'******************************************************************************************************
' Write a label on the floor at world coordinates X,Y,Z
  Dim I As Integer
  For I = 1 To 6
    LAB_CMT(I) = FL_CMT(I)
  Next I
  LAB_CMT(5) = LAB_CMT(5) + UVECT(3) * Z
  LAB_CMT(6) = LAB_CMT(6) + VVECT(3) * Z
  Call PSBUFW(" gsave", 6)
  Call MG3DMOVE(X, Y, Z)
  Call PSCONCAT(LAB_CMT)
  Call RotateLabel(Angle)
  Call SETCUR(X, Y)
  Call LABELC(Label, N)
  Call CANROT
  Call PSBUFW(" grestore", 9)
End Sub

Sub MGBOTTOMWLAB(X As Single, Y As Single, Z As Single, Label As String, N As Integer, ORIEN As Integer, Theta As Single, Phi As Single)
'************************************************************************************************************************************
' Write a label on a bottom wall at world coordinates X,Y,Z
  Dim I As Integer
  If (Theta > 0# And Theta < Pi) Then
    For I = 1 To 6
      LAB_CMT(I) = BW_CMT(I)
    Next I
    LAB_CMT(5) = LAB_CMT(5) + UVECT(2) * Y
    LAB_CMT(6) = LAB_CMT(6) + VVECT(2) * Y
    Call PSBUFW(" gsave", 6)
    Call MG3DMOVE(X, Y, Z)
    Call PSCONCAT(LAB_CMT)
    If (ORIEN = 2) Then Call ROTATE(90#)
    If (ORIEN = 3) Then Call ROTATE(180#)
    If (ORIEN = 4) Then Call ROTATE(270#)
    Call SETCUR(X, Z)
    Call LABELC(Label, N)
    If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
    Call PSBUFW(" grestore", 9)
  End If
End Sub

 Sub MGRIGHTWLAB(X As Single, Y As Single, Z As Single, Label As String, N As Integer, ORIEN As Integer, Theta As Single, Phi As Single)
'**********************************************************************************************************************************************
' Write a label on a right wall at world coordinates X,Y,Z
 Dim I As Integer
 If (Theta > 0.5 * Pi And Theta < 1.5 * Pi) Then
   For I = 1 To 6
     LAB_CMT(I) = RW_CMT(I)
   Next I
   LAB_CMT(5) = LAB_CMT(5) + UVECT(1) * X
   LAB_CMT(6) = LAB_CMT(6) + VVECT(1) * X
   Call PSBUFW(" gsave", 6)
   Call MG3DMOVE(X, Y, Z)
   Call PSCONCAT(LAB_CMT)
   If (ORIEN = 2) Then Call ROTATE(90#)
   If (ORIEN = 3) Then Call ROTATE(180#)
   If (ORIEN = 4) Then Call ROTATE(270#)
   Call SETCUR(Y, Z)
   Call LABELC(Label, N)
   If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
   Call PSBUFW(" grestore", 9)
 End If
 End Sub

Sub MGLEFTWLAB(X As Single, Y As Single, Z As Single, Label As String, N As Integer, ORIEN As Integer, Theta As Single, Phi As Single)
'**************************************************************************************************************************
' Write a label on a right wall at world coordinates X,Y,Z
  Dim I As Integer
  If (Theta > 1.5 * Pi Or Theta < 0.5 * Pi) Then
    For I = 1 To 6
      LAB_CMT(I) = LW_CMT(I)
    Next I
    LAB_CMT(5) = LAB_CMT(5) + UVECT(1) * X
    LAB_CMT(6) = LAB_CMT(6) + VVECT(1) * X
    Call PSBUFW(" gsave", 6)
    Call MG3DMOVE(X, Y, Z)
    Call PSCONCAT(LAB_CMT)
    If (ORIEN = 2) Then Call ROTATE(90#)
    If (ORIEN = 3) Then Call ROTATE(180#)
    If (ORIEN = 4) Then Call ROTATE(270#)
    Call SETCUR(-Y, Z)
    Call LABELC(Label, N)
    If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
    Call PSBUFW(" grestore", 9)
  End If
End Sub

Sub MGTOPWLAB(X As Single, Y As Single, Z As Single, Label As String, N As Integer, ORIEN As Integer, Theta As Single, Phi As Single)
'************************************************************************************************************************************
' Write a label on a back wall at world coordinates X,Y,Z
  Dim I As Integer
  If (Theta > Pi) Then
    For I = 1 To 6
      LAB_CMT(I) = TW_CMT(I)
    Next I
    LAB_CMT(5) = LAB_CMT(5) + UVECT(2) * Y
    LAB_CMT(6) = LAB_CMT(6) + VVECT(2) * Y
    Call PSBUFW(" gsave", 6)
    Call MG3DMOVE(X, Y, Z)
    Call PSCONCAT(LAB_CMT)
    If (ORIEN = 2) Then Call ROTATE(90#)
    If (ORIEN = 3) Then Call ROTATE(180#)
    If (ORIEN = 4) Then Call ROTATE(270#)
    Call SETCUR(-X, Z)
    Call LABELC(Label, N)
    If (ORIEN = 2 Or ORIEN = 3 Or ORIEN = 4) Then Call CANROT
    Call PSBUFW(" grestore", 9)
  End If
End Sub

Sub MGFLOORARRO(XM As Single, YM As Single, X As Single, Y As Single, Z As Single)
'******************************************************************************
' Draws a directed arrow from XM,YM,ZM to X,Y,Z
' Arrowhead is scaled to the length of the arrow.
' D1 is the relative head length and D2 is the relative head width.
  Dim d As Single, D1 As Single, D2 As Single
  Dim WX As Single, WY As Single, X1 As Single, Y1 As Single, XB As Single, YB As Single, XT As Single, YT As Single
  If (XM = X And YM = Y) Then Return

  d = Sqr((XM - X) ^ 2 + (YM - Y) ^ 2)
  D1 = 20#
  D2 = 6.6
  WX = (XM - X) / d
  WY = (YM - Y) / d
  X1 = WX * D1 + X
  Y1 = WY * D1 + Y
  XB = X1 - WY * D2
  YB = Y1 + WX * D2
  XT = X1 + WY * D2
  YT = Y1 - WX * D2
  Call MG3DMOVE(XT, YT, Z)
  Call MG3DPLOT(X, Y, Z)
  Call MG3DPLOT(XB, YB, Z)
  Call MG3DPLOT(XT, YT, Z)
  Call MG3DMOVE(XM, YM, Z)
  Call MG3DPLOT(X1, Y1, Z)
End Sub

Sub WALLPLOT(REFX As Single, REFY As Single, REFZ As Single, WY() As Single, WZ() As Single, N As Integer)
'*********************************************************************************************************
'  Plots the arrays WZ and WY as a wall plot
' Locate the left hand foot of the wall
 Dim VERTV(0 To 15) As xyrcoord
 Dim VX As Single, VY As Single, VZ As Single
 Dim X As Single, Y As Single, Z As Single
 Dim I As Integer

 Call VIEWC(REFX, REFY, REFZ, VX, VY, VZ)
 VERTV(0).xcoord = VX
 VERTV(0).ycoord = VY
 For I = 1 To N
   X = REFX
   Y = REFY + WY(I)
   Z = REFZ + WZ(I)
   Call VIEWC(X, Y, Z, VX, VY, VZ)
   VERTV(I).xcoord = VX
   VERTV(I).ycoord = VY
 Next I

' Locate the right hand foot of the wall
 X = REFX
 Y = REFY + WY(N)
 Z = REFZ
 Call VIEWC(X, Y, Z, VX, VY, VZ)
 VERTV(N + 1).xcoord = VX
 VERTV(N + 1).ycoord = VY
 Call MGPOLY(3, VERTV, N + 2)
End Sub
