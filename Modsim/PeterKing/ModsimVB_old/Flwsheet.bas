Attribute VB_Name = "MdlFlwsheet"
Option Explicit
Public Wide As Single
Public High As Single
Public FlowsheetScaleFactor As Single

Public COMFLG As Boolean

Private count(100) As Integer
Private Flag(100, 20) As Boolean
Public Unit(100, 20) As Integer
Private ID(100, 20) As Integer
Private IRR(100, 20) As Single
Private UNTYP(100) As Integer
Private UN_ID(100) As Integer
Public UnitLocX(100, 20) As Single
Public UnitLocY(100, 20) As Single
Public NOSPLIT(100) As Integer
Public NOCELL(20) As Integer
'Public FLOTF As Boolean
'Coordinates of second drum on conveyor
Public CONVEY_X(21) As Single, CONVEY_Y(21) As Single
Public ConveyorDel1X As Single, ConveyorDel2X As Single
Public ConveyorDel1Y As Single, ConveyorDel2Y As Single
Private UNSEG(100) As Integer
Private STRSEG(100) As Integer

Private StreamStart(200) As Integer
Public StreamType(200) As Integer
Private StreamEnd(200) As Integer
Private Str_ID(200) As Integer
Private StartX(200) As Single
Private StartY(200) As Single
Private EndX(200) As Single
Private EndY(200) As Single
Private StreamCorners(200) As Integer
Private CornerX(200, 10) As Single
Private CornerY(200, 10) As Single
Private SFlag(200) As Boolean
Public WFlag(200) As Boolean
Private NOTESG(99) As Integer
Public NOTEX(99) As Single, NOTEY(99) As Single
Public NOTEFL(99) As Boolean
Public NOTE(0 To 99) As String
Private BoxTop As Integer, BoxLeft As Integer

Type Flyout
  ULString As String
  LLString As String
  URString As String
  LRString As String
End Type
Public Flyouts(0 To 99) As Flyout

Public FlyoutX(0 To 99) As Single, FlyoutY(0 To 99) As Single
Public FlyoutAX(0 To 99) As Single, FlyoutAY(0 To 99) As Single
Public FlyoutFlag(0 To 99) As Boolean
Public FlyoutStream(0 To 99) As Integer
Public FlyoutSize As Single
Public FlyoutSizeFactor As Single

Private Left(100) As Integer
Private Right(100) As Integer
Private Top(100) As Integer
Private Bottom(100) As Integer

'Public NUNIT As Integer
Public Stream As Integer
Public UnitNumber As Integer
Public Water As Integer
Public RPFLAG As Integer
Public Unique_ID As Integer
Public UnitNumberFlagOn As Boolean
Public IR As Single
Public NSEL As Integer
Public LABNO As Integer
Public FlyoutNo As Integer


Private JOBNAME As String
Private FlowsheetNumber As Integer

Sub ANNOTE(Label As String, IX As Single, IY As Single)
'******************************************************
  'Records annotations on the flowsheet at the location of the cursor.
  Dim ISEG As Integer
  
  Call MGLABSEG(Label, ISEG, IX, IY, 0, 0)
  LABNO = LABNO + 1
  NOTEFL(LABNO) = True
  NOTEX(LABNO) = IX
  NOTEY(LABNO) = IY
  NOTESG(LABNO) = ISEG
  NOTE(LABNO) = Label
End Sub
Sub Flyout(IX As Single, IY As Single, AX As Single, AY As Single)
'*****************************************************************
'Record a stream flyout
  Dim XT As Single, YT As Single
  Dim I As Integer
  Dim YESNO As Boolean
  Call CoordConv(AX, AY, XT, YT)
  MGPage(0).MouseLocationX = XT
  MGPage(0).MouseLocationY = YT
  Call StreamPick(I, YESNO)
  If MGPage(0).SelectedFlyout = 0 Then
    YESNO = True
    I = 0
  End If
  If YESNO Then
    'Make sure that the upper left is inside the border
    If IY > High - 5 Then IY = High - 5
    If IX > Wide - 5 Then IX = Wide - 12 * FlyoutSize - 5
    MGPage(0).ForeColor = vbBlack
    MGPage(0).DrawMode = 13
    Call DrawFlyout(IX, IY, AX, AY)
    YESNO = False
    FlyoutNo = FlyoutNo + 1
    FlyoutFlag(FlyoutNo) = True
    FlyoutX(FlyoutNo) = IX
    FlyoutY(FlyoutNo) = IY
    FlyoutAX(FlyoutNo) = AX
    FlyoutAY(FlyoutNo) = AY
    FlyoutStream(FlyoutNo) = I
  End If
End Sub
Function FLYOUTPK(IX As Single, IY As Single) As Integer
'*******************************************************
'Picks a flyout
  Dim XCHK As Boolean, YCHK As Boolean
  Dim N As Integer
  For N = FlyoutNo To 0 Step -1
    If FlyoutFlag(N) Then
      XCHK = (FlyoutX(N) <= IX) And (FlyoutX(N) + 12 * FlyoutSize >= IX)
      YCHK = (FlyoutY(N) >= IY) And (FlyoutY(N) - 5 * FlyoutSize <= IY)
      If (XCHK And YCHK) Then
        FLYOUTPK = N
        Exit Function
      End If
    End If
  Next N
  FLYOUTPK = -1
End Function
Sub ARROW(XM As Single, YM As Single, X As Single, Y As Single)
'**************************************************************
' Draws an arrow.
' D1 is the relative head length and D2 is the relative head width.
' X1, Y1 are the coordinates of the intersection of the shaft with the base of the head.
  Dim d As Single, D1 As Single, D2 As Single
  Dim X1 As Single, Y1 As Single
  Dim XT As Single, YT As Single
  Dim XB As Single, YB As Single
  Dim WX As Single, WY As Single
  If (XM = X And YM = Y) Then Exit Sub
  d = Sqr((XM - X) ^ 2 + (YM - Y) ^ 2)
  D1 = 11# * FlowsheetScaleFactor
  D2 = 3.6 * FlowsheetScaleFactor
  WX = (XM - X) / d
  WY = (YM - Y) / d
  X1 = WX * D1 + X
  Y1 = WY * D1 + Y
  XB = X1 - WY * D2
  YB = Y1 + WX * D2
  XT = X1 + WY * D2
  YT = Y1 - WX * D2
  Call MGRMVE(XM, YM)
  Call MGPLOT(X1, Y1)
  Call MGRMVE(XT, YT)
  Call MGPLOT(X, Y)
  Call MGPLOT(XB, YB)
  Call MGPLOT(XT, YT)
End Sub

Sub DrawBorder()
'***************
' Draws a single line border around the flowsheet.

  Call MGRMVE(5, 30)
  Call MGPLOT(Wide - 5, 30)
  Call MGPLOT(Wide - 5, High - 5)
  Call MGPLOT(5, High - 5)
  Call MGPLOT(5, 30)
End Sub

Sub LocateStreamNumber(Stream As Integer, X As Single, Y As Single)
'******************************************************************
  'Locates a stream number at the center of the longest section of the stream.
  Dim d As Single, DMax As Single
  Dim XM As Single, YM As Single
  Dim I As Integer
  DMax = 0
  If StreamCorners(Stream) = 0 Then
    'Stream has one segment
    If StartX(Stream) = EndX(Stream) Then
      'Stream is vertical
      X = 0.5 * (StartX(Stream) + EndX(Stream)) + 2
      Y = 0.5 * (StartY(Stream) + EndY(Stream))
    ElseIf StartY(Stream) = EndY(Stream) Then
      'Stream is horizontal
      X = 0.5 * (StartX(Stream) + EndX(Stream)) - 5
      Y = 0.5 * (StartY(Stream) + EndY(Stream))
    Else
      'Stream is diagonal
      X = 0.5 * (StartX(Stream) + EndX(Stream)) + 2
      Y = 0.5 * (StartY(Stream) + EndY(Stream)) - 2
    End If
  Else
    'Stream has corners
    XM = StartX(Stream)
    YM = StartY(Stream)
    For I = 1 To StreamCorners(Stream)
      d = (CornerX(Stream, I) - XM) ^ 2 + (CornerY(Stream, I) - YM) ^ 2
      If (d > DMax) Then
        X = 0.5 * (CornerX(Stream, I) + XM) - 2
        Y = 0.5 * (CornerY(Stream, I) + YM)
        If CornerX(Stream, I) = XM Then
          'Segment is vertical
          X = 0.5 * (CornerX(Stream, I) + XM) + 2
          Y = 0.5 * (CornerY(Stream, I) + YM)
        ElseIf CornerY(Stream, I) = YM Then
          'Segment is horizontal
          X = 0.5 * (CornerX(Stream, I) + XM) - 2
          Y = 0.5 * (CornerY(Stream, I) + YM)
        Else
          'Segment is diagonal
          X = 0.5 * (CornerX(Stream, I) + XM) + 2
          Y = 0.5 * (CornerY(Stream, I) + YM) - 2
        End If
        DMax = d
      End If
      XM = CornerX(Stream, I)
      YM = CornerY(Stream, I)
    Next I
    d = (EndX(Stream) - XM) ^ 2 + (EndY(Stream) - YM) ^ 2
    If (d > DMax) Then
      X = 0.5 * (EndX(Stream) + XM) - 2
      Y = 0.5 * (EndY(Stream) + YM)
      If EndX(Stream) = XM Then
        'Segment is vertical
        X = 0.5 * (EndX(Stream) + XM) + 2
        Y = 0.5 * (EndY(Stream) + YM)
      ElseIf EndY(Stream) = YM Then
        'Segment is horizontal
        X = 0.5 * (EndX(Stream) + XM) - 2
        Y = 0.5 * (EndY(Stream) + YM)
      Else
        'Segment is diagonal
        X = 0.5 * (EndX(Stream) + XM) + 2
        Y = 0.5 * (EndY(Stream) + YM) - 2
      End If
      DMax = d
    End If
  End If
End Sub

Sub PaintUnit(I As Integer, J As Integer)
'****************************************
'Paints unit I,J during a unit move
  UnitNumberFlagOn = False
  MGPage(0).PaintFlag = True
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 7
  MGPage(0).PolygonType = 1
  Call DrawUnit(I, UnitLocX(I, J), UnitLocY(I, J))
  MGPage(0).PolygonType = 5
  MGPage(0).PaintFlag = False
End Sub
Sub PaintFlyout(N As Integer)
'****************************
'Paints a flyout during a move
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 7
  MGPage(0).PaintFlag = True
  Call DrawFlyout(FlyoutX(N), FlyoutY(N), FlyoutAX(N), FlyoutAY(N))
  MGPage(0).PaintFlag = False
End Sub
Sub ReconstructStream(P As Integer, J As Integer)
'************************************************
' Reconstruct stream P up to corner J.
  Dim I As Integer, ISEG As Integer
  If StreamStart(P) = 0 Then
    MGPage(0).FillStyle = 1
    Call MGCIRCLE(StartX(P), StartY(P), 5)
  End If
  Call MGRMVE(StartX(P), StartY(P))
  For I = 1 To J
    Call MGPLOT(CornerX(P, I), CornerY(P, I))
  Next I
End Sub

Sub Corner(X As Single, Y As Single)
'***********************************
  Dim SectionLength As Single
  SectionLength = Sqr((X - CornerX(Stream, StreamCorners(Stream))) ^ 2 + (Y - CornerY(Stream, StreamCorners(Stream))) ^ 2)
  If SectionLength = 0 Then Exit Sub
  StreamCorners(Stream) = StreamCorners(Stream) + 1
  CornerX(Stream, StreamCorners(Stream)) = X
  CornerY(Stream, StreamCorners(Stream)) = Y
  Call MGPLOT(X, Y)
End Sub

Sub DeleteStream(P As Integer)
'*****************************
' Deletes stream P
  Dim M As Integer, N As Integer
  
' If stream no longer exists do nothing
  If Not SFlag(P) Then Exit Sub
  
  JobSystemDataNeeded = True
  
' Check whether stream exits from a splitter
  Call FindUnit(StreamStart(P), M, N)
  If M = 16 Then NOSPLIT(N) = NOSPLIT(N) - 1
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 13
  RedrawStream P
  StreamStart(P) = 0
  StreamEnd(P) = 0
  SFlag(P) = False
  WFlag(P) = False
  'Delete any attached flyout
  N = FindFlyout(P)
  If N > 0 Then
    Call DeleteFlyout(N)
  End If
End Sub
 
Sub DRAWMIXER(X As Single, Y As Single)
'**************************************
  Dim ISS As Single
  ISS = Abs(IR)
  Call MGCIRCLE(X, Y, 2 * ISS)
  If UnitNumberFlagOn Then Call Number(X, Y, 1, -2, UnitNumber)
End Sub
 
Sub DrawMixerStream(X As Single, Y As Single)
'*******************************************
' Insert a mixer into an existing stream.
  Dim I As Integer, J As Integer, K As Integer, N As Integer
  Dim IX As Single, IY As Single, R As Single
  Dim XM As Single, YM As Single
  Dim XT As Single, YT As Single
  Dim ISS As Single
  Dim YESNO As Boolean
  Dim ISEG As Integer
  
  ISS = Abs(IR)
' Identify the stream.
  For I = 1 To Stream
    Call SREGION(I, J, YESNO)
    If YESNO And SFlag(I) Then Exit For
  Next I
  If I > Stream Then
    Beep
    MGPage(0).MixerFlag = False
    Exit Sub
  Else
    'Erase the entire stream from the flowsheet and the PS buffer
    MGPage(0).ForeColor = MGPage(0).BackColor
    MGPage(0).DrawMode = 7
    RedrawStream I
    'Delete any attached flyout
    N = FindFlyout(I)
    If N > 0 Then
      Call DeleteFlyout(N)
    End If
    
    'Reconstruct the stream to the identified corner
    MGPage(0).ForeColor = vbBlack
    MGPage(0).DrawMode = 13
    ReconstructStream I, J
    
    ' Attach the stream to the mixer and end it.
    Call ConvertCoordFromScreen(MGPage(0).CurrentX, MGPage(0).CurrentY, XM, YM)
    IX = MGPage(0).CursorLocationX
    IY = MGPage(0).CursorLocationY
    R = Sqr((IX - XM) ^ 2 + (IY - YM) ^ 2)
    If R <= 2.2 * ISS Then
      MsgBox "Selected point should not be inside the mixer", vbExclamation, "WARNING"
      IX = X
      IY = Y
    Else
      IX = XM + (R - 2.2 * ISS) * (IX - XM) / R
      IY = YM + (R - 2.2 * ISS) * (IY - YM) / R
    End If
    Call ARROW(XM, YM, IX, IY)
    COMFLG = True
  End If
  
  'Start the stream that leaves the mixer.
  Stream = Stream + 1
  StreamStart(Stream) = NUNIT
  StreamEnd(Stream) = StreamEnd(I)
  StreamEnd(I) = NUNIT
  EndX(Stream) = EndX(I)
  EndY(Stream) = EndY(I)
  EndX(I) = IX
  EndY(I) = IY
  SFlag(Stream) = True
  XT = EndX(Stream)
  YT = EndY(Stream)
  StreamCorners(Stream) = StreamCorners(I) - J
  StreamCorners(I) = J
  Call STRNUM(I)
  If StreamCorners(Stream) > 0 Then
    For K = 1 To StreamCorners(Stream)
      CornerX(Stream, K) = CornerX(I, J + K)
      CornerY(Stream, K) = CornerY(I, J + K)
    Next K
    XT = CornerX(Stream, 1)
    YT = CornerY(Stream, 1)
  End If
  R = Sqr((XT - X) ^ 2 + (YT - Y) ^ 2)
  Do While R <= 2.2 * ISS
    'First corner is inside the mixer. Eliminate it
    StreamCorners(Stream) = StreamCorners(Stream) - 1
    XT = EndX(Stream)
    YT = EndY(Stream)
    If StreamCorners(Stream) > 0 Then
      For K = 1 To StreamCorners(Stream)
        CornerX(Stream, K) = CornerX(Stream, K + 1)
        CornerY(Stream, K) = CornerY(Stream, K + 1)
      Next K
      XT = CornerX(Stream, 1)
      YT = CornerY(Stream, 1)
    End If
  R = Sqr((XT - X) ^ 2 + (YT - Y) ^ 2)
  Loop
  IX = XT - (R - 2.2 * ISS) * (XT - X) / R
  IY = YT - (R - 2.2 * ISS) * (YT - Y) / R
  Call MGRMVE(IX, IY)
  StartX(Stream) = IX
  StartY(Stream) = IY
  XM = IX
  YM = IY
  If StreamCorners(Stream) > 0 Then
    For K = 1 To StreamCorners(Stream)
      XM = CornerX(Stream, K)
      YM = CornerY(Stream, K)
      Call MGPLOT(XM, YM)
    Next K
  End If
  Call ARROW(XM, YM, EndX(Stream), EndY(Stream))
  If StreamEnd(Stream) = 0 Then
    MGCIRCLE EndX(Stream), EndY(Stream), 5
  End If
  Call STRNUM(Stream)
  'Call MGCSEG
  'Attach the unique ID to the stream
  Str_ID(Stream) = Unique_ID
  Unique_ID = Unique_ID + 1
End Sub


Sub DrawUnit(Selection As Integer, X As Single, Y As Single)
'***********************************************************
' Draws a unit of type Selection at location X, Y
  Select Case Selection
    Case 1
      Call DrawSpiralConcentrator(X, Y)
    Case 2
      Call DrawDrewboy(X, Y)
    Case 6
      Call DrawFlotationCell(X, Y)
    Case 8
      Call DrawHydrocyclone(X, Y)
    Case 9
      Call DrawMixerStream(X, Y)
    Case 10
      Call DrawShakingTable(X, Y)
    Case 11
      Call DrawWetDrumMagSeparator(X, Y)
    Case 12
      Call DrawFilter(X, Y)
    Case 13
      Call DrawGrindingMill(X, Y)
    Case 14
      Call DrawScreen(X, Y)
    Case 15
      Call DrawPuddlePan(X, Y)
    Case 16
      Call DrawStreamSplitter(X, Y)
    Case 17
      Call DrawPump(X, Y)
    Case 18
      Call DrawReichertCone(X, Y)
    Case 20
      Call DrawThickener(X, Y)
    Case 21
      Call DrawCrusher(X, Y)
    Case 22
      Call DrawSump(X, Y)
    Case 24
      Call DRAWMIXER(X, Y)
    Case 25
      Call DrawWhims(X, Y)
    Case 36
      Call DrawShallowBath(X, Y)
    Case 38
      Call DrawDenseMediumCyclone(X, Y)
    Case 39
      Call DrawJawCrusher(X, Y)
    Case 40
      Call DrawDoubleDeckScreen(X, Y)
    Case 41
      Call DrawDynaWhirlpool(X, Y)
    Case 42
      Call DrawBaumJig(X, Y)
    Case 43
      Call DrawBatacJig(X, Y)
    Case 44
      Call DrawNorwaltWasher(X, Y)
    Case 45
      Call DrawWemcoDrum(X, Y)
    Case 46
      Call DrawTeskaDrum(X, Y)
    Case 47
      Call DrawChanceSandBath(X, Y)
    Case 48
      Call DrawSegregatingBin(X, Y)
    Case 49
      Call DrawBlackBox(X, Y)
    Case 51
      Call DrawDenseMediumVessel(X, Y)
    Case 52
      Call DrawAutogenousMill(X, Y)
    Case 53
      Call DrawRodMill(X, Y)
    Case 54
      Call DrawGyratoryCrusher(X, Y)
    Case 55
      Call DrawStockpile(X, Y)
    Case 56
      Call DrawWateronlyCyclone(X, Y)
    Case 57
      Call DrawDewateringScreen(X, Y)
    Case 58
      Call DrawSieveBend(X, Y)
    Case 59
      Call DrawFixedRollMill(X, Y)
    Case 60
      Call DrawHPRollMill(X, Y)
    Case 61
      Call DrawElutriator(X, Y)
    Case 62
      Call DrawConveyor(X, Y)
    Case 63
      Call DrawSpiralClassifier(X, Y)
    Case 64
      Call DrawKNELSON(X, Y)
    Case 65
      Call DrawAutogenousMillWithTrommel(X, Y)
    Case 66
      Call DrawWaterInjectionCyclone(X, Y)
    Case 67
      Call DrawCompoundScreen(X, Y)
    Case 68
      Call DrawFlotationColumn(X, Y)
    Case 69
      Call DrawJones(X, Y)
  End Select
End Sub

Sub EndStream(XM As Single, YM As Single, X As Single, Y As Single)
'******************************************************************
  Dim I As Integer
  Dim J As Integer
  Dim N As Integer
  Dim YESNO As Boolean
  Dim XT As Single, YT As Single
  
  Call ARROW(XM, YM, X, Y)
  EndX(Stream) = X
  EndY(Stream) = Y
  Call STRNUM(Stream)
  ' Check for new stream and set the unique ID if necessary
  If RPFLAG = 1 Then
    Str_ID(Stream) = Unique_ID
    Unique_ID = Unique_ID + 1
  End If
  'Set the mouse location for use by REGION
  Call CoordConv(X, Y, XT, YT)
  MGPage(0).MouseLocationX = XT
  MGPage(0).MouseLocationY = YT
  Call REGION(I, J, YESNO)
  If YESNO Then
    StreamEnd(Stream) = Unit(I, J)
  Else
    StreamEnd(Stream) = 0
    If RPFLAG = 1 Then Beep
    MGPage(0).FillStyle = 1
    Call MGCIRCLE(X, Y, 5 * FlowsheetScaleFactor)
  End If
  'Call MGCSEG
  MGPage(0).WaterStreamFlagOn = False
  COMFLG = True
  If RPFLAG = 1 Then JobSystemDataNeeded = True
  
  'Check for multiple feed streams to UNIT(I,J)
  If YESNO And Not WFlag(Stream) Then
    Select Case I
      Case 9, 22, 24, 55, 62
        'Ignore Mixers, Sumps, Stockpiles, Conveyors
      Case Else
      For N = 1 To Stream - 1
        If Not WFlag(N) And SFlag(N) And StreamEnd(N) = Unit(I, J) Then
          WSTRING = "Unit " & CStr(Unit(I, J)) & " already has a feed stream. Stream " & CStr(Stream) & " will therefore be deleted."
          WSTRING = WSTRING & Chr(13) & Chr(10) & "Use a mixer ahead of this unit."
          WSTRING = WSTRING & Chr(13) & Chr(10) & "If no feed stream appears on the flowsheet, refresh the flowsheet and try again."
          MsgBox WSTRING, 48, "ERROR"
          Call DeleteStream(Stream)
        End If
      Next N
    End Select
  End If
End Sub

Sub FindUnit(UNITNO As Integer, I As Integer, J As Integer)
'**********************************************************
  ' Find I and J in the array UNIT(I,J) for unit number UNITNO
  For I = 1 To 100
    If count(I) > 0 Then
      For J = 1 To 20
        If UNITNO = Unit(I, J) Then Exit Sub
      Next J
    End If
  Next I
  I = 0
  J = 0
End Sub
Function FindFlyout(Stream As Integer) As Integer
'************************************************
'Finds the flyout that is linked to stream
  Dim I As Integer
  FindFlyout = 0
  For I = 1 To FlyoutNo
    If FlyoutStream(I) = Stream Then
      FindFlyout = I
      Exit For
    End If
  Next I
End Function
Sub MoveUnit(I As Integer, J As Integer)
'***************************************
' Moves a unit icon.
  Dim X As Single, Y As Single
  MGPage(0).DrawMode = 7
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).MovingFlag1 = True
  MGPage(0).PolygonType = 1
  Call DrawUnit(I, UnitLocX(I, J), UnitLocY(I, J))
  Call ConvertCoordFromScreen(MGPage(0).MouseLocationX, MGPage(0).MouseLocationY, X, Y)
  MGPage(0).MovingFlag1 = False
  MGPage(0).MovingFlag2 = True
  Call DrawUnit(I, X, Y)
  MGPage(0).MovingFlag2 = False
  MGPage(0).PolygonType = 5
  'The individual unit drawing routines can change the
  'arguments X and Y cf,DrawConveyor
  UnitLocX(I, J) = X
  UnitLocY(I, J) = Y
End Sub
Sub MoveFlyout(N As Integer)
'***************************
'Moves a flyout
  Call PaintFlyout(N)
  Call ConvertCoordFromScreen(MGPage(0).MouseLocationX, MGPage(0).MouseLocationY, FlyoutX(N), FlyoutY(N))
  Call PaintFlyout(N)
End Sub
Sub RedrawStream(P As Integer)
'*****************************
  Dim I As Integer
  Dim XM As Single, YM As Single
  If StreamStart(P) <= 0 Then
    MGPage(0).FillStyle = 1
    Call MGCIRCLE(StartX(P), StartY(P), 5 * FlowsheetScaleFactor)
  End If
  Call MGRMVE(StartX(P), StartY(P))
  If StreamCorners(P) > 0 Then
    For I = 1 To StreamCorners(P)
      Call MGPLOT(CornerX(P, I), CornerY(P, I))
    Next I
  End If
  Call ConvertCoordFromScreen(MGPage(0).CurrentX, MGPage(0).CurrentY, XM, YM)
  Call ARROW(XM, YM, EndX(P), EndY(P))
  If StreamEnd(P) = 0 Then
    MGPage(0).FillStyle = 1
    Call MGCIRCLE(EndX(P), EndY(P), 5 * FlowsheetScaleFactor)
  End If
  Call STRNUM(P)
End Sub

Sub AcceptFlowsheet()
'********************
  Dim I As Integer, J As Integer, L As Integer
  Dim response As Integer
  
  On Error GoTo ErrHandler
  
  'Check that each unit has a feed.
  For I = 1 To 100
    For J = 1 To count(I)
      For L = 1 To Stream
        If SFlag(L) Then
          If StreamEnd(L) = Unit(I, J) Then Exit For
        End If
      Next L
      If L = Stream + 1 Then
        response = MsgBox("Unit " & CStr(Unit(I, J)) & " has no feed stream", vbOKCancel, "WARNING")
        If response = vbCancel Then
          Exit Sub
        End If
      End If
    Next J
  Next I
  'Check that each unit has at least one product
  For I = 1 To 100
    For J = 1 To count(I)
      For L = 1 To Stream
        If SFlag(L) Then
          If StreamStart(L) = Unit(I, J) Then Exit For
        End If
      Next L
      If L = Stream + 1 Then
        response = MsgBox("Unit " & CStr(Unit(I, J)) & " has no product stream", vbOKCancel, "WARNING")
        If response = vbCancel Then
          Exit Sub
        End If
      End If
    Next J
  Next I
  'Check that any water injection cyclones have a water feed stream.
  For J = 1 To count(66)
    For L = 1 To Stream
      If WFlag(L) Then
        If StreamEnd(L) = Unit(66, J) Then Exit For
      End If
    Next L
    If L = Stream + 1 Then
      response = MsgBox("Unit " & CStr(Unit(66, J)) & " is a water injection cyclone and has no water feed!", vbOKCancel, "WARNING")
      If response = vbCancel Then
        Exit Sub
      End If
    End If
  Next J
  'Check that any Jones separator has a water feed stream.
  For J = 1 To count(69)
    For L = 1 To Stream
      If WFlag(L) Then
        If StreamEnd(L) = Unit(69, J) Then Exit For
      End If
    Next L
    If L = Stream + 1 Then
      response = MsgBox("Unit " & CStr(Unit(69, J)) & " is a Jones magnetic separator and has no water feed!", vbOKCancel, "WARNING")
      If response = vbCancel Then
        Exit Sub
      End If
    End If
  Next J
  
  JobFlowsheet = True
  'Print the flowsheet data to a file for re-entry
  Open RTrim(JobPath) & "PLNTR.OUT" For Output As #14
  If MODSIM.JOBNAME = "" Then
    MODSIM.JOBNAME = InputBox("Specify a job name")
    MODSIM.Caption = TITLE & MODSIM.JOBNAME
  End If
  JOBNAME = MODSIM.JOBNAME
  Print #14, FlowsheetNumber; Len(JOBNAME); MGPage(0).Iconsize; Unique_ID
  Print #14, MODSIM.JOBNAME
  Print #14, NUNIT; Stream; Water
  For J = 0 To 4
    For I = 1 To 19
      Print #14, count(J * 20 + I);
    Next I
    Print #14, count((J + 1) * 20)
  Next J
  If Stream > 0 Then
    For I = 1 To Stream
      Print #14, BriefFlag(SFlag(I));
    Next I
    Print #14,
  End If
  If Water > 0 Then
    For I = 1 To Stream
      Print #14, BriefFlag(WFlag(I));
    Next I
    Print #14,
  End If
  For I = 1 To 100
    If count(I) > 0 Then
      Print #14, I;
      For J = 1 To count(I)
        Print #14, BriefFlag(Flag(I, J));
      Next J
      Print #14,
      For J = 1 To count(I)
        If Flag(I, J) Then
          Print #14, Unit(I, J); UnitLocX(I, J); UnitLocY(I, J); IRR(I, J); ID(I, J)
          If I = 6 Then Print #14, NOCELL(J)
          If I = 62 Then Print #14, CONVEY_X(J), CONVEY_Y(J)
          UNTYP(Unit(I, J)) = I
          UN_ID(Unit(I, J)) = ID(I, J)
        End If
      Next J
    End If
  Next I
  If Stream > 0 Then
    For I = 1 To Stream
      If SFlag(I) Then
        Print #14, StreamCorners(I)
        Print #14, StartX(I), StartY(I), EndX(I), EndY(I), Str_ID(I)
        For J = 1 To StreamCorners(I)
          Print #14, CornerX(I, J), CornerY(I, J)
        Next J
      End If
    Next I
  End If
  ' ..and now the annotations
  Print #14, LABNO
  If LABNO > 0 Then
    For I = 1 To LABNO
      Print #14, BriefFlag(NOTEFL(I));
    Next I
    Print #14,
    For I = 1 To LABNO
      If NOTEFL(I) Then
        Print #14, NOTEX(I), NOTEY(I)
        Print #14, NOTE(I)
      End If
    Next I
  End If
  '...and now the flyouts
  Print #14, FlyoutNo
  If FlyoutNo >= 0 Then
    For I = 0 To FlyoutNo
      Print #14, BriefFlag(FlyoutFlag(I));
    Next I
    Print #14,
    For I = 0 To FlyoutNo
      If FlyoutFlag(I) Then
        Print #14, FlyoutX(I), FlyoutY(I), FlyoutAX(I), FlyoutAY(I)
      End If
    Next I
  End If
  Print #14, LabelFont.Name
  Print #14, LabelFont.size
  
  'Record the position of the control box
  BoxLeft = MGPage(0).frameBoxes.Left * FlowsheetScaleFactor
  BoxTop = MGPage(0).frameBoxes.Top * FlowsheetScaleFactor
  Print #14, BoxLeft, BoxTop
  
  'Record the flyout size factor
  Print #14, FlyoutSizeFactor
  
  'Record that flowsheet has changed
  ChangedFlowsheet = True
  If MODSIM.EditFlowsheetFlag Then
    NewFlowsheet = True
  End If
  SavedJob = False
  Close #14
  
  'Write the flowsheet data for transmission to the system data module
  Call WriteSystemData
  Call WriteModelData
  Definedflowsheet = True
  MGPage(0).NewFlowsheet = False
  MGPage(0).Cls
  Unload MGPage(0)
  
  MODSIM.Show
  Exit Sub
  
ErrHandler:
  MsgBox "Error when writing flowsheet data file", 16, "ERROR"
  Close #14
  Exit Sub
End Sub

Sub ReplaceNote(I As Integer, X As Single, Y As Single)
'******************************************************
  'Replaces a note on the flowsheet after a move
  Dim ISEG As Integer
  Dim XT As Single, YT As Single
  Call ConvertCoordFromScreen(0, MGPage(0).TextHeight("M"), XT, YT)
  Call MGLABSEG(NOTE(I), ISEG, X, Y, 0, 0)
  NOTEFL(I) = True
  NOTESG(I) = ISEG
  NOTEX(I) = X
  NOTEY(I) = Y
End Sub
Sub ReplaceFlyout(N As Integer, X As Single, Y As Single)
'********************************************************
' Replace a flyout after moving
  MGPage(0).ForeColor = vbBlack
  MGPage(0).DrawMode = 13
  Call DrawFlyout(X, Y, FlyoutAX(N), FlyoutAY(N))
  FlyoutX(N) = X
  FlyoutY(N) = Y
  FlyoutFlag(N) = True
End Sub
Sub SelectUnit(Selection As Integer, ISS As Single, Reflectflag As Boolean)
'**************************************************************************
  ' This is a common routine to select a unit from the flowsheet menu.
  
  If count(Selection) = 19 Then
     MsgBox "No more icons of this type may be used after this one"
     'Put some code here to disable the menu item for this unit type
  End If
  If count(Selection) > 19 Then
    MsgBox "20 icons of this type have already been used." & vbCrLf & "No more are available", vbCritical, " ERROR"
    Exit Sub
  End If
  RecordUnit Selection, MGPage(0).CursorLocationX, MGPage(0).CursorLocationY, ISS, Reflectflag
  UnitNumber = NUNIT
  MGPage(0).DrawMode = 13
  Call MGGRAYFILL(0.9)
  MGPage(0).PolygonType = 5
  MGPage(0).ForeColor = vbBlack
  DrawUnit Selection, MGPage(0).CursorLocationX, MGPage(0).CursorLocationY
  MGPage(0).Reflectflag = False
  If RPFLAG = 1 Then JobUnitParametersNeeded = True
 ' MGPage(0).DrawMode = 7
  If RPFLAG = 1 Then MGPage(0).LocCursor_Click
End Sub

Sub SREGION(P As Integer, J As Integer, YESNO As Boolean)
'********************************************************
' Finds whether the mouse is within a distance of 5 from
' a start, corner or end of stream P. J is the number of the identified corner.
  Dim IX As Single
  Dim IY As Single
  Dim I As Integer
  YESNO = False
  Call ConvertCoordFromScreen(MGPage(0).MouseLocationX, MGPage(0).MouseLocationY, IX, IY)
  For P = 1 To Stream
    If SFlag(P) Then
      If Abs(EndX(P) - IX) < 5 And Abs(EndY(P) - IY) < 5 Then
        J = StreamCorners(P) + 1
        YESNO = True
        Exit Sub
      End If
      If StreamCorners(P) > 0 Then
        For I = 1 To StreamCorners(P)
          If Abs(CornerX(P, I) - IX) < 5 And Abs(CornerY(P, I) - IY) < 5 Then
            J = I
            YESNO = True
            Exit Sub
          End If
        Next I
      End If
      If Abs(StartX(P) - IX) < 5 And Abs(StartY(P) - IY) < 5 Then
        J = 0
        YESNO = True
        Exit Sub
      End If
    End If
  Next P
End Sub

Sub StartFlowsheet()
'*******************
' This subroutine starts the flowsheet editor.
  Dim NoType As Integer
  Dim M As Integer, Mo As Integer
  Dim I As Integer, J As Integer, K As Integer
  Dim Str As String * 2
  Dim Lp As Long, lx As Long, ly As Long, lpsize As size
  
  'Set the Metgraph page to MGPage(0) by setting PageNumber to 0
  PageNumber = 0
  'This load is executed only on first load not every
  'time the flowsheet is started.
  Load MGPage(0)
  FlowsheetScaleFactor = 11715 / MODSIM.ScaleWidth '11715 is the width of the "standard" window

  MGPage(0).Cls
  'Set the font for the flowsheet page
  'LabelFont.Name = "MS Sans Serif"
  'LabelFont.size = 9
  LabelFont.Name = "Arial"
  LabelFont.size = 8
  LabelFont.Bold = False
  LabelFont.italic = False
  Call MGFONT(LabelFont)
  Flyoutfont.Name = "Arial"
  Flyoutfont.size = 8
  Flyoutfont.italic = False
  Flyoutfont.Bold = False
  MGPage(0).FontTransparent = False
  Call LBSIZE(9, 18)
  MGPage(0).KeyPreview = True
  MGPage(0).Top = 0
  MGPage(0).Left = 0
  MGPage(0).width = MODSIM.ScaleWidth
  MGPage(0).height = MODSIM.ScaleHeight
  High = 780
  Wide = 780# * MODSIM.ScaleWidth / MODSIM.ScaleHeight
'  MGPage(0).Scale (0, High)-(Wide, 0)
'  lx = Wide
'  ly = High
'  Lp = SetWindowExtEx(MGPage(0).hdc, lx, ly, lpsize)
'  MsgBox CStr(Lp) & " " & CStr(lpsize.cx) & " " & CStr(lpsize.cy) & "    " & CStr(lx) & " " & CStr(ly)
  MGPage(0).Show

  MGPage(0).StepLength = 1
  
  COMFLG = True
  Call MGPage(0).ResetFlags
  If MGPage(0).NewFlowsheet Then
    'Turn the location cursor on and display it
    MGPage(0).CursorLocationX = 500
    MGPage(0).CursorLocationY = 500
    Call MGPage(0).DrawCursor(500, 500)
    MGPage(0).LocCursorOn = True
    MGPage(0).StatusBar1.Panels(1).text = "Location cursor is active"
    MGPage(0).ChkUnitNumbers.Enabled = False
    MGPage(0).ChkStreamNumbers.Enabled = False
    MGPage(0).ChkShowFlyouts.Enabled = False
  End If
  
 UnitNumberFlagOn = True
  COMFLG = True
  'Initialize the counters
  NUNIT = 0
  For I = 1 To 100
    count(I) = 0
    For J = 1 To 20
      Flag(I, J) = False
      Unit(I, J) = 0
      ID(100, 20) = 0
      IRR(I, J) = 0
    Next J
  Next I
  For I = 1 To 100
    NOSPLIT(I) = 0
  Next I
  For I = 1 To 20
    NOCELL(I) = 0
  Next I
  Stream = 0
  Water = 0
  For I = 1 To 200
    StreamCorners(I) = 0
  Next I
  LABNO = 0
  FlyoutNo = -1
  FlyoutSize = FlyoutSizeFactor * 10# * FlowsheetScaleFactor
  IR = 5
  MGPage(0).Iconsize = IR
  MGPage(0).Reflectflag = False
  ' Get data about icons from file PLANT.DAT
  Open RTrim(FileReadPath) + "PLANT.DAT" For Input As #12
  Input #12, NoType
  For K% = 1 To NoType
    Str = Input(2, #12)
    I = Val(Str)
    Str = Input(2, #12)
    Left(I) = Val(Str)
    Str = Input(2, #12)
    Right(I) = Val(Str)
    Str = Input(2, #12)
    Top(I) = Val(Str)
    Str = Input(2, #12)
    Bottom(I) = Val(Str)
    Line Input #12, WSTRING$
    Input #12, Mo
    For M% = 1 To Mo
      Line Input #12, WSTRING$
    Next M%
  Next K%
  Close #12
  
  ' Initialize PostScript and the printer and metafile DCs.
  PSFlagOn = False
  'Dont write to the printer object...
  PrinterFlagOn = False
  '...nor to the metafile.
  MetaFileFlagOn = False
  
  Call DrawBorder
  MGPage(0).SelectedFlyout = -1
  If MGPage(0).NewFlowsheet Then
    FlowsheetNumber = 1
    MGPage(0).Iconsize = IR
    RPFLAG = 1
    Unique_ID = 1
    'Draw the reference flyout
    MGPage(0).SelectedFlyout = 0
    Call Flyout(1020, 100, 10, 10)
    MGPage(0).SelectedFlyout = -1
    COMFLG = True
  Else
    Call ReadFlowsheet
    MGPage(0).frameBoxes.Top = BoxTop / FlowsheetScaleFactor
    MGPage(0).frameBoxes.Left = BoxLeft / FlowsheetScaleFactor
    RPFLAG = 1
  End If

End Sub

Sub RecordUnit(Selection As Integer, X As Single, Y As Single, ISS As Single, Reflectflag As Boolean)
'****************************************************************************************************
  Dim ISEG As Integer
  NUNIT = NUNIT + 1
  count(Selection) = count(Selection) + 1
  NSEL = count(Selection)
  UnitLocX(Selection, NSEL) = X
  UnitLocY(Selection, NSEL) = Y
  Unit(Selection, NSEL) = NUNIT
  Flag(Selection, NSEL) = True
  If Reflectflag Then
    IRR(Selection, NSEL) = -ISS
  Else
    IRR(Selection, NSEL) = ISS
  End If
  IR = IRR(Selection, NSEL)
  
  ' Check for new units
  If (RPFLAG = 1) Then
     ID(Selection, NSEL) = Unique_ID
     Unique_ID = Unique_ID + 1
  End If

  ' Check for incomplete stream and redraw if necessary
  If Not COMFLG Then
    Call DeleteStream(Stream)
  End If
End Sub

Sub ReplaceUnit(I As Integer, J As Integer)
'******************************************
' Replace a unit after it has been moved.
  UnitNumberFlagOn = True
  MGPage(0).DrawMode = 13
  MGPage(0).ForeColor = vbBlack
  Call MGGRAYFILL(0.9)
  MGPage(0).PolygonType = 5
  MGPage(0).ReplaceFlag = True
  Call DrawUnit(I, UnitLocX(I, J), UnitLocY(I, J))
  MGPage(0).ReplaceFlag = False
  Flag(I, J) = True
 End Sub
 
 Sub ReflectExistingUnit(I As Integer, J As Integer)
 '**************************************************
   Call DeleteUnit(I, J)
   IRR(I, J) = -IRR(I, J)
   IR = IRR(I, J)
   Call ReplaceUnit(I, J)
   MGPage(0).StatusBar1.Panels(1).text = ""
 End Sub

Sub REGION(I As Integer, J As Integer, YESNO As Boolean)
'*******************************************************
' Finds the unit that contains the cursor
  Dim IS1 As Single
  Dim IX As Single
  Dim IY As Single
  Dim A As Single
  Dim B As Single
  Dim c As Single
  Dim d As Single
  Dim lx As Boolean
  Dim ly As Boolean
  Dim WS As Single, Slope As Single, DELX As Single, DELY As Single
  Dim UnitNumber As Integer
  Dim XT As Single, YT As Single
  
  XT = MGPage(0).MouseLocationX
  YT = MGPage(0).MouseLocationY
  Call ConvertCoordFromScreen(XT, YT, IX, IY)
  YESNO = False
  For UnitNumber = NUNIT To 1 Step -1
    Call FindUnit(UnitNumber, I, J)
    If Flag(I, J) Then
      IS1 = Abs(IRR(I, J))
      ' Conveyor belts are variable
      If I = 62 Then
        WS = Abs(UnitLocX(I, J) - CONVEY_X(J)) + 6 * IS1
        lx = Abs(IX - UnitLocX(I, J)) <= WS
        lx = lx And (Abs(IX - CONVEY_X(J)) <= WS)
        Slope = (UnitLocY(I, J) - CONVEY_Y(J)) / (UnitLocX(I, J) - CONVEY_X(J))
        DELY = 3.2 * IS1 / Sqr(1# + Slope * Slope)
        DELX = -Slope * DELY
        ly = (IY - (UnitLocY(I, J) + DELY)) <= (IX - (UnitLocX(I, J) + DELX)) * Slope And (IY - (UnitLocY(I, J) - DELY)) > (IX - (UnitLocX(I, J) - DELX)) * Slope
        If Abs(IX - UnitLocX(I, J)) <= 2 * IS1 And Abs(IY - UnitLocY(I, J)) <= 2 * IS1 Then
          MGPage(0).FirstDrumFlag = True
          MGPage(0).SecondDrumFlag = False
        ElseIf Abs(IX - CONVEY_X(J)) <= 2 * IS1 And Abs(IY - CONVEY_Y(J)) <= 2 * IS1 Then
          MGPage(0).FirstDrumFlag = False
          MGPage(0).SecondDrumFlag = True
        Else
          MGPage(0).FirstDrumFlag = False
          MGPage(0).SecondDrumFlag = False
          ConveyorDel1X = IX - UnitLocX(I, J)
          ConveyorDel1Y = IY - UnitLocY(I, J)
          ConveyorDel2X = IX - CONVEY_X(J)
          ConveyorDel2Y = IY - CONVEY_Y(J)
        End If
      Else
        A = UnitLocX(I, J) - Left(I) * IRR(I, J)
        B = UnitLocX(I, J) + Right(I) * IRR(I, J)
        ' Flotation cells have variable width
        If I = 6 Then B = B + (NOCELL(J) - 1) * 10 * IRR(I, J)
        c = UnitLocY(I, J) + Top(I) * IS1
        d = UnitLocY(I, J) - Bottom(I) * IS1
        If A <= B Then lx = IX <= B And IX >= A
        If A > B Then lx = IX >= B And IX <= A
        ly = IY <= c And IY >= d
      End If
      YESNO = lx And ly
      If YESNO Then
        UnitNumber = Unit(I, J)
        NSEL = J
        Exit Sub
      End If
    End If
  Next UnitNumber
End Sub

Sub DeleteUnit(I As Integer, J As Integer)
'*****************************************
' Deletes a unit icon.
  Dim P As Integer, M As Integer
  IR = IRR(I, J)
  UnitNumber = Unit(I, J)
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).FillColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 13
  MGPage(0).PolygonType = 5
  Call DrawUnit(I, UnitLocX(I, J), UnitLocY(I, J))
  Flag(I, J) = False
  ' Delete all product streams that are associated with this unit
  For P = 1 To Stream
    'If StreamEnd(P) = UNIT(I, J) Or StreamStart(P) = UNIT(I, J) And SFlag(P) Then
    If StreamStart(P) = Unit(I, J) And SFlag(P) Then
      DeleteStream P
    End If
  Next P
  JobUnitParametersNeeded = True
End Sub

Sub DeleteFlyout(I As Integer)
'*****************************
'Deletes stream flyout I
On Error GoTo ErrHandler
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 13
  Call DrawFlyout(FlyoutX(I), FlyoutY(I), FlyoutAX(I), FlyoutAY(I))
  FlyoutFlag(I) = False
  If MODSIM.ViewFlowsheetFlag Then
    Call DeleteFlyoutContents(I)
  End If
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub

Sub DeleteFlyoutContents(I As Integer)
'*************************************
  'Deletes the content of a flyout
  Dim extra As Single
  Dim ISEG As Integer, L As Integer
  On Error GoTo ErrHandler
  MGPage(0).ForeColor = MGPage(0).BackColor
  MGPage(0).DrawMode = 13
  extra = FlyoutSize
  If I = 0 Then
    L = 8
  Else
    L = 7
  End If
  Call MGLABSEG(Flyouts(I).ULString, ISEG, FlyoutX(I) + 0.1 * extra, FlyoutY(I) - 2.5 * FlyoutSize, -2, 0)
  Call MGLABSEG(Flyouts(I).URString, ISEG, FlyoutX(I) + 0.1 * extra, FlyoutY(I) - 5# * FlyoutSize, -2, 0)
  Call MGLABSEG(Flyouts(I).LLString, ISEG, FlyoutX(I) + L * FlyoutSize + 0.1 * extra, FlyoutY(I) - 2.5 * FlyoutSize, -2, 0)
  If Flyouts(I).LRString <> "--------" Then
    Call MGLABSEG(Flyouts(I).LRString, ISEG, FlyoutX(I) + L * FlyoutSize + 0.1 * extra, FlyoutY(I) - 5# * FlyoutSize, -2, 0)
  End If
  Exit Sub
  
ErrHandler:
  Exit Sub

End Sub

Sub StartStream(X As Single, Y As Single, WaterStream As Boolean)
'****************************************************************
'Start a stream on the flowsheet
  Dim I As Integer
  Dim J As Integer
  Dim YESNO As Boolean
  Dim ISSEG As Integer
  Dim XT As Single, YT As Single
  If Not COMFLG Then Exit Sub
  COMFLG = False
  'Set the mouse location for use by REGION
  Call CoordConv(X, Y, XT, YT)
  MGPage(0).MouseLocationX = XT
  MGPage(0).MouseLocationY = YT
  Stream = Stream + 1
  Call REGION(I, J, YESNO)
  If WaterStream Then
    If YESNO Then
      MsgBox "Water addition streams cannot start at a unit." & Chr(13) & Chr(10) & "A process sream will be drawn", 16, "ERROR"
      WFlag(Stream) = False
      MGPage(0).WaterStreamFlagOn = False
    Else
      Water = Water + 1
      WFlag(Stream) = True
      MGPage(0).ForeColor = vbBlue
    End If
  Else
    WFlag(Stream) = False
  End If
  SFlag(Stream) = True
  STRSEG(Stream) = ISSEG
  StartX(Stream) = X
  StartY(Stream) = Y
  StreamCorners(Stream) = 0
  If YESNO Then
    Call MGRMVE(X, Y)
    StreamStart(Stream) = Unit(I, J)
    ' Identify the stream type
    Call FindStreamType(I, J, X, Y)
  Else
    If RPFLAG = 1 Then Beep
    MGPage(0).FillStyle = 1
    Call MGCIRCLE(X, Y, 5 * FlowsheetScaleFactor)
    StreamType(Stream) = 0
    If WaterStream Then
      StreamStart(Stream) = -1
      MGPage(0).ForeColor = vbBlack
    Else
      StreamStart(Stream) = 0
    End If
    Call MGRMVE(X, Y)
  End If
End Sub

Sub FindStreamType(Selection As Integer, J As Integer, X As Single, Y As Single)
'*******************************************************************************
'Identify the stream type
Dim IR1 As Single, IS1 As Single, I As Integer, II As Integer
IR1 = IRR(Selection, J)
IS1 = Abs(IR1)
StreamType(Stream) = 1

Select Case Selection
  Case 1
    If (Y >= UnitLocY(1, J) - 5 * IS1) Then StreamType(Stream) = 3
    If Abs(X - UnitLocX(1, J)) >= Abs(3 * IR1) And UnitLocY(1, J) - Y >= 4 * IS1 Then
      StreamType(Stream) = 2
    End If
  Case 2
    If IR1 * X < IR1 * UnitLocX(2, J) Then StreamType(Stream) = 2
  Case 6
    If (Abs(X - (UnitLocX(6, J) + (NOCELL(J) - 1) * 10 * IR1)) < 2 * IS1) Then StreamType(Stream) = 2
  Case 8
    If (Y >= UnitLocY(8, J)) Then StreamType(Stream) = 2
  Case 10
    If (Y > (UnitLocY(10, J) - 4 * IS1)) Then StreamType(Stream) = 2
  Case 11
    If (Y > (UnitLocY(11, J) - 6 * IS1)) Then StreamType(Stream) = 2
  Case 12
    If (Y > (UnitLocY(12, J) - IS1)) Then StreamType(Stream) = 2
  Case 14  'Vibrating screen
    If (Y > UnitLocY(14, J) - IS1) Then StreamType(Stream) = 2
  Case 15
    If (X < UnitLocX(15, J) - 6 * IS1 Or X > UnitLocX(15, J) + 6 * IS1) Then StreamType(Stream) = 2
  Case 16
    II = Stream - 1
    'Does any other stream start at this splitter?
    For I = 1 To Stream - 1
      If (StreamStart(I) = Unit(Selection, J) And SFlag(I)) Then
        StreamType(Stream) = StreamType(Stream) + 1
      End If
    Next I
    NOSPLIT(J) = StreamType(Stream)
  Case 18
    If (Y >= (UnitLocY(18, J) - 5 * IS1)) Then
      StreamType(Stream) = 2
      If (Y <= UnitLocY(18, J)) Then
        StreamType(Stream) = 3
      End If
    End If
  Case 20
    If (Y > UnitLocY(20, J)) Then StreamType(Stream) = 2
  Case 25  'WHIMS
    If (Abs(X - UnitLocX(25, J)) > 9 * IS1) Then StreamType(Stream) = 2
  Case 36
    If (Y >= UnitLocY(36, J) + 2 * IS1) Then StreamType(Stream) = 2
  Case 38
    If (Y >= UnitLocY(38, J)) Then StreamType(Stream) = 2
  Case 40
    StreamType(Stream) = 3
    If (Y >= UnitLocY(40, J) + IS1) Then StreamType(Stream) = 2
    If (Y < UnitLocY(40, J) - 3 * IS1) Then StreamType(Stream) = 1
  Case 41
    If (Abs(X - UnitLocX(41, J)) > 10 * IS1) Then StreamType(Stream) = 2
  Case 42
    If (Y > UnitLocY(42, J)) Then StreamType(Stream) = 2
  Case 43
    If (Y > UnitLocY(43, J)) Then StreamType(Stream) = 2
  Case 44
    If (Y > UnitLocY(44, J)) Then StreamType(Stream) = 2
  Case 45
    If (Y < UnitLocY(45, J) + 2 * IS1) Then StreamType(Stream) = 2
  Case 46
    If (Y < UnitLocY(46, J) + 2 * IS1) Then StreamType(Stream) = 2
  Case 47
    If (Y > UnitLocY(47, J)) Then StreamType(Stream) = 2
  Case 48
    If (IR1 * X > IR1 * UnitLocX(48, J)) Then StreamType(Stream) = 2
  Case 49
    If (Y > UnitLocY(49, J) + 6 * IS1) Then StreamType(Stream) = 2
    If (Abs(X - UnitLocX(49, J)) > 9 * IS1) Then StreamType(Stream) = 3
  Case 51
    If (Y > UnitLocY(51, J)) Then StreamType(Stream) = 2
  Case 52
    If (Abs(Y - UnitLocY(52, J)) > 3 * IS1 Or Abs(X - UnitLocX(52, J)) < 4 * IS1) Then StreamType(Stream) = 2
  Case 56
    If (Y >= UnitLocY(56, J)) Then StreamType(Stream) = 2
  Case 57
    If (Y >= UnitLocY(57, J)) Then StreamType(Stream) = 2
  Case 58  'Sieve bend
    If (Abs(X - UnitLocX(58, J)) > 2) Then StreamType(Stream) = 2
  Case 61
    If (Y > UnitLocY(61, J)) Then StreamType(Stream) = 2
  Case 63
    If (Y <= UnitLocY(63, J) + 1) Then StreamType(Stream) = 2
  Case 64  'Knelson concentrator
    If (Y >= UnitLocY(64, J) - 4 * IS1) Then StreamType(Stream) = 2
  Case 65  'Autogenous mill with trommel
    If Abs(X - UnitLocX(65, J)) > 14 * IS1 Then StreamType(Stream) = 2
    If Abs(X - UnitLocX(65, J)) < 8 * IS1 Then StreamType(Stream) = 3
  Case 66  'Water injection cyclone
    If Y > UnitLocY(66, J) - 8 * IS1 Then StreamType(Stream) = 2
  Case 67  'Compound screen
    If (Y > UnitLocY(67, J) - IS1) Then
      StreamType(Stream) = 2
    ElseIf Abs(UnitLocX(67, J) + 9 * IR1 - X) < 9.5 * IS1 Then
      StreamType(Stream) = 3
    End If
  Case 68  'Flotation column
    If Y >= UnitLocY(68, J) + 6 * IS1 Then StreamType(Stream) = 2
  Case 69  'Jones magnetic separator
    If Y < UnitLocY(69, J) Then
      If X < UnitLocX(69, J) Then
        StreamType(Stream) = 2
      Else
        StreamType(Stream) = 3
      End If
    End If
    
  End Select
End Sub

Sub STRNUM(Number As Integer)
'*****************************
'  Places the stream number on the flowsheet and in the PostScript buffer.
  Dim XC As Single, YC As Single, ISEG As Integer
  If MGPage(0).ChkStreamNumbers = vbChecked Then
    Call LocateStreamNumber(Number, XC, YC)
    Call MGLABSEG(CStr(Number), ISEG, XC, YC, 0, 0)
  End If
End Sub

Sub Number(IX As Single, IY As Single, I As Single, J As Single, Number As Integer)
'**********************************************************************************
' LOCATES AND DRAWS THE UNIT NUMBER.
  Dim ISS As Single
  Dim ISEG As Integer, OffsetX As Single, OffsetY As Single
  If MGPage(0).ChkUnitNumbers = vbChecked Then
    ISS = Abs(IR)
    OffsetX = I * IR
    OffsetY = J * ISS
    Call MGLABSEG(CStr(Number), ISEG, IX, IY, OffsetX, OffsetY)
  End If
End Sub

Function NOTEPK(IX As Single, IY As Single) As Integer
'***************************************************
  'Finds whether a label has been picked
  Dim XCHK As Boolean, YCHK As Boolean
  Dim N As Integer
  For N = LABNO To 1 Step -1
    If (NOTEFL(N)) Then
      XCHK = (NOTEX(N) - 20) < IX And (NOTEX(N) + 20) > IX
      YCHK = (NOTEY(N) - 20) < IY And (NOTEY(N) + 20) > IY
      If (XCHK And YCHK) Then
        NOTEPK = N
        Exit Function
      End If
    End If
  Next N
  NOTEPK = 0
End Function

Sub NOTEDEL(I As Integer)
'***********************
  'DELETES AN ANNOTATION FROM THE FLOWSHEET AND THE BUFFERS.
  Dim ScX As Single, ScY As Single
  
  MGPage(0).ForeColor = MGPage(0).BackColor
  Call CoordConv(NOTEX(I), NOTEY(I), ScX, ScY)
  MGPage(0).CurrentX = ScX
  MGPage(0).CurrentY = ScY - MGPage(0).TextHeight("M")
  MGPage(0).Print NOTE(I)
  NOTEFL(I) = False
End Sub

Function BriefFlag(Flag As Boolean) As String
'********************************************
  If Flag Then
    BriefFlag = "T"
  Else
    BriefFlag = "F"
  End If
End Function

Public Sub ReadFlowsheet()
'*************************
'Read the flowsheet details from file
  Dim Length As Integer, I As Integer, II As Integer
  Dim J As Integer
  Dim NUNIT1 As Integer, STREAM1 As Integer, WATER1 As Integer
  Dim COUNT1(100) As Integer
  Dim LABNO1 As Integer, FlyoutNo1 As Integer
  Dim Corners1(200) As Integer
  Dim WSTRING As String
  
  On Error GoTo ErrHandler
  
  For I = 1 To 200
    SFlag(I) = False
    WFlag(I) = False
  Next I
  Open RTrim(JobPath) + "PLNTR.OUT" For Input As #15
  Input #15, FlowsheetNumber, Length, IR, Unique_ID
  MGPage(0).Iconsize = IR
  Line Input #15, JOBNAME
  Input #15, NUNIT1, STREAM1, WATER1
  For I = 1 To 100
    Input #15, COUNT1(I)
  Next I
  If STREAM1 > 0 Then
    Input #15, WSTRING
    For I = 1 To STREAM1
      If Mid(WSTRING, I, 1) = "T" Then
        SFlag(I) = True
      Else
        SFlag(I) = False
      End If
    Next I
  End If
  If WATER1 > 0 Then
    Input #15, WSTRING
    For I = 1 To STREAM1
      If Mid(WSTRING, I, 1) = "T" Then
        WFlag(I) = True
      Else
        WFlag(I) = False
      End If
    Next I
  End If
  'Read the data for the units
  For I = 1 To 100
    If COUNT1(I) > 0 Then
      Input #15, II, WSTRING
      For J = 1 To COUNT1(I)
        If Mid(WSTRING, J, 1) = "T" Then
          Flag(II, J) = True
        Else
          Flag(II, J) = False
        End If
      Next J
      For J = 1 To COUNT1(I)
        If Flag(I, J) Then Input #15, Unit(I, J), UnitLocX(I, J), UnitLocY(I, J), IRR(I, J), ID(I, J)
        If ID(I, J) > Unique_ID Then Unique_ID = ID(I, J) + 1
        If Flag(I, J) And I = 6 Then Input #15, NOCELL(J)
        If Flag(I, J) And I = 62 Then Input #15, CONVEY_X(J), CONVEY_Y(J)
      Next J
    End If
  Next I
  'Read the data for the streams
  If STREAM1 > 0 Then
    For I = 1 To STREAM1
      If SFlag(I) Then
        Input #15, Corners1(I)
        Input #15, StartX(I), StartY(I), EndX(I), EndY(I), Str_ID(I)
        If Corners1(I) > 0 Then
          For J = 1 To Corners1(I)
            Input #15, CornerX(I, J), CornerY(I, J)
          Next J
        End If
      End If
    Next I
  End If
  
  'Read the annotations
  Input #15, LABNO1
  If LABNO1 > 0 Then
    Input #15, WSTRING
    For I = 1 To LABNO1
      If Mid(WSTRING, I, 1) = "T" Then
        NOTEFL(I) = True
      Else
        NOTEFL(I) = False
      End If
    Next I
    For I = 1 To LABNO1
      If NOTEFL(I) Then
        Input #15, NOTEX(I), NOTEY(I)
        Line Input #15, NOTE(I)
      End If
    Next I
  End If
  
  'Read the Flyouts
  On Error GoTo CONTINUE
  Input #15, FlyoutNo1
  If FlyoutNo1 >= 0 Then
    Input #15, WSTRING
    For I = 0 To FlyoutNo1
      If Mid(WSTRING, I + 1, 1) = "T" Then
        FlyoutFlag(I) = True
      Else
        FlyoutFlag(I) = False
      End If
    Next I
    For I = 0 To FlyoutNo1
      If FlyoutFlag(I) Then
        Input #15, FlyoutX(I), FlyoutY(I), FlyoutAX(I), FlyoutAY(I)
      End If
    Next I
  End If
  
  'Read the font name and size
  Line Input #15, WSTRING
  LabelFont.Name = Trim(WSTRING)
  Input #15, LabelFont.size
  Call MGFONT(LabelFont)
  
  Flyoutfont.Name = LabelFont.Name
  Flyoutfont.size = LabelFont.size
  
  'Read the control box coordinates
  If EOF(15) Then
    BoxLeft = 608
    BoxTop = 8
  Else
    Input #15, BoxLeft, BoxTop
  End If
  If EOF(15) Then
    FlyoutSizeFactor = 1
  Else
    Input #15, FlyoutSizeFactor
  End If
  FlyoutSize = FlyoutSizeFactor * 10# * FlowsheetScaleFactor
  
CONTINUE:
  Close #15

  On Error GoTo ErrHandler2
  'Draw the units
  RPFLAG = 2
  Call RedrawUnits(NUNIT1, COUNT1())
  'Draw the streams
  RPFLAG = 3
  Call RedrawStreams(STREAM1, WATER1, Corners1())
  'Draw the annotations
  Call MGFONT(LabelFont)
  Call RedrawAnnotations(LABNO1)
  'Draw the flyouts
  Call RedrawFlyouts(FlyoutNo1)
  
  'If viewing the flowsheet, display the flyout data
  If MODSIM.ViewFlowsheetFlag Then
    If SimulationHasBeenRun Then
      Call FillFlyouts
    Else
      MsgBox "There is no valid output data" & vbCrLf & "Right click on the flowsheet background to run the simulator to generate output data", vbInformation
    End If
  End If
  Exit Sub

ErrHandler:
  MsgBox "Error reading flowsheet data file", 16, "ERROR"
  Close #15
  Exit Sub
  
ErrHandler2:
  MsgBox "Error drawing flowsheet", 16, "ERROR"
  Close #15
  Exit Sub
  End Sub
  
Public Sub FillFlyouts()
'***********************
  Dim Streams As Integer, ISEG As Integer
  Dim Solids As Single, Water As Single
  Dim WS As String
  Dim I As Integer, J As Integer, II As Integer
  Dim extra As Single, VPos As Single
  
  On Error GoTo ErrHandler
  
  If MODSIM.ViewFlowsheetFlag And MGPage(0).ChkShowFlyouts.Value = vbChecked Then
    Open RTrim(JobPath) + "FlyData.OUT" For Input As #15
    Call MGFONT(Flyoutfont)
    VPos = 2.5
    MGPage(0).FontTransparent = True
    If PrinterFlagOn Then
      Printer.FontTransparent = True
      VPos = 2.5 + 1# * (1 - FlyoutSizeFactor)
    End If
    If PSFlagOn Then
      VPos = 2.5 + 0.6 * (1 - FlyoutSizeFactor)
    End If
    'Fill the reference flyout first
    II = 0
    extra = FlyoutSize
    Line Input #15, WSTRING
    Flyouts(II).ULString = Trim(Mid(WSTRING, 1, 11))
    Call MGLABSEG(Flyouts(II).ULString, ISEG, FlyoutX(II) + 0.1 * extra, FlyoutY(II) - VPos * FlyoutSize, -2, 0)
    Flyouts(II).URString = Trim(Mid(WSTRING, 12, 9))
    Call MGLABSEG(Flyouts(II).URString, ISEG, FlyoutX(II) + 0.1 * extra, FlyoutY(II) - 2 * VPos * FlyoutSize, -2, 0)
    Flyouts(II).LLString = Trim(Mid(WSTRING, 21, 5))
    Call MGLABSEG(Flyouts(II).LLString, ISEG, FlyoutX(II) + 8 * FlyoutSize + 0.1 * extra, FlyoutY(II) - VPos * FlyoutSize, -2, 0)
    Flyouts(II).LRString = Trim(Mid(WSTRING, 26, 6))
    If Flyouts(II).LRString <> "------" Then
      Call MGLABSEG(Flyouts(II).LRString, ISEG, FlyoutX(II) + 8 * FlyoutSize + 0.1 * extra, FlyoutY(II) - 2 * VPos * FlyoutSize, -2, 0)
    End If
    '...and now the flyouts for the streams.
    Input #15, Streams
    For I = 1 To Streams
      Line Input #15, WSTRING
      J = Val(Mid(WSTRING, 1, 3))
      II = FindFlyout(J)
      If II > 0 Then
        Flyouts(II).ULString = Trim(Mid(WSTRING, 4, 8))
        Call MGLABSEG(Flyouts(II).ULString, ISEG, FlyoutX(II) + 0.1 * extra, FlyoutY(II) - VPos * FlyoutSize, -2, 0)
        Flyouts(II).URString = Trim(Mid(WSTRING, 12, 8))
        Call MGLABSEG(Flyouts(II).URString, ISEG, FlyoutX(II) + 0.1 * extra, FlyoutY(II) - 2 * VPos * FlyoutSize, -2, 0)
        Flyouts(II).LLString = Trim(Mid(WSTRING, 20, 8))
        Call MGLABSEG(Flyouts(II).LLString, ISEG, FlyoutX(II) + 7 * FlyoutSize + 0.1 * extra, FlyoutY(II) - VPos * FlyoutSize, -2, 0)
        Flyouts(II).LRString = Trim(Mid(WSTRING, 28, 8))
        If Flyouts(II).LRString <> "--------" Then
          Call MGLABSEG(Flyouts(II).LRString, ISEG, FlyoutX(II) + 7 * FlyoutSize + 0.1 * extra, FlyoutY(II) - 2 * VPos * FlyoutSize, -2, 0)
        End If
      End If
    Next I
  End If
  Close #15
  Exit Sub
  
ErrHandler:
  MsgBox "Flyouts could not be filled", vbExclamation, "Error"
  Close #15
  Exit Sub

End Sub
Public Sub RedrawUnits(NUNIT1 As Integer, COUNT1() As Integer)
'*************************************************************
'Redraw the units on the flowsheet from the data base
  Dim IUNIT As Integer, Selection As Integer, J As Integer
  Dim M As Integer
  Dim XT As Single, YT As Single
  
  For IUNIT = 1 To NUNIT1
    For Selection = 1 To 100
      If COUNT1(Selection) > 0 Then
        For J = 1 To COUNT1(Selection)
          If Unit(Selection, J) = IUNIT Then
            If Flag(Selection, J) Then
              MGPage(0).CursorLocationX = UnitLocX(Selection, J)
              MGPage(0).CursorLocationY = UnitLocY(Selection, J)
              If IRR(Selection, J) < 0 Then
                Call SelectUnit(Selection, -IRR(Selection, J), True)
              Else
                Call SelectUnit(Selection, IRR(Selection, J), False)
              End If
              ' Transfer the unique ID for redrawn units
              If RPFLAG = 2 Then
                ID(Selection, NSEL) = ID(Selection, J)
              End If
            Else
              'This unit was erased so need to adjust data for all remaining flotation cells, conveyors and splitters.
              For M = J + 1 To COUNT1(Selection)
                If Selection = 6 Then
                  NOCELL(M - 1) = NOCELL(M)
                End If
                'If unit is a conveyor belt, removethe coords of the second drum.
                If Selection = 62 And Not MGPage(0).MoveFlag Then
                  CONVEY_X(M - 1) = CONVEY_X(M)
                  CONVEY_Y(M - 1) = CONVEY_Y(M)
                End If
              Next M
            End If
          End If
        Next J
      End If
    Next Selection
  Next IUNIT
End Sub

Public Sub RedrawStreams(STREAM1 As Integer, WATER1 As Integer, Corners1() As Integer)
'*************************************************************************************
' Redraws the streams from the data in file PLNTR
  Dim MSTRE As Integer, J As Integer
  Dim XM As Single, YM As Single
  For MSTRE = 1 To STREAM1
    If SFlag(MSTRE) Then
      XM = StartX(MSTRE)
      YM = StartY(MSTRE)
      Call StartStream(StartX(MSTRE), StartY(MSTRE), WFlag(MSTRE))
      If Corners1(MSTRE) > 0 Then
        For J = 1 To Corners1(MSTRE)
          Call Corner(CornerX(MSTRE, J), CornerY(MSTRE, J))
          XM = CornerX(MSTRE, J)
          YM = CornerY(MSTRE, J)
        Next J
      End If
      Call EndStream(XM, YM, EndX(MSTRE), EndY(MSTRE))
      If RPFLAG = 3 Then
        'Transfer the unique ID for the stream
        Str_ID(Stream) = Str_ID(MSTRE)
      End If
    End If
  Next MSTRE
End Sub

Public Sub RefreshFlowsheet()
'****************************
'Refreshes the flowsheet eliminating redundant units and streams
  Dim I As Integer
  Dim NUNIT1 As Integer, COUNT1(100) As Integer
  Dim STREAM1 As Integer, WATER1 As Integer
  Dim LABNO1 As Integer, FlyoutNo1 As Integer
  Dim Corners1(200) As Integer
  PageNumber = 0
  MGPage(PageNumber).Cls
  RPFLAG = 2
  NUNIT1 = NUNIT
  STREAM1 = Stream
  WATER1 = Water
  LABNO1 = LABNO
  FlyoutNo1 = FlyoutNo
  NUNIT = 0
  Stream = 0
  Water = 0
  LABNO = 0
  FlyoutNo = -1
  Call MGFONT(LabelFont)
  FlyoutSize = FlyoutSizeFactor * 10# * FlowsheetScaleFactor
  
  MGPage(0).FontTransparent = False
  
  Call DrawBorder
  For I = 1 To 100
    COUNT1(I) = count(I)
    count(I) = 0
  Next I
  For I = 1 To 200
    Corners1(I) = StreamCorners(I)
    StreamCorners(I) = 0
  Next I
  If MetaFileFlagOn Or PSFlagOn Then
    Call MGLineWidth(0.7)
  End If
  Call RedrawUnits(NUNIT1, COUNT1())
  If MetaFileFlagOn Or PSFlagOn Then
    Call MGLineWidth(0.3)
  End If
  RPFLAG = 3
  Call RedrawStreams(STREAM1, WATER1, Corners1())
  
  'Draw the annotations
  Call RedrawAnnotations(LABNO1)
  
  'Draw the flyouts
  Call MGFONT(Flyoutfont)
  Call RedrawFlyouts(FlyoutNo1)
  
  'Fill the flyouts
  Call FillFlyouts
  
  RPFLAG = 1
End Sub

Sub WriteModelData()
'*******************
'WRITE DATA FOR TRANSMISSION TO THE MODEL DATA MODULE
  Dim I As Integer, J2 As Integer, N As Integer
  Open RTrim(JobPath) & "MODSDATA.OUT" For Output As #16
  Print #16, MODSIM.JOBNAME
  Print #16, NUNIT
  For I = 1 To NUNIT
    Print #16, UNTYP(I);
  Next I
  Print #16,
  For I = 1 To NUNIT
    Print #16, UN_ID(I);
  Next I
  Print #16,
  For N = 1 To NUNIT
    If UNTYP(N) > 0 Then
      If (UNTYP(N) = 1 Or UNTYP(N) = 49 Or UNTYP(N) = 16 Or UNTYP(N) = 18) Then
        Print #16, UN_ID(N), NO_OUT_STRS(N)
      End If
      If (UNTYP(N) = 6) Then
        J2 = Match(N, 6)
        Print #16, J2, NOCELL(J2)
      End If
    End If
  Next N
  Close #16
  JobModelData = True
End Sub

Function NO_OUT_STRS(UNITNO As Integer)
'**************************************
' Counts the number of outlet streams from unit UNITNO
   Dim I As Integer
   NO_OUT_STRS = 0
   For I = 1 To Stream
     If (StreamStart(I) = UNITNO) Then NO_OUT_STRS = NO_OUT_STRS + 1
   Next I
 End Function

Function Match(N As Integer, J As Integer) As Integer
'****************************************************
' FINDS THE ELEMENT OF THE ARRAY UNIT(J, .) THAT IS EQUAL TO N.
  Dim I As Integer
  For I = 1 To 20
    If Unit(J, I) = N Then Exit For
  Next I
  Match = I
End Function

Sub WriteSystemData()
'********************
' Write data for transmission to the SYSTEM DATA module.
  Dim NSTREAMS As Integer, J As Integer, I As Integer, L As Integer
  Dim STR_TYPE As String * 1
  Open RTrim(JobPath) & "SYSDATA.OUT" For Output As #17
  Print #17, MODSIM.JOBNAME
  NSTREAMS = 0
  For I = 1 To Stream
    If SFlag(I) Then NSTREAMS = NSTREAMS + 1
  Next I
  Print #17, NSTREAMS
  For J = 0 To 3
    For L = 1 To Stream
      If (SFlag(L) And StreamType(L) = J) Then
        STR_TYPE = "O"
        If (StreamStart(L) = 0) Then STR_TYPE = "F"
        If WFlag(L) Then STR_TYPE = "W"
        Print #17, L; Str_ID(L); STR_TYPE; " "; StreamStart(L); StreamEnd(L)
      End If
    Next L
  Next J
  Close #17
  JobSystemData = True
End Sub

Public Sub GetFlowsheet(FileName As String)
'******************************************
'Gets a flowsheet file that was saved by a previous version of MODSIM
  Dim Length As Integer, I As Integer, II As Integer
  Dim K As Integer, L As Integer, J As Integer
  Dim Dummy As Integer
  Dim NUNIT1 As Integer, STREAM1 As Integer, WATER1 As Integer, STRCHA As Integer
  Dim COUNT1(100) As Integer
  Dim LABNO1 As Integer
  Dim Corners1(200) As Integer
  Dim WSTRING As String
  
  On Error GoTo ErrHandler
  
  Open FileName For Input As #15
  Input #15, FlowsheetNumber, Length, IR, Unique_ID
  Line Input #15, JOBNAME
  Input #15, NUNIT1, STREAM1, WATER1, STRCHA
  For I = 1 To 67
    Input #15, COUNT1(I)
  Next I
  If STREAM1 > 0 Then
    Input #15, WSTRING
    For I = 1 To STREAM1
      If Mid(WSTRING, I, 1) = "T" Then
        SFlag(I) = True
      Else
        SFlag(I) = False
      End If
    Next I
  End If
  If WATER1 > 0 Then
    Input #15, WSTRING
    For I = 1 To STREAM1
      If Mid(WSTRING, I, 1) = "T" Then
        WFlag(I) = True
      Else
        WFlag(I) = False
      End If
    Next I
  End If
  'Read the data for the units
  For I = 1 To 67
    If COUNT1(I) > 0 Then
      Input #15, WSTRING
      II = Val(WSTRING)
      K = InStr(WSTRING, "T")
      L = InStr(WSTRING, "F")
      If K = 0 Then
        WSTRING = Mid(WSTRING, L)
      ElseIf L = 0 Then
        WSTRING = Mid(WSTRING, K)
      ElseIf K < L Then
        WSTRING = Mid(WSTRING, K)
      Else
        WSTRING = Mid(WSTRING, L)
      End If
      For J = 1 To COUNT1(I)
        If Mid(WSTRING, J, 1) = "T" Then
          Flag(II, J) = True
        Else
          Flag(II, J) = False
        End If
      Next J
      For J = 1 To COUNT1(I)
        If Flag(I, J) Then Input #15, Unit(I, J), UnitLocX(I, J), UnitLocY(I, J), IRR(I, J), ID(I, J)
        If ID(I, J) > Unique_ID Then Unique_ID = ID(I, J) + 1
        If Flag(I, J) And I = 6 Then Input #15, NOCELL(J)
        If Flag(I, J) And I = 62 Then Input #15, J, CONVEY_Y(J)
      Next J
    End If
  Next I
  'Read the data for the streams
  If STREAM1 > 0 Then
    For I = 1 To STREAM1
      If SFlag(I) Then
        Input #15, Corners1(I)
        Input #15, StartX(I), StartY(I), EndX(I), EndY(I), Str_ID(I)
        If Corners1(I) > 0 Then
          For J = 1 To Corners1(I)
            Input #15, CornerX(I, J), CornerY(I, J)
          Next J
        End If
      End If
    Next I
  End If
  
  'Read the annotations
  Input #15, LABNO1
  If LABNO1 > 0 Then
    Input #15, WSTRING
    For I = 1 To LABNO1
      If Mid(WSTRING, I, 1) = "T" Then
        NOTEFL(I) = True
      Else
        NOTEFL(I) = False
      End If
    Next I
    For I = 1 To LABNO1
      If NOTEFL(I) Then
        Input #15, NOTEX(I), NOTEY(I), Dummy
        Input #15, NOTE(I)
      End If
    Next I
  End If
  Close #15
  
  'Draw the units
  RPFLAG = 2
  Call RedrawUnits(NUNIT1, COUNT1())
  'Draw the streams
  RPFLAG = 3
  Call RedrawStreams(STREAM1, WATER1, Corners1())
  'Draw the annotations
  Call RedrawAnnotations(LABNO1)
  Exit Sub
  
ErrHandler:
  MsgBox "Error found in flowsheet data file", 16, "ERROR"
  Close #15
  Exit Sub
End Sub

Public Sub RedrawFlyouts(FlyoutNo1 As Integer)
'*********************************************
'Redraw the flyouts on the flowsheet
  Dim I As Integer
  FlyoutNo = -1
  If FlyoutNo1 >= 0 Then
    For I = 0 To FlyoutNo1
      If FlyoutFlag(I) Then
        MGPage(0).SelectedFlyout = I
        Call Flyout(FlyoutX(I), FlyoutY(I), FlyoutAX(I), FlyoutAY(I))
        MGPage(0).SelectedFlyout = -1
      End If
    Next I
  End If
End Sub
Public Sub RedrawAnnotations(LABNO1 As Integer)
'**********************************************
'Redraw the annotations on the flowsheet
  Dim I As Integer
  If LABNO1 > 0 Then
    For I = 1 To LABNO1
      If NOTEFL(I) Then
        Call ANNOTE(NOTE(I), NOTEX(I), NOTEY(I))
      End If
    Next I
  End If
End Sub
Public Sub ShowUnitReport(Unit As Integer)
'*****************************************

End Sub

Public Sub StreamPick(P As Integer, YESNO As Boolean)
'****************************************************
' Picks a stream on the flowsheet.
  Dim IX As Single, X1 As Single, DX As Single, DX12 As Single
  Dim IY As Single, Y1 As Single, DY As Single, DY12 As Single
  Dim I As Integer
  YESNO = False
  Call ConvertCoordFromScreen(MGPage(0).MouseLocationX, MGPage(0).MouseLocationY, IX, IY)
  For P = 1 To Stream
    If SFlag(P) Then
      X1 = StartX(P)
      Y1 = StartY(P)
      If StreamCorners(P) > 0 Then
        For I = 1 To StreamCorners(P)
          DX = IX - X1
          DY = IY - Y1
          DX12 = CornerX(P, I) - X1
          DY12 = CornerY(P, I) - Y1
          If SectionPick(DX, DY, DX12, DY12) Then
            YESNO = True
            Exit Sub
          End If
          X1 = CornerX(P, I)
          Y1 = CornerY(P, I)
        Next I
      End If
      DX = IX - X1
      DY = IY - Y1
      DX12 = EndX(P) - X1
      DY12 = EndY(P) - Y1
      If SectionPick(DX, DY, DX12, DY12) Then
        YESNO = True
        Exit Sub
      End If
    End If
  Next P
End Sub

Public Function SectionPick(DX As Single, DY As Single, DX12 As Single, DY12 As Single) As Boolean
'*************************************************************************************************
'Picks a section of a stream on the flowsheet.
  Dim eps As Single, epsDash As Single
  Dim cosTheta As Single, sinTheta As Single
  Dim SectionLength As Single
  epsDash = 5
  
  SectionLength = Sqr(DY12 ^ 2 + DX12 ^ 2)
  If SectionLength = 0 Then
    SectionPick = False
    Exit Function
  End If
  If Abs(DY12) >= Abs(DX12) Then
    sinTheta = DY12 / SectionLength
    eps = epsDash / sinTheta
    SectionPick = Abs(DX * DY12 - DX12 * DY) < eps * DY12 And 0 <= DY / DY12 And DY / DY12 <= 1#
  Else
    cosTheta = DX12 / SectionLength
    eps = epsDash / cosTheta
    SectionPick = Abs(DY * DX12 - DY12 * DX) < eps * DX12 And 0 <= DX / DX12 And DX / DX12 <= 1#
  End If
End Function
