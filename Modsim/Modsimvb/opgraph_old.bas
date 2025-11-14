Attribute VB_Name = "MdlOPGraph"
'*******************   PROGRAM OPGRAPH   *********************C
'                                                             C
'                                                             C
'  THIS IS THE MODULE FOR GRAPHIC OUTPUT FROM MODSIM.         C
'                                                             C
'  (C) COPYRIGHT RP KING  JOHANNESBURG 1985                   C
'*************************************************************C
'
Option Explicit
Private NSTRM As Integer, ISTRM(0 To 10) As Integer, STR_NAME(0 To 100) As String
Private SingleSizeDistribution As Boolean, SingleStreamNumber As Integer

Sub OPGRAPH()
'************
  Dim size(25) As Single
  Dim MEMCUM(25, 0 To 10) As Single, CUMSIZ(25) As Single
  Dim Vector(1 To 25) As Single, VECTOR2(1 To 25) As Single
  Dim COMPSIZE(25, 0 To 10) As Single, COMPARE(25, 0 To 10) As Single, MAXCOM(0 To 10) As Integer
  Dim WSTRING As String
  Dim SIZUN(7) As String
  Dim IWAS As Integer, NN As Integer, ISYM As Integer
  Dim ICODE As Integer
  Dim NSIZE(0 To 10) As Integer
  Dim NSTRE As Integer
  Dim AXTYPE As Integer
  Dim OPP As String * 1, Grid As String * 1
  Dim INPFL(100) As Boolean, MFFLG(100) As Boolean
  Dim BOXFL As Boolean, COMPFL As String * 1, COMPSTR(0 To 10) As Boolean
  Dim NDC As Integer, NGC As Integer, NSC As Integer, MSZ As Integer
  Dim I As Integer, N As Integer, J As Integer
  Dim ITYPE As Integer, IPEN As Integer, Icolor As Integer
  Dim X0 As Single, XF As Single, Y0 As Single, YF As Single, NDEC As Integer, NDECY As Integer

  On Error GoTo ErrHandler
  SIZUN(1) = "meters "
  SIZUN(2) = "       "
  SIZUN(3) = "cms    "
  SIZUN(4) = "mms    "
  SIZUN(5) = "       "
  SIZUN(6) = "       "
  SIZUN(7) = "microns"

  'Get number of streams, and the stream numbers for which graphs are required
  NSTRM = Sizgraph.GraphList.ListCount
  If (NSTRM > 10) Then
    MsgBox "Choose no more than 10 streams per graph" & Chr(13) & Chr(10) & "Remove " & CStr(NSTRM - 10) & " streams from the graph list", vbExclamation, "WARNING"
    Exit Sub
  End If
  'Get the information from the libgraph form.
  For I = 0 To 100
    STR_NAME(I) = " "
  Next I
  NSTRE = Sizgraph.StreamList.ListCount
  For I = 1 To NSTRE
    J = Val(Sizgraph.StreamList.List(I - 1))
    N = InStr(3, Sizgraph.StreamList.List(I - 1), " ")
    If N = 0 Then
      STR_NAME(J) = " "
    Else
      STR_NAME(J) = Trim(Mid(Sizgraph.StreamList.List(I - 1), N))
    End If
  Next I
  NSTRM = Sizgraph.GraphList.ListCount
  For I = 1 To NSTRM
    ISTRM(I) = Val(Sizgraph.GraphList.List(I - 1))
  Next I
  
  'SPECIFY CODE FOR REQUIRED UNIT FOR SIZE.  0=METERS, 2=CM  3=MMS. 6=MICRONS.")
  If Sizgraph.OptSize(0).Value = -1 Then ICODE = 6
  If Sizgraph.OptSize(1).Value = -1 Then ICODE = 3
  If Sizgraph.OptSize(2).Value = -1 Then ICODE = 2
  If Sizgraph.OptSize(3).Value = -1 Then ICODE = 0
  
  'Get preferred set of axes  1...LINEAR-LOG  2...LOG-LOG
  If Sizgraph.OptCoord(0).Value = -1 Then AXTYPE = 1
  If Sizgraph.OptCoord(1).Value = -1 Then AXTYPE = 2
  If Sizgraph.ChkShowGridLines.Value = 1 Then
    Grid = "Y"
  Else
    Grid = "N"
  End If
  OPP = "Y"
  BOXFL = False
  If Sizgraph.ChkExp.Value = 1 Then
    COMPFL = True
  Else
    COMPFL = False
  End If
   
  'Open a file to export the data to PSDPLOT as an external application
  Open RTrim(JobPath) & "PSDPLOT.OUT" For Output As #36
  Print #36, "PSD_PLOT"

  Print #36, NSTRM
  For IWAS = 1 To NSTRM
    Print #36, ISTRM(IWAS)
  Next IWAS
  Print #36, ICODE
  Print #36, AXTYPE

  'Read data coming from the simulator.
  '************************************
  Open RTrim(JobPath) & "OPGRAPH.DAT" For Input As #33
  If EOF(33) Then
    MsgBox "There is no particle size data available", vbExclamation, "Warning"
    Exit Sub
  End If
  Line Input #33, WSTRING
  Line Input #33, WSTRING
  NDC = Val(Mid(WSTRING, 18, 5))
  NGC = Val(Mid(WSTRING, 23, 5))
  NSC = Val(Mid(WSTRING, 28, 5))
  If NDC > 1 Then
    Line Input #33, WSTRING
    NSTRE = Val(Mid(WSTRING, 19, 5))
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    For I = 1 To NSTRE
      If Mid(WSTRING, I, 1) = "T" Then
        INPFL(I) = True
      Else
        INPFL(I) = False
      End If
    Next I
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    For I = 1 To NSTRE
      If Mid(WSTRING, I, 1) = "T" Then
        MFFLG(I) = True
      Else
        MFFLG(I) = False
      End If
    Next I
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    For I = 1 To NDC
      Input #33, size(I)
    Next I
    Line Input #33, WSTRING
    Line Input #33, WSTRING
    For I = 1 To NDC
      Input #33, CUMSIZ(I)
      CUMSIZ(I) = (10 ^ ICODE) * CUMSIZ(I)
    Next I
    For I = 1 To NSTRE
      Line Input #33, WSTRING
      NN = Val(Mid(WSTRING, 7, 5))
      'Has this stream been chosen for plotting?
      N = 0
      For IWAS = 1 To NSTRM
        If NN = ISTRM(IWAS) Then N = IWAS
      Next IWAS
      For J = 1 To NDC
        Input #33, MEMCUM(J, N)
        MEMCUM(J, N) = 100 * MEMCUM(J, N)
      Next J
    Next I
  End If
  Close (33)
  ITYPE = 0
  IPEN = 0
  Icolor = 7
'
' Plot experimental size distributions for comparison?
  If COMPFL = "T" Then
    Open RTrim(JobPath) & "SIZECOMP.OUT" For Input As #34
    Line Input #34, WSTRING
    If Mid(WSTRING, 1, 8) = "END OF F" Then GoTo 9001
10:
    Line Input #34, WSTRING
    If Mid(WSTRING, 1, 10) = "END OF FIL" Then GoTo 9001
    If Mid(WSTRING, 1, 10) = "Stream    " Then
      NN = Val(Mid(WSTRING, 11, 4))
20:
      Line Input #34, WSTRING
      If Mid(WSTRING, 1, 10) = "Size dist " Then
        'Has this stream been chosen for plotting?
        N = 0
        For IWAS = 1 To NSTRM
          If NN = ISTRM(IWAS) Then N = IWAS
        Next IWAS
        NSIZE(N) = Val(Mid(WSTRING, 11, 5))
        If NSIZE(N) > 0 Then
          COMPSTR(N) = True
          For I = 1 To NSIZE(N)
            Input #34, COMPSIZE(I, N), COMPARE(I, N)
          Next I
        End If
      ElseIf Mid(WSTRING, 1, 10) = "Water feed" Then
        GoTo 10
      Else
        GoTo 20
      End If
    End If
    GoTo 10
9001:
    For IWAS = 1 To NSTRM
      For I = 1 To NSIZE(IWAS)
        If COMPARE(I, IWAS) > 0# Then
          MAXCOM(IWAS) = I
          COMPSIZE(I, IWAS) = COMPSIZE(I, IWAS) * 10 ^ ICODE
          COMPARE(I, IWAS) = COMPARE(I, IWAS) * 100
        End If
      Next I
    Next IWAS
    Close (34)
  End If
  
  BOXFL = False
  If CHPAGE() <= 0 Then
    MsgBox "Could not open a METGRAPH page", vbCritical, "ERROR"
    Exit Sub
  End If

  For IWAS = 1 To NSTRM
    For I = 1 To NDC
      MSZ = NDC - I + 1
      If MEMCUM(MSZ, IWAS) > 0# Then
        Exit For
      End If
    Next I
    If MSZ < 2 Then
      MsgBox "Unable to plot size distribution for stream " & CStr(ISTRM(IWAS)), vbExclamation, "Warning"
    Else

      Print #36, "Data set  ", ISTRM(IWAS), MSZ, STR_NAME(ISTRM(IWAS))
      For I = 1 To MSZ
        Print #36, Format(CUMSIZ(I), "####0.0000"), Format(MEMCUM(I, IWAS), "####0.0000")
      Next I

      If IWAS = 1 Then Call PLOTID

      'Draw a box if this is the first graph
      If Not BOXFL Then
        Call BBOX(150, 100, 750, 500)
        Call LOGLIM(CUMSIZ(NDC - 1), CUMSIZ(1), X0, XF, NDEC)
        Call LGAXIS(X0, XF, 1, OPP, Grid)
        Call LABAX("Particle size", 13, SIZUN(ICODE + 1), 7, 13, 1)
        Select Case (AXTYPE)
          Case (1)
            Call LINAX(0#, 100#, 20, 3#, 2, OPP, Grid)
          Case (2)
            Call LOGLIM(MEMCUM(MSZ, IWAS), 100#, Y0, YF, NDECY)
          Call LGAXIS(Y0, YF, 2, OPP, Grid)
        End Select
        Call LABAX("Cumulative % smaller", 20, " ", 1, 13, 2)
        BOXFL = True
      End If
      'Draw the graph
      ITYPE = ITYPE + 1
      If ITYPE > 10 Then ITYPE = 0
      Call LNTYPE(ITYPE)
      Call CHANGECOL(Icolor)
      Call LBSIZE(9, 18)
      ISYM = IWAS
      Select Case (AXTYPE)
        Case (1)
          Call CLIPBOX
          Call SelectColumn(MEMCUM, 1, IWAS, MSZ, Vector)
          Call PLSMGR(CUMSIZ, Vector, MSZ, 0, "ALN", "LINTR")
          Call CLIPOFF
          If COMPSTR(IWAS) And NSIZE(IWAS) > 0 Then
            Call CLIPBOX
            Call SelectColumn(COMPSIZE, 1, IWAS, NSIZE(IWAS), Vector)
            Call SelectColumn(COMPARE, 1, IWAS, NSIZE(IWAS), VECTOR2)
            Call DISPNT(Vector, VECTOR2, MAXCOM(IWAS), ISYM, "ALN", "LINTR")
            Call CLIPOFF
            Call STRMID(IWAS, ISYM)
          Else
            Call STRMID(IWAS, 0)
          End If
        Case (2)
          Call CLIPBOX
          Call SelectColumn(MEMCUM, 1, IWAS, MSZ, Vector)
          Call PLSMGR(CUMSIZ, Vector, MSZ, 0, "ALN", "ALN")
          Call CLIPOFF
          If COMPSTR(IWAS) And NSIZE(IWAS) > 0 Then
            Call CLIPBOX
            Call SelectColumn(COMPSIZE, 1, IWAS, NSIZE(IWAS), Vector)
            Call SelectColumn(COMPARE, 1, IWAS, NSIZE(IWAS), VECTOR2)
            Call DISPNT(Vector, VECTOR2, MAXCOM(IWAS), ISYM, "ALN", "ALN")
            Call CLIPOFF
            Call STRMID(IWAS, ISYM)
          Else
            Call STRMID(IWAS, 0)
          End If
      End Select
    End If
  Next IWAS
  Close (36)
  Call GROUTCOORD
  Exit Sub
  
ErrHandler:
  MsgBox "Could not plot size distributions", vbCritical, "ERROR"
  Close #33
  Close #36
  Exit Sub
End Sub

Sub STRMID(IWAS As Integer, ISYM As Integer)
'*******************************************
'DRAWS A STREAM IDENTIFIER FOR A PART SIZE DISTRIBUTION CURVE.
  Dim IX As Integer, IY As Integer, NL As Integer
  
  ' Use the actual stream number and stream name
  If STR_NAME(ISTRM(IWAS)) = " " Then
    WSTRING = "Stream " & CStr(ISTRM(IWAS))
    NL = Len(WSTRING)
  Else
    WSTRING = CStr(ISTRM(IWAS)) & " " & STR_NAME(ISTRM(IWAS))
    NL = Len(WSTRING)
  End If
  IX = ((IWAS - 1) * 350) Mod 1050 + 20
  IY = 620 + ((IWAS - 1) \ 3) * 30
  Call LINIDC(IX, IY, 80, WSTRING, NL)
  If ISYM > 0 Then Call SYMBOL(IX + 40, IY, ISYM)
End Sub

Sub PLOTID()
'***********
'PLOTS THE RUN ID ON GRAPHIC OUTPUT.
  Call LBSIZE(8, 16)
  Call MGMOVE(20, 30)
  Call MGLABEL(MODSIM.JOBNAME)
End Sub

Sub PlotSingleStreamSizeDistribution(StreamNumber As Integer)
'************************************************************
  Dim I As Integer
  Dim Number As Integer, NDC As Integer
  Dim JOBNAME As String
  Dim CUMSIZE(50) As Single, Cum(50) As Single
  Dim X0 As Single, XF As Single, NDEC As Integer
  On Error GoTo ErrHandler
  If ChangePage("Small") = 0 Then Exit Sub
  'Get the data to plot
  Call GetStreamSizeDistribution(StreamNumber, CUMSIZE, Cum, NDC, JOBNAME)
  
  Call LOGLIM(CUMSIZE(NDC - 1), CUMSIZE(1), X0, XF, NDEC)
  Call BBOX(100, 100, 800, 640)
  Call LGAXIS(X0, XF, 1, "Y", "Y")
  Call LINAX(0#, 100, 10, 4#, 2, "Y", "Y")
  Call LBSIZE(17, 30)
  Call MGMOVE(110, 750)
  Call MGLABEL("Stream number " & CStr(StreamNumber))
  Call MGLABAX("Particle size  mm", 17, 1)
  Call MGLABAX("Percent passing", 17, 2)
  Call PLGRPH(CUMSIZE(), Cum(), NDC - 1, "ALN", "LINTR")
    
  'Now find any experimental data for this stream
  Open RTrim(JobPath) & "SizeComp.out" For Input As #31
  Line Input #31, WSTRING
  If Trim(WSTRING) <> JOBNAME Then
    MsgBox "Could not find experimental size distribution data for this job", vbExclamation, "WARNING"
  Else
    Do While Not EOF(31)
      Do
        Line Input #31, WSTRING
      Loop While Mid(WSTRING, 1, 6) <> "Stream" And Mid(WSTRING, 1, 11) <> "END OF FILE"
      If Val(Mid(WSTRING, 11, 4)) = StreamNumber Then
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 1, 9) <> "Size dist"
          Line Input #31, WSTRING
        Loop
        NDC = Val(Mid(WSTRING, 11, 4))
        For I = 1 To NDC
          Input #31, CUMSIZE(I), Cum(I)
          CUMSIZE(I) = 1000 * CUMSIZE(I)
          Cum(I) = 100 * Cum(I)
        Next I
        Call DISPNT(CUMSIZE, Cum, NDC, 1, "ALN", "LINTR")
        Exit Do
      End If
    Loop
  End If
  Close #31
        
  SingleStreamNumber = StreamNumber
  SingleSizeDistribution = True
  Call GROUTRightButtonEvent
  Exit Sub
ErrHandler:
  Close #31
  MsgBox "Could not plot size distribution", vbExclamation, "WARNING"
  Exit Sub
End Sub

Sub GetStreamSizeDistribution(StreamNumber, CUMSIZE() As Single, Cum() As Single, NDC As Integer, JOBNAME As String)
'******************************************************************************************************************
'Gets a size distribution from the stream properties file.
  Dim I As Integer, Number As Integer
  
  On Error GoTo ErrHandler
  'Get the data to plot
  Open RTrim(JobPath) & "STREAMPROPS.TXT" For Input As 31
  Do While Not EOF(31)
    Line Input #31, WSTRING
    If Mid(WSTRING, 1, 14) = "Stream number:" Then
      Number = Val(Mid(WSTRING, 30))
      If Number = StreamNumber Then
        Line Input #31, WSTRING
        JOBNAME = Trim(WSTRING)
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 8, 19) <> "Size      % passing"
          Line Input #31, WSTRING
          If Mid(WSTRING, 1, 23) = "Number of size classes:" Then
            NDC = Val(Mid(WSTRING, 30))
            If NDC < 3 Then
              MsgBox "Too few size classes to plot the distribution", vbExclamation, "WARNING"
              Exit Sub
            End If
          End If
        Loop
        Line Input #31, WSTRING
        If Mid(WSTRING, 8, 6) = "meters" Then
          For I = 1 To NDC - 1
            Input #31, CUMSIZE(I), Cum(I)
          Next I
        End If
      Exit Do
      End If
    End If
  Loop
  Close #31
  'Convert size to mm
  For I = 1 To NDC - 1
    CUMSIZE(I) = 1000 * CUMSIZE(I)
  Next I
  Exit Sub
ErrHandler:
  MsgBox "Could not find the data that was requested", vbExclamation, "WARNING"
  Close #31
  Exit Sub
End Sub

Sub SaveSizeDistribution(StreamNumber)
'*************************************
  Dim FileName As String, ReturnFile As String, JOBNAME As String
  Dim I As Integer, resp As Integer
  Dim NDC As Integer, CUMSIZE(25) As Single, Cum(25) As Single
  Dim DataSetName As String
  
  On Error GoTo ErrHandler
  DataSetName = InputBox("Specify a name for this size distribution", "Save data")
  If DataSetName = "" Then
    Exit Sub
  End If
  ' Set filters
  MODSIM.CommonDialog1.Filter = "Comma delimited files (*.csv)|*.csv"
  MODSIM.CommonDialog1.FilterIndex = 1
  MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  MODSIM.CommonDialog1.ShowSave
  
  'Strip off any file type extension
  WSTRING = Trim(MODSIM.CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    FileName = Mid(WSTRING, 1, I - 1)
  Else
    FileName = WSTRING
  End If
  FileName = FileName & ".csv"
  ReturnFile = Dir(FileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A file with name " & FileName & " already exists.  Overwrite?", 33, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  
  Call GetStreamSizeDistribution(StreamNumber, CUMSIZE(), Cum(), NDC, JOBNAME)
  Open FileName For Output As #31
  Print #31, "PSD_PLOT"
  Print #31, "1, 1, 0, 2"
  Print #31, "Data set  1,"; NDC - 1
  Print #31, DataSetName
  For I = 1 To NDC - 1
    Print #31, CUMSIZE(I); ", "; Cum(I)
  Next I
  Close #31
  Exit Sub
  
ErrHandler:
  Close #31
  MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub RightButtonProc(Which As String, X As Single, Y As Single)
'*************************************************************
  'Called from the single size distribution plot
  Dim response As Integer
  If Not SingleSizeDistribution Then
    Exit Sub
  End If
  response = MsgBox("Save this size distribution?", vbYesNo)
  If response = vbYes Then
    Call SaveSizeDistribution(SingleStreamNumber)
  End If
End Sub

