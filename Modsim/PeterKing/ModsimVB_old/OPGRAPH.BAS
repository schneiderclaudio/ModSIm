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
Private NSTRM As Integer, ISTRM(0 To 10) As Integer, STR_NAME(0 To 200) As String
Private SingleSizeDistribution As Boolean, SingleStreamNumber As Integer

Sub OPGRAPH()
'************
  Dim size(MaximumSizeClasses) As Single
  Dim MemCum(MaximumSizeClasses, 0 To 10) As Single, CUMSIZ(MaximumSizeClasses) As Single
  Dim Vector(1 To MaximumSizeClasses) As Single, VECTOR2(1 To MaximumSizeClasses) As Single
  Dim COMPSIZE(MaximumSizeClasses, 0 To 10) As Single, COMPARE(MaximumSizeClasses, 0 To 10) As Single, MAXCOM(0 To 10) As Integer
  Dim WSTRING As String
  Dim SIZUN(7) As String
  Dim IWAS As Integer, NN As Integer, ISYM As Integer
  Dim ICODE As Integer, SizeConv As Single
  Dim NSIZE(0 To 10) As Integer
  Dim NSTRE As Integer
  Dim AXTYPE As Integer
  Dim OPP As String * 1, Grid As String * 1
  Dim INPFL(100) As Boolean, MFFLG(100) As Boolean
  Dim BOXFL As Boolean, COMPFL As String * 1, COMPSTR(0 To 10) As Boolean
  Dim NDC As Integer, NGC As Integer, NSC As Integer, MSZ As Integer, NDCM As Integer
  Dim I As Integer, n As Integer, J As Integer
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
  'Get the information from the sizgraph form.
  For I = 0 To 100
    STR_NAME(I) = " "
  Next I
  NSTRE = Sizgraph.StreamList.ListCount
  For I = 1 To NSTRE
    J = Val(Sizgraph.StreamList.List(I - 1))
    n = InStr(3, Sizgraph.StreamList.List(I - 1), " ")
    If n = 0 Then
      STR_NAME(J) = " "
    Else
      STR_NAME(J) = Trim(Mid(Sizgraph.StreamList.List(I - 1), n))
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
  SizeConv = 10 ^ ICODE
  
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
    Next I

    For I = 1 To NSTRE
      Line Input #33, WSTRING
      NN = Val(Mid(WSTRING, 7, 5))
      'Has this stream been chosen for plotting?
      n = 0
      For IWAS = 1 To NSTRM
        If NN = ISTRM(IWAS) Then n = IWAS
      Next IWAS
      For J = 1 To NDC
        Input #33, MemCum(J, n)
        MemCum(J, n) = 100 * MemCum(J, n)
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
        n = 0
        For IWAS = 1 To NSTRM
          If NN = ISTRM(IWAS) Then n = IWAS
        Next IWAS
        NSIZE(n) = Val(Mid(WSTRING, 11, 5))
        If NSIZE(n) > 0 Then
          COMPSTR(n) = True
          For I = 1 To NSIZE(n)
            Input #34, COMPSIZE(I, n), COMPARE(I, n)
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
          COMPSIZE(I, IWAS) = COMPSIZE(I, IWAS) * SizeConv
          COMPARE(I, IWAS) = COMPARE(I, IWAS) * 100
        End If
      Next I
    Next IWAS
    Close (34)
  End If
  
  'Plot the graphs
  BOXFL = False
  If CHPAGE() <= 0 Then
    MsgBox "Could not open a METGRAPH page", vbCritical, "ERROR"
    Exit Sub
  End If

  For IWAS = 1 To NSTRM
    If ISTRM(IWAS) <= 100 Then
      NDCM = NDC
    Else
      I = ISTRM(IWAS) - 100
      NDCM = AdditionalDistribution(I).NDC
      For J = 1 To NDCM
        MemCum(J, IWAS) = AdditionalDistribution(I).Fraction(J)
      Next J
    End If
    For I = 1 To NDCM
      MSZ = NDCM - I + 1
      If MemCum(MSZ, IWAS) > 0# Then
        Exit For
      End If
    Next I
    If MSZ < 2 Then
      MsgBox "Unable to plot size distribution for stream " & CStr(ISTRM(IWAS)), vbExclamation, "Warning"
    Else
      Print #36, "Data set  ", ISTRM(IWAS), MSZ, STR_NAME(ISTRM(IWAS))
      For I = 1 To MSZ
        Print #36, Format(CUMSIZ(I), "0.000E+00"), Format(MemCum(I, IWAS), "####0.0000")
      Next I
      
      If IWAS = 1 Then Call PLOTID

      'Draw a box if this is the first graph
      If Not BOXFL Then
        Call BBOX(150, 100, 750, 500)
        Call LOGLIM(CUMSIZ(NDC - 1) * SizeConv, CUMSIZ(1) * SizeConv, X0, XF, NDEC)
        Call LGAXIS(X0, XF, 1, OPP, Grid)
        Call LABAX("Particle size", 13, SIZUN(ICODE + 1), 7, 13, 1)
        Select Case (AXTYPE)
          Case (1)
            Call LINAX(0#, 100#, 20, 3#, 2, OPP, Grid)
          Case (2)
            Call LOGLIM(MemCum(MSZ, IWAS), 100#, Y0, YF, NDECY)
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
      If ISTRM(IWAS) <= 100 Then
        For I = 1 To NDC
          Vector(I) = CUMSIZ(I) * SizeConv
        Next I
      Else
        I = ISTRM(IWAS) - 100
        NDCM = AdditionalDistribution(I).NDC
        For J = 1 To NDCM
          Vector(J) = AdditionalDistribution(I).CumSize(J) * SizeConv
        Next J
      End If
      Call SelectColumn(MemCum, 1, IWAS, MSZ, VECTOR2)
      Select Case (AXTYPE)
        Case (1)
          Call CLIPBOX
          Call PLSMGR(Vector, VECTOR2, MSZ, 0, "ALN", "LINTR")
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
          Call PLSMGR(Vector, VECTOR2, MSZ, 0, "ALN", "ALN")
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
  Dim CumSize(50) As Single, Cum(50) As Single
  Dim X0 As Single, XF As Single, NDEC As Integer
  On Error GoTo ErrHandler
  If ChangePage("Small") = 0 Then Exit Sub
  'Get the data to plot
  Call GetStreamSizeDistribution(StreamNumber, CumSize, Cum, NDC, JOBNAME)
  
  'This plot uses mm
  For I = 1 To NDC
    CumSize(I) = 1000 * CumSize(I)
  Next I
  Call LOGLIM(CumSize(NDC - 1), CumSize(1), X0, XF, NDEC)
  Call BBOX(100, 100, 800, 640)
  Call LGAXIS(X0, XF, 1, "Y", "Y")
  Call LINAX(0#, 100, 10, 4#, 2, "Y", "Y")
  Call LBSIZE(17, 30)
  Call MGMOVE(110, 750)
  Call MGLABEL("Stream number " & CStr(StreamNumber))
  Call MGLABAX("Particle size  mm", 17, 1)
  Call MGLABAX("Percent passing", 17, 2)
  Call PLGRPH(CumSize(), Cum(), NDC - 1, "ALN", "LINTR")
    
  'Now find any experimental data for this stream
  Open RTrim(JobPath) & "SizeComp.out" For Input As #31
  Line Input #31, WSTRING
  If Trim(WSTRING) <> MODSIM.JOBNAME Then
    MsgBox "Could not find experimental size distribution data for this job " & JOBNAME & vbCrLf & "Job name on data file " & WSTRING, vbExclamation, "WARNING"
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
          Input #31, CumSize(I), Cum(I)
          CumSize(I) = 1000 * CumSize(I)
          Cum(I) = 100 * Cum(I)
        Next I
        Call DISPNT(CumSize, Cum, NDC, 1, "ALN", "LINTR")
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

Sub GetStreamSizeDistribution(StreamNumber, CumSize() As Single, Cum() As Single, NDC As Integer, JOBNAME As String)
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
        JOBNAME = Trim(Mid(WSTRING, 13))
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
            Input #31, CumSize(I), Cum(I)
          Next I
        End If
      Exit Do
      End If
    End If
  Loop
  Close #31
  Exit Sub
ErrHandler:
  MsgBox "Could not find the data that was requested", vbExclamation, "WARNING"
  Close #31
  Exit Sub
End Sub

Sub SaveSizeDistribution(StreamNumber As Integer)
'************************************************
  Dim FileName As String, ReturnFile As String, JOBNAME As String
  Dim I As Integer, resp As Integer
  Dim NDC As Integer, CumSize(MaximumSizeClasses) As Single, Cum(MaximumSizeClasses) As Single
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
  MODSIM.CommonDialog1.FileName = DataSetName
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
  
  Call GetStreamSizeDistribution(StreamNumber, CumSize(), Cum(), NDC, JOBNAME)
  Open FileName For Output As #31
  Print #31, "PSD_PLOT from MODSIM"
  Print #31, "1, 1, 0, 2"
  Print #31, "Data set  1,"; NDC - 1
  Print #31, DataSetName
  For I = 1 To NDC - 1
    Print #31, CumSize(I); ", "; Cum(I)
  Next I
  Close #31
  Exit Sub
  
ErrHandler:
  Close #31
  MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub GetSavedSizeDistribution(DataSetName As String, NDC As Integer, CumSize() As Single, Cum() As Single)
'********************************************************************************************************
  Dim FileName As String
  Dim I As Integer
  
  On Error GoTo ErrHandler
  ' Set filters
  MODSIM.CommonDialog1.Filter = "Comma delimited files (*.csv)|*.csv"
  MODSIM.CommonDialog1.FilterIndex = 1
  MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  MODSIM.CommonDialog1.ShowOpen
  
  'Strip off any file type extension
  WSTRING = Trim(MODSIM.CommonDialog1.FileName)
  
  Open MODSIM.CommonDialog1.FileName For Input As #31
  Line Input #31, WSTRING
  If WSTRING <> "PSD_PLOT from MODSIM" Then
    MsgBox "Incorrect file format", vbCritical, "ERROR"
    Close #31
    Exit Sub
  Else
    Line Input #31, WSTRING
    Line Input #31, WSTRING
    If Mid(WSTRING, 1, 11) <> "Data set  1" Then GoTo ErrHandler
    NDC = Val(Mid(WSTRING, 13)) + 1
    Line Input #31, DataSetName
    For I = 1 To NDC - 1
      Input #31, CumSize(I), Cum(I)
    Next I
    Close #31
  End If
  Exit Sub
  
ErrHandler:
  Close #31
  MsgBox "Error reading file", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub ExportSizeDistributionData(ODATA As STREAMDATA)
'**************************************************
  Dim FileName As String, ReturnFile As String, JOBNAME As String
  Dim I As Integer, resp As Integer
  
  On Error GoTo ErrHandler1
  If ODATA.STREAM_NAME = "No Name" Then
    ODATA.STREAM_NAME = InputBox("Specify a stream name for this size distribution", "Save data")
    If ODATA.STREAM_NAME = "" Then
      Exit Sub
    End If
  End If
  ' Set filters
  MODSIM.CommonDialog1.Filter = "Comma delimited files (*.csv)|*.csv"
  MODSIM.CommonDialog1.FilterIndex = 1
  MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  MODSIM.CommonDialog1.ShowSave
  
  On Error GoTo ErrHandler

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
  
  Open FileName For Output As #31
  Print #31, "PSD_PLOT"
  Print #31, "1, 1, 0, 2"
  Print #31, "Data set  1,"; ODATA.NDC - 1
  Print #31, ODATA.STREAM_NAME
  For I = 1 To ODATA.NDC - 1
    Print #31, ODATA.CumSize(I); ", "; ODATA.Fraction(I)
  Next I
  Close #31
  Exit Sub
  
ErrHandler1:
  Exit Sub
  
ErrHandler:
  Close #31
  MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub ImportSizeDistributionData(ODATA As STREAMDATA)
'**************************************************
  Dim FileName As String
  Dim I As Integer, resp As Integer, n As Integer
  Dim DataSetFound As Boolean
  
  On Error GoTo ErrHandler1
  ' Set filters
  MODSIM.CommonDialog1.Filter = "Comma delimited files (*.csv)|*.csv"
  MODSIM.CommonDialog1.FilterIndex = 1
  MODSIM.CommonDialog1.Flags = cdlOFNHideReadOnly
  MODSIM.CommonDialog1.InitDir = Trim(JobSavePath)
  MODSIM.CommonDialog1.ShowOpen
  
  FileName = MODSIM.CommonDialog1.FileName
  On Error GoTo ErrHandler
  Open FileName For Input As #31
  DataSetFound = False
  Do While Not EOF(31)
    Line Input #31, WSTRING
    If Mid(WSTRING, 1, 8) = "Data set" Then
      DataSetFound = True
      ODATA.NDC = Val(Mid(WSTRING, 13)) + 1
      If ODATA.NDC <= 1 Then
        MsgBox "Too few size classes", vbExclamation, "ERROR"
        Close #31
        Exit Sub
      End If
      If ODATA.NDC > 20 Then
        MsgBox "Too many size classes", vbExclamation, "ERROR"
        Close #31
        Exit Sub
      End If
      Line Input #31, WSTRING
      ODATA.STREAM_NAME = Trim(WSTRING)
      For I = 1 To ODATA.NDC - 1
        Line Input #31, WSTRING
        n = InStr(1, WSTRING, ",")
        ODATA.CumSize(I) = Val(Mid(WSTRING, 1, n - 1))
        ODATA.Fraction(I) = Val(Mid(WSTRING, n + 1))
      Next I
    End If
  Loop
  If Not DataSetFound Then
    MsgBox "Data set not found", vbExclamation, "ERROR"
  End If
  Close #31
  Exit Sub
ErrHandler1:
  Exit Sub
ErrHandler:
  Close #31
  MsgBox "Error reading file", vbCritical, "ERROR"
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

