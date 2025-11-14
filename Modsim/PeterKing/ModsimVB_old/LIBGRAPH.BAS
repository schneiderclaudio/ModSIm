Attribute VB_Name = "MdlLibgraph"
Option Explicit

Private STR_NAME(0 To 100) As String

Sub LibgraphPlot()
'*****************
' Program to plot the liberation spectra

  Dim FG(15, 100) As Single, GCLAS(15) As Single, Cum(15) As Single
  Dim COMPARE(15, 100) As Single
  Dim NGCSTR(100) As Integer
  Dim STRELIST(100) As Integer
  Dim STRFLAG(100) As Boolean, COMPFL As Boolean, COMPSTR(100) As Boolean
  Dim NOS As Integer, ISTR As Integer, NSTREAMS As Integer
  Dim Icolor As Integer, ITYPE As Integer, ISYM As Integer
  Dim WSTRING As String, KEYWRD As String
  Dim NGC As Integer
  Dim J As Integer, I As Integer, N As Integer
  Dim WS As Single
  
  For I = 1 To 100
    STRFLAG(I) = False
    COMPSTR(I) = False
  Next I

' Read the spectra from the file
  On Error GoTo 9991
  Open RTrim(JobPath) & "LIBDISP.DAT" For Input As #31

  If EOF(31) Then
    MsgBox "No liberation data is available", vbExclamation, "Warning"
    Close (31)
    Exit Sub
  End If
  Input #31, NGC
10:
    Line Input #31, WSTRING
    KEYWRD = Mid(WSTRING, 1, 6)
    ISTR = Val(Mid(WSTRING, 7))
    If KEYWRD <> "Stream" Then GoTo 19
    STRFLAG(ISTR) = True
    For J = 1 To NGC
      Input #31, I, FG(J, ISTR)
    Next
  GoTo 10
19:
  Close (31)

20:
  'Get the information from the libgraph form.
  For I = 0 To 100
    STR_NAME(I) = " "
  Next I
  NSTREAMS = libgraph.StreamList.ListCount
  For I = 1 To NSTREAMS
    J = Val(libgraph.StreamList.List(I - 1))
    N = InStr(3, libgraph.StreamList.List(I - 1), " ")
    If N = 0 Then
      STR_NAME(J) = " "
    Else
      STR_NAME(J) = Trim(Mid(libgraph.StreamList.List(I - 1), N))
    End If
  Next I
  NSTREAMS = libgraph.GraphList.ListCount
  For I = 1 To NSTREAMS
    STRELIST(I) = Val(libgraph.GraphList.List(I - 1))
  Next I
  'Check if experimental data is to be plotted for comparison.
  If libgraph.ChkExp.Value = 1 Then
    COMPFL = True
  Else
    COMPFL = False
  End If

  If (COMPFL) Then
    On Error GoTo 9993
    Open RTrim(JobPath) & "LIBCOMP.OUT" For Input As #34
    If EOF(34) Then GoTo 9003
    Input #34, WSTRING
    If Trim(WSTRING) <> MODSIM.JOBNAME Then
      MsgBox "The jobname on the data file is " & WSTRING & ". This could indicate a data missmatch", vbExclamation, "Information"
    End If
110:
    Line Input #34, WSTRING
    If Mid(WSTRING, 1, 11) = "END OF FILE" Then GoTo 9003
    If Mid(WSTRING, 1, 10) <> "Stream    " Then
      MsgBox "Error in file LIBCOMP.OUT", vbCritical, "ERROR"
      GoTo 9003
    Else
      N = Val(Mid(WSTRING, 11, 4))
      Line Input #34, WSTRING
      If Mid(WSTRING, 1, 10) <> "Lib dist  " Then
        MsgBox "Error in file LIBCOMP.OUT", vbCritical, "ERROR"
        GoTo 9003
      Else
        NGCSTR(N) = Val(Mid(WSTRING, 11, 4))
        If NGCSTR(N) > 1 Then COMPSTR(N) = True
        For I = 1 To NGCSTR(N)
          Input #34, COMPARE(I, N)
        Next I
      End If
      GoTo 110
    End If
9003:
    Close (34)
  End If


' Plot the graphs
  If CHPAGE() > 0 Then
    Call BBOX(150, 100, 500, 500)
    WS = NGC - 1
    Call LINAX(1#, WS, NGC - 2, 2#, 1, "Y", "Y")
    Call LABAX("Grade class", 11, " ", 1, 13, 1)
    Call LINAX(0#, 1#, 10, 3.1, 2, "Y", "Y")
    Call LABAX("Cumulative distribution", 23, " ", 1, 13, 2)
    Call LBSIZE(10, 20)
    ISYM = 0
    ITYPE = 0
    Icolor = 7
    For J = 1 To NGC
      GCLAS(J) = J
    Next J
    For I = 1 To NSTREAMS
      NOS = STRELIST(I)
      If (STRFLAG(NOS)) Then
        ITYPE = ITYPE + 1
        If ITYPE > 10 Then ITYPE = 0
        Call LNTYPE(ITYPE)
        Call CHANGECOL(Icolor)
        ISYM = 0
        Cum(1) = FG(1, NOS)
        For J = 2 To NGC
          Cum(J) = Cum(J - 1) + FG(J, NOS)
        Next
        Call PLSMGR(GCLAS, Cum, NGC - 1, 0, "LINTR", "LINTR")
        If (COMPSTR(NOS) And COMPFL) Then
          ISYM = I
          Cum(1) = COMPARE(1, NOS)
          For J = 2 To NGCSTR(NOS)
            Cum(J) = Cum(J - 1) + COMPARE(J, NOS)
          Next
          Call DISPNT(GCLAS, Cum, NGCSTR(NOS) - 1, ISYM, "LINTR", "LINTR")
          Call STRMID(I, NOS, ISYM)
        Else
          Call STRMID(I, NOS, 0)
        End If
      End If
    Next
    Call GROUTCOORD
    Call HDCOPY(1#)
  End If
  Exit Sub
  
9991:
    MsgBox "Error reading liberation data from simulator", vbCritical, "ERROR"
    Close (31)
    Exit Sub
    
9992:
  MsgBox "Error reading data", vbCritical, "ERROR"
    Close (32)
    Exit Sub
    
9993:
    MsgBox "Error reading system data", vbCritical, "ERROR"
    Close (34)
    Exit Sub
    
ErrHandler:
    MsgBox "Error plotting liberation data", vbCritical, "ERROR"
    Exit Sub
End Sub

Sub STRMID(IWAS As Integer, NOS As Integer, ISYM As Integer)
'***********************************************************
'  DRAWS A STREAM IDENTIFIER FOR A LIBERATION DISTRIBUTION CURVE.
  Dim WSTRING As String
  Dim IX As Integer, IY As Integer
  Dim NL As Integer
' Use the actual stream number and stream name
  If STR_NAME(NOS) = " " Then
    WSTRING = "Stream " & CStr(NOS)
    NL = Len(WSTRING)
  Else
    WSTRING = CStr(NOS) & " " & STR_NAME(NOS)
    NL = Len(WSTRING)
  End If
  IX = ((IWAS - 1) * 350) Mod 1050 + 20
  IY = 620 + ((IWAS - 1) \ 3) * 30
  Call LINIDC(IX, IY, 80, WSTRING, NL)
  If (ISYM > 0) Then
    Call LNTYPE(0)
    Call SYMBOL(IX + 40, IY, ISYM)
  End If
End Sub
