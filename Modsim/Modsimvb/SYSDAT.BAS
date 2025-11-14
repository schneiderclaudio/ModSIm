Attribute VB_Name = "Sysdat"
Option Explicit

Public Const MaximumSizeClasses As Integer = 35
Public Const MaximumGClasses As Integer = 22
Public Const MaximumSClasses As Integer = 10

Public Type CLASSDATA
    SPECIFIED As Integer
    NDC As Integer
    NGC As Integer
    NSC As Integer
    Nomins As Integer
    MineralNames(7) As String * 16
    MineralSG(7) As Single
    size(1 To MaximumSizeClasses) As Single
    MassVol As String * 4
    GRDMV(1 To MaximumGClasses, 1 To 7) As Single
    NPPROP As Integer
    INDPPM As Integer
    INDPP(50, 2) As Integer
    PPROP(500) As Single
    LISTPP(50) As Integer
    PHIA As Single
    Conv As Integer
    ITER As Integer
    TOLM As Integer
    Flags As String * 5
End Type
Public Type STREAMDATA
    NSTR As Integer
    NDC As Integer
    ID As Integer
    STREAM_NAME As String * 30
    CumSize(1 To MaximumSizeClasses) As Single
    Fraction(0 To MaximumSizeClasses) As Single
End Type
Public Type SIZEDATA
    NSTR As Integer
    NDC As Integer
    ID As Integer
    STREAM_NAME As String * 30
    FEED_RATE As Single
    PERCENT_SOLIDS As Single
    WATER_RATE As Single
    CumSize(1 To MaximumSizeClasses) As Single
    Fraction(0 To MaximumSizeClasses) As Single
    NO_GRADE_RANGES As Integer
    NO_S_RANGES As Integer
End Type
Public Type WATERDATA
    NSTR As Integer
    ID As Integer
    STREAM_NAME As String * 30
    PERCENT_SOLIDS As Single
    WATER_RATE As Single
End Type
Public Type GRADEDATA
    NSTR As Integer
    NGC As Integer
    ID As Integer
    STREAM_NAME As String * 30
    SIZE_RANGE(2) As Single
    Fraction(1 To MaximumGClasses) As Single
End Type
Public Type SDATA
    NSTR As Integer
    NSC As Integer
    NGC As Integer
    Fraction(1 To MaximumSClasses) As Single
End Type
Public Type LIBDATA
    NSTR As Integer
    NGC As Integer
    ID As Integer
    STREAM_NAME As String * 30
    Fraction(1 To MaximumGClasses) As Single
End Type
Type WashabilityData
  NGC As Integer
  LowerDensity(30) As String
  UpperDensity(30) As String
End Type
Public CURRENT As CLASSDATA
'Note: Only 6 feed streams are permitted but 30 internal streams can have
'experimental size distributions. (100 in Professional or Academic version)
Public CURRDATA(0 To 6) As SIZEDATA
Public CURRODATA(0 To 100) As STREAMDATA
'Only 20 size ranges to group grade distributions or washabilities.
Public CURRGDATA(0 To 6, 0 To 19) As GRADEDATA
'S-classes distributed over 15 grade classes.
Public CURRSDATA(0 To 14, 0 To 6) As SDATA
'36 internal streams can have experimental liberation spectra
Public CURRLDATA(0 To 36) As GRADEDATA
'30 water feed streams are allowed
Public CURRWDATA(0 To 30) As WATERDATA
Public CurrentAncData(0 To 20) As WashabilityData
Public AdditionalDistribution(10) As STREAMDATA  'For use in sizgraph.frm

Public STREAM_ID(0 To 100) As Integer

Function ALGR(X As Single) As Single
'***********************************
On Error GoTo ErrHandler
  ALGR = Log(1! / (1! - X))
  Exit Function
ErrHandler:
  MsgBox "Floating point error", vbCritical, "ERROR"
  Exit Function
End Function

Sub FRPASS(CUMSIZ() As Single, Cum() As Single, NDC As Integer, P As Single, PSIZE As Single)
'********************************************************************************************
'  CALCULATES THE FRACTION PASSING A GIVEN SIZE. DISTRIBUTION IS ASSUMED
'  TO BE LOCALLY ROSIN-RAMMLER.

Dim COND1 As Integer, COND2 As Integer
Dim ND As Integer, I As Integer
Dim XI As Single, ETA As Single
Dim Alpha As Single

On Error GoTo ErrHandler:
  If PSIZE <= 0# Then
    'MsgBox "The system is attempting to calculate the fraction passing size zero." & Chr(13) & Chr(10) & "This indicates an inconsistency in your data", 48, "WARNING"
    P = 0#
    Exit Sub
  End If
  ND = NDC - 2
  I = 1
  P = 1!
  If (CUMSIZ(1) > PSIZE) Then
    For I = 1 To ND
      If (CUMSIZ(I) <= PSIZE And Cum(I) >= 1!) Then Exit Sub
      COND1 = CUMSIZ(I) > PSIZE And CUMSIZ(I + 1) <= PSIZE And Cum(I) < 1!
      COND2 = CUMSIZ(I) <= PSIZE And Cum(I) < 1!
      If COND1 Or COND2 Then Exit For
    Next I
  End If
  If I = ND + 1 Then
    If Cum(NDC - 1) <= 0.000001 Then
      P = 0
      Exit Sub
    Else
      Alpha = Log(Cum(NDC - 2) / Cum(NDC - 1)) / Log(CUMSIZ(NDC - 2) / CUMSIZ(NDC - 1))
      P = Cum(NDC - 1) * (PSIZE / CUMSIZ(NDC - 1)) ^ Alpha
    End If
  Else
    If Cum(I) > 0.999999 And Cum(I + 1) > 0.999999 Then
      P = 1!
      Exit Sub
    End If
    If (Cum(I) <= 0.000001) Then
      P = 0!
      Exit Sub
    End If
    If (Cum(I) >= 0.999999) Then I = I + 1
    If (Cum(I + 1) <= 0.000001) Then I = I - 1
    XI = Log(PSIZE / CUMSIZ(I + 1)) / Log(CUMSIZ(I) / CUMSIZ(I + 1))
    ETA = (ALGR(Cum(I)) / ALGR(Cum(I + 1))) ^ XI
    P = 1! - (1! - Cum(I + 1)) ^ ETA
  End If
  Exit Sub
  
ErrHandler:
  MsgBox " Error in interpolating size distribution", vbCritical, "ERROR"
  Exit Sub
  End Sub

Sub PRNT_8G104(Vector() As Single, START As Integer, N As Integer, NFILE As Integer)
'***********************************************************************************
  'Print a vector in FORTRAN 8G10.4 format. Prints N elements starting at START.
  Dim I As Integer
  Dim count As Integer
  On Error GoTo ErrHandler
  count = 0
  For I = 1 To N
    count = count + 1
    If count = 8 Or I = N Then
      Print #NFILE, Format$(Vector(START + I - 1), "#.0000E+#") + " "
      count = 0
    Else
      Print #NFILE, Format$(Vector(START + I - 1), "#.0000E+#") + " ";
    End If
  Next I
  Exit Sub
  
ErrHandler:
  MsgBox "Error writng to file", vbCritical, "ERROR"
  Exit Sub
End Sub

Function ROSIN_RAMM(CumSize As Single, D632 As Single, LAMBDA As Single) As Single
'*********************************************************************************
  'Calculates the Rosin Rammler cumulative distribution
  On Error GoTo ErrHandler
  ROSIN_RAMM = 1 - Exp(-(CumSize / D632) ^ LAMBDA)
  Exit Function
  
ErrHandler:
  MsgBox "Error calculating Rosin-Rammler distribution", vbCritical, "ERROR"
  Exit Function
End Function

Sub ADD_TO_PPROP(NDATA As CLASSDATA, PROPNO As Integer, VALUES As Integer)
'*************************************************************************
  'Adds a physical property to the physical property list
  
  On Error GoTo ErrHandler
  Dim I As Integer
  'Is this property already in the PPROP list?
  For I = 1 To NDATA.NPPROP
    If NDATA.LISTPP(I) = PROPNO Then
      'Has the number of values changed?
      If NDATA.INDPP(PROPNO, 2) <> VALUES Then
        Call REMOVE_FROM_PPROP(NDATA, PROPNO, NDATA.INDPP(PROPNO, 2))
      Else
        Exit For
      End If
    End If
  Next I
  If I > NDATA.NPPROP Then
    NDATA.NPPROP = NDATA.NPPROP + 1
    NDATA.LISTPP(I) = PROPNO
    NDATA.INDPP(PROPNO, 1) = NDATA.INDPPM
    NDATA.INDPP(PROPNO, 2) = VALUES
    NDATA.INDPPM = NDATA.INDPPM + VALUES
  End If
  Exit Sub
  
ErrHandler:
  MsgBox "Error in recording a physical property", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub REMOVE_FROM_PPROP(NDATA As CLASSDATA, PROPNO As Integer, VALUES As Integer)
'******************************************************************************
  'Removes a physical property from the physical property list
  On Error GoTo ErrHandler
  Dim I As Integer, J As Integer
  'Is this property already in the PPROP list?
  For I = 1 To NDATA.NPPROP
    If NDATA.LISTPP(I) = PROPNO Then
      NDATA.NPPROP = NDATA.NPPROP - 1
      NDATA.INDPP(PROPNO, 1) = 0
      NDATA.INDPP(PROPNO, 2) = 0
      For J = I To NDATA.NPPROP
        NDATA.LISTPP(J) = NDATA.LISTPP(J + 1)
      Next J
    End If
  Next I
  Exit Sub
  
ErrHandler:
  MsgBox " Error recording a physical property", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub StartSysdata()
'*****************
  Sysdata.Top = DisplayOffsetY
  Sysdata.Left = DisplayOffsetX
  Sysdata.Show 1
End Sub

Public Sub FileConCat(File1 As String, File2 As String, File3 As String)
'***********************************************************************
  'Concatenates two files
  On Error GoTo ErrHandler
  Dim N1 As Integer, N2 As Integer, N3 As Integer
  Dim WSTRING As String
  N1 = FreeFile
  Open File1 For Input As #N1
  N2 = FreeFile
  Open File2 For Input As #N2
  N3 = FreeFile
  Open File3 For Output As #N3
  Do While Not EOF(N1)
    Line Input #N1, WSTRING
    Print #N3, WSTRING
  Loop
  Do While Not EOF(N2)
    Line Input #N2, WSTRING
    Print #N3, WSTRING
  Loop
  Close #N1
  Close #N2
  Close #N3
  Exit Sub
  
ErrHandler:
  MsgBox "Error concatenating data files", vbCritical, "ERROR"
  Exit Sub
End Sub
