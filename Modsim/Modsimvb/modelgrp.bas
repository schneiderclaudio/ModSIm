Attribute VB_Name = "MdlModelGRP"
Option Explicit
Private Distance As Single, Theta As Single, PHI As Single

Sub ModelGraphs(UnitNumber As Integer, Model As String, Index As Integer)
'************************************************************************
' Plots any special graphs for specific models.
  Dim KEY As String * 4
  Dim ID As Integer, IND As Integer
  
  On Error GoTo ErrHandler

  'Get the data for the graphs from a file.
  Open RTrim(JobPath) & "ModelGrp.OUT" For Input As #22

  'Search for the data for this model
  KEY = ""
  Do While (KEY <> Model)
    If EOF(22) Then
      MsgBox "Could not find data for this unit", vbCritical, "ERROR"
      Close (22)
      Exit Sub
    End If
    Input #22, KEY
    If KEY = Model Then
    Input #22, ID, IND
      If ID <> UnitNumber Or IND <> Index Then KEY = ""
    End If
  Loop

  Select Case (Model)

    Case "SB16", "MILL", "GMIL", "GMI1", "GMSU", "RODL", "RODM", "HFMI", "HFML", "HFSU", "SAGM", "FAGM", "SAGT", "FAGT", "SAGP", "SAGL", "HPGR"
        If Index = 0 Then Call SelFuncGraph
        If Index = 2 Then Call AMDGRP
        
    Case "CYCL", "CYCA", "CYCB", "ELUT", "WICY", "SCR2", "KSCN", "PSCN"
        Call ClassificationFuncGraph
        
    Case "CSCN"
        Call MultClassificationFuncGraph
        
    Case "SPIR", "KELL", "LISP", "DYNA", "DMCY", "BAUJ", "BATJ", "NORW", "WEMC", "DREW", "CHAN", "SLIP", "WASH", "SHAK", "DRUM", "WOCY"
        Call PartitionFuncGraph
        
    Case ("KYNC")
        Call THICKEN_CONSTRUCT

    Case ("CONE")
        Call StratificationProfiles

    Case ("SJIG")
        Call JigStratprofiles

  End Select
  Close (22)
  Exit Sub

ErrHandler:
  Close #22
  Exit Sub
End Sub

Sub SelFuncGraph()
'*****************
' Plots the selection function for a mill.
  Dim size(100) As Single, SelFunc(100) As Single
  Dim XMIN As Single, XMAX As Single, XO As Single, XF As Single, NDEC As Integer
  Dim TITLE As String
  Dim LabLen As Integer, NDC As Integer, I As Integer
  Dim Model As String

  Input #22, TITLE
  LabLen = Len(TITLE)
  Input #22, Model
  Input #22, NDC
  For I = 1 To NDC
    Input #22, size(I), SelFunc(I)
    size(I) = size(I) * 1000
  Next
  XMIN = MINVAL(SelFunc, NDC)
  XMAX = MAXVAL(SelFunc, NDC)
  If CHPAGE() > 0 Then
    Call LBSIZE(14, 23)
    Call MGMOVE(300, 730)
    Call LABELC(TITLE, LabLen)
    Call BBOX(150, 100, 800, 600)
    Call LOGLIM(size(1), size(NDC), XO, XF, NDEC)
    Call LGAXIS(XO, XF, 1, "Y", "Y")
    Call LABAX("Particle size", 13, "mm", 2, 13, 1)
    Call LOGLIM(XMIN, XMAX, XO, XF, NDEC)
    Call LGAXIS(XO, XF, 2, "Y", "Y")
    If (Model = "Austin") Then
      Call LABAX("Selection function", 18, "min~-1~", 7, 13, 2)
    ElseIf Model = "Herbst" Then
      Call LABAX("Selection function", 18, "t/kWh", 5, 13, 2)
    End If
    Call PLGRPH(size, SelFunc, NDC, "ALN", "ALN")
    Call GROUTCOORD
  End If

  Exit Sub

End Sub

Sub ClassificationFuncGraph()
'****************************
'Draws the classification function for a classifier
  Dim size(100) As Single, ClassFunc(100) As Single
  Dim XO As Single, XF As Single, NDEC As Integer
  Dim TITLE As String
  Dim LabLen As Integer, NDC As Integer, I As Integer
  Dim Model As String

  On Error GoTo ErrHandler
  Input #22, TITLE
  LabLen = Len(TITLE)
  Input #22, NDC
  If NDC < 3 Then
    MsgBox "Must have at least 3 size classes to plot a classification curve", vbInformation, "WARNING"
    Exit Sub
  End If
  For I = 1 To NDC
    Input #22, size(I), ClassFunc(I)
    size(I) = size(I) * 1000
  Next
  If CHPAGE() > 0 Then
    Call LBSIZE(13, 23)
    Call MGMOVE(100, 730)
    Call LABELC(TITLE, LabLen)
    Call BBOX(150, 100, 800, 600)
    Call LOGLIM(size(1), size(NDC), XO, XF, NDEC)
    Call LGAXIS(XO, XF, 1, "Y", "Y")
    Call LABAX("Particle size", 13, "mm", 2, 13, 1)
    Call LINAX(0#, 1#, 10, 3.1, 2, "Y", "Y")
    Call LABAX("Classification function", 23, "", 0, 13, 2)
    Call PLGRPH(size, ClassFunc, NDC, "ALN", "LINTR")
    Call GROUTCOORD
  End If
  Exit Sub
ErrHandler:
  MsgBox "Error when attempting to plot classification function", vbInformation, "Warning"
  Exit Sub
End Sub

Sub MultClassificationFuncGraph()
'********************************
'Draws the classification function for a classifier with three output streams
  Dim size(100) As Single, ClassFunc(100) As Single
  Dim XO As Single, XF As Single, NDEC As Integer
  Dim TITLE As String
  Dim LabLen As Integer, NDC As Integer, I As Integer
  Dim Model As String

  On Error GoTo ErrHandler
  Input #22, TITLE
  LabLen = Len(TITLE)
  Input #22, NDC
  If NDC < 3 Then
    MsgBox "Must have at least 3 size classes to plot a classification curve", vbInformation, "WARNING"
    Exit Sub
  End If
  For I = 1 To NDC
    Input #22, size(I), ClassFunc(I)
    size(I) = size(I) * 1000
  Next
  Call LNTYPE(1)
  If CHPAGE() > 0 Then
    Call LBSIZE(13, 23)
    Call MGMOVE(100, 730)
    Call LABELC(TITLE, LabLen)
    Call BBOX(150, 100, 700, 500)
    Call LOGLIM(size(1), size(NDC), XO, XF, NDEC)
    Call LGAXIS(XO, XF, 1, "Y", "Y")
    Call LABAX("Particle size", 13, "mm", 2, 13, 1)
    Call LINAX(0#, 1#, 10, 3.1, 2, "Y", "Y")
    Call LABAX("Classification function", 23, "", 0, 13, 2)
    Call PLGRPH(size, ClassFunc, NDC, "ALN", "LINTR")
    Call LINIDC(150, 620, 50, "Oversize", 8)
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    For I = 1 To NDC
      Input #22, size(I), ClassFunc(I)
      size(I) = size(I) * 1000
    Next
    Call LNTYPE(2)
    Call COLOUR(9)
    Call PLGRPH(size, ClassFunc, NDC, "ALN", "LINTR")
    Call LINIDC(400, 620, 50, "Middling", 8)
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    Line Input #22, WSTRING
    For I = 1 To NDC
      Input #22, size(I), ClassFunc(I)
      size(I) = size(I) * 1000
    Next
    Call LNTYPE(3)
    Call COLOUR(12)
    Call PLGRPH(size, ClassFunc, NDC, "ALN", "LINTR")
    Call LINIDC(650, 620, 50, "Fines", 5)
      
    Call GROUTCOORD
  End If
  Exit Sub
ErrHandler:
  MsgBox "Error when attempting to plot classification function", vbInformation, "Warning"
  Exit Sub
End Sub

Sub PartitionFuncGraph()
'***********************
'Draws the partition function for a gravity separator
  Dim Spgr(100) As Single, ClassFunc(100) As Single
  Dim XO As Single, XF As Single, NDEC As Integer
  Dim TITLE As String
  Dim LabLen As Integer, NGC As Integer, I As Integer

  On Error GoTo ErrHandler
  Input #22, TITLE
  LabLen = Len(TITLE)
  Input #22, NGC
  If NGC < 3 Then
    MsgBox "Must have at least 3 grade classes to plot a partition curve", vbInformation, "WARNING"
    Exit Sub
  End If
  For I = 1 To NGC
    Input #22, Spgr(I), ClassFunc(I)
  Next
  If CHPAGE() > 0 Then
    Call LBSIZE(13, 23)
    Call MGMOVE(100, 730)
    Call LABELC(TITLE, LabLen)
    Call BBOX(150, 100, 800, 600)
    XO = BLIMNarrow(Spgr(1))
    XF = ULIMNarrow(Spgr(NGC))
    Call LINAX(XO, XF, 10, 3.1, 1, "Y", "Y")
    Call LABAX("Specific gravity", 16, "", 0, 13, 1)
    Call LINAX(0#, 1#, 10, 3.1, 2, "Y", "Y")
    Call LABAX("Partition function", 18, "", 0, 13, 2)
    Call PLGRPH(Spgr, ClassFunc, NGC, "LINTR", "LINTR")
    Call DISPNT(Spgr, ClassFunc, NGC, 1, "LINTR", "LINTR")
    Call GROUTCOORD
  End If
  Exit Sub
ErrHandler:
  MsgBox "Error when attempting to plot partition function", vbInformation, "Warning"
  Exit Sub
End Sub
Sub THICKEN_CONSTRUCT()
'**********************
' Draws the flux curve and operating line for a Kynch thickener.
  Dim Alpha(10) As Single, BETA(10) As Single
  Dim Vt As Single, Nterms As Integer
  Dim PsiF As Single, CI As Single, PsiMax As Single, Cm As Single, Cd As Single, CO As Single
  Dim CN As Single, DelC As Single, YN As Single
  Dim c(101) As Single, PSI(101) As Single
  Dim TITLE As String, LabLen As Integer
  Dim OvLoad As Boolean
  Dim I As Integer

  'Get the data for the construction from the file.
  Input #22, TITLE
  LabLen = Len(TITLE)
  Input #22, OvLoad
  Input #22, Vt, Nterms
  For I = 1 To Nterms
    Input #22, Alpha(I), BETA(I)
  Next I
  Input #22, PsiF, CI, PsiMax, Cm, Cd, CO
  CN = ULIM(Cd)
  YN = ULIM(PsiMax)
  DelC = CN / 100#
  For I = 0 To 100
    c(I + 1) = I * DelC
    PSI(I + 1) = Flux(c(I + 1), Alpha, BETA, Nterms, Vt)
  Next

  If CHPAGE() > 0 Then
    Call LBSIZE(14, 23)
    Call MGMOVE(300, 730)
    Call LABELC(TITLE, LabLen)
    Call BBOX(150, 100, 800, 600)
    Call LINAX(0#, CN, 10, 5#, 1, "Y", "Y")
    Call LINAX(0#, YN, 10, 5.2, 2, "Y", "Y")
    Call LABAX("Slurry concentration", 20, "kg/m~3~", 7, 13, 1)
    Call LABAX("Settling flux", 13, "kg/m~2~ s", 9, 13, 2)
    Call PLSMGR(c, PSI, 101, 1, "LINTR", "LINTR")
    c(1) = 0#
    PSI(1) = PsiF
    c(2) = Cd
    PSI(2) = 0#
    Call PLGRPH(c, PSI, 2, "LINTR", "LINTR")

    'Plot the upper zone operating line if the thickener is overloaded.
    If (OvLoad) Then
      c(1) = 0#
      PSI(1) = 0#
      c(2) = CO
      PSI(2) = Flux(CO, Alpha, BETA, Nterms, Vt)
      Call PLGRPH(c, PSI, 2, "LINTR", "LINTR")
      Call MGMOVE(500, 600)
      Call LBSIZE(13, 23)
      Call LABELC("The thickener is overloaded", 27)
    End If
    Call GROUTCOORD
  End If
Exit Sub

End Sub

Function Flux(c As Single, Alpha() As Single, BETA() As Single, Nterms As Integer, Vt As Single) As Single
'*************************************
' Calculates the flux.
  Dim Ws1 As Single, I As Integer
  On Error GoTo ErrHandler
  Ws1 = 1#
  For I = 1 To Nterms
    Ws1 = Ws1 + Alpha(I) * c ^ BETA(I)
  Next
  Flux = Vt * c / Ws1
  Exit Function

ErrHandler:
  MsgBox "Error when evalauating settling flux", vbCritical, "ERROR"
  Exit Function
End Function

Sub StratificationProfiles()
'***************************
' Plots the equilibrium stratification profiles
  Dim TITLE1 As String, TITLE2 As String, TITLE3 As String
  Dim WSTRING As String
  Dim Len1 As Integer, Len2 As Integer, Len3 As Integer
  Dim IDText As String
  Dim NGC As Integer
  Dim I As Integer, J As Integer, N As Integer, Icol As Integer
  Dim IX As Integer, IY As Integer
  Dim Stage As Integer, NSTAGES As Integer
  Dim V(101) As Single, Rho(101) As Single
  Dim c(101, 43) As Single, TOT(43) As Single
  Dim Vector(101) As Single
  Dim WS As Single, upper As Single, XLower As Single

  On Error GoTo ErrHandler

  Line Input #22, WSTRING
  TITLE1 = Mid(WSTRING, 1, 36)
  NGC = Val(Mid(WSTRING, 37))
  Line Input #22, TITLE2
  Len2 = Len(TITLE2)

' Get the number of stages
  NSTAGES = 0
  If Mid(TITLE2, 28, 2) = "SV" Then NSTAGES = 1
  If Mid(TITLE2, 28, 3) = "DSV" Then NSTAGES = 2
  If Mid(TITLE2, 28, 4) = "2DSV" Then NSTAGES = 4
  If Mid(TITLE2, 28, 4) = "3DSV" Then NSTAGES = 6
  If Mid(TITLE2, 28, 4) = "4DSV" Then NSTAGES = 8
  If Mid(TITLE2, 28, 5) = "DSVSV" Then NSTAGES = 3
  If Mid(TITLE2, 28, 6) = "2DSVSV" Then NSTAGES = 6
  If Mid(TITLE2, 28, 6) = "3DSVSV" Then NSTAGES = 9
  If Mid(TITLE2, 28, 10) = "2DSVSV.DSV" Then NSTAGES = 8
  If (NSTAGES = 0) Then
    MsgBox "Could not identify cone type", vbExclamation, "Warning"
    Exit Sub
  End If

  For Stage = 1 To NSTAGES
    Line Input #22, TITLE3
    Len3 = Len(TITLE3)
    'Read the concentration profile
    Line Input #22, WSTRING
    If WSTRING = "No flow to this stage" Then
      MsgBox "No flow to a cone in this stack", vbExclamation, "Warning"
    Else
      For N = 2 To 101
        V(N) = Val(Mid(WSTRING, 1, 6))
        For J = 1 To NGC
          c(N, J) = Val(Mid(WSTRING, 7 + 8 * (J - 1), 8))
        Next J
        Rho(N) = Val(Mid(WSTRING, 7 + 8 * NGC))
        Line Input #22, WSTRING
      Next N
      If CHPAGE() > 0 Then
        Call LNTYPE(0)
        Call COLOUR(0)
        Call LBSIZE(14, 23)
        Call MGMOVE(300, 730)
        Call LABELC(TITLE1, 36)
        Call LBSIZE(12, 23)
        Call MGMOVE(50, 700)
        Call LABELC(TITLE2, Len2)
        Call MGMOVE(50, 670)
        Call LABELC(TITLE3, Len3)
        Call BBOX(100, 70, 300, 400)
        Call LINAX(0#, 1#, 10, 4.2, 1, "Y", "Y")
        Call LINAX(0#, 1#, 10, 4.2, 2, "Y", "Y")
        Call LABAX("Concentration", 13, " ", 1, 13, 1)
        Call LABAX("Vertical position in the bed", 28, " ", 1, 13, 2)
        Icol = 8
        Call COLOUR(Icol)
        Call LBSIZE(9, 18)
        For J = 1 To NGC
          Call LNTYPE(J)
          For N = 1 To 101
            Vector(N) = c(N, J)
          Next N
          Call PLGRPH(Vector, V, 101, "LINTR", "LINTR")
          IDText = "G-Class " & CStr(J)
          Len1 = Len(IDText)
          IX = 50 + (160 * (J - 1)) Mod 800
          IY = 560 + ((J - 1) \ 5) * 20
          Call LINIDC(IX, IY, 70, IDText, Len1)
          Call CHANGECOL(Icol)
        Next J
        Call LNTYPE(0)

        'Plot the recovery profiles
        For J = 1 To NGC
          TOT(J) = 0#
          For N = 2 To 101
            TOT(J) = TOT(J) + c(N, J)
          Next N
        Next J
        For J = 1 To NGC
          c(1, J) = 0#
          For N = 2 To 101
            c(N, J) = c(N - 1, J) + c(N, J)
          Next N
          For N = 1 To 101
            c(N, J) = c(N, J) / TOT(J)
          Next N
        Next J
        Call LNTYPE(0)
        Call COLOUR(0)
        Call BBOX(550, 70, 300, 400)
        Call LINAX(0#, 1#, 10, 4.2, 1, "Y", "Y")
        Call LINAX(0#, 1#, 10, 4.2, 2, "Y", "Y")
        Call LABAX("Cumulative recovery", 19, " ", 1, 13, 1)
        Call LABAX("Vertical position in the bed", 28, " ", 1, 13, 2)
        Icol = 8
        Call COLOUR(Icol)
        Call LBSIZE(9, 18)
        For J = 1 To NGC
          Call LNTYPE(J)
          For N = 1 To 101
            Vector(N) = c(N, J)
          Next N
          Call PLGRPH(Vector, V, 101, "LINTR", "LINTR")
          Call CHANGECOL(Icol)
        Next

        'Now plot the bed density
        Call LNTYPE(0)
        WS = MAXVAL(Rho, 101)
        For upper = 2000 To 20000 Step 1000
          If WS <= upper Then Exit For
        Next upper
        WS = MINVAL(Rho, 101)
        For XLower = upper To 0# Step -1000
          If WS >= XLower Then Exit For
        Next XLower
        Call LINAX(XLower, upper, 10, 5#, 3, "N", "N")
        Call LABAX("Bed density", 11, "kg/m~3~ ", 7, 13, 3)
        Call LNTYPE(NGC + 1)
        Call PLGRPH(Rho, V, 101, "LINTR", "LINTR")
        Call LBSIZE(9, 18)
        Call LINIDC(700, 545, 60, "Bed density", 11)

        Call GROUT
      End If
    End If
  Next Stage
  Close (22)
  objectscreated = pensCreated + solidbrushesCreated + regionsCreated + fontsCreated + emfsCreated
  objectsdeleted = pensdeleted + brushesdeleted + regionsdeleted + fontsDeleted + emfsdeleted
  Exit Sub
  
ErrHandler:
  MsgBox "Could not plot stratification profiles", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub JigStratprofiles()
'*********************
' Plots the equilibrium stratification profiles
  Dim TITLE1 As String * 36
  Dim IDText As String, NL As Integer
  Dim Stage As Integer, NSTAGES As Integer
  Dim V(101) As Single, Rho(101) As Single, Vector(101) As Single
  Dim N As Integer, J As Integer, Icol As Integer
  Dim upper As Single, XLower As Single, WS As Single
  Dim IX As Integer, IY As Integer
  Dim c(1 To 101, 43) As Single, TOT(43) As Single
  Dim NGC As Integer
  
  On Error GoTo ErrHandler
  Line Input #22, WSTRING
  TITLE1 = Mid(WSTRING, 1, 36)
  NGC = Val(Mid(WSTRING, 37, 5))

' Get the number of stages
  NSTAGES = 1

  For Stage = 1 To NSTAGES
    'Read the concentration profile
    For N = 1 To 101
      Line Input #22, WSTRING
      V(N) = Val(Mid(WSTRING, 1, 6))
      For J = 1 To NGC
        c(N, J) = Val(Mid(WSTRING, 7 + 8 * (J - 1), 8))
      Next J
      Rho(N) = Val(Mid(WSTRING, 7 + 8 * NGC))
    Next N
    If CHPAGE() > 0 Then
      Call LNTYPE(0)
      Call COLOUR(0)
      Call LBSIZE(14, 23)
      Call MGMOVE(300, 730)
      Call LABELC(TITLE1, 36)
      Call BBOX(100, 70, 300, 400)
      Call LINAX(0#, 1#, 10, 4.2, 1, "Y", "Y")
      Call LINAX(0#, 1#, 10, 4.2, 2, "Y", "Y")
      Call LABAX("Concentration", 13, " ", 1, 13, 1)
      Call LABAX("Vertical position in the bed", 28, " ", 1, 13, 2)
      Icol = 8
      Call COLOUR(Icol)
      Call LBSIZE(9, 18)
      For J = 1 To NGC
        Call LNTYPE(J)
        For N = 1 To 101
          Vector(N) = c(N, J)
        Next N
        Call PLGRPH(Vector, V, 101, "LINTR", "LINTR")
        IDText = "G-Class " & CStr(J)
        NL = Len(IDText)
        IX = 50 + (160 * (J - 1)) Mod 800
        IY = 560 + ((J - 1) \ 5) * 20
        Call LINIDC(IX, IY, 70, IDText, NL)
        Call CHANGECOL(Icol)
      Next J
      Call LNTYPE(0)

      'Plot the recovery profiles
      For J = 1 To NGC
        TOT(J) = 0#
        For N = 2 To 101
          TOT(J) = TOT(J) + c(N, J)
        Next N
      Next J
      For J = 1 To NGC
        c(1, J) = 0#
        For N = 2 To 101
          c(N, J) = c(N - 1, J) + c(N, J)
        Next N
        For N = 2 To 101
          If TOT(J) > 0# Then c(N, J) = c(N, J) / TOT(J)
        Next N
      Next J
      Call LNTYPE(0)
      Call COLOUR(0)
      Call BBOX(550, 70, 300, 400)
      Call LINAX(0#, 1#, 10, 4.2, 1, "Y", "Y")
      Call LINAX(0#, 1#, 10, 4.2, 2, "Y", "Y")
      Call LABAX("Cumulative recovery", 19, " ", 1, 13, 1)
      Call LABAX("Vertical position in the bed", 28, " ", 1, 13, 2)
      Icol = 8
      Call COLOUR(Icol)
      Call LBSIZE(9, 18)
      For J = 1 To NGC
        Call LNTYPE(J)
        For N = 1 To 101
          Vector(N) = c(N, J)
        Next N
        Call PLGRPH(Vector, V, 101, "LINTR", "LINTR")
        Call CHANGECOL(Icol)
      Next J

      'Now plot the bed density
      WS = MAXVAL(Rho, 100)
      For upper = 3000 To 20000 Step 1000
        If WS <= upper Then Exit For
      Next upper
      WS = MINVAL(Rho, 100)
      For XLower = upper To 0# Step -1000
        If WS >= XLower Then Exit For
      Next XLower
      Call LINAX(XLower, upper, 10, 5#, 3, "N", "N")
      Call LNTYPE(0)
      Call LABAX("Bed density", 11, "kg/m~3~ ", 7, 13, 3)
      Call PLGRPH(Rho, V, 101, "LINTR", "LINTR")
      Call LBSIZE(9, 18)
      Call LINIDC(700, 545, 60, "Bed density", 11)
      Call GROUT
    End If
  Next Stage
  Close (22)
  Exit Sub
  
ErrHandler:
  MsgBox "Could not plot stratification profiles", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub AMDGRP()
'***********
' Plots the current AMD
  Dim Model As String * 4
  Dim size(MaximumSizeClasses) As Single
  Dim TITLE As String, NL As Integer
  Dim Bvg(12, MaximumSizeClasses, 12, MaximumSizeClasses) As Integer
  Dim NDC As Integer, NGC As Integer, NDParent As Integer, NGParent As Integer
  Dim I As Integer

  On Error GoTo ErrHandler
  MGPage(0).MousePointer = 11
' Get the data for the graph from a file.
  Line Input #22, TITLE
  If CHPAGE() <= 0 Then
    MsgBox "Could not open a new METGRAPH page", vbCritical, "ERROR"
    Exit Sub
  End If
  Call LBSIZE(9, 18)
  NL = Len(TITLE)
  Call MGMOVE(100, 700)
  Call LABELC(TITLE, NL)
  Input #22, NDC, NGC
  For I = 1 To NDC
    Input #22, size(I)
  Next I
  Input #22, Model
  Call MGMOVE(100, 670)
  Call LABELC(Model & " model was used", 19)
  Input #22, NDParent, NGParent
  Close (22)

' Open a file to receive the calculated A-M diagram
  Call READAMD(Model, Bvg, NDC, NGC)
' Plot the diagram
  Call PLOTAMD(NDC, NGC, NDParent, NGParent, Bvg, size)
  MGPage(0).MousePointer = 0
  Exit Sub
  
ErrHandler:
  MsgBox "Could not plot Andrews-Mika diagram", vbCritical, "ERROR"
  Exit Sub

End Sub


Sub READAMD(Model As String, Bvg() As Integer, NDC As Integer, NGC As Integer)
'*****************************************************************************
'  Reads a precomputed Andrews-Mika Diagram.
  Dim I As Integer, J As Integer, K As Integer, L As Integer
  Dim WSTRING As String

  On Error GoTo ErrHandler
  If Model = "BETA" Then
    Open RTrim(JobPath) & "BETAAMD.DAT" For Input As #42
  ElseIf Model = "LJUB" Then
    Open RTrim(JobPath) & "LJUBAMD.DAT" For Input As #42
  Else
    Exit Sub
  End If

  For L = 1 To NDC
    For K = 1 To NGC
      For J = L + 1 To NDC
        For I = 1 To NGC
         Input #42, Bvg(I, J, K, L)
        Next I
      Next J
      Line Input #42, WSTRING
    Next K
  Next L
  Close (42)
  Exit Sub
 
ErrHandler:
  MsgBox "Error opening file containing Andrews-Mika diagram", vbCritical, "ERROR"
  Exit Sub
End Sub


Sub PLOTAMD(NDC As Integer, NGC As Integer, NDParent As Integer, NGParent As Integer, Bvg() As Integer, size() As Single)
'************************************************************************************************************************
'Plots the Andrews-Mika diagram
  Dim AMD(12, MaximumSizeClasses) As Single
  Dim SizeLabel(MaximumSizeClasses) As String
  Dim Pmax As Single
  Dim I As Integer, J As Integer, Isize As Long
  
  On Error GoTo ErrHandler:
  For I = 1 To 12
    For J = 1 To 25
      AMD(I, J) = 0#
    Next J
  Next I
  AMD(NGParent, NDParent) = 10000#
  For J = NDParent + 1 To NDC
    For I = 1 To NGC
      AMD(I, J) = Bvg(I, J, NGParent, NDParent)
    Next
  Next

' Find the largest element of AMD and scale
  Pmax = AMD(1, 1)
  For I = 1 To NGC
    For J = 1 To NDC
      If AMD(I, J) > Pmax Then Pmax = AMD(I, J)
    Next
  Next
  For I = 1 To NGC
    For J = 1 To NDC
      AMD(I, J) = AMD(I, J) / Pmax
    Next
  Next
  
  For I = 1 To NDC
    Isize = Fix(size(I) * 1000000#)
    SizeLabel(I) = CStr(Isize)
  Next

' Plot the distributions
  Call THREE_D_BARCHART(AMD, Pmax, NGC, NDC, SizeLabel, "Particle composition  %", 23, "Particle size  \2m\1m", 21)

  Exit Sub
  
ErrHandler:
  MsgBox "Could not plot the Andrews-Mika diagram", vbCritical, "ERROR"
  Exit Sub
End Sub

Sub THREE_D_BARCHART(P() As Single, Pmax As Single, NX As Integer, NY As Integer, SizeLabs() As String, GradeAxisTitle As String, NL As Integer, SizeAxisTitle As String, NL1 As Integer)
' ***************************************************************************************************************************************************************************************
' Plots a 3-D barchart.
  Dim StartX As Integer, EndX As Integer, StepX As Integer, StartY As Integer, EndY As Integer, StepY
  Dim xspan As Single, YSPAN As Single, XMargin As Single, Ymargin As Single, XGap As Single, YGAP As Single
  Dim width As Single, depth As Single, height As Single, R As Single
  Dim XORG As Single, YORG As Single, ZORG As Single
  Dim X As Single, Y As Single
  Dim XLABEL(12) As String
  Dim LABELTEXT  As String
  Dim LabelWidth As Single
  Dim NLab As Integer
  Dim I As Integer, J As Integer

  XLABEL(1) = "    0%   "
  XLABEL(2) = "  0 - 10 "
  XLABEL(3) = " 10 - 20 "
  XLABEL(4) = " 20 - 30 "
  XLABEL(5) = " 30 - 40 "
  XLABEL(6) = " 40 - 50 "
  XLABEL(7) = " 50 - 60 "
  XLABEL(8) = " 60 - 70 "
  XLABEL(9) = " 70 - 80 "
  XLABEL(10) = " 80 - 90 "
  XLABEL(11) = " 90 -100 "
  XLABEL(12) = "   100%  "

  Distance = 500#
  Theta = 55 '120 '200
  PHI = 120
  Theta = Theta * Pi / 180#
  PHI = PHI * Pi / 180#
  xspan = 500
  XMargin = 100
  Ymargin = 80
  YSPAN = NY * (xspan - 2 * XMargin) \ NX + 2 * Ymargin
  XGap = 0.02 * xspan
  YGAP = 1.1 * XGap
  width = (xspan - 2 * XMargin) / NX
  depth = (YSPAN - 2 * Ymargin) / NY
  width = width - XGap
  depth = depth - YGAP
  height = 260
  R = Sqr((0.5 * xspan) ^ 2 + (0.5 * YSPAN) ^ 2)
  XORG = 0.5 * R * Sin(Theta + 0.25 * Pi) + 400 '140 '300
  YORG = 0.5 * R * Cos(Theta - 0.25 * Pi) - 250 '700 '300
  ZORG = -120 '0
  Page(PageNumber).XORG = XORG
  Page(PageNumber).YORG = YORG
  Page(PageNumber).SPAN = xspan
  Page(PageNumber).RANGE = YSPAN

If CHPAGE() > 0 Then
  Call VIEWPLANE(Distance, Theta, PHI)
  Call PSLNWD(0.2)
  Call MGGRAYFILL(0.8)
  Call PEDESTAL(XORG, YORG, ZORG, NX, NY, width, depth, 60#, XMargin, Ymargin, XGap, YGAP)
  Call LBSIZE(14, 26)
  LabelWidth = 6# * NL
  Call MGTOPWLAB(XORG + 0.5 * xspan + LabelWidth, YORG + YSPAN, ZORG - 40, GradeAxisTitle, NL, 1, Theta, PHI)
  Call MGBOTTOMWLAB(XORG + 0.5 * xspan - LabelWidth, YORG, ZORG - 40, GradeAxisTitle, NL, 1, Theta, PHI)
  LabelWidth = 6# * NL1
  Call MGRIGHTWLAB(XORG + xspan, YORG + 0.5 * YSPAN - 130, ZORG - 40, SizeAxisTitle, NL1, 1, Theta, PHI)
  Call MGLEFTWLAB(XORG, YORG + 0.5 * YSPAN + 130, ZORG - 40, SizeAxisTitle, NL1, 1, Theta, PHI)
  Call MGGRAYFILL(0.95)
  Call LBSIZE(9, 16)

'Draw the axis labels
  X = XORG + XMargin + 0.5 * XGap
  For J = 1 To NY
    Y = YORG + Ymargin + 0.5 * YGAP + (NY - J) * (depth + YGAP)
    NLab = Len(SizeLabs(J))
    Call MGFLOORLAB(XORG + 5, Y - 0.3 * YGAP, ZORG, SizeLabs(J), NLab, 1, Theta, PHI)
    Call MGFLOORLAB(XORG + xspan - XMargin + 5, Y - 0.3 * YGAP, ZORG, SizeLabs(J), NLab, 1, Theta, PHI)
  Next J
  For I = 1 To NX
    X = XORG + XMargin + 0.8 * XGap + (I - 1) * (width + XGap)
    Call MGFLOORLAB(X + 0.9 * XGap, YORG + 10, ZORG, XLABEL(I), 9, 2, Theta, PHI)
    Call MGFLOORLAB(X + 0.9 * XGap, YORG + YSPAN - Ymargin, ZORG, XLABEL(I), 9, 2, Theta, PHI)
  Next I

  'Draw the scale posts
  If Pmax <= 0.2 Then
    Pmax = 0.2
  ElseIf Pmax <= 0.5 Then
    Pmax = 0.5
  ElseIf Pmax <= 0.8 Then
    Pmax = 0.8
  Else
    Pmax = 1#
  End If
  Call ScalePost(XORG, YORG, ZORG, xspan, YSPAN, Pmax, height)

'Draw the bars
  If Theta > 0.5 * Pi And Theta <= 1.5 * Pi Then
    StartX = 1
    EndX = NX
    StepX = 1
  Else
    StartX = NX
    EndX = 1
    StepX = -1
  End If

  If (Theta > 0# And Theta <= Pi) Then
    StartY = 1
    EndY = NY
    StepY = 1
  Else
    StartY = NY
    EndY = 1
    StepY = -1
  End If

  For I = StartX To EndX Step StepX
    X = XORG + XMargin + 0.5 * XGap + (I - 1) * (width + XGap)
    For J = StartY To EndY Step StepY
      If J < 10 Then
        Call MGGRAYFILL(1#)   'CALL MGGRAYFILL(0.95)
      Else
        Call MGGRAYFILL(1#)
      End If
      If P(I, J) <= 0# Then Call MGGRAYFILL(0.9)
      Y = YORG + Ymargin + 0.5 * YGAP + (NY - J) * (depth + YGAP)
      Call THREED_BAR(X, Y, ZORG, width, depth, P(I, J) * height)
    Next
  Next
  Call GROUT
End If
End Sub

Sub THREED_BAR(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'********************************************************************************************************
' Sets the 3-D bar at world coordinate X Y Z

' Draw the visible faces.
  If Theta > 0.5 * Pi And Theta <= 1.5 * Pi Then
    Call RIGHTFACE(X, Y, Z, width, depth, height)
  Else
    Call LEFTFACE(X, Y, Z, width, depth, height)
  End If
  If Theta > 0# And Theta <= 180# Then
    Call BOTTOMFACE(X, Y, Z, width, depth, height)
  Else
    Call TOPFACE(X, Y, Z, width, depth, height)
  End If
  Call BARTOP(X, Y, Z, width, depth, height)
  Exit Sub
End Sub


Sub TOPFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'*****************************************************************************************************
' Construct the top face of the bar.
  Dim w As Single, d As Single, XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X, Y + d, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
  Exit Sub
End Sub

Sub BOTTOMFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'********************************************************************************************************
' Construct the bottom face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord

  w = width
  d = depth
  Call VIEWC(X, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
  Exit Sub
End Sub

Sub LEFTFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'******************************************************************************************************
' Construct the left hand face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  w = width
  d = depth
  Call VIEWC(X, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
  Exit Sub
End Sub

Sub RIGHTFACE(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'*******************************************************************************************************
' Construct the right hand face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single
  Dim Vert(4) As xyrcoord
  w = width
  d = depth
  Call VIEWC(X + w, Y, Z, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X + w, Y + d, Z, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
  Exit Sub
End Sub

Sub BARTOP(X As Single, Y As Single, Z As Single, width As Single, depth As Single, height As Single)
'****************************************************************************************************
' Construct the roof face of the bar.
  Dim w As Single, d As Single
  Dim XV As Single, YV As Single, ZV As Single

  Dim Vert(4) As xyrcoord
  w = width
  d = depth
  Call VIEWC(X + w, Y, Z + height, XV, YV, ZV)
  Vert(1).xcoord = XV
  Vert(1).ycoord = YV
  Call VIEWC(X + w, Y + d, Z + height, XV, YV, ZV)
  Vert(2).xcoord = XV
  Vert(2).ycoord = YV
  Call VIEWC(X, Y + d, Z + height, XV, YV, ZV)
  Vert(3).xcoord = XV
  Vert(3).ycoord = YV
  Call VIEWC(X, Y, Z + height, XV, YV, ZV)
  Vert(4).xcoord = XV
  Vert(4).ycoord = YV
  Call MGPOLY(5, Vert, 4)
  Exit Sub
End Sub

Sub PEDESTAL(X As Single, Y As Single, Z As Single, NX As Integer, NY As Integer, w As Single, d As Single, H As Single, XMargin As Single, Ymargin As Single, XGap As Single, YGAP As Single)
'     ****************************************************************************************************************************************************************************************
' Draw the pedestal for the 3-D bar chart
  Dim xspan As Single, YSPAN As Single
  Dim GrWidth As Single, GrHeight As Single, depth As Single
  Dim XT As Single, YT As Single
  Dim I As Integer
  GrWidth = XMargin + NX * (w + XGap) + XMargin
  depth = Ymargin + NY * (d + YGAP) + Ymargin
  Call THREED_BAR(X, Y, Z - H, GrWidth, depth, H)
' Draw the tick marks
  Call MG3DMOVE(X + XMargin, Y + Ymargin, Z)
  Call MG3DPLOT(X + GrWidth - XMargin, Y + Ymargin, Z)
  Call MG3DPLOT(X + GrWidth - XMargin, Y + depth - Ymargin, Z)
  Call MG3DPLOT(X + XMargin, Y + depth - Ymargin, Z)
  Call MG3DPLOT(X + XMargin, Y + Ymargin, Z)
  For I = 1 To NX - 1
    XT = X + XMargin + I * (w + XGap)
    Call X_TICK("+", XT, Y + Ymargin, Z, 0.5 * YGAP)
    Call X_TICK("-", XT, Y + depth - Ymargin, Z, 0.5 * YGAP)
  Next
  For I = 1 To NY - 1
    YT = Y + Ymargin + I * (d + YGAP)
    Call Y_TICK("+", X + XMargin, YT, Z, 0.5 * XGap)
    Call Y_TICK("-", X + GrWidth - XMargin, YT, Z, 0.5 * XGap)
  Next
End Sub

Sub X_TICK(ORIENT As String, X As Single, Y As Single, Z As Single, Length As Single)
'************************************************************************************
' Draws a tick on the x-axis
  Call MG3DMOVE(X, Y, Z)
  If (ORIENT = "+") Then
    Call MG3DPLOT(X, Y + Length, Z)
  Else
    Call MG3DPLOT(X, Y - Length, Z)
  End If
  Exit Sub
End Sub

Sub Y_TICK(ORIENT As String, X As Single, Y As Single, Z As Single, Length As Single)
'************************************************************************************
' Draws a tick on the y-axis
  Call MG3DMOVE(X, Y, Z)
  If (ORIENT = "+") Then
    Call MG3DPLOT(X + Length, Y, Z)
  Else
    Call MG3DPLOT(X - Length, Y, Z)
  End If
  Exit Sub
End Sub

Sub ScalePost(XORG As Single, YORG As Single, ZORG As Single, xspan As Single, YSPAN As Single, Pmax As Single, height As Single)
'********************************************************************************************************************************
  Dim PostLabel As String
  Dim upper As Single, FRACT As Single

  Call LNTYPE(0)
  Call LINEWD(0.1)
  Call COLOUR(7)

   upper = Pmax * height
  'Top thread and posts first.
  If Theta > Pi And Theta < 2 * Pi Then
    Call ThreeDLINE(XORG, YORG, ZORG + upper, XORG + xspan, YORG, ZORG + upper)
    Call ThreeDLINE(XORG, YORG, ZORG + 0#, XORG, YORG, ZORG + upper)
    Call MGTOPWLAB(XORG, YORG, ZORG + upper, "TOPW", 4, 1, Theta, PHI)
  End If
  If Theta > 1.25 * Pi Or Theta < 0.25 * Pi Then
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + upper, XORG + xspan, YORG + YSPAN, ZORG + upper)
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + 0#, XORG, YORG + YSPAN, ZORG + upper)
    Call MGRIGHTWLAB(XORG + xspan, YORG, ZORG + upper, "RIGW", 4, 1, Theta, PHI)
  End If
  If (Theta > 1.5 * Pi Or Theta < 0.5 * Pi) Then
    For FRACT = 0.2 To Pmax + 0.0001 Step 0.2
      Call ThreeDLINE(XORG + xspan, YORG + YSPAN, ZORG + FRACT * upper, XORG, YORG + YSPAN, ZORG + FRACT * upper)
      PostLabel = Format(FRACT, "#0.00")
      Call MGBOTTOMWLAB(XORG + 10, YORG + YSPAN + 5, ZORG + FRACT * upper, PostLabel, 4, 1, Theta, PHI)
    Next
    Call MGBOTTOMWLAB(XORG - 5, YORG + YSPAN, ZORG + 10#, "Fraction in class", 17, 2, Theta, PHI)
    Call ThreeDLINE(XORG + xspan, YORG + YSPAN, ZORG + 0#, XORG + xspan, YORG + YSPAN, ZORG + Pmax * upper)
    Call ThreeDLINE(XORG, YORG + YSPAN, ZORG + 0#, XORG, YORG + YSPAN, ZORG + Pmax * upper)
  End If
  If Theta > 1.75 * Pi Or Theta < 0.75 * Pi Then
    For FRACT = 0.2 To Pmax + 0.0001 Step 0.2
      Call ThreeDLINE(XORG + xspan, YORG, ZORG + FRACT * upper, XORG + xspan, YORG + YSPAN, ZORG + FRACT * upper)
      PostLabel = Format(FRACT, "#0.00")
      Call MGLEFTWLAB(XORG + xspan, YORG + YSPAN - 10, ZORG + FRACT * upper, PostLabel, 4, 1, Theta, PHI)
    Next
    Call ThreeDLINE(XORG + xspan, YORG, ZORG + 0#, XORG + xspan, YORG, ZORG + Pmax * upper)
  End If
 '*
' *0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper )
' Call ThreeDLINE(0.5 * SIZE, 0.5 * SIZE * T30, PlinthHeight + Upper, -0.5 * SIZE, 0.5 * SIZE * T30, PlinthHeight + Upper)
'  !Now the intermediate thread
'  CALL ThreeDLINE(-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2,0.0,-0.5*SIZE/C30,PlinthHeight+Upper/2)
'  CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PlinthHeight+Upper/2,0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2 )
'  CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2,-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper/2 )
  Call RESLWD
  Call COLOUR(0)
  Call LNTYPE(0)

  'Now the posts

' *-0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper+10)
'  CALL PostTick(1,SIZE,PlinthHeight,0.5*Upper)
'  CALL ThreeDLINE(0.0,-0.5*SIZE/C30,PlinthHeight,0.0,-0.5*SIZE/C30,PlinthHeight+Upper+10)
'  CALL PostTick(2,SIZE,PlinthHeight,0.5*Upper)
'  CALL ThreeDLINE(0.5*SIZE,0.5*SIZE*T30,PlinthHeight,0.5*SIZE,0.5*SIZE*T30,PlinthHeight+Upper+10)
'  CALL PostTick(3,SIZE,PlinthHeight,0.5*Upper)
  End Sub

Sub PostTick(Vertex As Single, size As Single, PHeight As Single, upper As Single)
' **********************************************************************************
' Ticks the vertical scale posts
  Dim TL As Single
  Dim T30 As Single, C30 As Single
   
   C30 = Cos(Pi / 6)
   T30 = Tan(Pi / 6)
   TL = 0.02 * size
   If (Vertex = 1) Then
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PHeight + upper, -0.5 * size + TL, 0.5 * size * T30, PHeight + upper)
     Call ThreeDLINE(-0.5 * size, 0.5 * size * T30, PHeight + upper, -0.5 * size + 0.5 * TL, 0.5 * size * T30 - TL * C30, PHeight + upper)
   End If
   If (Vertex = 2) Then
     Call ThreeDLINE(0#, -0.5 * size / C30, PHeight + upper, -0.5 * TL, -0.5 * size / C30 + TL / C30, PHeight + upper)
     Call ThreeDLINE(0#, -0.5 * size / C30, PHeight + upper, 0.5 * TL, -0.5 * size / C30 + TL / C30, PHeight + upper)
   End If
   If (Vertex = 3) Then
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PHeight + upper, 0.5 * size - TL, 0.5 * size * T30, PHeight + upper)
     Call ThreeDLINE(0.5 * size, 0.5 * size * T30, PHeight + upper, 0.5 * size - 0.5 * TL, 0.5 * size * T30 - TL * C30, PHeight + upper)
   End If
End Sub

Function MINVAL(A() As Single, N As Integer) As Single
'*****************************************************
'Finds the minimum value in the vector A. Minimum value must b > 0
  Dim I As Integer
  MINVAL = A(1)
  For I = 2 To N
    If A(I) < MINVAL And A(I) > 0# Then MINVAL = A(I)
  Next I
End Function

Function MAXVAL(A() As Single, N As Integer) As Single
'*****************************************************
'Finds the maximum value in the vector A.
  Dim I As Integer
  MAXVAL = A(1)
  For I = 2 To N
    If A(I) > MAXVAL Then MAXVAL = A(I)
  Next I
End Function

