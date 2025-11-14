VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form UnitMods 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Select models and parameters for units"
   ClientHeight    =   4920
   ClientLeft      =   1185
   ClientTop       =   1470
   ClientWidth     =   8895
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00FFFFFF&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4920
   ScaleWidth      =   8895
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   7920
      Top             =   1800
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "CANCEL"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   7560
      TabIndex        =   9
      Top             =   3240
      Width           =   1080
   End
   Begin VB.CommandButton cmdQuit 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "ACCEPT"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   7560
      TabIndex        =   0
      Top             =   3960
      Width           =   1080
   End
   Begin VB.CheckBox ChkHelp 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Show help"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00400000&
      Height          =   255
      Left            =   7320
      TabIndex        =   8
      ToolTipText     =   "Check for description of model"
      Top             =   1080
      Width           =   1215
   End
   Begin VB.ListBox Unit_number 
      BackColor       =   &H00FFFFFF&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   3420
      Left            =   120
      TabIndex        =   5
      ToolTipText     =   "Click to select"
      Top             =   960
      Width           =   5760
   End
   Begin VB.ListBox Models 
      BackColor       =   &H00FFFFFF&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   2790
      Left            =   6240
      TabIndex        =   6
      ToolTipText     =   "Double click to select model"
      Top             =   960
      Width           =   960
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   120
      Width           =   8655
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Models"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   6240
      TabIndex        =   7
      Top             =   720
      Width           =   960
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Unit type"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   1440
      TabIndex        =   4
      Top             =   720
      Width           =   4440
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Unit number"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   3
      Top             =   720
      Width           =   1320
   End
   Begin VB.Label JOBNAME 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "undefined"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   1200
      TabIndex        =   2
      Top             =   480
      Width           =   1200
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Job Name:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   1
      Top             =   480
      Width           =   1080
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
   End
End
Attribute VB_Name = "UnitMods"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
  Option Explicit
  
  Public ID As Integer
  Public MODEL As String
  Private N_CURR_DATA_SETS As Integer
  Private WSTRING As String * 80, KEYS As String * 4
  Private AdditionalOffset As Integer

  Public Unit As Integer
  Public NKVALS As Integer
  
  Const HelpFinder = &HB

Private Sub CmdCancel_Click()
'****************************
  Unload UnitMods
End Sub

Public Sub cmdQuit_Click()
'*************************
  Dim OUTFORM As String * 10
  Dim N As Integer, I As Integer
  Dim response As Integer
  
  On Error GoTo ErrHandler
  
  Open RTrim(JobPath) & "CURRDATA.RUN" For Output As #34

  response% = 0
  JobUnitParametersNeeded = False
  For N = 1 To NUNIT
    If UNIT_TYPE(N) > 0 Then
      WSTRING = "No model has been specified for unit" + Str$(N)
      ID = 0
      For I = 1 To NUNIT + N_CURR_DATA_SETS
          If CURRMODELDATA(I).ID = UNIT_ID(N) Then
            ID = I
            CURRMODELIndex(N) = I
          Exit For
        End If
      Next I
      If ID = 0 Then
        response% = MsgBox(WSTRING, 257, "WARNING")
        JobUnitParametersNeeded = True
      ElseIf CURRMODELDATA(ID).MODEL = "NONE" Then
        response% = MsgBox(WSTRING, 257, "WARNING")
        JobUnitParametersNeeded = True
      Else
        Print #34, "TYPE";
        Print #34, Format(N, " 000");
        Print #34, Chr$(32); CURRMODELDATA(ID).MODEL;
        Print #34, Format(CURRMODELDATA(ID).NOPAR, " 000"); Format(CURRMODELDATA(ID).ID, " 000")
        If CURRMODELDATA(ID).NOPAR > 0 Then
          Call PRNT_8G104(CURRMODELDATA(ID).PARAM(), 1, CURRMODELDATA(ID).NOPAR, 34)
        End If
      End If
      If response% = vbCancel Then Exit For
    End If
  Next N
  Print #34, "OUTC"
  Print #34, "   0   0   6   1   1   6   1   6   0   0"
  Print #34, "STOP"
  Close #34
  If response% <> vbCancel Then
    Unload UnitMods
    'MODSIM.Show
    JobModelParameterData = True
    ChangedModelParameterData = True
    SavedJob = False
    'Everything is OK so can cancel the changed flowsheet flag specially for repetitive simulations
    ChangedFlowsheet = False
   End If
   
  Exit Sub
  
ErrHandler:
  MsgBox "Error when writing model parameter data", 16, "ERROR"
  Close #34
  Exit Sub
End Sub

Private Sub Form_Load()
'***********************
  Dim N As Integer, I As Integer, I1 As Integer, J2 As Integer
  Dim DummyString As String
  Dim Dummy As Integer
  Dim JOBNAMEVAR As String
  Dim NOPAR As Integer
  
  On Error GoTo ErrHandler
  
  AdditionalOffset = 200
  Instruction.Visible = False
  
  Me.CommonDialog1.HelpFile = FileReadPath & "modsim.chm"

  If Not JobModelData Then
    MsgBox "No model specification data is available from the flowsheet", 16, "ERROR"
    Exit Sub
  End If
' Get the data on the unit models that was generated by the flowsheet editor
  Open RTrim(JobPath) + "MODSDATA.OUT" For Input As #31
  Line Input #31, JOBNAMEVAR$
  JOBNAME.Caption = JOBNAMEVAR$
  Input #31, NUNIT
  For N = 1 To NUNIT
    Input #31, UNIT_TYPE(N)
  Next N
  For N = 1 To NUNIT
    Input #31, UNIT_ID(N)
  Next N
  For N = 1 To NUNIT
    If UNIT_TYPE(N) = 1 Then
      Input #31, Dummy, NOSPLIT(N)
    End If
    If UNIT_TYPE(N) = 6 Then
     ' J2 = Match(N, 6)
      Input #31, J2
      Input #31, NOCELL(J2)
    End If
    If UNIT_TYPE(N) = 16 Then
      Input #31, Dummy, NOSPLIT(N)
    End If
    If UNIT_TYPE(N) = 18 Then
      Input #31, Dummy, NOSPLIT(N)
    End If
    If UNIT_TYPE(N) = 49 Then
      Input #31, Dummy, NOSPLIT(N)
    End If
  Next N
  Close (31)
  For N = 1 To NUNIT
    If UNIT_TYPE(N) > 0 Then
      DummyString = Str$(N) + "        " + UNIT_DESCR$(UNIT_TYPE(N))
      Unit_number.AddItem DummyString
    End If
  Next N

' Initialize the model data
  For I = 1 To 100
    CURRMODELDATA(I).MODEL = "NONE"
    CURRMODELDATA(I).ID = 0
  Next I

  If JobModelParameterData Then
  ' Get the current values of the paramaters for the units.
    Open RTrim(JobPath) + "CURRDATA.RUN" For Input As #33
    ID = 0
    Do While Not EOF(33)
      KEYS = Input$(4, #33)
      If (KEYS = "STOP") Then Exit Do
      If (KEYS = "TYPE") Then
        ID = ID + 1
        N_CURR_DATA_SETS = ID
        Input #33, Unit
        CURRMODELIndex(Unit) = ID
        MODEL$ = Input$(4, #33)
        Input #33, NOPAR%
        Input #33, CURRMODELDATA(ID).ID
        CURRMODELDATA(ID).MODEL = MODEL$
        CURRMODELDATA(ID).NOPAR = NOPAR%
        For I = 1 To CURRMODELDATA(ID).NOPAR
          Input #33, CURRMODELDATA(ID).PARAM(I)
        Next I
      Else
        Line Input #33, DummyString
      End If
    Loop
    Close #33
  End If

  'Turn the help check off
  ChkHelp.Value = 0

  'Get any system data that is needed by the unit models
  Open RTrim(JobPath) + "CURRDATA.SYD" For Input As #35
  Line Input #35, WSTRING
  Line Input #35, WSTRING
  KEYS = "    "
  Do Until KEYS = "PLAN"
    Line Input #35, WSTRING
    KEYS = Mid$(WSTRING, 1, 4)
    If (KEYS = "PHYP") Then
      I1 = Val(Mid$(WSTRING, 5, 4))
      If I1 = 5 Then
        NKVALS = Val(Mid$(WSTRING, 10, 4))
        For I = 1 To NKVALS
          Input #35, KK(I)
        Next I
        Close #35
        Exit Sub
      End If
    End If
  Loop
  Close #35
  'MSGBOX "Specific flotation rate constants have not been specified", 0, "ERROR"
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading form", 16, "ERROR"
  Exit Sub
End Sub

Private Sub MnuAccept_Click()
'****************************
  Call cmdQuit_Click
End Sub

Private Sub MnuCancel_Click()
'****************************
  Call CmdCancel_Click
End Sub

Private Sub mnuHelp_Click()
'**************************
  Dim RetVal As Long
  RetVal = Shell("HH " & RTrim(FileReadPath) & "MODSIM.chm", vbNormalFocus)
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm
End Sub

Public Sub Models_Click()
'************************
  MODEL = Models.text
  If ChkHelp.Value Then
    Help.Top = DisplayOffsetY + AdditionalOffset
    Help.Left = DisplayOffsetX + AdditionalOffset
    AdditionalOffset = AdditionalOffset + 200
    If DontShowHelp Then
      Unload Help
    Else
      Help.Show 1
    End If
  End If
End Sub

Public Sub Models_DblClick()
'***************************
  MODEL = Models.text
  Call ShowModelParameter
End Sub

Public Sub ShowModelParameter()
'******************************
  
  On Error GoTo ErrHandler
  
  If ChkHelp.Value Then
    Help.Top = DisplayOffsetY + AdditionalOffset
    Help.Left = DisplayOffsetX + AdditionalOffset
    AdditionalOffset = AdditionalOffset + 200
    If Not DontShowHelp Then Help.Show 1
    'Me.CommonDialog1.HelpCommand = HelpFinder
    'Me.CommonDialog1.ShowHelp
  ElseIf MODEL$ = "ADOR" Then
    ADOR.Top = DisplayOffsetY + AdditionalOffset
    ADOR.Left = DisplayOffsetX + AdditionalOffset
    ADOR.Show 1
  ElseIf MODEL$ = "BATJ" Then
    COAL_GEN.Top = DisplayOffsetY + AdditionalOffset
    COAL_GEN.Left = DisplayOffsetX + AdditionalOffset
    COAL_GEN.Show 1
  ElseIf MODEL$ = "BAUJ" Then
    COAL_GEN.Top = DisplayOffsetY + AdditionalOffset
    COAL_GEN.Left = DisplayOffsetX + AdditionalOffset
    COAL_GEN.Show 1
  ElseIf MODEL$ = "BLBX" Then
    Blbx.Top = DisplayOffsetY + AdditionalOffset
    Blbx.Left = DisplayOffsetX + AdditionalOffset
    Blbx.Show 1
  ElseIf MODEL$ = "CHAN" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "CONE" Then
    Cone.Top = DisplayOffsetY + AdditionalOffset
    Cone.Left = DisplayOffsetX + AdditionalOffset
    Cone.Show 1
  ElseIf MODEL$ = "CONV" Then
    Conv.Top = DisplayOffsetY + AdditionalOffset
    Conv.Left = DisplayOffsetX + AdditionalOffset
    Conv.Show 1
  ElseIf MODEL$ = "CSCN" Then
    CSCN.Top = DisplayOffsetY + AdditionalOffset
    CSCN.Left = DisplayOffsetX + AdditionalOffset
    CSCN.Show 1
  ElseIf MODEL$ = "CSCR" Then
    CSCR.Top = DisplayOffsetY + AdditionalOffset
    CSCR.Left = DisplayOffsetX + AdditionalOffset
    CSCR.Show 1
  ElseIf MODEL$ = "CYCA" Then
    Cyca.Top = DisplayOffsetY + AdditionalOffset
    Cyca.Left = DisplayOffsetX + AdditionalOffset
    Cyca.Show 1
  ElseIf MODEL$ = "CYCB" Then
    Cyca.Top = DisplayOffsetY + AdditionalOffset
    Cyca.Left = DisplayOffsetX + AdditionalOffset
    Cyca.Show 1
  ElseIf MODEL$ = "CYCL" Then
    Cycl.Top = DisplayOffsetY + AdditionalOffset
    Cycl.Left = DisplayOffsetX + AdditionalOffset
    Cycl.Show 1
  ElseIf MODEL$ = "DMCY" Then
    DMCY.Top = DisplayOffsetY + AdditionalOffset
    DMCY.Left = DisplayOffsetX + AdditionalOffset
    DMCY.Show 1
  ElseIf MODEL$ = "DREW" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "DRUM" Then
    COAL_GEN.Top = DisplayOffsetY + AdditionalOffset
    COAL_GEN.Left = DisplayOffsetX + AdditionalOffset
    COAL_GEN.Show 1
  ElseIf MODEL$ = "DSC1" Then
    DSC1.Top = DisplayOffsetY + AdditionalOffset
    DSC1.Left = DisplayOffsetX + AdditionalOffset
    DSC1.Show 1
  ElseIf MODEL$ = "DSC2" Then
    DSC2.Top = DisplayOffsetY + AdditionalOffset
    DSC2.Left = DisplayOffsetX + AdditionalOffset
    DSC2.Show 1
  ElseIf MODEL$ = "DWSC" Then
    DWSC.Top = DisplayOffsetY + AdditionalOffset
    DWSC.Left = DisplayOffsetX + AdditionalOffset
    DWSC.Show 1
  ElseIf MODEL$ = "DYNA" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "ELUT" Then
    elut.Top = DisplayOffsetY + AdditionalOffset
    elut.Left = DisplayOffsetX + AdditionalOffset
    elut.Show 1
  ElseIf MODEL$ = "FAGM" Or MODEL = "FAGT" Then
    FAGM.Top = DisplayOffsetY + AdditionalOffset
    FAGM.Left = DisplayOffsetX + AdditionalOffset
    FAGM.Show 1
  ElseIf MODEL$ = "FLTK" Then
    Fltk.Top = DisplayOffsetY + AdditionalOffset
    Fltk.Left = DisplayOffsetX + AdditionalOffset
    Fltk.Show 1
  ElseIf MODEL$ = "FLTN" Then
    Fltn.Top = DisplayOffsetY + AdditionalOffset
    Fltn.Left = DisplayOffsetX + AdditionalOffset
    Fltn.Show 1
  ElseIf MODEL$ = "FMIL" Then
    Fmil.Top = DisplayOffsetY + AdditionalOffset
    Fmil.Left = DisplayOffsetX + AdditionalOffset
    Fmil.Show 1
  ElseIf MODEL$ = "GMIL" Then
    GMIL.Top = DisplayOffsetY + AdditionalOffset
    GMIL.Left = DisplayOffsetX + AdditionalOffset
    GMIL.Show 1
  ElseIf MODEL$ = "GMI1" Then
    Gmi1.Top = DisplayOffsetY + AdditionalOffset
    Gmi1.Left = DisplayOffsetX + AdditionalOffset
    Gmi1.Show 1
  ElseIf MODEL$ = "GMSU" Then
    GMSU.Top = DisplayOffsetY + AdditionalOffset
    GMSU.Left = DisplayOffsetX + AdditionalOffset
    GMSU.Show 1
  ElseIf MODEL$ = "GYRA" Then
    GYRA.Top = DisplayOffsetY + AdditionalOffset
    GYRA.Left = DisplayOffsetX + AdditionalOffset
    GYRA.Show 1
  ElseIf MODEL$ = "HFML" Then
    HFML.Top = DisplayOffsetY + AdditionalOffset
    HFML.Left = DisplayOffsetX + AdditionalOffset
    HFML.Show 1
  ElseIf MODEL$ = "HFMI" Then
    HFMI.Top = DisplayOffsetY + AdditionalOffset
    HFMI.Left = DisplayOffsetX + AdditionalOffset
    HFMI.Show 1
  ElseIf MODEL$ = "HFSU" Then
    HFSU.Top = DisplayOffsetY + AdditionalOffset
    HFSU.Left = DisplayOffsetX + AdditionalOffset
    HFSU.Show 1
  ElseIf MODEL$ = "KELL" Then
    KELL.Top = DisplayOffsetY + AdditionalOffset
    KELL.Left = DisplayOffsetX + AdditionalOffset
    KELL.Show 1
  ElseIf MODEL$ = "KLIM" Then
    KLIM.Top = DisplayOffsetY + AdditionalOffset
    KLIM.Left = DisplayOffsetX + AdditionalOffset
    KLIM.Show 1
  ElseIf MODEL$ = "KSCN" Then
    KSCN.Top = DisplayOffsetY + AdditionalOffset
    KSCN.Left = DisplayOffsetX + AdditionalOffset
    KSCN.Show 1
  ElseIf MODEL$ = "KYNC" Then
    KYNC.Top = DisplayOffsetY + AdditionalOffset
    KYNC.Left = DisplayOffsetX + AdditionalOffset
    KYNC.Show 1
  ElseIf MODEL$ = "LISP" Then
    LISP.Top = DisplayOffsetY + AdditionalOffset
    LISP.Left = DisplayOffsetX + AdditionalOffset
    LISP.Show 1
  ElseIf MODEL$ = "MILL" Then
    Mill.Top = DisplayOffsetY + AdditionalOffset
    Mill.Left = DisplayOffsetX + AdditionalOffset
    Mill.Show 1
  ElseIf MODEL$ = "MIXR" Then
    Mixr.Top = DisplayOffsetY + AdditionalOffset
    Mixr.Left = DisplayOffsetX + AdditionalOffset
    Mixr.Show 1
  ElseIf MODEL$ = "NOP_" Then
    NOP_.Top = DisplayOffsetY + AdditionalOffset
    NOP_.Left = DisplayOffsetX + AdditionalOffset
    NOP_.Show 1
  ElseIf MODEL$ = "NORW" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "PAN1" Then
   MsgBox "Sorry, this model is not available", 48, "NOTICE"
   Exit Sub
  ElseIf MODEL = "PSCN" Then
    PSCN.Top = DisplayOffsetY + AdditionalOffset
    PSCN.Left = DisplayOffsetX + AdditionalOffset
    PSCN.Show 1
  ElseIf MODEL = "PUMP" Then
    NOP_.Top = DisplayOffsetY + AdditionalOffset
    NOP_.Left = DisplayOffsetX + AdditionalOffset
    NOP_.Caption = "Select parameters for model PUMP"
    NOP_.Show 1
  ElseIf MODEL$ = "RODM" Then
    RODM.Top = DisplayOffsetY + AdditionalOffset
    RODM.Left = DisplayOffsetX + AdditionalOffset
    RODM.Show 1
  ElseIf MODEL$ = "RODL" Then
    RODL.Top = DisplayOffsetY + AdditionalOffset
    RODL.Left = DisplayOffsetX + AdditionalOffset
    RODL.Show 1
  ElseIf MODEL$ = "SAGM" Or MODEL = "SAGT" Then
    SAGM.Top = DisplayOffsetY + AdditionalOffset
    SAGM.Left = DisplayOffsetX + AdditionalOffset
    SAGM.Show 1
  ElseIf MODEL$ = "SCRN" Then
    SCRN.Top = DisplayOffsetY + AdditionalOffset
    SCRN.Left = DisplayOffsetX + AdditionalOffset
    SCRN.Show 1
  ElseIf MODEL$ = "SCR1" Then
    SCR1.Top = DisplayOffsetY + AdditionalOffset
    SCR1.Left = DisplayOffsetX + AdditionalOffset
    SCR1.Show 1
  ElseIf MODEL$ = "SCR2" Then
    SCR2.Top = DisplayOffsetY + AdditionalOffset
    SCR2.Left = DisplayOffsetX + AdditionalOffset
    SCR2.Show 1
  ElseIf MODEL$ = "SLIP" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "SPIR" Then
    Spir.Top = DisplayOffsetY + AdditionalOffset
    Spir.Left = DisplayOffsetX + AdditionalOffset
    Spir.Show 1
  ElseIf MODEL$ = "SPLT" Then
    Splt.Top = DisplayOffsetY + AdditionalOffset
    Splt.Left = DisplayOffsetX + AdditionalOffset
    Splt.Show 1
  ElseIf MODEL$ = "SPL1" Then
    Spl1.Top = DisplayOffsetY + AdditionalOffset
    Spl1.Left = DisplayOffsetX + AdditionalOffset
    Spl1.Show 1
  ElseIf MODEL$ = "TESK" Then
   MsgBox "Sorry, this model is not available", 48, "NOTICE"
   Exit Sub
  ElseIf MODEL$ = "VJIG" Then
    VJIG.Top = DisplayOffsetY + AdditionalOffset
    VJIG.Left = DisplayOffsetX + AdditionalOffset
    VJIG.Show 1
  ElseIf MODEL$ = "WASH" Then
    COAL_GEN.Top = DisplayOffsetY + AdditionalOffset
    COAL_GEN.Left = DisplayOffsetX + AdditionalOffset
    COAL_GEN.Show 1
  ElseIf MODEL$ = "WEMC" Then
    DENSE_MD.Top = DisplayOffsetY + AdditionalOffset
    DENSE_MD.Left = DisplayOffsetX + AdditionalOffset
    DENSE_MD.Show 1
  ElseIf MODEL$ = "WDMS" Then
    WDMS.Top = DisplayOffsetY + AdditionalOffset
    WDMS.Left = DisplayOffsetX + AdditionalOffset
    WDMS.Show 1
  ElseIf MODEL$ = "WHIM" Then
   MsgBox "Sorry, this model is not available", 48, "NOTICE"
   Exit Sub
  ElseIf MODEL$ = "WOCY" Then
    WOCY.Top = DisplayOffsetY + AdditionalOffset
    WOCY.Left = DisplayOffsetX + AdditionalOffset
    WOCY.Show 1
  ElseIf MODEL$ = "WICY" Then
    WICY.Top = DisplayOffsetY + AdditionalOffset
    WICY.Left = DisplayOffsetX + AdditionalOffset
    WICY.Show 1
  ElseIf MODEL$ = "CRSH" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  ElseIf MODEL$ = "CRS1" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  ElseIf MODEL$ = "JAW1" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  ElseIf MODEL$ = "JAW2" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  ElseIf MODEL$ = "FILT" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  ElseIf MODEL$ = "SHHD" Then
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  Else
    GENERAL.Top = DisplayOffsetY + AdditionalOffset
    GENERAL.Left = DisplayOffsetX + AdditionalOffset
    GENERAL.Show 1
  End If
  Exit Sub
  
ErrHandler:
  MsgBox " Could not display parameter input form", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub Models_GotFocus()
'****************************
Instruction.Caption = "Double click a model to select it"
Instruction.Visible = True
End Sub

Private Sub Models_KEYDown(KEYSCode As Integer, Shift As Integer)
'******************************************************************
  Dim KEYS_SPACE As Integer, KEYS_RETURN As Integer
  KEYS_SPACE = 32
  KEYS_RETURN = 13
  If (KEYSCode = KEYS_SPACE Or KEYSCode = KEYS_RETURN) Then
    Call Models_DblClick
  End If
End Sub

Private Sub Models_LostFocus()
'*****************************
Instruction.Caption = ""
Instruction.Visible = False
End Sub

Private Sub Unit_number_Click()
'*******************************
  Dim IND As Integer
  IND = Unit_number.ListIndex
  If IND >= 0 Then
    Unit = Val(Unit_number.text)
    Call ShowUnitModels(Unit)
  End If
End Sub
    
Public Function FindIndexNumber(Unit As Integer) As Boolean
'**********************************************************
'Finds the list index for unit number
  Dim IND As Integer, I As Integer
  IND = Unit_number.ListCount
  For I = 0 To IND - 1
    If Val(Unit_number.List(I)) = Unit Then
      FindIndexNumber = True
      Unit_number.ListIndex = I
    End If
  Next I
End Function

Public Sub ShowUnitModels(Unit As Integer)
'******************************************
  Dim N As Integer, M As Integer
  Dim NUM As Integer
  'Find the index of the current data if available
  ID = 0
  For N = 1 To NUNIT
    If CURRMODELDATA(N).ID = UNIT_ID(Unit) Then
      ID = N
      Exit For
    End If
  Next N
  'If no current data set, then find the next available ID.
  If ID = 0 Then
    For N = 1 To NUNIT + N_CURR_DATA_SETS
      If CURRMODELDATA(N).MODEL = "NONE" Then
        ID = N
        CURRMODELDATA(ID).ID = UNIT_ID(Unit)
        Exit For
      End If
    Next N
  Else
    MODEL = CURRMODELDATA(ID).MODEL
  End If
  NUM = Models.ListCount
  For M = 1 To NUM
    Models.RemoveItem 0
  Next M
  'Display message if no models are available for this unit.
  If UNIT_MODS(UNIT_TYPE(Unit)) = 0 Then
    MsgBox "Sorry!  No models for this unit type are licensed in this package." & Chr(13) & Chr(10) & "Contact your software supplier for the full version of MODSIM.", 64, "INFORMATION"
    Exit Sub
  Else
    For M = 1 To UNIT_MODS(UNIT_TYPE(Unit))
      If (SUBR$(UNIT_TYPE(Unit), M) = CURRMODELDATA(ID).MODEL) Then
        Models.AddItem SUBR$(UNIT_TYPE(Unit), M), 0
      Else
        Models.AddItem SUBR$(UNIT_TYPE(Unit), M)
      End If
    Next M
  End If
End Sub


