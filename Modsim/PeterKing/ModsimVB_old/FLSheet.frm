VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form FLSheet 
   AutoRedraw      =   -1  'True
   BackColor       =   &H00FFFFFF&
   Caption         =   "Flowsheet Editor"
   ClientHeight    =   6840
   ClientLeft      =   105
   ClientTop       =   1695
   ClientWidth     =   11400
   FontTransparent =   0   'False
   ForeColor       =   &H00C0C0C0&
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   456
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   760
   Begin VB.Frame frameBoxes 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   975
      Left            =   9120
      TabIndex        =   3
      Tag             =   "BoxFrame"
      Top             =   120
      Width           =   2175
      Begin VB.CheckBox ChkShowFlyouts 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Show flyouts"
         Height          =   255
         Left            =   120
         TabIndex        =   6
         Top             =   600
         Value           =   1  'Checked
         Width           =   1815
      End
      Begin VB.CheckBox ChkStreamNumbers 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Show stream numbers"
         Height          =   255
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Top             =   360
         Value           =   1  'Checked
         Width           =   1935
      End
      Begin VB.CheckBox ChkUnitNumbers 
         BackColor       =   &H00FFFFFF&
         Caption         =   "Show unit numbers"
         Height          =   255
         Left            =   120
         TabIndex        =   4
         TabStop         =   0   'False
         Top             =   120
         Value           =   1  'Checked
         Width           =   1695
      End
   End
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   6585
      Width           =   11400
      _ExtentX        =   20108
      _ExtentY        =   450
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   2
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   2
            Bevel           =   2
            TextSave        =   ""
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   2
            Bevel           =   2
            TextSave        =   ""
            Key             =   ""
            Object.Tag             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.TextBox TxtData 
      Height          =   225
      Left            =   240
      TabIndex        =   0
      Top             =   6240
      Visible         =   0   'False
      Width           =   3375
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   10800
      Top             =   1800
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label LblData 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      BackColor       =   &H00C0FFFF&
      Caption         =   "     "
      Height          =   225
      Left            =   11040
      TabIndex        =   1
      Top             =   1440
      Visible         =   0   'False
      Width           =   225
   End
   Begin VB.Menu File 
      Caption         =   "File"
      Begin VB.Menu Accept 
         Caption         =   "Accept flowsheet"
      End
      Begin VB.Menu PrintFlowsheet 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuExport 
         Caption         =   "Export"
         Begin VB.Menu MnuExportPS 
            Caption         =   "Export as PostScript file"
         End
         Begin VB.Menu MnuExportMetaFile 
            Caption         =   "Export as Windows metafile"
         End
      End
      Begin VB.Menu GetFlowsheet 
         Caption         =   "Get flowsheet from file"
      End
      Begin VB.Menu Cancel 
         Caption         =   "Cancel"
      End
   End
   Begin VB.Menu Edit 
      Caption         =   "Edit"
      Begin VB.Menu Refresh 
         Caption         =   "Refresh flowsheet"
      End
      Begin VB.Menu LocCursor 
         Caption         =   "Location cursor"
      End
      Begin VB.Menu RBCursor 
         Caption         =   "Rubber-band Cursor"
      End
      Begin VB.Menu RectCursor 
         Caption         =   "Rectangular cursor"
      End
      Begin VB.Menu WaterStream 
         Caption         =   "Add water stream"
      End
      Begin VB.Menu MnuFont 
         Caption         =   "Font"
      End
      Begin VB.Menu Annotate 
         Caption         =   "Annotate"
      End
      Begin VB.Menu MnuEditAnnotation 
         Caption         =   "Edit annotation"
      End
      Begin VB.Menu mnuAddFlyout 
         Caption         =   "Add stream flyout"
      End
      Begin VB.Menu MnuIconSize 
         Caption         =   "&Set icon size"
         Shortcut        =   ^S
      End
      Begin VB.Menu ReflectIcon 
         Caption         =   "&Reflect icon"
         Shortcut        =   ^R
      End
      Begin VB.Menu MnuMove 
         Caption         =   "&Move"
         Shortcut        =   ^M
      End
      Begin VB.Menu Delete 
         Caption         =   "&Delete"
         Shortcut        =   ^D
      End
      Begin VB.Menu mnuEditSystemData 
         Caption         =   "Edit system data"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuEditModelParameterData 
         Caption         =   "Edit model parameter data"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuEditOutputFormat 
         Caption         =   "Edit output format"
         Visible         =   0   'False
      End
   End
   Begin VB.Menu MnuSelect 
      Caption         =   "Select"
      Begin VB.Menu BlackBox 
         Caption         =   "Black box"
      End
      Begin VB.Menu Classifier 
         Caption         =   "Classifier"
         Begin VB.Menu mnuCompoundScreen 
            Caption         =   "Compound single deck screen"
         End
         Begin VB.Menu DoubleScreen 
            Caption         =   "Double-deck screen"
         End
         Begin VB.Menu Elutriator 
            Caption         =   "Elutriator"
         End
         Begin VB.Menu Hydrocyclone 
            Caption         =   "Hydrocyclone"
         End
         Begin VB.Menu SieveBend 
            Caption         =   "Sieve bend"
         End
         Begin VB.Menu Screen 
            Caption         =   "Single deck screen"
         End
         Begin VB.Menu SpiralClassifier 
            Caption         =   "Spiral classifier"
         End
         Begin VB.Menu mnuWaterInjectionCyclone 
            Caption         =   "Water injection cyclone"
         End
      End
      Begin VB.Menu Conveyor 
         Caption         =   "Conveyor"
      End
      Begin VB.Menu Crusher 
         Caption         =   "Crusher"
         Begin VB.Menu JawCrusher 
            Caption         =   "Jaw crusher"
         End
         Begin VB.Menu Gyratory 
            Caption         =   "Gyratory crusher"
         End
         Begin VB.Menu ConeCrusher 
            Caption         =   "Cone crusher"
         End
      End
      Begin VB.Menu DewateringUnit 
         Caption         =   "Dewatering unit"
         Begin VB.Menu DewateringScreen 
            Caption         =   "Dewatering screen"
         End
         Begin VB.Menu Filter 
            Caption         =   "Filter"
         End
         Begin VB.Menu Thickener 
            Caption         =   "Thickener"
         End
      End
      Begin VB.Menu SegregatingBin 
         Caption         =   "Feed bin"
      End
      Begin VB.Menu Flotation 
         Caption         =   "Flotation cells or column"
         Begin VB.Menu FlotationCells 
            Caption         =   "Bank of flotation cells"
         End
         Begin VB.Menu mnuFlotColumn 
            Caption         =   "Flotation column"
         End
      End
      Begin VB.Menu GravSeparator 
         Caption         =   "Gravity separator"
         Begin VB.Menu BatacJig 
            Caption         =   "Batac jig"
         End
         Begin VB.Menu BaumJig 
            Caption         =   "Baum jig"
         End
         Begin VB.Menu Chance 
            Caption         =   "Chance sand bath"
         End
         Begin VB.Menu DenseMediumCyclone 
            Caption         =   "Dense medium cyclone"
         End
         Begin VB.Menu DenseMediumVessel 
            Caption         =   "Dense medium vessl"
         End
         Begin VB.Menu Drewboy 
            Caption         =   "Drewboy dense medium separator"
         End
         Begin VB.Menu Dynawhirlpool 
            Caption         =   "Dynawhirlpool"
         End
         Begin VB.Menu MnuKnelson 
            Caption         =   "Knelson concentartor"
         End
         Begin VB.Menu Norwalt 
            Caption         =   "Norwalt dense medium separator"
         End
         Begin VB.Menu PuddlePan 
            Caption         =   "Puddle pan"
         End
         Begin VB.Menu ReichertCone 
            Caption         =   "Reichert cone"
         End
         Begin VB.Menu ShakingTable 
            Caption         =   "Shaking table"
         End
         Begin VB.Menu ShallowBath 
            Caption         =   "Shallow bath coal washer"
         End
         Begin VB.Menu SpiralConcentrator 
            Caption         =   "Spiral concentrator"
         End
         Begin VB.Menu Teska 
            Caption         =   "Teska drum"
         End
         Begin VB.Menu WateronlyCyclone 
            Caption         =   "Water-only cyclone"
         End
         Begin VB.Menu Wemco 
            Caption         =   "Wemco drum"
         End
      End
      Begin VB.Menu MagneticSeparator 
         Caption         =   "Magnetic separator"
         Begin VB.Menu WHIMS 
            Caption         =   "Wet high intensity magnetic separator"
         End
         Begin VB.Menu WetMagDrumSeparator 
            Caption         =   "Wet magnetic drum separator"
         End
         Begin VB.Menu mnuJones 
            Caption         =   "Jones magnetic separator"
         End
      End
      Begin VB.Menu Mill 
         Caption         =   "Mill"
         Begin VB.Menu AutMill 
            Caption         =   "Autogenous mill"
         End
         Begin VB.Menu AutMillTrom 
            Caption         =   "Autogenous mill with trommel"
         End
         Begin VB.Menu RodMill 
            Caption         =   "Rod Mill"
         End
         Begin VB.Menu BallMill 
            Caption         =   "Ball mill"
         End
         Begin VB.Menu FixedRollMill 
            Caption         =   "Fixed roll mill"
         End
         Begin VB.Menu HPRollMill 
            Caption         =   "High pressure roll mill"
         End
      End
      Begin VB.Menu MixingUnit 
         Caption         =   "Mixing unit"
         Begin VB.Menu Mixer 
            Caption         =   "Mixer"
         End
         Begin VB.Menu Stockpile 
            Caption         =   "Stockpile"
         End
         Begin VB.Menu Sump 
            Caption         =   "Sump"
         End
      End
      Begin VB.Menu StreamSplittingUnit 
         Caption         =   "Stream splitting unit"
         Begin VB.Menu StreamSplitter 
            Caption         =   "Stream splitter"
         End
      End
      Begin VB.Menu Pump 
         Caption         =   "Pump"
         Begin VB.Menu CentrifugalPump 
            Caption         =   "Centrifugal pump"
         End
      End
   End
   Begin VB.Menu mnuView1 
      Caption         =   "View"
      Visible         =   0   'False
      Begin VB.Menu mnuSizeDistributionGraphs 
         Caption         =   "Size distribution graphs"
      End
      Begin VB.Menu mnuLibGraphs 
         Caption         =   "Liberation distribution graphs"
      End
   End
   Begin VB.Menu mnuRun 
      Caption         =   "Run"
      Begin VB.Menu mnuRunSimulation 
         Caption         =   "Run simulation"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
      Begin VB.Menu mnuShowHelp 
         Caption         =   "Help"
         Shortcut        =   {F1}
      End
      Begin VB.Menu mnuSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu mnuAbout 
         Caption         =   "About"
      End
   End
   Begin VB.Menu mnuView 
      Caption         =   "View"
      Visible         =   0   'False
      Begin VB.Menu MnuReportFile 
         Caption         =   "Report file"
      End
      Begin VB.Menu mnuModelParameters 
         Caption         =   "Select new model"
      End
      Begin VB.Menu mnuChangeParameters 
         Caption         =   "Change model parameters"
      End
      Begin VB.Menu MnuGraphs 
         Caption         =   "Graphs"
         Index           =   0
         Visible         =   0   'False
      End
      Begin VB.Menu MnuGraphs 
         Caption         =   "Graphs"
         Index           =   1
         Visible         =   0   'False
      End
      Begin VB.Menu MnuGraphs 
         Caption         =   "Graphs"
         Index           =   2
         Visible         =   0   'False
      End
      Begin VB.Menu MnuCancelPopUp 
         Caption         =   "Cancel"
      End
   End
   Begin VB.Menu mnuStreamProps 
      Caption         =   "Stream properties"
      Visible         =   0   'False
      Begin VB.Menu mnuStreamName 
         Caption         =   "Name this stream"
      End
      Begin VB.Menu mnuEditFeedStreamData 
         Caption         =   "Edit feed stream data"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuEditWaterFeedData 
         Caption         =   "Edit water feed data"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuDisplayStreamProps 
         Caption         =   "Display stream properties"
      End
      Begin VB.Menu mnuStreamSizeDistribution 
         Caption         =   "Plot size distribution"
      End
      Begin VB.Menu MnuStreamPropertiesCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "FLSheet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public MouseLocationX As Single
Public MouseLocationY As Single
Public CursorLocationX As Single
Public CursorLocationY As Single
Public NewFlowsheet As Boolean
Public Iconsize As Single
Public Reflectflag As Boolean
Public StepLength As Single
Public LocCursorOn As Boolean

Dim DragLocCursorOn As Boolean
Dim RBCursorOn As Boolean
Dim RBCursorOrigin As Boolean
Dim RectCursorOn As Boolean
Dim RectCursorOrigin As Boolean
Dim LastCursorUsed As Integer   '= 0 for rectangular  = 1 for rubberband
Dim OriginX As Single
Dim OriginY As Single
Dim OldMouseLocationX As Single
Dim OldMouseLocationY As Single
Dim CornerX As Single
Dim CornerY As Single
Dim OldCornerX As Single
Dim OldCornerY As Single

Public MoveFlag As Boolean
Public DeleteFlag As Boolean
Public PaintFlag As Boolean
Public MovingFlag1 As Boolean
Public MovingFlag2 As Boolean
Public MoveBoxFrame As Boolean
Public PolygonType As Integer
Public ReplaceFlag As Boolean
Public IcSzOn As Boolean
Private FlotationFlagOn As Boolean
Public ConveyorFlag As Boolean
Public FirstDrumFlag As Boolean
Public SecondDrumFlag As Boolean
Public MixerFlag As Boolean
Public WaterStreamFlagOn As Boolean
Public AnnotateFlag As Boolean
Public EditAnnotationFlag As Boolean
Public FlyoutFlag As Boolean
Private NoteLength As Integer

Public Nocells As String

Private Selection As Integer
Private SelectionJ As Integer
Private SelectionYesNo As Boolean
Private SelectedNote As Integer
Public SelectedFlyout As Integer
Private UnitNumber As Integer
Public StreamNumber As Integer
Private MODEL As String * 4

Private HndMFFont As Long

Private Sub DrawLine(X1 As Single, Y1 As Single, X2 As Single, Y2 As Single)
'***************************************************************************
  'Implements the line method in sheet coordinates
  Dim ScX1 As Single, ScY1 As Single, ScX2 As Single, ScY2 As Single
  Call CoordConv(X1, Y1, ScX1, ScY1)
  Call CoordConv(X2, Y2, ScX2, ScY2)
  Line (ScX1, ScY1)-(ScX2, ScY2)
End Sub

Sub DrawCursor(X As Single, Y As Single)
'***************************************
  DrawMode = 7
  'ForeColor = BackColor
  ForeColor = vbCyan
  DrawLine X - 20, Y, X + 20, Y
  DrawLine X, Y - 20, X, Y + 20
  CursorLocationX = X
  CursorLocationY = Y
  ForeColor = BackColor
End Sub

Public Sub ResetFlags()
'**********************
  StatusBar1.Panels(1).text = ""
  If Not MdlFlwsheet.COMFLG Then
    Line (OriginX, OriginY)-(MouseLocationX, MouseLocationY)
    Call EndStream(OriginX, OriginY, MouseLocationX, MouseLocationY)
    DeleteStream (MdlFlwsheet.Stream)
    MdlFlwsheet.COMFLG = True
  End If
  RBCursorOn = False
  RBCursorOrigin = False
  RectCursorOn = False
  RectCursorOrigin = False
  LocCursorOn = False
  DragLocCursorOn = False
  MoveFlag = False
  DeleteFlag = False
  'Reflectflag = False Must stay true until the next unit icon has been drawn.
  PaintFlag = False
  ReplaceFlag = False
  AnnotateFlag = False
  EditAnnotationFlag = False
  FlyoutFlag = False
  MovingFlag1 = False
  MovingFlag2 = False
  MoveBoxFrame = False
  PolygonType = 5
  IcSzOn = False
  FlotationFlagOn = False
  ConveyorFlag = False
  FirstDrumFlag = False
  SecondDrumFlag = False
  SelectionYesNo = False
  MixerFlag = False
  End Sub

Private Sub MoveCursor(X As Single, Y As Single)
'  Move the location cursor a distance X in X direction and Y in the Y direction
   Call DrawCursor(CursorLocationX, CursorLocationY)
   CursorLocationX = CursorLocationX + X
   CursorLocationY = CursorLocationY + Y
   Call DrawCursor(CursorLocationX, CursorLocationY)
End Sub

Private Sub Accept_Click()
'*************************
  Call RefreshFlowsheet
  Call AcceptFlowsheet
End Sub

Private Sub Annotate_Click()
'***************************
  AnnotateFlag = True
  NOTE(0) = ""
  NoteLength = 0
  LocCursor_Click
  StatusBar1.Panels(1).text = "Set the cursor and type the annotation."
End Sub

Private Sub AutMill_Click()
'**************************
 SelectUnit 52, Iconsize, Reflectflag
End Sub

Private Sub AutMillTrom_Click()
'******************************
 SelectUnit 65, Iconsize, Reflectflag
End Sub

Private Sub BallMill_Click()
'***************************
 SelectUnit 13, Iconsize, Reflectflag
End Sub

Private Sub BatacJig_Click()
'*************************
  SelectUnit 43, Iconsize, Reflectflag

End Sub

Private Sub BaumJig_Click()
'*************************
  SelectUnit 42, Iconsize, Reflectflag
End Sub

Private Sub BlackBox_Click()
'*************************
  SelectUnit 49, Iconsize, Reflectflag
End Sub

Private Sub Cancel_Click()
  StatusBar1.Panels(1).text = ""
  MGPage(0).Cls
  Unload MGPage(0)
  MODSIM.Show
End Sub

Private Sub CentrifugalPump_Click()
'**********************************
  SelectUnit 17, Iconsize, Reflectflag
End Sub

Private Sub Chance_Click()
'*************************
  SelectUnit 47, Iconsize, Reflectflag
End Sub

Private Sub ChkShowFlyouts_Click()
'*********************************
  Call RefreshFlowsheet
  If LocCursorOn Then
     Call DrawCursor(CursorLocationX, CursorLocationY)
  End If
End Sub

Private Sub ChkStreamNumbers_Click()
'***********************************
  Call RefreshFlowsheet
End Sub

Private Sub ChkShowFlyouts_KeyDown(KeyCode As Integer, Shift As Integer)
'*************************************************************************
Call Form_KeyDown(KeyCode, Shift)
End Sub

Private Sub ChkStreamNumbers_KeyDown(KeyCode As Integer, Shift As Integer)
'*************************************************************************
Call Form_KeyDown(KeyCode, Shift)
End Sub

Private Sub ChkUnitNumbers_Click()
'*********************************
  Call RefreshFlowsheet
End Sub

Private Sub ChkUnitNumbers_KeyDown(KeyCode As Integer, Shift As Integer)
'***********************************************************************
Call Form_KeyDown(KeyCode, Shift)
End Sub


Private Sub ConeCrusher_Click()
  SelectUnit 21, Iconsize, Reflectflag

End Sub

Private Sub Conveyor_Click()
'*************************
  UnitNumberFlagOn = False
  SelectUnit 62, Iconsize, Reflectflag

End Sub


Private Sub Delete_Click()
  DeleteFlag = True
  If MODSIM.EditFlowsheetFlag Then
    StatusBar1.Panels(1).text = "Select unit, stream, annotation or flyout to delete"
  ElseIf MODSIM.ViewFlowsheetFlag Then
    StatusBar1.Panels(1).text = "Select a flyout to delete"
  End If
End Sub

Private Sub DenseMediumCyclone_Click()
'*************************
  SelectUnit 38, Iconsize, Reflectflag

End Sub

Private Sub DenseMediumVessel_Click()
'*************************
  SelectUnit 51, Iconsize, Reflectflag

End Sub


Private Sub DewateringScreen_Click()
'*************************
  SelectUnit 57, Iconsize, Reflectflag

End Sub


Private Sub DoubleScreen_Click()
'*******************************
  SelectUnit 40, Iconsize, Reflectflag
End Sub

Private Sub Drewboy_Click()
'**************************
  SelectUnit 2, Iconsize, Reflectflag
End Sub

Private Sub Dynawhirlpool_Click()
'*************************
  SelectUnit 41, Iconsize, Reflectflag

End Sub

Private Sub Edit_Click()
  StatusBar1.Panels(1).text = ""
  ChkUnitNumbers.Enabled = True
  ChkStreamNumbers.Enabled = True
  ChkShowFlyouts.Enabled = True
  If LocCursorOn Then
    DrawCursor CursorLocationX, CursorLocationY
    LocCursorOn = False
  End If
  ResetFlags
End Sub

Private Sub Elutriator_Click()
'*************************
  SelectUnit 61, Iconsize, Reflectflag

End Sub

Private Sub Filter_Click()
'*************************
  SelectUnit 12, Iconsize, Reflectflag

End Sub


Private Sub FixedRollMill_Click()
'*************************
  SelectUnit 59, Iconsize, Reflectflag

End Sub


Private Sub FlotationCells_Click()
'*********************************
  WSTRING = "How many cells in this floation bank? "
  Nocells = InputBox(WSTRING, "DATA REQUIRED")
  If Val(Nocells) < 1 Then
    Call MsgBox("Invalid value for number of cells", 16, "ERROR")
    Exit Sub
  End If
  SelectUnit 6, Iconsize, Reflectflag
  'LblData.Visible = True
  'TxtData.Visible = True
 ' LblData.Top = CursorLocationY
 ' LblData.Left = CursorLocationY
 ' TxtData.SetFocus
  FlotationFlagOn = True
End Sub

Private Sub Form_Activate()
'*************************
  PageNumber = 0
  'Diagnostic.lstDiagnostic.AddItem "Activating FLSheet as page " & CStr(PageNumber)
  PSFlagOn = False
End Sub

Private Sub Form_Deactivate()
  'Diagnostic.lstDiagnostic.AddItem "Deactivating FLSheet"
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
'*********************************************
Dim LABNO1 As Integer
Dim XT As Single, YT As Single

If AnnotateFlag Then
  If KeyAscii = 8 Then
    Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
    CurrentX = XT
    CurrentY = YT - TextHeight("M")
    'Backspace
    ForeColor = BackColor
    Print NOTE(0)
    NoteLength = NoteLength - 1
    If NoteLength > 0 Then
      NOTE(0) = Mid(NOTE(0), 1, NoteLength)
      ForeColor = vbRed
      Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
      CurrentX = XT
      CurrentY = YT - TextHeight("M")
      Print NOTE(0);
    Else
      Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
      CurrentX = XT
    End If
  ElseIf KeyAscii = 13 Then
    'Enter ends the annotation.
    ForeColor = vbBlack
    StatusBar1.Panels(1).text = "Position the location cursor and type the next annotation"
    Call ANNOTE(NOTE(0), CursorLocationX, CursorLocationY)
    NOTE(0) = ""
    NoteLength = 0
    'AnnotateFlag = False
  Else
    Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
    CurrentY = YT - TextHeight("M")
    ForeColor = vbRed
    Print Chr(KeyAscii);
    NOTE(0) = NOTE(0) & Chr(KeyAscii)
    NoteLength = NoteLength + 1
  End If
  KeyAscii = 0
End If
  
If EditAnnotationFlag Then
  If KeyAscii = 13 Then
    TxtData.Visible = False
    NOTE(SelectedNote) = TxtData.text
    NOTEFL(SelectedNote) = True
    TxtData.text = ""
    KeyAscii = 0
    Call MGFONT(LabelFont)
    ForeColor = vbBlack
    LABNO1 = LABNO
    LABNO = 0
    RedrawAnnotations (LABNO1)
  End If
End If

End Sub

Private Sub Form_Resize()
'***********************
' Refreshes the flowsheet
'  Call RefreshFlowsheet
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Dim di As Long
  If hndMetafile <> 0 Then
    di = DeleteEnhMetaFile(hndMetafile)
    emfsdeleted = emfsdeleted + 1
  End If
End Sub

Private Sub frameBoxes_Click()
  If MoveFlag Then
    MoveBoxFrame = True
  End If
End Sub

Private Sub frameBoxes_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If MoveFlag Then
    MoveBoxFrame = True
  End If
End Sub

Private Sub GetFlowsheet_Click()
'*******************************
'Get a previously drawn flowsheet from file
  Dim FileType As String
  Dim I As Integer, J As Integer
  On Error GoTo ErrHandler
  
  'Set filters
  CommonDialog1.Filter = "Flowsheet files (*.TR)|*.TR|Flowsheet files (*.TRN)|*.TRN"
  'Default filter
  CommonDialog1.FilterIndex = 2
  'Set the cancel error flag
  CommonDialog1.CancelError = True
  CommonDialog1.Flags = cdlOFNHideReadOnly
  'Display OPEN dialog box
  CommonDialog1.ShowOpen
  WSTRING = Trim(CommonDialog1.FileName)
  J = Len(WSTRING)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    FileType = Mid(WSTRING, I + 1, J)
    If FileType = "TR" Or FileType = "tr" Or FileType = "Tr" Then
      Call MdlFlwsheet.GetFlowsheet(WSTRING)
    ElseIf FileType = "TRN" Or FileType = "trn" Then
      FileCopy WSTRING, RTrim(JobPath) & "PLNTR.OUT"
      Call ReadFlowsheet
    Else
      MsgBox "Invalid file type", 16, "ERROR"
      Exit Sub
    End If
  Else
    MsgBox "Invalid file type", 16, "ERROR"
    Exit Sub
  End If

  Exit Sub
ErrHandler:
'User pressed CANCEL button
  Exit Sub
End Sub

Private Sub Gyratory_Click()
'*************************
  SelectUnit 54, Iconsize, Reflectflag
End Sub

Private Sub HPRollMill_Click()
'*************************
  SelectUnit 60, Iconsize, Reflectflag

End Sub

Private Sub Hydrocyclone_Click()
'*************************
  SelectUnit 8, Iconsize, Reflectflag
End Sub

Private Sub mnuAbout_Click()
'***************************
  'About.Caption = "About MODSIM"
  'About.lblTitle = "MODSIM"
  'About.lblVersion = "Version " & App.Major & "." & App.Minor & "." & App.Revision
  If ProfessionalVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Professional version"
  ElseIf DemoVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Demonstration version"
  ElseIf StudentVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Student version"
  ElseIf AcademicVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Academic version"
    
  Else
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants."
  End If
  About.lblDisclaimer = "Copyright 1978 - 2004 R. P. King and M. A. Ford"
  About.Show

End Sub

Private Sub mnuAddFlyout_Click()
'*******************************
'Add a stream flyout to the flowsheet
  StatusBar1.Panels(1).text = "Click the stream to tag with a flyout"
  FlyoutFlag = True
End Sub

Private Sub mnuChangeParameters_Click()
'**************************************
  If ChangedFlowsheet Then
    Call ResetLevels
    MsgBox "The flowsheet has changed" & vbCrLf & "Edit the system data before proceeding", vbExclamation, "WARNING"
    Exit Sub
  End If
  If UnitMods.FindIndexNumber(UnitNumber) Then
    FastParameterChange = True
    UnitMods.ShowModelParameter
  Else
    MsgBox "Could not load parameter form for this unit", vbInformation, "WARNING"
    Exit Sub
  End If
End Sub

Private Sub mnuCompoundScreen_Click()
'***********************************
  SelectUnit 67, Iconsize, Reflectflag
End Sub

Private Sub mnuDisplayStreamProps_Click()
'****************************************
  EchoFileName = "STREAMPROPS.TXT"
  Call ExtractStreamViewFile(StreamNumber)
End Sub

Private Sub MnuEditAnnotation_Click()
'************************************
  EditAnnotationFlag = True
  StatusBar1.Panels(1).text = "Click the annotation that you wish to edit"
End Sub

Private Sub mnuEditFeedStreamData_Click()
'***************************************
  Load Sysdata
  If Sysdata.FindFeedStreamIndexNumber(StreamNumber) Then
    FastFeedStreamChange = True
    Sysdata.StreamList_DblClick
  Else
    MsgBox "Could not load stream data for this stream", vbInformation, "WARNING"
    Exit Sub
  End If
End Sub

Private Sub mnuEditOutputFormat_Click()
  OutFormat.Top = DisplayOffsetY
  OutFormat.Left = DisplayOffsetX
  OutFormat.Show 1
  Call Refresh_Click
End Sub

Private Sub mnuFlotColumn_Click()
'********************************
  SelectUnit 68, Iconsize, Reflectflag
End Sub

Private Sub mnuJones_Click()
'***************************
  SelectUnit 69, Iconsize, Reflectflag
End Sub

Private Sub mnuShowHelp_Click()
'******************************
  Dim RetVal As Long
  RetVal = Shell("HH " & RTrim(FileReadPath) & "MODSIM.chm", vbNormalFocus)
End Sub

Private Sub mnuStreamName_Click()
'********************************
  Dim N As Integer, I As Integer
  Dim IND As Integer, NSTR As Integer
  Dim OldName As String, NewName As String
  Load Sysdata
  If Sysdata.FindFeedStreamIndexNumber(StreamNumber) Then
    FastFeedStreamChange = True
    Sysdata.StreamList_DblClick
  ElseIf Sysdata.FindOtherStreamIndexNumber(StreamNumber) Then
    FastOtherStreamChange = True
    IND = Sysdata.OStreamList.ListIndex
    If IND >= 0 Then
      NSTR = Val(Sysdata.OStreamList.text)
      For N = 1 To 100
        If CURRODATA(N).NSTR = NSTR Then
          OldName = CURRODATA(N).STREAM_NAME
          NewName = InputBox("Specify the new stream name", "MODSIM", OldName)
          If NewName = "" Then
            CURRODATA(N).STREAM_NAME = OldName
          Else
            CURRODATA(N).STREAM_NAME = NewName
          End If
          Exit For
        ElseIf CURRODATA(N).NSTR = 0 Then
          I = Sysdata.FindSequentialStreamNumber(StreamNumber)
          CURRODATA(N).ID = STREAM_ID(I)
          CURRODATA(N).NSTR = NSTR
          CURRODATA(N).NDC = 1
          OldName = "No name"
          NewName = InputBox("Specify the new stream name", "MODSIM", OldName)
          If NewName = "" Then
            CURRODATA(N).STREAM_NAME = OldName
          Else
            CURRODATA(N).STREAM_NAME = NewName
          End If
          Sysdata.N_O_DATA_STREAMS = Sysdata.N_O_DATA_STREAMS + 1
          Exit For
        End If
      Next N
    End If
    Call Sysdata.CmdReturn_Click
  ElseIf Sysdata.FindWaterStreamIndexNumber(StreamNumber) Then
    FastWaterStreamChange = True
    Sysdata.WStreamList_DblClick
  Else
    MsgBox "Could not find stream data for this stream", vbInformation, "WARNING"
    Exit Sub
  End If
End Sub

Private Sub mnuEditModelParameterData_Click()
'********************************************
  If ChangedFlowsheet Then
    Call ResetLevels
    MsgBox "The flowsheet has changed" & vbCrLf & "Edit the system data before proceeding", vbExclamation, "WARNING"
    Exit Sub
  End If
  Call StartUnitMods
  FastParameterChange = False
  UnitMods.Top = DisplayOffsetY
  UnitMods.Left = DisplayOffsetX
  UnitMods.Show 1
End Sub

Private Sub mnuEditSystemData_Click()
'************************************
  FastFeedStreamChange = False
  FastWaterStreamChange = False
  FastOtherStreamChange = False
  Sysdata.Top = DisplayOffsetY
  Sysdata.Left = DisplayOffsetX
  Sysdata.Show 1
End Sub

Private Sub mnuEditWaterFeedData_Click()
'***************************************
  Load Sysdata
  If Sysdata.FindWaterStreamIndexNumber(StreamNumber) Then
    FastWaterStreamChange = True
    Sysdata.WStreamList_DblClick
  Else
    MsgBox "Could not load stream data for this stream", vbInformation, "WARNING"
    Exit Sub
  End If
End Sub

Private Sub MnuExportMetaFile_Click()
'************************************
  Dim FileName As String, ReturnFile As String
  Dim resp As Integer, I As Integer
  Dim response As Long
  Dim Left As Long, Top As Long, Right As Long, Bottom As Long
  Dim di As Long, OldPen As Long, OldFont As Long
  Dim lf As LOGFONT
  Dim TempByteArray() As Byte
  Dim ByteArrayLimit As Long
  
  Dim xExt As Long, yExt As Long
  Dim Description As String, lpRect As RECT, refDC As Long
  Dim dl As Long, lx As Long, ly As Long, lppoint As POINTAPI, lpsize As size

  On Error GoTo ErrHandler
  
    'Open an enhanced metafile device context for this page
    MetaFileFlagOn = True
    refDC = GetDC(0)  'Use screen as the reference DC
    FileName = RTrim(JobPath) & "metafile00.emf"
    Description = "METGRAPH" & Chr(0) & "Page00" & Chr(0) & Chr(0)
    'Set the metafile playback size to 50% of the picture box size.
    lpRect.Top = CLng(0)
    lpRect.Left = CLng(0)
    'lprect.Bottom = CLng(12500)
    lpRect.Bottom = CLng(100 * GetDeviceCaps(MGPage(0).hdc, VERTSIZE))
    'lprect.Right = CLng(12500)
    lpRect.Right = CLng(100 * GetDeviceCaps(MGPage(0).hdc, HORZSIZE))
    'Use 1:1 aspect ratio
    'If lpRect.Right < lpRect.Bottom Then
      'lpRect.Right = lpRect.Bottom
    'Else
      'lpRect.Bottom = lpRect.Right
    'End If
    
    'Create a new metafile device context
    EnhMetafileDC(0) = CreateEnhMetaFile(refDC, FileName, lpRect, Description)
    response = ReleaseDC(0, refDC)
    If EnhMetafileDC(0) <> 0 Then
      emfsCreated = emfsCreated + 1
    Else
      MsgBox "Could not create a metafile", vbExclamation, "WARNING"
      Exit Sub
    End If
    dl = SetMapMode(EnhMetafileDC(0), MM_TEXT)
    lx = 0
    ly = 0
    dl = SetWindowOrgEx(EnhMetafileDC(0), lx, ly, lppoint)
    lx = CLng(MGPage(0).width)
    ly = CLng(MGPage(0).height)
    dl = SetWindowExtEx(EnhMetafileDC(0), lx, ly, lpsize)
    lx = 0
    ly = 0
    dl = SetViewportOrgEx(EnhMetafileDC(0), lx, ly, lppoint)
    lx = CLng(MGPage(0).ScaleWidth)
    ly = CLng(MGPage(0).ScaleHeight)
    dl = SetViewportExtEx(EnhMetafileDC(0), lx, ly, lpsize)
    'MetaFileInch = ScaleWidth / 7.5
  
    'Select a pen into the VBMetgraph metafile DC
    If hndPen Then
      di = DeleteObject(hndPen)
      pensdeleted = pensdeleted + 1
    End If
    hndPen = CreatePen(PS_SOLID, 1, BLACK_PEN)
    pensCreated = pensCreated + 1
    OldPen = SelectObject(EnhMetafileDC(0), hndPen)
  
    'Select a font into the VBMetgraph metafile
    If HndMFFont <> 0 Then
      di = DeleteObject(HndMFFont)
      fontsDeleted = fontsDeleted + 1
    End If
   TempByteArray = StrConv("Arial" & Chr(0), vbFromUnicode)
   ByteArrayLimit = UBound(TempByteArray)
   For I = 1 To ByteArrayLimit
     lf.lfFaceName(I) = TempByteArray(I)
   Next I
   lf.lfHeight = 1.8 * PSFONTSIZE
   lf.lfWidth = 0.7 * PSFONTSIZE
   HndMFFont = CreateFontIndirect(lf)
   fontsCreated = fontsCreated + 1
   OldFont = SelectObject(EnhMetafileDC(0), HndMFFont)
  
  'Draw into the metafile
   PageNumber = 0
   Call RefreshFlowsheet
   EnhMetafileHnd(PageNumber) = CloseEnhMetaFile(EnhMetafileDC(PageNumber))
   dl = DeleteEnhMetaFile(EnhMetafileHnd(PageNumber))
   emfsdeleted = emfsdeleted + 1
   MetaFileFlagOn = False
   
   On Error GoTo ErrHandler
   CommonDialog1.Filter = "Enhanced metafiles (.emf)|*.emf"
   CommonDialog1.FilterIndex = 1
   CommonDialog1.Flags = cdlOFNHideReadOnly
   CommonDialog1.ShowSave
   FileName = CommonDialog1.FileName
   'Strip any file extensions and add the .emf
   I = InStr(FileName, ".")
   If I > 0 Then
      FileName = Mid(FileName, 1, I - 1) & ".emf"
   Else
      FileName = FileName & ".emf"
   End If
   'Check for existing file
   ReturnFile = Dir(FileName)
   If ReturnFile <> "" Then
     resp = MsgBox("A file with name " & FileName & " already exists.  Overwrite?", vbExclamation + vbOKCancel, "WARNING")
     If resp = vbCancel Then Exit Sub
   End If
   
   FileCopy RTrim(JobPath) & "metafile00.emf", FileName
 
  Exit Sub
ErrHandler:
  Exit Sub
ErrHandlerOpen:
  MsgBox "Metafile could not be created", vbCritical, "ERROR"
  Exit Sub
  
End Sub

Private Sub MnuExportPS_Click()
'******************************
  Dim FileName As String, ReturnFile As String
  Dim resp As Integer, I As Integer
  Dim sLLX As Single, sLLY As Single, sURX As Single, sURY As Single
  Dim IMAGE_SIZE As Long
  On Error GoTo ErrHandler
  
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "PostScript files (*.ps)|*.ps"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  'Strip off any file type extension
  WSTRING = Trim(CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    FileName = Mid(WSTRING, 1, I - 1) & ".ps"
  Else
    FileName = WSTRING & ".ps"
  End If
  'Check for existing file
  ReturnFile = Dir(FileName)
  If ReturnFile <> "" Then
    resp = MsgBox("A file with name " & FileName & " already exists.  Overwrite?", vbExclamation + vbOKCancel, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  
  'Open the file and write the PostScript code
  On Error GoTo OpenErrHandler
  'Open FileName For Output As #11
  MousePointer = 11
  PSFlagOn = True
  Call PSINIT
  Call RefreshFlowsheet
  Call LBSIZE(8, 16)  'This is a good size for the flyouts and labels
  Call WritePSFile(FileName)
  PSFlagOn = False
  Call RefreshFlowsheet
  MousePointer = 0
  Exit Sub
  
ErrHandler:
  Exit Sub
  
OpenErrHandler:
  MsgBox "Could not open the file that you requested", vbCritical, "ERROR"
  MousePointer = 0
  Exit Sub

End Sub

Private Sub MnuFont_Click()
'**************************
'Get a new font using the font dialog
  Call MGNEWFONT(LabelFont)
  Flyoutfont.Name = LabelFont.Name
  Flyoutfont.size = LabelFont.size
  Flyoutfont.Bold = LabelFont.Bold
  Flyoutfont.italic = LabelFont.italic
  FlyoutSizeFactor = Flyoutfont.size / 8
  Call RefreshFlowsheet
End Sub

Private Sub MnuGraphs_Click(Index As Integer)
'********************************************
  Dim RetVal As Double
  Call ModelGraphs(UnitNumber, MODEL, Index)
End Sub

Private Sub MnuKnelson_Click()
'*****************************
  SelectUnit 64, Iconsize, Reflectflag
End Sub

Private Sub mnuLibGraphs_Click()
'*******************************
  libgraph.Top = 0
  libgraph.Left = 0
  libgraph.Show
End Sub

Private Sub mnuModelParameters_Click()
'*************************************
If UnitMods.FindIndexNumber(UnitNumber) Then
  FastParameterChange = False
  UnitMods.Top = DisplayOffsetY
  UnitMods.Left = DisplayOffsetX
  UnitMods.Show 1
End If
End Sub

Private Sub MnuReportFile_Click()
'********************************
  EchoFileName = "REPORT.OUT"
  Call ExtractViewFile(UnitNumber)
End Sub

Private Sub mnuRunSimulation_Click()
'***********************************
  'Run a single simulation
  Call MODSIM.RunSimulation(0)
  Call RefreshFlowsheet
End Sub

Private Sub mnuSizeDistributionGraphs_Click()
'********************************************
  Sizgraph.Top = 0
  Sizgraph.Left = 0
  Sizgraph.Show
End Sub

Private Sub mnuStreamSizeDistribution_Click()
'********************************************
  Call PlotSingleStreamSizeDistribution(StreamNumber)
End Sub

Private Sub Norwalt_Click()
'*************************
  SelectUnit 44, Iconsize, Reflectflag
End Sub

Private Sub PuddlePan_Click()
'*************************
  SelectUnit 15, Iconsize, Reflectflag
End Sub

Private Sub Refresh_Click()
'**************************
' Refreshes the flowsheet
  Call RefreshFlowsheet
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
'*************************************************************
   If LocCursorOn Then
     Select Case KeyCode
       Case vbKeyLeft
         MoveCursor -StepLength, 0
       Case vbKeyRight
         MoveCursor StepLength, 0
       Case vbKeyUp
         MoveCursor 0, StepLength
       Case vbKeyDown
         MoveCursor 0, -StepLength
       Case vbKeyInsert
         If StepLength < 10 Then
           StepLength = 100
         Else
           StepLength = 1
         End If
     End Select
     KeyCode = 0
     LastCursorUsed = 0  'Start with rectangular cursor
   End If
End Sub

Private Sub Form_Load()
'**********************
  Dim High As Single
  Dim N As Integer, I As Integer, ID As Integer
  Dim DummyString As String, KEYS As String * 4, MODEL As String * 4
  Dim Unit As Integer
  Dim JOBNAMEVAR As String
  Dim NOPAR As Integer

  StatusBar1.Panels(1).text = ""
  StatusBar1.Panels(2).Visible = False
  CommonDialog1.CancelError = True
  MGPage(0).KeyPreview = True
  If MODSIM.ViewFlowsheetFlag Then
    GetFlowsheet.Visible = False
    MnuSelect.Visible = False
    'LocCursor.Visible = False
    RBCursor.Visible = False
    RectCursor.Visible = False
    WaterStream.Visible = False
    MnuFont.Visible = True
    MnuIconSize.Visible = False
    Annotate.Visible = False
    MnuEditAnnotation.Visible = False
    ReflectIcon.Visible = False
    MnuMove.Caption = "Move flyout or box"
    MnuMove.Visible = True
    Delete.Caption = "Delete flyout"
    Delete.Visible = True
    mnuEditSystemData.Visible = True
    mnuEditModelParameterData.Visible = True
    mnuEditOutputFormat.Visible = True
    mnuView1.Visible = True

    If JobModelParameterData Then
    ' Get the current values of the parameters for the units including model names for special model graphs.
      Open RTrim(JobPath) + "CURRDATA.RUN" For Input As #33
      ID = 0
      Do While Not EOF(33)
        KEYS = Input$(4, #33)
        If (KEYS = "STOP") Then Exit Do
        If (KEYS = "TYPE") Then
          ID = ID + 1
          Input #33, Unit
          MODEL$ = Input$(4, #33)
          Input #33, NOPAR%
          Input #33, CURRMODELDATA(ID).ID
          CURRMODELDATA(ID).Unit = Unit
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
  End If
  If MODSIM.EditFlowsheetFlag Then
    GetFlowsheet.Visible = True
    MnuSelect.Visible = True
    LocCursor.Visible = True
    RBCursor.Visible = True
    RectCursor.Visible = True
    WaterStream.Visible = True
    MnuIconSize.Visible = True
    Annotate.Visible = True
    MnuEditAnnotation.Visible = True
    ReflectIcon.Visible = True
    MnuMove.Visible = True
    Delete.Visible = True
    mnuEditSystemData.Visible = False
    mnuEditModelParameterData.Visible = False
    mnuEditOutputFormat.Visible = False
    mnuView1.Visible = False
  End If
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, ScX As Single, ScY As Single)
'********************************************************************************************
Dim I As Integer, J As Integer, N As Integer
Dim X As Single, Y As Single, XT As Single, YT As Single
Dim YESNO As Boolean
Dim RetVal As Double

  'Define page number in case a Metgraph page is loaded
  PageNumber = 0
  Call ConvertCoordFromScreen(ScX, ScY, X, Y)
  MouseLocationX = ScX
  MouseLocationY = ScY
  
  If MODSIM.SizgraphIsActive Then
    Sizgraph.Hide
    MsgBox "Close the graph setup control before proceeding"
    Sizgraph.Show
    Exit Sub
  End If
  If MODSIM.LibGraphIsActive Then
    libgraph.Hide
    MsgBox "Close the graph setup control before proceeding"
    libgraph.Show
    Exit Sub
  End If
  If MODSIM.EditFlowsheetFlag Then GoTo EditFlowsheet
  If MODSIM.ViewFlowsheetFlag Then GoTo ViewFlowsheet
  
EditFlowsheet:
'Edit the flowsheet

  If MixerFlag Then
    DrawMixerStream CursorLocationX, CursorLocationY
    MixerFlag = False
    Exit Sub
  End If
  
  If LocCursorOn Then
    If (Abs(CursorLocationX - X) + Abs(CursorLocationY - Y)) < 25 Then
      MoveCursor X - CursorLocationX, Y - CursorLocationY
      DragLocCursorOn = True
    End If
    Exit Sub
  End If
  
  If RBCursorOn And (Button And 1) Then
    OriginX = ScX
    OriginY = ScY
    OldMouseLocationX = OriginX
    OldMouseLocationY = OriginY
    If Not RBCursorOrigin Then
      RBCursorOrigin = True
      ForeColor = vbBlack
      DrawMode = 13
      Call StartStream(X, Y, WaterStreamFlagOn)
      WaterStreamFlagOn = False
    Else
      Call Corner(X, Y)
    End If
    Exit Sub
  End If
  
  If RectCursorOn And (Button And 1) Then
    If Not RectCursorOrigin Then
      OriginX = ScX
      OriginY = ScY
      OldMouseLocationX = OriginX
      OldMouseLocationY = OriginY
      RectCursorOrigin = True
      ForeColor = vbBlack
      DrawMode = 13
      Call StartStream(X, Y, WaterStreamFlagOn)
      WaterStreamFlagOn = False
    Else
      Call ConvertCoordFromScreen(OldMouseLocationX, OldMouseLocationY, XT, YT)
      Call Corner(XT, YT)
      OriginX = OldMouseLocationX
      OriginY = OldMouseLocationY
    End If
    Exit Sub
  End If
  
  If FlyoutFlag Then
    Call Flyout(X + 10, Y + FlyoutSize * 5 + 25, X, Y)
    Exit Sub
  End If
  
  If DeleteFlag Then
    N = FLYOUTPK(X, Y)
    If N > 0 Then
      Call DeleteFlyout(N)
      Exit Sub
    End If
    N = NOTEPK(X, Y)
    If N > 0 Then
      Call NOTEDEL(N)
      Exit Sub
    End If
    Call StreamPick(I, YESNO)
    If YESNO Then
      DeleteStream I
      YESNO = False
      Exit Sub
    End If
    Call REGION(I, J, YESNO)
    If YESNO Then
      DeleteUnit I, J
      YESNO = False
    End If
    Exit Sub
  End If
  
  If MoveFlag Then
    N = FLYOUTPK(X, Y)
    If N >= 0 Then
      SelectedFlyout = N
      Call DeleteFlyout(N)
      Call PaintFlyout(N)
      Exit Sub
    End If
    N = NOTEPK(X, Y)
    If N > 0 Then
      Call NOTEDEL(N)
      SelectedNote = N
      Exit Sub
    End If
    'The sequence for moving a unit is:
    'Select the unit
    'Delete it
    'Paint it
    'Move it repeatedly
    'Paint it
    'Replace it
    Call REGION(Selection, SelectionJ, SelectionYesNo)
    If SelectionYesNo Then
      StatusBar1.Panels(1).text = "Drag this unit to a new location"
      DeleteFlag = True
      Call DeleteUnit(Selection, SelectionJ)
      DeleteFlag = False
      Call PaintUnit(Selection, SelectionJ)
    End If
    Exit Sub
  End If
  
  If Reflectflag And Button = 1 Then
    Call REGION(Selection, SelectionJ, SelectionYesNo)
    If SelectionYesNo Then
      Call ReflectExistingUnit(Selection, SelectionJ)
      Reflectflag = False
    End If
    Exit Sub
  End If
 
  If AnnotateFlag Then
    'End the annotation....
    ForeColor = vbBlack
    StatusBar1.Panels(1).text = "Position the location cursor and type the next annotation"
    Call ANNOTE(NOTE(0), CursorLocationX, CursorLocationY)
    NOTE(0) = ""
    '...then move the cursor to the next location
    Call MoveCursor(X - CursorLocationX, Y - CursorLocationY)
    DragLocCursorOn = True
    Exit Sub
  End If

  If EditAnnotationFlag Then
    N = NOTEPK(X, Y)
    If N > 0 Then
      Call CoordConv(X, Y, XT, YT)
      TxtData.Left = XT
      TxtData.Top = YT - TextHeight("M")
      TxtData.Visible = True
      TxtData.text = NOTE(N)
      SelectedNote = N
      Call NOTEDEL(N)
      Exit Sub
    End If
  End If
  Exit Sub
  
  
  
  
ViewFlowsheet:
  'View the flowsheet
  
  If Button = vbRightButton Then
    ResetFlags
    'Has stream been picked?
    Call StreamPick(StreamNumber, YESNO)
    If YESNO Then
      If StreamType(StreamNumber) = 0 Then
        If WFlag(StreamNumber) Then
          mnuEditWaterFeedData.Visible = True   'At least one menu item must be visible
          mnuStreamSizeDistribution.Visible = False
          mnuEditFeedStreamData.Visible = False
        Else
          mnuEditFeedStreamData.Visible = True
          mnuEditWaterFeedData.Visible = False
        End If
        If SimulationHasBeenRun Then
          mnuDisplayStreamProps.Visible = True
          If Not WFlag(StreamNumber) Then
            mnuStreamSizeDistribution.Visible = True
          End If
        Else
          mnuDisplayStreamProps.Visible = False
        End If
        PopupMenu MnuStreamProps
        Exit Sub
      End If
      If SimulationHasBeenRun Then
        mnuDisplayStreamProps.Visible = True   'At least one menu item must be visible
        mnuEditWaterFeedData.Visible = False
        mnuEditFeedStreamData.Visible = False
        mnuStreamSizeDistribution.Visible = True
        PopupMenu MnuStreamProps
      End If
      Exit Sub
    End If
      
    'Has a unit icon been picked?
    Call REGION(I, J, YESNO)
    If YESNO Then
      'Load UnitMods to fill CURRMODELIndex
      Call StartUnitMods
      Load UnitMods
      If SimulationHasBeenRun Then
        'Find the unit model attached to the selected unit.
        UnitNumber = MdlFlwsheet.Unit(I, J)
        MODEL = CURRMODELDATA(CURRMODELIndex(UnitNumber)).MODEL
        If MODEL = "KYNC" Then
          MnuGraphs(0).Caption = "Flux curve"
          MnuGraphs(0).Visible = True
          PopupMenu mnuView
        ElseIf MODEL = "HFML" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(9) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
        ElseIf MODEL = "HFSU" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(12) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
        ElseIf MODEL = "GMIL" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(6) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
        ElseIf MODEL = "GMSU" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(36) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
        ElseIf MODEL = "GMI1" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(6) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
        ElseIf MODEL = "RODL" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          If CURRMODELDATA(CURRMODELIndex(UnitNumber)).PARAM(11) > 0 Then
            MnuGraphs(2).Caption = "Andrews-Mika diagram"
            MnuGraphs(2).Visible = True
          End If
          PopupMenu mnuView
          
        ElseIf MODEL = "SAGM" Or MODEL = "FAGM" Or MODEL = "SAGT" Or MODEL = "FAGT" Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf I = 13 Or I = 52 Or I = 53 Then
          MnuGraphs(0).Caption = "Selection function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Caption = "Breakage function"
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "CYCL" Or MODEL = "CYCA" Or MODEL = "CYCB" Or MODEL = "ELUT" Or MODEL = "WICY" Then
          MnuGraphs(0).Caption = "Classification function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "SCR2" Or MODEL = "KSCN" Or MODEL = "PSCN" Or MODEL = "CSCN" Then
          MnuGraphs(0).Caption = "Classification function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "SPIR" Or MODEL = "KELL" Or MODEL = "LISP" Then
          MnuGraphs(0).Caption = "Partition function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "DYNA" Or MODEL = "DMCY" Then
          MnuGraphs(0).Caption = "Partition function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "BAUJ" Or MODEL = "BATJ" Then
          MnuGraphs(0).Caption = "Partition function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
          
        ElseIf MODEL = "NORW" Or MODEL = "WEMC" Or MODEL = "DREW" Or MODEL = "CHAN" Or MODEL = "SLIP" Or MODEL = "WASH" Or MODEL = "SHAK" Or MODEL = "DRUM" Or MODEL = "WOCY" Then
          MnuGraphs(0).Caption = "Partition function"
          MnuGraphs(0).Visible = True
          MnuGraphs(1).Visible = False
          PopupMenu mnuView
       
        ElseIf MODEL = "CONE" Then
          MnuGraphs(0).Caption = "Stratification profiles"
          MnuGraphs(0).Visible = True
          PopupMenu mnuView
        
        ElseIf MODEL = "SJIG" Or MODEL = "VJIG" Then
          MnuGraphs(0).Caption = "Stratification profiles"
          MnuGraphs(0).Visible = True
          PopupMenu mnuView
        
        Else
          MnuGraphs(0).Visible = False
          MnuGraphs(1).Visible = False
          MnuGraphs(2).Visible = False
          PopupMenu mnuView
        End If
      End If
      Unload UnitMods
    Else
      PopupMenu mnuRun
    End If
    Exit Sub
  End If
  
  'Now catch the left button clicks
  If FlyoutFlag Then
    Call Flyout(X + 10, Y + FlyoutSize * 5 + 25, X, Y)
    Exit Sub
  End If
  
  If LocCursorOn Then
    If (Abs(CursorLocationX - X) + Abs(CursorLocationY - Y)) < 25 Then
      MoveCursor X - CursorLocationX, Y - CursorLocationY
      DragLocCursorOn = True
    End If
    Exit Sub
  End If
  
  If DeleteFlag Then
    N = FLYOUTPK(X, Y)
    If N > 0 Then
      Call DeleteFlyout(N)
      Exit Sub
    End If
  End If
  
  If MoveFlag Then
    N = FLYOUTPK(X, Y)
    If N >= 0 Then
      SelectedFlyout = N
      Call DeleteFlyout(N)
      Call PaintFlyout(N)
      Exit Sub
    End If
    Exit Sub
  End If
  Exit Sub
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, ScX As Single, ScY As Single)
'********************************************************************************************
  Dim X As Single, Y As Single
  Dim XT As Single, YT As Single
  
  'Define page number in case a Metgraph page is loaded
  PageNumber = 0
  Call ConvertCoordFromScreen(ScX, ScY, X, Y)
  MouseLocationX = ScX
  MouseLocationY = ScY
  
  If MoveBoxFrame Then
    Me.frameBoxes.Move ScX + 5, ScY + 5  ' Add 5 to get the frame out from under the cursor
  End If
    
  If RBCursorOn Then
    If RBCursorOrigin Then
      DrawMode = 7
      ForeColor = BackColor
      Line (OriginX, OriginY)-(OldMouseLocationX, OldMouseLocationY)
      Line (OriginX, OriginY)-(MouseLocationX, MouseLocationY)
    End If
  End If
  
  If RectCursorOn Then
    If RectCursorOrigin Then
      DrawMode = 7
      ForeColor = BackColor
      Line (OriginX, OriginY)-(OldMouseLocationX, OldMouseLocationY)
      If Abs(ScX - OriginX) > Abs(ScY - OriginY) Then
        MouseLocationY = OriginY
      Else
        MouseLocationX = OriginX
      End If
      Line (OriginX, OriginY)-(MouseLocationX, MouseLocationY)
    End If
  End If
  
  If DragLocCursorOn Then
  ' Drag the location cursor
    Call ConvertCoordFromScreen(OldMouseLocationX, OldMouseLocationY, XT, YT)
    MoveCursor X - XT, Y - YT
  End If
  
  If MoveFlag And SelectionYesNo Then
  ' Move a unit if required
    Call MoveUnit(Selection, SelectionJ)
  End If
  
  If MoveFlag And SelectedNote > 0 Then
  'Move an annotation if required
  'The move remains invisible
  End If
  
  If MoveFlag And SelectedFlyout >= 0 Then
    'Move a flyout
    Call MoveFlyout(SelectedFlyout)
  End If
  
  If ConveyorFlag Then
    If SecondDrumFlag Then
      StatusBar1.Panels(1).text = "Drag the second conveyor drum to the desired position"
      ' Move the second drum
      MGPage(0).DrawMode = 7
      MGPage(0).ForeColor = MGPage(0).BackColor
      Call DrawConveyorBelt(UnitLocX(62, NSEL), UnitLocY(62, NSEL), CONVEY_X(NSEL), CONVEY_Y(NSEL))
      Call DrawConveyorBelt(UnitLocX(62, NSEL), UnitLocY(62, NSEL), X, Y)
      CONVEY_X(NSEL) = X
      CONVEY_Y(NSEL) = Y
    End If
  End If
  
  OldMouseLocationX = MouseLocationX
  OldMouseLocationY = MouseLocationY
 
End Sub

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, ScX As Single, ScY As Single)
'******************************************************************************************
  Dim X As Single, Y As Single, XT As Single, YT As Single, XTT As Single, YTT As Single
  
  'Define page number in case a Metgraph page is loaded
  PageNumber = 0
  Call ConvertCoordFromScreen(ScX, ScY, X, Y)
  MouseLocationX = ScX
  MouseLocationY = ScY
  
  If MoveBoxFrame Then
    MoveBoxFrame = False
  End If
  
  If RBCursorOn Then
    If (Button And 2) And Not COMFLG Then
      RBCursorOrigin = False
      Line (OriginX, OriginY)-(MouseLocationX, MouseLocationY)
      ForeColor = vbBlack
      DrawMode = 13
      Call ConvertCoordFromScreen(OriginX, OriginY, XT, YT)
      Call EndStream(XT, YT, X, Y)
    End If
  End If
  
  If RectCursorOn Then
    If (Button And 2) And Not COMFLG Then
      RectCursorOrigin = False
      Line (OriginX, OriginY)-(OldMouseLocationX, OldMouseLocationY)
      ForeColor = vbBlack
      DrawMode = 13
      Call ConvertCoordFromScreen(OriginX, OriginY, XT, YT)
      Call ConvertCoordFromScreen(OldMouseLocationX, OldMouseLocationY, XTT, YTT)
      Call EndStream(XT, YT, XTT, YTT)
    End If
  End If
  
  If DragLocCursorOn Then
  ' Drop the location cursor
    DragLocCursorOn = False
    Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
    CurrentX = XT
    CurrentY = YT
  End If
  
  If MoveFlag Then
  ' Drop the moving unit
    If SelectionYesNo Then
      Call PaintUnit(Selection, SelectionJ)
      Call ReplaceUnit(Selection, SelectionJ)
    End If
    If SelectedNote > 0 Then
      MGPage(0).ForeColor = vbBlack
      Call ReplaceNote(SelectedNote, X, Y)
      SelectedNote = 0
    End If
    If SelectedFlyout >= 0 Then
      Call ReplaceFlyout(SelectedFlyout, X, Y)
      SelectedFlyout = -1
      If MODSIM.ViewFlowsheetFlag Then
        Call RefreshFlowsheet
      End If
    End If
    'MoveFlag = False
    SelectionYesNo = False
    FirstDrumFlag = False
    SecondDrumFlag = False
    If MODSIM.EditFlowsheetFlag Then
      StatusBar1.Panels(1).text = "Select a unit, annotation, flyout or box to move."
    ElseIf MODSIM.ViewFlowsheetFlag Then
      StatusBar1.Panels(1).text = "Select a flyout or box to move."
    End If
  End If
   
  If ConveyorFlag Then
    'Drop the second drum at the end of the initial draw.
    Call PaintUnit(62, NSEL)
    ReplaceFlag = True
    Call ReplaceUnit(62, NSEL)
    ReplaceFlag = False
    FirstDrumFlag = False
    SecondDrumFlag = False
    StatusBar1.Panels(1).text = ""
    ConveyorFlag = False
  End If
End Sub

Private Sub JawCrusher_Click()
  SelectUnit 39, Iconsize, Reflectflag
End Sub

Public Sub LocCursor_Click()
'***************************
  Dim XT As Single, YT As Single

  RBCursorOn = False
  RectCursorOn = False
  Reflectflag = False
  Call DrawCursor(CursorLocationX, CursorLocationY)
  Call CoordConv(CursorLocationX, CursorLocationY, XT, YT)
  CurrentX = XT
  CurrentY = YT
  LocCursorOn = True
  MGPage(0).KeyPreview = True
  ChkUnitNumbers.Enabled = False
  ChkStreamNumbers.Enabled = False
  ChkShowFlyouts.Enabled = False
  StatusBar1.Panels(1).text = "Location cursor is active"
End Sub

Private Sub Mixer_Click()
  MixerFlag = True
  SelectUnit 24, Iconsize, Reflectflag
End Sub

Private Sub MnuIconSize_Click()
'******************************
  IcSzOn = True
  Iconsize = Val(InputBox("Specify icon size  1-20", "DATA", CStr(Iconsize), 100, 100))
  If Iconsize < 1 Then Iconsize = 1
  If Iconsize > 20 Then Iconsize = 20
  
End Sub

Private Sub MnuMove_Click()
  MoveFlag = True
  If MODSIM.EditFlowsheetFlag Then
    StatusBar1.Panels(1).text = "Select a unit, annotation, flyout or box to move."
  ElseIf MODSIM.ViewFlowsheetFlag Then
    StatusBar1.Panels(1).text = "Select a flyout or box to move."
  End If
End Sub

Private Sub PrintFlowsheet_Click()
'*********************************
  Dim X As Single
  On Error GoTo CancelPrint

  CommonDialog1.CancelError = True
  CommonDialog1.PrinterDefault = True
  CommonDialog1.ShowPrinter
  PrinterFlagOn = True
  X = Printer.height / Printer.width
  Printer.ScaleTop = X * Wide
  Printer.ScaleLeft = 0
  Printer.ScaleWidth = Wide
  Printer.ScaleHeight = -X * Wide
'  Printer.ScaleTop = X * Wide
'  Printer.ScaleLeft = 0
'  Printer.ScaleWidth =  Wide
'  Printer.ScaleHeight = -X * Wide
'  Printer.Orientation = vbPRORLandscape
  With Printer.FONT
    .Name = LabelFont.Name
    .size = LabelFont.size
    .Bold = LabelFont.Bold
    .italic = LabelFont.italic
  End With
  Printer.FontTransparent = False
  Call RefreshFlowsheet
  
  Printer.EndDoc
  PrinterFlagOn = False
  Call RefreshFlowsheet
  Exit Sub
  
CancelPrint:
  Printer.KillDoc
  Exit Sub
End Sub

Private Sub RBCursor_Click()
  RBCursorOn = True
  LastCursorUsed = 1
  StatusBar1.Panels(1).text = "Rubber-band cursor is active"
End Sub


Private Sub RectCursor_Click()
  RectCursorOn = True
  LastCursorUsed = 0
  StatusBar1.Panels(1).text = "Rectangular cursor is active"
End Sub

Private Sub ReflectIcon_Click()
  Reflectflag = True
  Me.StatusBar1.Panels(1).text = "Click on icon to reflect"
End Sub

Private Sub ReichertCone_Click()
'*******************************
  SelectUnit 18, Iconsize, Reflectflag

End Sub

Private Sub RodMill_Click()
'*************************
  SelectUnit 53, Iconsize, Reflectflag
End Sub

Private Sub Screen_Click()
'*************************
  SelectUnit 14, Iconsize, Reflectflag
End Sub

Private Sub SegregatingBin_Click()
'*************************
  SelectUnit 48, Iconsize, Reflectflag
End Sub

Private Sub MnuSelect_Click()
  StatusBar1.Panels(1).text = "Select the type of unit required"
  ChkUnitNumbers.Enabled = True
  ChkStreamNumbers.Enabled = True
  ChkShowFlyouts.Enabled = True
  If LocCursorOn Then
    DrawCursor CursorLocationX, CursorLocationY
    LocCursorOn = False
  End If
  ResetFlags
End Sub

Private Sub ShakingTable_Click()
'*************************
  SelectUnit 10, Iconsize, Reflectflag
End Sub

Private Sub ShallowBath_Click()
'*************************
  SelectUnit 36, Iconsize, Reflectflag
End Sub

Private Sub SieveBend_Click()
'*************************
  SelectUnit 58, Iconsize, Reflectflag
End Sub

Private Sub SpiralClassifier_Click()
'*************************
  SelectUnit 63, Iconsize, Reflectflag
End Sub


Private Sub SpiralConcentrator_Click()
  SelectUnit 1, Iconsize, Reflectflag
End Sub

Private Sub Stockpile_Click()
'*************************
  SelectUnit 55, Iconsize, Reflectflag
End Sub

Private Sub StreamSplitter_Click()
'*************************
  SelectUnit 16, Iconsize, Reflectflag
End Sub

Private Sub Sump_Click()
'*************************
  SelectUnit 22, Iconsize, Reflectflag
End Sub

Private Sub Teska_Click()
'*************************
  SelectUnit 46, Iconsize, Reflectflag
End Sub

Private Sub Thickener_Click()
'*************************
  SelectUnit 20, Iconsize, Reflectflag
End Sub

Private Sub TxtData_KeyPress(KeyAscii As Integer)
If KeyAscii = 13 Then
  If IcSzOn Then
    If Val(TxtData.text) < 1 Then
      Call MsgBox("Invalid icon size has been specified", 16, "ERROR")
      Exit Sub
    Else
      Iconsize = Val(TxtData.text)
      IcSzOn = False
    End If
  End If
  If FlotationFlagOn Then
    If Val(TxtData.text) < 1 Then
     Call MsgBox("Invalid value for number of cells", 16, "ERROR")
     Exit Sub
    Else
    SelectUnit 6, Iconsize, Reflectflag
    FlotationFlagOn = False
    End If
  End If
  KeyAscii = 0
  TxtData.text = ""
  LblData.Visible = False
  TxtData.Visible = False
End If
End Sub

Private Sub TxtData_LostFocus()
  If IcSzOn Then
    If Val(TxtData.text) < 1 Then
      Call MsgBox("Invalid icon size has been specified", 16, "ERROR")
      Exit Sub
    Else
      Iconsize = Val(TxtData.text)
      IcSzOn = False
    End If
  End If
  If FlotationFlagOn Then
    If Val(TxtData.text) < 1 Then
     Call MsgBox("Invalid value for number of cells", 16, "ERROR")
     Exit Sub
    Else
    SelectUnit 6, Iconsize, Reflectflag
    FlotationFlagOn = False
    End If
  End If
  LblData.Visible = False
  TxtData.Visible = False
End Sub

Public Sub ExtractViewFile(UnitNumber As Integer)
'************************************************
  Dim LinesFromFile As String
  Dim ReturnFile As String
  Dim Match As Boolean
  
  On Error GoTo ErrHandler
  
  ViewFile.Caption = EchoFileName
  
  ReturnFile = Dir(RTrim(JobPath) + EchoFileName)
  If ReturnFile = "" Then
    MsgBox "Could not find the requested file", 16, "ERROR"
    Exit Sub
  End If
 
  Open RTrim(JobPath) + EchoFileName For Input As #36
  ViewFile.Text1.text = ""
  LinesFromFile = ""
  
  Match = False
  Do While Not Match And Not EOF(36)
    Line Input #36, WSTRING
    If Mid(WSTRING, 6, 11) = "Unit number" And Val(Mid(WSTRING, 17, 4)) = UnitNumber Then Match = True
  Loop
  If Not Match Then GoTo ErrHandler2
  LinesFromFile = LinesFromFile & WSTRING & Chr(13) & Chr(10)
  
  Match = False
  Do While Not Match And Not EOF(36)
    Line Input #36, WSTRING
    If Mid(WSTRING, 2, 25) = "UNIT EQUIPMENT DATA SHEET" Then
      LinesFromFile = LinesFromFile & " "
      Exit Do
    Else
      LinesFromFile = LinesFromFile & WSTRING & Chr(13) & Chr(10)
    End If
  Loop
  
  Close #36
  ViewFile.Text1.text = LinesFromFile
  ViewFile.LblDate.Caption = FileDateTime(RTrim(JobPath) + EchoFileName)
  
  If EchoFileName = "TEARS.OUT" Then
    ViewFile.MnuClearTears.Visible = True
  End If
  ViewFile.Top = 0
  ViewFile.Left = 0
  ViewFile.Show
  Exit Sub
  
ErrHandler:
  MsgBox "Error when setting up file to view", 16, "ERROR"
  Close #36
  Exit Sub
ErrHandler2:
  MsgBox "Could not find any report data for this unit", 64, "INFORMATION"
  Close #36
  Exit Sub
End Sub

Public Sub ExtractStreamViewFile(StreamNumber As Integer)
'********************************************************
  Dim LinesFromFile As String
  Dim ReturnFile As String
  Dim Match As Boolean
  
  On Error GoTo ErrHandler
  
  ViewFile.Caption = EchoFileName
  
  ReturnFile = Dir(RTrim(JobPath) + EchoFileName)
  If ReturnFile = "" Then
    MsgBox "Could not find the requested file", 16, "ERROR"
    Exit Sub
  End If
 
  Open RTrim(JobPath) + EchoFileName For Input As #36
  ViewFile.Text1.text = ""
  LinesFromFile = ""
  
  Match = False
  Do While Not Match And Not EOF(36)
    Line Input #36, WSTRING
    If Mid(WSTRING, 1, 14) = "Stream number:" And Val(Mid(WSTRING, 31, 3)) = StreamNumber Then
      Match = True
    End If
  Loop
  If Not Match Then GoTo ErrHandler2
  LinesFromFile = LinesFromFile & WSTRING & Chr(13) & Chr(10)
  
  Match = False
  Do While Not Match And Not EOF(36)
    Line Input #36, WSTRING
    If Mid(WSTRING, 1, 14) = "Stream number:" Then
      LinesFromFile = LinesFromFile & " "
      Exit Do
    Else
      LinesFromFile = LinesFromFile & WSTRING & Chr(13) & Chr(10)
    End If
  Loop
  
  Close #36
  ViewFile.Text1.text = LinesFromFile
  ViewFile.LblDate.Caption = FileDateTime(RTrim(JobPath) + EchoFileName)
  
  If EchoFileName = "TEARS.OUT" Then
    ViewFile.MnuClearTears.Visible = True
  End If
  ViewFile.Top = 0
  ViewFile.Left = 0
  ViewFile.Show
  Exit Sub
  
ErrHandler:
  MsgBox "Error when setting up file to view", 16, "ERROR"
  Close #36
  Exit Sub
ErrHandler2:
  MsgBox "Could not find any properties for this stream", 64, "INFORMATION"
  Close #36
  Exit Sub
End Sub

Private Sub WateronlyCyclone_Click()
'*************************
  SelectUnit 56, Iconsize, Reflectflag
End Sub

Private Sub mnuWaterInjectionCyclone_Click()
'*******************************************
  SelectUnit 66, Iconsize, Reflectflag
End Sub

Private Sub WaterStream_Click()
'******************************
'Start a water addition stream
  StatusBar1.Panels(1).text = "The next stream to be started will be for water addition."
  WaterStreamFlagOn = True
  If LastCursorUsed = 0 Then
    Call RectCursor_Click
  Else
    Call RBCursor_Click
  End If
End Sub

Private Sub Wemco_Click()
'************************
  SelectUnit 45, Iconsize, Reflectflag

End Sub

Private Sub WetMagDrumSeparator_Click()
'**************************************
  SelectUnit 11, Iconsize, Reflectflag

End Sub


Private Sub WHIMS_Click()
'************************
  SelectUnit 25, Iconsize, Reflectflag

End Sub


