VERSION 5.00
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "tabctl32.ocx"
Begin VB.Form Models 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Add model names"
   ClientHeight    =   8700
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   10125
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   8700
   ScaleWidth      =   10125
   Begin VB.CommandButton cmdDelete 
      Caption         =   "Delete"
      Height          =   375
      Left            =   7920
      TabIndex        =   52
      Top             =   120
      Width           =   975
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   7695
      Left            =   120
      TabIndex        =   56
      Top             =   840
      Width           =   9855
      _ExtentX        =   17383
      _ExtentY        =   13573
      _Version        =   393216
      Tabs            =   2
      Tab             =   1
      TabsPerRow      =   5
      TabHeight       =   520
      BackColor       =   12632256
      TabCaption(0)   =   "Model parameters"
      TabPicture(0)   =   "Models.frx":0000
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "Frame1"
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Help text"
      TabPicture(1)   =   "Models.frx":001C
      Tab(1).ControlEnabled=   -1  'True
      Tab(1).Control(0)=   "txtEdit"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "cmdAddParameters"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).ControlCount=   2
      Begin VB.CommandButton cmdAddParameters 
         Caption         =   "Add parameters"
         Height          =   255
         Left            =   8280
         TabIndex        =   63
         Top             =   0
         Width           =   1335
      End
      Begin VB.TextBox txtEdit 
         Height          =   6855
         Left            =   240
         MultiLine       =   -1  'True
         TabIndex        =   62
         Top             =   600
         Width           =   9255
      End
      Begin VB.Frame Frame1 
         Caption         =   "Parameter specifications"
         Height          =   7005
         Left            =   -74760
         TabIndex        =   57
         Top             =   480
         Width           =   9015
         Begin VB.TextBox txtNumberOfParameters 
            Height          =   285
            Left            =   3000
            TabIndex        =   2
            Top             =   360
            Width           =   615
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   0
            Left            =   240
            TabIndex        =   3
            Top             =   1080
            Width           =   5415
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   0
            Left            =   5880
            TabIndex        =   4
            Top             =   1080
            Width           =   975
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   0
            ItemData        =   "Models.frx":0038
            Left            =   7440
            List            =   "Models.frx":003A
            TabIndex        =   5
            Text            =   "cboConversion"
            Top             =   1080
            Width           =   1095
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   1
            Left            =   240
            TabIndex        =   6
            Top             =   1440
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   1
            Left            =   5880
            TabIndex        =   7
            Top             =   1440
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   1
            ItemData        =   "Models.frx":003C
            Left            =   7440
            List            =   "Models.frx":003E
            TabIndex        =   8
            Text            =   "cboConversion"
            Top             =   1440
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   2
            ItemData        =   "Models.frx":0040
            Left            =   7440
            List            =   "Models.frx":0042
            TabIndex        =   11
            Text            =   "cboConversion"
            Top             =   1800
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   2
            Left            =   5880
            TabIndex        =   10
            Top             =   1800
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   2
            Left            =   240
            TabIndex        =   9
            Top             =   1800
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   3
            ItemData        =   "Models.frx":0044
            Left            =   7440
            List            =   "Models.frx":0046
            TabIndex        =   14
            Text            =   "cboConversion"
            Top             =   2160
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   3
            Left            =   5880
            TabIndex        =   13
            Top             =   2160
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   3
            Left            =   240
            TabIndex        =   12
            Top             =   2160
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   4
            ItemData        =   "Models.frx":0048
            Left            =   7440
            List            =   "Models.frx":004A
            TabIndex        =   17
            Text            =   "cboConversion"
            Top             =   2520
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   4
            Left            =   5880
            TabIndex        =   16
            Top             =   2520
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   4
            Left            =   240
            TabIndex        =   15
            Top             =   2520
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   5
            Left            =   7440
            TabIndex        =   20
            Text            =   "cboConversion"
            Top             =   2880
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   5
            Left            =   5880
            TabIndex        =   19
            Top             =   2880
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   5
            Left            =   240
            TabIndex        =   18
            Top             =   2880
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   6
            Left            =   7440
            TabIndex        =   23
            Text            =   "cboConversion"
            Top             =   3240
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   6
            Left            =   5880
            TabIndex        =   22
            Top             =   3240
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   6
            Left            =   240
            TabIndex        =   21
            Top             =   3240
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   7
            Left            =   7440
            TabIndex        =   26
            Text            =   "cboConversion"
            Top             =   3600
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   7
            Left            =   5880
            TabIndex        =   25
            Top             =   3600
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   7
            Left            =   240
            TabIndex        =   24
            Top             =   3600
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   8
            Left            =   7440
            TabIndex        =   29
            Text            =   "cboConversion"
            Top             =   3960
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   8
            Left            =   5880
            TabIndex        =   28
            Top             =   3960
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   8
            Left            =   240
            TabIndex        =   27
            Top             =   3960
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   9
            Left            =   7440
            TabIndex        =   32
            Text            =   "cboConversion"
            Top             =   4320
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   9
            Left            =   5880
            TabIndex        =   31
            Top             =   4320
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   9
            Left            =   240
            TabIndex        =   30
            Top             =   4320
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   10
            Left            =   7440
            TabIndex        =   35
            Text            =   "cboConversion"
            Top             =   4680
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   10
            Left            =   5880
            TabIndex        =   34
            Top             =   4680
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   10
            Left            =   240
            TabIndex        =   33
            Top             =   4680
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   11
            Left            =   7440
            TabIndex        =   38
            Text            =   "cboConversion"
            Top             =   5040
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   11
            Left            =   5880
            TabIndex        =   37
            Top             =   5040
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   11
            Left            =   240
            TabIndex        =   36
            Top             =   5040
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   12
            Left            =   7440
            TabIndex        =   41
            Text            =   "cboConversion"
            Top             =   5400
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   12
            Left            =   5880
            TabIndex        =   40
            Top             =   5400
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   12
            Left            =   240
            TabIndex        =   39
            Top             =   5400
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   13
            Left            =   7440
            TabIndex        =   44
            Text            =   "cboConversion"
            Top             =   5760
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   13
            Left            =   5880
            TabIndex        =   43
            Top             =   5760
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   13
            Left            =   240
            TabIndex        =   42
            Top             =   5760
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   14
            Left            =   7440
            TabIndex        =   47
            Text            =   "cboConversion"
            Top             =   6120
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   14
            Left            =   5880
            TabIndex        =   46
            Top             =   6120
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   14
            Left            =   240
            TabIndex        =   45
            Top             =   6120
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.ComboBox cboConversion 
            Appearance      =   0  'Flat
            Height          =   315
            Index           =   15
            Left            =   7440
            TabIndex        =   50
            Text            =   "cboConversion"
            Top             =   6480
            Visible         =   0   'False
            Width           =   1095
         End
         Begin VB.TextBox txtDefault 
            Height          =   285
            Index           =   15
            Left            =   5880
            TabIndex        =   49
            Top             =   6480
            Visible         =   0   'False
            Width           =   975
         End
         Begin VB.TextBox txtPrompt 
            Height          =   285
            Index           =   15
            Left            =   240
            TabIndex        =   48
            Top             =   6480
            Visible         =   0   'False
            Width           =   5415
         End
         Begin VB.Label Label3 
            Caption         =   "Number of parameters for this model"
            Height          =   255
            Left            =   240
            TabIndex        =   61
            Top             =   360
            Width           =   2655
         End
         Begin VB.Label Label4 
            Alignment       =   2  'Center
            Caption         =   "Prompt"
            Height          =   255
            Left            =   240
            TabIndex        =   60
            Top             =   840
            Width           =   5175
         End
         Begin VB.Label Label5 
            Caption         =   "Default value"
            Height          =   255
            Left            =   5880
            TabIndex        =   59
            Top             =   840
            Width           =   1095
         End
         Begin VB.Label Label6 
            Caption         =   "Unit conversion"
            Height          =   255
            Left            =   7440
            TabIndex        =   58
            Top             =   840
            Width           =   1455
         End
      End
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "Clear"
      Height          =   375
      Left            =   9120
      TabIndex        =   53
      Top             =   120
      Width           =   855
   End
   Begin VB.CommandButton cmdAccept 
      Caption         =   "Accept"
      Height          =   375
      Left            =   6720
      TabIndex        =   51
      Top             =   120
      Width           =   975
   End
   Begin VB.ComboBox cboModel 
      Height          =   315
      Left            =   4200
      TabIndex        =   1
      Tag             =   "cboModel"
      ToolTipText     =   "4-character model name"
      Top             =   360
      Width           =   855
   End
   Begin VB.ComboBox cboClasses 
      Height          =   315
      ItemData        =   "Models.frx":004C
      Left            =   240
      List            =   "Models.frx":004E
      Sorted          =   -1  'True
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   360
      Width           =   2775
   End
   Begin VB.Label lblUnitType 
      Caption         =   "Unit type"
      Height          =   255
      Left            =   3120
      TabIndex        =   64
      Top             =   360
      Visible         =   0   'False
      Width           =   735
   End
   Begin VB.Label Label2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "4-character model name"
      Height          =   255
      Left            =   4080
      TabIndex        =   55
      Top             =   120
      Width           =   1815
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select unit type for this model"
      Height          =   255
      Left            =   120
      TabIndex        =   54
      Top             =   120
      Width           =   2655
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuSave 
         Caption         =   "Save"
      End
      Begin VB.Menu mnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "Models"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private ModelName As String * 4
Private UnitClassID As Integer
Private UnitClassName As String
Private NumberOfUserModels As Integer
Private ModelID As Integer
Private EditFlag As Boolean
Private HelpStringStart As Integer, HelpStringEnd As Integer, HelpStringLength As Integer

Private Sub cboClasses_Click()
'*****************************
  Dim I As Integer, J As Integer
    ClearForm
    I = FindUnitClass(cboClasses.Text)
    Me.lblUnitType.Caption = "Unit type " & CStr(UnitClass(I).ClassNumber)
    For J = 1 To UnitClass(I).NumberClassModels
      cboModel.AddItem UnitClass(I).ClassModel(J)
    Next J
End Sub

Private Sub cboClasses_Change()
'*****************************
  Dim I As Integer, J As Integer
    ClearForm
    I = FindUnitClass(cboClasses.Text)
    For J = 1 To UnitClass(I).NumberClassModels
      cboModel.AddItem UnitClass(I).ClassModel(J)
    Next J
End Sub

Private Sub cboModel_Change()
'****************************
  Dim Name As String * 4
  EditFlag = False
  Name = Me.cboModel.Text
  ModelName = Name
  Call LoadPrompts(Name)
End Sub

Private Sub cboModel_Click()
'****************************
  Dim Name As String * 4
  EditFlag = True
  Name = Me.cboModel.Text
  ModelName = Name
  Call LoadPrompts(Name)
End Sub

Private Sub cboModel_LostFocus()
'******************************
  Dim Name As String * 4
  Name = Me.cboModel.Text
  If VerifyName(Name) Then Exit Sub
  ModelName = Name
  Call LoadPrompts(Name)
End Sub

Private Sub LoadPrompts(Name As String)
'**************************************
  Dim I As Integer, J As Integer
  'Load the parameter prompts and defaults for this model.
  For I = 0 To 100
    If Name = Model(I).Name Then
      Me.txtNumberOfParameters.Text = CStr(Model(I).NumberOfPrompts)
      For J = 0 To Model(I).NumberOfPrompts - 1
        Me.txtPrompt(J).Text = Model(I).Prompt(J)
        Me.txtPrompt(J).Visible = True
        Me.txtDefault(J).Text = CStr(Model(I).Default(J))
        Me.txtDefault(J).Visible = True
        Me.cboConversion(J).Text = Model(I).Conversion(J)
        Me.cboConversion(J).Visible = True
      Next J
      For J = Model(I).NumberOfPrompts To 15
        Me.txtPrompt(J).Visible = False
        Me.txtDefault(J).Visible = False
        Me.cboConversion(J).Visible = False
      Next J
    End If
  Next I
  
  'Load the help file text for this model
  HelpStringEnd = 1
  Do
    HelpStringStart = InStr(HelpStringEnd, StringIn, ".context")
    HelpStringEnd = InStr(HelpStringStart + 9, StringIn, ".context")
    If HelpStringEnd = 0 Then
      HelpStringEnd = Len(StringIn) + 1
    End If
    If Mid(StringIn, HelpStringStart + 9, 4) = ModelName Then
      HelpStringLength = HelpStringEnd - HelpStringStart
      txtEdit.Text = Mid(StringIn, HelpStringStart + 15, HelpStringLength - 15)
      Exit Do
    Else
      HelpStringStart = HelpStringEnd
    End If
  Loop While HelpStringStart < Len(StringIn)
End Sub

Private Function VerifyName(Name As String) As Boolean
'*****************************************************
Dim I As Integer, J As Integer
Dim Response As Integer
VerifyName = False
For I = 1 To MainNumberOfClasses
  For J = 1 To MainUnitClass(I).NumberClassModels
    If Name = MainUnitClass(I).ClassModel(J) Then
      Response = MsgBox("This model name is already in use." & vbCrLf & "You cannot specify parameter prompts.", vbOKCancel, "WARNING")
      If Response = vbCancel Then
        VerifyName = True
        ClearForm
      Else
        VerifyName = False
      End If
      Exit Function
    End If
  Next J
  For J = 1 To UnitClass(I).NumberClassModels
    If Name = UnitClass(I).ClassModel(J) And Not EditFlag Then
      Response = MsgBox("This model name is already in use", vbOKCancel, "WARNING")
      If Response = vbCancel Then
        VerifyName = True
        Call cmdCancel_Click
      Else
        VerifyName = False
      End If
      Exit Function
    End If
  Next J
Next I
End Function

Private Sub cmdAccept_Click()
'****************************
  Dim I As Integer, J As Integer
  Dim NewModel As Boolean
  
  'Check data validity before accepting
  If Me.cboModel.Text = "" Then
    MsgBox "A 4-character model name must be specified", vbCritical, "ERROR"
    Me.cboModel.SetFocus
    Exit Sub
  End If
  If Me.txtNumberOfParameters.Text = "" Then
    MsgBox "Number of parameters must be specified", vbCritical, "ERROR"
    Me.txtNumberOfParameters.SetFocus
    Exit Sub
  End If
  If Val(Me.txtNumberOfParameters.Text) > 0 Then
    For I = 1 To Val(Me.txtNumberOfParameters.Text)
      If Me.txtPrompt(I - 1).Text = "" Then
        MsgBox "Parameter prompt is missing", vbCritical, "ERROR"
        Me.txtPrompt(I - 1).SetFocus
        Exit Sub
      End If
      If Me.txtDefault(I - 1).Text = "" Then
        MsgBox "Default value is missing", vbCritical, "ERROR"
        Me.txtDefault(I - 1).SetFocus
        Exit Sub
      End If
    Next I
  End If
  If Me.txtEdit.Text = "" Then
    MsgBox "Help text has not been specified", vbCritical, "ERROR"
    Me.txtEdit.SetFocus
    Exit Sub
  End If
  
  UnitClassName = cboClasses.Text
  For UnitClassID = 1 To NumberOfClasses
    If UnitClass(UnitClassID).ClassName = UnitClassName Then
      Exit For
    End If
  Next UnitClassID
    
  If UnitClassID > 0 Then
    'Is this a new model for this unit class?
    NewModel = True
    For I = 1 To UnitClass(UnitClassID).NumberClassModels
      If UnitClass(UnitClassID).ClassModel(I) = cboModel.Text Then
        NewModel = False
        Exit For
      End If
    Next I
    If NewModel Then
      J = UnitClass(UnitClassID).NumberClassModels + 1
      UnitClass(UnitClassID).NumberClassModels = J
      UnitClass(UnitClassID).ClassModel(J) = cboModel.Text
    End If

    'Is this a new user model?
    NewModel = True
    For I = 0 To NumberOfUserModels - 1
      If Model(I).Name = cboModel.Text Then
        NewModel = False
        ModelID = I
        Exit For
      End If
    Next I
    If NewModel Then
      NumberOfUserModels = NumberOfUserModels + 1
      Model(NumberOfUserModels - 1).Name = cboModel.Text
      Model(NumberOfUserModels - 1).NumberOfPrompts = Val(txtNumberOfParameters.Text)
      For I = 0 To Model(NumberOfUserModels - 1).NumberOfPrompts - 1
        Model(NumberOfUserModels - 1).Prompt(I) = txtPrompt(I)
        Model(NumberOfUserModels - 1).Default(I) = Val(txtDefault(I).Text)
        Model(NumberOfUserModels - 1).Conversion(I) = cboConversion(I).Text
      Next I
    Else
      Model(ModelID).NumberOfPrompts = Val(txtNumberOfParameters.Text)
      For I = 0 To Model(ModelID).NumberOfPrompts - 1
        Model(ModelID).Prompt(I) = txtPrompt(I)
        Model(ModelID).Default(I) = Val(txtDefault(I).Text)
        Model(ModelID).Conversion(I) = cboConversion(I).Text
      Next I
    End If
  End If
  
  'Accept the edited help text instead of the original. Put at the end of the file.
  StringOut = Mid(StringIn, 1, HelpStringStart - 1) & Mid(StringIn, HelpStringEnd) & vbCrLf & ".context " & ModelName & vbCrLf & txtEdit.Text
  StringIn = StringOut
  
  ClearForm
  HasChanged = True
End Sub

Private Sub cmdAddParameters_Click()
'***********************************
'Add the parameter descriptions to the help text.
  Dim I As Integer
  txtEdit = txtEdit & vbCrLf & "Parameters:"
  For I = 0 To Val(txtNumberOfParameters)
    txtEdit = txtEdit & vbCrLf & "  " & txtPrompt(I).Text
  Next I
End Sub

Private Sub cmdDelete_Click()
'****************************
  Dim I As Integer, J As Integer
  Dim NewModel As Boolean
  Dim Response As Long
  
  Response = MsgBox("Are you sure that you want to delete this model?", vbOKCancel, "WARNING")
  If Response = vbCancel Then
    Exit Sub
  End If
  
  UnitClassName = cboClasses.Text
  For UnitClassID = 1 To NumberOfClasses
    If UnitClass(UnitClassID).ClassName = UnitClassName Then
      Exit For
    End If
  Next UnitClassID
    
  If UnitClassID > 0 Then
    'Is this a new model for this unit class?
    NewModel = True
    For I = 1 To UnitClass(UnitClassID).NumberClassModels
      If UnitClass(UnitClassID).ClassModel(I) = cboModel.Text Then
        ModelID = I
        NewModel = False
        Exit For
      End If
    Next I
    If Not NewModel Then
      J = UnitClass(UnitClassID).NumberClassModels - 1
      UnitClass(UnitClassID).NumberClassModels = J
      For I = ModelID To J
        UnitClass(UnitClassID).ClassModel(I) = UnitClass(UnitClassID).ClassModel(I + 1)
      Next I
    End If

    'Is this a new user model?
    NewModel = True
    For I = 0 To NumberOfUserModels - 1
      If Model(I).Name = cboModel.Text Then
        NewModel = False
        ModelID = I
        Exit For
      End If
    Next I
    If NewModel Then
    Else
      NumberOfUserModels = NumberOfUserModels - 1
      For J = ModelID To NumberOfUserModels - 1
        Model(J).Name = Model(J + 1).Name
        Model(J).NumberOfPrompts = Model(J + 1).NumberOfPrompts
        For I = 0 To Model(J).NumberOfPrompts - 1
          Model(J).Prompt(I) = Model(J + 1).Prompt(I)
          Model(J).Default(I) = Model(J + 1).Default(I)
          Model(J).Conversion(I) = Model(J + 1).Conversion(I)
        Next I
      Next J
    End If
  End If
  
  'Save the help text without the current stuff
  StringOut = Mid(StringIn, 1, HelpStringStart - 1) & Mid(StringIn, HelpStringEnd)
  StringIn = String(Len(StringOut), " ")
  StringIn = StringOut
  ClearForm
  Call mnuSave_Click
End Sub

Private Sub mnuClose_Click()
'***************************
  Dim Response As Integer
  If HasChanged Then
    Response = MsgBox("Save changes to Modsim?", vbYesNoCancel)
    If Response = vbYes Then
      Call mnuSave_Click
    ElseIf Response = vbNo Then
    Else
      Exit Sub
    End If
  End If
  Unload Me
End Sub

Private Sub mnuSave_Click()
'**************************
  Dim I As Integer, J As Integer
  Dim WSTRING As String
  Open RTrim(JobPath) & "UserPlant.txt" For Output As #32
  Print #32, NumberOfClasses
  For I = 1 To NumberOfClasses
    Print #32, Format(UnitClass(I).ClassNumber, "00") & "           " & UnitClass(I).ClassName
    Print #32, UnitClass(I).NumberClassModels
    For J = 1 To UnitClass(I).NumberClassModels
      Print #32, UnitClass(I).ClassModel(J)
    Next J
  Next I
  Close #32
   
  Open RTrim(JobPath) & "UserModQues.txt" For Output As #32
  For I = 0 To NumberOfUserModels - 1
    Print #32, Model(I).Name
    Print #32, Model(I).NumberOfPrompts
    For J = 0 To Model(I).NumberOfPrompts - 1
      Print #32, Model(I).Prompt(J); ","; Model(I).Default(J); ","; Model(I).Conversion(J)
    Next J
  Next I
  Close #32
  'Write the help text
  Open RTrim(JobPath) & "UserModels.hlp" For Output As #32
  Print #32, StringOut
  Close #32
  
  HasChanged = False
End Sub

Private Sub cmdCancel_Click()
'****************************
ClearForm
End Sub

Private Sub Form_Load()
'*********************
  Dim I As Integer, J As Integer
  Dim UserModel As Integer
  
  'Load the unit type data from PLANT.DAT
  Open RTrim(JobPath) & "Plant.dat" For Input As #31
  Input #31, MainNumberOfClasses
  For I = 1 To MainNumberOfClasses
    WSTRING = Input(2, #31)
    MainUnitClass(I).ClassNumber = Val(WSTRING)
    WSTRING = Input(11, #31)
    Input #31, MainUnitClass(I).ClassName
    Input #31, MainUnitClass(I).NumberClassModels
    For J = 1 To MainUnitClass(I).NumberClassModels
      Input #31, MainUnitClass(I).ClassModel(J)
    Next J
  Next I
  Close (31)
  
  'Load the classes into the class list
  Open RTrim(JobPath) & "UserPlant.txt" For Input As #31
  Input #31, NumberOfClasses
  For I = 1 To NumberOfClasses
    WSTRING = Input(2, #31)
    UnitClass(I).ClassNumber = Val(WSTRING)
    WSTRING = Input(11, #31)
    Input #31, UnitClass(I).ClassName
    If Not ExcludeClass(UnitClass(I).ClassName) Then
      cboClasses.AddItem UnitClass(I).ClassName
    End If
    Input #31, UnitClass(I).NumberClassModels
    For J = 1 To UnitClass(I).NumberClassModels
      Input #31, UnitClass(I).ClassModel(J)
    Next J
  Next I
  Close 31
  
  'Read the model parameter data
  Open RTrim(JobPath) & "UserModQues.txt" For Input As #31
  J = 0
  Do While Not EOF(31)
    Input #31, Model(J).Name
    Input #31, Model(J).NumberOfPrompts
    For I = 0 To Model(J).NumberOfPrompts - 1
      Input #31, Model(J).Prompt(I)
      Input #31, Model(J).Default(I)
      Input #31, Model(J).Conversion(I)
    Next I
    J = J + 1
  Loop
  NumberOfUserModels = J
  Close #31
  For J = 0 To 15
    Me.cboConversion(J).AddItem "NONE"
    Me.cboConversion(J).AddItem "SIZE"
    Me.cboConversion(J).AddItem "DENSITY"
    Me.cboConversion(J).AddItem "FLOW"
  Next J
  
  'Load the help text
  Open RTrim(JobPath) & "UserModels.hlp" For Binary As #31
  StringIn = Space(LOF(31))
  Get #31, , StringIn
  Close #31
End Sub

Function ExcludeClass(Name As String) As Boolean
'***********************************************
'Check if class is excluded
ExcludeClass = False
If Name = "Mixer" Then ExcludeClass = True
If Name = "Sump" Then ExcludeClass = True
If Name = "Stream splitter" Then ExcludeClass = True
If Name = "Stockpile" Then ExcludeClass = True
If Name = "Conveyor belt" Then ExcludeClass = True
If Name = "Pass-through unit" Then ExcludeClass = True
End Function

Function FindUnitClass(Name As String) As Integer
'************************************************
  Dim I As Integer
  For I = 1 To NumberOfClasses
    If Name = UnitClass(I).ClassName Then
      FindUnitClass = I
      Exit Function
    End If
  Next I
  FindUnitClass = 0
End Function

Sub ClearForm()
'**************
'Clears the form
Dim I As Integer
Me.cboModel.Clear
Me.txtNumberOfParameters.Text = ""
For I = 0 To 15
  Me.txtPrompt(I).Text = ""
  Me.txtPrompt(I).Visible = False
  Me.txtDefault(I).Text = ""
  Me.txtDefault(I).Visible = False
  Me.cboConversion(I).Text = "NONE"
  Me.cboConversion(I).Visible = False
Next I
Me.txtPrompt(0).Visible = True
Me.txtDefault(0).Visible = True
Me.cboConversion(0).Visible = True

Me.txtEdit.Text = ""

End Sub

Private Sub txtNumberOfParameters_Lostfocus()
'********************************************
  Dim I As Integer, J As Integer
  J = Val(Me.txtNumberOfParameters.Text)
  For I = 0 To J - 1
    Me.txtPrompt(I).Visible = True
    Me.txtDefault(I).Visible = True
    Me.cboConversion(I).Visible = True
  Next I
  For I = J To 15
     Me.txtPrompt(I).Text = ""
    Me.txtPrompt(I).Visible = False
    Me.txtDefault(I).Text = ""
    Me.txtDefault(I).Visible = False
    Me.cboConversion(I).Clear
    Me.cboConversion(I).Visible = False
  Next I
End Sub
