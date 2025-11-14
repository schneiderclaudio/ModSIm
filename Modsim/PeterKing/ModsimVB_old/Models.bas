Attribute VB_Name = "ModelBase"
Option Explicit
  Public Type ModelData
    ID As Integer
    Model As String * 4
    Unit  As Integer
    NOPAR As Integer
    PARAM(1 To 50) As Single
  End Type
  Public CURRMODELDATA(0 To 100) As ModelData
  Public CURRMODELIndex(100) As Integer
  Public UNIT_DESCR(99) As String
  Public UNIT_MODS(99) As Integer
  Public SUBR(99, 20) As String * 4
  Public Model As String * 4
  Public KK(15) As Single
  
  Public NUNIT As Integer, UNIT_TYPE(100) As Integer, UNIT_ID(100) As Integer
  'Parameters for repeated simulation
  Public IDIFF As Integer
  Public RepUnitNumber(1 To 5) As Integer
  Public UnitID(1 To 5) As Integer, ParameterNumber(1 To 5) As Integer
  Public StartValue(1 To 5) As Single, EndValue(1 To 5) As Single, StepValue(1 To 5) As Single
  Public LevelFlag(1 To 5) As Boolean, ParameterName(1 To 5) As String

Sub StartUnitMods()
'******************
  Dim I As Integer, M As Integer
  Dim UNIT_TYPES As Integer
  Dim Unit As Integer, DefaultModels As Integer, UserModels As Integer
  Dim K As Integer
  
  On Error GoTo ErrHandler
  'Get the data relevant to each model
  Open RTrim(FileReadPath) & "PLANT.DAT" For Input As #32
  Open RTrim(FileReadPath) & "UserPlant.txt" For Input As #33
  Input #33, K
  Input #32, UNIT_TYPES
  If UNIT_TYPES <> K Then GoTo ErrHandler
  For I = 1 To UNIT_TYPES
    Unit = Val(Input$(2, #32))
    WSTRING = Input$(11, #32)
    Input #32, UNIT_DESCR(Unit)
    Line Input #33, WSTRING
    Input #32, DefaultModels
    Input #33, UserModels
    UNIT_MODS(Unit) = DefaultModels + UserModels
    For M = 1 To DefaultModels
      Input #32, SUBR(Unit, M)
    Next M
    For M = DefaultModels + 1 To UNIT_MODS(Unit)
      Input #33, SUBR(Unit, M)
    Next M
  Next I
  Close (32)
  Close (33)
  Exit Sub
  
ErrHandler:
  MsgBox "List of user models is incorrect" & Chr(13) & Chr(10) & "Edit user models using the Model Software Development Kit", vbCritical, "ERROR"
  Close (32)
  Close (33)
  Exit Sub
End Sub

Public Sub ResetLevels()
'***********************
'Reset all repeat simulation level data
Dim I As Integer
For I = 1 To 5
  UnitID(I) = 0
  ParameterNumber(I) = 0
  StartValue(I) = 0
  EndValue(I) = 0
  StepValue(I) = 1
  LevelFlag(I) = False
  ParameterName(I) = ""
Next I
End Sub
