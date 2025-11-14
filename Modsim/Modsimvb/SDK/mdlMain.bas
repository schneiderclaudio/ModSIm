Attribute VB_Name = "mdlMain"
Option Explicit

Public JobPath As String * 256
Public NumberOfClasses As Integer, MainNumberOfClasses As Integer
Public Type UnitType
  ClassNumber As Integer
  ClassName As String
  NumberClassModels As Integer
  ClassModel(25) As String * 4
End Type
Public UnitClass(99) As UnitType
Public MainUnitClass(99) As UnitType
Public Type ModelData
  Name As String * 4
  NumberOfPrompts As Integer
  Prompt(0 To 50) As String
  Default(0 To 50) As Single
  Conversion(0 To 50) As String
End Type
Public Model(0 To 100) As ModelData
  
Public HasChanged As Boolean
Public StringIn As String, StringOut As String
Public WSTRING As String

Sub Main()
'*********
'JobPath = "C:\Documents and Settings\CSchneid\My Documents\My Software\Modsim\Modsimvb\"
JobPath = CurDir & "\"
HasChanged = False
Models.Show
End Sub
