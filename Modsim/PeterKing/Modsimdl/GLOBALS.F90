!This module is created by program DIMINP.FOR
!It must not be changed by the user.
MODULE GLOBALS
  !MS$ ATTRIBUTES DLLEXPORT :: /VBVariables/
  Integer MaxOutputStreams,MaxInputStreams
  Real MeshSizes( 35)
  COMMON /Global/MaxOutputStreams,MaxInputStreams,MeshSizes
 
  CHARACTER*255 UnitJobPath
  INTEGER*4 UnitExitValue,UnitDiagFile
  COMMON /VBVariables/UnitExitValue,UnitDiagFile,UnitJobPath
End MODULE GLOBALS
