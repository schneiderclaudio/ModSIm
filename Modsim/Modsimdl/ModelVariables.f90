!This module is created by program DIMINP.FOR
!It must not be changed by the user.
MODULE MODELVARIABLES
  !MS$ ATTRIBUTES DLLEXPORT :: /MODELVARS/
  INTEGER UnitNumber
  LOGICAL Reporting
  Character*80 JobName
  CHARACTER*256 Message
  INTEGER NumSizeClasses,NumGClasses,NumSClasses
  REAL TotalSolidsF
  REAL TotalSolidsT, TotalSolidsC, TotalSolidsM
  REAL FeedWater
  REAL TailingsWater, ConcentrateWater, MiddlingsWater
  INTEGER NumberOfMessages,NumberOfMinerals,UnitType

  REAL GradeM( 22,  7), GradeV( 22,  7)
  REAL SolidSpGr( 22)
  REAL Texture( 50)
  REAL MagnSusceptG( 22)
  REAL OtherPropG( 22)
  REAL FlotnRateConst( 10)
  REAL MagnSusceptS( 10)
  REAL OtherPropS( 10)
  REAL CalValue( 22)
  REAL TotalSulfur( 22)
  REAL PyriticSulf( 22)
  REAL F_(  7702)
  REAL Feed( 35, 22, 10)
  EQUIVALENCE (F_(1),Feed(1,1,1))
  Real T_(  7702)
  REAL Tailing( 35, 22, 10)
  EQUIVALENCE (T_(1),Tailing(1,1,1))
  Real C_(  7702)
  REAL Concentrate( 35, 22, 10)
  EQUIVALENCE (C_(1),Concentrate(1,1,1))
  Real M_(  7702)
  REAL Middling( 35, 22, 10)
  EQUIVALENCE (M_(1),Middling(1,1,1))
  REAL Parameters( 150)
  REAL RepSize(  35)
 
  COMMON /MODELVARS/&
  UnitNumber,Reporting,NumSizeClasses,NumGClasses,NumSClasses,&
  UnitType,Feed,Tailing,Concentrate,Middling,&
  TotalSolidsF,TotalSolidsT,TotalSolidsC,TotalSolidsM,&
  FeedWater, TailingsWater, ConcentrateWater, MiddlingsWater,&
  Parameters,RepSize,NumberOfMessages,NumberOfMinerals,&
  GradeM,GradeV,&
  SolidSpGr,Texture,MagnSusceptG,OtherPropG,FlotnRateConst,&
  MagnSusceptS,OtherPropS,CalValue,TotalSulfur,PyriticSulf,&
  JobName
End MODULE MODELVARIABLES
