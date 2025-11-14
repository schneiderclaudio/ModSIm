MODULE WorkingCommon
!*******************
	!Need a dummy common area to hold the experimental settings to transmit to SOFSQ
  Character*255 WCJobPath
  Integer PathLength
  TYPE ExptObs
    SEQUENCE
    Integer Stream
    Real MeshSize
    Real FractionPassing
    Real LRESP
    Real URESP
  END TYPE ExptObs
  TYPE(ExptObs) :: Observations(2500)
  Integer NoSizeDistributions,StreamNumbers(100),StreamNDC(100)
  Real Objectives(2500)
	Integer ParameterNumber(5),UnitNumber(5),UnitID(5)
	INTEGER NumberOfObservations
  Integer NDC
	COMMON /DataArea/Observations,NumberOfObservations,ParameterNumber,NDC,NoSizeDistributions,StreamNumbers,StreamNDC,Objectives,WCJobPath
END MODULE