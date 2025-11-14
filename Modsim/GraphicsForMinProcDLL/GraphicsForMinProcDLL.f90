!  GraphicsForMinProcDLL.f90 

Real function GammaFunction(z)
!*****************************
  !DEC$ ATTRIBUTES STDCALL, DLLEXPORT::GammaFunction
  !DEC$ ATTRIBUTES ALIAS : 'GammaFunction' :: GammaFunction
  Real z
  !DEC$ ATTRIBUTES REFERENCE :: z
  REAL GAMMA

  Gammafunction = GAMMA(z)

end function GammaFunction
!
subroutine Kolmogorov_Smirnov(NOBSX,X,NOBSY,Y,D,ProbOfExceedingD)
!****************************************************************

  ! Expose subroutine Kolmogorov_Smirnov to users of this DLL
  !
  !DEC$ ATTRIBUTES STDCALL, DLLEXPORT::Kolmogorov_Smirnov
  !DEC$ ATTRIBUTES ALIAS : 'Kolmogorov_Smirnov' :: Kolmogorov_Smirnov
  INTEGER NOBSX,NOBSY
  !DEC$ ATTRIBUTES REFERENCE :: NOBSX,NOBSY
  REAL  X(NOBSX),Y(NOBSY)
  REAL D,ProbOfExceedingD
  !DEC$ ATTRIBUTES REFERENCE :: X, Y, D, ProbOfExceedingD

  REAL PDIF(6)


  Call KSTWO(NOBSX,X,NOBSY,Y,PDIF,NMISSX,NMISSY)
  D = PDIF(1)
  ProbOfExceedingD = PDIF(6)

end subroutine Kolmogorov_Smirnov

subroutine TandFtest(JobPath,NOBSX,X,NOBSY,Y,T,F,ProbOfExceedingT,ProbOfExceedingF)
!**********************************************************************************
  !DEC$ ATTRIBUTES STDCALL, DLLEXPORT::TandFtest
  !DEC$ ATTRIBUTES ALIAS : 'TandFtest' :: TandFtest
  CHARACTER*256 JobPath
  !Dec$ ATTRIBUTES REFERENCE :: JobPath
  INTEGER*4 NOBSX,NOBSY
  !DEC$ ATTRIBUTES REFERENCE :: NOBSX,NOBSY
  REAL*4  X(NOBSX),Y(NOBSY)
  REAL*4 T,ProbOfExceedingT
  REAL*4 F,ProbOfExceedingF
  !DEC$ ATTRIBUTES REFERENCE :: X, Y, T, ProbOfExceedingT, F, ProbOfExceedingF

  REAL STAT(25)
  INTEGER IDO,IPRINT,DiagFile

 

	!Open a file to report diagnostics coming from the DLL
	LENG = LEN_TRIM(JobPath)
	DiagFile = 13
	OPEN(DiagFile, FILE = JobPath(1:LENG)//'DIAGDLL.TXT')
	WRITE(DiagFile,*)' This file shows a record of the progress through the T and F test '
	!Set the IMSL error unit number to 13
	Call UMACH(-3,DiagFile)
	!Set the IMSL Stdout unit number to 13
	Call UMACH(-2,DiagFile)

  IDO = 0
  IPRINT = 0
  CONPRM = 95.0
  CONPRV = 95.0
  Call TWOMV(IDO,NOBSX,X,NOBSY,Y,CONPRM,CONPRV,IPRINT,STAT)
  T = STAT(15)
  ProbOfExceedingT = STAT(16)
  F = STAT(20)
  ProbOfExceedingF = STAT(21)


  Close(Diagfile)
end subroutine TandFtest