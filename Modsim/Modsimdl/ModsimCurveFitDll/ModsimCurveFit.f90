Subroutine LevMarCurveFit(JobPath,JobReadFile,NOPAR,Parameters,XLB,XUB,SumOfSquares,LowerConfLim,UpperConfLim,ConfLevel)
!***********************************************************************************************************************

  ! Expose subroutine LevMarModsimFit to users of this DLL
  !
  !DEC$ ATTRIBUTES STDCALL, DLLEXPORT::LevMarCurveFit
  !DEC$ ATTRIBUTES ALIAS :'LevMarCurveFit' :: LEVMARCURVEFIT
   
	Use WorkingCommon
  Use GLOBALS

  ! Variables
  CHARACTER*255 JobPath
  !Dec$ ATTRIBUTES REFERENCE :: JobPath
  CHARACTER*255 JobReadFile
  !Dec$ ATTRIBUTES REFERENCE :: JobReadFile
	Integer*4 NOPAR
  !DEC$ ATTRIBUTES REFERENCE :: NOPAR
	Real Parameters(NOPAR)
  !DEC$ ATTRIBUTES REFERENCE :: Parameters
  Real XLB(NOPAR)
  !DEC$ ATTRIBUTES REFERENCE :: XLB
  Real XUB(NOPAR)
  !DEC$ ATTRIBUTES REFERENCE :: XUB
	Real SumOfSquares(2)
	!DEC$ ATTRIBUTES REFERENCE :: SumOfSquares
	Real*4 LowerConfLim(NOPAR),UpperConfLim(NOPAR),ConfLevel
  !DEC$ ATTRIBUTES REFERENCE :: LowerConfLim
  !DEC$ ATTRIBUTES REFERENCE :: UpperConfLim
  !DEC$ ATTRIBUTES REFERENCE :: ConfLevel

	!Import the model subroutines from MODSIM.DLL
	INTERFACE
	  SUBROUTINE MESH_SIZE(SIZES,NDC)
	  !DEC$ ATTRIBUTES DLLIMPORT::MESH_SIZE
		  REAL SIZES(NDC)
			INTEGER NDC
    END SUBROUTINE MESH_SIZE
  END INTERFACE

	INTERFACE
	  SUBROUTINE FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
	  !DEC$ ATTRIBUTES DLLIMPORT::FRPASS
		  REAL CUMSIZ(NDC),CUM(NDC),P,PSIZE
			INTEGER NDC
    END SUBROUTINE FRPASS
  END INTERFACE

	INTERFACE
	  SUBROUTINE PARTSZ(SIZES,FIDGK,NDC,NGC,NSC,CUMSIZ,CUM,DENS)
	  !DEC$ ATTRIBUTES DLLIMPORT::MESH_SIZE
			INTEGER NDC,NGC,NSC
      REAL SIZES(NDC),FIDGK(NDC,NGC,NSC),CUMSIZ(NDC),CUM(NDC),DENS(NDC)
    END SUBROUTINE PARTSZ
  END INTERFACE

	INTERFACE
	  SUBROUTINE PASSSZ(CUMSIZ,CUM,NDC,P,PSIZE)
	  !DEC$ ATTRIBUTES DLLIMPORT::MESH_SIZE
		  REAL CUMSIZ(NDC),CUM(NDC),P,PSIZE
			INTEGER NDC
    END SUBROUTINE PASSSZ
  END INTERFACE

	Integer NOBVAL
	Integer Direction
  Real, ALLOCATABLE :: XSCALE(:),FSCALE(:),Residuals(:),FJAC(:,:),XGUESS(:)
	Real, ALLOCATABLE :: Hessian(:,:),Gradient(:),VARCOV(:,:)
	Complex, ALLOCATABLE :: EigVals(:),EigVects(:,:)
	CHARACTER*6, ALLOCATABLE :: RLABEL(:),CLABEL(:)
	Real, ALLOCATABLE :: b(:)

	Real SS,SA,EPSFCN
  EXTERNAL TERMS


	Integer IPARAM(6)
	Real RPARAM(7)
	Real*4 Y
	Character*80 DataSetName
  Character*80 StreamName
	Character*5 OptMethod
	INTEGER DiagFile,LENG
	Integer iersvr,ipact,isact
  character*80 title(1)
  character*7 fmt(1) 


  ! Body of LevMarCurvrFit

	!Open a file to report diagnostics coming from the DLL
	LENG = LEN_TRIM(JobPath)
  UnitJobPath = JobPath
  UnitDiagFile = 113
	DiagFile = 113
	OPEN(DiagFile, FILE = JobPath(1:LENG)//'DIAGCurveFit.TXT')
	WRITE(DiagFile,*)' This file shows a record of the progress through the LevMarCurveFit DLL '
	Write(Diagfile,*) ' Number of parameters ',NOPAR
	!Set the error/stdout unit (IMSL removed — no-op stubs)
	Call UMACH(-3,DiagFile)
	Call UMACH(-2,DiagFile)

  WCJobPath = JobPath
  PathLength = LENG
	
	OPEN(31,FILE = JobPath(1:LENG)//'LevMarCurveFit.DAT')
	READ(31,'(A80)') DataSetName
	Write(DiagFile,'(80A)') DataSetName
	READ(31,'(A5)') OptMethod
	Write(DiagFile,'(A5)') OptMethod
	Do I = 1,NOPAR
	  READ(31,*)UnitNumber(I),UnitID(I),ParameterNumber(I)
  End Do
	READ(31,*) NoSizeDistributions,NDC
	Write(DiagFile,'(2I4)') NoSizeDistributions
	!Put this in common
  NumberOfObservations = 0
	DO I=1,NoSizeDistributions
    READ(31,'(A80)') StreamName
    READ(31,*) StreamNumbers(I),StreamNDC(I)
    Write(DiagFile,*)'I,StreamNumbers(I),StreamNDC(I)',I,StreamNumbers(I),StreamNDC(I)
    DO J = 1,StreamNDC(I) - 1
      NumberOfObservations = NumberOfObservations + 1
      Observations(NumberOfObservations)%Stream = StreamNumbers(I)
	  	READ(31,*) Observations(NumberOfObservations)%MeshSize,Observations(NumberOfObservations)%FractionPassing
		  Write(Diagfile,'(2G12.4)') Observations(NumberOfObservations)%MeshSize,Observations(NumberOfObservations)%FractionPassing
    End Do
	End Do
  NOBVAL = NumberOfObservations
	Close(31)

  IF( .NOT. ALLOCATED(XSCALE)) ALLOCATE(XSCALE(NOPAR))
  IF( .NOT. ALLOCATED(XGUESS)) ALLOCATE(XGUESS(NOPAR))
  IF( .NOT. ALLOCATED(FSCALE)) ALLOCATE(FSCALE(NOBVAL))
  IF( .NOT. ALLOCATED(Residuals)) ALLOCATE(Residuals(NOBVAL))
  IF( .NOT. ALLOCATED(FJAC)) ALLOCATE(FJAC(NOBVAL,NOPAR))

	IF( .NOT. ALLOCATED(Hessian)) ALLOCATE(Hessian(NOPAR,NOPAR))
	IF( .NOT. ALLOCATED(Gradient)) ALLOCATE(Gradient(NOPAR))
	IF( .NOT. ALLOCATED(RLABEL)) ALLOCATE(RLABEL(NOPAR))
	IF( .NOT. ALLOCATED(CLABEL)) ALLOCATE(CLABEL(NOPAR))
	IF( .NOT. ALLOCATED(VARCOV)) ALLOCATE(VARCOV(NOPAR,NOPAR))
	IF( .NOT. ALLOCATED(EigVals)) ALLOCATE(EigVals(NOPAR))
	IF( .NOT. ALLOCATED(EigVects)) ALLOCATE(EigVects(NOPAR,NOPAR))
	IF( .NOT. ALLOCATED(b)) ALLOCATE(b(NOPAR))

	XGUESS = Parameters  
	Write(DiagFile,*) ' XGUESS',XGUESS
  FSCALE = 1.0
	XSCALE = 1.0
	Call U4LSF(IPARAM,RPARAM)

  Call SOFSQ(NOPAR,Parameters,SumOfSquares(1))

  iersvr = 0
	ipact = 1
	isact = 0
  Call ERSET(iersvr,ipact,isact) !Stub — was IMSL error handling
  
  IF (OptMethod .EQ. 'UNLSF' ) Then
		!Levenberg-Marquardt without derivatives (MINPACK lmdif1 replacement)
		Call UNLSF(TERMS,NOBVAL,NOPAR,XGUESS,XSCALE,FSCALE,IPARAM,RPARAM,Parameters,Residuals,FJAC,NOBVAL)
		Write(DiagFile,*)'Parameters',Parameters
		Write(DiagFile,*)'Residuals ',Residuals
		Write(DiagFile,*)' Iterations used ',IPARAM(3)
		Write(DiagFile,*)' Function evaluations ',IPARAM(4)
		Write(DiagFile,*)' Jacobian evaluations ',IPARAM(5)
		Write(DiagFile,*)'Jacobian',((FJAC(I,J),J = 1,NOPAR),I = 1,NOBVAL)
	End If
	IF (OptMethod .EQ. 'UMCGF') Then
	  !Unconstrained conjugate gradient — not implemented, use UNLSF fallback
		GRADTL = 1.0e-8
		MAXFN = 100
		DFPRED = 0.1
		Write(DiagFile,*)'UMCGF not implemented, no optimisation performed'
		Write(DiagFile,*)'Parameters',Parameters
  End If
  IF (OptMethod .EQ. 'BCLSF') Then
    !Bounded Levenberg-Marquardt without derivatives (MINPACK lmdif1 replacement)
    IBTYPE = 0
	  Call BCLSF(TERMS,NOBVAL,NOPAR,XGUESS,IBTYPE,XLB,XUB,XSCALE,FSCALE,IPARAM,RPARAM,Parameters,Residuals,FJAC,NOBVAL)
		Write(DiagFile,*)'Parameters',Parameters
		Write(DiagFile,*)'Residuals ',Residuals
		Write(DiagFile,*)' Iterations used ',IPARAM(3)
		Write(DiagFile,*)' Function evaluations ',IPARAM(4)
		Write(DiagFile,*)' Jacobian evaluations ',IPARAM(5)
		Write(DiagFile,*)'Jacobian',((FJAC(I,J),J = 1,NOPAR),I = 1,NOBVAL)
  End If

  Write(DiagFile,*)' Making final call to SOFSQ'
  Call SOFSQ(NOPAR,Parameters,SS)
	SumOfSquares(2) = SS

! Write the model response to file ModsimParEst.out
  Write(DiagFile,*)' Writing model response for graphic display'
	Call PrintOutput(1,NOPAR,Parameters,JobPath,DataSetName)

  IF (ConfLevel .GT. 0.0) Then
	  !Evaluate the approximate Hessian matrix as sums of products of first order derivatives
	  Do N = 1,NOPAR
	    Do M = 1,NOPAR
		    Hessian(N,M) = 0.0
		    Do I = 1,NOBVAL
			    Hessian(N,M) = Hessian(N,M) + FJAC(I,N)*FJAC(I,M)
			  End do
		  End do
	  End do
	  RLABEL(1) = 'NUMBER'
	  CLABEL(1) = 'NUMBER'
    title ='Approximate Hessian matrix'
    fmt = '(G12.4)'
	  Call WRRRL(DiagFile,title,NOPAR,NOPAR,Hessian,NOPAR)

	  !Evaluate the approximate variance-covariance matrix as the inverse of the Hessian
	  Call LINRG(NOPAR,Hessian,NOPAR,VARCOV,NOPAR)
    title = 'Variance-covariance matrix'
	  Call WRRRL(DiagFile,title,NOPAR,NOPAR,VARCOV,NOPAR)

	  !Evaluate the eigenvalues and eigenvectors of the Hessiam matrix.
	  Call EVCRG(NOPAR,Hessian,NOPAR,EigVals,EigVects,NOPAR)
    title = 'Eigen Values'
	  Call WRCRN(DiagFile,title,1,NOPAR,EigVals,1)
    title = 'Eigen vectors'
	  Call WRCRN(DiagFile,title,NOPAR,NOPAR,EigVects,NOPAR)
	  Write(Diagfile,'(''Confidence ellipse shape factor = '',G12.4)')REAL(Eigvals(1))/REAL(Eigvals(NOPAR))

	  !Evaluate the confidence interval.
	  DegsOfFreedom = NOBVAL - NOPAR
	  VarianceEstimate = SS/DegsOfFreedom
	  Write(DiagFile,'(''Estimate of the variance = '',G12.4)')VarianceEstimate
	  RealNOPAR = NOPAR
	  Write(DiagFile,'(''Degrees of freedom'',2I5)')NOPAR,NOBVAL-NOPAR
	  Write(DiagFile,'(''F value '',F7.2)') FIN_REPL(0.01*ConfLevel,RealNOPAR,DegsOfFreedom)
	  ConfSS = SS*(1 + RealNOPAR*FIN_REPL(0.01*ConfLevel,RealNOPAR,DegsOfFreedom)/DegsOfFreedom)
	  Write(DiagFile,'(''Confidence region bounded by sum of squares contour '',G12.4)')ConfSS

	  !Get the intersections of the contour with the eigen vectors.
    !Start at the intersection with the quadractic SA contour and use Newton-Raphson
	  SA = SS
	  Do N = 1,NOPAR
	    LowerConfLim(N) = HUGE(Y)
		  UpperConfLim(N) = -HUGE(Y)
    End do
	  Do Direction = -1,1,2
      Do M = 1,NOPAR
		    iter = 0
			  WS = 0.0
			  Do N = 1,NOPAR
				  Do L = 1,NOPAR
				    WS = WS + REAL(EigVects(N,M))*Hessian(N,L)*REAL(EigVects(L,M))
				  End Do
			  End Do
			  alpha = Direction*SQRT((ConfSS - SS)/WS)
			  Do While (ABS(SA - ConfSS)/ConfSS .GT. 0.01 .AND. iter .LT. 10)
				  Do N = 1,NOPAR
					  b(N) = Parameters(N) + alpha*REAL(EigVects(N,M))
				  End Do
				  write(DiagFile,'('' Parameters for iteration ''I3,5G12.4)')iter,b
				  Call SOFSQ(NOPAR,b,SA)
 				  Call GRADSS(NOPAR,b,Gradient)
				  DSSDalpha = 0.0
				  Do N = 1,NOPAR
					  DSSDalpha = DSSDalpha + REAL(EigVects(N,M))*Gradient(N)
				  End do
				  Write(DiagFile,*)'alpha,SA,DSSDalpha',alpha,Sa,DSSDalpha
				  alpha = alpha + (ConfSS - SA)/DSSDalpha
				  iter = iter + 1
			  End Do
        Write(DiagFile,'(''Direction '',I3,''   Vector'',I3)')Direction,M
			  Write(DiagFile,'(''Sum of squares, Parameters'',(7G12.4))')SA,(b(N),N=1,NOPAR)
			  Do N = 1,NOPAR
				  If (UpperConfLim(N) .LT. b(N)) UpperConfLim(N) = b(N)  
				  If (LowerConfLim(N) .GT. b(N)) LowerConfLim(N) = b(N) 
			  End do 
			  SA = SS
      End Do
	  End Do
    write(DiagFile,*)'Upper conf limit',UpperConfLim
    write(DiagFile,*)'Lower conf limit',LowerConfLim
  End If
  Close(DiagFile)


  IF( ALLOCATED(XSCALE)) DEALLOCATE(XSCALE)
  IF( ALLOCATED(XGUESS)) DEALLOCATE(XGUESS)
  IF( ALLOCATED(FSCALE)) DEALLOCATE(FSCALE)
  IF( ALLOCATED(Residuals)) DEALLOCATE(Residuals)
  IF( ALLOCATED(FJAC)) DEALLOCATE(FJAC)

	IF(  ALLOCATED(Hessian)) DEALLOCATE(Hessian)
	IF(  ALLOCATED(Gradient)) DEALLOCATE(Gradient)
	IF(  ALLOCATED(RLABEL)) DEALLOCATE(RLABEL)
	IF(  ALLOCATED(CLABEL)) DEALLOCATE(CLABEL)
	IF(  ALLOCATED(VARCOV)) DEALLOCATE(VARCOV)
	IF(  ALLOCATED(EigVals)) DEALLOCATE(EigVals)
	IF(  ALLOCATED(EigVects)) DeALLOCATE(EigVects)
	IF(  ALLOCATED(b)) DEALLOCATE(b)

End Subroutine LevMarCurveFit


Subroutine TERMS(NOBVAL,NOPAR,Parameters,F)
!******************************************
  !Calculates each term in the sum of squares function.
	USE WorkingCommon
	Integer NOBVAL,NOPAR
	Real Parameters(NOPAR)
	Real F(NOBVAL)
	Real ModelResponse
  Integer I

  Write(113,*) ' In TERMS'
	SS = 0.0
  Do I  = 1,NOBVAL
		F(I) = Observations(I)%FractionPassing - ModelResponse(NOPAR,Parameters,I)
		SS = SS + F(I)*F(I)
		!WRITE(113,*)'I,F(I)',I,F(I)
  End do
	SS = 0.5*SS
	Write(113,*)' Parameters,Sum of squares ', Parameters,SS
End Subroutine TERMS


Subroutine SOFSQ(NOPAR,Parameters,SS)
!************************************
	USE WorkingCommon
	Integer NOPAR
	Real Parameters(NOPAR)
	Real ModelResponse
	Real SS

	SS = 0.0
  Write(113,*)'In SOFSQ'
  Do I  = 1,NumberOfObservations
		F = Observations(I)%FractionPassing - ModelResponse(NOPAR,Parameters,I)
		SS = SS + F*F
		!WRITE(113,*)'I,F',I,F
  End do
	SS = 0.5*SS
	Write(113,*)' Parameters, Sum of squares ',Parameters,SS
End Subroutine SOFSQ

Subroutine GRADSS(NOPAR,Parameters,G)
!************************************
	!Calculate the gradient of the sum of squares using a finite difference approximation
	Integer NOPAR
	Real Parameters(NOPAR)
	Real G(NOPAR)
	Real ModelResponse
	Real WS
  Real alpha
  REAL, ALLOCATABLE :: ParametersP(:),ParametersN(:)

  IF (.Not. ALLOCATED(ParametersP)) ALLOCATE(ParametersP(NOPAR))
  IF (.Not. ALLOCATED(ParametersN)) ALLOCATE(ParametersN(NOPAR))


  alpha = 0.001
	Do N = 1,NOPAR
    ParametersP = Parameters
    ParametersN = Parameters
    ParametersP(N) = Parameters(N)*(1+0.5*alpha)
    ParametersN(N) = Parameters(N)*(1-0.5*alpha)
    Call SOFSQ(NOPAR,ParametersP,SSP)
    Call SOFSQ(NOPAR,ParametersN,SSN)
		G(N) = (SSP - SSN)/(Parameters(N)*alpha)
	End do

  IF (ALLOCATED(ParametersP)) DEALLOCATE(ParametersP)
  IF (ALLOCATED(ParametersN)) DEALLOCATE(ParametersN)
END

Subroutine PrintOutput(Code,NOPAR,Parameters,JobPath,DataSetName)
!****************************************************************
!Write the calculated model response to plot 
	USE WorkingCommon

	Integer Code   !1=PSD in one stream
	Integer NOPAR
	Real Parameters(NOPAR)
	Character*255 JobPath
	Character*80 DataSetName
	Real ModelResponse

	LENG = LEN_TRIM(JobPath)
	OPEN(21, FILE = JobPath(1:LENG)//'LevMarCurveFit.out')
	Write(21,'(A80)')DataSetName
  I = 0
  Do J = 1,NoSizeDistributions
	  Write(21,'(''Stream'',I5,9X,''from MODSIM'')')StreamNumbers(J)
	  Write(21,'(''Size dist '',I4)')StreamNDC(J) - 1
	  Do K = 1,StreamNDC(J) - 1 
    I = I + 1
	    Write(21,'(2G10.3)')Observations(I)%MeshSize,Objectives(I)
	  End do
  End Do
	Write(21,'(''END OF FILE'')')
	Close(21)
END