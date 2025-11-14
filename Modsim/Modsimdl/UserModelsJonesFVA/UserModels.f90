! UserModels.f90 
! User-written models should be placed in this file.

subroutine BLBX
!**************
!This model simply passes the feed through to the tailings.
!The model has no parameters.
	USE ModelVariables
	USE GLOBALS

	Do I = 1,NumSizeClasses
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
				Tailing(I,J,K) = Feed(I,J,K)
        Concentrate(I,J,K) = 0.0
        Middling(I,J,K) = 0.0
			End Do
		End Do
	End Do
	TotalSolidsT = TotalSolidsF
  TotalSolidsC = 0.0
  TotalSolidsM = 0.0
	TailingsWater = FeedWater
  ConcentrateWater = 0.0
  MiddlingsWater = 0.0
	Message = 'In BLBX'
	Call DIAGNOSTIC(Message)
	Write(Message,'(''Unit number'',I3)')UnitNumber
	Call DIAGNOSTIC(Message)

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Black box','BLBX')
		WRITE(ioUnit,1001)
		1001 FORMAT(/' This model simply passes the feed through to the tailing'/&
		' The back box icon is available for your special models')
	End If
  
End subroutine BLBX

Subroutine NageswararaoCyclone
!*****************************
!This implements the Nagaswararao model for the hydrcyclone using the correlation for water recovery.
!Reference: Napier-Munn,T.J., Morrell,S., Morrison,R.D. and Kojovic,T. Mineral Comminution Circuits: Their Operation and Optimization. 
!Julius Kruttschnitt Mineral Research  Centre. 1996 p 316
	USE ModelVariables
	USE GLOBALS

  Real KD0, KD1, KQ1, KQ0, KW1, KV1
	Real alpha, lambda
	Real Dc, Di, Dvf, Du, Lc, theta
	Real d50c, Qf, Rf
	Real Cv, HoverDc
	Real PF,xi
	Real WS,WS1
			 Write(Message,'(3E10.4)')RepSize(1),RepSize(10),RepSize(15)
			 Call DIAGNOSTIC(Message)
	!Write parameters into model variables
	KD0 = parameters(1)
	KQ0 = parameters(2)
	KW1 = parameters(3)
	alpha = parameters(4)
	Dc = parameters(5)
	Di = parameters(6)/Dc
	Dvf = parameters(7)/Dc
	Du = parameters(8)/Dc
	Lc = parameters(9)/Dc
	theta = parameters(10)

	!Calculate the pressure at the inlet.
  KQ1 = KQ0 * Dc**(-0.1)
	WS = KQ1 *Dc**2* Dvf**0.68 * Di**0.45 * theta**(-0.1) * Lc**0.2
	Qf = 3600*VolumeFlow(Feed,FeedWater)
	HoverDc = (Qf/WS)**2

			write(Message,'(''Qf  '',E10.4)')Qf
			Call DIAGNOSTIC(Message)

	!Calculate d50c
	Cv = VolumeFraction(Feed,FeedWater)
	lambda = 10**(1.82*Cv)/(8.05*(1.0 - Cv)**2)
  KD1 = KD0*Dc**(-0.65)
	D50c = Dc*KD1 * Dvf**0.52 * Du**(-0.47) * lambda**0.93 *&
		   (HoverDc)**(-0.22) * Di**(-0.5) * Lc**0.2 * theta**0.15
			write(Message,'(''D50c  '',E10.4)')D50c
			Call Diagnostic(Message)
  
	!Calculate the recovery of water to underflow.
	Rf = 0.01*KW1 * Dvf**(-1.19) * Du**2.4 * HoverDc**(-0.53) * lambda**0.27 * Di**(-0.5)*&
		 theta**(-0.24) * Lc**0.22
	If(Rf .LT. 0.0 .OR. Rf .GT. 1.0) Then
		Message = 'Inadmissible value of Rf'
		Call DIAGNOSTIC(Message)
		Write(Message,'('' Value of Rf= '',G10.4)')Rf
		Call Diagnostic(Message)
		If(Rf .LT. 0.0) Then
			  Rf = 0.0
		Else
			  Rf = 0.9
		End If
	End if
	Write(Message,'('' Value of Rf= '',G10.4)')Rf
	Call DIAGNOSTIC(Message)

	WS1 = exp(alpha)
	TotalSolidsT = 0.0 
	Do I = 1,NumSizeClasses
		xi = RepSize(I)/D50c
		If(alpha*xi .LT. 20.0) then
			WS = exp(alpha*xi)
			PF = Rf + (1 - Rf)*(WS - 1)/(WS + WS1 - 2)
		Else
			PF = 1.0
		End if
				write(Message,'(''PF = '',E10.4)')PF
				Call Diagnostic(Message)
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
				Tailing(I,J,K) = PF*Feed(I,J,K)
				TotalSolidsT = TotalSolidsT + Tailing(I,J,K)
				Concentrate(I,J,K) = (1 - PF)*Feed(I,J,K)
			End Do
		End Do
	End Do
	TotalSolidsC = TotalSolidsF - TotalSolidsT
	TailingsWater = Rf*FeedWater
	ConcentrateWater = FeedWater - TailingsWater

	WS = Sum(Concentrate)
	WSF = Sum(Feed)
	write(Message,'(''Feed,SumFeed, Tailings, Conc, SumConc'',5E12.5)')TotalSolidsF,WSF,TotalSolidsT,TotalSolidsC,WS
	Call DIAGNOSTIC(Message)


	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Hydrocyclone','NAGE')
		WRITE(ioUnit,1010) KD0,KQ0,KW1,alpha
		1010 FORMAT(/&
		'Model parameters'/&
		'  Calibration factor for D50c  KD0',T50,E9.3/&
		'  Calibration factor for flowrate  KQ0',T50,F8.1/&
		'  Calibration factor for water recovery  KW1',T50,F8.1/&
		'  Efficiency factor for classification function ',T50,F8.2)
		WRITE(ioUnit,1015)Dc,Parameters(6),Parameters(7),Parameters(8),Parameters(9),theta
		1015 FORMAT(/&
		'  Diameter of cylindrical section ',T50,F8.3,' m'/&
		'  Diameter of inlet ',T50,F8.3,' m'/&
		'  Diameter of vortex finder',T50,F8.3,' m'/&
		'  Diameter of underflow', T50,F8.3,' m'/&
		'  Length of cylindrical section',T50,F8.3,' m'/&
		'  Cone angle: ',T50,F8.1) 
		Write(ioUnit,1020)Qf,HoverDc*Dc,Cv,KD1,KQ1,Rf, 1e6*D50c
		1020 FORMAT(//&
		'Calculated quantities'/ &
		'  Volumetric feedrate',T40,F7.2,' m^3/h'/&
    '  Feed head',T40,F7.3,' m of slurry'/&
		'  Volume fraction in feed',T40,F7.3/&
    '  Calibration factor for D50c, KD1',T40,E9.3/&
		'  Calibration factor for flowrate, KQ1',T40,F8.1/&
		'  Recovery of water to underflow',T40,F7.3/&
		'  D50c',T40,F7.2,' microns')   
		  
		!Calculate the volumetric recovery to underflow from NageSwararao formula
		KV1 = 4.65
		Rv = KV1 *Dvf**(-0.94) * Du**1.83 * HoverDc**(-0.31) * Di**(-0.25) * theta**(-0.24) * Lc**0.22  
		ActualRV= 100*VolumeFlow(Tailing,TailingsWater)/VolumeFlow(Feed,FeedWater)
		Write(ioUnit,1030)Rv,ActualRv
		1030 FORMAT(/&
		'Vol. recovery to underflow from Nageswararao regression formula ',F8.3,' %'/&
		'Vol. recovery to underflow calculated from Nageswararao partition model ',F8.3,' %')
	End If
End subroutine

Subroutine JonesFVA
!******************
!This implements the Jones model within the FVA project for CVRD, Itabira
	USE ModelVariables
	USE GLOBALS
	!Receive information about water feed to this unit.
    LOGICAL UNITWFEED
    COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
!(1) DH is the gradient of the magnetic field strength applied in the unit cell
!(2) AreaEff is the effective cross section are of the cell.
!(3) HEff is the height of the unit cell
!(4) FillTime is the time the unit cell is filled
!(5) Slurry feed viscosity
!(6) Residual water on retained magnetics before flushing

    Real DH, AreaEff, HEff, FillTime, SlurryViscosity, ResWaterConc, RECCell
	PARAMETER (PI = 3.1415927)
		
	!Write parameters into model variables
	DH = parameters(1)*100000
	AreaEff = parameters(2)
	HEff = parameters(3)
	FillTime = parameters(4)
	SlurryViscosity = parameters(5)
	ResWaterConc = parameters(6)
    !The magnetic force density ( N/ m3)
	fm = DH*DH*4.0*PI*1e-7
	!Calculate the water flows if water is added to the unit.

    If(UNITWFEED) Then
	   write(Message,'(''Flush water is added to this unit'')')
	   Call Diagnostic(Message)
       If(SOLIDCONT .LE. 0.0) Then
          FlushWater =  WATERADD/1000.0
       Else
          WS = TotalSolidsF*(100.0 - SOLIDCONT)/SOLIDCONT
          FlushWater = (WS - FeedWater)/1000.0
       End If
     End If
	 write(Message,'(''Flush water = '',F8.4, '' kg/s'')')FlushWater*1000
	 Call Diagnostic(Message)

!CALCULATE VOLUME FLOWRATES IN FEED
	  write(Message,'(''Flowrate of solids = '',F8.4, '' kg/s'')')TotalSolidsF
	  Call Diagnostic(Message)
	  write(Message,'(''Flowrate of water = '',F8.4, '' kg/s'')')FeedWater
	  Call Diagnostic(Message)
      RhoMin = SolidSpGr(1)
      VS = 0.0
      Do J=1,NumGClasses
        SM = 0.0
        Do I=1,NumSizeClasses
          Do K=1,NumSClasses
            SM = SM+FEED(I,J,K)
          EndDo
        EndDo
        If(SolidSpGr(J).LT.0.001) Then
          Write(Message, '( ''Particle type '',I2,'' does not have a proper density'')')J
          Call DIAGNOSTIC(Message)
          Return
        End If
        VS = VS+SM/SolidSpGr(J)
        IF(SolidSpGr(J) .LT. RhoMin) RhoMin = SolidSpGr(J)
      EndDo
      SG=TMSF/VS
      VS = VS/1000.0  !cubic meters per second
	  write(Message,'(''Volume flowrate of solids = '',F8.4, '' m3/s'')')VS
	  Call Diagnostic(Message)
      Q = FeedWater/1000.0 + VS
	  write(Message,'(''Volume flowrate = '',F8.4,'' m3/s'')')Q
	  Call Diagnostic(Message)
	  PHI = VS/Q
	  write(Message,'(''Volume fraction of solids = '',F8.4)')PHI
	  Call Diagnostic(Message)
      !SHARPNESS INDEX will be a function of volume % solids but
	  !we have a very conceptual model at this point of what this function 
	  !could look like
      SI = 0.4/(PHI+0.4)
	  write(Message,'(''Sharpness index = '',F8.4)')SI
	  Call Diagnostic(Message)
	  !THIS IS THE LOGISTIC EXPONENT FOR SEPARATION SHARPNESS
      CLAMBDA = -2.1972/ALOG(SI)

!The maximum amount of solids that the cell can hold is proportional to AreaEff*HEff
!and the solids going in the unit cell is VS*FillTime
    If(AreaEff*HEff .GT. VS*FillTime) Then 
	   RECCell = 1.0
	Else 
	   RECCell = AreaEff*HEff/(VS*FillTime)
	End If
    write(Message,'(''Fill volume = '',F8.4)')VS*FillTime
	Call Diagnostic(Message)
	write(Message,'(''Capacity volume = '',F8.4)')AreaEff*HEff
	Call Diagnostic(Message)
	write(Message,'(''RECCell = '',F6.4)')RECCell
	Call Diagnostic(Message)
!With total bypass defined we can proceed to calculate recovery for each particle type
!Assume the hydrodynamic force equal to the magnetic force defines 
!a 50% probability of recovery

    SlurrySpeed = Q/AreaEff   !meter/second
	GeometricFactor = 0.27 !N in King's book.  Value for cylindrical particles

	TotalSolidsT = 0.0 
	Do I = 1,NumSizeClasses
		xi = RepSize(I)
		F50 = xi*SlurryViscosity*SlurrySpeed
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
			    VM = 0.5*1000*SolidSpGr(J)*MagnSusceptS(K)*1e-9*xi*xi*xi*PI/(6.0*(1+1000*SolidSpGr(J)*MagnSusceptS(K)*1e-9*GeometricFactor))
				F = VM*fm  !Magnetic force experienced by particle
				X = F/F50
				If(X .LT. 0.0) X = 0.0
				Logistic = 1.0/(1.0+(X**(-CLAMBDA)))
				If(Logistic .LT. 0.0) Logistic = 0.0
				If(Logistic .GT. 1.0) Logistic = 1.0
				REC = (1-RECCell) + RecCell*Logistic
				write(Message,'(''REC = '',F10.4)')REC
				Call Diagnostic(Message)
				Tailing(I,J,K) = REC*Feed(I,J,K)
				TotalSolidsT = TotalSolidsT + Tailing(I,J,K)
				Concentrate(I,J,K) = (1-REC)*Feed(I,J,K)
				Middling(I,J,K) = 0.0
			End Do
		End Do
	End Do
	TotalSolidsC = TotalSolidsF - TotalSolidsT
	TotalSolidsM = 0
	TailingsWater = 0.01*parameters(6)*TotalSolidsC
	ConcentrateWater = FeedWater - TailingsWater 
	!at this point we have defined tailings.  Now calculate water addition

!Now we have the same exact model with water only flushing the particles
     WaterSpeed = FlushWater/AreaEff   !meter/second
	 WaterViscosity = 0.001    !Kg/ms
	 TotalSolidsM = 0.0 
	 Do I = 1,NumSizeClasses
		xi = RepSize(I)
		F50 = xi*WaterViscosity*WaterSpeed
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
			    VM = 0.5*1000*SolidSpGr(J)*MagnSusceptS(K)*1e-9*xi*xi*xi*PI/(6.0*(1+1000*SolidSpGr(J)*MagnSusceptS(K)*1e-9*GeometricFactor))
				F = VM*fm  !Magnetic force experienced by particle
				X = F/F50
				If(X .LT. 0.0) X = 0.0
				Logistic = 1.0/(1.0+(X**(-CLAMBDA)))
				If(Logistic .LT. 0.0) Logistic = 0.0
				If(Logistic .GT. 1.0) Logistic = 1.0
				Middling(I,J,K) = Logistic*Concentrate(I,J,K)
				TotalSolidsM = TotalSolidsM + Middling(I,J,K)
			End Do
		End Do
	End Do
	!need to calculate Concentrate
	TotalSolidsC = 0.0
	Do I = 1,NumSizeClasses
	Do J = 1,NumGClasses
	Do K = 1,NumSClasses
	   Concentrate(I,J,K) = Feed(I,J,K) - Tailing(I,J,K) - Middling(I,J,K)
	   TotalSolidsC = TotalSolidsC + Concentrate(I,J,K)
	End Do
	End Do
    End Do
	MiddlingsWater = 0.01*parameters(6)*TotalSolidsC + FlushWater*1000.0
	ConcentrateWater = FeedWater + FlushWater*1000.0 - TailingsWater - MiddlingsWater
	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Jones Magnetic Separator','JONS')
		WRITE(ioUnit,1010) DH, AreaEff, HEff, FillTime, SlurryViscosity, ResWaterConc
		1010 FORMAT(/&
		'Model parameters'/&
		'  Magnetic field gradient',T50,E9.3/&
		'  Effective cross-sectional area of matrix',T50,F8.1/&
		'  Effective height of separating unit cell',T50,F8.1/&
		'  Filling time of separating unit cell ',T50,F8.2/&
		'  Slurry Viscosity ',T50,F8.3/&
		'  Residual water on retained magnetics before flushing ',T50,F8.2)
		Write(ioUnit,1020)Q,PHI*100,SI,RecCell*100,SlurrySpeed,WaterSpeed
		1020 FORMAT(//&
		'Calculated quantities'/ &
		'  Volumetric feedrate of slurry',T40,F7.2,' m^3/h'/&
        '  Vulume fraction of solids',T40,F7.3,' %'/&
		'  Sharpness Index',T40,F7.5/&
        '  by-pass to Tailings stream ',T40,F7.2, ' %'/&
		'  Slurry speed ',T40,F8.3, ' m/s'/&
		'  Flush water speed',T40,F8.3, 'm/s')  
	End If
End subroutine