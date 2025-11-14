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
			End Do
		End Do
	End Do
	TotalSolidsT = TotalSolidsF
	TailingsWater = FeedWater
	Message = 'In BLBX'
	Call Diagnostic(Message)
	Write(Message,'(''Unit number'',I3)')UnitNumber
	Call Diagnostic(Message)

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Black box','BLBX')
		WRITE(ioUnit,1001)
		1001 FORMAT(/' This model simply passes the feed through to the tailing'/&
		' The black box icon is available for your special models')
	End If
  
End subroutine BLBX

subroutine BLBS
!**************
!This model simply passes the feed through to the tailings.
!The model has one parameter.
!It reports specific surface area for the material passing the unit.
  USE ModelVariables
  USE GLOBALS
  Real B, Sv, S

  B = parameters(1)

	Do I = 1,NumSizeClasses
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
				Tailing(I,J,K) = Feed(I,J,K)
			End Do
		End Do
	End Do
	TotalSolidsT = TotalSolidsF
	TailingsWater = FeedWater
	Message = 'In BLBS'
	Call Diagnostic(Message)
	Write(Message,'(''Unit number'',I3)')UnitNumber
	Call Diagnostic(Message)

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Black box S','BLBS')
		!Calculate Sv and S in centimeters
		Sv = 0.0
		S = 0.0
		Do I = 1,NumSizeClasses
		    DpI = RepSize(I)*100
			If(DpI .LT. 1e-4) DpI = 1e-4
			Do J = 1,NumGClasses
				Do K = 1,NumSClasses
					If(RepSize(I) .GT. 0.0) then 
					   Sv = Sv + Feed(I,J,K)*B/DpI
					   S = S + Feed(I,J,K)*B/(DpI*SolidSpGr(J))
					End if
				End Do
			End Do
		End Do
		S = S/TotalSolidsT
		Sv = Sv/TotalSolidsT
		WRITE(ioUnit,1005) B, Sv, S
		1005 FORMAT(/&
		'  Particle surface area to volume shape factor',T50,F5.2/&
		'  Average surface are per unit volume (cm-1)',T50,F7.1/&
		'  Average specific surface area (cm2/g)',T50,F7.1)
	End If
  
End subroutine BLBS

Subroutine NageswararaoCyclone
!*****************************
!This implements the Nagaswararao model for the hydrcyclone using the correlation for water recovery.
!Reference: Napier-Munn,T.J., Morrell,S., Morrison,R.D. and Kojovic,T. Mineral Comminution Circuits: Their Operation and Optimization. 
!Julius Kruttschnitt Mineral Research  Centre. 1996 p 316
  USE ModelVariables
	USE GLOBALS

  Real KD0, KQ0, KW1, KV1
	Real alpha, lambda
	Real Dc, Di, Dvf, Du, Lc, theta
	Real d50c, Qf, Rf
	Real Cv, HoverDc
	Real PF,xi
	Real WS,WS1

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
	WS = KQ0 * Dc**1.9 * Dvf**0.68 * Di**0.45 * theta**(-0.1) * Lc**0.2
	Qf = 3600*VolumeFlow(Feed,FeedWater)
	HoverDc = (Qf/WS)**2

	!Calculate d50c
	Cv = VolumeFraction(Feed,FeedWater)
  lambda = 10**(1.82*Cv)/(8.05*(1.0 - Cv)**2)
	D50c = KD0*Dc**0.35 * Dvf**0.52 * Du**(-0.47) * lambda**0.93 *&
	       (HoverDc)**(-0.22) * Di**(-0.5) * Lc**0.2 * theta**0.15
  
	!Calculate the recovery of water to underflow.
  Rf = 0.01*KW1 * Dvf**(-1.19) * Du**2.4 * HoverDc**(-0.53) * lambda**0.27 * Di**(-0.5)*&
	     theta**(-0.24) * Lc**0.22
  If(Rf .LT. 0.0 .OR. Rf .GT. 1.0) Then
		Message = 'Inadmissible value of Rf'
	  Call Diagnostic(Message)
	  Write(Message,'('' Value of Rf= '',G10.4)')Rf
		Call Diagnostic(Message)
		If(Rf .LT. 0.0) Then
		  Rf = 0.0
    Else
		  Rf = 0.9
    End If
  End if

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

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Hydrocyclone','NAGE')
		WRITE(ioUnit,1010) KD0,KQ0,KW1,alpha
		1010 FORMAT(/&
		'Model parameters'/&
		'  Calibration factor for D50c  KD0',T45,E9.3/&
		'  Calibration factor for flowrate  KQ0',T45,F8.1/&
		'  Calibration factor for water recovery  KW1',T45,F8.1/&
		'  Efficiency factor ',T45,F8.2)
		WRITE(ioUnit,1015)Dc,Parameters(6),Parameters(7),Parameters(8),Parameters(9),theta
		1015 FORMAT(/&
		'  Diameter of cylindrical section ',T45,F8.3,' m'/&
		'  Diameter of inlet ',T45,F8.3,' m'/&
		'  Diameter of vortex finder',T45,F8.3,' m'/&
		'  Diameter of underflow', T45,F8.3,' m'/&
		'  Length of cylindrical section',T45,F8.3,' m'/&
		'  Cone angle: ',T45,F8.1) 
		Write(ioUnit,1020)Qf,HoverDc*Dc,Cv,Rf, 1e6*D50c
		1020 FORMAT(//&
		'Calculated quantities'/ &
		'  Volumetric feedrate',T40,F7.2,' m^3/h'/&
        '  Feed head',T40,F7.3,' m of slurry'/&
		'  Volume fraction in feed',T40,F7.3/&
		'  Recovery of water to underflow',T40,F7.3/&
		'  D50c',T40,F7.2,' microns')   
		  
		!Calculate the volumetric recovery to underflow from NageSwararao formula
		KV1 = 4.65
		Rv = 0.01*KV1 *Dvf**(-0.94) * Du**1.83 * HoverDc**(-0.31) * Di**(-0.25) * theta**(-0.24) * Lc**0.22  
		ActualRV= VolumeFlow(Tailing,TailingsWater)/VolumeFlow(Feed,FeedWater)
		Write(ioUnit,1030)Rv,ActualRv
		1030 FORMAT(/&
		'Vol. recovery to underflow from Nageswararao regression formula ',F8.3/&
		'Vol. recovery to underflow from Nageswararao partition model ',F8.3)
	End If
End subroutine

Subroutine FishHookCyclone
!*****************************
!This implements the Austin Model for the Carajas project.  It is not meant 
!as a cyclone simulation model, but rather a black box that reproduces
!the exact (abnormal) partition function measured in the Carajas 
!Classifier cyclone
  USE ModelVariables
	USE GLOBALS

  Real FH_Sizes(14), FH_Pu(14)
  Integer NSizes, Model

  Model = parameters(1)

  NSizes = 12
!Define the sizes, in mm
  FH_Sizes(1) = 0.250
  FH_Sizes(2) = 0.177
  FH_Sizes(3) = 0.125
  FH_Sizes(4) = 0.088
  FH_Sizes(5) = 0.063
  FH_Sizes(6) = 0.044
  FH_Sizes(7) = 0.031
  FH_Sizes(8) = 0.022
  FH_Sizes(9) = 0.016
  FH_Sizes(10) = 0.011
  FH_Sizes(11) = 0.008
  FH_Sizes(12) = 0.006
  If(Model .EQ. 2) Then
  !The partition function for the desliming cyclone
      FH_Pu(1) = 1.00
      FH_Pu(2) = 1.00
      FH_Pu(3) = 1.00
      FH_Pu(4) = 1.00
      FH_Pu(5) = 1.00
      FH_Pu(6) = 1.00
      FH_Pu(7) = 1.00
      FH_Pu(8) = 1.00
      FH_Pu(9) = 1.00
      FH_Pu(10) = 1.00
      FH_Pu(11) = 1.00
      FH_Pu(12) = 0.35
  Else
    !Len's partition factors for these sizes
      FH_Pu(1) = 1.000
      FH_Pu(2) = 0.982
      FH_Pu(3) = 0.952
      FH_Pu(4) = 0.875
      FH_Pu(5) = 0.780
      FH_Pu(6) = 0.326
      FH_Pu(7) = 0.263
      FH_Pu(8) = 0.185
      FH_Pu(9) = 0.161
      FH_Pu(10) = 0.178
      FH_Pu(11) = 0.205
      FH_Pu(12) = 0.336
  EndIf
!Convert to meters
  Do L = 1,NSizes
      FH_Sizes(L) = FH_Sizes(L)/1000
  End Do
  TotalSolidsT = 0.0
  Do I = 1,NumSizeClasses
    C = 0.0
	DpI = RepSize(I)*1.189
    IF(DpI .GT. FH_Sizes(1)) THEN 
	   C = 1.0
	ELSE IF(DpI .LE. FH_Sizes(NSizes)) THEN 
	   C = FH_Pu(NSizes)
	ELSE 
	   Do L = 1,NSizes
		  IF(DpI .LE. FH_Sizes(L)) THEN
			 IF(DpI .GT. FH_Sizes(L+1)) THEN
			    C = (DpI - FH_Sizes(L+1))/(FH_Sizes(L)-FH_Sizes(L+1))
				C = FH_Pu(L+1) + C*(FH_Pu(L)-FH_Pu(L+1))
			  END IF
	      END IF	
	   END Do
	END IF
	IF(C .EQ. 0.0) Then 
	    Message = 'C value not found in Fish Hook model'
		Call Diagnostic(Message)
	END IF

  	Do J = 1,NumGClasses
	  Do K = 1,NumSClasses
	 	 Tailing(I,J,K) = C*Feed(I,J,K)
	     TotalSolidsT = TotalSolidsT + Tailing(I,J,K)
		 Concentrate(I,J,K) = (1 - C)*Feed(I,J,K)
	  End Do
	End Do
  End Do
  TotalSolidsC = TotalSolidsF - TotalSolidsT
  If(Model .EQ. 2) Then
	TailingsWater = 0.12945*FeedWater
  Else
	TailingsWater = 0.12916*FeedWater
  End If
  ConcentrateWater = FeedWater - TailingsWater 

  If(.NOT. reporting) then
	 Return
  Else
	 CALL ReportHeader(ioUnit,'Hydrocyclone','CFHK')

	 If(Model .EQ. 1) Then
	 	 Write(ioUnit,1001)
	 Else
	 	 Write(ioUnit,1002)
	 EndIf
1001 FORMAT(/'Partition factors calculated for Classifier Cyclone') 
1002 FORMAT(/'Partition factors calculated for Desliming Cyclone')
	 Write(ioUnit,1010)
	 1010 FORMAT(/'  Rep. Size   Partition to Underflow') 
	 Do I = 1,NumSizeClasses
       C = 0.0
	   DpI = RepSize(I)*1.189
       IF(DpI .GT. FH_Sizes(1)) THEN 
	      C = 1.0
	   ELSE IF(DpI .LE. FH_Sizes(NSizes)) THEN 
	      C = FH_Pu(NSizes)
	   ELSE 
	      Do L = 1,NSizes
		     IF(DpI .LE. FH_Sizes(L)) THEN
		  	    IF(DpI .GT. FH_Sizes(L+1)) THEN
			       C = (DpI - FH_Sizes(L+1))/(FH_Sizes(L)-FH_Sizes(L+1))
			 	   C = FH_Pu(L+1) + C*(FH_Pu(L)-FH_Pu(L+1))
			    END IF
	         END IF	
	      END Do
	   END IF
	   Write(ioUnit,1020)DpI*1000.0,C*100
	   1020 FORMAT(T2,F8.5,' mm',T20,F7.2,' %') 
	END Do
  END IF
	   

End subroutine

Subroutine LogisticClassifer
!*****************************
!This implements the Logistic Classifier Model for the Carajas project.  
!It is a logistic classifier model, which allows for setting the  
!water split to the underflow 
  USE ModelVariables
	USE GLOBALS


!   GENERAL EMPIRICAL MODELS FOR CLASSIFIERS.
!   See LG Austin RR KLIMPEL & PT Luckie PROCESS ENGINEERING OF SIZE
!   REDUCTION: BALL MILLING page 305
!
!     PARAMETERS IN ORDER (SI UNITS)
!     ************************************
!
!  1....By-pass fraction.
!  2....Sharpness index.
!  3....d50
!  4....Water SPlit to Underlow
!  5....Choice of model.
 Real WaterSplit, D50, SI, SCOS
 Real Lambda,Part,X,PF

	  WaterSplit = Parameters(4)
      D50=Parameters(3)
      SI=Parameters(2)
      SCOS=Parameters(1)

      TotalSolidsT = 0.0
	  Lambda=2.1972/ALOG(SI)
	  Do I = 1,NumSizeClasses
         X=RepSize(I)/D50
		 Part = 1/(1+X**Lambda)
		Write(Message,'('' Value of Part= '',G10.4)')Part
		Call Diagnostic(Message)
         PF=SCOS+(1.0-SCOS)*Part
	     Do J = 1,NumGClasses
	         Do K = 1,NumSClasses
              Tailing(I,J,K)=Feed(I,J,K)*PF
		  	  TotalSolidsT = TotalSolidsT + Tailing(I,J,K)
              Concentrate(I,J,K)=Feed(I,J,K)-Tailing(I,J,K)
             End Do
		End Do
	  End Do
   TotalSolidsC = TotalSolidsF - TotalSolidsT
   TailingsWater = WaterSplit*FeedWater
   ConcentrateWater = FeedWater - TailingsWater 

  If(.NOT. reporting) then
	 Return
  Else
	 CALL ReportHeader(ioUnit,'Hydrocyclone','CYCC')
	 	WRITE(ioUnit,1010) SCOS,SI,-Lambda,D50*1e6,WaterSplit
		1010 FORMAT(/&
		'Model parameters'/&
		'  Bypass factor ',T35,F8.5/&
		'  Sharpness Index',T35,F8.5, '  Lambda = ',F9.5/&
		'  Cut Size D50 (micron)',T35,F8.4/&
		'  Water Split to Underflow ',T35,F8.5)
  EndIf

End subroutine

Subroutine SaoLuisHPGR
!*********************
!This is a model for the Sao Luis HPGR.  It is a
!passthrough model.  The only quantities reported are 
!related to scale-up and increase in surface area
  USE ModelVariables
	USE GLOBALS

Real PMD, PML, PMRPM, PMu, PMGP
Real w, Volume, Rho, Phi
Real a, b, c, d, n
Real m, F, p, MP, E, Sc
Real k1, k2, kC

PMD = Parameters(1) !Production Mill Roll Diameter
PML = Parameters(2) !Production Mill Roll Length
PMRPM = Parameters(3) !Production Mill velocity of roll surface (m/s)
PMGP = Parameters(4) !Production Mill grinding pressure (MPascal)

    Volume = 0
	Do I = 1,NumSizeClasses
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
			    Volume = Volume + Feed(I,J,K)/SolidSpGr(J)
				Tailing(I,J,K) = Feed(I,J,K)
			End Do
		End Do
	End Do
	TotalSolidsT = TotalSolidsF
	TailingsWater = FeedWater

	Message = 'In SaoLuisHPGR'
	Call Diagnostic(Message)
	Write(Message,'(''Unit number'',I3)')UnitNumber
	Call Diagnostic(Message)


  If(.NOT. reporting) then
	 Return
  Else
    w = FeedWater*100/(TotalSolidsF+FeedWater)
    Rho = TotalSolidsF*1000/Volume
    Phi = PMD*PML*PMGP*1e3  !Operating force
    !Calculate speed of roll surface (m/s)
	PMu = 3.1416*PMD*PMRPM/60
    !Parameters from pilot scale test
	a = 1.54
	b = 0.47
	c = -1.9
	d = 0.034
	n = 1.32
	!calculate the specific capacity factor
	m = a*(w**c)*(PMGP**(-b)) !specific capacity factor
	!calculate mill capacity
	F = m*PMD*PML*PMu*Rho*3.6 !mill capacity in tons/hour
	!calculate the specific power factor
	p = a*d*w**(n+c)*1e-6*((PMGP)**(-b))*Rho*3600
	!calculate mill net power MP
	MP = p*PMD*PML*PMu*PMGP*1e3  !in kW
	!calculate specific grinding energy
	E = MP/F !in kWh/ton
	!calculate specific surface area
	!parameters from pilot scale test
	k1 = 280
	k2 = 81
	kC = 2120 
	Sc = k1*E+(kC-k2*w) !in cm2/g
	 CALL ReportHeader(ioUnit,'Sal Luis HPGR','SLGR')
	 	WRITE(ioUnit,1010) PMD,PML,PMRPM,PMu,PMGP
		1010 FORMAT(/&
		'Model parameters'/&
		'  Roll Diameter (meters) ',T35,F8.3/&
		'  Roll Length (meters) ',T35,F8.3/&
		'  Velocity of Roll Surface ',T35,F7.1,' RPM  ',F8.4,' m/s'/&
		'  Grinding Pressure (MPascal) ',T35,F8.2)
		WRITE(ioUnit,1020) w,Rho,Phi
		1020 FORMAT(/&
		'Operating variables'/&
		'  Water content (%) ',T35,F8.2/&
		'  Solids Density (kg/m3) ',T35,F8.2/&
		'  Operating Force (kN) ',T35,E9.3)
	 	WRITE(ioUnit,1030) m,F,p,MP,E,Sc
		1030 FORMAT(/&
		'Scale up calculations'/&
		'  Specific capacity factor ',T35,F8.5/&
		'  Mill capacity (t/h) ',T35,F8.2/&
		'  Specific power factor ',T35,F6.3/&
		'  Mill net power (kW) ',T35,F8.1/&
		'  Specific grinding energy (kWh/t) ',T35,F8.2/&
		'  Product specific surface area (cm2/g) ',T45,F8.2)
  EndIf

End subroutine