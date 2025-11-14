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

subroutine Lynch_and_Rao
!*****************************
!This implements the Lynch and Rao model for the hydrcyclone.
!Imlpementation based on Jaime Sepulved course syllabus. The formula for water recovery is hopeless.  
!The water recovery implemented is a quick guess, based on the parameters that are available.
  USE ModelVariables
	USE GLOBALS

    Real A1, A2, A3
	Real alpha, Pressure
	Real Dvf, Du 
	Real d50c, Qf, Rf
	Real Bpw, WOF
	Real PF,xi


	!Write parameters into model variables
	A1 = parameters(1)
	A2 = parameters(2)
	A3 = parameters(3)
	alpha = parameters(4)
	Dvf = parameters(5)
	Du = parameters(6)
	
	!Calculate volume flowrate of slurry in feed
    Qf = 3600*VolumeFlow(Feed,FeedWater)
	!Calculate % solids, by weight, in feed
	PercentSolids = 100.0*TotalSolidsF/(TotalSolidsF + FeedWater)
	!Calculate pressure in feed (pressure in PSI)
	Pressure = (Qf * 0.0254/(A1 * Dvf * (100.0-PercentSolids)**0.125))**2
	PressurePascal = Pressure*6.89476
	PressureMeters = Pressure*0.70323
	!Overflow water  (This is my guess, as the formulae is BS) 
	WOF = A2*FeedWater*(Dvf**2)/((Dvf**2)+(Du**2))
	!WOF = A2 + 1.1*FeedWater - 10.0*Du/0.0254
	ConcentrateWater = WOF
    !Bypass estimate
	Bpw = 1. - (WOF/FeedWater)
	!Corrected d50, in microns
	d50c = A3*(Dvf/0.0254)**0.385 * Pressure**0.0935 / ((Du/0.0254)**0.286 * WOF**0.0192)
    
	TotalSolidsT = 0.0 
	Do I = 1,NumSizeClasses
	  xi = RepSize(I)*1e6/D50c
	  PF = (exp(alpha*xi) - 1.0)/(exp(alpha*xi) + exp(alpha) - 2)
	  PF = Bpw + (1.0 - Bpw)*PF
	  if (PF .LT. 0.0) Then
	      PF = 0.0
	  Else if(PF .GT. 1.0) Then
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
	TailingsWater = FeedWater - ConcentrateWater

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
		CALL ReportHeader(ioUnit,'Hydrocyclone','CLYN')
		SI = log((exp(alpha)+2.0)/3.0)/log(3.0*exp(alpha)-2.0)
		WRITE(ioUnit,1010) A1,A2,A3,alpha,SI
		1010 FORMAT(/&
		'Model parameters'/&
		'  Calibration factor for Pressure  A1',T45,F8.1/&
		'  Calibration factor for Overflow Water A2',T45,F8.1/&
		'  Calibration factor for d50c A3',T45,F8.1/&
		'  Efficiency factor alpha',T45,F8.2/&
		'     Sharpness Index',T45,F8.4)
		WRITE(ioUnit,1015)Dvf,Du
		1015 FORMAT(/&
		'  Diameter of vortex finder',T45,F8.3,' m'/&
		'  Diameter of underflow', T45,F8.3,' m')
		Write(ioUnit,1020)Qf,Pressure,PressurePascal,PressureMeters,100.0*Bpw,D50c
		1020 FORMAT(//&
		'Calculated quantities'/ &
		'  Volumetric feedrate',T40,F7.2,' m^3/h'/&
        '  Feed head',T40,F7.3,' PSI '/&
        '  Feed head',T40,F7.3,' kPa '/&
        '  Feed head',T40,F7.3,' m H2O '/&
		'  Recovery of water to underflow',T40,F15.3/&
		'  D50c',T40,F7.2,' microns')   
	End If

End subroutine
	
subroutine CIMM_Sepulveda
!*****************************
!This implements the Lynch and Rao model for the hydrcyclone.
!Imlpementation based on Jaime Sepulved course syllabus. The formula for water recovery is hopeless.  
!The water recovery implemented is a quick guess, based on the parameters that are available.
  USE ModelVariables
	USE GLOBALS

    Real A1, A2, A3, A4, A5
	Real m, Cv, Pressure, S
	Real Dvf, Du, Di, Lc, Dc 
	Real d50c, Qf, Rf, Rv, Rsc
	Real Bpw
	Real PF,xi, RhoS


	!Write parameters into model variables
	A1 = parameters(1) !corrects pressure
	A2 = parameters(2) !corrects d50
	A3 = parameters(3) !corrects pulp partition
	A4 = parameters(4) !Plitt parameter
	A5 = parameters(5) !Corrects bypass (lambda)
	Dc = parameters(6)/0.0254
	Di = parameters(7)/0.0254
	Dvf = parameters(8)/0.0254
	Du = parameters(9)/0.0254
	Lc = parameters(10)/0.0254
	
	!Calculate volume flowrate of slurry in feed
    Qf = 3600*VolumeFlow(Feed,FeedWater)
	!Calculate volume fraction of solids in feed
	Cv = VolumeFraction(Feed,FeedWater)
	!Calculate pressure in feed (pressure in m H2O)
	Pressure = A1 * Qf**1.46 * exp(-7.63 * Cv + 10.79*Cv*Cv)
	Pressure = Pressure/(Dc**0.20 * Lc**0.15 * Di**0.51 * Dvf**1.65 * Du**0.53)
	PressureMeters = Pressure*3.0
	!Pulp partition S
	S = A3 * Lc**0.19 * (Du/Dvf)**2.64 * exp(-4.33*Cv + 8.77*Cv*Cv)
	S = S/(Pressure**0.54 * Dc**0.38)
	!Volumetric recovery of pulp in underflow
	Rv = S/(S+1)
	!Plitt parameter
	m = exp(A4 - 1.58*Rv) * (Dc*Dc*Lc/Qf)**0.15
	!Corrected d50, in microns.  Take the first density for now.
	RhoS = SolidSpGr(1)
	d50c = A2 * Dc**0.54 * Di**0.58 * Dvf**1.91 * exp(11.12*Cv)
	d50c = d50c/(Du**0.80 * Lc**0.37 * Qf**0.44 * (RhoS - 1.0)**0.5)
    
	TotalSolidsT = 0.0 
	Do I = 1,NumSizeClasses
	  xi = RepSize(I)*1e6/D50c
	  PF = 1.0 - exp(-0.693 * xi**m)
	  if (PF .LT. 0.0) Then
	      PF = 0.0
	  Else if(PF .GT. 1.0) Then
	       PF = 1.0
      End if
		Do J = 1,NumGClasses
			Do K = 1,NumSClasses
				Tailing(I,J,K) = PF*Feed(I,J,K)
				TotalSolidsT = TotalSolidsT + Tailing(I,J,K)
			End Do
		End Do
	End Do
	!Recovery of solids in underflow if there was no bypass
	Rsc = TotalSolidsT/TotalSolidsF
	!Bypass
	Bpw = (Rv - Cv*Rsc)/(1.0 - Cv*(1.0 - A5 * (1.0 - Rsc)))
    !Bpw = (3.1416*Du*Du/4.0)/(Dvf*3.1416*Lc*0.5 + 3.1416*Du*Du/4.0)
	If(Bpw .LT. 0.0) Then 
	   Bpw = 0.0
	Else If (Bpw .GT. 1.0) Then
	   Bpw = 1.0
	End If

	TotalSolidsT = 0.0 
	Do I = 1,NumSizeClasses
	  xi = RepSize(I)*1e6/D50c
	  PF = 1.0 - exp(-0.693 * xi**m)
	  PF = Bpw + (1.0 - Bpw)*PF
	  if (PF .LT. 0.0) Then
	      PF = 0.0
	  Else if(PF .GT. 1.0) Then
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
	ConcentrateWater = (1.0 - Bpw)*FeedWater
	TailingsWater = FeedWater - ConcentrateWater

	!Create a report file at convergence.
	If(.NOT. reporting) then
		Return
	Else
	    !Back to meters
		Dc = Dc*0.0254
	    Di = Di*0.0254
	    Dvf = Dvf*0.0254
	    Du = Du*0.0254
	    Lc = Lc*0.0254
		CALL ReportHeader(ioUnit,'Hydrocyclone','CIMM')
		WRITE(ioUnit,1010) A1,A2,A3,A4,A5
		1010 FORMAT(/&
		'Model parameters'/&
		'  Calibration factor for Pressure  A1',T45,F8.1/&
		'  Calibration factor for d50c A2',T45,F8.1/&
		'  Calibration factor for pulp partition A3',T45,F8.1/&
		'  Calibration factor for Plitt parameter',T45,F8.2/&
		'  Calibration factor for Bypass',T45,F8.2)
		WRITE(ioUnit,1015)Dc,Di,Dvf,Du,Lc
		1015 FORMAT(/&
		'  Cyclone diameter',T45,F8.4,' m'/&
		'  Inlet diameter',T45,F8.4,' m'/&
		'  Vortex finder diameter',T45,F8.4,' m'/&
		'  Apex diameter', T45,F8.4,' m'/&
		'  Vortex-Spigot distance',T45,F8.4,' m')
		Write(ioUnit,1020)Qf,PressureMeters,RhoS,Cv,100.0*Bpw,Rsc,Rv,m,D50c
		1020 FORMAT(//&
		'Calculated quantities'/ &
		'  Volumetric feedrate',T45,F7.2,' m^3/h'/&
        '  Feed head',T45,F7.3,' m H2O '/&
        '  Solid specific gravity',T45,F7.3,/&
		'  Volume fraction of solids in feed',T45,F15.3/&
		'  Recovery of water to underflow',T45,F15.3,' %',/&
		'  No bypass recovery of solids to underflow',T45,F15.3/&
		'  Volumetric recovery of pulp to underflow',T45,F15.3/&
	    '  Plitt parameter m', T45,F15.6/&
		'  D50c',T45,F7.2,' microns')   
	End If

End subroutine