C************************** AM_SCHN.FOR *******************************C
C                                                                      C
C  Program to set up the transfer coefficients for the calculation of  C
C  liberation during comminution.                                      C
C  Claudio Schneider's model for the internal structure of             C
C  the Andrews-Mika diagram is used.                                   C
C  See file AM_LJUB.FOR for the Ljubljana model                        C
C                                                                      C
C  MODSIM (C) COPYRIGHT  RP KING  JOHANNESBURG 1985                    C
C**********************************************************************C
C
      INTERFACE TO INTEGER*4 FUNCTION A_M_GRAD[C,ALIAS:'_a_m'](NG,
     *CumGRDV,SIZEII,GRDV,SIZEI,n1m,PHIA,AMPAR,FGD,OKFLAG)
      INTEGER*4 NG
      REAL*8 SIZEII
      REAL*8 GRDV
      REAL*8 SIZEI
      REAL*8 n1m
      REAL*8 PHIA
      REAL*8 CumGRDV[REFERENCE](20)
      REAL*8 AMPAR[REFERENCE](0:7)
      REAL*8 FGD[REFERENCE](0:*)
      INTEGER*4 OKFLAG[REFERENCE]
      END

      PROGRAM AM_COEFF
C     ****************
C Sets up the liberation transfer coeffients usin Claudio schneider's
C model of the A-M diagram.
      REAL SPGR(2)
      REAL GRDM(20,2),GRDMV(20,2),SIZE(25)
      CHARACTER*4 TYPE

C Open the file produced by the system data form
      OPEN(31, FILE = 'AM_COEFF.DAT')
      READ(31,*) NDC,NGC,NSC
      READ(31,*) (SIZE(I),I = 1,NDC)
      READ(31,'(A4)') TYPE
      write(*,*)
     *' Calculating Andrews-Mika coefficients for Schneider''s model'
      DO I = 1,NGC
        READ(31,*) GRDMV(I,1),GRDMV(I,2)
      END DO
      READ(31,*) SPGR(1),SPGR(2)
      READ(31,*) PHIA
      CLOSE (31)
      CALL AM_SCHNEID(NDC,NGC,NSC,SIZE,TYPE,GRDMV,SPGR,PHIA)
      STOP
      END


      SUBROUTINE AM_SCHNEID(NDC,NGC,NSC,SIZE,TYPE,GRDMV,SPGR,PHIA)
C     ************************************************************
C Write a file containing the Andrews-Mika transfer coefficients
C for later use by the mill models.
      REAL SPGR(*)
      REAL GRDMV(20,2),SIZE(*)
      INTEGER*2 IBRF(20)
      REAL SPVOL(20)
      REAL*8 FGD(0:20)
      CHARACTER*4 TYPE

      OPEN(41, FILE = 'AMDIAG.DAT')
      DO J = 1,NGC
	IF(TYPE .EQ. 'GRDM') THEN
	  SPVOL(J) = GRDMV(J,1)/SPGR(1) + GRDMV(J,2)/SPGR(2)
	ELSE
	  SPVOL(J) = 1.0/(GRDMV(J,1)*SPGR(1) + GRDMV(J,2)*SPGR(2))
	END IF
      END DO
      DO J = 1,NGC
	IF(TYPE .EQ. 'GRDM') THEN
	  PAR_GRADE = GRDMV(J,2)/(SPGR(2)*SPVOL(J))
	ELSE
	  PAR_GRADE = GRDMV(J,2)
	END IF
        DO II = 2,NDC
          SZII = SIZE(II)
          CALL AM_CONDBR(NGC,SZII,SIZE(1),FGD,PAR_GRADE,PHIA)

C Convert to fraction by mass for MODSIM to use.
          TOTMASS = 0.0
          DO M = 1,NGC
            TOTMASS = (FGD(M) - FGD(M-1))/SPVOL(M) + TOTMASS
          END DO
          DO M = 1,NGC
            WS = (FGD(M) - FGD(M-1))/(SPVOL(M)*TOTMASS)
            IBRF(M) = NINT(WS*10000)
          END DO
          WRITE(41,'(15I6)')(IBRF(M), M = 1,NGC)
        END DO
      WRITE(41,*)
      END DO
      CLOSE(41)
      RETURN
      END

      SUBROUTINE AM_CONDBR(NGC,SZII,SZI,FGD,PAR_GRADE,PHIA)
C     *****************************************************
C Calculate the conditional breakage function using Schneider's
c model of the full Andrews-Mika diagram
      INTEGER*4 NG
      REAL*8 DSIZEII,DSIZEI
      REAL*8 FGD(0:*)
      REAL*8 GRDV
      REAL*8 CumGRDV(20)
      REAL*8 PHI
      REAL*8 n1m
      REAL*8 AMPAR(0:7)
      INTEGER*4 OKFLAG
      INTEGER*4 a_m_grad
      INTEGER*4 RetVal
C
C Set up the parameters
      PHI = PHIA
      AMPAR(0) = 4.651688
      AMPAR(1) = 28.005430
      AMPAR(2) = 0.771211
      AMPAR(3) = 0.000323
      AMPAR(4) = 0.951286

      OKFLAG = 1

      GRDV = PAR_GRADE
      n1m = GRDV
      DSIZEII = SZII*1D6
      DSIZEI = SZI*1D6
      NG = NGC - 1
      FGD(0) = 0.0

c NOTE October 16 1997
c A_M_GRAD.C now requires the boundary points of the grade classes
c by volume to facilitate calculations on unequal G-classes.
      DO J = 1,NGC-1
        CumGRDV(J) = 0.1*(J-1)
      END DO
      CumGRDV(NGC) = 1.0

      RetVal =  A_M_GRAD(NG,CumGRDV,DSIZEII,GRDV,DSIZEI,n1m,PHI,AMPAR,
     *FGD(1),OKFLAG)
      RETURN
      END
