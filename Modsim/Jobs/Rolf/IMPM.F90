!  IMPM.f90 
!
!  FUNCTIONS:
!	IMPM      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: IMPM
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

      PROGRAM IMPMProgram

!	Variables
	  REAL FEED(25,2,1)
!      REAL OUT1(25,2,1),OUT2(25,2,1),OUT3(25,2,1)
!	  REAL DER1(25,2,1),DER2(25,2,1),DER3(25,2,1)
!	  REAL GRDM(12,2),GRDV(12,2)
	  REAL SIZE(25),PARAM(5),PPROP(1)
!	  REAL TMS1,TMS2,TMS3,WTR,WTR2,WTR3
	  INTEGER INDPP(1,2),FL
!	  INTEGER i,j,k
!	  INTEGER NDC,NGC,NSC,NGCM,NMIN,NPP

!	Body of IMPM

 	  TMSF=100.0
 	  NDC=25
 	  NGC=2
      NSC=1

	  PARAM(1)=20
	  PARAM(2)=0.1
      PARAM(3)=0.02
	  PARAM(4)=0.015
	  PARAM(5)=0.295
	  PARAM(6)=0.295
	  PARAM(7)=0.295
	  PARAM(8)=7.0

      	 
! feed dist. parameters
	  a=2.0
 	  b=0.8
	  topsize=20.0
	  DO k=1,NSC
	    do j=1,NGC
	     do i=1,NDC
	      SIZE(i)=(NSC-i)*(topsize/NSC)
	      FEED(i,j,k)=TMSF*(1-exp(-(SIZE(i)/a)**b))
	     end do
	    end do
	  end do
	
      CALL IMPM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
      
	  END PROGRAM IMPMProgram
!***********************************************************************
	  SUBROUTINE IMPM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED(NDC,NGC,NSC)
	  REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
	  REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
	  REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
	  REAL SIZE(NDC),PARAM(11),PPROP(1),CUMBF(NDC,NDC-1),DISBF(NDC,NDC-1),MSIZE(NDC-1),FEEDBF(NDC),FEEDDIS(NDC)
	  REAL TMS1,TMS2,TMS3,WTR,WTR2,WTR3,d63,n,RRx1,RRx2,RR1,RR1a,RR1b,RR2,RR2a,RR2b,sum,n1,n2,sum1
	  INTEGER INDPP(NPP,2),FL
	  COMMON NPLNT,NUNIT,ITER,IW,IFLAG
 	
	  speed=PARAM(1)
 	  width=PARAM(2) 
	  dia=PARAM(3)
	  gap=PARAM(4)
	  RRx1=PARAM(5)
	  RRx2=PARAM(6)
	  RR1a=PARAM(7)
	  RR1b=PARAM(8)
	  RR2a=PARAM(9)
	  RR2b=PARAM(10)
	  n1=PARAM(11)
	  n2=PARAM(12)
      

!  calculate total mass flow rate (kg/s)

	  TMS1=0.0	 
      do k=1,NSC
	    do j=1,NGC
	      do i=1,NDC
	         TMS1=TMS1+FEED(i,j,k)
	      end do
        end do
	  end do

!  Calc feed breakage function FEEDDIS is Not in %
      do i=2,NDC-1
        sum=0.0
		sum1=0.0
		do j=1,NGC
		  sum1=sum1+(FEED(1,j,k)/TMS1)
          sum=sum+(FEED(i,j,k)/TMS1)
		end do
		FEEDDIS(1)=sum1
!		FEEDBF(1)=100.0-sum1
        FEEDDIS(i)=sum
!		FEEDBF(i)=FEEDBF(i-1)-sum
	   end do

!  j is feed size classes and i are the distribution size classes

	  do j=1,NDC-1
	    MSIZE(j)=(SIZE(j)+SIZE(j+1))/2
		RR1=RR1a*(TMS1/width)**RR1b
		RR2=RR2a*(TMS1/width)+RR2b
		d63=MSIZE(j)/(RR1*(speed**RR2)*(RRx1*(MSIZE(j)**RRx2))+1)
		n=n1*(speed**n2)
	    CUMBF(1,j)=100*(1-exp(-(SIZE(1)/d63)**n))
		do i=2,NDC
	      CUMBF(i,j)=100*(1-exp(-(SIZE(i)/d63)**n))
	      DISBF(i,j)=CUMBF(i-1,j)-CUMBF(i,j)
	    end do
      end do

!   sum each size i across all the j's 

      do i=1,NDC
        do j=1,NDC-1
	      sum=0.0
		  sum=sum+FEEDDIS(j)*DISBF(i,j)
        end do
		OUT1(i,j,k)=TMS1*sum
      end do
	  
	  RETURN
	  END