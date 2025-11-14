#include<c:\progra~1\devstu~1\VC\include\math.h>
#include<c:\progra~1\devstu~1\VC\include\stdio.h>

#define PI 3.14159265358979323846
#define MAXKOUNT 40
#define EPS_DIC 1e-4

/* The Andrews-Mika function with folding */
/***********PARAMETERS**************************************************/
/* int *rung: Run grades from 0 to rung                                */
/* double gvclass[]; The grade classes to be used in the cumulative    */
/* double D: the progeny size                                          */
/* double gvP: parent particle volume fraction of phase B              */
/* double DP: parent particle size                                     */
/* double n1m: the first moment of the progeny grade distribution      */
/*             not necessarily equal to parent grade                   */
/* double Phi; geometrical texture parameter                           */
/* double ampar[]:                                                     */
/********  BOUND PARAMETERS ********************************************/
/*   0 -  DA: Model size at which liberated phase A starts to appear   */
/*   1 -  DB: Model size at which liberated phase B starts to appear   */
/*   2 -  zeta: One parameter sigma_csi function (varaince)            */
/*   3 - DeltaA parameter for left model bound (0.0 for dissertation)  */
/*   4 - DeltaB parameter for right model bound (0.0 for dissertation) */
/* double FvD[]: the conditional, on D, grade distribution             */
/* int *okflag: 1 if normal termination, 0 if abnormal                 */

double betai();
double betacf();
double gammln();
double inc_beta();
double inc_betaplus();
void case_2();
void case_3();
void case_4();


int a_m(rung,gvclass,D,gvP,DP,n1m,Phi,ampar,FvD,okflag) 
	 int rung, *okflag;
	 double D, gvP, DP, n1m;
	 double FvD[], ampar[], Phi;
	 double gvclass[];
	 {

	 int j;
	 double sigma_csi;
	 double gvA, gvB; /* the grades at the bounds */
	 double LA, LB;                      /* The fraction liberated at size D,      */
					      /* Fv(gv=0|D) = LA , 1-Fv(gv=1|D) = LB   */
	 double n1mcsi, n2mcsi, gamma_, alpha_, beta_; /* IBeta parameters */
	 double gv, gv_csi;
	 double CA, CB, zeta;  /* the parameters */
	 double DA, DB; /* the critical sizes */
	 double DAV, DBV; /* the A-M critical sizes */
	 double aux, auxvar;

	 void case_2(), case_3(), case_4();
	 double dsvar(), dsleft(), nspcleft(), nspcright(), dsright();
	 double tacvar(), tacright();
      
/* if progeny size is larger then parent, the distribution is null */
	 if(D > DP) {
	    for(j=0;j<=rung;++j) FvD[j] = 0.0;
	    fprintf(stdout,"Warning, trying to compute size larger then parent!\n");
	    return 1;
	 }
/* if parent grade is 0, then the distribution is the step function at 0 */
	 if(gvP == 0.0) {
	    for(j=0;j<=rung;++j) 
		FvD[j] = 1.0;
	    return 0;
	 }
/* if parent grade is 1, then the distribution is the step function at 1 */
	 if(gvP == 1.0) {
	    for(j=0;j<rung;++j) 
		FvD[j] = 0.0;
	    FvD[rung] = 1.0;
	    return 0;
	 }
/* if this size is the parent size, then the distribution is the step function at gvP */ 
	 if(D == DP) {
	    for(j=0;j<=rung;++j) {
		if(gvclass[j] < gvP) FvD[j] = 0.0; /* The cumulative is 0.0 
					     while grade is less then gvP */
		else FvD[j] = 1.0; /* and becomes 1.0 just as gv == gvP, or higher */
	    }
	    return 0;
	 } 

/* Now, for sizes smaller then parent size ... */
/* we know the volumetric grade distribution at the limit when size goes to 0 ...*/
	 if(D == 0.0) {
	    for(j=1;j<rung;++j) 
		FvD[j] = 0.0; 
            FvD[0] = 1.0-gvP;
            FvD[rung] = gvP;
	    return 0;
	 }

/* get the parameters from vector ampar[] */
	 CA = ampar[0];   /* critical phi A */
	 CB = ampar[1];   /* critical phi B */
	 zeta = ampar[2]; /* a parameter for the variance */

/* Calculate the variance auxiliary auxvar using the appropriate */
/* variance model */
/* For Dolomite-Spahlerite the variance model is: */ 
/* auxvar = dsvar(gvP,zeta); */
/* For Taconite the variance model is: */ 
   auxvar = tacvar(gvP,zeta);  

/* calculate the approximate A-M critical sizes */
	 DAV = DP*pow(1.0-gvP,1.0/3.0);
	 DBV = DP*pow(gvP,1.0/3.0);

/* calculate the critical sizes for the model bounds */
	 if(Phi == 0.0) {
	    fprintf(stdout,"Warning! Null textural parameter passed to\n");
	    fprintf(stdout,"liberation model. Assuming A-M limiting sizes\n");
	    DA = DAV;
	    DB = DBV;
	 }
	 else {
	    DA = (CA/Phi)*DAV; /* a fraction of the A-M critical size */
	    DB = (CB/Phi)*DBV;  /* a fraction of the A-M critical size */
	 }
/* adjust this sizes in case limitting bounds violate A-M bounds */
	 if(DA > DAV) {
	    DA = DAV;
	    fprintf(stdout,"Warning! Limiting bounds violate Andrews-Mika bounds\n");
	    fprintf(stdout,"for phase A.  Andrews-Mika bounds in effect ...\n");
	 }
	 if(DB > DBV) {
	    DB = DBV;
	    fprintf(stdout,"Warning! Limiting bounds violate Andrews-Mika bounds\n");
	    fprintf(stdout,"for phase B.  Andrews-Mika bounds in effect ...\n");
	 }

/* Find the grade at the left model bound corresponding to this size */
	/* gvA = dsleft(DA,DP,gvP,D,n1m);     for Dolomite-Sphalerite */
	   if(ampar[3] < 0.e0) ampar[3] = 0.e0;
	   else if(ampar[3] > 1.e0) ampar[3] = 1.e0;
	   gvA = nspcleft(DA,DP,gvP,D,n1m,ampar[3]);     /* for NSPC Taconite */

/* Find the grade at the right model bound corresponding to this size */
/*       gvB = dsright(DB,DP,gvP,D,n1m);      for Dolomite-Sphalerite */
/*       gvB = tacright(DB,DP,gvP,D,n1m);    for Taconite */
	 if(ampar[4] < 0.e0) ampar[4] = 0.e0;
	 else if(ampar[4] > 1.e0) ampar[4] = 1.e0;
         gvB = nspcright(DB,DP,gvP,D,n1m,ampar[4]);    /* for NSPC Taconite */

/* Compare with Andrews-Mika bounds and make the appropriate choice */
	 aux = 1.0 - pow(DP/D,3.0)*(1.0-gvP);
	 if(gvA < aux)  gvA = aux;
	 aux = pow(DP/D,3.0)*gvP;
	 if(gvB > aux ) gvB = aux;
/* check bounds */
	 if(gvB <= gvA) {
	    fprintf(stdout,
		    "Upper bounds at D = %lf are: \nleft = %lf right = %lf\n exiting ...\n",
		     D,gvA, gvB);
	    return 1;    
	 }
	 if(gvB < n1m) {
	    fprintf(stdout,
		    "Upper bounds at D = %lf are: \nleft = %lf right = %lf n1m = %lf\n exiting ...\n",
		     D,gvA, gvB, n1m);

	    *okflag = 0;    
	    return 1;
	 }
	 if(gvA > n1m) {
	    fprintf(stdout,
		    "Upper bounds at D = %lf are: \nleft = %lf right = %lf n1m = %lf\n exiting ...\n",
		     D,gvA, gvB, n1m);
	    *okflag = 0;    
	    return 1;
	 }

/* Find amounts liberated */
	if(gvA >= 0 && gvB <= 1.0 ) {
	   /* no liberation */
	   LA = 0.0;
	   LB = 0.0;
	   n1mcsi = (n1m-gvA)/(gvB-gvA);
	}
	else if(gvA < 0.0 && gvB <= 1.0) {
	  /* liberated phase A only */
	     LB = 0.0;
	     case_2(&LA,&n1mcsi,gvA,gvB,auxvar,n1m,okflag);
	     if(*okflag == 0) return 1;
	}
	else if(gvA >= 0.0 && gvB > 1.0) {
	  /* liberated phase B only */
	     LA = 0.0;
	     case_3(&LB,&n1mcsi,gvA,gvB,auxvar,n1m,okflag);
	     if(*okflag == 0) return 1;
	}
	else if(gvA <= 0 && gvB >= 1.0) {
	     /* liberated phases A and B */
	     case_4(&LA,&LB,&n1mcsi,gvA,gvB,auxvar,n1m,okflag);
	     if(*okflag == 0) return 1;
	}
	else {
	  fprintf(stdout,"Cannot find the appropriate case\n");
	  return 1;
	}
	/* in the case of very small sizes, calculated liberated ends may be
	   slightly higher then possible (beyond phase volume), due to floating point 
	   precision, so we check for that ... */
	if(LB >= n1m || LA >= (1.0-n1m)) {
	/* if one phase is totally liberated, then the other is also totally liberated */
	   LB = n1m;
	   LA = 1.0 - n1m;
	/* ... there are no unliberated particles */
	   FvD[rung] = 1.0;
	   for(j=0;j<rung;++j)
	       FvD[j] =  LA;
/*         fprintf(stdout,"Went for total liberation:\n");
	   fprintf(stdout,"gvP = %lf, DP = %lf, DAV = %lf, DBV = %lf\n",gvP,DP,DAV,DBV);
	   fprintf(stdout,"D = %lf, DA = %lf, DB = %lf\n",D,DA,DB); */
	   return 0;
	}
	/* if there are unliberated particles ... */

    /* calculate  sigma_csi ... */
	sigma_csi = auxvar*(n1mcsi-n1mcsi*n1mcsi);
    /* Now compute Ibeta parameters */
	n2mcsi = sigma_csi + n1mcsi*n1mcsi;
	gamma_ = (n1mcsi - n2mcsi)/sigma_csi;
	alpha_ = n1mcsi*gamma_;
	beta_ = (1.0-n1mcsi)*gamma_;
     
    /* Good point to report what is going on...
       fprintf(stdout," D = %lf,  gvA = %lf , gvB = %lf \n",
	       D, gvA, gvB);
       fprintf(stdout," LA = %lf , LB = %lf \n",
	       LA, LB);
       fprintf(stdout," n1mcsi = %lf,  n2mcsi = %lf , sigma_csi = %lf \n", 
	       n1mcsi, n2mcsi, sigma_csi);
       fprintf(stdout," alpha= %lf,  beta = %lf , gamma = %lf \n", 
	       alpha_, beta_, gamma_); */
    /* Ready. Calculate the unliberated cum. distribution */
	FvD[rung] = 1.0;
	FvD[rung-1] = 1.0-LB;
	FvD[0] = LA;
    /*  Run grades */
	for(j=1;j<(rung-1);++j) {
	    gv = gvclass[j];
	    if(gv <= gvA) FvD[j] = 0.0;
	    else if(gv >= gvB) FvD[j] = 1.0;
	    else if(gv > gvA && gv < gvB ) {
		    gv_csi = (gv - gvA)/(gvB - gvA);
		    FvD[j] = betai(alpha_,beta_,gv_csi,okflag);
		    if(*okflag == 0) {
		       fprintf(stdout," Major bummer in a_m() \n");
		       return 1;
		    }
		 }
	}
	return 0;

} /*  Done... */


double betai(a,b,x,okflag)
	     double a,b,x;
	     int *okflag;
{
	double bt, control;

	if (a < 0.0 || b < 0.0) {
	    fprintf(stdout,"Bad  a = %lf  or  b = %lf\n",a,b);
	    fprintf(stdout,"in function BETAI\n");
	    *okflag = 0;
	    return 0.0;
	}
	if (x < 0.0 || x > 1.0) {
	    fprintf(stdout,"Bad x = %lf in routine BETAI",x);
	    *okflag = 0;
	    return 0.0;
	}
	if (x == 0.0 || x == 1.0)
	    bt=0.0;
	else
	    bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.0-x));
	
	if (x < (a+1.0)/(a+b+1.0)) {
	    control = betacf(a,b,x,okflag);
	    if(*okflag == 0) {
	       /* use step function at a/(a+b) as approximation */
	       *okflag = 1;
	       if(x < a/(a+b) ) return 0.0;
	       else return 1.0;
	    }
	    if(control<0.0) {
	       fprintf(stdout,"betacf returned with negative values\n");
	       fprintf(stdout,"alpha = %lf  beta = %lf  x = %lf\n",a,b,x);
	    }
	    return bt*control/a;
	}
	else  {
	    control = betacf(b,a,1.0-x,okflag);
	    if(*okflag == 0) {
	       /* use step function at a/(a+b) as approximation */
	       *okflag = 1;
	       if(x < a/(a+b) ) return 0.0;
	       else return 1.0;
	    }
	    if(control<0.0) {
	       fprintf(stdout,"betacf returned with negative values\n");
	       fprintf(stdout,"alpha = %lf  beta = %lf  x = %lf\n",a,b,1.0-x);
	    }
	    return 1.0 - bt*control/b;
	}
}

double gammln(xx)
double xx;
{
	double x,tmp,ser;
	static double cof[6]={76.18009173,-86.50532033,24.01409822,
		-1.231739516,0.120858003e-2,-0.536382e-5};
	int j;

	x=xx-1.0;
	tmp=x+5.5;
	tmp -= (x+0.5)*log(tmp);
	ser=1.0;
	for (j=0;j<=5;j++) {
		x += 1.0;
		ser += cof[j]/x;
	}
	if(ser <= 0.0) 
	   fprintf(stdout,"xx = %lf, ser = %lf in gammaln\n",xx,ser);
	return -tmp+log(2.50662827465*ser);
}

#define ITMAX_BETACF 1000
#define EPS_BETACF 3.0e-7

double betacf(a,b,x,okflag)
double a,b,x;
int *okflag;
{
	double qap,qam,qab,em,tem,d;
	double bz,bm=1.0,bp,bpp;
	double az=1.0,am=1.0,ap,app,aold;
	int m;

	qab=a+b;
	qap=a+1.0;
	qam=a-1.0;
	bz=1.0-qab*x/qap;
	for (m=1;m<=ITMAX_BETACF;m++) {
		em=(double) m;
		tem=em+em;
		d=em*(b-em)*x/((qam+tem)*(a+tem));
		ap=az+d*am;
		bp=bz+d*bm;
		d = -(a+em)*(qab+em)*x/((qap+tem)*(a+tem));
		app=ap+d*az;
		bpp=bp+d*bz;
		aold=az;
		am=ap/bpp;
		bm=bp/bpp;
		az=app/bpp;
		bz=1.0;
		if (fabs(az-aold) < (EPS_BETACF*fabs(az))) return az;
	}
        fprintf(stdout,"a or b too big, or ITMAX_BETACF too small in BETACF\n");
	fprintf(stdout,"setting okflag to 0!\n");  
	*okflag = 0;
	return(0.0);
}

double inc_beta(x,first_moment,variance,okflag)
/* returns ibeta at x from variance and first moment */
       double first_moment, variance, x;
       int *okflag;
       {

       double alpha_, beta_, gamma_;
       double n1m, n2m, aux;

       double betai();

       n1m = first_moment;
       aux = n1m*n1m;
       if(variance > (n1m - aux)) {
	  *okflag = 0;
	  fprintf(stdout,"Variance out of range inc_beta() \n");
	  fflush(stdout);
	  return 0.0;
       }
       if(n1m < 0.0 || n1m > 1.0) {
	  *okflag = 0;
	  fprintf(stdout,"Wrong first moment in inc_beta() \n");
	  fflush(stdout);
	  return 0.0;
       }
       if(n1m == 0.0) {
	  if( x == 0.0 ) return 0.0;
	  else return 1.0;
       }
       if(n1m == 1.0) {
	  if( x < 1.0 ) return 0.0;
	  else return 1.0;
       }
       if(variance == 0.0) {
	  if(x < n1m) return 0.0;
	  else return 1.0;
       }

       n2m = variance + aux;
       gamma_ = (n1m - n2m)/(n2m-aux);
       alpha_ = n1m*gamma_;
       beta_ = (1.0 - n1m)*gamma_;

       return betai(alpha_,beta_,x,okflag);
}

double inc_betaplus(x,first_moment,variance,okflag)
/* returns ibeta(alpha_+1,beta_) at x from variance and first moment */
       double first_moment, variance, x;
       int *okflag;
       {

       double alpha_, beta_, gamma_;
       double n1m, n2m, aux;

       double betai();

       n1m = first_moment;
       aux = n1m*n1m;
       if(variance > (n1m - aux)) {
	  *okflag = 0;
	  fprintf(stdout,"Variance out of range inc_beta() \n");
	  fflush(stdout);
	  return 0.0;
       }
       if(n1m < 0.0 || n1m > 1.0) {
	  *okflag = 0;
	  fprintf(stdout,"Wrong first moment in inc_beta() \n");
	  fflush(stdout);
	  return 0.0;
       }

       n2m = variance + aux;
       gamma_ = (n1m - n2m)/(n2m-aux);
       alpha_ = n1m*gamma_ + 1.0;
       beta_ = (1.0 - n1m)*gamma_;
       
       return n1m*betai(alpha_,beta_,x,okflag);
}

double dsvar(gvP,zeta) 
/********************/
/* The variance model used for Dolomite-Sphalerite */
   double gvP, zeta;
   {
   double auxvar;
/* the variance parameter is modeled here with a simple model  */
/* decide on the variance auxiliary variable */
/* and to cut on unnecessary multiplications in the do loops ... */
   if(gvP > 0.5) auxvar = zeta*(1.0 - gvP);
   else auxvar = zeta*gvP; 
   return(auxvar);
}

double tacvar(gvP,zeta)
/*********************/
/* The variance model used for taconite */
   double gvP, zeta;
   {
   double gvL, gvR;
   double auxvar;
/* define volumetric grades between which variance is maximum */
/* this is for the Taconite Ore */
   gvL = 0.3;
   gvR = 0.8;
   if(gvP <= gvL) auxvar = zeta*0.5*gvP/gvL;
   else if(gvP >= gvR) auxvar = zeta*0.5*(1.0 - gvP)/(1.0 - gvR);
	else auxvar = 0.5*zeta;
   return(auxvar);
}
 
double dsleft(DA,DP,gvP,D,n1m)  
/****************************/
 /* The left model bound for Dolomite-Sphalerite */
      double DA, DP, gvP, D, n1m;
      {
      double pA, gvA;

/* The parameter that define the left hand side model bound */
	 pA = log(DA/DP)/log(1.0-gvP);
/* The grade at the left model bound corresponding to size D */
	 gvA = (1.0 - exp(log(DA/D)/pA))*n1m/gvP;
	 return(gvA);
}        

double nspcleft(DA,DP,gvP,D,n1m,deltaA) 
/**************************************/ 
 /* The left model bound for A-M diagram */ 
 double DA, DP, gvP, D, n1m, deltaA; 
 { 
 double pA, gvA, gvA_AM; 
 
 /* DA = Critical size for phase A */ 
 /* DP = Parent size */ 
 /* gvP = Parent volumetric grade */ 
 /* D = progeny size */ 
 /* n1m = first moment at this progeny size */ 
 /* deltaA = a boundary shape adjustment parameter */ 
 
 /* The parameter that define the left hand side model bound */ 
 pA = log(DA/DP)/log(1.e0-gvP); 
 /* The grade at the left model bound corresponding to size D */ 
 gvA = (1.e0 - exp(log(DA/D)/pA))*n1m/gvP;   
 /* this is the approximate A-M grade at this size */ 
 gvA_AM = 1.e0 - pow(DP/D,3.e0)*(1.e0-gvP); 
 if( gvA <= gvA_AM ) return gvA_AM; /* bound violation */

 /* Use the extra parameter to change the shape of the bound */ 
 if( gvA == 0.e0 ) return 0.e0; /* at the critical size there is no change */

 if( gvA_AM >= 0.e0 ) {
    /* we are above both critical sizes */
    gvA = (1.e0 - deltaA)*gvA + deltaA*gvA_AM;
 }
 else {
    /* we are bellow the A_M critical size */
    gvA = (1.0 - deltaA)*gvA;
 }
 
 return gvA; 
} 


double dsright(DB,DP,gvP,D,n1m)  
/****************************/
/* The right model bound for Dolomite-Sphalerite */
     double DB, DP, gvP, D, n1m;
     {
     double pB, gvB;

/* The parameter that define the right hand side model bound */
	 pB = log(DB/DP)/log(gvP);
/* Find the grade at the right model bound corresponding to this size */
	 gvB = exp(log(DB/D)/pB);
	 gvB = 1.0 - (1.0-gvB)*(1.0-n1m)/(1.0-gvP);
	 return(gvB);
}

double tacright(DB,DP,gvP,D,n1m)  
/******************************/
/* The right model bound for Taconite */
     double DB, DP, gvP, D, n1m;
     {
     double pB, gvB, gvL, gvR;

     gvL = 0.3;
     gvR = 0.8;
/* The parameter that define the right hand side model bound */
	 pB = log(DB/DP)/log(gvP);
/* Find the grade at the right model bound corresponding to this size */
	 gvB = exp(log(DB/D)/pB);
	 gvB = 1.0 - (1.0-gvB)*(1.0-n1m)/(1.0-gvP);
/* For particles in the range (gvL,gvR), the right bound is */
/* closer to the A-M bound */
	 if(gvP >= gvL && gvP <= gvR && gvB < 1.0) 
	    gvB = (gvB + 1.0)/2.0;
	 
	 return(gvB);
}

double nspcright(DB,DP,gvP,D,n1m,deltaB) 
/**************************************/ 
/* The right model bound for A-M diagram */ 
 double DB, DP, gvP, D, n1m, deltaB; 
 { 
 double pB, gvB, gvB_AM; 
 
 /* DB = Critical size for phase B */ 
 /* DP = Parent size */ 
 /* gvP = Parent volumetric grade */ 
 /* D = progeny size */ 
 /* n1m = first moment at this progeny size */ 
 /* deltaB = a boundary shape adjustment parameter */ 
 
 /* The parameter that define the right hand side model bound */ 
 pB = log(DB/DP)/log(gvP); 
 /* Find the grade at the right model bound corresponding to size D */ 
 gvB = exp(log(DB/D)/pB); 
 gvB = 1.e0 - (1.e0-gvB)*(1.e0-n1m)/(1.e0-gvP); 
 /* this is the approximate A-M grade at this size */ 
 gvB_AM = pow(DP/D,3.e0)*gvP;
 if( gvB >= gvB_AM ) return gvB_AM; /* bound violation */

 /* Use the extra parameter to change the shape of the bound */    
 if( gvB == 1.e0 ) return 1.e0; /* at the critical size there is no change */

 if( gvB_AM <= 1.e0 ) {
    /* we are above both critical sizes */
    gvB = (1.e0 - deltaB)*gvB + deltaB*gvB_AM;
 }
 else {
    /* we are bellow the A_M critical size */
    gvB = (1.0 - deltaB)*gvB + deltaB;
 }

 return gvB; 
} 


void case_2(LA,n1mcsi_,gvA,gvB,auxvar,n1m,okflag) 
/* liberated phase A only */
      double *LA, *n1mcsi_;
      double gvA, gvB;
      double auxvar, n1m;
      int *okflag;
      {

      int flag, kount;
      double gvcsi0;
      double n1mcsia, n1mcsib;
      double n1mcsi,sigma_csi;
      double n1m_a, n1m_b, n1m_;
      double mucsi;

      double inc_beta(), inc_betaplus();

	  flag = 0;
	  gvcsi0 =  -gvA/(gvB-gvA);
	  kount = 0;
	  n1mcsia = 1.0/3.0;
	  n1mcsib = 2.0/3.0;
	  /* find interval where root is */
	  do{
	     /* calculate LA at gvcsi0 for n1mcsia*/
	     sigma_csi = auxvar*(n1mcsia-n1mcsia*n1mcsia);
	     *LA = inc_beta(gvcsi0,n1mcsia,sigma_csi,okflag);
	     if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	    mucsi = n1mcsia - inc_betaplus(gvcsi0,n1mcsia,sigma_csi,okflag);
	    n1m_a = mucsi*(gvB - gvA) + gvA*(1.0 - *LA);
	     /* calculate LA at gvcsi0 for n1mcsib*/
	     sigma_csi = auxvar*(n1mcsib-n1mcsib*n1mcsib);
	     *LA = inc_beta(gvcsi0,n1mcsib,sigma_csi,okflag);
	     if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	    mucsi = n1mcsib - inc_betaplus(gvcsi0,n1mcsib,sigma_csi,okflag);
	    n1m_b = mucsi*(gvB - gvA) + gvA*(1.0 - *LA);
	     kount++;
	     if(kount > MAXKOUNT) {
		fprintf(stdout,"case 2:Unable to find interval containing root\n");
		*okflag = 0;
		return;
	     }
	     if((n1m_a-n1m)*(n1m_b-n1m) < 0.0) flag = 1;
	     else if(n1m_a<n1m && n1m_b<n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
		     else {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
	     }
	     else if(n1m_a>n1m && n1m_b>n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
		     else {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
	     }
	  } while(flag != 1); 
	  kount = 0;
	  flag = 0;
	  do{
	    n1mcsi = (n1mcsia+n1mcsib)/2.0;
	    *n1mcsi_ = n1mcsi;
	    /* calculate LA at gvcsi0 for n1mcsi*/
	    sigma_csi = auxvar*(n1mcsi-n1mcsi*n1mcsi);
	    *LA = inc_beta(gvcsi0,n1mcsi,sigma_csi,okflag);
	    if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	    mucsi = n1mcsi - inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
	    n1m_ = mucsi*(gvB - gvA) + gvA*(1.0 - *LA);
/*          fprintf(stdout,"iter = %d  n1m = %lf  n1m_ = %lf  n1mcsi = %lf\n",
		    kount, n1m, n1m_, n1mcsi);       
	    fprintf(stdout,"LA = %lf   mucsi = %lf\n",*LA,mucsi);       */
	    kount++;
	    if(kount > MAXKOUNT) {
	       fprintf(stdout,"Too many iterations in dicotomy \n");
	       /* this is a fix added to version 2 */
	       /* for when the first moment is just too close to 1.0 or 0.0 */
	       if(n1m > 0.99) { /* too close to 1.0 */
		   n1mcsi = n1m;
		   *LA = 1.0 - n1m;
		   *n1mcsi_ = n1mcsi;
		   mucsi = n1mcsi - inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
		   n1m_ = mucsi*(gvB - gvA) + gvA*(1.0 - *LA);
	       }
	       if(n1m < 0.01) { /* too close to 0.0 */
		   n1mcsi = n1m;
		   *LA = 1.0 - n1m;
		   *n1mcsi_ = n1mcsi;
		   mucsi = n1mcsi - inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
		   n1m_ = mucsi*(gvB - gvA) + gvA*(1.0 - *LA);
	       }
	       fprintf(stdout,"LA: n1mcsi = %lf, n1m = %lf, n1m_ = %lf, EPS = %lf\n",n1mcsi,n1m ,n1m_,fabs(n1m-n1m_));
	       flag = 1;
	    }

	    if(fabs(n1m-n1m_) < EPS_DIC) flag = 1;
	    if((n1m_-n1m)*(n1m_a-n1m)<0.0) {
		n1mcsib = n1mcsi;
		n1m_b = n1m_;
	    }
	    else {
		 n1mcsia = n1mcsi;
		 n1m_a = n1m_;
	    }

	  }while(flag != 1 );
}

void case_3(LB,n1mcsi_,gvA,gvB,auxvar,n1m,okflag) 
/* liberated phase B only */
      double *LB, *n1mcsi_;
      double gvA, gvB;
      double auxvar, n1m;
      int *okflag;
      {

      int flag, kount;
      double gvcsi1;
      double n1mcsia, n1mcsib;
      double n1mcsi, sigma_csi;
      double n1m_a, n1m_b, n1m_;
      double mucsi, mu;

      double inc_beta(), inc_betaplus();

	  /* liberated phase B only */
	  flag = 0;
	  gvcsi1 = (1.0-gvA)/(gvB - gvA);
	  kount = 0;
	  n1mcsia = 1.0/3.0;
	  n1mcsib = 2.0/3.0;
	  /* find interval where root is */
	  do{
	     /* calculate LB at gvcsi1 for n1mcsia*/
	     sigma_csi = auxvar*(n1mcsia-n1mcsia*n1mcsia);
	     *LB = 1.0 - inc_beta(gvcsi1,n1mcsia,sigma_csi,okflag);
	     if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	     mucsi = inc_betaplus(gvcsi1,n1mcsia,sigma_csi,okflag);
	     mu = mucsi*(gvB-gvA) + gvA*(1.0 - *LB);
	     n1m_a = mu + *LB;
	     /* calculate LB at gvcsi1 for n1mcsib*/
	     sigma_csi = auxvar*(n1mcsib-n1mcsib*n1mcsib);
	     *LB = 1.0 - inc_beta(gvcsi1,n1mcsib,sigma_csi,okflag);
	     if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	     mucsi = inc_betaplus(gvcsi1,n1mcsib,sigma_csi,okflag);
	     mu = mucsi*(gvB-gvA) + gvA*(1.0 - *LB);
	     n1m_b = mu + *LB;
	     kount++;
	     if(kount > MAXKOUNT) {
		fprintf(stdout,"case 3:Unable to find interval containing root\n");
		*okflag = 0;
		return;
	     }
	     if((n1m_a-n1m)*(n1m_b-n1m) < 0.0) flag = 1;
	     else if(n1m_a<n1m && n1m_b<n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
		     else {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
	     }
	     else if(n1m_a>n1m && n1m_b>n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
		     else {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
	     }
	  } while(flag != 1); 
	  kount = 0;
	  flag = 0;
	  do{
	    n1mcsi = (n1mcsia+n1mcsib)/2.0;
	    *n1mcsi_ = n1mcsi;
	    /* calculate LB at gvcsi1 for n1mcsi */
	    sigma_csi = auxvar*(n1mcsi-n1mcsi*n1mcsi);
	    *LB = 1.0 - inc_beta(gvcsi1,n1mcsi,sigma_csi,okflag);
	    if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	    mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
	    mu = mucsi*(gvB-gvA) + gvA*(1.0 - *LB);
	    n1m_ = mu + *LB;
/*          fprintf(stdout,"iter = %d  n1m = %lf  n1m_ = %lf  n1mcsi = %lf\n",
		    kount, n1m, n1m_, n1mcsi); */    
	    kount++;
	    if(kount > MAXKOUNT) {
	       fprintf(stdout,"Too many iterations in dicotomy \n");
	       /* this is a fix added to version 2 */
	       /* for when the first moment is just too close to 1.0 or 0.0 */
	       if(n1m > 0.99) { /* too close to 1.0 */
		   n1mcsi = n1m;
		   *LB = n1m;
		   *n1mcsi_ = n1mcsi;
		   mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
		   mu = mucsi*(gvB-gvA) + gvA*(1.0 - *LB);
		   n1m_ = mu + *LB;
	       }
	       if(n1m < 0.01) { /* too close to 0.0 */
		   n1mcsi = n1m;
		   *LB = n1m;
		   *n1mcsi_ = n1mcsi;
		   mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
		   mu = mucsi*(gvB-gvA) + gvA*(1.0 - *LB);
		   n1m_ = mu + *LB;
	       }
	       fprintf(stdout,"LB: n1mcsi = %lf, n1m = %lf, n1m_ = %lf, EPS = %lf\n",n1mcsi,n1m ,n1m_,fabs(n1m-n1m_));
	       flag = 1;
	    }

	    if(fabs(n1m-n1m_) < EPS_DIC) flag = 1;
	    if((n1m_-n1m)*(n1m_a-n1m)<0.0) {
		n1mcsib = n1mcsi;
		n1m_b = n1m_;
	    }
	    else {
		 n1mcsia = n1mcsi;
		 n1m_a = n1m_;
	    }

	  }while(flag != 1 );
}

void case_4(LA,LB,n1mcsi_,gvA,gvB,auxvar,n1m,okflag) 
/* liberated phases A and B */
      double *LA, *LB, *n1mcsi_;
      double gvA, gvB;
      double auxvar, n1m;
      int *okflag;
      {

      int flag, kount;
      double gvcsi0, gvcsi1;
      double n1mcsia, n1mcsib;
      double n1mcsi, sigma_csi;
      double n1m_a, n1m_b, n1m_;
      double mucsi, mu;

      double inc_beta(), inc_betaplus();
	  flag = 0;
	  kount = 0;
	  gvcsi0 =  -gvA/(gvB-gvA);
	  gvcsi1 = (1.0-gvA)/(gvB - gvA);
      /*  fprintf(stdout,"gvcsi0 = %lf  gvcsi1 = %lf\n",gvcsi0,gvcsi1); */
      /* This is the bracketing procedure */
	  n1mcsia = 1.0/3.0;
	  n1mcsib = 2.0/3.0;
	  do{
	     /* calculate parameters of beta function for n1mcsia*/
	     sigma_csi = auxvar*(n1mcsia-n1mcsia*n1mcsia);
	     *LA = inc_beta(gvcsi0,n1mcsia,sigma_csi,okflag);
	     if(*okflag == 0) return;
	     *LB = 1.0 - inc_beta(gvcsi1,n1mcsia,sigma_csi,okflag);
	     if(*okflag == 0) return;
	     /* calculate the first moment of the liberated distribution */
	     mucsi = inc_betaplus(gvcsi1,n1mcsia,sigma_csi,okflag);
	     mucsi -= inc_betaplus(gvcsi0,n1mcsia,sigma_csi,okflag);
	     mu = mucsi*(gvB - gvA) + gvA*(1.0 - *LA - *LB);   
	     n1m_a = mu + *LB;
	     /* calculate parameters of beta function for n1mcsib*/
	     sigma_csi = auxvar*(n1mcsib-n1mcsib*n1mcsib);
	     *LA = inc_beta(gvcsi0,n1mcsib,sigma_csi,okflag);
	     if(*okflag == 0) return;
	     *LB = 1.0 - inc_beta(gvcsi1,n1mcsib,sigma_csi,okflag);
	     if(*okflag == 0) return;
	     /* calculate the first moment of the liberated distribution */
	     mucsi = inc_betaplus(gvcsi1,n1mcsib,sigma_csi,okflag);
	     mucsi -= inc_betaplus(gvcsi0,n1mcsib,sigma_csi,okflag);
	     mu = mucsi*(gvB - gvA) + gvA*(1.0 - *LA - *LB);   
	     n1m_b = mu + *LB;
	     kount++;
	     if(kount > MAXKOUNT) {
/*              fprintf(stdout,"case 4:Unable to find interval containing root\n");
		fprintf(stdout,"case 4:n1m = %lf, gvA = %lf gvB = %lf\n", n1m, gvA, gvB);
		fprintf(stdout,"case 4:going for total liberation\n"); */
		*LA = 1.0 - n1m;
		*LB = n1m;
		return;
	     }
	     if((n1m_a-n1m)*(n1m_b-n1m) < 0.0) flag = 1;
	     else if(n1m_a<n1m && n1m_b<n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
		     else {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
	     }
	     else if(n1m_a>n1m && n1m_b>n1m) {
		     if(n1m_a>n1m_b) {
			n1mcsia = n1mcsib;
			n1mcsib = (1.0-n1mcsib)/2.0 + n1mcsib;
		     }
		     else {
			n1mcsib = n1mcsia;
			n1mcsia = n1mcsia/2.0;
		     }
	     }
	  } while(flag != 1); 
/*  Find a root between n1mcsia and n1mcsib */
	  kount = 0;
	  flag = 0;
	  do{
	    n1mcsi = (n1mcsia+n1mcsib)/2.0;
	    *n1mcsi_ = n1mcsi;
	    /* calculate parameters of beta function for n1mcsi*/
	    sigma_csi = auxvar*(n1mcsi-n1mcsi*n1mcsi);
	    *LA = inc_beta(gvcsi0,n1mcsi,sigma_csi,okflag);
	    if(*okflag == 0) return;
	    *LB = 1.0 - inc_beta(gvcsi1,n1mcsi,sigma_csi,okflag);
	    if(*okflag == 0) return;
	    /* calculate the first moment of the liberated distribution */
	    mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
	    mucsi -= inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
	    mu = mucsi*(gvB - gvA) + gvA*(1.0 - *LA - *LB);   
	    n1m_ = mu + *LB;
/*          fprintf(stdout,"iter = %d  n1m = %lf  n1m_ = %lf  n1mcsi = %lf\n",
		    kount, n1m, n1m_, n1mcsi);       
	    fprintf(stdout,"LA = %lf  LB = %lf  mu = %lf  mucsi = %lf\n",
		    *LA,*LB,mu,mucsi);       */
	    kount++;
	    if(kount > MAXKOUNT) {
/*             fprintf(stdout,"Too many iterations in dicotomy \n"); */
	       /* this is a fix added to version 2 */
	       /* for when the first moment is just too close to 1.0 or 0.0 */
	       if(n1m > 0.99) { /* too close to 1.0 */
		   n1mcsi = n1m;
		   *LA = 1.0 - n1m;
		   *LB = n1m;
		   *n1mcsi_ = n1mcsi;
/*                 mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
		   mucsi -= inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
		   mu = mucsi*(gvB - gvA) + gvA*(1.0 - *LA - *LB);
		   n1m_ = mu + *LB; */
	       }
	       if(n1m < 0.01) { /* too close to 0.0 */
		   n1mcsi = n1m;
		   *LA = 1.0 - n1m;
		   *LB = n1m;
		   *n1mcsi_ = n1mcsi;
/*          mucsi = inc_betaplus(gvcsi1,n1mcsi,sigma_csi,okflag);
		   mucsi -= inc_betaplus(gvcsi0,n1mcsi,sigma_csi,okflag);
		   mu = mucsi*(gvB - gvA) + gvA*(1.0 - *LA - *LB);
		   n1m_ = mu + *LB;  */
	       }
/*      fprintf(stdout,"n1mcsi = %lf, n1m = %lf, n1m_ = %lf, EPS = %lf\n",
		       n1mcsi,n1m ,n1m_,fabs(n1m-n1m_)); */
	       flag = 1; 
	    }

	    if(fabs(n1m-n1m_) < EPS_DIC) flag = 1;
	    if((n1m_-n1m)*(n1m_a-n1m)<0.0) {
		n1mcsib = n1mcsi;
		n1m_b = n1m_;
	    }
	    else {
		 n1mcsia = n1mcsi;
		 n1m_a = n1m_;
	    }

	  }while(flag != 1 );
}
