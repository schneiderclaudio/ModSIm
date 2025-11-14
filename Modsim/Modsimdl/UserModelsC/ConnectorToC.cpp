#include <stdio.h>
#include <string.h>
#include "CModelVariables.h"

extern struct modsimvariables MODELVARS;
extern struct VisualBasicVariables VBVARIABLES;

void cmodelroutines( char *cmodel, char *JobPath); 
void writeHeader(char *ModelType, char *model);

FILE *fptrRF;

void cbox();

void cmodelroutines( char *cmodel, char *JobPath) 
{
	strcpy(JobPathC,JobPath);

	if(strcmp(cmodel,"CBOX")==0) cbox();

	//Insert your model subroutine call here

}


void writeHeader(char *ModelType,char *model)
//Writes the model header in the report file.
//This subroutine must not be changed by the user.
{
	unsigned int i;
	char ReportFile[267];
	strcpy(ReportFile,JobPathC);
	strcat(ReportFile,"REPORT2.OUT");
	if((fptrRF = fopen(ReportFile,"a"))!=0)
	{
		fprintf(fptrRF,"\n \n UNIT EQUIPMENT DATA SHEET - %s \n",ModelType);
		fprintf(fptrRF," ****************************");
		for(i = 0 ; i<strlen(ModelType); i++)
		{
			fprintf(fptrRF,"*");
		}
        fprintf(fptrRF,"\n \n");
		fprintf(fptrRF,"     Unit number %3d           MODSIM model name %s\n",MODELVARS.UnitNumber,model);
		fprintf(fptrRF,"       Job name: %s\n",MODELVARS.JobName);
	}
}



	
void cbox()
{
	int i,j,k;
	float rec;

    rec = MODELVARS.Parameters[0];
	MODELVARS.TotalSolidsT = 0;
	MODELVARS.TotalSolidsC = 0;

	for(i=0; i<MODELVARS.NumSizeClasses; i++)
	{
		for(j=0; j<MODELVARS.NumGClasses; j++)
		{
			for(k=0; k<MODELVARS.NumSClasses; k++)
			{
				MODELVARS.Tailing[k][j][i]=(float) rec*MODELVARS.Feed[k][j][i];
				MODELVARS.Concentrate[k][j][i]=(float) (1-rec)*MODELVARS.Feed[k][j][i];
            	MODELVARS.TotalSolidsT += MODELVARS.Tailing[k][j][i];
            	MODELVARS.TotalSolidsC += MODELVARS.Concentrate[k][j][i];
			}
		}
	}
	MODELVARS.TailingsWater = (float) rec*MODELVARS.FeedWater;
	MODELVARS.ConcentrateWater = (float) (1-rec)*MODELVARS.FeedWater;

	if(MODELVARS.Reporting) 
	{
		writeHeader("Black box","CBOX");
		fprintf(fptrRF,"This is a simple splitting model.\nIt is written in C and can be used as a profile for user model routines\n");
		fprintf(fptrRF,"    Splitting factor to tailing = %7.3f\n",rec);
		fclose(fptrRF);
	}
}

