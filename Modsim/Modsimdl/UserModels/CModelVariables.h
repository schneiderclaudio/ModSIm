//This file is created by program DIMINP.FOR
//It must not be changed by the user.

#ifdef __cplusplus
extern "C" {
#endif

#pragma pack(2)
extern struct modsimvariables{
    int UnitNumber;
    int Reporting;
    int NumSizeClasses,NumGClasses,NumSClasses;
    int UnitType;
    float Feed[ 10][ 22][ 35];
    float Tailing[ 10][ 22][ 35];
    float Concentrate[ 10][ 22][ 35];
    float Middling[ 10][ 22][ 35];
    float TotalSolidsF;
    float TotalSolidsT, TotalSolidsC,TotalSolidsM;
    float FeedWater;
    float TailingsWater, ConcentrateWater, MiddlingsWater;
    float Parameters[150];
    float RepSize[ 35];
    int NumberOfMessages, NumberOfMinerals;
    float GradeM[  7][ 22], GradeV[  7][ 22];
    float SolidSpGr[ 22];
    float Texture[ 50];
    float MagnSusceptG[ 22];
    float OtherPropG[ 22];
    float FlotnRateConst[ 10];
    float MagnSusceptS[ 10];
    float OtherPropS[ 10];
    float CalValue[ 22];
    float TotalSulfur[ 22];
    float PyriticSulf[ 22];
    char JobName[80];
}MODELVARS;
#pragma pack()
 
#pragma pack(2)
extern struct VisualBasicVariables{
    long UnitExitValue;
    long UnitDiagFile;
    long UnitJobPath[255];
}VBVARIABLES;
#pragma pack()
 
char JobPathC[256];

#ifdef __cplusplus
}
#endif
