      PROGRAM TRYAMD
C     **************
C This can be used to try the simulation A-M Diagram module outside of the VB shell.
c Compile and link: fl32 tryamd.for MODSIM.LIB
      CHARACTER(255) JobPath,JobName
      INTEGER*4 resp
      REAL*4 Parameters(4)
      INTERFACE
        FUNCTION LJUBAMD(JobPath, JobName)
          !MS$ATTRIBUTES STDCALL, DLLIMPORT :: LJUBAMD
          !MS$ATTRIBUTES ALIAS :'LJUBAMD' :: LJUBAMD
          INTEGER*4 LJUBAMD
          CHARACTER*255 JobPath,JobName
          !MS$ATTRIBUTES REFERENCE :: JobPath
          !MS$ATTRIBUTES REFERENCE :: JobName
        END FUNCTION LJUBAMD
      END INTERFACE
      INTERFACE
        FUNCTION BETAAMD(JobPath, JobName,Parameters)
          !MS$ATTRIBUTES STDCALL, DLLIMPORT :: BETAAMD
          !MS$ATTRIBUTES ALIAS :'BETAAMD' :: BETAAMD
          INTEGER*4 BETAAMD
          CHARACTER*255 JobPath,JobName
          !MS$ATTRIBUTES REFERENCE :: JobPath
          !MS$ATTRIBUTES REFERENCE :: JobName
          REAL*4 Parameters(4)
          !MS$ATTRIBUTES REFERENCE :: Parameters
        END FUNCTION BETAAMD
      END INTERFACE

      write(*,*)' Starting TRYAMD'
      JobPath = 'c:\mysoft~1\MODSIM\modsimvb\'
c      JobPath = 'D:\PROGRA~1\MICROS~1\PROJECTS\MODSIM\'
      write(*,*) JobPath
      LENG = LEN_TRIM(JobPath)
      JobName = 'Fairlane'
      write(*,*) Jobname


      ! Parameters for Austin's breakage function.
      BETA = 3.723
      GAMMA = 0.748
      PHI1 = 0.72
      DELTA = 0.0
      Parameters(1) = BETA
      Parameters(2) = GAMMA
      Parameters(3) = DELTA
      Parameters(4) = PHI1
      resp = LJUBAMD(JobPath,JobName)
      WRITE(*,*)' Return value from LJUBAMD ',resp
      resp = BETAAMD(JobPath,JobName,Parameters)
      WRITE(*,*)' Return value from BETAAMD ',resp
      end
