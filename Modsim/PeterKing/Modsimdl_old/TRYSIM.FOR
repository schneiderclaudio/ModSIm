      PROGRAM TRYSIM
C     **************
C This can be used to try the simulation module outside of the VB shell.
c Compile with FL32 TRYSIM.FOR MODSIM.LIB
      CHARACTER(255) JobPath
      INTEGER*2 resp,CumOut

      INTERFACE
        FUNCTION INORDCALC(JobPath)
          !MS$ATTRIBUTES STDCALL, DLLIMPORT :: INORDCALC
          !MS$ATTRIBUTES ALIAS :'INORDCALC' :: INORDCALC
          INTEGER*2 INORDCALC
          CHARACTER*255 JobPath
          !MS$ATTRIBUTES REFERENCE :: JobPath
        END FUNCTION INORDCALC
      END INTERFACE
      INTERFACE
        FUNCTION SIMOP(JobPath,CumOut)
          !MS$ATTRIBUTES STDCALL, DLLIMPORT :: SIMOP
          !MS$ATTRIBUTES ALIAS :'SIMOP' :: SIMOP
          INTEGER*2 SIMOP
          CHARACTER*255 JobPath
          !MS$ATTRIBUTES REFERENCE :: JobPath
          INTEGER*2 CumOut
          !MS$ATTRIBUTES REFERENCE :: CumOut
        END FUNCTION SIMOP
      END INTERFACE

      write(*,*)' Starting TRYSIM'
      JobPath = 'c:\mysoft~1\modsim\modsimvb\'
      LENG = LEN_TRIM(JobPath)
      write(*,*)' JobPath is ',JobPath(1:LENG)


C Call the routine to run the simulation.
      resp = INORDCALC(JobPath)
      write(*,*)' resp from INORDCALC',resp
C Call the routine to set up the output data.
      CumOut = 0
      write(*,*)' CumOut',CumOut
      resp = SIMOP(JobPath,CumOut)
      write(*,*)' resp from SIMOP',resp
      END
