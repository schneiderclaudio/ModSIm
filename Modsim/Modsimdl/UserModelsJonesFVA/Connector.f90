!*************************************************************************
!                                                                        *
!            Connecting subroutine that is visible to MODSIM             *
!                                                                        *
! (c) MODSIM Copyright R P King 2002                                     *
!                                                                        *
!*************************************************************************

Subroutine MODELSEQUENCE(Model)
!******************************
  ! This SUBROUTINE is exported from UserModels.dll and is visible to MODSIM.DLL
  !MS$ ATTRIBUTES DLLEXPORT::MODELSEQUENCE

  USE GLOBALS

	Character*4 Model
  Character*5 cmodel
  Character*256 JobPath

  INTERFACE
    subroutine cmodelroutines(cmodel,JobPath)
      !DEC$ ATTRIBUTES C, ALIAS : '_cmodelroutines' :: cmodelroutines
      Character*5 cmodel
      !DEC$ ATTRIBUTES REFERENCE :: cmodel
      Character*256 JobPath
      !DEC$ ATTRIBUTES REFERENCE :: JobPath
    end subroutine
  END INTERFACE

	!Enter from MODSIM	
  Select case (Model)
	  Case ('BLBX')
	  Call BLBX

    Case ('NAGE')
	  Call NageswararaoCyclone

  !***** Add your Case and Call statements for Fortran model routines here.*****
     Case ('JONS')
	   Call JonesFVA

  
    CASE DEFAULT
    cmodel = Model//char(0)
    Len = LEN_TRIM(UnitJobPath)
    JobPath(1:Len) = UnitJobPath(1:Len)
    JobPath(Len+1:Len+1) = char(0)

    Call cmodelroutines(cmodel,JobPath)

  End Select

	!Close the intermediate report files and the intermediate diagnostic file.
  Close(9)
  Close(14)

  !Return to MODSIM
End Subroutine MODELSEQUENCE

