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

	Character*4 Model
	
	!Enter from MODSIM	
	Select case (Model)
	  Case ('BLBX')
		  Call BLBX

    Case ('NAGE')
		  Call NageswararaoCyclone

  !***** Add your Case and Call statements here. *****
     Case ('CLYN')
	      Call Lynch_and_Rao
	 Case ('CIMM')
	      Call CIMM_Sepulveda

  End Select

	!Close the intermediate report files and the intermediate diagnostic file.
  Close(9)
	Close(14)

  !Return to MODSIM
End Subroutine MODELSEQUENCE

