! =============================================================================
! msflib_stub.f90  –  gfortran compatibility stub for Intel MSFLIB module
!
! Intel Fortran's MSFLIB (Microsoft Fortran library) provides Win32 helper
! routines with "QQ"-suffix names.  Only the two functions actually called
! in this codebase are stubbed here:
!
!   COMMITQQ(nUnit)          – flush a Fortran unit to disk
!                              gfortran equivalent: FLUSH intrinsic
!
!   MESSAGEBOXQQ(msg,cap,t)  – display a Win32 message box
!                              stubbed as a console write + return value 1
! =============================================================================
MODULE MSFLIB
  IMPLICIT NONE

CONTAINS

  ! ---------------------------------------------------------------------------
  ! COMMITQQ  –  flush Fortran unit nUnit to disk.
  ! Returns .TRUE. on success (always, for this stub).
  ! ---------------------------------------------------------------------------
  LOGICAL(4) FUNCTION COMMITQQ(nUnit)
    INTEGER(4), INTENT(IN) :: nUnit
    FLUSH(nUnit)
    COMMITQQ = .TRUE.
  END FUNCTION COMMITQQ

  ! ---------------------------------------------------------------------------
  ! MESSAGEBOXQQ  –  display a message.
  ! The Intel version pops a Win32 dialog; here we write to stderr instead.
  ! Returns 1 (IDOK equivalent).
  ! ---------------------------------------------------------------------------
  INTEGER(4) FUNCTION MESSAGEBOXQQ(message, caption, buttontype)
    CHARACTER(*), INTENT(IN) :: message
    CHARACTER(*), INTENT(IN) :: caption
    INTEGER(4),   INTENT(IN) :: buttontype
    WRITE(0,'(A,": ",A)') TRIM(caption), TRIM(message)
    MESSAGEBOXQQ = 1
  END FUNCTION MESSAGEBOXQQ

END MODULE MSFLIB
