! =============================================================================
! portlib_stub.f90  –  gfortran compatibility stub for Intel PORTLIB module
!
! Intel Fortran's PORTLIB module provides system/portability services.
! None of its symbols are explicitly called in this codebase (the USE
! statement appears without an ONLY clause), so an empty module is enough
! to satisfy the compiler.
! =============================================================================
MODULE PORTLIB
  IMPLICIT NONE
END MODULE PORTLIB
