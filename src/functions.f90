!> \file functions.f90  Functions and subroutines for astroTools


!***********************************************************************************************************************************
!> \brief  Initialise an astroTools program

subroutine astroTools_init()
  use SUFR_constants, only: set_SUFR_constants
  use AT_version, only: print_astroTools_version
  implicit none
  
  ! Print version:
  write(*,'(/,A)', advance='no') '  '
  call print_astroTools_version(6)
  write(*,*) ' -  astrotools.sf.net'
  
  ! Initialise libSUFR constants:
  call set_SUFR_constants()
  
end subroutine astroTools_init
!***********************************************************************************************************************************


