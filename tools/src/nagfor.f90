!> \file nagfor.f90  Provide some redirection/dummy routines for NAG Fortran (and g95?)


!> \brief  Dummy version of system() intrinsic function
function system(str1)
   implicit none
   character, intent(in) :: str1*(*)
   integer :: system
   character :: dummystr*99
   dummystr = trim(str1)
   system = 0
end function system

!> \brief  Dummy version of sleep() intrinsic function
!subroutine sleep(nr)
!   implicit none
!   integer, intent(in) :: nr
!   integer :: dummyint
!   dummyint = nr
!end subroutine sleep

