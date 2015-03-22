!> \file rect2spher  Convert rectangular coordinates to spherical coordinates

!***********************************************************************************************************************************
!> \brief  Convert rectangular coordinates to spherical coordinates

program rect2spher
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_constants, only: set_SUFR_constants, r2d
  use SUFR_angles, only: rev,rev2
  
  implicit none
  real(double) :: x,y,z, l,b,r
  character :: str*(99)
  
  call set_SUFR_constants()
  
  if(command_argument_count().ne.3) &
       call syntax_quit('syntax: rect2spher <x> <y> <z>', 0, &
       'Convert ecliptical coordinates from a rectangular to a spherical frame')
  
  call get_command_argument(1,str)
  read(str,*) x
  call get_command_argument(2,str)
  read(str,*) y
  call get_command_argument(3,str)
  read(str,*) z
  
  call rect_2_spher(x,y,z, l,b,r)
  
  write(*,'(/,2x,A,3(F20.10,A))') 'x,y,z: ', x, ' AU', y, ' AU', z, ' AU'
  write(*,'(2x,A,3(F20.10,A),/)') 'l,b,r: ', rev(l)*r2d, ' ° ', rev2(b)*r2d, ' ° ', r, ' AU'
  
end program rect2spher
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief  Convert rectangular coordinates x,y,z to spherical coordinates l,b,r
!!
!! \param  x  Rectangular x coordinate (same unit as r)
!! \param  y  Rectangular y coordinate (same unit as r)
!! \param  z  Rectangular z coordinate (same unit as r)
!!
!! \retval l  Longitude (rad)
!! \retval b  Latitude  (rad)
!! \retval r  Distance  (same unit as x,y,z)

subroutine rect_2_spher(x,y,z, l,b,r) 
  use SUFR_kinds, only: double
  use SUFR_angles, only: rev
  use SUFR_numerics, only: deq0
  
  implicit none
  real(double), intent(in) :: x,y,z
  real(double), intent(out) :: l,b,r
  real(double) :: x2,y2
  
  if(deq0(x) .and. deq0(y) .and. deq0(z)) then
     l = 0.d0
     b = 0.d0
     r = 0.d0
  else
     x2 = x*x
     y2 = y*y
     
     l = rev( atan2(y,x) )        ! Longitude
     b = atan2(z, sqrt(x2 + y2))  ! Latitude
     r = sqrt(x2 + y2 + z*z)      ! Distance
  end if
  
end subroutine rect_2_spher
!***********************************************************************************************************************************


