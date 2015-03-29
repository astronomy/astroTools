!> \file rect2spher  Convert rectangular coordinates to spherical coordinates

!***********************************************************************************************************************************
!> \brief  Convert rectangular coordinates to spherical coordinates

program rect2spher
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_constants, only: r2d
  use SUFR_angles, only: rev,rev2
  use TheSky_coordinates, only: rect_2_spher
  
  implicit none
  real(double) :: x,y,z, l,b,r
  
  call astroTools_init()
  
  if(command_argument_count().ne.3) &
       call syntax_quit('syntax: rect2spher <x> <y> <z>', 0, &
       'Convert (ecliptical) coordinates from a rectangular to a spherical frame')
  
  call get_command_argument_d(1,x)
  call get_command_argument_d(2,y)
  call get_command_argument_d(3,z)
  
  call rect_2_spher(x,y,z, l,b,r)
  
  write(*,'(/,2x,A,3(F20.10,A))') 'x,y,z: ', x, ' AU', y, ' AU', z, ' AU'
  write(*,'(2x,A,3(F20.10,A),/)') 'l,b,r: ', rev(l)*r2d, ' ° ', rev2(b)*r2d, ' ° ', r, ' AU'
  
end program rect2spher
!***********************************************************************************************************************************
