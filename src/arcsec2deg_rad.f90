!> \file  arcsec2deg_rad.f90  Convert an angle expressd in arcseconds to degrees and radians


!***********************************************************************************************************************************
!> \brief  Convert an angle expressd in arcseconds to degrees and radians

program arcsec2deg_rad
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi, as2d,as2r
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  
  implicit none
  real(double) :: angle
  
  call astroTools_init()
  
  if(command_argument_count().ne.1)  call syntax_quit('<angle (arcseconds)>',0, &
       'arcsec2deg_rad converts an angle expressd in arcseconds to degrees and radians')
     
  call get_command_argument_d(1, angle)
  
  write(*,*)
  write(*,*) '  Arcseconds:    ',angle
  write(*,*) '  Degrees:       ',angle*as2d
  write(*,*) '  Radians:       ',angle*as2r, '  =  ',angle*as2r/pi,' pi'
  write(*,*)
  
end program arcsec2deg_rad
!***********************************************************************************************************************************

