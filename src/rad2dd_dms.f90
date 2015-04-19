!> \file  rad2dd_dms.f90  Convert an angle expressd in radians to  decimal degrees,  degrees and arcminutes,  and
!!                        degrees, arcminutes and arcseconds


!***********************************************************************************************************************************
!> \brief  Convert an angle expressd in radians to  decimal degrees,  degrees and arcminutes,  and
!!                        degrees, arcminutes and arcseconds

program rad2dd_dms
  use SUFR_kinds, only: double
  use SUFR_constants, only: r2d, pi
  use SUFR_angle2string, only: dmss2, dmmmmm2
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  
  implicit none
  real(double) :: angle
  
  call astroTools_init()
  
  if(command_argument_count().ne.1)  call syntax_quit('<angle (radians)>',0, &
       'rad2dd_dms converts an angle expressed in radians to decimal degrees and degrees, arcminutes and arcseconds')
     
  call get_command_argument_d(1, angle)
  
  write(*,*)
  write(*,*) '  Radians:                                     ',angle, '  =  ',angle/pi,' pi'
  write(*,*) '  Decimal degrees:                             ',angle*r2d
  write(*,*) '  Degrees and decimal arcminutes:                ',dmmmmm2(angle)
  write(*,*) '  Degrees, arcminutes and decimal arcseconds:    ',dmss2(angle)
  write(*,*)
  
end program rad2dd_dms
!***********************************************************************************************************************************

