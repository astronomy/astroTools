!> \file  dd2dms_rad.f90  Convert an angle expressd in decimal degrees to  degrees and arcminutes,
!!                        degrees, arcminutes and arcseconds,  and  radians


!***********************************************************************************************************************************
!> \brief  Convert an angle expressd in decimal degrees to  degrees and arcminutes,  degrees, arcminutes and arcseconds,
!!         and  radians

program dd2dms_rad
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, d2r
  use SUFR_angle2string, only: dmss2
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  
  implicit none
  real(double) :: angle
  character :: dmmmmm2*(12)
  
  call set_SUFR_constants()
  
  if(command_argument_count().ne.1)  call syntax_quit('<angle (degrees)>',0, &
       'dd2dms_rad converts an angle expressed in decimal degrees to degrees, arcminutes and arcseconds, and radians')
     
  call get_command_argument_d(1, angle)
  
  !write(*,'(2x,F14.7,A,A15,A16, F20.12,A)') angle,' d', dmmmmm2(angle*d2r), dmss2(angle*d2r), angle*d2r,' rad'
  write(*,*)
  write(*,*) '  Decimal degrees:                             ',angle
  write(*,*) '  Degrees and decimal arcminutes:                ',dmmmmm2(angle*d2r)
  write(*,*) '  Degrees, arcminutes and decimal arcseconds:    ',dmss2(angle*d2r)
  write(*,*) '  Radians:                                     ',angle*d2r, '  =  ',angle/180.d0,' pi'
  write(*,*)
  
end program dd2dms_rad
!***********************************************************************************************************************************

