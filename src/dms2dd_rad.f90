!> \file program dms2dd_rad.f90  Convert an angle expressed in degrees, arcminutes and arcseconds to decimal degrees and radians


!***********************************************************************************************************************************
!> \brief  Convert an angle expressed in degrees, arcminutes and arcseconds to decimal degrees and radians

program dms2dd_rad
  use SUFR_kinds, only: double
  use SUFR_constants, only: d2r
  use SUFR_angle2string, only: dmss2, dmmmmm2
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  
  implicit none
  real(double) :: d,m,s, angle
  
  call astroTools_init()
  
  if(command_argument_count().ne.3)  call syntax_quit('<degrees> <arcminutes> <arcseconds>',0, &
       'dms2dd_rad converts an angle expressed in degrees, arcminutes and arcseconds to decimal degrees and radians')
  
  call get_command_argument_d(1, d)
  call get_command_argument_d(2, m)
  call get_command_argument_d(3, s)
  
  if(d.lt.0.d0) then
     m = -dabs(m)
     s = -dabs(s)
  end if
  
  angle = d + m/60.d0 + s/3600.d0
  
  write(*,*)
  write(*,*) '  Decimal degrees:                             ',angle
  write(*,*) '  Degrees and decimal arcminutes:                ',dmmmmm2(angle*d2r)
  write(*,*) '  Degrees, arcminutes and decimal arcseconds:    ',dmss2(angle*d2r)
  write(*,*) '  Radians:                                     ',angle*d2r, '  =  ',angle/180.d0,' pi'
  write(*,*)
  
end program dms2dd_rad
!***********************************************************************************************************************************
