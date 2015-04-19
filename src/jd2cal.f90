!> \file jd2cal.f90  Compute the calendar date, time and GMST for a given Julian day


!***********************************************************************************************************************************
!> \brief  Compute the calendar date, time and GMST for a given Julian day

program jd2calendar
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_date_and_time, only: jd2cal
  use SUFR_time2string, only: hms_sss
  use TheSky_datetime, only: calc_gmst
  
  implicit none
  integer :: y,m
  real(double) :: d,jd
  
  ! Initialise astroTools:
  call astroTools_init()
  
  if(command_argument_count().ne.1) call syntax_quit('<JD>',0,'Computes the calendar date for a given julian date')
  
  call get_command_argument_d(1, jd)
  
  call jd2cal(jd,y,m,d)
  
  write(*,'(/,A,F20.7)')    '    JD:     ',jd
  write(*,'(A,I9,I3,F7.3)') '    Date:   ',y,m,d
  write(*,'(A,5x,A)')       '    UT:     ',hms_sss((d-floor(d))*24)
  write(*,'(A,5x,A,/)')     '    GMST:   ',hms_sss(calc_gmst(jd))
  
end program jd2calendar
!***********************************************************************************************************************************
