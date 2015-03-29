!> \file cal2jd.f90  Compute the Julian day for a given calendar date

!***********************************************************************************************************************************
!> \brief  Compute the Julian day for a given calendar date

program calendar2jd
  use SUFR_kinds, only: double
  use SUFR_date_and_time, only: dtm2jd
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  
  implicit none
  integer :: Narg, year,month,day, hour,minute
  real(double) :: second,jd, time
  
  call astroTools_init()
  
  Narg = command_argument_count()
  if(Narg.lt.3 .or. Narg.gt.6) call syntax_quit('<year> <month> <day> [<hour> [<minute> [<second>]]]  (date/time in UT)', &
       0, 'Compute the Julian day for a given calendar date and time')
  
  
  ! Default time: midnight UT:
  hour   = 0
  minute = 0
  second = 0.d0
  
  ! Get command-line arguments:
  call get_command_argument_i(1,year)
  call get_command_argument_i(2,month)
  call get_command_argument_i(3,day)
  
  if(Narg.ge.4) then
     call get_command_argument_i(4,hour)
     
     if(Narg.ge.5) then
        call get_command_argument_i(5,minute)
        
        if(Narg.ge.6) call get_command_argument_d(6,second)
     end if
  end if
  
  
  ! Compute time and JD:
  time = dble(hour) + dble(minute)/60.d0 + second/3600.d0  ! Time in decimal hours
  jd = dtm2jd(year,month,day, time)
  
  ! Print output:
  write(*,'(/,A, I0,2I3.2, I4.2,I3.2,F7.3, A)') '  Date/time:  ',year,month,day, hour,minute,second, ' UT'
  write(*,'(A, F0.8,/)') '  JD:         ', jd
  
end program calendar2jd
!***********************************************************************************************************************************


