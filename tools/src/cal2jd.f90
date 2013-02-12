!> \file cal2jf.f90  Compute the Julian day for a given calendar date

!***********************************************************************************************************************************
!> \brief  Compute the Julian day for a given calendar date
program calendar2jd
  use SUFR_kinds, only: double
  use SUFR_date_and_time, only: dtm2jd !cal2jd, 
  
  implicit none
  real(double) :: second,jd, dhour
  integer :: narg, year,month,day, hour,minute
  character :: str*(99)
  
  hour = 0
  minute = 0
  second = 0.d0
  
  narg = command_argument_count()
  if(narg.ge.3 .and. narg.le.6) then
     call get_command_argument(1,str)
     read(str,*) year
     call get_command_argument(2,str)
     read(str,*) month
     call get_command_argument(3,str)
     read(str,*) day
     
     if(narg.ge.4) then
        call get_command_argument(4,str)
        read(str,*) hour
        if(narg.ge.5) then
           call get_command_argument(5,str)
           read(str,*) minute
           if(narg.ge.6) then
              call get_command_argument(6,str)
              read(str,*) second
           end if
        end if
     end if
  else
     write(0,'(/,A,/)')'  syntax: cal2jd <year> <month> <day> [<hour> [<minute> [<second>]]]  (date/time in UT)'
     stop
  end if
  
  dhour = dble(hour) + dble(minute)/60.d0 + second/3600.d0
  !jd = cal2jd(year,month,day)
  jd = dtm2jd(year,month,day, dhour)
  write(6,'(/,I6,2I3.2, I6.2,I3.2,F7.3, A4, F20.8,/)') year,month,day, hour,minute,second, 'UT', jd
  
end program calendar2jd
!***********************************************************************************************************************************


