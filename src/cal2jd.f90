!> \file cal2jd.f90  Compute the Julian day for a given calendar date


!  Copyright (c) 2002-2020  AstroFloyd - astrofloyd.org
!   
!  This file is part of the astroTools package, 
!  see: http://astrotools.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.




!***********************************************************************************************************************************
!> \brief  Compute the Julian day for a given calendar date

program calendar2jd
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays
  use SUFR_system, only: syntax_quit, system_time
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_date_and_time, only: dtm2jd, dow_ut, correct_time
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: Narg, year,month,day, hour,minute
  real(double) :: second,jd, time, tz
  
  call astroTools_init()
  
  Narg = command_argument_count()
  if(Narg.eq.1 .or. Narg.eq.2 .or. Narg.gt.6) &
       call syntax_quit('<year> <month> <day> [<hour> [<minute> [<second>]]]  (date/time in UT)', &
       0, 'Compute the Julian day for a given calendar date and time')
  
  
  ! Default time: midnight UT:
  hour   = 0
  minute = 0
  second = 0.d0
  
  ! Get command-line arguments:
  if(Narg.eq.0) then  ! Use system clock:
     write(*,'(/,A)') '  Using the system time'
     call system_time(year,month,day, hour,minute,second, tz)
     second = second - tz*3600
     call correct_time(year,month,day, hour,minute,second)
  else
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
  end if
  
  ! Compute time and JD:
  time = dble(hour) + dble(minute)/60.d0 + second/3600.d0 + 1.d-10  ! Time in decimal hours
  jd = dtm2jd(year,month,day, time)
  
  ! Print output:
  write(*,'(/,A,2x, I0,2I3.2, I4.2,I3.2,F7.3, A)') '  Date/time:  '//trim(endays(dow_ut(jd))), &
       year,month,day, hour,minute,second, ' UT'
  write(*,'(A, F0.8,/)') '  JD:         ', jd
  
end program calendar2jd
!***********************************************************************************************************************************


