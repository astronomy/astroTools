!> \file cal2times.f90  Convert calendar date/time from the system clock or command line to different time formats


!  Copyright (c) 2002-2015  AstroFloyd - astrofloyd.org
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
!> \brief  Convert calendar date/time from the system clock or command line to different time formats

program cal2times
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays,endys, enmonths,enmntsb, r2h
  use SUFR_system, only: syntax_quit, system_time
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms_sss
  use SUFR_date_and_time, only: cal2jd,jd2cal,jd2time,ymdhms2jd, jd2gps,jd2unix, dow_ut, doy, leapyr
  use SUFR_numerics, only: dne0
  use TheSky_datetime, only: calc_gmst
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: day,second,jd, tz, jd_lt,day_lt
  integer :: Narg, year,month,hour,minute, dy, year_lt,month_lt
  character :: ny(0:1)*(3)
  
  ! Initialise astroTools:
  call astroTools_init()
  ny(0:1) = ['No ','Yes']
  
  hour = 0
  minute = 0
  second = 0.d0
  tz = 0.d0
  
  write(*,*)
  Narg = command_argument_count()
  if(Narg.eq.0) then  ! Using system clock -> time = LT
     write(*,'(A,/)') '    Using time from the system clock'
     call system_time(year,month,dy, hour,minute,second, tz)
  else if(Narg.eq.3 .or. Narg.eq.6 .or. Narg.eq.7) then
     call get_command_argument_i(1, year)
     call get_command_argument_i(2, month)
     call get_command_argument_i(3, dy)
     
     if(Narg.ge.6) then
        call get_command_argument_i(4, hour)
        call get_command_argument_i(5, minute)
        call get_command_argument_d(6, second)
     end if
     
     tz = 0.d0  ! Assume UT
     if(Narg.eq.7) call get_command_argument_d(7, tz)  ! Time zone is specified -> time = LT
     
  else
     call syntax_quit('[<yr  mon  day[.dd]>  [hr  min  sec.ss [tz]]]',0,'Computes the GPS time for a given calendar date')
  end if
  
  jd = ymdhms2jd(year,month,dy, hour,minute,second-tz*3600)  ! UT
  call jd2cal(jd, year,month,day)
  jd_lt = jd + tz/24.d0                      ! LT
  call jd2cal(jd_lt, year_lt,month_lt,day_lt)
  
  if(dne0(tz)) then  ! Using system clock; have LT and TZ
     write(*,'(A,I9,I3,F7.3)')       '    Date (LT): ', year_lt,month_lt,day_lt
     write(*,'(A,A17)')              '    LT:        ', hms_sss(jd2time(jd_lt + 1.d-9))
     write(*,'(A,F9.2)')              '    TZ:        ', tz
     write(*,*)
  end if
  write(*,'(A,I9,I3,F7.3)')       '    Date:      ', year,month,day
  write(*,'(A,A17)')              '    Time:      ', hms_sss(jd2time(jd + 1.d-9))
  
  write(*,*)
  write(*,'(A,I3,2(5x,A))')          '    Month:          ', month, trim(enmntsb(month)), trim(enmonths(month))
  write(*,'(A,I3,2(5x,A))')          '    Day of week:    ', dow_ut(jd), trim(endys(dow_ut(jd))), trim(endays(dow_ut(jd)))
  write(*,'(A,I3)')               '    Day of year:    ', doy(jd)
  !write(*,'(A,I3)')               '    Week number:    ', woy(jd)
  
  write(*,*)
  write(*,'(A,A)')                '    Leap year:      ', ny(leapyr(year))
  
  
  
  write(*,*)
  write(*,'(A,F20.7)')            '    JD:        ', jd
  write(*,'(A,A17)')              '    GMST:      ', hms_sss(calc_gmst(jd)*r2h)
  write(*,*)
  write(*,'(A,F19.3)')            '    Unix time: ', jd2unix(jd)
  write(*,'(A,A)')                '    GPS time:       '//dbl2str(jd2gps(jd), 3)
  write(*,*)
  
end program cal2times
!***********************************************************************************************************************************

