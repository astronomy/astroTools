!> \file jd2times.f90  Convert a Julian day to different time formats


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
!> \brief  Convert a Julian day to different time formats

program jd2times
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
  real(double) :: day,second,jd, tz
  integer :: year,month,hour,minute
  character :: ny(0:1)*(3)
  
  ! Initialise astroTools:
  call astroTools_init()
  ny(0:1) = ['No ','Yes']
  
  hour = 0
  minute = 0
  second = 0.d0
  tz = 0.d0
  
  if(command_argument_count().ne.1) call syntax_quit('<JD>',0,'Computes times for a given Julian day')
  
  call get_command_argument_d(1, jd)
  
  call jd2cal(jd, year,month,day)
  
  
  write(*,*)
  write(*,'(A,A)')           '    JD:             ', dbl2str(jd,7)
  write(*,*)
  
  write(*,'(A,I0,I3,F7.3)')  '    Date:           ', year,month,day
  write(*,'(A,A)')           '    Time:           ', hms_sss(jd2time(jd + 1.d-9))
  
  write(*,*)
  write(*,'(A,I3,2(5x,A))')  '    Month:          ', month, trim(enmntsb(month)), trim(enmonths(month))
  write(*,'(A,I3,2(5x,A))')  '    Day of week:    ', dow_ut(jd), trim(endys(dow_ut(jd))), trim(endays(dow_ut(jd)))
  write(*,'(A,I3)')          '    Day of year:    ', doy(jd)
  !write(*,'(A,I3)')         '    Week number:    ', woy(jd)
  
  write(*,*)
  write(*,'(A,A)')           '    Leap year:      ', ny(leapyr(year))
  
  
  
  write(*,*)
  write(*,'(A,A)')           '    GMST:           ', hms_sss(calc_gmst(jd)*r2h)
  write(*,*)
  write(*,'(A,A)')           '    Unix time:      ', dbl2str(jd2unix(jd), 3)
  write(*,'(A,A)')           '    GPS time:       ', dbl2str(jd2gps(jd), 3)
  write(*,*)
  
end program jd2times
!***********************************************************************************************************************************

