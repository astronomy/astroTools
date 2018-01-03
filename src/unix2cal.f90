!> \file unix2cal.f90  Convert UNIX time stamp to calendar


!  Copyright (c) 2002-2018  AstroFloyd - astrofloyd.org
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
!> \brief  Convert Unix time stamp to calendar
!!
!! \note Unix time 0 = 1970-01-01 = JD 2440587.5

program unix2cal
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_date_and_time, only: unix2jd, jd2cal, dow_ut
  use SUFR_time2string, only: hms_sss
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: day,jd,unixTime
  integer :: iArg,nArg, year,month
  
  call astroTools_init()
  
  nArg = command_argument_count()
  if(nArg.lt.1) call syntax_quit('<Unix timestamp>  (date/time will be in UT)', &
       0, 'Compute the Julian day for a given Unix timestamp')

  do iArg=1,nArg
     call get_command_argument_d(1, unixTime)
     
     jd = unix2jd(unixTime)
     call jd2cal(jd, year,month,day)
     
     write(*,*)
     write(*,'(A,A)')                '  Unix time:  ', dbl2str(unixTime,3)
     write(*,'(A,A)')                '  JD:         ', dbl2str(jd,7)
     write(*,'(A,A,2x,I0,2I3.2,A)')  '  Date:       ', trim(endays(dow_ut(jd))), year,month,floor(day),' UT'
     write(*,'(A,A,A)')              '  Time:       ', hms_sss((day-floor(day))*24),' UT'
     write(*,*)
  end do
  
end program unix2cal
!***********************************************************************************************************************************



