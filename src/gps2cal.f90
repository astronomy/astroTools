!> \file gps2cal.f90  Convert GPS time to calendar date and time


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
!> \brief  Convert GPS time to calendar date and time
!!
!! \note 1/1/2000 = 630720013.0, i.e. 13 leap seconds after 1/1/1980

program gps2cal 
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_date_and_time, only: gps2jd, jd2cal
  use SUFR_time2string, only: hms_sss
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: day,jd,GPStime
  integer :: year,month
  
  ! Initialise astroTools:
  call astroTools_init()
  
  if(command_argument_count().ne.1) call syntax_quit('<GPS time>',0,'Computes calendar date and time for a given GPS time')
  
  call get_command_argument_d(1, GPStime)
  
  jd = gps2jd(GPStime)
  call jd2cal(jd, year,month,day)
  
  write(*,*)
  write(6,'(A,A)')         '    GPStime:  ', dbl2str(GPStime, 3)
  write(6,'(A,A)')         '    JD:       ', dbl2str(jd,7)
  write(6,'(A,I0,I3,I3)')  '    Date:     ', year,month,floor(day)
  write(6,'(A,A)')         '    UT:       ', hms_sss((day-floor(day))*24 + 1.d-9)
  write(*,*)
  
end program gps2cal
!***********************************************************************************************************************************

