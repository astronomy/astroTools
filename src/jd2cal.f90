!> \file jd2cal.f90  Compute the calendar date, time and GMST for a given Julian day


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
!> \brief  Compute the calendar date, time and GMST for a given Julian day

program jd2calendar
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays, r2h
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_date_and_time, only: jd2cal, dow_ut
  use SUFR_time2string, only: hms_sss
  
  use TheSky_datetime, only: calc_gmst
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: y,m
  real(double) :: d,jd
  
  ! Initialise astroTools:
  call astroTools_init()
  
  if(command_argument_count().ne.1) call syntax_quit('<JD>',0,'Computes the calendar date for a given julian date')
  
  call get_command_argument_d(1, jd)
  
  call jd2cal(jd,y,m,d)
  
  write(*,'(/,A,F20.7)')     '    JD:     ',jd
  write(*,'(A,5x,A,I7,2I3)') '    Date:   ',trim(endays(dow_ut(jd))),y,m,floor(d)
  write(*,'(A,5x,A)')        '    UT:     ',hms_sss((d-floor(d))*24 + 1.d-9)
  write(*,'(A,5x,A,/)')      '    GMST:   ',hms_sss(calc_gmst(jd)*r2h)
  
end program jd2calendar
!***********************************************************************************************************************************
