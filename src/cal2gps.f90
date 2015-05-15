!> \file cal2gps.f90  Convert calendar date/time to GPS time (1/1/2000 = 630720013.0)


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
!> \brief  Convert calendar date/time to GPS time (1/1/2000 = 630720013.0)

program cal2gps
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays, r2h
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms
  use SUFR_date_and_time, only: cal2jd, jd2gps,jd2unix, dow_ut
  use TheSky_datetime, only: calc_gmst
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: d,s,time,jd
  integer :: Narg, y,mon,h,min
  
  ! Initialise astroTools:
  call astroTools_init()
  
  h = 0
  min = 0
  s = 0.d0
  
  Narg = command_argument_count()
  if(Narg.eq.3 .or. Narg.eq.6) then
     call get_command_argument_i(1, y)
     call get_command_argument_i(2, mon)
     call get_command_argument_d(3, d)
     
     if(Narg.eq.6) then
        call get_command_argument_i(4, h)
        call get_command_argument_i(5, min)
        call get_command_argument_d(6, s)
     end if
  else
     call syntax_quit('<yr  mon  day[.dd]>  [hr  min  sec.ss]',0,'Computes the GPS time for a given calendar date')
  end if
  
  time =  dble(h) + dble(min)/60.d0 + s/3600.d0
  d = d + time/24.d0
  jd = cal2jd(y,mon,d)
  
  write(*,*)
  write(*,'(A,A)')            '    GPS time:       '//dbl2str(jd2gps(jd), 3)
  write(*,*)
  write(*,'(A,5x,A,I7,2I3)')  '    Date:      ', trim(endays(dow_ut(jd))),y,mon,floor(d)
  write(*,'(A,A13)')          '    UT:        ', hms(time + 1.d-9)
  write(*,*)
  write(*,'(A,F20.7)')        '    JD:        ', jd
  write(*,'(A,A13)')          '    GMST:      ', hms(calc_gmst(jd)*r2h)
  write(*,'(A,F19.3)')        '    Unix time: ', jd2unix(jd)
  write(*,*)
  
end program cal2gps
!***********************************************************************************************************************************

