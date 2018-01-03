!> \file  time2hms.f90  Convert time expressed in decimal hours to  hours and minutes,  and  hours, minutes and seconds


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
!> \brief  Convert an time expressed in decimal hours to  hours and minutes,  and  hours, minutes and seconds

program time2hms
  use SUFR_kinds, only: double
  use SUFR_text, only: d2s
  use SUFR_date_and_time, only: tm2hmss
  use SUFR_time2string, only: hm_mmm, hms_sss
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: iArg, status,  hr,mn
  real(double) :: time, se, dmn
  
  call astroTools_init()  ! Initialise astroTools, libTheSky and libSUFR
  
  if(command_argument_count().lt.1)  call syntax_quit('<time (hours)>',0, &
       'time2hms converts an time expressed in decimal hours to hours, minutes and seconds')
  
  do iArg=1,command_argument_count()
     call get_command_argument_d(iArg, time, status)
     
     if(status.ne.0) call syntax_quit('<time (hours)>',0, &
          'time2hms converts an time expressed in decimal hours to hours, minutes and seconds')
     
     call tm2hmss(time, hr,mn,se)
     dmn = mn + se/60.d0
     
     write(*,*)
     write(*,'(A)')              '  Decimal hours:                          '//d2s(time,6)
     write(*,'(A, I3,F7.3, A)')  '  Hours, decimal minutes:                ', hr,dmn, '     =  '//hm_mmm(time)
     write(*,'(A, 2I3,F7.3, A)') '  Hours, minutes and decimal seconds:    ', hr,mn,se, '  =  '//hms_sss(time)
     write(*,*)
  end do
  
end program time2hms
!***********************************************************************************************************************************

