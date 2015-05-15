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
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init, print_times_ut
  
  implicit none
  real(double) :: jd
  
  ! Initialise astroTools:
  call astroTools_init()
  
  if(command_argument_count().ne.1) call syntax_quit('<JD>',0,'Computes times for a given Julian day')
  
  call get_command_argument_d(1, jd)
  
  call print_times_ut(jd)
  
end program jd2times
!***********************************************************************************************************************************


