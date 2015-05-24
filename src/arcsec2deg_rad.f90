!> \file  arcsec2deg_rad.f90  Convert an angle expressed in arcseconds to degrees and radians


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
!> \brief  Convert an angle expressed in arcseconds to degrees and radians

program arcsec2deg_rad
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi, as2d,as2r
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: angle
  
  call astroTools_init()
  
  if(command_argument_count().ne.1)  call syntax_quit('<angle (arcseconds)>',0, &
       'arcsec2deg_rad converts an angle expressed in arcseconds to degrees and radians')
     
  call get_command_argument_d(1, angle)
  
  write(*,*)
  write(*,*) '  Arcseconds:    ',angle
  write(*,*) '  Degrees:       ',angle*as2d
  write(*,*) '  Radians:       ',angle*as2r, '  =  ',angle*as2r/pi,' pi'
  write(*,*)
  
end program arcsec2deg_rad
!***********************************************************************************************************************************

