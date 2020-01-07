!> \file  rad2dd_dms.f90  Convert an angle expressed in radians to  decimal degrees,  degrees and arcminutes,  and
!!                        degrees, arcminutes and arcseconds


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
!> \brief  Convert an angle expressed in radians to  decimal degrees,  degrees and arcminutes,  and
!!                        degrees, arcminutes and arcseconds

program rad2dd_dms
  use SUFR_kinds, only: double
  use SUFR_constants, only: r2d, pi
  use SUFR_angle2string, only: dmss2, dmmmmm2
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: angle
  
  call astroTools_init()
  
  if(command_argument_count().ne.1)  call syntax_quit('<angle (radians)>',0, &
       'rad2dd_dms converts an angle expressed in radians to decimal degrees and degrees, arcminutes and arcseconds')
     
  call get_command_argument_d(1, angle)
  
  write(*,*)
  write(*,*) '  Radians:                                     ',angle, '  =  ',angle/pi,' pi'
  write(*,*) '  Decimal degrees:                             ',angle*r2d
  write(*,*) '  Degrees and decimal arcminutes:                ',dmmmmm2(angle)
  write(*,*) '  Degrees, arcminutes and decimal arcseconds:    ',dmss2(angle)
  write(*,*)
  
end program rad2dd_dms
!***********************************************************************************************************************************

