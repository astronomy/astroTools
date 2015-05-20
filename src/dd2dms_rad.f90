!> \file  dd2dms_rad.f90  Convert an angle expressd in decimal degrees to  degrees and arcminutes,
!!                        degrees, arcminutes and arcseconds,  and  radians


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
!> \brief  Convert an angle expressd in decimal degrees to  degrees and arcminutes,  degrees, arcminutes and arcseconds,
!!         and  radians

program dd2dms_rad
  use SUFR_kinds, only: double
  use SUFR_constants, only: d2r
  use SUFR_angle2string, only: dmss2, dmmmmm2
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: iArg, status
  real(double) :: angle
  
  call astroTools_init()
  
  if(command_argument_count().lt.1)  call syntax_quit('<angle (degrees)>',0, &
       'dd2dms_rad converts an angle expressed in decimal degrees to degrees, arcminutes and arcseconds, and radians')
  
  do iArg=1,command_argument_count()
     call get_command_argument_d(iArg, angle, status)
     
     if(status.ne.0) call syntax_quit('<angle (degrees)>',0, &
          'dd2dms_rad converts an angle expressed in decimal degrees to degrees, arcminutes and arcseconds, and radians')
     
     write(*,*)
     write(*,*) '  Decimal degrees:                             ', angle
     write(*,*) '  Degrees and decimal arcminutes:                ', dmmmmm2(angle*d2r), '  =  ', &
          dmmmmm2(angle*d2r, noSymbols=.true.)
     write(*,*) '  Degrees, arcminutes and decimal arcseconds:    ',dmss2(angle*d2r), '  =  ', dmss2(angle*d2r, noSymbols=.true.)
     write(*,*) '  Radians:                                     ', angle*d2r, '  =  ', angle/180.d0,' pi'
     write(*,*)
  end do
  
end program dd2dms_rad
!***********************************************************************************************************************************

