!> \file azimuth2winddir.f90  Convert azimuth to wind direction


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
!> \brief  Convert azimuth to wind direction

program azimuth2winddir
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, pi, d2r
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_angles, only: wdstr_ens, wdstr_en
  
  implicit none
  integer :: nArg,iArg
  real(double) :: az, dAz
  character :: arg*(99)
  
  call set_SUFR_constants()
  
  nArg = command_argument_count()
  if(nArg.lt.1.or.nArg.gt.2) call syntax_quit('<azimuth (deg; S=0)>', 0, 'Convert azimuth to wind direction (north = 0 deg; '// &
       'use --south for south = 0 deg')
  
  dAz = 0.d0
  do iArg = 1,nArg
     call get_command_argument(iArg, arg)
     if(arg(1:2).eq.'--') then
        if(trim(arg).eq.'--south') dAz = pi
     else
        call get_command_argument_d(iArg, az)
     end if
  end do
  
  write(*,'(F6.1,A6,A17)') az, wdstr_ens(az*d2r + dAz), wdstr_en(az*d2r + dAz)
  
end program azimuth2winddir
!***********************************************************************************************************************************
