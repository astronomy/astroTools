!> \file azimuth2winddir.f90  Convert azimuth to wind direction


!  Copyright (c) 2002-2022  AstroFloyd - astrofloyd.org
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
  if(nArg.lt.1.or.nArg.gt.2) call syntax_quit('<azimuth (deg; south=0)>', 0, 'Convert azimuth to wind direction '// &
       '(south = 0 deg; use --north0 for north = 0 deg')
  
  dAz = pi
  do iArg = 1,nArg
     call get_command_argument(iArg, arg)
     if(arg(1:2).eq.'--') then
        if(trim(arg).eq.'--north0') dAz = 0.d0
     else
        call get_command_argument_d(iArg, az)
     end if
  end do
  
  write(*,'(F6.1,A6,A17)') az, wdstr_ens(az*d2r + dAz), wdstr_en(az*d2r + dAz)
  
end program azimuth2winddir
!***********************************************************************************************************************************
