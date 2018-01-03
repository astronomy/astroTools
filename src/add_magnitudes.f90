!> \file add_magnitudes.f90  Add magitudes and compute the total magnitude


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
!> \brief  Add magitudes and compute the total magnitude

program add_magnitudes
  use SUFR_kinds, only: double
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_system, only: syntax_quit
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: iMag,nMag
  real(double), allocatable :: mags(:)
  real(double) :: sumMag,totMag
  
  call astroTools_init()  ! Initialise aT and libSUFR
  
  nMag = command_argument_count()
  if(nMag.eq.0) call syntax_quit('<m_1> <m_2> ... <m_n>', 0, 'Add magnitudes and compute the total magnitude')
  
  allocate(mags(nMag))
  
  sumMag = 0.d0
  do iMag=1,nMag
     call get_command_argument_d(iMag, mags(iMag))
     sumMag = sumMag + 10.d0**(-mags(iMag)/2.5d0)
  end do
  totMag = -2.5d0*log10(sumMag)
  
  write(*,'(/,2x,A)', advance='no') 'Input magnitudes: '
  do iMag=1,nMag
     write(*,'(F6.2)', advance='no') mags(iMag)
  end do
  write(*,'(/,2x,A,F6.2,/)') 'Total magnitude:  ', totMag
  
end program add_magnitudes
!***********************************************************************************************************************************

