!> \file distance_in_sky_ecl.f90  Calculate the angular distance between two positions on a stellar globe, in ecliptic
!!                                coordinates (or any other system with longitude and latitude in degrees, like galactic)


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
!> \brief  Calculate the angular distance between two positions on a stellar globe, in ecliptic coordinates (or any other system
!!         with longitude and latitude in degrees, like galactic)

program distance_in_sky_ecl
  use SUFR_kinds, only: double
  use SUFR_constants, only: d2r,r2d
  use SUFR_system, only: quit_program_error
  use SUFR_angles, only: rev2, asep
  use SUFR_angle2string, only: dmss,dmss2
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: status
  real(double) :: x1,x2,x3,l1,l2,b1,b2, dist
  
  call astroTools_init()  ! Initialise aT and libSUFR
  
  write(*,'(/,A)') '  Calculate the angular distance between two positions on a stellar globe, in ecliptic, galactic, etc. '// &
       'coordinates'
  
  
  ! Enter position 1:
  write(*,'(/,2x,A)') 'Position 1:'
  write(*,'(4x,A)', advance='no') 'Please give the longitude (d,m,s):  '
  read(*,*, iostat=status) x1,x2,x3
  if(status.ne.0) call quit_program_error('I expected three numbers...', 1)
  
  l1 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(x1.lt.0) l1 = -l1
  
  
  write(*,'(4x,A)', advance='no') 'Please give the latitude (d,m,s):   '
  read(*,*, iostat=status) x1,x2,x3
  if(status.ne.0) call quit_program_error('I expected three numbers...', 1)
  
  b1 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(x1.lt.0) b1 = -b1
  
  
  
  ! Enter position 2:
  write(*,'(/,2x,A)') 'Position 2:'
  write(*,'(4x,A)', advance='no') 'Please give the longitude (d,m,s):  '
  read(*,*, iostat=status) x1,x2,x3
  if(status.ne.0) call quit_program_error('I expected three numbers...', 1)
  
  l2 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(x1.lt.0) l2 = -l2
  
  
  write(*,'(4x,A)', advance='no') 'Please give the latitude (d,m,s):   '
  read(*,*, iostat=status) x1,x2,x3
  if(status.ne.0) call quit_program_error('I expected three numbers...', 1)
  
  b2 = rev2((abs(x1) + abs(x2)/60.d0 + abs(x3)/3600.d0)*d2r)
  if(x1.lt.0) b2 = -b2
  
  
  ! Compute the distance between positions 1 and 2:
  dist = asep(l1,l2, b1,b2)
  
  
  
  write(*,'(/)')
  write(*,'(A)') '                        Longitude:            Latitude:'
  write(*,'(2x,A,2(7x,A))')    'Location 1: ',dmss(l1),dmss2(b1)
  write(*,'(2x,A,2(7x,A))')    'Location 2: ',dmss(l2),dmss2(b2)
  write(*,*) ''
  write(*,'(2x,A,2(6x,A))')    'Differences:',dmss2(l2-l1)//' ',dmss2(b2-b1)
  write(*,*) ''
  write(*,'(2x,A,6x,A,F20.6)') 'Separation: ',dmss2(dist), dist*r2d
  write(*,'(/)')
  
  
end program distance_in_sky_ecl
!***********************************************************************************************************************************
