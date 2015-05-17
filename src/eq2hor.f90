!> \file eq2hor.f90  Convert equatorial coordinates to horizontal


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
!> \brief  Convert equatorial coordinates (RA, dec) to horizontal (az,alt)

program eq2hor
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi, d2r,r2d, h2r,r2h
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use SUFR_date_and_time, only: year2jd
  use SUFR_angles, only: rev
  
  use TheSky_data, only: readnutation, read_deltat
  use TheSky_datetime, only: calc_deltat, calc_gmst
  use TheSky_nutation, only: nutation
  use TheSky_coordinates, only: eq2horiz
  
  use AT_general, only: astroTools_init
  
  implicit none
  real(double) :: ra,dec,eps,gmst,dpsi,eps0,deps
  real(double) :: lat,lon,agst,hh,az,alt, year,jd,jde,t,deltat
  integer :: Narg
  
  call astroTools_init()
  
  Narg = command_argument_count()
  if(Narg.lt.4.or.Narg.gt.5) call syntax_quit('<lat (°)> <lon (°, E=-)> <ra (h)> <dec (°)> [<year.yy> (default: 2000.0)]', &
       0, 'Convert equatorial coordinates (RA (h), dec (°)) to horizontal coordinates (az, alt (°))')
  
  call get_command_argument_d(1, lat)
  call get_command_argument_d(2, lon)
  call get_command_argument_d(3, ra)
  call get_command_argument_d(4, dec)
  
  year = 2000.d0
  if(Narg.eq.5) call get_command_argument_d(5, year)
  
  call readnutation()
  call read_deltat()
  
  ra  = ra*h2r
  dec = dec*d2r
  lat = lat*d2r
  lon = lon*d2r
  
  jd = year2jd(year)
  deltat = calc_deltat(jd)
  jde = jd + deltat/86400.d0
  t = (jde-2451545.d0)/365250.d0      ! Julian Millennia after 2000.0 in dynamical time, the tau in Meeus
  call nutation(t, dpsi,eps0,deps)    ! Calculate (cheap) nutation
  eps = eps0 + deps
  
  gmst = calc_gmst(jd)                ! Greenwich mean siderial time
  agst = rev(gmst + dpsi*dcos(eps))   ! Correction for equation of the equinoxes -> Gr. apparent sid. time
  
  call eq2horiz(ra,dec, agst,  hh,az,alt,  lat=lat,lon=lon)
  
  write(*,*)
  write(*,'(A,F15.3,A)') '  year: ', year
  write(*,'(A,F15.3,A)') '  JD:   ', jd
  write(*,*)
  write(*,'(A,F13.8,A)') '  ra:   ', ra*r2h,' h'
  write(*,'(A,F13.8,A)') '  dec:  ', dec*r2d,' °'
  write(*,*)
  write(*,'(A,F13.8,A)') '  gmst: ', rev(gmst)*r2h,' h'
  write(*,'(A,F13.8,A)') '  agst: ', rev(agst)*r2h,' h'
  write(*,'(A,F13.8,A)') '  hh:   ', rev(hh)*r2h,' h'
  write(*,*)
  write(*,'(A,F13.8,A)') '  az:   ', rev(az+pi)*r2d,' °'
  write(*,'(A,F13.8,A)') '  alt:  ', alt*r2d,' °'
  write(*,*)
  
end program eq2hor
!***********************************************************************************************************************************

