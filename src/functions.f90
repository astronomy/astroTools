!> \file functions.f90  General procedures for astroTools


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
!> \brief  General procedures for astroTools

module AT_general
  implicit none
  save
  
contains
  
  
  !*********************************************************************************************************************************
  !> \brief  Initialise an astroTools program
  !!
  !! \param banner  Print aT banner with version number (optional, default: true)
  
  subroutine astroTools_init(banner)
    use SUFR_constants, only: set_SUFR_constants
    use AT_version, only: print_astroTools_version
    
    implicit none
    logical, intent(in), optional :: banner
    logical :: lbanner
    
    lbanner = .true.
    if(present(banner)) lbanner = banner
    
    ! Print version:
    if(lbanner) then
       write(*,'(/,A)', advance='no') '  '
       call print_astroTools_version(6)
       write(*,*) ' -  astrotools.sf.net'
    end if
    
    ! Initialise libSUFR constants:
    call set_SUFR_constants()
    
  end subroutine astroTools_init
  !*********************************************************************************************************************************
  
  
end module AT_general
!***********************************************************************************************************************************
