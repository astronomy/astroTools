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
  
  
  !*********************************************************************************************************************************
  !> \brief  Print times for *2times output, assuming that everything is UT
  !!
  !! \param jd  JD to print times for
  
  subroutine print_times_ut(jd)
    use SUFR_kinds, only: double
    use SUFR_constants, only: endays,endys, enmonths,enmntsb, r2h
    use SUFR_text, only: dbl2str
    use SUFR_time2string, only: hms_sss
    use SUFR_date_and_time, only: cal2jd,jd2cal,jd2time,ymdhms2jd, jd2gps,jd2unix, dow_ut, doy, leapyr
    
    use TheSky_datetime, only: calc_gmst
    
    implicit none
    real(double), intent(in) :: jd
    real(double) :: day
    integer :: year,month
    character :: ny(0:1)*(3)
    
    ny(0:1) = ['No ','Yes']
    
    call jd2cal(jd, year,month,day)
    
    write(*,*)
    write(*,'(A,A)')           '    JD:             ', dbl2str(jd,7)
    write(*,*)
    
    write(*,'(A,I0,I3,F7.3)')  '    Date:           ', year,month,day
    write(*,'(A,A)')           '    Time:           ', hms_sss(jd2time(jd + 1.d-9))
    
    write(*,*)
    write(*,'(A,I3,2(5x,A))')  '    Month:          ', month, trim(enmntsb(month)), trim(enmonths(month))
    write(*,'(A,I3,2(5x,A))')  '    Day of week:    ', dow_ut(jd), trim(endys(dow_ut(jd))), trim(endays(dow_ut(jd)))
    write(*,'(A,I3)')          '    Day of year:    ', doy(jd)
    !write(*,'(A,I3)')         '    Week number:    ', woy(jd)
    
    write(*,*)
    write(*,'(A,A)')           '    Leap year:      ', ny(leapyr(year))
    
    
    
    write(*,*)
    write(*,'(A,A)')           '    GMST:           ', hms_sss(calc_gmst(jd)*r2h)
    write(*,*)
    write(*,'(A,A)')           '    Unix time:      ', dbl2str(jd2unix(jd), 3)
    write(*,'(A,A)')           '    GPS time:       ', dbl2str(jd2gps(jd), 3)
    write(*,*)
    
  end subroutine print_times_ut
  !*********************************************************************************************************************************
  
end module AT_general
!***********************************************************************************************************************************
