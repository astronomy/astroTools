!> \file functions.f90  General procedures for astroTools


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
!> \brief  Settings for astroTools

module AT_settings
  use SUFR_kinds, only: double
  implicit none
  save
  
  integer :: dstRule
  real(double) :: geoLat, geoLon, geoAlt,  tz0
  
  character :: settingsFile*(256)
  
end module AT_settings
!***********************************************************************************************************************************


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
    use SUFR_constants, only: homedir
    use TheSky_data, only: set_TheSky_constants
    use AT_settings, only: settingsFile
    
    implicit none
    logical, intent(in), optional :: banner
    logical :: lbanner
    
    lbanner = .true.
    if(present(banner)) lbanner = banner
    
    ! Print version banner:
    if(lbanner) call print_astroTools_banner()
    
    ! Initialise libSUFR and TheSky constants:
    call set_TheSky_constants()
    
    settingsFile = trim(homedir)//'/.astroTools'
    call readSettingsFile()
    
  end subroutine astroTools_init
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print a banner with version information
  
  subroutine print_astroTools_banner()
    use AT_version, only: print_astroTools_version
    
    implicit none
    
    write(*,'(/,A)', advance='no') ''
    call print_astroTools_version(6)
    write(*,*) ' -  astrotools.sf.net'
    
  end subroutine print_astroTools_banner
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Read the astroTools settings from ~/.astroTools
  
  subroutine readSettingsFile()
    use SUFR_system, only: find_free_io_unit
    use SUFR_constants, only: d2r
    
    use AT_settings, only: settingsFile, geoLat,geoLon,geoAlt, tz0,dstRule
    
    implicit none
    integer :: ip, status
    namelist /astroTools_settings/ geoLat,geoLon,geoAlt, tz0,dstRule
    
    call find_free_io_unit(ip)
    open(unit=ip,form='formatted',status='old',action='read',position='rewind',file=trim(settingsFile),iostat=status)
    if(status.ne.0) then
       write(*,'(/,A)') '  Settings file  '//trim(settingsFile)//"  not found;  I'll ask you five questions to generate it."
       call createSettingsFile()
    end if
    
    read(ip, nml=astroTools_settings, iostat=status)
    
    close(ip)
    
    geoLat = geoLat * d2r
    geoLon = geoLon * d2r
    
    call AT2TheSky_location()  ! Use the astroTools location as the TheSky location
    
  end subroutine readSettingsFile
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Print the astroTools settings from ~/.astroTools to screen
  
  subroutine printSettings()
    use SUFR_constants, only: stdOut, r2d
    use AT_settings, only: geoLat,geoLon,geoAlt, tz0,dstRule
    
    implicit none
    
    write(stdOut,*)
    write(stdOut,'(2x,A)') 'Current astroTools settings:'
    write(stdOut,'(4x,A,F7.3,A)') 'geoLat   = ',  geoLat*r2d, '     Geographical latitude (degrees, north is positive)'
    write(stdOut,'(4x,A,F7.3,A)') 'geoLon   = ',  geoLon*r2d, '     Geographical longitude (degrees, east is positive)'
    write(stdOut,'(4x,A,F5.1,A)') 'geoAlt   = ',    geoAlt, '       Altitude above sea level (metres)'
    write(stdOut,'(4x,A,F6.2,A)') 'tz0      = ',      tz0,   '      Base (winter) timezone ((decimal) hours, east is positive)'
    write(stdOut,'(4x,A,I3,A)')   'dstRule  = ', dstRule, '         Daylight-saving-time rules (0-none, 1-EU, 2-US)'
    write(stdOut,*)
    
  end subroutine printSettings
  !*********************************************************************************************************************************
  
  
  !*********************************************************************************************************************************
  !> \brief  Create the astroTools settings file (~/.astroTools)
  
  subroutine createSettingsFile()
    use SUFR_kinds, only: double
    use SUFR_system, only: find_free_io_unit, file_open_error_quit
    use AT_settings, only: settingsFile
    
    implicit none
    integer :: op, status, dstRule
    real(double) :: geoLat,geoLon,geoAlt, tz0
    
    ! Gather the data for the settings file:
    write(*,'(4x,A)', advance='no') 'Your default geographical latitude (degrees, north is positive):          '
    read(*,*) geoLat
    write(*,'(4x,A)', advance='no') 'Your default geographical longitude (degrees, east is positive):          '
    read(*,*) geoLon
    write(*,'(4x,A)', advance='no') 'Your default altitude above sea level (metres):                           '
    read(*,*) geoAlt
    write(*,'(4x,A)', advance='no') 'Your default base (winter) timezone ((decimal) hours, east is positive):  '
    read(*,*) tz0
    write(*,'(4x,A)', advance='no') 'Your default daylight-saving-time rules (0-none, 1-EU, 2-US):             '
    read(*,*) dstRule
    
    tz0 = dble(nint(tz0*4))/4.d0  ! Round off time zone to nearest quarter of an hour
    
    
    ! Open the settings file for (over)writing:
    call find_free_io_unit(op)
    open(unit=op,form='formatted',status='replace',action='write',position='rewind',file=trim(settingsFile),iostat=status)
    if(status.ne.0) call file_open_error_quit(trim(settingsFile), 0, 1)  ! 0: output file, 1: status: not ok
    
    ! Mimick a namelist format, but with comments:
    write(op,'(A)') '! astroTools settings file - astrotools.sf.net'
    write(op,'(A)') ''
    write(op,'(A)') '&astroTools_settings'
    write(op,'(A,F7.3,A)') ' geoLat   = ',      geoLat, ',  ! Geographical latitude (degrees, north is positive)'
    write(op,'(A,F7.3,A)') ' geoLon   = ',      geoLon, ',  ! Geographical longitude (degrees, east is positive)'
    write(op,'(A,F5.1,A)') ' geoAlt   = ',    geoAlt, ',    ! Altitude above sea level (metres)'
    write(op,'(A,F6.2,A)') ' tz0      = ',        tz0, ',   ! Base (winter) timezone ((decimal) hours, east is positive)'
    write(op,'(A,I3,A)')   ' dstRule  = ', dstRule, ',      ! Daylight-saving-time rules (0-none, 1-EU, 2-US)'
    write(op,'(A)') ' /'
    
    close(op)
    
    write(*,'(A)') '  Settings file  '//trim(settingsFile)//'  created.  Run  at_settings  to see its contents'
    
  end subroutine createSettingsFile
  !*********************************************************************************************************************************
  
  !*********************************************************************************************************************************
  !> \brief  Use the astroTools location as the TheSky location
  !!
  !! \note This copies the location data from the ~/.astroTools settings file to the TheSky_local module
  subroutine AT2TheSky_location()
    use TheSky_functions, only: set_TheSky_location
    use AT_settings, only: geoLat,geoLon,geoAlt, tz0,dstRule
    implicit none
    
    call set_TheSKy_location(geoLat,geoLon,geoAlt, tz0,dstRule)
    
  end subroutine AT2TheSky_location
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
    use SUFR_date_and_time, only: cal2jd,jd2cal,jd2time, jd2gps,jd2unix, dow_ut, doy, leapyr
    
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
    
    write(*,'(A,I0,2I3)')      '    Date:           ', year,month,floor(day)
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
