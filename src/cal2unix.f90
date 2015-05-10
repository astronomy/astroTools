!> \file cal2unix.f90  Convert calendar date/time to UNIX time stamp  0 = 1/1/1970 = JD 2440587.5

!***********************************************************************************************************************************
!> \brief  Convert calendar date/time to UNIX time stamp  0 = 1/1/1970 = JD 2440587.5

program cal2unix
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms
  use SUFR_date_and_time, only: cal2jd, dow_ut
  use TheSky_datetime, only: calc_gmst
  
  implicit none
  real(double) :: d,s,time,jd,unix,gps,gmst
  integer :: y,mon,h,min
  
  ! Initialise astroTools:
  call astroTools_init()
  
  h = 0
  min = 0
  s = 0.d0
  if(iargc().eq.3.or.iargc().eq.6) then
     call get_command_argument_i(1, y)
     call get_command_argument_i(2, mon)
     call get_command_argument_d(3, d)
     
     if(iargc().eq.6) then
        call get_command_argument_i(4, h)
        call get_command_argument_i(5, min)
        call get_command_argument_d(6, s)
     end if
  else
     call syntax_quit('<yr  mon  day[.dd]>  [hr  min  sec.ss]',0,'Computes the Unix time for a given calendar date')
  end if
  
  time =  dble(h) + dble(min)/60.d0 + s/3600.d0
  d = d + time/24.d0
  jd = cal2jd(y,mon,d)
  gmst = calc_gmst(jd)
  
  unix = (jd - 2440587.5d0)*86400
  
  gps = (jd - 2451544.5d0)*86400.d0 + 630720013.d0
  if(jd.lt.2444239.5d0) write(*,'(A)')'  WARNING: leap seconds are not taken into account when computing GPS time before 1/1/1980!'
  !if(jd.lt..5d0) gps = gps - 1  ! Leap second on 1//19
  if(jd.lt.2444786.5d0) gps = gps - 1  ! Leap second on 1/7/1981
  if(jd.lt.2445151.5d0) gps = gps - 1  ! Leap second on 1/7/1982
  if(jd.lt.2445516.5d0) gps = gps - 1  ! Leap second on 1/7/1983
  if(jd.lt.2446247.5d0) gps = gps - 1  ! Leap second on 1/7/1985
  if(jd.lt.2447161.5d0) gps = gps - 1  ! Leap second on 1/1/1988
  if(jd.lt.2447892.5d0) gps = gps - 1  ! Leap second on 1/1/1990
  if(jd.lt.2448257.5d0) gps = gps - 1  ! Leap second on 1/1/1991
  if(jd.lt.2448804.5d0) gps = gps - 1  ! Leap second on 1/7/1992
  if(jd.lt.2449169.5d0) gps = gps - 1  ! Leap second on 1/7/1993
  if(jd.lt.2449534.5d0) gps = gps - 1  ! Leap second on 1/7/1994
  if(jd.lt.2450083.5d0) gps = gps - 1  ! Leap second on 1/1/1996
  if(jd.lt.2450630.5d0) gps = gps - 1  ! Leap second on 1/7/1997
  if(jd.lt.2451179.5d0) gps = gps - 1  ! Leap second on 1/1/1999
  if(jd.ge.2453736.5d0) gps = gps + 1  ! Leap second on 1/1/2006
  if(jd.ge.2454832.5d0) gps = gps + 1  ! Leap second on 1/1/2009
  
  write(*,*)
  write(*,'(A,A)')                '    GPStime:     '//dbl2str(gps, 3)
  write(*,'(A,F20.7)')            '    JD:     ', jd
  write(*,'(A,5x,A,I7,I3,F7.3)')  '    Date:   ',trim(endays(dow_ut(jd))),y,mon,d
  write(*,'(A,A13)')              '    UT:     ',hms(time)
  write(*,'(A,A13)')              '    GMST:   ',hms(gmst)
  write(*,'(A,F18.3)')            '    Unix:    ',unix
  write(*,*)
  
end program cal2unix
!***********************************************************************************************************************************


      
