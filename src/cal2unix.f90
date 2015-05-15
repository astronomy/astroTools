!> \file cal2unix.f90  Convert calendar date/time to UNIX time stamp  0 = 1/1/1970 = JD 2440587.5

!***********************************************************************************************************************************
!> \brief  Convert calendar date/time to UNIX time stamp  0 = 1/1/1970 = JD 2440587.5

program cal2unix
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays, r2h
  use SUFR_system, only: warn, syntax_quit
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms
  use SUFR_date_and_time, only: cal2jd, jd2gps, jd2unix, dow_ut
  use TheSky_datetime, only: calc_gmst
  
  implicit none
  real(double) :: d,s,time,jd
  integer :: Narg, y,mon,h,min
  
  ! Initialise astroTools:
  call astroTools_init()
  
  h = 0
  min = 0
  s = 0.d0
  
  Narg = command_argument_count()
  if(Narg.eq.3 .or. Narg.eq.6) then
     call get_command_argument_i(1, y)
     call get_command_argument_i(2, mon)
     call get_command_argument_d(3, d)
     
     if(Narg.eq.6) then
        call get_command_argument_i(4, h)
        call get_command_argument_i(5, min)
        call get_command_argument_d(6, s)
     end if
  else
     call syntax_quit('<yr  mon  day[.dd]>  [hr  min  sec.ss]',0,'Computes the Unix time for a given calendar date')
  end if
  
  time =  dble(h) + dble(min)/60.d0 + s/3600.d0
  d    =  d + time/24.d0
  jd   =  cal2jd(y,mon,d)
  
  write(*,*)
  write(*,'(A,F19.3)')            '    Unix time: ',  jd2unix(jd)
  write(*,*)
  write(*,'(A,5x,A,I7,I3,F7.3)')  '    Date:      ',  trim(endays(dow_ut(jd))),y,mon,d
  write(*,'(A,A13)')              '    UT:        ',  hms(time + 1.d-9)
  write(*,*)
  write(*,'(A,F20.7)')            '    JD:        ',  jd
  write(*,'(A,A13)')              '    GMST:      ',  hms(calc_gmst(jd)*r2h)
  write(*,'(A,A)')                '    GPS time:       '//dbl2str(jd2gps(jd), 3)
  write(*,*)
  
end program cal2unix
!***********************************************************************************************************************************

