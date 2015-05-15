!> \file cal2times.f90  Convert calendar date/time to different time formats

!***********************************************************************************************************************************
!> \brief  Convert calendar date/time to different time formats

program cal2times
  use SUFR_kinds, only: double
  use SUFR_constants, only: endays,endys, enmonths,enmntsb, r2h
  use SUFR_system, only: syntax_quit, system_time
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  use SUFR_text, only: dbl2str
  use SUFR_time2string, only: hms_sss
  use SUFR_date_and_time, only: cal2jd,jd2cal,jd2dtm,jd2time,ymdhms2jd, jd2gps,jd2unix, dow_ut, doy, leapyr
  use SUFR_numerics, only: dne0
  use TheSky_datetime, only: calc_gmst
  
  implicit none
  real(double) :: d,s,jd, tz, jd_lt,lt_d
  integer :: Narg, y,mon,h,min, dy, lt_y,lt_mon
  character :: ny(0:1)*(3)
  
  ! Initialise astroTools:
  call astroTools_init()
  ny(0:1) = ['No ','Yes']
  
  h = 0
  min = 0
  s = 0.d0
  tz = 0.d0
  
  write(*,*)
  Narg = command_argument_count()
  if(Narg.eq.0) then  ! Using system clock -> time = LT
     write(*,'(A,/)') '    Using time from the system clock'
     call system_time(y,mon,dy, h,min,s, tz)
  else if(Narg.eq.3 .or. Narg.eq.6 .or. Narg.eq.7) then
     call get_command_argument_i(1, y)
     call get_command_argument_i(2, mon)
     call get_command_argument_i(3, dy)
     
     if(Narg.ge.6) then
        call get_command_argument_i(4, h)
        call get_command_argument_i(5, min)
        call get_command_argument_d(6, s)
     end if
     
     tz = 0.d0  ! Assume UT
     if(Narg.eq.7) call get_command_argument_d(7, tz)  ! Time zone is specified -> time = LT
     
  else
     call syntax_quit('[<yr  mon  day[.dd]>  [hr  min  sec.ss [tz]]]',0,'Computes the GPS time for a given calendar date')
  end if
  
  jd = ymdhms2jd(y,mon,dy, h,min,s-tz*3600)  ! UT
  call jd2cal(jd, y,mon,d)
  jd_lt = jd + tz/24.d0                      ! LT
  call jd2cal(jd_lt, lt_y,lt_mon,lt_d)
  
  if(dne0(tz)) then  ! Using system clock; have LT and TZ
     write(*,'(A,I9,I3,F7.3)')       '    Date (LT): ', lt_y,lt_mon,lt_d
     write(*,'(A,A17)')              '    LT:        ', hms_sss(jd2time(jd_lt+1.d-9))
     write(*,'(A,F9.2)')              '    TZ:        ', tz
     write(*,*)
  end if
  write(*,'(A,I9,I3,F7.3)')       '    Date:      ', y,mon,d
  write(*,'(A,A17)')              '    Time:      ', hms_sss(jd2time(jd+1.d-9))
  
  write(*,*)
  write(*,'(A,I3,2(5x,A))')          '    Month:          ', mon, trim(enmntsb(mon)), trim(enmonths(mon))
  write(*,'(A,I3,2(5x,A))')          '    Day of week:    ', dow_ut(jd), trim(endys(dow_ut(jd))), trim(endays(dow_ut(jd)))
  write(*,'(A,I3)')               '    Day of year:    ', doy(jd)
  !write(*,'(A,I3)')               '    Week number:    ', woy(jd)
  
  write(*,*)
  write(*,'(A,A)')                '    Leap year:      ', ny(leapyr(y))
  
  
  
  write(*,*)
  write(*,'(A,F20.7)')            '    JD:        ', jd
  write(*,'(A,A17)')              '    GMST:      ', hms_sss(calc_gmst(jd)*r2h+1.d-9)
  write(*,*)  
  write(*,'(A,F19.3)')            '    Unix time: ', jd2unix(jd)
  write(*,'(A,A)')                '    GPS time:       '//dbl2str(jd2gps(jd), 3)
  write(*,*)
  
end program cal2times
!***********************************************************************************************************************************

