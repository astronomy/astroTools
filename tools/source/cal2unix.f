!Convert calendar date/time to UNIX time stamp  0 = 1/1/1970 = JD 2440587.5

!************************************************************************
program unix2cal  
  implicit none
  real*8 :: d,jd,unix,tz,time,cal2jd
  integer :: y,mo,narg,h,mi,s
  character :: hms*8,bla*99
  call declconst
  
  tz = 0.d0
  h = 0
  mi = 0
  s = 0
  narg = iargc()
  if(narg.ge.3) then
     call getarg(1,bla)
     read(bla,*)y
     call getarg(2,bla)
     read(bla,*)mo
     call getarg(3,bla)
     read(bla,*)d
     if(narg.ge.4) then
        call getarg(4,bla)
        read(bla,*)h
     end if
     if(narg.ge.5) then
        call getarg(5,bla)
        read(bla,*)mi
     end if
     if(narg.ge.6) then
        call getarg(6,bla)
        read(bla,*)s
     end if
     if(narg.ge.7) then
        call getarg(7,bla)
        read(bla,*)tz
     end if
  else
     write(6,'(/,A)')'  This program computes the unix time stamp for a given calendar date and time.'
     write(6,'(A,/)')'  Syntax:  unix2cal <year> <month> <day> [<hour> [<minute> [<second>  [<time zone> (>0: east)]]]]'
     stop
  end if
  !jd = unix/86400d0 + 2440587.5d0 + tz/24.d0
  !call jd2cal(jd,y,m,d)
  time = dble(h) + dble(mi)/60.d0 + dble(s)/3600.d0
  d = d + (time-tz)/24.d0
  jd = cal2jd(y,mo,d)
  unix = (jd - 2440587.5d0)*86400
  if(dabs(tz).lt.0.1) then
     write(6,'(A,I9,2I3,A)')'    Date:    ',y,mo,floor(d),' UT'
     write(6,'(A,A13,A)')'    Time:    ',hms((d-floor(d))*24),' UT'
  else
     write(6,'(A,I9,2I3)')'    Date:    ',y,mo,floor(d)
     write(6,'(A,A13)')'    Time:    ',hms((d-floor(d))*24)
  end if
  write(6,'(A,F20.7)')'    JD:      ',jd
  write(6,'(A,F18.3)')'    UNIX time: ',unix
end program unix2cal
!************************************************************************


      
