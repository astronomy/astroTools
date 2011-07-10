!Convert UNIX time stamp to calendar 0 = 1/1/1970 = JD 2440587.5

!************************************************************************
program unix2cal  
  implicit none
  real*8 :: d,jd,unix,tz
  integer :: y,m,narg
  character :: hms*8,bla*99
  call declconst
  
  tz = 0.d0
  narg = iargc()
  if(narg.ge.1) then
     call getarg(1,bla)
     read(bla,*)unix
     if(narg.ge.2) then
        call getarg(2,bla)
        read(bla,*)tz
     end if
  else
     write(6,'(/,A)')'  This program computes the calendar date and time for a given unix time stamp.'
     write(6,'(A,/)')'  Syntax:  unix2cal <unix time stamp>  [<time zone> (>0: east)]'
     stop
  end if
  jd = unix/86400d0 + 2440587.5d0 + tz/24.d0
  call jd2cal(jd,y,m,d)
  write(6,'(A,F18.3)')'    UNIX time:',unix
  write(6,'(A,F20.7)')'    JD:      ',jd
  if(dabs(tz).lt.0.1) then
     write(6,'(A,I9,2I3,A)')'    Date:    ',y,m,floor(d),' UT'
     write(6,'(A,A13,A)')'    Time:    ',hms((d-floor(d))*24),' UT'
  else
     write(6,'(A,I9,2I3)')'    Date:    ',y,m,floor(d)
     write(6,'(A,A13)')'    Time:    ',hms((d-floor(d))*24)
  end if
end program unix2cal
!************************************************************************


      
