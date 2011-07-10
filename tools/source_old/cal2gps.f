!Convert calendar date/time to GPS time (1/1/2000 = 630720013.0)

!************************************************************************
program cal2gps
  use constants
  implicit none
  real*8 :: d,s,time,jd,gps,gmst,unix,calcgmst,cal2jd
  integer :: y,mon,h,min
  character :: hms*8,str*99
  call declconst
  
  h = 0
  min = 0
  s = 0.d0
  if(iargc().eq.3.or.iargc().eq.6) then
     call getarg(1,str)
     read(str,*)y
     call getarg(2,str)
     read(str,*)mon
     call getarg(3,str)
     read(str,*)d
     
     if(iargc().eq.6) then
        call getarg(4,str)
        read(str,*)h
        call getarg(5,str)
        read(str,*)min
        call getarg(6,str)
        read(str,*)s
     end if
  else
     write(*,'(/,A)')'  cal2gps:'
     write(*,'(A)')'    Computes the GPS time for a given calendar date'
     write(*,'(A,/)')'    syntax: cal2gps <yr  mon  day[.dd]>  [hr  min  sec.ss]'
     stop
  end if
  time =  dble(h) + dble(min)/60.d0 + s/3600.d0
  d = d + time/24.d0
  jd = cal2jd(y,mon,d)
  gmst = calcgmst(jd)
  
  gps = (jd - 2451544.5d0)*86400.d0 + 630720013.d0
  if(jd.lt.2444239.5d0) write(6,'(A)')'  WARNING: leap seconds are not taken into account when computing GPS time before 1/1/1980!'
  !if(jd.lt..5d0) gps = gps - 1  !Leap second on 1//19
  if(jd.lt.2444786.5d0) gps = gps - 1  !Leap second on 1/7/1981
  if(jd.lt.2445151.5d0) gps = gps - 1  !Leap second on 1/7/1982
  if(jd.lt.2445516.5d0) gps = gps - 1  !Leap second on 1/7/1983
  if(jd.lt.2446247.5d0) gps = gps - 1  !Leap second on 1/7/1985
  if(jd.lt.2447161.5d0) gps = gps - 1  !Leap second on 1/1/1988
  if(jd.lt.2447892.5d0) gps = gps - 1  !Leap second on 1/1/1990
  if(jd.lt.2448257.5d0) gps = gps - 1  !Leap second on 1/1/1991
  if(jd.lt.2448804.5d0) gps = gps - 1  !Leap second on 1/7/1992
  if(jd.lt.2449169.5d0) gps = gps - 1  !Leap second on 1/7/1993
  if(jd.lt.2449534.5d0) gps = gps - 1  !Leap second on 1/7/1994
  if(jd.lt.2450083.5d0) gps = gps - 1  !Leap second on 1/1/1996
  if(jd.lt.2450630.5d0) gps = gps - 1  !Leap second on 1/7/1997
  if(jd.lt.2451179.5d0) gps = gps - 1  !Leap second on 1/1/1999
  if(jd.ge.2453736.5d0) gps = gps + 1  !Leap second on 1/1/2006
  if(jd.ge.2454832.5d0) gps = gps + 1  !Leap second on 1/1/2009
  
  unix = (jd - 2440587.5d0)*86400
  
  write(*,*)
  write(6,'(A,F18.3)')'    GPStime:',gps
  write(6,'(A,F20.7)')'    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,mon,d
  write(6,'(A,A13)')       '    GMT:    ',hms((d-floor(d))*24)
  !write(6,'(A,A13)')       '    GMT:    ',hms(time)
  write(6,'(A,A13)')       '    GMST:   ',hms(gmst)
  write(6,'(A,F18.3)')       '    Unix:    ',unix
  write(*,*)
end program cal2gps
!************************************************************************


      
