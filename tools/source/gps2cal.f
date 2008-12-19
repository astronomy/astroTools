!Convert GPS time to calendar (1/1/2000 = 630720013.0, i.e. 13 leap seconds after 1/1/1980)

!************************************************************************
program gps2cal 
  use constants
  implicit none
  real*8 :: d,jd,gps,gmst,calcgmst,leapsecond
  integer :: y,m
  character :: hms*8,str*99
  call declconst
  
  if(iargc().eq.1) then
     call getarg(1,str)
     read(str,*)gps
  else
     write(*,'(/,A)')'  gps2cal:'
     write(*,'(A)')'    Computes the calendar date for a given GPS time'
     write(*,'(A,/)')'    syntax: gps2cal <gpstime>'
     stop
  end if
  
  leapsecond = 1.d0/86400.d0  !1s in days
  jd = (gps - 630720013.d0)/86400d0 + 2451544.5d0
  if(jd.lt.2451179.5d0) write(6,'(A)')'  WARNING: leap seconds are not taken into account before 1/1/1999!'
  if(jd.gt.2453736.5d0) jd = jd - leapsecond  !Leap second on 1/1/2006
  if(jd.gt.2454832.5d0) jd = jd - leapsecond  !Leap second on 1/1/2009
  
  call jd2cal(jd,y,m,d)
  gmst = calcgmst(jd)
  write(*,*)
  write(6,'(A,F18.3)')'    GPStime:',gps
  write(6,'(A,F20.7)')'    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,m,d
  write(6,'(A,A13)')       '    UT:     ',hms((d-floor(d))*24)
  write(6,'(A,A13)')       '    GMST:   ',hms(gmst)
  write(*,*)
end program gps2cal
!************************************************************************


      
