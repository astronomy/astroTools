!Convert (LIGO?) GPS time to calendar (1/1/2000 = 630720013.0)

!************************************************************************
program gps2cal 
  use constants
  implicit none
  real*8 :: d,jd,gps,gmst,calcgmst
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
  !jd = (gps - 630720013.d0)/86400d0 + 2451545.d0
  jd = (gps - 630720013.d0)/86400d0 + 2451544.5d0
  call jd2cal(jd,y,m,d)
  gmst = calcgmst(jd)
  write(*,*)
  write(6,'(A,F18.3)')'    GPStime:',gps
  write(6,'(A,F20.7)')'    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,m,d
  write(6,'(A,A13)')       '    GMT:    ',hms((d-floor(d))*24)
  write(6,'(A,A13)')       '    GMST:   ',hms(gmst)
  write(*,*)
end program gps2cal
!************************************************************************


      
