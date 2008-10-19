      !Convert (LIGO?) GPS time to calendar (1/1/2000 = 630720013.0)

!************************************************************************
program gps2cal  
  implicit none
  real*8 :: d,jd,gps,gmst,calcgmst
  integer :: y,m
  character :: hms*8
  write(6,'(A,$)')'  Give GPStime: '
  read*,gps
  !jd = (gps - 630720013.d0)/86400d0 + 2451545.d0
  jd = (gps - 630720013.d0)/86400d0 + 2451544.5d0
  call jd2cal(jd,y,m,d)
  gmst = calcgmst(jd)
  write(6,'(A,F18.3)')'    GPStime:',gps
  write(6,'(A,F20.7)')'    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,m,d
  print*,hms(gmst)
end program gps2cal
!************************************************************************


      
