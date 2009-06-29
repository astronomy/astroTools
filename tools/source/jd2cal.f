
program jd2calendar
  implicit none
  integer :: y,m
  real*8 :: d,jd,gmst,calcgmst
  character :: hms*8,str*99
  call declconst
  
  if(iargc().eq.1) then
     call getarg(1,str)
     read(str,*)jd
  else
     write(*,'(/,A)')'  jd2cal:'
     write(*,'(A)')'    Computes the calendar date for a given julian date'
     write(*,'(A,/)')'    syntax: jd2cal <JD>'
     stop
  end if
  
  call jd2cal(jd,y,m,d)
  gmst = calcgmst(jd)
  write(*,*)
  write(6,'(A,F20.7)')'    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,m,d
  write(6,'(A,A13)')       '    UT:     ',hms((d-floor(d))*24)
  write(6,'(A,A13)')       '    GMST:   ',hms(gmst)
  write(*,*)
end program jd2calendar


