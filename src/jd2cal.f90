!> \file jd2cal.f90  Compute the calendar date for a given Julian day


!***********************************************************************************************************************************
!> \brief  Compute the calendar date for a given Julian day

program jd2calendar
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants
  use SUFR_date_and_time, only: jd2cal
  use SUFR_time2string, only: hms_sss
  implicit none
  integer :: y,m
  real(double) :: d,jd,gmst,calcgmst
  character :: str*99
  call declconst
  
  if(iargc().eq.1) then
     call getarg(1,str)
     read(str,*) jd
  else
     write(*,'(/,A)')'  jd2cal:'
     write(*,'(A)')'    Computes the calendar date for a given julian date'
     write(*,'(A,/)')'    syntax: jd2cal <JD>'
     stop
  end if
  
  call set_SUFR_constants()
  
  call jd2cal(jd,y,m,d)
  gmst = calcgmst(jd)
  write(*,*)
  write(6,'(A,F20.7)')      '    JD:     ',jd
  write(6,'(A,I9,I3,F7.3)') '    Date:   ',y,m,d
  write(6,'(A,5x,A)')       '    UT:     ',hms_sss((d-floor(d))*24)
  write(6,'(A,5x,A)')       '    GMST:   ',hms_sss(gmst)
  write(*,*)
  
end program jd2calendar
!***********************************************************************************************************************************
