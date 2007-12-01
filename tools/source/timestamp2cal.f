!Convert a timestamp (seconds since 1/1/1970) to calendar 

program timestamp2cal
  implicit none
  real*8 :: jd,s
  integer :: y,mo,d,h,mi,timestamp
  character :: str*99
  write(*,'(/,A)')' This programme converts a timestamp (seconds since 1/1/1970) to a calendar date and time (UT)'
  if(iargc().eq.0) then
     write(*,'(A)')'  Usage: timestamp2cal  <timestamp>'
     write(6,'(A,$)')'  Give timestamp: '
     read*,timestamp
  else
     call getarg(1,str)
     read(str,*)timestamp
  end if
  jd = dble(timestamp)/86400.d0 + 2440587.5d0
  call jd2dtm(jd,y,mo,d,h,mi,s)
  write(6,'(A,I22)')'    Timestamp:',timestamp
  write(6,'(A,F24.7)')'    JD:     ',jd
  write(6,'(A,I6.4,A1,2(I2.2,A1),3(I2.2,A1),/)')'    UT Date:   ',y,'/',mo,'/',d,' ',  h,':',mi,':',nint(s),' '
end program timestamp2cal
