!julian.f: <= 03/05/2003

program julian
  implicit none
  integer :: y,m,dow,doy,woy
  real*8 :: d
  character*9 :: wdays(0:6),months(12)
  
  data wdays/'Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'/
  data months/'January','February','March','April','May','June','July','August','September','October','November','December'/
  
  d = 26.0
  m = 12
  y = 1
  
  print*,wdays(dow(2003,5,d)),months(m),int(d),y
  print*,'Week number:',woy(y,m,d),'   Day of year:',doy(y,m,d)
  
end program julian


