      implicit none
      real*8 :: d,jd,cal2jd
      integer :: y,m
      write(6,'(A23,$)')'Give year, month, day: '
      read*,y,m,d
      jd = cal2jd(y,m,d)
      write(6,'(30x,F20.8)')jd
      end
            
      
