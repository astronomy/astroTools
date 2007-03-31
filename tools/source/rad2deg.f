      implicit none
      real*8 :: i,o,pi
      pi = 4.d0*datan(1.d0)
 10   write(6,'(A13,$)')'Input (rad): '
      read*,i
      o = i*180.d0/pi
      write(6,*)'                             ',o
      goto 10
      end
