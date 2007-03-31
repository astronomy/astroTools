      use constants
      implicit none
      real*8 :: i,o!,pi
      character :: dmss2*14,dmmmmm2*12
      call declconst
!      pi = 4*datan(1.d0)
 10   write(6,'(A13,$)')'Input (deg): '
      read*,i
      o = i/180.d0*pi
      write(6,'(30x,F14.7,A15,A16)')i,dmmmmm2(o),dmss2(o)
!      goto 10
      end
