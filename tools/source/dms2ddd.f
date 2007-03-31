      implicit none
      real*8 :: o,pi,d,m,s
      pi = 4*datan(1.d0)
      write(6,'(A22,$)')'Input angle (d,m,s): '
      read*,d,m,s
      if(d.lt.0.d0) then
        m = -dabs(m)
	s = -dabs(s)
      endif
      o = d+m/60.d0+s/3600.d0
      write(6,'(30x,F13.10)')o
      end
