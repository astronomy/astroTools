      use constants
      implicit none
      real*8 :: o,d,m,s,dms2rad
      call declconst
      write(6,'(A22,$)')'Input angle (d,m,s): '
      read*,d,m,s
      write(6,'(30x,F13.10)')dms2rad(d,m,s)
      
      end
