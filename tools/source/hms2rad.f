      use constants
      implicit none
      real*8 :: o,h,m,s,hms2rad
      call declconst
      write(6,'(A18,$)')'Input RA (h,m,s): '
      read*,h,m,s
      write(6,'(30x,F13.10)')hms2rad(h,m,s)
      end
