      !Convert galactic coordinates to equatorial.
      use constants
      implicit none
      real*8 :: d,m,s,dms2rad,ra,dec,l,b
      call declconst
      write(6,'(A36,$)')'Input galactic longitude (d,m,s): '
      read*,d,m,s
      l = dms2rad(d,m,s)
      write(6,'(A35,$)')'Input galactic latitude (d,m,s): '
      read*,d,m,s
      b = dms2rad(d,m,s)
      write(6,'(A30,F13.8)')'l:  ',l*r2d
      write(6,'(A30,F13.8)')'b:  ',b*r2d
      call gal2eq(l,b,ra,dec)
      write(6,'(A30,F13.8)')'ra:  ',ra*r2h
      write(6,'(A30,F13.8)')'dec:  ',dec*r2d
      
      end
