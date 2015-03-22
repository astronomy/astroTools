      !Convert equatorial coordinates to galactic.
      use constants
      implicit none
      real*8 :: d,h,m,s,dms2rad,hms2rad,ra,dec,l,b
      call declconst
      write(6,'(A33,$)')'Input right ascension (h,m,s): '
      read*,h,m,s
      ra = hms2rad(h,m,s)
      write(6,'(A29,$)')'Input declination (d,m,s): '
      read*,d,m,s
      dec = dms2rad(d,m,s)
      write(6,'(A30,F13.8)')'ra:  ',ra*r2h
      write(6,'(A30,F13.8)')'dec:  ',dec*r2d
      call eq2gal(ra,dec,l,b)
      write(6,'(A30,F13.8)')'l:  ',l*r2d
      write(6,'(A30,F13.8)')'b:  ',b*r2d
      
      call gal2eq(l,b,ra,dec)
      write(6,'(A30,F13.8)')'ra:  ',ra*r2h
      write(6,'(A30,F13.8)')'dec:  ',dec*r2d
      
      end
