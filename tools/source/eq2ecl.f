      !Convert equatorial coordinates to ecliptical.
      use constants
      implicit none
      real*8 :: d,h,m,s,ra,dec,l,b,hms2rad,dms2rad,eps
      call declconst
      eps = 0.409092804222d0 !J2000.0
      write(6,'(A33,$)')'Input right ascension (h,m,s): '
      read*,h,m,s
      ra = hms2rad(h,m,s)
      write(6,'(A29,$)')'Input declination (d,m,s): '
      read*,d,m,s
      dec = dms2rad(d,m,s)
      write(6,'(A30,F13.8)')'ra:  ',ra*r2h
      write(6,'(A30,F13.8)')'dec:  ',dec*r2d
      call radec2lb(ra,dec,eps,l,b)
      write(6,'(A30,F13.8)')'l:  ',l*r2d
      write(6,'(A30,F13.8)')'b:  ',b*r2d
      end
