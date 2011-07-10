      !Convert equatorial coordinates to ecliptical.
      use constants
      implicit none
      real*8 :: d,h,m,s,ra,dec,l,b,dms2rad,eps
      call declconst
      eps = 0.409092804222d0 !J2000.0
      write(6,'(A27,$)')'Input longitude (d,m,s): '
      read*,d,m,s
      l = dms2rad(d,m,s)
      write(6,'(A26,$)')'Input latitude (d,m,s): '
      read*,d,m,s
      b = dms2rad(d,m,s)
      write(6,'(A30,F13.8)')'l:  ',l*r2d
      write(6,'(A30,F13.8)')'b:  ',b*r2d
      call lb2radec(l,b,eps,ra,dec)
      write(6,'(A30,F13.8)')'ra:  ',ra*r2h
      write(6,'(A30,F13.8)')'dec:  ',dec*r2d
      
      end
