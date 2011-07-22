      !Precess ra and decl.
      use constants
      implicit none
      integer :: yr1,yr2
      real*8 :: h,d,m,s,jd1,jd2,ra1,dec1,ra2,dec2,cal2jd,rev
      character :: hmss*12,dmss2*14,hms*8
      
      
      call declconst()
      
      print*,'Give the starting position: '
      write(6,'(A16,$)')'  R.A. (h,m,s): '
      read*,h,m,s
      ra1 = (h+m/60.d0+s/3600.d0)/r2h
      
      write(6,'(A16,$)')' Decl. (d,m,s): '
      read*,d,m,s
      if(d.lt.0.d0) then
        m = -dabs(m)
	s = -dabs(s)
      endif
      dec1 = (d+m/60.d0+s/3600.d0)/r2d
      
      write(6,'(A25,$)')'Give the starting year: '
      read*,yr1
      jd1 = cal2jd(yr1,1,1)
      write(6,'(A25,$)')' Give the end year:     '
      read*,yr2
      
      
!      do yr2 = 2090,2110,1
      jd2 = cal2jd(yr2,1,1)
!      do jd2=2488863.5,2489003.5,1
      ra2 = ra1
      dec2 = dec1
      call preceseq(jd1,jd2,ra2,dec2)
      
      if(1.eq.1) then
        print*,''
        print*,''
        write(6,'(A17,I7,A8,F10.1,A12,A12,A10,A14)')'Input:    year: ',yr1,'JD: ',jd1,'R.A.: ',hmss(ra1*r2h),'decl.: ',dmss2(dec1)
        write(6,'(A17,I7,A8,F10.1,A12,A12,A10,A14)')'Output:   year: ',yr2,'JD: ',jd2,'R.A.: ',hmss(ra2*r2h),'decl.: ',dmss2(dec2)
      endif
      !print*,ra1,dec1,ra2,dec2
      
!      write(6,'(A17,I7,A8,F10.1,A12,A12,A10,A14,A20)')'Output:   year: ',yr2,'JD: ',jd2,'R.A.: ',hmss(ra2*r2h),'decl.: ',dmss2(dec2),dmss2(pi/2.d0-dec2)
!      print*,jd2,pi/2.d0-dec2
      
!      enddo
      
      print*,''
      end
      
      
      
      
