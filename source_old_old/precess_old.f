      implicit none
      integer :: yr1,mo1,dy1,yr2,mo2,dy2
      real*8 :: o,pi,h,d,m,s,jd1,jd2,ra1,dec1,ra2,dec2,cal2jd
      character :: hmss*12,dmss2*14
      
      
      pi = 4.d0*datan(1.d0)
      print*,'Give the starting position: '
      write(6,'(A16,$)')'  R.A. (h,m,s): '
      read*,h,m,s
      ra1 = (h+m/60.d0+s/3600.d0)/12.d0*pi
      
      write(6,'(A16,$)')' Decl. (d,m,s): '
      read*,d,m,s
      if(d.lt.0.d0) then
        m = -dabs(m)
	s = -dabs(s)
      endif
      dec1 = (d+m/60.d0+s/3600.d0)/180.d0*pi
      
      write(6,'(A33,$)')' Give the starting date (y,m,d): '
      read*,yr1,mo1,dy1
      jd1 = cal2jd(yr1,mo1,dy1)
      
      write(6,'(A33,$)')' Give the end date (y,m,d):      '
      read*,yr2,mo2,dy2
      jd2 = cal2jd(yr2,mo2,dy2)
      
      ra2 = ra1
      dec2 = dec1
      call preceseq(jd1,jd2,ra2,dec2)
      
      
      write(6,'(A16,I6,2I3.2,A6,F10.1,A7,A12,A8,A14)')
     &  ' Input:   date: ',yr1,mo1,dy1,'  JD: ',jd1,
     &  ' R.A.: ',hmss(ra1),' decl.: ',dmss2(dec1)
      
      end
      
      
      
      
************************************************************************
      function cal2jd(yy,mm,dd)   !Input in UT
************************************************************************
      implicit none
      real*8 :: d,dd,cal2jd
      integer :: y,yy,m,mm,a,b,greg
      y=yy
      m=mm
      d=dd
      greg=0		!Julian or gregorian?
      if (y.gt.1582) greg=1
      if (y.eq.1582) then
        if (m.gt.10) greg=1
	if ((m.eq.10).and.(d.ge.15)) greg=1
      endif
      if (m.le.2) then 
         y = y-1
         m = m+12
      endif
      b = 0
      if(greg.eq.1) then	!For a Gregorian date
        a = int(y/100.d0)
        b = 2 - a + int(a/4.d0)
      endif
      cal2jd = int(365.25d0*(y+4716)) + int(30.6001d0*(m+1)) + 
     &           d + b - 1524.5d0
      return
      end function cal2jd
************************************************************************
      
      
      
************************************************************************
      subroutine preceseq(jd1,jd2,a1,d1) ! Precession in equat. coord. 
                                         ! (ra,dec) from jd1 to jd2
************************************************************************
      implicit none
      real*8 :: jd1,jd2,t1,t2,a1,d1
      real*8 :: dz,ze,th,a,b,c
      
      t1 = (jd1 - 2451545.d0)/36525.d0
      t2 = (jd2 - jd1)/36525.d0
      
      dz = (1.11808609d-2 + 6.770714d-6*t1 - 6.739d-10*t1*t1)*t2 
     & + (1.463556d-6 - 1.668d-9*t1)*t2*t2 + 8.725677d-8*t2**3
      ze = (1.11808609d-2 + 6.770714d-6*t1 - 6.739d-10*t1*t1)*t2 
     & + (5.307158d-6 + 3.20d-10*t1)*t2*t2 + 8.825063d-8*t2**3
      th = (9.71717346d-3 - 4.136915d-6*t1 - 1.0520d-9*t1*t1)*t2 
     & - (2.068458d-6 + 1.052d-9*t1)*t2*t2 - 2.028121d-7*t2**3
      
      a = dcos(d1)*dsin(a1+dz)
      b = dcos(th)*dcos(d1)*dcos(a1+dz) - dsin(th)*dsin(d1)
      c = dsin(th)*dcos(d1)*dcos(a1+dz) + dcos(th)*dsin(d1)

      a1 = datan2(a,b) + ze
      d1 = dasin(c)
      
      return
      end subroutine preceseq
************************************************************************
      
      
************************************************************************
      function hmss(t)   !Print time as string in hms.sss, input in hours
************************************************************************
      implicit none
      real*8 :: t,t1,rev,s,pi,r2h
      integer :: h,m
      character :: hmss*12,hh*2,mm*2,ss*6
            
      pi = 4*datan(1.d0)
      r2h = 12.d0/pi
      
      t1 = rev((t+1.d-10)/r2h)*r2h
      h = int(t1)
      m = int((t1-h)*60.d0)
      s = (t1-h-m/60.d0)*3600.d0
      
      if(s.gt.59.999d0) then
        s = s-60.d0
	m = m+1
      endif      
      if(m.ge.60) then
        m = m - 60
	h = h+1
      endif
      if(h.ge.24) h = h-24

      write(hh,'(i2.2)') h
      write(mm,'(i2.2)') m
      write(ss,'(f6.3)') s
      if(s.lt.10) write(ss,'(a1,f5.3)') '0',s

      write(hmss,'(2(a2,a1),a6)') hh,':',mm,':',ss
      if(t.eq.0.d0) write(hmss,'(a12)') '--:--:--.---'

      return
      end function hmss
************************************************************************
      
************************************************************************
      function dmss2(a1)   !Print angle as dms.ss string, 
                        !input in rad, output between -180 and +180
************************************************************************
      implicit none
      real*8 :: a1,a,rev2,s,r2d,pi
      integer :: d,m
      character :: dmss2*14,mm*2,ss*5,ddd*4,sig
      
      pi = 4*datan(1.d0)
      r2d = 180.d0/pi
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = int((a-d)*60.d0)
      s = (a-d-m/60.d0)*3600.d0
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(a1,i3.3)') sig,d
      write(mm,'(i2.2)') m
      write(ss,'(f5.2)') s
      if(s.lt.10) write(ss,'(a1,f4.2)') '0',s
      write(dmss2,'(a4,a1,a2,a1,a5,a1)') ddd,'°',mm,"'",ss,'"'
      return
      end function dmss2
************************************************************************
      
      
      
      
      
      
