************************************************************************
      module constants
************************************************************************
      implicit none
      save
      integer, parameter :: deltatn=441
      real*8 :: pi,pi2,r2d,d2r,r2h,h2r,d2as,as2d,r2as,as2r
      real*8 :: au,rsun,msun,jd2000,deltatvals(deltatn),deltatyrs(deltatn),nutationdat(9,63)
      integer :: mlen(12)
      character*9 :: months(12),monthsm(12),nlmonths(12),nlmonthsb(12),nlmnts(12)*3
      character*9 :: days(0:6),nldays(0:6),nlds(0:6)*2
      character :: phases(0:3)*13,nlphases(0:3)*16
      character :: vsopdir*43 !Local pc, www-server
      end module constants
************************************************************************

************************************************************************
      subroutine declconst
************************************************************************
      use constants
      implicit none
      
      vsopdir = '/home/sluys/diverse/popular/fortran/VSOP87/'		!Local pc
!      vsopdir = '/home/strknd/sluys/public_html/Hemel/vsopd/'    	!www-server
      
      pi  = 4.d0*datan(1.d0)
      pi2 = 8.d0*datan(1.d0)
      
      r2d = 180.d0/pi
      d2r = pi/180.d0
      r2h = 12.d0/pi
      h2r = pi/12.d0
      
      d2as = 3600.d0
      as2d = 1/3600.d0
      r2as = 180.d0*3600.d0/pi
      as2r = pi/(180.d0*3600.d0)
      
      au = 1.4959787d13
      rsun = 6.9599d10
      msun = 1.9891d33
      
      jd2000 = 2451545.d0

      months=(/'January  ','February ','March    ','April    ',
     &'May      ','June     ','July     ','August   ','September',
     &'October  ','November ','December '/)
      monthsm=(/'january  ','february ','march    ','april    ',
     &'may      ','june     ','july     ','august   ','september',
     &'october  ','november ','december '/)
      nlmonths=(/'januari  ','februari ','maart    ','april    ',
     &'mei      ','juni     ','juli     ','augustus ','september',
     &'oktober  ','november ','december '/)
      nlmonthsb=(/'Januari  ','Februari ','Maart    ','April    ',
     &'Mei      ','Juni     ','Juli     ','Augustus ','September',
     &'Oktober  ','November ','December '/)
      nlmnts = (/'jan','feb','mrt','apr','mei','jun','jul','aug','sep',
     &           'okt','nov','dec'/)
      
      mlen = (/31,28,31,30,31,30,31,31,30,31,30,31/)
     
      days=(/'Sunday   ','Monday   ','Tuesday  ','Wednesday',
     &       'Thursday ','Friday   ','Saturday '/)
      nldays=(/'zondag   ','maandag  ','dinsdag  ','woensdag ',
     &         'donderdag','vrijdag  ','zaterdag '/)
      nlds=(/'zo','ma','di','wo','do','vr','za'/)
      
      phases = (/'New Moon     ','First Quarter','Full Moon    ',
     &          'Last Quarter '/)
      nlphases = (/'Nieuwe Maan     ','Eerste Kwartier ',
     &           'Volle Maan      ','Laatste Kwartier'/)
      
      end subroutine declconst
************************************************************************
      
      
************************************************************************
      function asep(l1,l2,b1,b2)   !Calculates the angular separation 
                                   !  between two objects
************************************************************************
      use constants
      real*8 :: asep,l1,l2,b1,b2,dl,db,b,rev2
      dl = rev2(l2-l1)
      db = rev2(b2-b1)
      b  = rev2((b1+b2)/2.d0)
      
      asep = dacos(dsin(b1)*dsin(b2) + dcos(b1)*dcos(b2)*dcos(dl))
      if(asep.lt.3.d-3) asep = dsqrt((dl*dcos(b))**2 + db**2)
      return
      end function asep
************************************************************************
      
      
      
************************************************************************
      function pastr(pa1)   !Converts PA (rad) to string (NE, SW)
************************************************************************
      use constants
      implicit none
      real*8 :: pa1,pa,rev
      character :: pastr*2,pas(9)*2
      pas = (/' N','NE',' E','SE',' S','SW',' W','NW',' N'/)
      pastr = pas(ceiling(rev(pa1)*r2d/360.*8.+.5))
      return
      end function pastr
************************************************************************
    
      
      
      
************************************************************************
      function pastrnl(pa1)   !Converts PA (rad) to full Dutch string
************************************************************************
      use constants
      implicit none
      real*8 :: pa1,pa,rev
      character :: pastrnl*11,pas(9)*11
      pas = (/'    noorden','noordoosten','     oosten',
     &      ' zuidoosten','     zuiden',' zuidwesten','     westen',
     &      'noordwesten','    noorden'/)
      pastrnl = pas(ceiling(rev(pa1)*r2d/360.*8.+.5))
      return
      end function pastrnl
************************************************************************
    
      
      
      
************************************************************************
      function rev(x)        !Returns angle in radians between 0 and 2pi
************************************************************************
      use constants
      real*8 :: x,rev
      rev = x-floor(x/(2*pi))*2*pi
      return
      end function rev
************************************************************************
    
      
      
      
************************************************************************
      function rev2(x)      !Returns angle in radians between -pi and pi
************************************************************************
      use constants
      real*8 :: x,rev2
      rev2 = x-floor(x/(2*pi))*2*pi
      if(rev2.gt.pi) rev2 = rev2 - 2*pi
      return
      end function rev2
************************************************************************
    
      
      
      
************************************************************************
      function revc(x,c)      !Returns angle in radians between c-pi and 
                              !  c+pi, c is the 'central value'
************************************************************************
      use constants
      real*8 :: x,c,revc
      revc = x-floor(x/(2*pi))*2*pi
      if(c.ge.pi.and.revc.lt.c-pi) revc = revc + 2*pi
      if(c.lt.pi.and.revc.gt.c+pi) revc = revc - 2*pi
      return
      end function revc
************************************************************************
    
      
      
      
************************************************************************
      function rv(x)        !Returns time in hours between 0 and 24
************************************************************************
      real*8 :: x,rv
      rv = x-floor(x/24.d0)*24
      return
      end function rv
************************************************************************
      
      
************************************************************************
      function rv12(x)        !Returns time in hours between -12 and 12
************************************************************************
      real*8 :: x,rv12
      rv12 = x-floor(x/24.d0)*24
      if(rv12.gt.12.d0) rv12 = rv12 - 24.d0
      return
      end function rv12
************************************************************************
      
      
      
      
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
        a = floor(y/100.d0)
        b = 2 - a + floor(a/4.d0)
      endif
      cal2jd = floor(365.25d0*(y+4716)) + floor(30.6001d0*(m+1)) + d + b - 1524.5d0
      return
      end function cal2jd
************************************************************************
      
      
      
      
************************************************************************
      function tzstr(tz)                   !Get a 4-character TZ string
************************************************************************
      implicit none
      real*8 :: tz
      character :: tzstr*4
      
      if(tz.eq.1.d0) tzstr = ' MET'
      if(tz.eq.2.d0) tzstr = 'MEZT'
      return
      end function tzstr
************************************************************************
      
      
      
************************************************************************
      function hms(t)    !Print time as hh:mm:ss string, input in hours
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rev
      integer :: h,m,s
      character :: hms*8,hh*2,mm*2,ss*2
            
      t1 = rev(t*h2r)*r2h
      h = int(t1)
      m = int((t1-h)*60.d0)
      s = nint((t1-h-m/60.d0)*3600.d0)
      
      if(s.ge.60) then
        s = s-60
	m = m+1
      endif      
      if(m.ge.60) then
        m = m-60
	h = h+1
      endif
      if(h.ge.24) h = h-24

      write(hh,'(i2.2)') h
      write(mm,'(i2.2)') m
      write(ss,'(i2.2)') s

      write(hms,'(a2,2(a1,a2))') hh,':',mm,':',ss
      if(t.eq.0.d0) write(hms,'(a8)') '--:--:--'

      return
      end function hms
************************************************************************
      
      
      
      
************************************************************************
      function hmss(t)   !Print time as string in hms.sss, input in hours
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rev,s
      integer :: h,m
      character :: hmss*12,hh*2,mm*2,ss*6
      t1 = rev((t+1.d-10)*h2r)*r2h
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
      function hmm(t)     !Print time as string in hm.m, input in hours
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rev,m
      integer :: h
      character :: hmm*7,hh*2,mm*4
            
      t1 = rev(t*h2r)*r2h
      h = int(t1)
      m = (t1-h)*60.d0
      
      if(m.eq.60) then
        m=0
	h=h+1
      endif
      if(h.eq.24) h=0

      write(hmm,'(i2.2,a1,f4.1)') h,':',m
      if(m.lt.10.) write(hmm,'(i2.2,a2,f3.1)') h,':0',m
      if(t.eq.0.d0) write(hmm,'(a7)') '--:--.-'
      return
      end function hmm
************************************************************************
      
      
      
      
************************************************************************
      function hm(t)     !Print time as string in hh:mm, input in hours
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rev
      integer :: h,m
      character :: hm*5,hh*2,mm*2
            
      t1 = rev(t*h2r)*r2h
      h = int(t1)
      m = nint((t1-h)*60.d0)

      if(m.eq.60) then
        m=0
	h=h+1
      endif
      if(h.eq.24) h=0
      
      write(hh,'(i2)') h
      write(mm,'(i2)') m
      if(h.lt.10) write(hh,'(a1,i1)') '0',h
      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      
      write(hm,'(a2,2(a1,a2))') hh,':',mm
      if(t.eq.0.d0) write(hm,'(a5)') '--:--'

      return
      end function hm
************************************************************************
      
      
      
      
************************************************************************
      function hm2(t)     !Print time as string in hh:mm, input in hours, between -12 and 12
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rv12
      integer :: h,m
      character :: hm2*6,hh*2,mm*2,sign
            
      t1 = dabs(rv12(t))
      h = int(t1)
      m = nint((t1-h)*60)
      sign = '+'
      if(rv12(t).lt.0.d0) sign = '-'

      if(m.eq.60) then
        m=0
	h=h+1
      endif
      
      write(hh,'(i2)') h
      write(mm,'(i2)') m
      if(h.lt.10) write(hh,'(a1,i1)') '0',h
      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      
      write(hm2,'(a1,a2,2(a1,a2))') sign,hh,':',mm
      if(t.eq.0.d0) write(hm2,'(a6)') '---:--'

      return
      end function hm2
************************************************************************
      
      
      
      
************************************************************************
      function hdm(t)   !Print time as a nice string in hh.mm, so with a
                        ! . in stead of :  Input in hours
************************************************************************
      use constants
      implicit none
      real*8 :: t,t1,rev
      integer :: h,m
      character :: hdm*5,hh*2,mm*2
            
      t1 = rev(t*h2r)*r2h
      h = int(t1)
      m = nint((t1-h)*60.d0)

      if(m.eq.60) then
        m=0
	h=h+1
      endif
      if(h.eq.24) h=0
      
      write(hh,'(i2)') h
      write(mm,'(i2)') m
      if(h.lt.10) write(hh,'(a1,i1)') '0',h
      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      
      write(hdm,'(a2,2(a1,a2))') hh,'.',mm
      if(t.eq.0.d0) write(hdm,'(a5)') '--.--'

      return
      end function hdm
************************************************************************
      
      
      
      
************************************************************************
      function prs(a1)    !Print separation angle (>0) as ddd.mm or mm.ss string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev
      integer :: d,m,s
      character :: prs*7,mm*2,ss*2,ddd*3
      
      a = a1
      a = rev(a)*r2d
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(i3.3)') d
      write(mm,'(i2.2)') m
      write(ss,'(i2.2)') s

      if(d.gt.0) write(prs,'(a3,a1,a2,a1)') ddd,'°',mm,"'"
      if(d.eq.0) write(prs,'(a3,a1,a2,a1)') mm,"'",ss,'"'

      return
      end function prs
************************************************************************
      
      
************************************************************************
      function dms(a1)    !Print angle as ddd.mm.ss string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev
      integer :: d,m,s
      character :: dms*10,mm*2,ss*2,ddd*3
      
      a = a1
      a = rev(a)*r2d
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(i3.3)') d
      write(mm,'(i2.2)') m
      write(ss,'(i2.2)') s

      write(dms,'(a3,2(a1,a2),a1)') ddd,'°',mm,"'",ss,'"'

      return
      end function dms
************************************************************************
      
      
************************************************************************
      function dmss(a1)   !Print angle as a ddd.mm.ss.ss string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev,s
      integer :: d,m
      character :: dmss*13,mm*2,ss*5,ddd*3
      
      a = a1
      a = rev(a)*r2d
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
      
      write(ddd,'(i3.3)') d
      write(mm,'(i2.2)') m
      write(ss,'(f5.2)') s
      if(s.lt.10) write(ss,'(a1,f4.2)') '0',s

      write(dmss,'(a3,a1,a2,a1,a5,a1)') ddd,'°',mm,"'",ss,'"'

      return
      end function dmss
************************************************************************
      
      
************************************************************************
      function dms2(a1)   !Print angle as dms string, 
                        !input in rad, output between -180 and +180
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m,s
      character :: dms2*11,mm*2,ss*2,ddd*4,sig
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
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
      write(ss,'(i2.2)') s
      write(dms2,'(a4,2(a1,a2),a1)') ddd,'°',mm,"'",ss,'"'
      return
      end function dms2
************************************************************************
      
      
************************************************************************
      function ddms2(a1)   !Print angle as dd.mm.ss string, 
                           !input in rad, output between -99 and +99 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m,s
      character :: ddms2*10,sig!,mm*2,ss*2,dd*4
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddms2,'(a1,i2.2,2(a1,i2.2),a1)') sig,d,'°',m,"'",s,'"'
      return
      end function ddms2
************************************************************************
      
      
************************************************************************
      function d1ms2(a1)   !Print angle as d.mm.ss string, 
                           !input in rad, output between -9 and +9 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m,s
      character :: d1ms2*9,sig
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(d1ms2,'(a1,i1.1,2(a1,i2.2),a1)') sig,d,'°',m,"'",s,'"'
      return
      end function d1ms2
************************************************************************
      
      
************************************************************************
      function dmss2(a1)   !Print angle as dms.ss string, 
                        !input in rad, output between -180 and +180
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2,s
      integer :: d,m
      character :: dmss2*14,mm*2,ss*5,ddd*4,sig
      
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
      
      
************************************************************************
      function dm(a1)       !Print angle as ddd:mm string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev
      integer :: d,m
      character :: dm*7,mm*2,ddd*3
      
      a = a1
      a = rev(a)*r2d
      d = int(a)
      m = nint((a-d)*60.d0)
      
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(i3)') d
      write(mm,'(i2.2)') m
!      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      write(dm,'(a3,a1,a2,a1)') ddd,'°',mm,"'"
      return
      end function dm
************************************************************************
      
      
************************************************************************
      function dm2(a1)   !Print angle as ddd.mm string, 
                        !input in rad, output between -180 and +180
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m
      character :: dm2*8,mm*2,ddd*4,sig
      
      a = a1
      a = rev2(a)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = nint((a-d)*60.d0)
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(a1,i3.3)') sig,d
      write(mm,'(i2.2)') m
      write(dm2,'(a4,a1,a2,a1)') ddd,'°',mm,"'"

      return
      end function dm2
************************************************************************
      
      
      
************************************************************************
      function dmm2(a1)   !Print angle as dd.mm string, 
                           !input in rad, output between -99 and +99 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m
      character :: dmm2*7,mm*2,dd*3,sig
      
      a = a1
      a = rev2(a)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = nint((a-d)*60.d0)
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(dd,'(a1,i2.2)') sig,d
      write(mm,'(i2.2)') m
      write(dmm2,'(a3,a1,a2,a1)') dd,'°',mm,"'"
      return
      end function dmm2
************************************************************************
      
      
      
************************************************************************
      function dmmm2(a1)   !Print angle as dd:mm.m string, 
                           !input in rad, output between -99 and +99 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2,m
      integer :: d
      character :: dmmm2*9,mm*4,dd*3,sig
      
      a = a1
      a = rev2(a)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = (a-d)*60.d0
      if(m.ge.59.95d0) then
        d = d+1
	m = dabs(m-60d0)
      endif
      
      write(dd,'(a1,i2.2)') sig,d
      write(mm,'(F4.1)') m
      if(m.lt.10.d0) write(mm,'(I1,F3.1)') 0,m
      write(dmmm2,'(a3,a1,a4,a1)') dd,'°',mm,"'"
      return
      end function dmmm2
************************************************************************
      
      
      
************************************************************************
      function dmmmmm2(a1)   !Print angle as dd:mm.mmm string (for gps), 
                           !input in rad, output between -180 and +180 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2,m
      integer :: d
      character :: dmmmmm2*12,mm*6,dd*4,sig
      
      a = a1
      a = rev2(a)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = (a-d)*60.d0
      if(m.ge.59.95d0) then
        d = d+1
	m = dabs(m-60d0)
      endif
      
      write(dd,'(a1,i3.3)') sig,d
      write(mm,'(F6.3)') m
      if(m.lt.10.d0) write(mm,'(I1,F5.3)') 0,m
      write(dmmmmm2,'(a4,a1,a6,a1)') dd,'°',mm,"'"
      return
      end function dmmmmm2
************************************************************************
      
      
      
************************************************************************
      function ddd2(a1)   !Print angle as dd.d string, 
                           !input in rad, output between -99.9 and +99.9 !!!
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      character :: ddd2*6,sig
      
      a = rev2(a1)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -a
      endif
      
      write(ddd2,'(A1,F4.1,A1)') sig,a,'°'
      if(a.lt.10.d0) write(ddd2,'(A1,I1,F3.1,A1)') sig,0,a,'°'
      return
      end function ddd2
************************************************************************
      
      
      
************************************************************************
      function wdms(a1)    !Print angle as ddd.mm.ss string, input in rad with &deg; rather than °
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev
      integer :: d,m,s
      character :: wdms*14,mm*2,ss*2,ddd*3
      
      a = a1
      a = rev(a)*r2d
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      if(s.eq.60) then
        m = m+1
	s = 0
      endif
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(i3.3)') d
      write(mm,'(i2.2)') m
      write(ss,'(i2.2)') s

      write(wdms,'(a3,a5,a2,a1,a2,a1)') ddd,'&deg;',mm,"'",ss,'"'

      return
      end function wdms
************************************************************************
      
      
************************************************************************
      function wdms2(a1)   !Print angle as dms string, 
                        !input in rad, output between -180 and +180 with &deg; rather than °
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m,s
      character :: wdms2*15,mm*2,ss*2,ddd*4,sig
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
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
      write(ss,'(i2.2)') s
      write(wdms2,'(a4,a5,a2,a1,a2,a1)') ddd,'&deg;',mm,"'",ss,'"'
      return
      end function wdms2
************************************************************************
      
      
************************************************************************
      function wdm(a1)       !Print angle as ddd:mm string, input in rad with &deg; rather than °
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev
      integer :: d,m
      character :: wdm*11,mm*2,ddd*3
      
      a = a1
      a = rev(a)*r2d
      d = int(a)
      m = nint((a-d)*60.d0)
      
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(i3)') d
      write(mm,'(i2.2)') m
!      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      write(wdm,'(a3,a5,a2,a1)') ddd,'&deg;',mm,"'"
      return
      end function wdm
************************************************************************
      
      
************************************************************************
      function wdm2(a1)   !Print angle as ddd.mm string, 
                        !input in rad, output between -180 and +180 with &deg; rather than °
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2
      integer :: d,m
      character :: wdm2*12,mm*2,ddd*4,sig
      
      a = a1
      a = rev2(a)*r2d
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      d = int(a)
      m = nint((a-d)*60.d0)
      if(m.eq.60) then
        d = d+1
	m = 0
      endif
      
      write(ddd,'(a1,i3.3)') sig,d
      write(mm,'(i2.2)') m
      write(wdm2,'(a4,a5,a2,a1)') ddd,'&deg;',mm,"'"

      return
      end function wdm2
************************************************************************
      
      
      
      
************************************************************************
      function ams(a1)   !Print angle as mm:ss.s string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev,s
      integer :: m
      character :: ams*8,mm*2,ss*4
      
      a = a1
      a = rev(a)*r2d
      m = int((a)*60.d0)
      s = (a-m/60.d0)*3600.d0
            
      write(mm,'(i2.2)') m
      write(ss,'(f4.1)') s
      if(s.lt.10.d0) write(ss,'(a1,f3.1)') '0',s
      write(ams,'(a2,a1,a4,a1)') mm,"'",ss,'"'
      
      return
      end function ams
************************************************************************
      
      
************************************************************************
      function ams2(a1)    !Print angle as mm:ss.s string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev2,s
      integer :: m
      character :: ams2*9,mm*2,ss*4,sig
      
      a = a1
      a = rev2(a)*r2d
      
      sig = '+'
      if(a.lt.0.d0) then
        sig = '-'
	a = -1.d0*a
      endif
      
      m = int((a)*60.d0)
      s = (a-m/60.d0)*3600.d0
            
      write(mm,'(i2.2)') m
      write(ss,'(f4.1)') s
      if(nint(s).lt.10) write(ss,'(a1,f3.1)') '0',s
      write(ams2,'(a1,a2,a1,a4,a1)') sig,mm,"'",ss,'"'
      
      return
      end function ams2
************************************************************************
      
      
************************************************************************
      function amss(a1)   !Print angle as mm:ss.ss string, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev,s
      integer :: m
      character :: amss*9,mm*2,ss*5
      
      a = a1
      a = rev(a)*r2d
      m = int((a)*60.d0)
      s = (a-m/60.d0)*3600.d0
            
      write(mm,'(i2.2)') m
      write(ss,'(f5.2)') s
      if(s.lt.10.d0) write(ss,'(a1,f4.2)') '0',s
      write(amss,'(a2,a1,a5,a1)') mm,"'",ss,'"'
      
      return
      end function amss
************************************************************************
      
      
************************************************************************
      function ass(a1)   !Print angle as a string of ss.s, input in rad
************************************************************************
      use constants
      implicit none
      real*8 :: a1,a,rev,s
      integer :: m
      character :: ass*5
      
      a = a1
      a = rev(a)*r2d
      m = int((a)*60.d0)
      s = (a-m/60.d0)*3600.d0
            
      write(ass,'(f4.1,a1)') s,'"'
      
      return
      end function ass
************************************************************************
      
      
************************************************************************
      function ds(d)   !Print AU distance as a nice string
************************************************************************
      use constants
      implicit none
      real*8 :: d
      character :: ds*11
      write(ds,'(f11.8)') d
      if(d.lt.0.01d0) write(ds,'(f11.4)') d*au/1.d5  !For the Moon
      return
      end function ds
************************************************************************
      
      
************************************************************************
      function ds1(d)   !Print AU distance as a nice, smaller string
************************************************************************
      use constants
      implicit none
      real*8 :: d
      character :: ds1*9
      write(ds1,'(f9.6)') d
      if(d.lt.0.01d0) write(ds1,'(f9.2)') d*au/1.d5  !For the Moon
      return
      end function ds1
************************************************************************
      
      


************************************************************************
      function prmag(m) !Print magnitude nicely, rounded to 1 digit
************************************************************************
      implicit none
      real*8 :: m
      integer :: pl
      character :: prmag*5
      
      write(prmag,'(f5.1)')m
      return
      end function prmag
************************************************************************


************************************************************************
      function timestamp()  !Get time stamp in seconds since 1970-01-01 00:00:00 UTC
************************************************************************
      implicit none
      real*8 :: timestamp
      integer :: i,system
      
      i = system('date +%s.%N &> ~/.tmp_map_f')
      open(unit=9,status='old',file='~/.tmp_map_f')
      read(9,'(F20.9)')timestamp
      close(9)
      i = system('rm -f ~/.tmp_map_f')
      end function timestamp
************************************************************************
      
      
      
      
****************************************************************************************************
      subroutine twilight_rgb(h1,hs1,rgb)  !Get the sky colour as a function of altitude and sun alt.
                                           !h: altitude in degrees, hs: sun altitude in degrees
****************************************************************************************************
      implicit none
      real :: rgb(3),h1,h,hs1,hs,r,g,b,sunfac,x
      
      r = 0.
      g = 0.1
      b = 0.
      
      hs = hs1
!      h  = min(h1 - hs - 3. , 45.)
      h  = min(h1 - hs , 45.)
      
      !R:
      if(h.le.2.41) 			r = 250.77
      if(h.gt.2.41.and.h.le.9.7) 	r = 250.77 - (h-2.41)*16.233
      if(h.gt.9.7.and.h.le.15.98) 	r = 132.29 - (h-9.70)*8.751
      if(h.gt.15.98.and.h.le.23.47) 	r = 77.42 - (h-15.98)*3.693
      if(h.gt.23.47) 			r = max(49.74 - (h-23.47)*2.198 , 0.)
      
      !G:
      if(h.le.2.55) 			g = 173.08 + 28.594*h
      if(h.gt.2.55.and.h.le.4.56) 	g = 245.92 + (h-2.55)*0.967
      if(h.gt.4.56.and.h.le.14.16) 	g = 247.86 - (h-4.56)*10.874
      if(h.gt.14.16.and.h.le.23.18) 	g = 143.46 - (h-14.16)*5.980
      if(h.gt.23.18) 			g = max(89.56 - (h-23.18)*3.249 , 0.)
      
      !B:
      if(h.le.2.75) 			b = 127.92 + 33.216*h
      if(h.gt.2.75.and.h.le.6.18) 	b = 220.67 + (h-2.75)*8.213
      if(h.gt.6.18.and.h.le.20.53) 	b = 248.83 - (h-6.18)*6.191
      if(h.gt.20.53) 			b = max(159.97 - (h-20.53)*4.531 , 0.)
      
      
      sunfac = 1.
      if(hs.le.0.) sunfac = max(1. - (-hs/18.)*0.5 , 0.5)
      rgb = (/r,g,b/)*sunfac
      if(hs.gt.0.) then
	if(hs.lt.3.) then
	  x = hs/3. !0-1
	  rgb = rgb*(1.-x) + (/62.574,108.537,163.270/)/164.*220*x
	else
          rgb = (/62.574,108.537,163.270/)/164.*220  !'blue'
	endif
      endif
      
      return 
      end subroutine twilight_rgb
****************************************************************************************************
      
      
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
      function dms2rad(d,m,s) !Convert d:m:s to radians
************************************************************************
      use constants
      real*8 :: dms2rad,d,m,s
      if(d.lt.0.d0) then
        m = -dabs(m)
	s = -dabs(s)
      endif
      dms2rad = (d+m/60.d0+s/3600.d0)*d2r
      end function dms2rad
************************************************************************
      
************************************************************************
      function hms2rad(h,m,s) !Convert h:m:s to radians
************************************************************************
      use constants
      real*8 :: hms2rad,h,m,s
      if(d.lt.0.d0) then
        m = -dabs(m)
	s = -dabs(s)
      endif
      hms2rad = (h+m/60.d0+s/3600.d0)*h2r
      end function hms2rad
************************************************************************
      
      
      
      
************************************************************************
      subroutine eq2gal(ra,dec,l,b) !Convert equatorial coordinates to 
      !                                                         galactic
************************************************************************
      use constants
      real*8 :: ra,dec,l,b,ra0,dec0,l0,b0,rev
      ra0 = 3.36603472d0  !RA of GNP, in rad, J2000.0 (12h51m26.3s ~ 192.8596deg)
      dec0 = 0.473478737d0 !Decl. of gal. NP in rad, J2000.0 (27d07'42"=27.1283deg)
      l0 = 5.287194d0  !J2000.0?, = 302.9339deg (the 303deg in Meeus, p.94) = l0+180deg
      l = rev(l0 - datan2(dsin(ra0-ra),dcos(ra0-ra)*dsin(dec0) - dtan(dec)*dcos(dec0)))
      b = dasin(dsin(dec)*dsin(dec0) + dcos(dec)*dcos(dec0)*dcos(ra0-ra))
      end subroutine eq2gal
************************************************************************
      
      
************************************************************************
      subroutine gal2eq(l,b,ra,dec) !Convert galactic coordinates to 
      !                                                      equatorial
************************************************************************
      use constants
      real*8 :: ra,dec,l,b,ra0,dec0,l0,b0,rev
      ra0 = 0.22444207d0  !RA of GNP - 12h, in rad, J2000.0 (12h51m26.3s - 12h ~ 12.8596deg)
      dec0 = 0.473478737d0 !Decl. of gal. NP in rad, J2000.0 (from Sterrengids?: 27d07'42"=27.1283deg)
      l0 = 2.145601d0  !J2000.0?, = 123.9339deg (the 123deg in Meeus, p.94)
      ra = rev(datan2(dsin(l-l0),dcos(l-l0)*dsin(dec0) - dtan(b)*dcos(dec0)) + ra0)
      dec = dasin(dsin(b)*dsin(dec0) + dcos(b)*dcos(dec0)*dcos(l-l0))
      end subroutine gal2eq
************************************************************************
      
      
      
      
************************************************************************
      subroutine lb2radec(l,b,eps,ra,dec) !Calculate RA, Dec from l,b,eps
************************************************************************
      implicit none
      real*8 :: l,b,eps,ra,dec,rev
      ra  = rev(datan2(dsin(l)*dcos(eps) - dtan(b)*dsin(eps),dcos(l)))
      dec = dasin(dsin(b)*dcos(eps) + dcos(b)*dsin(eps)*dsin(l))
      return
      end subroutine lb2radec
************************************************************************


************************************************************************
      subroutine radec2lb(a,d,eps,l,b) !Calculate l,b from RA, Dec
************************************************************************
      implicit none
      real*8 :: a,d,eps,l,b,rev
      l = rev(datan2(dsin(a)*dcos(eps) + dtan(d)*dsin(eps),dcos(a)))
      b = dasin(dsin(d)*dcos(eps) - dcos(d)*dsin(eps)*dsin(a))
      return
      end subroutine radec2lb
************************************************************************

