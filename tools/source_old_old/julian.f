************************************************************************
      program julian
************************************************************************
      implicit none
      real*8 jd,d
      integer y,m,dow,doy,woy,i
      character*9 wdays(7),months(12)
      
      data wdays/'Sunday','Monday','Tuesday','Wednesday','Thursday',
     &'Friday','Saturday'/
      data months/'January','February','March','April','May','June',
     &'July','August','September','October','November','December'/
      
      d = 26.0
      m = 12
      y = 1
      
      print*,wdays(dow(2003,5,d)+1),months(m),int(d),y
      print*,'Weeknumber:',woy(y,m,d),'   Day of year:',doy(y,m,d)
    
99999 continue

      end
      



************************************************************************
      subroutine cal2jd(yy,mm,dd,jd)
************************************************************************
      ! Calendar date -> Julian Day
      ! Valid for all JD > 0
      ! Note that the year 0 exists, so that 10 BC = -9 !
      ! Julian: upto Oct. 4, 1582, Gregorian: from Oct 15, 1582
      
      implicit none
      real*8 jd,d,dd
      integer y,yy,m,mm,a,b,greg
      
      !Make sure input is unchanged
      y = yy
      m = mm
      d = dd

      !Julian or gregorian?
      greg=0
      if (y.gt.1582) greg=1
      
      if (y.eq.1582) then
        if (m.gt.10) greg=1
	if ((m.eq.10).and.(d.ge.15)) greg=1
      endif
      

      if (m.lt.2) then 
         y = y-1
         m = m+12
      endif
      
      b = 0

      !For a Gregorian date:
      if(greg.eq.1) then
        a = int(y/100.)
        b = 2 - a + int(a/4.)
      endif
  

      jd = int(365.25*(y+4716)) + int(30.6001*(m+1)) + d + b - 1524.5
      
      end



************************************************************************
      subroutine jd2cal(jdd,yr,mon,day)
************************************************************************
      ! Julian Day -> Calendar date
      ! Valid for all JD > 0
      ! Note that the year 0 exists, so that 10 BC = -9 !
      ! Julian: upto Oct. 4, 1582, Gregorian: from Oct 15, 1582
       
      implicit none
      real day,jd,jdd,f
      integer*8 z,a,b,c,d,e,alpha
      integer mon,yr
      
      !Make sure input is unchanged
      jd = jdd

      z = int(jd+0.5)
      f = jd+0.5 - z

      if (z.lt.2299161) a = z

      if (z.ge.2299161) then 
         alpha = int((z-1867216.25)/36524.25)
         a = z + 1 + alpha - int(alpha/4.)
      endif

      b = a + 1524
      c = int((b - 122.1)/365.25)
      d = int(365.25*c)
      e = int((b-d)/30.6001)

      day = b - d - int(30.6001*e) + f

      if (e.lt.14)  mon = e - 1
      if (e.ge.14)  mon = e - 13

      if (mon.gt.2)  yr = c - 4716
      if (mon.le.2)  yr = c - 4715
      
      end



************************************************************************
      function dow(y,m,d)
************************************************************************
      ! Calculates day of week number:
      ! Sunday = 0, ..., Saturday = 6.
      ! Time (date) should be in UT
      
      implicit none
      real*8 d,jd,x1
      integer y,m,dow
      
      d = int(d)
      call cal2jd(y,m,d,jd)
      
      x1 = jd + 1.5
      dow = x1 - int(x1/7.)*7.
      
      end
      
      
************************************************************************
      function doy(y,m,d)
************************************************************************
      ! Calculates day of year number:
      
      implicit none
      real*8 d,jd,jd2
      integer y,m,doy
      
      d = int(d)
      call cal2jd(y,m,d,jd)
      call cal2jd(y,1,0.d0,jd2)      
      
      doy = jd - jd2
            
      end
      
      
************************************************************************
      function woy(y,m,d)
************************************************************************
      ! Calculates week of year number:
      
      implicit none
      real*8 d,jd,jd2
      integer y,m,woy,doy,dow
      
      woy = int((doy(y,m,d) + 7 - dow(y,m,d))/7.)
            
      end
