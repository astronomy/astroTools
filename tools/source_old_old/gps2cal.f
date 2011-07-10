      !Convert (LIGO?) GPS time to calendar (1/1/2000 = 630720013.0)
  
      implicit none
      real*8 :: d,jd,gps
      integer :: y,m
      write(6,'(A,$)')'  Give GPStime: '
      read*,gps
      !jd = (gps - 630720013.d0)/86400d0 + 2451545.d0
      jd = (gps - 630720013.d0)/86400d0 + 2451544.5d0
      call jd2cal(jd,y,m,d)
      write(6,'(A,F18.3)')'    GPStime:',gps
      write(6,'(A,F20.7)')'    JD:     ',jd
      write(6,'(A,I9,I3,F7.3)')'    Date:   ',y,m,d
      end
            
      
************************************************************************
      subroutine jd2cal(jdd,y,m,dd) !in UT
************************************************************************
      implicit none
      real*8 :: dd,jd,jdd,f
      integer*8 :: z,a,b,c,d,e,alpha
      integer :: m,y
      jd = jdd		
      z = int(jd+0.5d0)
      f = jd+0.5d0 - z
      if (z.lt.2299161) a = z
      if (z.ge.2299161) then 
         alpha = int((z-1867216.25d0)/36524.25d0)
         a = z + 1 + alpha - int(alpha/4.d0)
      endif
      b = a + 1524
      c = int((b - 122.1d0)/365.25d0)
      d = int(365.25d0*c)
      e = int((b-d)/30.6001d0)
      dd = b - d - int(30.6001d0*e) + f
      if (e.lt.14)  m = e - 1
      if (e.ge.14)  m = e - 13
      if (m.gt.2)  y = c - 4716
      if (m.le.2)  y = c - 4715
      return
      end subroutine jd2cal
************************************************************************
      
