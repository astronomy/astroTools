      implicit none
      real*8 :: dist
      character :: ms*6
      write(6,'(A42,$)')'Give Moon distance in km:                 '
      read*,dist
      
      write(6,'(A42,A6)')'The apparent diameter of the Moon is:     ',
     &ms(2.d0*datan(1738./dist))
      
      end
      

************************************************************************
      function ms(a1)   !Print angle as a nice M&S string, input in rad
************************************************************************
      implicit none
      real*8 :: a1,a
      integer :: d,m,s
      character*10 :: ms*6,mm*2,ss*2,ddd*3
      
      a = a1*180.d0/(4.d0*datan(1.d0))
      d = int(a)
      m = int((a-d)*60.d0)
      s = nint((a-d-m/60.d0)*3600.d0)
      
      write(mm,'(i2)') m
      write(ss,'(i2)') s
      if(m.lt.10) write(mm,'(a1,i1)') '0',m
      if(s.lt.10) write(ss,'(a1,i1)') '0',s

      write(ms,'(2(a2,a1))') mm,"'",ss,'"'

      return
      end function ms
************************************************************************
      
      
