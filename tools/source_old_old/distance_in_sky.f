!Distance_in_sky.f: Calculate the distance between two positions on a stellar globe:
      use constants
      implicit none
      real*8 :: a,fl,x1,x2,x3,l1,l2,b1,b2
      real*8 :: f,g,l,s,c,o,r,d,h1,h2,dist,rev,rev2,asep
      character :: hmss*12,dmss*13,dmss2*14
      
      call declconst
      
      
      
      write(6,*)''
      write(6,'(A)')' This programme calculates the distance between two positions in the sky in degrees.'
     
      write(6,*)''
      write(6,'(A)')' Position 1:'
      write(6,'(A43,$)')'Please give the right ascension (h,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x1.lt.0.or.x2.lt.0) x3 = -x3
      l1 = rev2((x1 + x2/60.d0 + x3/3600.d0)*h2r)
      
      write(6,'(A39,$)')'Please give the declination (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x1.lt.0.or.x2.lt.0) x3 = -x3
      b1 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      
      write(6,*)''
      write(6,'(A)')' Position 2:'
      write(6,'(A43,$)')'Please give the right ascension (h,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x1.lt.0.or.x2.lt.0) x3 = -x3
      l2 = rev2((x1 + x2/60.d0 + x3/3600.d0)*h2r)
      
      write(6,'(A39,$)')'Please give the declination (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x1.lt.0.or.x2.lt.0) x3 = -x3
      b2 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      dist = asep(l1,l2,b1,b2)
      
!      print*,l1/h2r,b1/d2r
!      print*,l2/h2r,b2/d2r
!      print*,dist
      
      
      write(6,*)''
      write(6,*)''
      write(6,'(A)')'                  Right Ascension:       Declination:'
      write(6,'(A13,2A20)')'Location 1:',hmss(l1/h2r),dmss2(b1)
      write(6,'(A13,2A20)')'Location 2:',hmss(l2/h2r),dmss2(b2)
      write(6,*)''
      write(6,'(A13,2A20)')'Differences:',dmss(dabs(l2-l1)),dmss(dabs(b2-b1))
      write(6,'(A13,A20)')'Separation:',dmss(asep(l1,l2,b1,b2))
      write(6,*)''
!      write(6,'(A13,F20.3,A4,F5.3,A3)')'Distance:  ',dist,
!     &' +- ',dist*fl**2,'km'
!      write(6,'(A)')'(the error is in the order of 10^-5)'
      write(6,*)''
      write(6,*)''
      
      
      
      end
      
