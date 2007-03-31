!Distance_on_earth.f: Calculate the distance between two points on the Earth's globe:
      use constants
      implicit none
      real*8 :: a,fl,x1,x2,x3,l1,l2,b1,b2
      real*8 :: f,g,l,s,c,o,r,d,h1,h2,dist,rev,rev2,asep
      character :: dmss*13,dmss2*14
      
      call declconst
      
!      d2r = datan(1.d0)/45.d0		!Degrees to radians
      a   = 6378.14 			!Earth's radius in km
      fl  = 1.d0/298.257d0		!Earth's flattening
      
      
      print*,''
      print*," This programme calculates the distance between two points
     & on the Earth's globe in degrees and kilometres."
     
      print*,''
      print*,' Location 1:'
      write(6,'(A37,$)')'Please give the longitude (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x2.lt.0) x3 = -x3
      l1 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      write(6,'(A36,$)')'Please give the latitude (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x2.lt.0) x3 = -x3
      b1 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      
      print*,''
      print*,' Location 2:'
      write(6,'(A37,$)')'Please give the longitude (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x2.lt.0) x3 = -x3
      l2 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      write(6,'(A36,$)')'Please give the latitude (d,m,s): '
      read*,x1,x2,x3
      if(x1.lt.0) x2 = -x2
      if(x2.lt.0) x3 = -x3
      b2 = rev2((x1 + x2/60.d0 + x3/3600.d0)*d2r)
      
      
      
      f = (b1+b2)*0.5d0
      g = (b1-b2)*0.5d0
      l = (l1-l2)*0.5d0
      
      s = dsin(g)**2*dcos(l)**2 + dcos(f)**2*dsin(l)**2
      c = dcos(g)**2*dcos(l)**2 + dsin(f)**2*dsin(l)**2
      o = datan2(dsqrt(s),dsqrt(c))
      r = dsqrt(s*c)/o
      d = 2*o*a
      h1 = (3*r-1)/(2*c)
      h2 = (3*r+1)/(2*s)
      
      dist = d*(1.d0 + fl*h1*dsin(f)**2*dcos(g)**2 - 
     &                 fl*h2*dcos(f)**2*dsin(g)**2)
      
      
!      print*,l1/d2r,b1/d2r
!      print*,l2/d2r,b2/d2r
!      print*,f/d2r,g/d2r,l/d2r
!      print*,s,c,o
!      print*,r,dist
      
      
      print*,''
      print*,''
      write(6,'(A13,2A20)')'Location 1:',dmss2(l1),dmss2(b1)
      write(6,'(A13,2A20)')'Location 2:',dmss2(l2),dmss2(b2)
      print*,''
      write(6,'(A13,2A20)')'Difference:',dmss(dabs(l2-l1)),
     &dmss(dabs(b2-b1))
      write(6,'(A13,A20)')'Separation:',dmss(asep(l1,l2,b1,b2))
      print*,''
      write(6,'(A13,F20.3,A4,F5.3,A3)')'Distance:  ',dist,
     &' +- ',dist*fl**2,'km'
!      print*,'(the error is in the order of 10^-5)'
      print*,''
      print*,''
      
      
      
      end
      
      
