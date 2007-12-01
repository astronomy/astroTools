      implicit none
      
      integer :: h1,m1,s1,h2,m2,s2,t1,t2,dt
      character :: hms*8
      
      print*,'Give time 1 in h,m,s'
      read*,h1,m1,s1
      print*,'Give time 2 in h,m,s'
      read*,h2,m2,s2
      
      t1 = h1*3600+m1*60+s1
      t2 = h2*3600+m2*60+s2
      
      dt = t2-t1
      if(dt.lt.0) dt = -dt
      
      print*,dt
      print*,hms(dt/3600.d0)
      
      end
      



