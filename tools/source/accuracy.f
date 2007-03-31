      ! accuracy.f calculates the number of digits used be the computer
      implicit none
      
      real x
      real*8 y
      integer j
      
      
      x=1.
      j=0
 10   x = x*2
      if (x+1.ne.x) then
        j = j+1
        goto 10
      endif
      
      print*,'Single precision: ',j,float(j)*log10(2.d0)
      
      
      y=1.d0
      j=0
 20   y = y*2
      if (y+1.ne.y) then
        j = j+1
        goto 20
      endif
      
      print*,'Double precision: ',j,float(j)*log10(2.d0)
      
      print*,''
      
      x = 1/3.
      y = 1/3.d0
      print*,'Single, double precision: ',0,x,y
      do j=1,31
        x = (9.*x+1.)*x - 1.
        y = (9.d0*y+1.d0)*y - 1.d0
	print*,'Single, double precision: ',j,x,y
      enddo
      
      
      print*,''
      
      x = 1.0000001
      y = 1.0000001d0
      print*,'Single, double precision: ',0,x,y
      do j=1,27
        x = x**2
        y = y**2
	print*,'Single, double precision: ',j,x,y
      enddo

      
      end
      
      
