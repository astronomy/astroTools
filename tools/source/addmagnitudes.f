      !Add magitudes and compute total magnitude
      implicit none
      integer :: i,n,iargc
      real*8 :: mags(10),sum,totmag
      character :: str*25
      
      sum = 0.d0
      n = iargc()
      if(n.gt.10) then
         write(6,'(A)')' Up to 10 magnitudes are allowed'
         stop
      end if
      do i=1,n
         call getarg(i,str)
         read(str,*)mags(i)
         sum = sum + 10.d0**(-mags(i)/2.5d0)
      end do
      if(n.eq.0) then
10       write(6,'(A)')' Syntax: addmagnitudes <m1><m2>...<m10>'
         stop
      end if
      
      totmag = -2.5d0*dlog10(sum)
      
      write(6,'(A,$)')' Input magnitudes: '
      do i=1,n
         write(6,'(F6.2,$)')mags(i)
      end do
      write(6,*)''
      write(6,'(A,F6.2)')' Total magnitude: ',totmag
      end
