      !Convert azimuth to wind direction
      implicit none
      real :: az
      integer :: i,iargc,narg
      character :: wds(0:15)*3,wdsnl(0:15)*3,str*25
      
      wds =(/'S  ','SSW','SW ','WSW','W  ','WNW','NW ','NNW','N  ',
     &'NNE','NE ','ENE','E  ','ESE','SE ','SSE'/)
      wdsnl =(/'Z  ','ZZW','ZW ','WZW','W  ','WNW','NW ','NNW','N  ',
     &'NNO','NO ','ONO','O  ','OZO','ZO ','ZZO'/)
     
      narg = iargc()
      if(narg.eq.1) then
        call getarg(1,str)
	read(str,*)az
      else
 10     write(6,'(A26,$)')'Azimuth (deg, south = 0): '
        read*,az
      endif
      
      i = mod(nint(az/22.5)+32,16) !So that -1 -> 15
!      print*,i
      write(6,'(F6.1,2A10)')az,wds(i),wdsnl(i)
!      goto 10
      end
