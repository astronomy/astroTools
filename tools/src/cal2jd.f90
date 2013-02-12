!> \file cal2jf.f90  Compute the Julian day for a given calendar date

!***********************************************************************************************************************************
!> \brief  Compute the Julian day for a given calendar date
program calendar2jd
      implicit none
   real*8 :: d,jd,cal2jd
   integer :: y,m,iargc
   character :: str*99
   
   if(iargc().eq.3) then
      call getarg(1,str)
      read(str,*)y
      call getarg(2,str)
      read(str,*)m
      call getarg(3,str)
      read(str,*)d
   else
      write(0,'(/,A,/)')'  syntax: cal2jd <year> <month> <day> '
      stop
   end if
   jd = cal2jd(y,m,d)
   write(6,'(/,I6,2I3.2,F20.8,/)')y,m,nint(d),jd
end program calendar2jd
!***********************************************************************************************************************************


