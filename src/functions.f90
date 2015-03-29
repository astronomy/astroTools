!***********************************************************************************************************************************
module constants
  use SUFR_kinds, only: double
  implicit none
  save
  private :: double
  
  !  real(double) :: pi,pi2,r2d,d2r,r2h,h2r,d2as,as2d,r2as,as2r
  !  real(double) :: au,rsun,msun,jd2000
  !  integer :: mlen(12)
  !  character*9 :: months(12),monthsm(12),nlmonths(12),nlmonthsb(12),nlmnts(12)*3
  !  character*9 :: days(0:6),nldays(0:6),nlds(0:6)*2
  !  character :: phases(0:3)*13,nlphases(0:3)*16
  
  integer, parameter :: deltatnmax=1000
  integer :: deltatn
  real(double) :: deltatvals(deltatnmax),deltatyrs(deltatnmax),nutationdat(9,63)
  character :: vsopdir*(43)
  
end module constants
!***********************************************************************************************************************************


!***********************************************************************************************************************************
subroutine declconst
  use constants, only: vsopdir
  implicit none
  
  vsopdir = '/home/sluys/diverse/popular/TheSky/'  !New dir
  
end subroutine declconst
!***********************************************************************************************************************************


!***********************************************************************************************************************************
function calcgmst(jd) !Calculate Greenw Mean Siderial Time in RAD!
  use SUFR_kinds, only: double
  use SUFR_constants, only: jd2000
  
  implicit none
  real(double) :: calcgmst,jd,t,gmst,rev
  
  t = (jd-jd2000)/36525.d0      !Julian Centuries after 2000.0 UT
  gmst = 4.894961212735793d0 + 6.300388098984957d0*(jd-2451545.d0) + 6.77070812713916d-6*t*t - 4.50872966158d-10*t*t*t
  calcgmst = rev(gmst)  !If corrected for equation of the equinoxes: = rev(gmst + dpsi*dcos(eps))
  
end function calcgmst
!***********************************************************************************************************************************



!***********************************************************************************************************************************
function dmmmmm2(a1)   !Print angle as dd:mm.mmm string (for gps), 
  !input in rad, output between -180 and +180 !!!
  use SUFR_kinds, only: double
  use SUFR_constants, only: r2d
  
  implicit none
  real(double) :: a1,a,rev2,m
  integer :: d
  character :: dmmmmm2*(12),mm*(6),dd*(4),sig
  
  a = a1
  a = rev2(a)*r2d
  sig = '+'
  if(a.lt.0.d0) then
     sig = '-'
     a = -1.d0*a
  end if
  
  d = int(a)
  m = (a-d)*60.d0
  if(m.ge.59.95d0) then
     d = d+1
     m = dabs(m-60d0)
  end if
  
  write(dd,'(a1,i3.3)') sig,d
  write(mm,'(F6.3)') m
  if(m.lt.10.d0) write(mm,'(I1,F5.3)') 0,m
  write(dmmmmm2,'(a4,a1,a6,a1)') dd,'d',mm,"'"
  
end function dmmmmm2
!***********************************************************************************************************************************


!***********************************************************************************************************************************
function rev2(x)      !Returns angle in radians between -pi and pi
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi
  
  implicit none
  real(double) :: x,rev2
  
  rev2 = x-floor(x/(2*pi))*2*pi
  if(rev2.gt.pi) rev2 = rev2 - 2*pi
  
end function rev2
!***********************************************************************************************************************************


!***********************************************************************************************************************************
function rev(x)        !Returns angle in radians between 0 and 2pi
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi
  
  implicit none
  real(double) :: x,rev
  
  rev = x-floor(x/(2*pi))*2*pi
  
end function rev
!***********************************************************************************************************************************




