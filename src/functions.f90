!***********************************************************************************************************************************
!> \brief  Initialise an astroTools program

subroutine astroTools_init()
  use SUFR_constants, only: set_SUFR_constants
  use AT_version, only: print_astroTools_version
  implicit none
  
  ! Print version:
  write(6,'(/,A)', advance='no') '  '
  call print_astroTools_version(6)
  write(6,*) ' -  astrotools.sf.net'
  
  ! Initialise libSUFR constants:
  call set_SUFR_constants()
  
end subroutine astroTools_init
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Calculate Greenw Mean Siderial Time in RAD!

function calcgmst(jd)
  use SUFR_kinds, only: double
  use SUFR_constants, only: jd2000
  use SUFR_angles, only: rev
  
  implicit none
  real(double), intent(in) :: jd
  real(double) :: calcgmst,t,gmst
  
  t = (jd-jd2000)/36525.d0      !Julian Centuries after 2000.0 UT
  gmst = 4.894961212735793d0 + 6.300388098984957d0*(jd-2451545.d0) + 6.77070812713916d-6*t*t - 4.50872966158d-10*t*t*t
  calcgmst = rev(gmst)  !If corrected for equation of the equinoxes: = rev(gmst + dpsi*dcos(eps))
  
end function calcgmst
!***********************************************************************************************************************************



!***********************************************************************************************************************************
!> \brief Print angle as dd:mm.mmm string (for gps), input in rad, output between -180 and +180 !!!

function dmmmmm2(a1)
  use SUFR_kinds, only: double
  use SUFR_constants, only: r2d
  use SUFR_angles, only: rev2
  
  implicit none
  real(double), intent(in) :: a1
  real(double) :: a,m
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


