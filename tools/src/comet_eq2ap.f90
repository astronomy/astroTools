!> \file comet_eq2ap  Calculate comet a,P from e and q

!***********************************************************************************************************************************
!> \brief  Calculate comet a,P from e and q

program comet_eq2ap
  use SUFR_kinds, only: double
  
  implicit none
  integer :: iargc,narg
  real(double) :: a,p,q,e,da,dp,dq,de,dadq,dade,dpda
  character :: str*25
  
  write(6,'(/,A)') '  This program computes the semimajor axis and period of a comet from its perihelion distance q and '// &
       'eccentricity e'
  
  narg = iargc()
  if(narg.eq.2) then
     call getarg(1,str)
     read(str,*) q
     call getarg(2,str)
     read(str,*) e
     dq = 0.d0
     de = 0.d0
  else if(narg.eq.4) then
     call getarg(1,str)
     read(str,*) q
     call getarg(2,str)
     read(str,*) dq
     call getarg(3,str)
     read(str,*) e
     call getarg(4,str)
     read(str,*) de
  else
     write(6,'(A)') 'Syntax: comet_eq2ap <q> <e>'
     write(6,'(A,/)') 'Syntax: comet_eq2ap <q> <dq> <e> <de>'
     stop
  endif
  
  a = q/(1.d0-e)
  dadq = 1.d0/(1.d0-e)
  dade = (1.d0-e+q)/(1.d0-e)**2
  da = dadq*dq + dade*de
  
  p = a**(1.5d0)
  dpda = 1.5d0*a**0.5d0
  dp = dpda*da
  
  
  write(*,'(2x,3(A,ES12.3),A)') 'e:',e, '       de:',de, ',  de/e:',de/e*100,' %'
  write(*,'(2x,3(A,ES12.3),A)') 'q:',q, ' AU,   dq:',dq, ',  dq/q:',dq/q*100,' %'
  write(*,'(2x,3(A,ES12.3),A)') 'a:',a, ' AU,   da:',da, ',  da/a:',da/a*100,' %'
  write(*,'(2x,3(A,ES12.3),A)') 'P:',P, ' yr,   dP:',dP, ',  dP/P:',dP/P*100,' %'
  write(*,*)
  
end program comet_eq2ap
!***********************************************************************************************************************************
