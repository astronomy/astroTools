!> \file comet_eq2ap.f90  Calculate comet a,P from e and q


!  Copyright (c) 2002-2020  AstroFloyd - astrofloyd.org
!   
!  This file is part of the astroTools package, 
!  see: http://astrotools.sourceforge.net/
!   
!  This is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published
!  by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!  
!  This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License along with this code.  If not, see 
!  <http://www.gnu.org/licenses/>.




!***********************************************************************************************************************************
!> \brief  Calculate comet a, P from e and q

program comet_eq2ap
  use SUFR_kinds, only: double
  use SUFR_system, only: syntax_quit
  use SUFR_command_line, only: get_command_argument_d
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: Narg
  real(double) :: a,p,q,e, da,dp,dq,de, dadq,dade,dpda
  
  call astroTools_init()
  
  Narg = command_argument_count()
  if(Narg.eq.2) then
     call get_command_argument_d(1,q)
     call get_command_argument_d(2,e)
     dq = 0.d0
     de = 0.d0
  else if(Narg.eq.4) then
     call get_command_argument_d(1,q)
     call get_command_argument_d(2,dq)
     call get_command_argument_d(3,e)
     call get_command_argument_d(4,de)
  else
     call syntax_quit('comet_eq2ap <q> <e>    comet_eq2ap <q> <dq> <e> <de>', 0, 'Compute the semimajor axis and period of a '// &
          'comet from its perihelion distance q and eccentricity e')
  end if
  
  
  ! Orbital separation:
  a = q/(1.d0-e)
  dadq = 1.d0/(1.d0-e)
  dade = (1.d0-e+q)/(1.d0-e)**2
  da = dadq*dq + dade*de
  
  ! Orbital period:
  p = a**1.5d0
  dpda = 1.5d0*sqrt(a)
  dp = dpda*da
  
  
  ! Print output:
  write(*,*)
  write(*,'(2x,A)') 'Orbital elements:'
  write(*,'(4x,A,ES12.3,A)', advance='no') 'e:',e
  if(Narg.eq.4) write(*,'(2(A,ES12.3),A)', advance='no') ',      de:',de, ',  de/e:',de/e*100,' %'
  write(*,*)
  
  write(*,'(4x,A,ES12.3,A)', advance='no') 'q:',q, ' AU'
  if(Narg.eq.4) write(*,'(2(A,ES12.3),A)', advance='no') ',   dq:',dq, ',  dq/q:',dq/q*100,' %'
  write(*,*)
  
  write(*,'(4x,A,ES12.3,A)', advance='no') 'a:',a, ' AU'
  if(Narg.eq.4) write(*,'(2(A,ES12.3),A)', advance='no') ',   da:',da, ',  da/a:',da/a*100,' %'
  write(*,*)
  
  write(*,'(4x,A,ES12.3,A)', advance='no') 'P:',P, ' yr'
  if(Narg.eq.4) write(*,'(2(A,ES12.3),A)', advance='no') ',   dP:',dP, ',  dP/P:',dP/P*100,' %'
  write(*,*)
  
  write(*,*)
  
end program comet_eq2ap
!***********************************************************************************************************************************
