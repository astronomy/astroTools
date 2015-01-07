      !Calculate comet a,P from e and q
      implicit none
      real*8 :: a,p,q,e,da,dp,dq,de,dadq,dade,dpda
      integer :: i,iargc,narg
      character :: str*25
      
      narg = iargc()
      if(narg.eq.2) then
        call getarg(1,str)
	read(str,*)e
        call getarg(2,str)
	read(str,*)q
	de = 0.d0
	dq = 0.d0
      else if(narg.eq.4) then
        call getarg(1,str)
	read(str,*)e
        call getarg(2,str)
	read(str,*)de
        call getarg(3,str)
	read(str,*)q
        call getarg(4,str)
	read(str,*)dq
      else
        write(6,'(A13,$)')'Give e,de: '
        read*,e,de
        write(6,'(A13,$)')'Give q,dq: '
        read*,q,dq
      endif
      
      a = q/(1.d0-e)
      dadq = 1.d0/(1.d0-e)
      dade = (1.d0-e+q)/(1.d0-e)**2
      da = dadq*dq + dade*de
      
      p = a**(1.5d0)
      dpda = 1.5d0*a**0.5d0
      dp = dpda*da
      
      
      print*,e,de,de/e*100
      print*,q,dq,dq/q*100
      print*,a,da,da/a*100
      print*,p,dp,dp/p*100
      end
