
!***********************************************************************************************************************************
program ddd2dms
  use SUFR_kinds, only: double
  use SUFR_constants, only: set_SUFR_constants, d2r
  use SUFR_system, only: quit_program
  
  implicit none
  real(double) :: angle
  character :: dmss2*(14),dmmmmm2*(12), tmpstr*(99)
  
  if(command_argument_count().ne.1)  call quit_program('syntax: ddd2dms <angle>')
     
  call set_SUFR_constants()
  
  call get_command_argument(1,tmpstr)
  read(tmpstr,*) angle
  
  write(6,'(30x,F14.7,A15,A16)') angle, dmmmmm2(angle*d2r), dmss2(angle*d2r)
  
end program ddd2dms
!***********************************************************************************************************************************

