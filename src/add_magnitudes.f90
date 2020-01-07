!> \file add_magnitudes.f90  Add magitudes and compute the total magnitude


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
!> \brief  Add magitudes and compute the total magnitude

program add_magnitudes
  use SUFR_kinds, only: double
  use SUFR_getopt, only: getopt_t, getopt_long, optArg, optCount
  
  use AT_general, only: astroTools_init, print_astroTools_banner
  
  implicit none
  integer :: iMag,nMag
  real(double), allocatable :: mags(:)
  real(double) :: sumMag,totMag
  logical :: quiet
  
  ! Set up the longopts struct to define the valid options: short option, long option, argument (0/1), short description:
  type(getopt_t) :: longopts(3) = [ &
       getopt_t('h', 'help',       0, 'Print help'),         &
       getopt_t('m', 'magnitude',  1, 'Argument <arg> is a magnitude - needed for negative values'),         &
       getopt_t('q', 'quiet',      0, 'Quiet mode - just print the answer')                    ]
  
  call astroTools_init(banner=.false.)  ! Initialise aT and libSUFR
  
  nMag = command_argument_count()
  allocate(mags(nMag))
  
  iMag = 0
  sumMag = 0.d0
  quiet = .false.
  
  do  ! scan all the command-line parameters
     select case(getopt_long(longopts))
     case('>')  ! Last parameter
        if(optCount.le.1) call print_syntax_quit(longopts)  ! Print syntax and quit program
        exit  ! All arguments were read - exit do loop
     case('!')  ! Unknown option (starting with "-" or "--")
        write(*,'(A)') 'WARNING: unknown option:  '//trim(optArg)//'  Use --help for a list of valid options'
     case('h')
        call print_syntax_quit(longopts)  ! Print syntax and quit program
     case('m')
        call read_mag(nMag,iMag,optCount, mags, sumMag)  ! Read a magnitude from stdin and add its luminosity to sumMag
     case('q')
        quiet = .true.
     case('.')  ! Parameter is not an option (i.e., it doesn't start with "-" or "--")
        call read_mag(nMag,iMag,optCount, mags, sumMag)  ! Read a magnitude from stdin and add its luminosity to sumMag
     case default
     end select
  end do
  
  nMag = iMag
  totMag = -2.5d0*log10(sumMag)
  
  if(quiet) then
     write(*,'(F7.3)') totMag
  else
     call print_astroTools_banner()  ! Print banner
     write(*,'(/,2x,A)', advance='no') 'Input magnitudes: '
     do iMag=1,nMag
        write(*,'(F7.3)', advance='no') mags(iMag)
     end do
     write(*,'(/,2x,A,F7.3,/)') 'Total magnitude:  ', totMag
  end if
  
end program add_magnitudes
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief  Print syntax and quit program
!!
!! \param  longopts  getopt_t array containing long-option info

subroutine print_syntax_quit(longopts)
  use SUFR_system, only: syntax_print
  use SUFR_getopt, only: getopt_t, getopt_long_help
  use AT_general, only: print_astroTools_banner
  
  implicit none
  type(getopt_t), intent(in) :: longopts(3)

  call print_astroTools_banner()
  call syntax_print('[options]  <m_1> <m_2> ... <m_n>', 'Add magnitudes and compute the total magnitude')
  call getopt_long_help(longopts,0,1)  ! No parameters found - print help
  
  stop
end subroutine print_syntax_quit
!***********************************************************************************************************************************


!***********************************************************************************************************************************
!> \brief Read a magnitude from stdin and add its luminosity to sumMag
!!
!! \param  nMag      Maximum number of elements in mags array (I/O)
!! \param  iMag      Reading the iMag-th magnitude (I/O)
!! \param  optCount  Reading the optCount-th command-line option
!! \param  mags      Array containing magnitudes read so far (I/O)
!! \param  sumMag    Sum of the magnitudes (actually luminosities - I/O)

subroutine read_mag(nMag,iMag,optCount, mags, sumMag)
  use SUFR_kinds, only: double
  use SUFR_system, only: quit_program_error
  use SUFR_command_line, only: get_command_argument_d
  
  implicit none
  integer, intent(inout) :: nMag,iMag
  integer, intent(in) :: optCount
  real(double), intent(inout) :: mags(nMag), sumMag
  integer :: status
  
  iMag = iMag + 1
  call get_command_argument_d(optCount, mags(iMag), status=status)
  if(status.ne.0) call quit_program_error('Arguments must be numbers',1)
  sumMag = sumMag + 10.d0**(-mags(iMag)/2.5d0)
  
end subroutine read_mag
!***********************************************************************************************************************************
