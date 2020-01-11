!> \file date_time_calc.f90  Compute the difference between two dates or times.


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
!> \brief  Compute the difference between two dates or times.

program date_time_calc
  use SUFR_kinds, only: double
  use SUFR_system, only: quit_program_error
  use SUFR_getopt, only: getopt_t, getopt_long, curArg,optArg, optCount, getopt_split_short_options  !, getopt_get_command_argument_d
  use SUFR_command_line, only: get_command_argument_d, read_all_commandline_arguments, get_commandline_argument_types, split_short_cl_options
  use SUFR_date_and_time, only: cal2jd, ymdhms2jd
  use SUFR_time2string, only: hms_sss
  use SUFR_text, only: int2str, dbl2str
  
  use TheSky_datetime, only: printdate
  
  use AT_general, only: astroTools_init, print_astroTools_banner
  
  implicit none
  integer, parameter :: maxArgs=12
  integer :: nArgs, status
  real(double) :: args(maxArgs),  y1,mo1,d1, h1,mi1,s1,  y2,mo2,d2, h2,mi2,s2
  real(double) :: t1,t2, dth, jd1,jd2,djd, djdtot
  logical :: timeOnly, printY,printD,printH,printM,printS, quiet,verbose
  
  
  ! Set up the longopts struct to define the valid options: short option, long option, argument (0/1), short description:
  type(getopt_t) :: longopts(11) = [                                                                &
       getopt_t('t', 'time',    0, 'Specify times only (h,m,s - no dates)'),                        &
       getopt_t('',  '',        0, ''),                                                             &
       getopt_t('y', 'years',   0, 'Print output in years'),                                        &
       getopt_t('d', 'days',    0, 'Print output in days  (default if dates are involved)'),        &
       getopt_t('H', 'hours',   0, 'Print output in hours  (default if only times are involved)'),  &
       getopt_t('m', 'minutes', 0, 'Print output in minutes'),                                      &
       getopt_t('s', 'seconds', 0, 'Print output in seconds'),                                      &
       getopt_t('',  '',        0, ''),                                                             &
       getopt_t('q', 'quiet',   0, 'Quiet mode - just print the computed number (no units)'),       &
       getopt_t('v', 'verbose', 0, 'Verbose mode - print additional details'),                      &
       getopt_t('h', 'help',    0, 'Print help')                                                    ]
  
  call astroTools_init(banner=.false.)  ! Initialise aT and libSUFR
  
  
  ! Set default options before parsing command line:
  timeOnly = .false.
  
  printY   = .false.
  printD   = .false.
  printH   = .false.
  printM   = .false.
  printS   = .false.
  
  quiet    = .false.
  verbose  = .false.
  
  nArgs = 0
  
  do  ! scan all the command-line parameters
     select case(getopt_long(longopts))
     case('>')  ! Last parameter
        if(optCount.le.1) call print_syntax_quit(longopts)  ! Print syntax and quit program
        exit  ! All arguments were read - exit do loop
        
     case('!')  ! Unknown option (starting with "-" or "--")
        write(*,'(A)') 'WARNING: unknown option:  '//trim(optArg)//'  Use -h or --help for a list of valid options'
        
     case('t')
        timeOnly = .true.
        
     case('y')
        printY = .true.
     case('d')
        printD = .true.
     case('H')
        printH = .true.
     case('m')
        printM = .true.
     case('s')
        printS = .true.
        
     case('q')
        quiet = .true.
     case('v')
        verbose = .true.
     case('h')
        call print_syntax_quit(longopts)  ! Print syntax and quit program
        
     case('.')  ! Parameter is not an option (i.e., it doesn't start with "-" or "--")
        nArgs = nArgs + 1
        if(nArgs.gt.maxArgs) call quit_program_error('Can have '//int2str(maxArgs)//' arguments at most', 1)
        !call getopt_get_command_argument_d(optCount, args(nArgs), status=status)
        read(curArg,*, iostat=status) args(nArgs)  ! Read the double-precision value from the current argument string
        if(status.ne.0) call quit_program_error('Arguments must be numbers',1)
        !print*,nArgs,optCount,args(nArgs)
     case default
     end select
  end do
  
  
  if(quiet .and. verbose) call quit_program_error('You cannot specify both -q/--quiet and -v/--verbose', 1)
  
  if(.not. (printY .or. printD .or. printH .or. printM .or. printS)) then
     if(timeOnly) then
        printH = .true.
     else
        printD = .true.
     end if
  end if
  
  if(floor(dble(nArgs)/2.d0) .ne. ceiling(dble(nArgs)/2.d0)) call quit_program_error('Cannot have an odd number of arguments', 1)

  if(verbose) call print_astroTools_banner()
  
  y1=0; mo1=1; d1=1;  y2=0; mo2=1; d2=1;
  h1=0; mi1=0; s1=0;  h2=0; mi2=0; s2=0;
  dth=0; djd=0
  
  if(timeOnly) then  ! Arguments are h,m,s
     
     if(nArgs.eq.2)  then  ! Hours only
        h1  = args(1)
        
        h2  = args(2)
     else if(nArgs.eq.4)  then  ! Hours and minutes only
        h1  = args(1)
        mi1 = args(2)
        
        h2  = args(3)
        mi2 = args(4)
     else if(nArgs.eq.6)  then  ! Hours, minutes and seconds
        h1  = args(1)
        mi1 = args(2)
        s1  = args(3)
        
        h2  = args(4)
        mi2 = args(5)
        s2  = args(6)
     else
        call quit_program_error('time only:  can have at most 6 arguments, not '//int2str(nArgs), 1)
     end if
     
     t1 = h1 + mi1/60.d0 + s1/3600.d0
     t2 = h2 + mi2/60.d0 + s2/3600.d0
     
     dth = t2-t1  ! In hours
     
     if(verbose) then
        write(*,'(A)') 'Time 1: '//hms_sss(t1)
        write(*,'(A)') 'Time 2: '//hms_sss(t2)
        write(*,'(A)') 'Δt:     '//dbl2str(dth,6)//' h'
        if(dth.lt.0) then
           write(*,'(A)') 'Δt:    -'//hms_sss(-dth)
        else
           write(*,'(A)') 'Δt:     '//hms_sss(dth)
        end if
     end if
     
  else  ! Arguments are y,mo,d, h,mi,s
     
     if(nArgs.eq.2)  then  ! Years only
        y1  = args(1)
        
        y2  = args(2)
     else if(nArgs.eq.4)  then  ! Years and months only
        y1  = args(1)
        mo1 = args(2)
        
        y2  = args(3)
        mo2 = args(4)
     else if(nArgs.eq.6)  then  ! Years, months and days only
        y1  = args(1)
        mo1 = args(2)
        d1  = args(3)
        
        y2  = args(4)
        mo2 = args(5)
        d2  = args(6)
     else if(nArgs.eq.8)  then  ! Years, months, days and hours only
        y1  = args(1)
        mo1 = args(2)
        d1  = args(3)
        h1  = args(4)
        
        y2  = args(5)
        mo2 = args(6)
        d2  = args(7)
        h2  = args(8)
     else if(nArgs.eq.10)  then  ! Years, months, days, hours and minutes only
        y1  = args(1)
        mo1 = args(2)
        d1  = args(3)
        h1  = args(4)
        mi1 = args(5)
        
        y2  = args(6)
        mo2 = args(7)
        d2  = args(8)
        h2  = args(9)
        mi2 = args(10)
        
     else if(nArgs.eq.12)  then  ! Years, months, days, hours, minutes and seconds
        y1  = args(1)
        mo1 = args(2)
        d1  = args(3)
        h1  = args(4)
        mi1 = args(5)
        s1  = args(6)
        
        y2  = args(7)
        mo2 = args(8)
        d2  = args(9)
        h2  = args(10)
        mi2 = args(11)
        s2  = args(12)
     else
        call quit_program_error('Cannot have '//int2str(nArgs)//' arguments', 1)
     end if
     
     jd1 = ymdhms2jd(nint(y1),nint(mo1),nint(d1), nint(h1),nint(mi1),s1)
     jd2 = ymdhms2jd(nint(y2),nint(mo2),nint(d2), nint(h2),nint(mi2),s2)
     
     djd = jd2-jd1
     
     if(verbose) then
        write(*,'(A)', advance='no') 'Date 1: '
        call printdate(jd1)
        write(*,'(A)', advance='no') 'Date 2: '
        call printdate(jd2)
        write(*,'(A)') 'Δt:     '//dbl2str(djd,6)//' d'
     end if
     
  end if
  
  djdtot = djd + dth/24.d0
  
  if(quiet) then
     if(printY) write(*,'(A)') dbl2str(djdtot/365.2425d0, 6)
     if(printD) write(*,'(A)') dbl2str(djdtot, 6)
     if(printH) write(*,'(A)') dbl2str(djdtot*24, 6)
     if(printM) write(*,'(A)') dbl2str(djdtot*1440, 6)
     if(printS) write(*,'(A)') dbl2str(djdtot*86400, 3)
  else
     if(printY) write(*,'(A)') 'Δt = '//dbl2str(djdtot/365.2425d0, 6)//' years'
     if(printD) write(*,'(A)') 'Δt = '//dbl2str(djdtot, 6)//' days'
     if(printH) write(*,'(A)') 'Δt = '//dbl2str(djdtot*24, 6)//' hours'
     if(printM) write(*,'(A)') 'Δt = '//dbl2str(djdtot*1440, 6)//' minutes'
     if(printS) write(*,'(A)') 'Δt = '//dbl2str(djdtot*86400, 3)//' seconds'
  end if
  
end program date_time_calc
!***********************************************************************************************************************************




!***********************************************************************************************************************************
!> \brief  Print syntax and quit program
!!
!! \param  longopts  getopt_t array containing long-option info

subroutine print_syntax_quit(longopts)
  use SUFR_constants, only: program_name
  use SUFR_system, only: syntax_print
  use SUFR_getopt, only: getopt_t, getopt_long_help
  use AT_general, only: print_astroTools_banner
  
  implicit none
  type(getopt_t), intent(in) :: longopts(11)

  call print_astroTools_banner()
  !call syntax_print('[options]  <year1> <month1> <day1>  <hour1> <minute1> <second1>  <year2> <month2> <day2>  <hour2> <minute2> <second2>', 'Compute the time difference between two dates or times.  An EVEN number of arguments is required - if date/time 1 is specified up to hours, so must date/time 2.  Daylight savings are NOT taken into account!')
  write(*,'(A)') 'Compute the time difference between two dates or times.'
  write(*,'(A)') 'An EVEN number of arguments is required - if date/time 1 is specified up to hours, so must date/time 2.'
  write(*,'(A)') 'Daylight savings are NOT taken into account!'
  call syntax_print('[options]  <year1> <month1> <day1>  <hour1> <minute1> <second1>  <year2> <month2> <day2>  <hour2> <minute2> <second2>')
  
  write(*,'(A)') 'Examples:'
  write(*,'(A)') '  '//trim(program_name)//'  2020 1 1  2021 1 1                             # The time difference between 2020-01-01 and 2021-01-01 is 366 days.'
  write(*,'(A)') '  '//trim(program_name)//'  2020  2021                                     # The time difference between 2020(-01-01) and 2021(-01-01) is 366 days.'
  write(*,'(A)') '  '//trim(program_name)//'  -t  9 30  20 0                                 # The time difference between 09:30 and 20:00 is 10.5 hours.'
  write(*,'(A)') '  '//trim(program_name)//'  -t -D  9 30  20 0                              # The time difference between 09:30 and 20:00 is 0.4375 days.'
  write(*,'(A)') '  '//trim(program_name)//'  2020 1 2  3 4 5.678  2021 2 3  4 5 6.789       # The time difference between 2020-01-02 03:04:05.678 and 2021-02-03 04:05:06.789 is 398.042374 days.'
  write(*,'(A)') '  '//trim(program_name)//'  -S  2020 1 2  3 4 5.678  2021 2 3  4 5 6.789   # The time difference between 2020-01-02 03:04:05.678 and 2021-02-03 04:05:06.789 is 34390861.111 seconds.'
  
  call getopt_long_help(longopts,1,1)  ! Print help
  
  stop
end subroutine print_syntax_quit
!***********************************************************************************************************************************


