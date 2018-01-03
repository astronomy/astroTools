!> \file planetdata.f90  Display the rise and set times of the Sun, the Moon and the planets for a given date and time, their
!                        positions and mutual angular separations, and distances to some bright stars close to the ecliptic.


!  Copyright (c) 2002-2018  AstroFloyd - astrofloyd.org
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
!> \brief  Display the rise/set times, positions and distances of/between the Sun, the Moon and the planets

program planetdata
  use SUFR_kinds, only: double
  use SUFR_constants, only: pi,r2d,r2h,h2r, enpname !, d2r
  use SUFR_angles, only: rv,rev,rev2, asep, wdstr_ens
  use SUFR_date_and_time, only: cal2jd
  use SUFR_angle2string, only: dms, ddms2, ams, prs, ddd2
  use SUFR_time2string, only: hm, hms
  use SUFR_text, only: dbl2str
  use SUFR_command_line, only: get_command_argument_i, get_command_argument_d
  
  use TheSky_planets, only: planet_position
  use TheSky_riset, only: riset, riset_ad
  use TheSky_stars, only: calcstars
  use TheSky_data, only: TheSky_readdata, readmoondata
  use TheSky_planetdata, only: planpos,nplanpos, plcon
  use TheSky_stardata, only: conabr, starra,stardec,starnames, nstars
  use TheSky_local, only: tz
  use TheSky_datetime, only: calctime, set_date_and_time, get_date_and_time, set_date_and_time_to_system_clock, gettz
  use TheSky_datetime, only: print_date_time_and_location
  use TheSky_functions, only: const_id, ds1  !, set_TheSky_location
  use TheSky_visibility, only: limmag_jd_pl
  
  use AT_general, only: astroTools_init
  
  implicit none
  integer :: year,month,dy, hour,minute, tc
  integer :: pli,plj,pl(10),pls(-1:9),plor(11),npl, sti, narg,  showplandata,showriset,showdist,showstars
  real(double) :: day,second, jd,jd0,jde,ut,  rt,tt,st,rh,ta,sh, risetdat(19,6), alt
  real(double) :: dist(-1:9,-1:9),topra(-1:9),topdec(-1:9),pld(nplanpos), plandat(19,nplanpos), dist2(nstars,-1:9), limMag
  character :: twilight(4,3)*(8),twilights(4)*(12), twlstr(11,3)*(12),twlstri(3)*(12), magContr*(9)
  
  
  ! Set constants and read data:
  call astroTools_init()       ! Initialise astroTools, libTheSky and libSUFR
  call TheSky_readdata()       ! Read TheSky data
  call readmoondata()          ! Moon data are read separately, since it's a large set
  
  
  ! Hardcoded location, overriding the location from the astroTools settings file:
  ! Greenwich, using UT:  51.4778 N; 0 E; 10m; TZ0=0.0;  DST type = 0 (no DST):
  !call set_TheSky_location(51.4778d0*d2r, 0.d0, 10.d0, 0.d0, 0)
  ! Chicago, IL, USA;   41.9 N; -87.7 E; 180 m;  TZ0=-6.00  (CST) DST type: 2 (USA):
  !call set_TheSky_location(41.9d0*d2r, -87.7d0*d2r, 180.d0, -6.d0, 2)
  
  
  ! Flip some hard-coded switches:
  tc            = 1     ! Topocentric coordinates: 0-no (geocentric), 1-yes
  showriset     = 1     ! Print rise & set info
  showplandata  = 1     ! Print planetary positions
  showdist      = 1     ! Print distance matrix
  showstars     = 1     ! Show stars in distance matrix + stellar rise/set times
  
  
  ! Choose which bodies to print data for, and in which order:
  !       ES,M,m,v,S,m,j,s,u,n,p
  pls = [0,1,1,1,1,1,1,1,1,1,0]  ! Planets switch (1=display) - Sun, Moon and all true celestial planets
  !pls = [0,1,0,0,1,0,0,0,0,0,0]  ! Planets switch (1=display) - Sun and Moon only
  
  ! Planet order: Su,Mo, Ve,Me, Ma,Ju,Sa, Ne,Ur, Pl, ES:
  plor = [3,0, 1,2, 4,5,6, 7,8, 9, -1]
  
  
  ! Work out the 'planet' order:
  npl = sum(pls)
  do pli=-1,9
     if(pls(pli).gt.0) then
        do plj=1,11
           if(plor(plj).eq.pli) pl(plj) = pli  ! Planet pli comes in place plj
        end do
     end if
  end do
  
  
  ! Set default date and time, and overrule them with command-line arguments if desired:
  call set_date_and_time_to_system_clock()      ! Use the system clock as local time by default
  call get_date_and_time(year,month,dy, hour,minute,second)  ! Get the current TheSky date and time
  day = dble(dy)
  
  narg = command_argument_count()
  if(narg.ge.3) then
     hour = 0  ! Midnight local time
     minute = 0
     second = 1.d-9
     call get_command_argument_i(1, year)
     call get_command_argument_i(2, month)
     call get_command_argument_d(3, day)
     if(narg.gt.3) call get_command_argument_i(4, hour)
     if(narg.gt.4) call get_command_argument_i(5, minute)
     if(narg.gt.5) call get_command_argument_d(6, second)
     
  else if(narg.eq.2) then
     call get_command_argument_i(1, hour)
     call get_command_argument_i(2, minute)
     second = 1.d-9
     
  else
     write(*,*)
     write(*,'(A)') '  This program shows detailed planet data for a certain moment'
     write(*,'(A)') '  Syntax:'
     write(*,'(A)') '     planetdata <hour> <minute>  (assumes today)'
     write(*,'(A)') '     planetdata <year> <month> <day> [<hour> [<minute> [<second>]]]'
     write(*,*)
     
     write(*,'(/,A,/)') '  Continueing, using the current system time...'
  end if
  
  
  !************************************************************************
  !***   Time and location
  !************************************************************************
  
  call set_date_and_time(year,month,floor(day), hour,minute,second)  ! Set the date and time
  
  ! The second and third line below will become obsolete from libTheSky v0.3.3 onwards!
  call calctime(ut,jd,jde)  ! Compute JD
  tz = gettz(jd)            ! Get proper timezone for the current location (DST?)
  call calctime(ut,jd,jde)  ! Recompute JD
  
  call print_date_time_and_location(6,1,0, ut,jd,jde)  ! Unit 6 - stdOut; 1 line before, 0 after
  
  
  
  
  
  !************************************************************************
  !***   Twilight, rise, transit and set data
  !************************************************************************
  
  if(showriset.eq.1) then
     
     ! Twilight:
     jd0 = cal2jd(year,month,day+0.5d0)
     
     twilights = ['    Daylight', '       Civil','    Nautical','Astronomical']
     twlstr = ''
     
     do plj=1,4
        call riset(jd0,3, rt,tt,st, rh,ta,sh, dble(-6*(plj-1)))  
        twilight(plj,1:3) = [hms(rv(rt)), hms(rv(tt)), hms(rv(st))]
        twlstr(plj,:) = [twilights(plj),' '//twilight(plj,1)//' - ',twilight(plj,3)//'    ']
     end do
     
     
     ! Rise, transit and set data:
     write(*,*)
     write(*,'(/,A)') '********** RISE TRANSIT SET and TWILIGHT DATA **********'
     
     write(*,'(A9,5x,A10,A9,A5,5x,A10,A9,5x,A10,A9,A5,14x,A14,A13,A8)')  &
          'Planet ','Rise time','azimuth','wd','Trans time','altitude','Set time','azimuth','wd','Twilights:'
     
     do pli=1,npl
        call riset(jd0,pl(pli),rt,tt,st,rh,ta,sh,0.d0)
        risetdat(pli,1:6) = [rt,tt,st,rh,ta,sh]
     end do
     
     do pli=1,npl
        rt = risetdat(pli,1)
        tt = risetdat(pli,2)
        st = risetdat(pli,3)
        rh = risetdat(pli,4)
        ta = risetdat(pli,5)
        sh = risetdat(pli,6)
        twlstri(:) = twlstr(pli,:)
        
        if(pl(pli).eq.3 .or. pl(pli).eq.1 .or. pl(pli).eq.4 .or. pl(pli).eq.7) write(*,*)  ! Empty line before Sun, Mer, Mar, Ur
        
        write(*,'(A9,5x,A10,F8.1,A1,A5,5x,A10,F8.1,A1,5x,A10,F8.1,A1,A5,14x,A14,A13,A8)')  &
             enpname(pl(pli)),hms(rv(rt)),rev(rh+pi)*r2d,'d',trim(wdstr_ens(rh+pi)),hms(rv(tt)),rev2(ta)*r2d,'d', &
             hms(rv(st)),rev(sh+pi)*r2d,'d',trim(wdstr_ens(sh+pi)),twlstri
        
     end do  ! pl
     
  end if  ! showriset
  
  
  
  
  
  !************************************************************************
  !***   Planets observational data
  !************************************************************************
  
  if(showplandata.eq.1) then
     write(*,*)
     if(tc.eq.0) write(*,'(/,A)') '********** GEOCENTRIC PLANET POSITION DATA **********'
     if(tc.eq.1) write(*,'(/,A)') '********** TOPOCENTRIC PLANET POSITION DATA **********'
     write(*,'(A139)') '  Planet    Longitude   Latitude     R (AU) Delta(AU)      R.A.' &
          //'       Dec.   Azim. W.D.   Alt.   Elon.  Diameter  Magn. Contr. Illum.   P.A.   Con'
     call calctime(ut,jd,jde)
     jd0 = jd
     
     do pli=1,npl
        call planet_position(jd0,pl(pli))
        pld = planpos
        
        topra(pl(pli))  = pld(25)  ! Use for angular separations, topocentric
        topdec(pl(pli)) = pld(26)
        
        pld(5)  = pld(5)*r2h
        pld(8)  = pld(8)*r2h
        pld(14) = pld(14)*100.
        pld(25) = pld(25)*r2h
        
        plandat(pli,:) = pld(:)
     end do
     
     do pli=1,npl
        pld(:) = plandat(pli,:)
        
        alt = pld(10)
        if(tc.gt.0) alt = pld(31)  ! Topocentric + refraction
        
        plcon(pl(pli)) = conabr(const_id(jd,rev(pld(5)*h2r),pld(6)))
        
        ! Magnitude contrast:
        magContr = '-'
        if(pl(pli).ne.3 .and. alt.gt.0.d0) then
           limMag = limmag_jd_pl(jd, pl(pli))       ! Limiting magnitude
           magContr = dbl2str(pld(13) - limMag, 1)  ! Magnitude contrast
        end if
        
        if(pl(pli).eq.3 .or. pl(pli).eq.1 .or. pl(pli).eq.4 .or. pl(pli).eq.7) write(*,*)  ! Empty line before Sun, Mer, Mar, Ur
        
        write(*,'(A9,1x,2A11,A11,2A10,A11,F7.1,A1,A4,A8,F7.1,A1, A10,1x,F6.1,A7,F6.1,A1,F6.1,A1,A6)')  &
             enpname(pl(pli)),dms(pld(1+tc*20)),ddms2(pld(2+tc*20)),ds1(pld(3+tc*20)),ds1(pld(4+tc*20)), &
             hms(pld(5+tc*20)),ddms2(pld(6+tc*20)), &
             rev(pld(9+tc*20)+pi)*r2d,'d',trim(wdstr_ens(pld(9+tc*20)+pi)),ddd2(alt), &
             pld(11)*r2d,'d',  ams(pld(12+tc*20)),pld(13),trim(magContr), pld(14),'%',pld(15)*r2d,'d',plcon(pl(pli))
        
     end do  ! pli
     
  end if  ! showplandata
  
  
  
  
  
  !************************************************************************
  !***   Angular separations and star rise/set
  !************************************************************************
  
  if(showdist.eq.1) then
     ! Planet-planet separations:
     
     do pli=1,npl
        do plj=1,npl
           if(pli.eq.plj) then
              dist(pl(pli),pl(plj)) = 0.d0
           else
              dist(pl(pli),pl(plj)) = asep(topra(pl(pli)),topra(pl(plj)),topdec(pl(pli)),topdec(pl(plj)))
           end if
        end do  ! plj
     end do  ! pli
     
     write(*,*)
     write(*,'(/,A)') '********** TOPOCENTRIC ANGULAR SEPARATIONS **********'
     write(*,'(12x,10A9)') (trim(enpname(pl(pli))),pli=1,npl)
     do pli=1,npl
        if(pl(pli).eq.3 .or. pl(pli).eq.1 .or. pl(pli).eq.4 .or. pl(pli).eq.7) write(*,*)  ! Empty line before Sun, Mer, Mar, Ur
        write(*,'(2x,A7,3x)', advance='no') enpname(pl(pli))
        do plj=1,npl
           if(plj.eq.pli) cycle    ! Diagonal
           if(plj.eq.pli+1) write(*,'(9x)', advance='no')  ! One after the diagonal
           write(*,'(2x,A)', advance='no') prs(dist(pl(pli),pl(plj)))
        end do
        
        
        write(*,*)
     end do
     
     
     if(showstars.eq.1) then
        ! Star-planet separations:
        do pli=1,npl
           write(*,'(9x)', advance='no')
        end do
        write(*,'(17x,A)') 'Ri.tm Tr.tm Se.tm   R.az T.a S.az'
        call calcstars(jd)
        do sti=1,6 ! nstars
           do plj=1,npl
              dist2(sti,pl(plj)) = asep(starra(sti),topra(pl(plj)),stardec(sti),topdec(pl(plj)))
           end do
           
           ! stardec(sti) = min(stardec(sti)*r2d + 20, 89. )*d2r  ! Test riset_ad when body doesn't rise/set (but transits!)
           
           call riset_ad(jd,starra(sti),stardec(sti),rt,tt,st,rh,ta,sh,0.d0)
           
           write(*,'(2x,A10)', advance='no') starnames(sti)
           do plj=1,npl
              write(*,'(2x,A7)', advance='no') prs(dist2(sti,pl(plj)))
           end do
           write(*,'(4x,  3A6,2x,  I4,A1,I3,A1,I4,A1)') hm(rt),hm(tt),hm(st), &
                nint(rev(rh)*r2d),'d',nint(rev(ta)*r2d),'d',nint(rev(sh)*r2d),'d'
           
        end do
     end if  ! if(showstars.eq.1) then
     
  end if  ! if(showdist.eq.1) then
  
  
  write(*,*)
end program planetdata
!***********************************************************************************************************************************


