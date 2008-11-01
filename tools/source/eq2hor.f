!Convert equatorial coordinates to horizontal (az,alt)

program eq2hor
  use constants
  implicit none
  !real*8 :: d,h,m,s,l,b,hms2rad,dms2rad
  real*8 :: ra,dec,eps,rev,gmst,calcgmst,dpsi,eps0,deps
  real*8 :: lat,lon,agst,hh,az,alt,jd,jde,t,deltat,calcdeltat
  integer :: iargc
  character :: str*99
  
  call declconst
  call readnutation
  call readdeltat
  eps = 0.409092804222d0 !J2000.0
  
  if(iargc().eq.5) then
     call getarg(1,str)
     read(str,*)lat
     call getarg(2,str)
     read(str,*)lon
     call getarg(3,str)
     read(str,*)jd
     call getarg(4,str)
     read(str,*)ra
     call getarg(5,str)
     read(str,*)dec
  else
     write(*,'(A)')'  Syntax:  eq2horiz <lat> <lon> <jd> <ra> <dec>  (degrees (E=-) and hours)'
     stop
  end if
  
  ra = ra*h2r
  dec = dec*d2r
  lat = lat*d2r
  lon = lon*d2r
  
  
  !write(6,'(A33,$)')'Input right ascension (h,m,s): '
  !read*,h,m,s
  !ra = hms2rad(h,m,s)
  !write(6,'(A29,$)')'Input declination (d,m,s): '
  !read*,d,m,s
  !dec = dms2rad(d,m,s)
  
  
  deltat = calcdeltat(jd)
  jde = jd + deltat/86400.d0
  t = (jde-2451545.d0)/365250.d0 		!Julian Millennia after 2000.0 in dynamical time, the tau in Meeus
  call nutation(t,dpsi,eps0,deps)	    ! Calculate nutation
  eps = eps0 + deps
  !print*,jd,jde,deltat,t
  
  gmst = calcgmst(jd)		!Greenwich mean siderial time
  agst = rev(gmst + dpsi*dcos(eps))	!Correction for equation of the equinoxes -> Gr. apparent sid. time
  !agst = rev(gmst)	!Correction for equation of the equinoxes -> Gr. apparent sid. time
  
  call eq2horiz(ra,dec,lat,lon,agst,hh,az,alt)
  
  write(6,'(A30,F13.8)')'ra:   ',ra*r2h
  write(6,'(A30,F13.8)')'dec:  ',dec*r2d
  write(6,'(A30,F13.8)')'gmst: ',rev(gmst)*r2h
  write(6,'(A30,F13.8)')'agst: ',rev(agst)*r2h
  write(6,'(A30,F13.8)')'hh:   ',rev(hh)*r2h
  write(6,'(A30,F13.8)')'az:   ',rev(az+pi)*r2d
  write(6,'(A30,F13.8)')'alt:  ',alt*r2d
end program eq2hor
