MODULE convert
!
!  #[ description
!
! this module contains some functions to enable conversions
! between windvectors (u,v) and windvectors (speed,dir) and to convert between
! different coordinate systems on the Earths surface
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and
! Meteo France.
!
!     Unit Name:         convert
!
!     Created on:        15-09-2004
!
!     Last Modified on:  $Date: 2016-12-02 11:29:41 +0100 (Fri, 02 Dec 2016) $
!
!     Modifications Log:
!
!  $Id: convert.F90 12091 2016-12-02 10:29:41Z verspeek $
! 
!  convert.F90 10675 2015-05-26 09:39:48Z verhoefa $
! 
!  convert.F90 10674 2015-05-26 09:35:20Z verhoefa $
!
!  convert.F90 10663 2015-05-22 07:28:39Z josdekloe $
!
!  convert.F90 10591 2015-04-28 09:10:31Z josdekloe $
! 
!  convert.F90 9923 2014-09-17 10:04:46Z verhoefa $
! 
!  convert.F90 9247 2013-12-03 07:41:59Z verhoefa $
! 
!  convert.F90 8271 2012-10-05 11:56:35Z verspeek $
! 
!  convert.F90 7734 2011-11-15 06:59:22Z verhoefa $
!
! Revision 1.33  2010/10/13 09:33:14  vogelzan
! Set WVC orientation to -90 degrees when both WVCs in
! routine WVC_Orientation have the same longitude
!
! Revision 1.32  2010/09/21 11:51:40  verhoefa
! Added routine to extend the swath
!
! Revision 1.31  2009/05/26 08:17:04  kloedej
! updated internet link to source for earth radius
!
! Revision 1.30  2008/12/19 10:45:46  verspeek
! added angle conventions in relative_angle
!
! Revision 1.29  2008/12/10 13:42:50  verspeek
! routine relative_angle: check on missing value of antennadir
!
! Revision 1.28  2008/10/03 13:53:17  verspeek
! added function logDiff
!
! Revision 1.27  2008/10/03 12:59:16  verspeek
! added function trnsfrm
!
! Revision 1.26  2008/10/01 14:20:00  verspeek
! added function relative_angle
!
! Revision 1.25  2008/07/17 14:56:10  verhoefa
! Fixed some compiler warnings
!
! Revision 1.24  2006/10/25 13:03:29  kloedej
! repaired the folding marks
!
! Revision 1.23  2006/09/26 13:05:33  vogelzan
! Added routine WVC_Orientation to calculate the orientation of
! two WVC's from their geographical positions.
! This routine is needed by 2DVAR and the programs for statistical
! analysis of wind fields.
! It (partly) replaced routine SetAlpha in Ambrem2DVAR.
!
! Revision 1.22  2006/08/22 07:58:31  vogelzan
! Included important comment in routine uv2met.
!
!  #]
  !  #[ USE statements
  USE numerics, only: missing_real, missing_indicator_real
  USE numerics, only: r_, r8_
  !  #]
  !  #[ parameters
  implicit none
  private ! all is private to this module, unless stated otherwise

  real(r8_), parameter :: dpi     = 3.14159265358979323846_r8_ ! = pi
  real(r_),  parameter :: pi      = 3.1415926536_r_  ! = pi
  real(r_),  parameter :: two_pi  = 2.0_r_ * pi   ! = pi*2
  real(r_),  parameter :: rad2deg = 180.0_r_/pi   ! = 180/pi
  real(r_),  parameter :: deg2rad = pi/180.0_r_   ! = pi/180

  real(r_),  parameter :: R_earth_equatorial = 6378.1_r_ ! [km]
  real(r_),  parameter :: R_earth_polar      = 6356.8_r_ ! [km]
  ! source:
  ! http://astrogeology.usgs.gov/Projects/BrowseTheSolarSystem/earth.html
  ! outdated link: http://wwwflag.wr.usgs.gov/USGSFlag/Space/wall/earth.html

  !  #]
  !  #[ user interface
  ! specify the public functions of this MODULE
  public :: speeddir_to_u, speeddir_to_v
  public :: speeddir_to_v_oce, speeddir_to_u_oce, uv_to_dir_oce
  public :: uv_to_dir, uv_to_speed
  public :: met2uv, uv2met
  public :: rotuv
  ! the following functions/routines/parameters are needed in
  ! the OrbitModule of the ADM project
  public :: latlon2xyz, xyz2latlon
  public :: orbit2xyz, xyz2orbit
  public :: WVC_Orientation
  public :: my_arctan, my_arctan_rad
  public :: dpi, pi, deg2rad, rad2deg
  public :: R_earth_equatorial, R_earth_polar
  public :: get_angle_distance, get_distance
  public :: relative_angle
  public :: trnsfrm
  public :: logDiff
  public :: extend_wvcs
  public :: average_2_angles
  !  #]
contains

  ! the next 2 functions are used by sdp, and are basically identical
  ! to the following 4 functions, so probably they should be merged
  ! if someone finds the time to do it ...
  ! Just leave it like this; it is more efficient so.

  subroutine met2uv(spd,dir,u,v)
    !  #[
    !
    ! Description: conversion  meteorlogical convention to (u,v)
    !
    !   Hans Bonekamp KNMI 2003
    !
    ! arguments
    real(r_), intent(in)   :: spd
    real(r_), intent(in)   :: dir   ! in degrees  clockwise  from north
    real(r_), intent(out)  :: u, v
 
    if (missing_real(spd)) then
      u = missing_indicator_real
      v = missing_indicator_real
    else
      u = - spd*sin(dir*deg2rad)
      v = - spd*cos(dir*deg2rad)
    endif

  end subroutine met2uv
  !  #]

  subroutine uv2met(u,v,spd,dir)
    !  #[
    !
    ! Description: conversion  meteorlogical convention to (u,v)
    !   See http://www.atd.ucar.edu/rtf/facilities/isff/wind_ref.shtml
    !
    !   Hans Bonekamp KNMI 2003
    !
    ! arguments
    real(r_), intent(in)    :: u, v
    real(r_), intent(out)   :: spd
    real(r_), intent(out)   :: dir   ! in degrees  clockwise  from north

    !------------------------------------------------------------------------------!
    ! REMARK: Note that the output of atan2 lies within (-pi,+pi]. As formulated   !
    ! below, the direction (in degrees) is in the interval (0,360] - as it should  !
    ! be in the meteorological convention. The construction                        !
    !   dir=atan2(-u,-v)*rad2deg                                                   !
    ! seems more efficient, but returns dir in (-180,+180].                        !
    !------------------------------------------------------------------------------!
 
    if (missing_real(u)) then
      spd = missing_indicator_real
      dir = missing_indicator_real
    else
      spd  =  sqrt(u*u+v*v)
      if (spd == 0.0) then
        dir=180.0
      else
        dir = atan2(u,v)*rad2deg + 180.0  ! clockwise rotating
      endif
    endif

  end subroutine uv2met
  !  #]

  !--------------------------------
  !  #[ note for the following 4 functions:
  !--------------------------------
  !
  ! winddirection is with respect to south,
  ! (so a southward wind has direction 0)
  ! clockwise increment=positive
  ! (meteorological convention, see notebook Jos, p.48)
  ! u=eastward component
  ! v=northward component
  !--------------------------------
  !  #]
  !--------------------------------
  function speeddir_to_u(sp,dir) result(u)
    !  #[ uses meteorological convention
    real(r_) :: sp,dir ! input
    real(r_) :: u      ! output

    if (missing_real(sp) .or. missing_real(dir)) then
      u = missing_indicator_real
    else
      u = -1.0*sp*sin(dir*deg2rad)
    endif

  end function speeddir_to_u
  !  #]

  function speeddir_to_v(sp,dir) result(v)
    !  #[ uses meteorological convention
    real(r_) :: sp,dir ! input
    real(r_) :: v      ! output

    if (missing_real(sp) .or. missing_real(dir)) then
      v = missing_indicator_real
    else
      v = -1.0*sp*cos(dir*deg2rad)
    endif

  end function speeddir_to_v
  !  #]

  function uv_to_dir(u,v) result(dir)
    !  #[ uses meteorological convention
    real(r_) :: u,v ! input
    real(r_) :: dir ! output

    if (missing_real(u) .or. missing_real(v)) then
      dir = missing_indicator_real
    else
      dir = 270.0-my_arctan(u,v)
      if (dir .lt. 0.0) dir = 360.0 + dir
    endif

  end function uv_to_dir
  !  #]

  function uv_to_speed(u,v) result(speed)
    !  #[ does not depend on winddir convention
    real(r_) :: u,v   ! input
    real(r_) :: speed ! output

    if (missing_real(u) .or. missing_real(v)) then
      speed = missing_indicator_real
    else
      speed = sqrt(u*u+v*v)
    endif

  end function uv_to_speed
  !  #]

  !--------------------------------
  ! same functions but now for oceanographic convention
  !--------------------------------
  function speeddir_to_u_oce(sp,dir) result(u)
    !  #[ uses oceanographic convention
    real(r_) :: sp,dir ! input
    real(r_) :: u      ! output

    ! local variable
    real(r_) :: tmp_dir 

    tmp_dir = dir
    if (.not. missing_real(tmp_dir)) then
       tmp_dir = tmp_dir + 180.0
       if (tmp_dir .gt. 360.0) tmp_dir = tmp_dir - 360.0
    endif
    u = speeddir_to_u(sp, tmp_dir)
    
  end function speeddir_to_u_oce
    !  #]
  function speeddir_to_v_oce(sp,dir) result(v)
    !  #[ uses oceanographic convention
    real(r_) :: sp,dir ! input
    real(r_) :: v      ! output

    ! local variable
    real(r_) :: tmp_dir 

    tmp_dir = dir
    if (.not. missing_real(tmp_dir)) then
       tmp_dir = tmp_dir + 180.0
       if (tmp_dir .gt. 360.0) tmp_dir = tmp_dir - 360.0
    endif
    v = speeddir_to_v(sp, tmp_dir)
    
  end function speeddir_to_v_oce
    !  #]
  function uv_to_dir_oce(u,v) result(dir)
    !  #[ uses oceanographic convention
    real(r_) :: u,v ! input
    real(r_) :: dir ! output

    dir = uv_to_dir(u,v)
    if (.not. missing_real(dir)) then
       dir = dir + 180.0
       if (dir .gt. 360.0) dir = dir - 360.0
    endif
    
  end function uv_to_dir_oce
    !  #]
  !--------------------------------
  subroutine rotuv(alpha,u,v,ur,vr)
    !  #[
    !  rotate wind vector
    ! this routine was first part of the Ambrem2DVAR module.
    ! It rotates a vector (u,v) over an angle alpha to obtain (ur,vr).
    !
    implicit none
    ! arguments:
    real ,intent(in)   :: alpha
    real ,intent(in)   :: u
    real ,intent(in)   :: v
    real ,intent(out)  :: ur
    real ,intent(out)  :: vr
    ! local
    real :: CosAlpha
    real :: SinAlpha
    !
    CosAlpha = cos(alpha)
    SinAlpha = sin(alpha)
    ur       =     u*CosAlpha + v*SinAlpha
    vr       =  -  u*SinAlpha + v*CosAlpha

  end subroutine rotuv
  !  #]

  ! these functions were first part of the Grib module, and calculate
  ! the angle-distance and km-distance between 2 latlon pairs

  function get_angle_distance(lat1,lon1,lat2,lon2) result(angle_dist)
    !  #[
    ! determine the angle (in degrees) along a circle on the earths surface
    ! between 2 sets of lat-lon coordinates
    ! (assuming that the shape of the earth is a perfect globe)

    ! compare with the old PRESCAT code in NODEQC.F, lines 210/214
    ! calculation of zDlat, zDlon

    real(r_), intent(in) :: lat1,lon1,lat2,lon2
    real(r_) :: angle_dist

    ! loal variables
    real(r_) :: lat_diff_deg, lon_diff_deg
    real(r_) :: lat_min_deg, lat_min_rad
    real(r_) :: distance_sq

    ! get the differences
    lat_diff_deg = lat2 - lat1
    lon_diff_deg = lon2 - lon1

    ! determine lowest latitude of both positions
    lat_min_deg = min( abs(lat1),abs(lat2) )

    ! convert lowest latitude to radians
    ! remind: 180./pi = 57.295780
    lat_min_rad = lat_min_deg/57.29577951

    ! account for the fact that lon=-179.9 and lon=179.9 are neighbouring
    IF (lon_diff_deg .gt.  180.) lon_diff_deg = 360. - lon_diff_deg
    IF (lon_diff_deg .lt. -180.) lon_diff_deg = 360. + lon_diff_deg

    ! the triangle in lat-lon grid defining the distance between the
    ! two nodes, has sides of length (R=radius of the earth):
    !     R*lat_difference_rad                      (in north-south direction)
    ! and R*cos(lat_minimum_rad)*lon_difference_rad (in east-west direction)
    !
    ! so the square of the third side (and the distance) is:
    !     R^2*lat_difference_rad^2 +
    !     R^2*cos(lat_minimum_rad)^2*lon_difference_rad^2

    ! For equality of 2 nodes, we use the condition that they are
    ! within a distance equivalent to 0.25 degrees north-south
    ! distance, so within R*0.25/57.29577951
    ! This is in km a distance of ca. 40000/(360*4) = 27.8 km

    ! size of the earth: 12756.3 * !dpi = 40075.098 km (omtrek)
    ! see http://nl.wikipedia.org/wiki/Aarde

    ! for simplicity, we remove the factors R and 57.29577951
    ! from this calculation, and the following remains:

    distance_sq = lon_diff_deg**2 * cos(lat_min_rad)**2 + lat_diff_deg**2
    angle_dist = sqrt(distance_sq)

  end function get_angle_distance
  !  #] ----------------------------------------------------------
  function get_distance(lat1,lon1,lat2,lon2) result(distance)
    !  #[
    real(r_) :: lat1,lon1,lat2,lon2 ! input
    real(r_) :: distance            ! result

    ! local variables
    real(r_) :: angle_distance

    angle_distance = get_angle_distance(lat1,lon1,lat2,lon2)
    distance = 2.*pi*R_earth_equatorial*angle_distance/360.

  end function get_distance
    !  #]

  ! these 2 routines are taken from SDP/sws/SwsSupport.F90
  subroutine xyz2latlon(x,y,z,lat,lon)
    !  #[
    !      lat lon  of  x,y,z
    ! R = radius of the circular satellite orbit
    !     = assumed to be 1 inside this routine

    ! arguments:
    real(r_), intent(in)   :: x,y,z
    real(r_), intent(out)  :: lat,lon
    !local:
    real(r_)               :: rcos
    ! --
    rcos   = sqrt(x**2+y**2)
    if ((z == 0.0d0) .and. (rcos == 0.0d0)) then
       lat = 0.0d0
    else
       lat = atan2(z,rcos)*rad2deg
    endif
    if ((y == 0.0d0) .and. (x == 0.0d0)) then
       lon = 0.0d0
    else
       lon = atan2(y,x)*rad2deg
    endif
    !-
  end subroutine xyz2latlon
  !  #]

  subroutine latlon2xyz(lat,lon,x,y,z)
    !  #[
    !
    ! Description:
    ! R = radius of the circular satellite orbit
    !     = assumed to be 1 inside this routine
    !
    !   Hans Bonekamp  KNMI, 2003
    !
    implicit NONE
    ! arguments:
    real(r_), intent(in)   :: lat,lon
    real(r_), intent(out)  :: x,y,z
    ! local
    real(r_)               :: rlon,rlat
    real(r_)               :: coslat
    !--
    rlon   = lon*deg2rad
    rlat   = lat*deg2rad
    coslat = cos(rlat)
    x      = cos(rlon)*coslat
    y      = sin(rlon)*coslat
    z      = sin(rlat)
  end subroutine latlon2xyz
  !  #]

  ! routines to convert orbit angles to x,y,z (and from there to lat-lon)
  ! and vice versa (see notebook Jos, p. 71, for formulas)

  subroutine orbit2xyz(p,a,rot_z,x,y,z)
    !  #[
    real(r_), intent(in)  :: p,a,rot_z
    real(r_), intent(out) :: x,y,z

    ! R = radius of the circular satellite orbit
    !     = assumed to be 1 inside this routine
    ! p = the angle of the orbit plane wrt the xz plane in deg
    ! a = the angle within the orbit in deg (angle=0 equals lat=0,lon=0)
    ! rot_z = rotation angle around the z-axis
    !         (corresponding to a start lon-value, and an offset determined
    !          from the time delay*360/24

    ! local variables
    real(r_) :: tmp1_x, tmp1_y, tmp1_z, rot_x
    real(r_) :: tmp2_x, tmp2_y, tmp2_z, a_rad

    a_rad = a*deg2rad

    tmp1_x = cos(a_rad)
    tmp1_y = 0.
    tmp1_z = sin(a_rad)

    rot_x = p ! still in degrees

    call rotate_around_x(tmp1_x,tmp1_y,tmp1_z, rot_x, tmp2_x,tmp2_y,tmp2_z)

    call rotate_around_z(tmp2_x,tmp2_y,tmp2_z, rot_z, x,y,z)

  end subroutine orbit2xyz
    !  #]

  subroutine xyz2orbit(x,y,z,p,a1,rot_z1,a2,rot_z2)
    !  #[
    real(r_), intent(in)   :: x,y,z,p
    real(r_), intent(out)  :: a1,a2,rot_z1,rot_z2

    real(r_), parameter :: eps = 1.0e-3

    ! R = radius of the circular satellite orbit
    !     = assumed to be 1 inside this routine
    ! p = the angle of the orbit plane wrt the xz plane in deg
    ! a = the angle within the orbit in deg (angle=0 equals lat=0,lon=0)
    ! rot_z = rotation angle around the z-axis
    !         (corresponding to a start lon-value, and an offset determined
    !          from the time delay*360/24
    ! note: since from the x,y,z position it can not be inferred if the
    ! satellite is in the ascending or descending part of the orbit,
    ! two possible results are given for a and rot_z.

    ! local variables
    real(r_) :: p_rad, x_prime, y_prime, x_tmp, y_tmp, z_tmp, tmp
    real(r_) :: rot_z1a, rot_z1b, rot_z2a, rot_z2b, r_sq

    integer :: which_sol_a1, which_sol_a2
    logical :: debug

    ! convert input to radians
    p_rad = p*deg2rad

    ! check whether radius = 1
    r_sq = x*x+y*y+z*z
    if (abs(r_sq-1) .gt. eps) then
       print *,"ERROR inside  xyz2orbit()"
       print *,"orbit radius is assumed to be 1 in this routine,"
       print *,"but the supplied x,y,z result in a radius of: ",sqrt(r_sq)
       stop
    end if

    debug = .false.
    !debug = .true.
    !if (p .eq. 90.) debug = .true.

    ! following calculation is in radians
    if (abs(abs(p)-90.) .lt. eps) then
       ! orbit is exactly above the equator now, so z is always zero
       if (abs(z) .lt. eps) then
          a1 = asin(y)
          a2 = pi - a1
       else
          print *,"ERROR in xyz2orbit():"
          print *,"for p=90. z should always be zero, but it is not...."
       end if

       rot_z1 = 0.
       rot_z2 = 0.

       if (debug) then
          print *,"rot_z1,a1 = ",rot_z1,a1
          print *,"rot_z2,a2 = ",rot_z2,a2
       end if

    else
       a1 = asin(z/cos(p_rad))
       a2 = pi - a1

       ! first case 1
       x_prime = cos(a1) ; y_prime = sin(a1)*sin(p_rad)

       if (abs(x*x+y*y-x_prime*x_prime-y_prime*y_prime) .gt. eps) then
          print *,"x,y check problems (a1) !!!!"
          print *,"length of vector [x,y] should be identical to lenght"
          print *,"of vector [x_prime,y_prime], since they only differ"
          print *,"because of a rotation around the z-axis..."
          print *,"p=",p," a1 = ",a1*rad2deg
          print *,"x=",x," y=",y," z=",z
          print *,"x_prime=",x_prime," y_prime=",y_prime
          print *,"x*x+y*y=",x*x+y*y
          print *,"x_prime*x_prime+y_prime*y_prime=",&
               x_prime*x_prime+y_prime*y_prime
          stop
       end if

!       tmp = (x_prime*y - y_prime*x)/(x_prime*x_prime + y_prime*y_prime)
       tmp = (x_prime*x + y_prime*y)/(x_prime*x_prime + y_prime*y_prime)

       ! protect the acos() function from the numerical coincidence that
       ! tmp is a little above 1 (this does happen sometimes !!)
       if (tmp .gt.  1.) tmp =  1.
       if (tmp .lt. -1.) tmp = -1.

!       rot_z1a = asin(tmp)! possible solutions are rot_z or pi-rot_z
!       rot_z1b = pi - rot_z1a

       rot_z1a = acos(tmp)    ! possible solutions are rot_z or -1*rot_z
       rot_z1b = -1.*rot_z1a

       ! then case 2
       x_prime = cos(a2) ; y_prime = sin(a2)*sin(p_rad)

       if (abs(x*x+y*y-x_prime*x_prime-y_prime*y_prime) .gt. eps) then
          print *,"x,y check problems (a2) !!!!"
          print *,"length of vector [x,y] should be identical to lenght"
          print *,"of vector [x_prime,y_prime], since they only differ"
          print *,"because of a rotation around the z-axis..."
          print *,"p=",p," a2 = ",a2*rad2deg
          print *,"x=",x," y=",y," z=",z
          print *,"x_prime=",x_prime," y_prime=",y_prime
          print *,"x*x+y*y=",x*x+y*y
          print *,"x_prime*x_prime+y_prime*y_prime=",&
               x_prime*x_prime+y_prime*y_prime
          stop
       end if

!       tmp = (x_prime*y - y_prime*x)/(x_prime*x_prime + y_prime*y_prime)
       tmp = (x_prime*x + y_prime*y)/(x_prime*x_prime + y_prime*y_prime)

       ! protect the acos() function from the numerical coincidence that
       ! tmp is a little above 1 (this does happen sometimes !!)
       if (tmp .gt.  1.) tmp =  1.
       if (tmp .lt. -1.) tmp = -1.

!       rot_z2a = asin(tmp)! possible solutions are rot_z or pi-rot_z
!       rot_z2b = pi - rot_z2a

       rot_z2a = acos(tmp)    ! possible solutions are rot_z or -1*rot_z
       rot_z2b = -1.*rot_z2a

       ! now we have 4 possible combinations of a and rot_z
       ! so do the reverse for all of them, and see what x,y,z we find

       which_sol_a1 = -1
       which_sol_a2 = -1

       ! case 1
       call orbit2xyz(p,a1*rad2deg,rot_z1a*rad2deg,x_tmp,y_tmp,z_tmp)
       if (debug) then
          print "(a,6(f10.5,1x))","p,a1,rot_z1a,x_tmp,y_tmp,z_tmp:",&
               p,a1*rad2deg,rot_z1a*rad2deg,x_tmp,y_tmp,z_tmp
       end if

       if ( (abs(x-x_tmp) .lt. eps) .and. &
            (abs(y-y_tmp) .lt. eps) .and. &
            (abs(z-z_tmp) .lt. eps)       ) then
          if (debug) print *,"case 1 is the wanted solution..."
          which_sol_a1 = 1
          rot_z1 = rot_z1a
       end if

       ! case 2
       call orbit2xyz(p,a1*rad2deg,rot_z1b*rad2deg,x_tmp,y_tmp,z_tmp)
       if (debug) then
          print "(a,6(f10.5,1x))","p,a1,rot_z1b,x_tmp,y_tmp,z_tmp:",&
               p,a1*rad2deg,rot_z1b*rad2deg,x_tmp,y_tmp,z_tmp
       end if

       if ( (abs(x-x_tmp) .lt. eps) .and. &
            (abs(y-y_tmp) .lt. eps) .and. &
            (abs(z-z_tmp) .lt. eps)       ) then
          if (debug) print *,"case 2 is the wanted solution..."
          if (which_sol_a1 .ne. -1) then
             !print *,"multiple solutions found !!!!!!! using the first one."
          else
             which_sol_a1 = 2
             rot_z1 = rot_z1b
          end if
       end if

       if (which_sol_a1 .eq. -1) then
          ! this should never happenn
          print *,"no valid solution found for a1=",a1*rad2deg
          stop
       endif

       ! case 3
       call orbit2xyz(p,a2*rad2deg,rot_z2a*rad2deg,x_tmp,y_tmp,z_tmp)
       if (debug) then
          print "(a,6(f10.5,1x))","p,a2,rot_z2a,x_tmp,y_tmp,z_tmp:",&
               p,a2*rad2deg,rot_z2a*rad2deg,x_tmp,y_tmp,z_tmp
       end if

       if ( (abs(x-x_tmp) .lt. eps) .and. &
            (abs(y-y_tmp) .lt. eps) .and. &
            (abs(z-z_tmp) .lt. eps)       ) then
          if (debug) print *,"case 3 is the wanted solution..."
          which_sol_a2 = 3
          rot_z2 = rot_z2a
       end if

       ! case 4
       call orbit2xyz(p,a2*rad2deg,rot_z2b*rad2deg,x_tmp,y_tmp,z_tmp)
       if (debug) then
          print "(a,6(f10.5,1x))","p,a2,rot_z2b,x_tmp,y_tmp,z_tmp:",&
               p,a2*rad2deg,rot_z2b*rad2deg,x_tmp,y_tmp,z_tmp
       end if

       if ( (abs(x-x_tmp) .lt. eps) .and. &
            (abs(y-y_tmp) .lt. eps) .and. &
            (abs(z-z_tmp) .lt. eps)       ) then
          if (debug) print *,"case 4 is the wanted solution..."
          if (which_sol_a2 .ne. -1) then
             !print *,"multiple solutions found !!!!!!! using the first one"
          else
             which_sol_a2 = 4
             rot_z2 = rot_z2b
          end if
       end if

       if (which_sol_a2 .eq. -1) then
          ! this should never happen
          print *,"no valid solution found for a2=",a2*rad2deg
          stop
       endif

    end if

    ! convert output to degrees
    a1 = a1*rad2deg ; rot_z1  = rot_z1 *rad2deg
    a2 = a2*rad2deg ; rot_z2  = rot_z2 *rad2deg

  end subroutine xyz2orbit
    !  #]
  subroutine rotate_around_x(x,y,z,rotx,xnew,ynew,znew)
    !  #[
    ! apply rotation around x-axis to orbit
    ! angle is given in degrees
    real(r_), intent(in)  :: x,y,z,rotx
    real(r_), intent(out) :: xnew,ynew,znew

    xnew = x ! no change when rotation around x
    ynew = z*sin(rotx*deg2rad)+y*cos(rotx*deg2rad)
    znew = z*cos(rotx*deg2rad)-y*sin(rotx*deg2rad)
  end subroutine rotate_around_x
    !  #]

  subroutine rotate_around_z(x,y,z,rotz,xnew,ynew,znew)
    !  #[
    ! apply rotation around z-axis to orbit
    ! angle is given in degrees
    real(r_), intent(in)  :: x,y,z,rotz
    real(r_), intent(out) :: xnew,ynew,znew

    xnew = x*cos(rotz*deg2rad)-y*sin(rotz*deg2rad)
    ynew = x*sin(rotz*deg2rad)+y*cos(rotz*deg2rad)
    znew = z ! no change when rotation around z
  end subroutine rotate_around_z
    !  #]

  Subroutine WVC_Orientation(Lam1,Phi1,Lam2,Phi2 , Alfa1,Alfa2)
    !  #[
!------------------------------------------------------------------------------!
! WVC_Orientation calculates the orientations of two WVC's with coordinates    !
! (Lam1,Phi1) and (Lam2,Phi2) in the same row, using a great spherical         !
! triangle with the North Pole as third point. If Lam1 = Lam2 the orientation  !
! is set to -90 degrees (i.e. pointing to the west).                           !
! The orientation is given as the angle in degrees of the local along-track    !
! direction measured counterclockwise from the north.                          !
! This routine is needed by 2DVAR for the transformation to diagonalize the    !
! error covariance matrices in the spatial frequency domain.                   !
!------------------------------------------------------------------------------!
  implicit none

  real, intent(in)   :: Lam1,Phi1      ! Geographical coordinates of WVC1 (deg)
  real, intent(in)   :: Lam2,Phi2      ! Geographical coordinates of WVC2 (deg)
  real, intent(out)  :: Alfa1,Alfa2    ! WVC orientations (deg)
!------------------------------------------------------------------------------!
! Local declarations.                                                          !
!------------------------------------------------------------------------------!
  real     :: SinPhi1, CosPhi1         ! Sines and cosines
  real     :: SinPhi2, CosPhi2
  real     :: dLam                     ! Difference in longitude,
  real     :: CosdLam, SinDLam         ! with its sine and cosine
  real     :: CosC, SinC_inv           ! Cosine of angle C (North Pole) and inverse of its sine
  real     :: Aux
!------------------------------------------------------------------------------!
! Initialize. Check positions.                                                 !
!------------------------------------------------------------------------------!
  Alfa1=missing_indicator_real
  Alfa2=missing_indicator_real

  if (missing_real(Lam1) .or. missing_real(Phi1)) return
  if (missing_real(Lam2) .or. missing_real(Phi2)) return
!------------------------------------------------------------------------------!
! Set angles, sines and cosines. Calculate orientations.                       !
!------------------------------------------------------------------------------!
  DLam=Lam2 - Lam1
  if (DLam == 0.0) then                ! Both points have same longitude
    Alfa1=90.0
    Alfa2=90.0
  else                                 ! Both points have different longitude
    SinPhi1=sin(Deg2Rad*Phi1)
    CosPhi1=cos(Deg2Rad*Phi1)
    SinPhi2=sin(Deg2Rad*Phi2)
    CosPhi2=cos(Deg2Rad*Phi2)

    SinDLam=sin(Deg2Rad*DLam)
    CosDLam=cos(Deg2Rad*DLam)

    CosC=SinPhi1*SinPhi2 + CosPhi1*CosPhi2*CosDLam
    SinC_inv=1.0/sqrt(1.0 - CosC*CosC)

    Aux = CosPhi2*SinDLam*SinC_inv
    if (Aux .ge.  1.0) Aux =  1.0
    if (Aux .le. -1.0) Aux = -1.0
    Alfa1=Rad2Deg*acos(Aux)

    Aux = CosPhi1*SinDLam*SinC_inv
    if (Aux .ge.  1.0) Aux =  1.0
    if (Aux .le. -1.0) Aux = -1.0
    Alfa2=Rad2Deg*acos(Aux)
  endif

  ! The acos function used above always yields angles between 0 and 180 degrees.
  ! This is valid for sun-synchronous polar orbiters but not for platforms
  ! moving from left to right over the world map, like ISS.
  ! Correct for this when necessary.
  if (Phi1 > Phi2) then       ! Lat of point 1 is larger than lat of point 2
    Alfa1=360.0-Alfa1
    Alfa2=360.0-Alfa2
  endif

  end subroutine WVC_Orientation
    !  #]

  function my_arctan(x,y) result(angle)
    !  #[
    ! a small support function, used by the above uv_to_dir conversion
    ! a function to calculate my own version of atan()
    ! making sure the result is between 0-360 (output in deg)

    real(r_) :: x,y   ! input
    real(r_) :: angle ! output

    IF (x .gt. 0.0) THEN
      IF (y .ge. 0.0) THEN
        angle = atan(y/x)*rad2deg
      ELSE
        angle = 360.0-atan(abs(y/x))*rad2deg
      END IF
    ELSE IF (x .lt. 0.0) THEN
      IF (y .ge. 0.0) THEN
        angle = 180.0-atan(abs(y/x))*rad2deg
      ELSE
        angle = 180.0+atan(abs(y/x))*rad2deg
      END IF
    ELSE  ! uitzondering, anders veroorzaakt x=0 delen door 0
      IF (y .ge. 0.0) THEN
        angle = 90.0
      ELSE
        angle = 270.0
      END IF
    END IF

  end function my_arctan
  !  #]

  function my_arctan_rad(x,y) result(angle)
    !  #[
    ! a similar function in radians (not used by me at the moment)
    ! a function to calculate my own version of atan()
    ! making sure the result is between 0-2*pi (output in radians)

    real(r_) :: x,y   ! input
    real(r_) :: angle ! output

    IF (x .gt. 0.0) THEN
      IF (y .ge. 0.0) THEN
        angle = atan(y/x)
      ELSE
        angle = two_pi-atan(abs(y/x))
      END IF
    ELSE IF (x .lt. 0.0) THEN
      IF (y .ge. 0.0) THEN
        angle = pi-atan(abs(y/x))
      ELSE
        angle = pi+atan(abs(y/x))
      END IF
    ELSE  ! uitzondering, anders veroorzaakt x=0 delen door 0
      IF (y .ge. 0.0) THEN
        angle = 0.5*pi
      ELSE
        angle = 1.5*pi
      END IF
    END IF

  end function my_arctan_rad
  !  #]

  function relative_angle(windDir, antennaDir, &
       windConvention, antennaConvention) result(relWinddir)
    !  #[ Calculate the relative wind direction
    !**********************************************************************
    ! Routine to calculate the relative direction of the wind
    ! with respect to the antenna upwind azimuth angle
    !
    ! Version : 2012-10-05 made convention parameters optional
    !
    ! Input   :
    !   windDir                      - wind direction wrt North (deg)
    !   antennaDir                   - antenna azimuth angle wrt North (deg)
    !   windConvention (optional)    - convention used for windDir
    !   antennaConvention (optional) - convention used for antennaDir
    !
    ! Output  :
    !   relWindir  - Wind direction angle difference
    !                in interval [0..360).
    !
    ! Note :
    ! 1. ERS/ASCAT:
    !      windDir   : meteorological convention for BUFR files,
    !                  oceanographic convention for netCDF files
    !     antennaDir : meteorological convention
    !    OCEANSAT/SEAWINDS:
    !      windDir   : meteorological convention for BUFR files,
    !                  oceanographic convention for netCDF files
    !      antennaDir: oceanographic convention
    !
    ! 2. When relative_angle is used as input for the GMF an extra 180. degree
    !    has to be added due to the GMF wind direction input convention.
    !**********************************************************************

    !use inversion, only: angle_convention_meteorological ! = 1
    !use inversion, only: angle_convention_oceanographic  ! = 2

    real, intent(in)              :: windDir, antennaDir
    integer, intent(in), optional :: windConvention, antennaConvention
    real                          :: relWinddir ! result

    relWinddir = missing_indicator_real
    if (missing_real(windDir)) return
    if (missing_real(antennaDir)) return

    relWinddir =  modulo(windDir-antennaDir, 360.)
    if (present(windConvention)) then
      if (present(antennaConvention)) then
        if (windConvention /= antennaConvention) then
          relWinddir =  modulo(windDir-antennaDir+180., 360.)
        endif
      endif
    endif

    ! The modulo function returns values between [0..360]
    ! instead of between [0..360) due to machine roundoffs of reals:
    if (relWinddir >= 360.) relWinddir = relWinddir-360.
  end function relative_angle
  !  #]

  function trnsfrm(a, trans) result(a_tr)
    !  #[
    !**********************************************************************
    ! Routine to calculate a transformation of a between linear, logarithmic
    ! and z-space.
    !
    ! Version : 2008-12-18 added missing value handling
    ! Version : 2006-09-25 negative values for 'lin_2_z' and 'z_2_lin' supported
    ! Version : 2006-06-26 'log_2_z' and 'z_2_log' added
    ! Version : 2006-03-27
    !
    ! Input   : a -
    ! Input   : trans - 'lin_2_log', 'log_2_lin',
    !                   'lin_2_z', 'z_2_lin',
    !                   'z_2_log', 'log_2_z'
    !
    ! Output  :
    !           a_tr - a transformed
    !
    ! Notes : 1.
    !**********************************************************************
    
    real, intent(in) :: a
    character(len=*), intent(in) :: trans
    real :: a_tr ! result

    ! Locals:
    real, parameter :: minimum_db_value = -40 ! dB
    real :: a_dum

    a_tr = missing_indicator_real
    if (missing_real(a)) then
      return
    endif

    a_tr = 0.

    select case(trim(trans))
    case('lin_2_log')
      if (a /= 0.) then
        if (a > 0.) then
          a_tr = 10.*log10(a)
        elseif (a < 0.) then
          a_tr = minimum_db_value
        endif
      else ! a == 0.
        ! a_tr = 0.
      endif
    case('log_2_lin')
      a_tr = 10.**(a/10.)
    case('lin_2_z')
      if (a >= 0.) then
        a_tr = a**0.625
      else
        a_tr = -(-a)**0.625
      endif
    case('z_2_lin')
      if (a >= 0.) then
        a_tr = a**1.6
      else
        a_tr = -(-a)**1.6
      endif
    case('log_2_z')
      a_dum = 10.**(a/10.)
      a_tr = a_dum**0.625
    case('z_2_log')
      if (a < 0.) then
        a_tr = minimum_db_value
      elseif (a > 0.) then
        a_dum = a**1.6
        a_tr = 10.*log10(a_dum)
      else ! (a == 0.)
        !a_tr = 0.
      endif
    case default
      write(6, *) 'trnsfrm ERROR - unknown transformation type.'
      write(6, *) 'trnsfrm trans: ', trans
    end select

  end function trnsfrm
    !  #]

  function logDiff(a, b) result(c)
    !  #[
    !**********************************************************************
    ! Routine to calculate the log of the quotient of a and b.
    !
    ! Version : 2006-03-27
    !
    ! Input   : a, b -
    !
    ! Output  :
    !           c = sign(a/b)*10*log10(abs(a/b))
    !
    ! Notes : 1.
    !**********************************************************************

    real, intent(in) :: a, b
    real :: c ! result

    c = 0.
    if (b /= 0.) then
      c = a/b
    endif
    if (c /= 0.) then
      if (c > 0.) then
        c = 10.*log10(c)
      elseif (c < 0.) then
        c = -10.*log10(-c)
      else ! c == 0.
        ! c = 0.
      endif
    endif

  end function logDiff
    !  #]

  subroutine extend_wvcs(lat1,lon1,lat2,lon2,latnew,lonnew)
    !  #[
    !
    ! Description: given the coordinates of two WVCs (1 and 2),
    !   the coordinates of a third WVC (new) that is located along the
    !   connecting line between 1 and 2 and with the same distance to 2
    !   as the distance between 1 and 2
    !
    !   x-----x-----x
    !   1     2    new
    !
    !   This routine can be used to extend the swath.
    !   The approach here is rather symplistic, but it appears that
    !   the position error is less than 1.5 km, even near the poles.
    !
    !   Anton Verhoef, KNMI 2010
    !
    ! arguments
    real(r_), intent(in)   :: lat1
    real(r_), intent(in)   :: lon1
    real(r_), intent(in)   :: lat2
    real(r_), intent(in)   :: lon2
    real(r_), intent(out)  :: latnew
    real(r_), intent(out)  :: lonnew

    real(r_) :: lon2tmp

    latnew = 2.0 * lat2 - lat1

    lon2tmp = lon2
    if (lon2tmp - lon1 .gt.  180.0) lon2tmp = lon2tmp - 360.0
    if (lon2tmp - lon1 .lt. -180.0) lon2tmp = lon2tmp + 360.0
    lonnew = 2.0 * lon2tmp - lon1
    lonnew = mod(lonnew+3600.0,360.0)

  end subroutine extend_wvcs
  !  #]

  function average_2_angles(dir1, dir2) result(avg_dir)
    !  #[
    ! A simple function to average 2 angels (in degrees)
    ! taking into account the circular property of angles
    ! (i.e. 359 and 1 are close together)
    !
    ! Results are wrapped to the range 0-360 degrees

    ! interface
    real, intent(in)  :: dir1, dir2
    real              :: avg_dir ! result

    ! local variables
    real :: angle_diff

    ! take difference between both direction
    angle_diff = dir2-dir1

    ! map to range -180 upto +180
    angle_diff = modulo(angle_diff+180., 360.) - 180.

    ! add half the difference to dir1
    avg_dir = dir1 + 0.5*angle_diff
    
    ! remap to the range 0 - 360
    avg_dir = modulo(avg_dir, 360.)

  end function average_2_angles
    !  #]
end module convert
