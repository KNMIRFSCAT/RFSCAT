program test_convert
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             test_convert
!
!     Created on:            15-09-2004
!
!     Last Modified on:      $Date: 2015-05-22 09:29:05 +0200 (Fri, 22 May 2015) $
!
!     Modifications Log:
!
! $Id: test_convert.F90 10664 2015-05-22 07:29:05Z josdekloe $
! 
!  test_convert.F90 7733 2011-11-15 06:59:21Z verhoefa $
!
! Revision 1.10  2006/03/28 10:16:17  vogelzan
! Included NWP SAF acknowledgement.
!
!

  USE convert, only: uv_to_speed, uv_to_dir
  USE convert, only: speeddir_to_u, speeddir_to_v
  USE convert, only: met2uv, uv2met
  USE convert, only: latlon2xyz, xyz2latlon
  USE convert, only: orbit2xyz, xyz2orbit
  USE convert, only: get_distance, get_angle_distance
  USE convert, only: WVC_Orientation, average_2_angles
  USE numerics, only: r_
!  USE numerics, only: r_=>r8_ ! to test the module with real8*

  implicit none

  real(r_) :: u,v,sp,dir
  real(r_) :: lat, lon, x, y, z
  real(r_) :: lat1, lat2, lon1, lon2
  real(r_) :: a1, rot_z1, a2, rot_z2

  real(r_), dimension(5) :: p, a, rot_z, t
  real(r_) :: test_a1, test_a2, test_z1, test_z2

  real    :: Phi1,Lam1 , Phi2,Lam2 , Alfa1,Alfa2

  integer :: i,j,k
  logical :: case1_ok, case2_ok

  ! a better accuracy for xyz2orbit is not to be expected for real*4 !
  real(r_), parameter :: eps = 1.e-1

  print *,"==========================================="
  u = 5. ; v = -7.
  sp = 0. ; dir = 0.
  sp  = uv_to_speed(u,v)
  dir = uv_to_dir(  u,v)
  print *,"u = ",u," v = ",v
  print *,"uv_to_speed, uv_to_dir ====> sp = ",sp," dir = ",dir

  print *,"==========================================="
  u = 0. ; v = 0.
  u = speeddir_to_u(sp,dir)
  v = speeddir_to_v(sp,dir)
  print *,"sp = ",sp," dir = ",dir
  print *,"speeddir_to_u, speeddir_to_v ====> u = ",u," v = ",v

  print *,"==========================================="
  sp  = 10. ; dir = 135.
  u = 0. ; v = 0.
  call met2uv(sp,dir,u,v)
  print *,"met2uv: sp = ",sp," dir = ",dir
  print *,"met2uv: ====> u = ",u," v = ",v

  sp  = 0. ; dir = 0.
  call uv2met(u,v,sp,dir) 
  print *,"uv2met: u = ",u," v = ",v
  print *,"uv2met: ====> sp = ",sp," dir = ",dir
  
  print *,"==========================================="

  lat = 55. ;   lon = 5.
  x = 0. ; y = 0. ; z = 0.
  call latlon2xyz(lat,lon,x,y,z)
  print *,"lat,lon = ",lat,lon
  print *,"latlon2xyz: ====> x,y,z = ",x,y,z

  lat = 0. ;   lon = 0.
  call xyz2latlon(x,y,z,lat,lon)
  print *,"x,y,z = ",x,y,z
  print *,"xyz2latlon: ====>lat,lon = ",lat,lon

  print *,"==========================================="

!  real(r_), dimension(5) :: p, a, rot_z

  p(1:5)     = (/  0.,  10., 45., 70., 90./) 
  a(1:5)     = (/-90., -10., 20., 45., 90./)
  t(1:5)     = (/  0.,   1.,  2.,  3.,  4./)
  rot_z(1:5) = t(1:5)*360./24.

  print "(10(a10,1x))",&
       "p","a","rot_z","x","y","z","a1","rot_z1","a2","rot_z2"

!  p = 0. ; a = 45. ; t = 3. ; rot_z = t*360./24.

  DO i=1,5
     DO j=1,5
        DO k=1,5
           x=0. ; y=0. ; z=0.
           ! getest 1-11-2004, en werkt ok! getest 1-11-2004, en werkt ok
           call orbit2xyz(p(i),a(j),rot_z(k),x,y,z)           

           call xyz2orbit(x,y,z,p(i),a1,rot_z1,a2,rot_z2)
           print "(10(f10.5,1x))",p(i),a(j),rot_z(k),x,y,z,a1,rot_z1,a2,rot_z2

           ! check for correct result
           case1_ok = .true.
           case2_ok = .true.

           test_a1 = abs(mod(a(j)     - a1    ,360._r_))
           test_z1 = abs(mod(rot_z(k) - rot_z1,360._r_))
           test_a2 = abs(mod(a(j)     - a2    ,360._r_))
           test_z2 = abs(mod(rot_z(k) - rot_z2,360._r_))

           ! in case p=90 the a and z rotation are along identical axes
           ! so just add them, before comparing the result
           if ( (abs(p(i))-90.) .lt. eps) then
              test_a1 = 0.
              test_a2 = 0.
              test_z1 = abs(mod(a(j)+rot_z(k)- a1-rot_z1,360._r_))
              test_z2 = abs(mod(a(j)+rot_z(k)- a2-rot_z2,360._r_))
           end if

           ! if abs(z).eq.1 then we are on one of the poles, in which
           ! case p=0, and the rot_z rotation angle has no meaning, 
           ! so disable testing it.
           if ( abs(abs(z)-1.) .lt. eps) then
              test_z1 = 0.
              test_z2 = 0.
           end if

           if ( (test_a1 .gt. eps) .or. (test_z1 .gt. eps) ) then
              case1_ok = .false.
           end if

           if ( (test_a2 .gt. eps) .or. (test_z2 .gt. eps) ) then
              case2_ok = .false.
           end if
           if ( (.not. case1_ok) .and. (.not. case2_ok) ) then
              print *,"conversion test failed:"
              print "(10(a10,1x))",&
                    "p","a","rot_z","x","y","z","a1","rot_z1","a2","rot_z2"
              print "(10(f10.5,1x))",&
                   p(i),a(j),rot_z(k),x,y,z,a1,rot_z1,a2,rot_z2
              print *,"test_a1 = ", test_a1," test_z1 = ", test_z1
              print *,"test_a2 = ", test_a2," test_z2 = ", test_z2
              stop
           end if

        END DO
     END DO
  END DO

  print *,"==========================================="
  lat1 = 5.
  lat2 = lat1+1.
  lon1 = 5.0
  lon2 = lon1 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  lat1 = 55.
  lat2 = lat1+1.
  lon1 = 5.0
  lon2 = lon1 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  lat1 = 85.
  lat2 = lat1+1.
  lon1 = 5.0
  lon2 = lon1 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  print *,"==========================================="
  lat1 = 5.
  lat2 = lat1
  lon1 = 5.0
  lon2 = lon1+1. 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  lat1 = 55.
  lat2 = lat1
  lon1 = 5.0
  lon2 = lon1+1. 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  lat1 = 85.
  lat2 = lat1
  lon1 = 5.0
  lon2 = lon1+1. 
  print *,"latlon1 = ",lat1,lon1,"latlon2 = ",lat2,lon2
  print *,"angle distance = ",get_angle_distance(lat1,lon1,lat2,lon2)
  print *,"km distance    = ",get_distance(lat1,lon1,lat2,lon2)
  print *,"==========================================="

  print *,""
  print *,"Test WVC_Orientation"
  Phi1=-18.61
  Lam1=-115.20
  Phi2=-17.52
  Lam2=-123.65
  call WVC_Orientation(Lam1,Phi1 , Lam2,Phi2 , Alfa1,Alfa2)
  print *,"  WVC1 coordinates (Lam1,Phi1) =",Lam1,Phi1
  print *,"  WVC2 coordinates (Lam2,Phi2) =",Lam2,Phi2
  print *,"  WVC1 orientation Alfa1 =",Alfa1,"   (Should equal 173.5994720)"
  print *,"  WVC2 orientation Alfa2 =",Alfa2,"   (Should equal 170.9747467)"
  print *,"==========================================="
  print *,'angle avg: 10,20: ',average_2_angles(10., 20.)
  print *,'angle avg: -10,20: ',average_2_angles(-10., 20.)
  print *,'angle avg: -10,20: ',average_2_angles(179., 185.)
  print *,'angle avg: 12,340: ',average_2_angles(12., 340.)
  print *,'angle avg: 1,359: ',average_2_angles(1., 359.)
  print *,'angle avg: 3,359: ',average_2_angles(3., 359.)
  print *,'angle avg: 1,357: ',average_2_angles(1., 357.)
!352   328  508  148  -32  -16  -4
!176
  print *,"==========================================="


end program test_convert
