module constants
!  #[ Documentation
!
! This little module collects several mathematical, physical,
! and natural constant parameters that may be used by the software.
! All of them are provided in 4 and 8 byte precision. 
!
! Note that using a 4 byte real parameter to initialise an 8 byte variable
! is not a good idea, since this will in many cases result in rounding 
! errors which may lead to wrong results of your calculation.
!
!  #]
!  #[ Modifications
! 
! Created by: J. de Kloe, 26-may-2009
!
! Last modified: $Date: 2011-11-15 07:59:41 +0100 (Tue, 15 Nov 2011) $
!
! $Id: constants.F90 7753 2011-11-15 06:59:41Z verhoefa $
!
! Revision 1.3  2009/05/26 08:24:19  kloedej
! another test of the Log and Date include commands
!
! Revision 1.2  2009/05/26 08:19:45  kloedej
! test of the Log include line
!
!
!  #]
!  #[ Modules used
  use numerics, only: r4_, r8_
!  #]
!  #[ Parameters defining mathematical constants
  implicit none

  real(r8_), parameter :: pi_r8 = 3.14159265358979323846264338328_r8_
  real(r4_), parameter :: pi_r4 = 3.141592654_r4_ ! = pi
  real(r8_), parameter :: dpi   = pi_r8
  real(r4_), parameter :: pi    = pi_r4
! sometimes this expression is used: pi=4.0*atan(1.0)
! but then you have to trust the result of the atan() function, and that
! result may actually be less precise then desired when certain
! compiler optimisations are activated.

  real(r8_), parameter :: two_pi_r8 = 2._r8_ * pi_r8
  real(r4_), parameter :: two_pi_r4 = 2._r4_ * pi_r4

  real(r8_), parameter :: pi_over_180_r8 = pi_r8/180._r8_
  real(r4_), parameter :: pi_over_180_r4 = pi_r4/180._r4_
  real(r8_), parameter :: deg2rad_r8 = pi_over_180_r8
  real(r4_), parameter :: deg2rad_r4 = pi_over_180_r4

  real(r8_), parameter :: c180_over_pi_r8 = 180._r8_/pi_r8
  real(r4_), parameter :: c180_over_pi_r4 = 180._r4_/pi_r4
  real(r8_), parameter :: rad2deg_r8 = c180_over_pi_r8
  real(r4_), parameter :: rad2deg_r4 = c180_over_pi_r4

!  #]
!  #[ Parameters defining physics constants
  real(r8_), parameter :: speed_of_light_r8 = 299792458._r8_ ! [m/s]
  real(r4_), parameter :: speed_of_light_r4 = 299792458._r4_ ! [m/s]
!  #]
!  #[ Parameters defining natural constants
! Earth radius
  real(r8_),  parameter :: R_earth_equatorial_r8 = 6378.1_r8_ ! [km]
  real(r4_),  parameter :: R_earth_equatorial_r4 = 6378.1_r4_ ! [km]
  real(r8_),  parameter :: R_earth_polar_r8      = 6356.8_r8_ ! [km]
  real(r4_),  parameter :: R_earth_polar_r4      = 6356.8_r4_ ! [km]
! source: 
! http://astrogeology.usgs.gov/Projects/BrowseTheSolarSystem/earth.html
! outdated link: http://wwwflag.wr.usgs.gov/USGSFlag/Space/wall/earth.html
!  #]
end module constants
