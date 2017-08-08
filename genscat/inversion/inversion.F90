  !  #[ Still to do

! te doen: 
! -warning messages opschonen en nakijken (routine my_warn opschonen)
! -consequent overal intent() toevoegen
!
! - NSCAT case is broken now. Should be repaired
!   and maybe made more efficient
!
! -Test plan
!    -voor alle functies een aparte test functie maken die
!     door een test programma aangeroepen kan worden.

  !  #]

MODULE inversion
  !  #[ Documentation

! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             inversion
!
!     Created on:            long ago
!
!     Last Modified on:      $Date: 2015-03-11 12:15:48 +0100 (Wed, 11 Mar 2015) $
!
!     Modifications Log:
!
! $Id: inversion.F90 10451 2015-03-11 11:15:48Z verspeek $
! 
!  inversion.F90 10424 2015-02-24 10:38:38Z vogelzan $
! 
!  inversion.F90 10351 2015-01-28 12:12:28Z verspeek $
! 
!  inversion.F90 10337 2015-01-21 16:27:47Z verspeek $
! 
!  inversion.F90 10101 2014-11-18 11:18:33Z verhoefa $
! 
!  inversion.F90 9032 2013-09-23 12:45:35Z verhoefa $
! 
!  inversion.F90 8921 2013-07-03 07:32:37Z verhoefa $
! 
!  inversion.F90 8807 2013-05-29 10:08:27Z verhoefa $
! 
!  inversion.F90 8763 2013-04-22 06:50:12Z verhoefa $
! 
!  inversion.F90 8760 2013-04-18 17:47:27Z josdekloe $
! 
!  inversion.F90 8564 2013-01-30 16:29:32Z josdekloe $
! 
!  inversion.F90 8550 2013-01-28 15:31:40Z josdekloe $
! 
!  inversion.F90 8547 2013-01-28 14:13:51Z josdekloe $
! 
!  inversion.F90 8479 2013-01-04 15:55:09Z josdekloe $
! 
!  inversion.F90 8473 2013-01-04 15:07:13Z josdekloe $
! 
!  inversion.F90 7725 2011-11-14 16:05:36Z verhoefa $
!
! Revision 1.111  2010/09/07 07:05:15  verhoefa
! Make print of inversion data better readable
!
! Revision 1.110  2009/10/19 13:09:55  verspeek
!
! calc_B_coefs_cmod - change from integer cmodVersion to character cmod_version
!
! Revision 1.109  2009/08/31 13:00:32  verspeek
!
! Made CMOD6 equal to CMOD5.n plus a correction function (under development).
! Added temporarily functions calcPoly3 and inc_B0_corr.
!
! Revision 1.108  2009/05/20 13:41:17  kloedej
! changed the parameter list to the subroutine Get_2D_MLE_array
!
! Revision 1.107  2009/05/19 08:18:38  kloedej
! modified an error message to be a little more clear
!
! Revision 1.106  2009/05/15 07:50:58  kloedej
! added the undefined_pol parameter
!
! Revision 1.105  2009/05/13 12:23:25  verspeek
!
! added routines calc_B_coefs_cmod...
! -
!
! Revision 1.104  2009/05/11 14:58:24  kloedej
! added Get_2D_MLE_array() subroutine needed for FoM calculations in rfscat
!
! Revision 1.103  2008/12/29 16:03:36  verhoefa
! Several efficiency improvements
!
! Revision 1.102  2008/07/17 10:12:58  verhoefa
! Some warning messages were issued incorrectly
!
! Revision 1.101  2008/07/17 09:27:39  verhoefa
! Set reject_lutmax_sol to false
!
! Revision 1.100  2008/07/17 08:28:49  verhoefa
! Code simplifications and computing speed gains
!
! Revision 1.99  2008/05/21 07:01:53  verhoefa
! Added function get_closest_solution
!
! Revision 1.98  2008/03/27 08:43:27  verhoefa
! Store pointers to normal solutions in MSS case
!
! Revision 1.97  2008/02/27 08:50:31  verhoefa
! Added some intent statements
!
! Revision 1.96  2008/02/27 07:10:19  verhoefa
! Fixed gfortran compiler warning
!
! Revision 1.95  2008/02/06 14:19:52  verhoefa
! Changed CMOD5.n coefficients according to Email Hans Hersbach 5 Feb 2008
!
! Revision 1.94  2008/01/31 08:55:53  verhoefa
! Corrected small bug
!
! Revision 1.93  2008/01/31 08:32:51  verhoefa
! Included CMOD5_N (neutral winds)
!
! Revision 1.92  2007/11/28 10:11:45  verhoefa
! Changed interface of get_indices_lowest_local_minima and made it public
!
! Revision 1.91  2007/10/04 14:27:30  verhoefa
! Allow values of 0.0 in VV or HH lut
!
! Revision 1.90  2007/06/28 08:12:48  verhoefa
! Let cmod5_5 smoothly approach zero at low wind speeds
!
! Revision 1.89  2007/05/03 13:05:58  verhoefa
! Removed temporary bias of ASCAT winds in combination with cmod5
!
! Revision 1.88  2007/04/27 08:18:29  verspeek
! adaptation for CMOD5_5 for case with optional parameter calc_B0
!
! Revision 1.87  2007/04/23 12:52:38  verspeek
! added cmod5_5 : cmod5 plus 0.5 m/s
! added cmod6: placeholder, identical to cmod5 for now
!
! Revision 1.86  2007/02/08 12:11:04  verhoefa
! Removed redundant write statement
!
! Revision 1.85  2007/02/08 11:24:54  verhoefa
! GMF_version_used was not always properly determined
!
! Revision 1.84  2007/02/07 15:38:03  verhoefa
! Swapped calculation of MLE sign and application of bias to ASCAT winds
!
! Revision 1.83  2006/12/13 10:00:49  verhoefa
! add 0.5 m/s to all ASCAT wind speeds when CMOD5 is used
!
! Revision 1.82  2006/11/08 14:19:32  verhoefa
! Added probability to inv_output structure
!
! Revision 1.81  2006/11/03 15:40:03  verhoefa
! Added some elements to the inv_input structure
!
! Revision 1.80  2006/11/02 18:09:23  portabel
! - Subroutine get_indices_lowest_lcal_minima modified: a logical variable is
!   included to discard minima at LUT max as solution.
! - New flags indicate presence of minima at LUT max and LUT min in the cost
!   function.
! - Bug repaired: MLE rank sorting is now performed also for MSS.
!
! Revision 1.79  2006/10/20 06:46:03  verhoefa
! Undone all changes from 19 Oct 2006
!
! Revision 1.78  2006/10/19 13:59:08  verhoefa
! Introduced calc_sector_sizes in inversion
!
! Revision 1.77  2006/10/19 09:16:57  verhoefa
! Sorting of wind solutions done both for the normal and MSS cases
!
! Revision 1.76  2006/10/13 12:48:03  verhoefa
! Changed naming of wqc flags
!
! Revision 1.75  2006/10/10 16:56:18  portabel
! Some clean-up has been done to make the code more readable: Remaining NSCAT-related code
! and references to the allowed_nr_of_sol parameter have been removed.
! ERS specific wind_quality_codes are now declared and set in post_inversion module. Only
! generic wqc remains.
! SortMod module is used to perform the solution ranking (by MLE value) after parabolic search.
! The sorting from "get_indices_lowest_local_minima" has been removed.
!
! Revision 1.74  2006/09/11 12:10:40  verhoefa
! Forced input to parabolic search to be symmetric when we are near the LUT edge
!
! Revision 1.73  2006/08/28 10:09:52  verhoefa
! Added some elements in init_inv_output
!
! Revision 1.72  2006/08/18 09:20:03  verhoefa
! Improved searching for wind speed minima at low speeds near LUT edge
!
! Revision 1.71  2006/08/15 08:46:21  verhoefa
! Fix for artificial minima when we are at LUT boundary
!
! Revision 1.70  2006/07/14 08:24:03  verhoefa
! Made max_nr_of_sol public
!
! Revision 1.69  2006/07/14 06:33:10  verhoefa
! Apply conversion for meteorological/oceanographic conventions before inversion instead of afterwards
!
! Revision 1.68  2006/07/03 15:11:12  verhoefa
! Moved large part of fill_wind_quality_code from inversion to post_inversion
!
! Revision 1.67  2006/06/29 08:04:26  verhoefa
! Moved some routines to new module post_inversion
!
! Revision 1.66  2006/06/28 06:48:58  verhoefa
! New approch following Marcos' FoM (Figure of Merit) work for conedistance normalisation
!
! Revision 1.65  2006/06/16 07:43:47  verhoefa
! Rearranged some ers related subroutines and removed some commented statements
!
! Revision 1.64  2006/04/20 07:09:48  verhoefa
! Some changes to verbosity
!
! Revision 1.63  2006/03/28 11:46:53  vogelzan
! Corrected NWP SAF acknowledgement.
!
! Revision 1.62  2006/03/28 09:53:53  vogelzan
! Included NWP SAF acknowledgement.
!

  !  #]
  !  #[ Description

  !---------------------------------------------------------
  ! this module determines a set of possible wind velocities
  ! and directions from a set of sigma_0 measurements for 
  ! a certain wind vector cell (wvc).
  ! It is an extended and generalised version of the
  ! inversion.f routine written by Julia Figa (1997-1999)
  ! which was based on inversion software written by
  ! Ad Stoffelen (1995).
  ! 
  !     Written by Jos de Kloe 
  !     With a lot of help from Marcos Portabella
  !
  !     version number and change information can be retrieved
  !     from the svn version control system.
  !
  !     copyright KNMI
  !---------------------------------------------------------

  ! the calling program should set the following variables
  ! that are defined below:
  ! -inv_input_type%nr_of_views   (nr of measurements per WVC)
  ! -inv_input_type%scat_type     (type of instrument)
  ! (the following elements use a dimension of: 1..nr_of_views)
  ! -inv_input_type%pol           (polarisation type used)
  ! -inv_input_type%antenna_dir   (antenna azimuth direction)
  ! -inv_input_type%theta_angle   (antenna incidence angle)
  ! -inv_input_type%sigma_0       (the actual backscatter values)
  ! -inv_input_type%sigma_0_vh    (only used for polarimetric measurements)
  ! -inv_input_type%kp_a          (noise estimate, a value)
  ! -inv_input_type%kp_b          (noise estimate, b value)
  ! -inv_input_type%kp_c          (noise estimate, c value)
  ! the following elements are scalar
  ! -inv_input_type%wvc_number    (needed by Seawinds only for get_Rn)
  ! -inv_input_type%nr_of_nodes   (needed by ASCAT to determine MLE normalisation and QC threshold tables)

  ! the code used for polarisation type can be one of the following:
  !    c_vv_pol,  c_hh_pol,  c_vh_pol,  c_po_pol
  !    ku_vv_pol, ku_hh_pol, ku_vh_pol, ku_po_pol
  ! (special cases, only for RFSCAT:) no_pol, copy_pol 
  ! (special case, only for DopSCAT:) c_vv_dop, c_hh_dop

  ! the code used for scat type can be one of the following:
  !   scat_type_rfscat, scat_type_seawinds, scat_type_nscat
  !   scat_type_ers, scat_type_ascat, scat_type_dopscat

  !  #]
  !  #[ Recent changes

  !---------------------------------------------------------
  !   
  ! the changelist has been phased out.
  ! This information can be retrieved
  ! from the svn version control system.
  !
  !---------------------------------------------------------

  !  #]
  !  #[ USE and Include statements

! defines from the former "inversion.h" file

! an option to switch to using some 8byte reals
! in stead of 4 byte reals (only used with RFSCAT)
!#define USE_8BYTE_REALS

! how to interpolate the sigma0 tables
!        3D  = do a 3D interpolation on the table on v, theta and phi,
!        2D  = do a 2D interpolation on the table on theta and phi,
!        2DV = do a 2D interpolation on the table on v and theta
!        1D  = do a 1D interpolation along theta only
!#define INTERPOLATE1D
!#define INTERPOLATE2D
!#define INTERPOLATE2DV
#define INTERPOLATE3D
! beware ! for the polarimetric case it is essential to use INTERPOLATE3D
!        ! otherwise you will probably go very fast to the origin
!        ! (u,v)=(0,0) and never leave it again, since this is a very
!        ! flat region far from the minima here.

! replace the interpolate function by the choosen one
!
#ifdef INTERPOLATE1D
#define INTERPOLATE(table, v, phi, theta) interpolate1d(table, v,phi,theta)
#endif
#ifdef INTERPOLATE2D
#define INTERPOLATE(table, v, phi, theta) interpolate2d(table, v,phi,theta)
#endif
#ifdef INTERPOLATE2DV
#define INTERPOLATE(table, v, phi, theta) interpolate2dv(table, v,phi,theta)
#endif
#ifdef INTERPOLATE3D
#define INTERPOLATE(table, v, phi, theta) interpolate3d(table, v,phi,theta)
#endif

  ! define external MODULES needed by inversion

  ! defines fileunit handling
  USE LunManager, only: get_lun, free_lun, fileunit_stdout

  ! defines the real(l_) type and the missing_indicators
  USE numerics, only: l_, s_, r4_, &
                      missing_real, missing_indicator_real, &
                      missing_int, missing_indicator_integer, &
                      convert_missing_real,&
                      convert_missing_integer

  use convert, only: speeddir_to_u, speeddir_to_v, &
                     uv_to_speed, uv_to_dir

  use convert, only: trnsfrm

  USE SortMod, only: GetSortIndex, SortWithIndex

#ifdef USE_MPI
  ! enables use of this module on multiple cpu s 
  ! (only the my_exit subroutine is MPI dependent)
  USE mpi_module, only: MPI_abort
#endif
  !  #]
  !  #[ Wind Quality Code definition

  !===================================================================
  IMPLICIT NONE     ! no implicit variable typing

  !===================================================================
  ! code definition for wind_quality_code:
  ! [Note that instrument-specific wind quality coding is defined after 
  !  inversion, e.g., for ERS it can be found in the "post_inversion" module.]

  integer, parameter :: wqc_ok = 0
  !    0   = ok, there are solutions found (at least 1)
  integer, parameter :: wqc_ok_144sol = 4
  !    4   = ok, but get_all_winddirs switch was used, so usually all 144
  !          solutions are returned UNSORTED
  integer, parameter :: wqc_ok_toomany = 6
  !    5   = more than inv_settings%max_nr_of_solutions (typically 4)
  !          were found.
  integer, parameter :: wqc_below_this_are_ok = 14 
  !   14   = this is just the threshold value that separates "ok" from
  !          "notok" cases.
  integer, parameter :: wqc_notok_nodirfound = 360
  !    360 = no wind direction found, only min,average and max of windspeed
  !          which is returned in foundwindspeed(1),(2) and (3)
  integer, parameter :: wqc_notok_noresult = 361
  !    361 = no wind direction found, 
  !          but inv_settings%max_nr_of_solutions < 3, so it is not possible 
  !          to give min/avg/max as output, therefore no output is available
  integer, parameter :: wqc_notok_nosol = 999
  !    999 = no solutions
  integer, parameter :: wqc_undefined = -1
  !    -1 is value that may be used to initialise the wqc before use
  !===================================================================

  !  #]
  !  #[ Local Parameters

  !---------------------------------------------------------
  ! Then a number of parameters are defined
  !---------------------------------------------------------

  ! some numerical constants
  real(l_), parameter :: pi             = 3.141592654 ! = pi
  real(l_), parameter :: two_pi         = 6.283185308 ! = pi*2
  real(l_), parameter :: rad2deg        = 57.295780   ! = 180/pi
  real(l_), parameter :: deg2rad        = 0.017453293 ! = pi/180
  real(l_), parameter :: min15dB_factor = 1.0/31.6228 ! = -15dB

  ! some Look-Up-Table properties
  real(l_),parameter :: LUT_min_windspeed  =  0.2 ! [m/s] (wind speed v)
  real(l_),parameter :: LUT_max_windspeed  = 50.0 ! [m/s]
  real(l_),parameter :: LUT_windspeed_step =  0.2 ! [m/s]
  real(l_),parameter :: LUT_min_winddir    =  0.0 ! [deg] (azimuth angle phi)
  real(l_),parameter :: LUT_max_winddir    =180.0 ! [deg]
  real(l_),parameter :: LUT_winddir_step   =  2.5 ! [deg]
  real(l_),parameter :: LUT_min_theta      = 16.0 ! [deg] (incidence angle 
  real(l_),parameter :: LUT_max_theta      = 66.0 ! [deg]            theta)
  real(l_),parameter :: LUT_theta_step     =  1.0 ! [deg]
  integer, parameter :: LUT_nr_windspeed_steps = 250 ! =1+(max-min)/step
  integer, parameter :: LUT_nr_winddir_steps   =  73 ! =1+(max-min)/step
  integer, parameter :: LUT_nr_theta_steps     =  51 ! =1+(max-min)/step
  real(l_),parameter :: one_over_LUT_windspeed_step = 1.0/LUT_windspeed_step
  real(l_),parameter :: one_over_LUT_winddir_step   = 1.0/LUT_winddir_step  
  real(l_),parameter :: one_over_LUT_theta_step     = 1.0/LUT_theta_step

  ! the tablesize in memory will be:
  ! 250x73x51x8 = 930.750x8 = 7.446.000 =7.1 MB for 8 byte reals or
  ! 250x73x51x4 = 930.750x4 = 3.723.000 =3.6 MB for 4 byte reals
  ! so no memory problems expected

  ! boundaries for the inversion routine
  real(l_), parameter :: min_windvelocity = 0.2
  real(l_), parameter :: max_windvelocity = 50.0

  ! define how many solutions can be reported
  integer, parameter :: max_nr_of_sol     = 144
  ! (this can typically be 2,4 or 144)
  ! this parameter is ONLY used in variable/array definitions
  ! all executable code uses: inv_settings%max_nr_of_solutions
  ! to make the nr of solutions adjustable by the user !!!

  ! define how many normal solutions can be reported in the MSS case
  integer, parameter :: max_nr_of_sol_n   = 4
  
  ! define maximum sizes for some datastructures

  ! the maximum nr of views/beams per wind vector cell
  ! Note that 4 should be OK for almost all instruments, 
  ! but rfscat uses more, upto 60 for some special cases, 
  ! and upto 15 for more common configurations,
  ! so for this case please keep this number at 15
  ! (if rfscat needs more, adapt the following line in a local copy)
  integer, parameter :: max_nr_of_views = 15

  ! nr of winddir steps used to determine minimum valley for inversion
  ! (a property for the arrays in which the "minimum valley" 
  !  that is used by the inversion routine is stored)
  integer,  parameter :: nr_of_winddir_steps = 144
  real(l_), parameter :: winddirstep  = 360.0/nr_of_winddir_steps ! degree

  real(l_), parameter :: Zfactor          = 0.625 ! = 1/1.6 = 5/8 ! old: rExp
  real(l_), parameter :: een_over_Zfactor = 1.0/Zfactor ! = 1.6

  ! LOCAL parameters: (to be used with print_message)
  integer, parameter :: WARN_ERASE_FLAGS =  0
  integer, parameter :: WARN_NO_DATA     =  1
  integer, parameter :: WARN_ONE_VIEW    =  2
  integer, parameter :: WARN_LUT_LOW_WS  =  3
  integer, parameter :: WARN_LUT_HIGH_WS =  4
  integer, parameter :: WARN_LUT_LOW_WD  =  5
  integer, parameter :: WARN_LUT_HIGH_WD =  6
  integer, parameter :: WARN_LUT_LOW_TH  =  7
  integer, parameter :: WARN_LUT_HIGH_TH =  8
  integer, parameter :: WARN_NO_MINIMA   =  9
  integer, parameter :: WARN_HIGHNR_MIN  = 10
  integer, parameter :: WARN_LOW_VAR_S0  = 11
  integer, parameter :: WARN_WS_STEP     = 12
  integer, parameter :: MSG_HELLO        = 13
  integer, parameter :: NR_OF_WARNINGS   = 13

  !  #]
  !  #[ Local Variables

  !---------------------------------------------------------
  ! LOCAL global (private) vars:
  !---------------------------------------------------------

  ! (for testing purposes only)
  !  logical  :: debug_switch = .false.

  logical :: polarimetric_scheme_used = .false.
  logical :: doppler_scheme_used = .false.

  ! possible values for verbosity:
  integer, parameter :: inversion_verbosity_error   = 0
  integer, parameter :: inversion_verbosity_warn    = 1
  integer, parameter :: inversion_verbosity_report  = 2

  ! possible instrument types:
  integer, parameter :: instrument_type_rfscat   = 1
  integer, parameter :: instrument_type_nscat    = 2
  integer, parameter :: instrument_type_seawinds = 3
  integer, parameter :: instrument_type_ers      = 4
  integer, parameter :: instrument_type_ascat    = 5
  integer, parameter :: instrument_type_dopscat  = 6

  ! possible angle conventions:
  integer, parameter :: angle_convention_meteorological = 1
  integer, parameter :: angle_convention_oceanographic  = 2
  !integer, parameter :: angle_convention_mathematical   = 3 ! not yet used
  ! oceanographic:  0 deg. = pointing/flowing towards the north
  !                 clockwise increment (seen from above)
  !                 (components u=0, v>0)
  ! meteorological: 0 deg. = pointing/flowing towards the south, from the north
  !                 clockwise increment (seen from above)
  !                 (components u=0, v<0)
  ! mathematical:   0 deg. = pointing/flowing towards the east
  !                 counterclockwise increment (seen from above)
  !                 (components u>0, v=0)

  ! a number to fill the "struct_was_properly_initialised" flag.
  ! I use this number, because a logical can be set unintentionally
  ! in 50% of all cases if the struct was NOT YET properly initialised,
  ! but only contains random data.....
  ! The only routine that will set this struct element is
  ! init_inv_settings_to_default(), so in this way I hope to be able to
  ! enforce the use of this routine on this struct before it is used ...
  integer, parameter :: inv_settings_canary = 7654321

  ! this struct is intended to replace all or as much as possible,
  ! the settings that are now still in the inversion.h file
  TYPE inv_settings_type
     integer  :: struct_was_properly_initialised
     integer  :: verbosity
     integer  :: instrument_type
     integer  :: antenna_angle_convention
     integer  :: wind_angle_convention
     logical  :: use_zspace
     logical  :: no_normalisation
     logical  :: add_sign_to_mle
     logical  :: get_all_winddirs ! use the 144 solution scheme
     logical  :: do_parabolic_winddir_search
     logical  :: do_parabolic_minimum_search
     integer  :: max_nr_of_solutions
     logical  :: experimental_ers_wqc_checks
     logical  :: use_meas_sigma0_for_normfactor
     logical  :: add_log_term_to_cost
     integer  :: use_which_fn_to_construct_LUT
     logical  :: first_warning_only
     logical  :: do_input_data_check
     real(l_) :: initial_ws_step
     real(l_) :: minimum_ws_step
     real(l_) :: refiner_ws_step
     real(l_) :: ws_initial_guess
     ! a weight factor to be able to combine polarimetric measurements
     ! with normal VV/HH measurements (tested for RFSCAT only)
     real     :: polarimetric_weight ! default = 1.0
     ! a weight factor to be able to combine doppler measurements
     ! with normal VV/HH measurements (tested for RFSCAT only)
     ! doppler_weight = 1.0e10  ==>almost no effect
     ! doppler_weight = 1.0e8   ==>small effect, but wqc already fails with 
     !                             code 400 and noticable change in conedistance
     ! doppler_weight = 1.0e6   ==>strong effect, winddir totally wrong now
     real     :: doppler_weight ! default = 1.0e5

  END TYPE inv_settings_type

  ! options for: use_which_fn_to_construct_LUT
  ! (only relevant for C-band VV polarisation channels)
  integer, parameter :: dont_try_to_construct_LUT    = -1
  integer, parameter :: use_cmod4_to_construct_LUT   = 4
  integer, parameter :: use_cmod5_to_construct_LUT   = 5
  integer, parameter :: use_cmod5_5_to_construct_LUT = 55
  integer, parameter :: use_cmod5_n_to_construct_LUT = 571
  integer, parameter :: use_cmod6_to_construct_LUT   = 6
  integer, parameter :: use_cmod7_to_construct_LUT   = 7

  ! remember that the settings have been initialised
  logical, save :: inv_settings_are_defined = .false.  
  ! a struct in which the settiongs can be saved
  TYPE(inv_settings_type), save :: inv_settings

  ! remember what type of GMF was used (only for the c_vv case for now)
  integer, save :: GMF_version_used 

  ! definitions of the files to use as Look-Up-Tables (LUTs):
  character(len=256) :: filename_c_vv     = "./c-vv2.dat"
  character(len=256) :: filename_c_hh     = "./c-hh2.dat"
  character(len=256) :: filename_c_pol    = "./c-pol2.dat"
  character(len=256) :: filename_ku_vv    = "./ku-vv2.dat"
  character(len=256) :: filename_ku_hh    = "./ku-hh2.dat"
  character(len=256) :: filename_ku_pol   = "./ku-pol2.dat"
  character(len=256) :: filename_c_vv_dop = "./c-vv-dop.dat"
  character(len=256) :: filename_c_hh_dop = "./c-hh-dop.dat"

  ! a global array of norm_factors, used in case they can be calculated
  ! before starting the scan in the speed-dir plane
  real(l_), dimension(1:max_nr_of_views) :: norm_factor_meas  = 0.

  ! a variable to remember last found windspeed, so that 
  ! it can be used as a first guess for the next direction
  real(l_), save :: previous_windspeed

  ! boundaries for the previous_windspeed variable
  real(l_):: lower_speed_threshold  = 4.0
  real(l_):: higher_speed_threshold = 20.0

  ! these pointers are used to hold references to allocated
  ! memory for the tables in use
  real(r4_), save, pointer, dimension(:,:,:) :: &
       c_vv_table,  c_hh_table,  c_pol_table, &
       ku_vv_table, ku_hh_table, ku_pol_table, &
       c_vv_dop_table, c_hh_dop_table

  ! used to remember which LUTs are already in memory
  logical, save       :: overall_first_call  = .true.
  logical, save       :: first_call_c_vv     = .true.
  logical, save       :: first_call_c_hh     = .true.
  logical, save       :: first_call_c_pol    = .true.
  logical, save       :: first_call_ku_vv    = .true.
  logical, save       :: first_call_ku_hh    = .true.
  logical, save       :: first_call_ku_pol   = .true.
  logical, save       :: first_call_c_vv_dop = .true.
  logical, save       :: first_call_c_hh_dop = .true.
    
  !---------------------------------------------------------
  ! now all global variables for the module are defined
  !---------------------------------------------------------

  !  #]
  !  #[ User Interface

  !---------------------------------------------------------
  ! Then folows the user interface:
  !---------------------------------------------------------
  
  !------------!
  ! INPUTS:    !
  !------------!

  ! vars and arrays for input of the problem to be inverted
  ! these inputs should always be defined, before calling invert_one_wvc
  TYPE inv_input_type
     integer                                :: nr_of_views
     integer,  dimension(1:max_nr_of_views) :: pol
     real(l_), dimension(1:max_nr_of_views) :: antenna_dir
     real(l_), dimension(1:max_nr_of_views) :: theta_angle
     real(l_), dimension(1:max_nr_of_views) :: sigma_0
     real(l_), dimension(1:max_nr_of_views) :: kp_a
     real(l_), dimension(1:max_nr_of_views) :: kp_b
     real(l_), dimension(1:max_nr_of_views) :: kp_c
     ! this input is only needed for ERS and ASCAT, and allows a conedistance
     ! normalisation as function of node-number
     ! it is used only in the post_inversion
     integer                                :: node_nr
     ! this input is only needed for ERS, and allows to decide
     ! whether to use the Kp from the BUFR input or from a lookup table
     ! it is used only in the post_inversion
     integer                                :: year
     ! this input is only needed for ASCAT, and allows to decide
     ! which MLE normalisation table to use
     ! it is used only in the post_inversion
     integer                                :: cell_sampling ! in meters
     ! this input is only used for the polarimetric measurements
     ! (needed to calculate the kp !!)
     real(l_), dimension(1:max_nr_of_views) :: sigma_0_vh
     ! this input is only used for doppler measurements
     real(l_), dimension(1:max_nr_of_views) :: doppler_shift
     ! 
     ! for internal use only, not part of the user interface,
     ! unfortunately setting just this one element to private
     ! seems not possible
     logical                                :: converted_to_zspace
     ! this input is needed for ASCAT to determine the MLE normalisation
     ! and QC threhold tables, in particular for ASCAT-6.25.
     integer                                :: nr_of_nodes  ! 42, 82, 162 or 164
  END TYPE inv_input_type

  ! inputs in the form of environment settings: these will
  ! set the filenames used for the look-up- tables:
  !       LUT_FILENAME_C_VV,  LUT_FILENAME_C_HH,  LUT_FILENAME_C_POL
  !       LUT_FILENAME_KU_VV, LUT_FILENAME_KU_HH, LUT_FILENAME_KU_POL
  !       LUT_FILENAME_C_VV_DOP, LUT_FILENAME_C_HH_DOP
  
  !------------!
  ! OUTPUTS:   !
  !------------!

  TYPE inv_output_type
     ! arrays for output of the result
     !
     ! BEWARE: max_nr_of_sol=144 at the moment, so storing this output
     !         struct for all nodes of all msgs in memory while processing 
     !         may not be such a good idea ....
     !
     real(l_),dimension(1:max_nr_of_sol)  :: foundwindspeed
     real(l_),dimension(1:max_nr_of_sol)  :: foundwinddir
     real(l_),dimension(1:max_nr_of_sol)  :: conedistance_measured
     real(l_),dimension(1:max_nr_of_sol)  :: probability

     ! store the indices to the normal solutions if MSS is used
     integer,dimension(1:max_nr_of_sol_n) :: indices_solutions_n

     real(l_)                      :: skill
     integer                       :: nr_of_windsolutions
     integer                       :: nr_of_windsolutions_n
     ! number of solutions at lowest and highest edge of LUT
     logical                       :: minimum_at_lutmin
     logical                       :: minimum_at_lutmax
     integer                       :: wind_quality_code
     integer                       :: GMF_version_used
     ! needed for skill calculation of ERS
     real(l_)                      :: mean_cone_distance
     ! min and max cone distances of all considered wind directions
     real(l_)                      :: min_cone_distance
     real(l_)                      :: max_cone_distance
     ! output the smoothing window size (for debugging/checking)
     integer                       :: smooth_width
  END TYPE inv_output_type

  !------------------------------------!
  ! parameters used for input settings !
  !------------------------------------!

  ! some definitions to make setting of the polarisation easier
  integer, parameter :: c_vv_pol  = 1 ! transmit v and receive v polarisation
  integer, parameter :: c_hh_pol  = 2 ! transmit h and receive h polarisation
  integer, parameter :: c_vh_pol  = 3 ! transmit v and receive h polarisation  
  integer, parameter :: c_po_pol  = 4 ! use polarimetric scheme (hvvv)
  integer, parameter :: ku_vv_pol = 5 ! transmit v and receive v polarisation
  integer, parameter :: ku_hh_pol = 6 ! transmit h and receive h polarisation
  integer, parameter :: ku_vh_pol  =7 ! transmit v and receive h polarisation
  integer, parameter :: ku_po_pol = 8 ! use polarimetric scheme (hvvv)
  ! special switches, only used by simulators like RFSCAT and CDOP
  integer, parameter :: no_pol    =  9 ! dont use this beam (only for 2nd beam)
  integer, parameter :: copy_pol  = 10 ! copy pol. info from geometry file
  integer, parameter :: c_vv_dop  = 11 ! VV doppler information available
  integer, parameter :: c_hh_dop  = 12 ! HH doppler information available
  integer, parameter :: largest_pol_code = 12

  ! for initialisation purposes only
  integer, parameter :: undefined_pol = -1

  !----------------------------!
  ! parameters used for output !
  !----------------------------!

  ! some definitions to be used in the GMF_version_used field
  integer, parameter :: cmod4_used_as_GMF   = 4
  integer, parameter :: cmod5_used_as_GMF   = 5
  integer, parameter :: cmod5_5_used_as_GMF = 55
  integer, parameter :: cmod5_n_used_as_GMF = 571
  integer, parameter :: cmod6_used_as_GMF   = 6
  integer, parameter :: cmod7_used_as_GMF   = 7
  ! may be extended to other types in the future

  !  #]
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  subroutine invert_one_wvc(inv_input,inv_output)
    !  #[ This is the main routine for this module

    ! Inversion of s0 'triplets' to create the most probable wind solutions.
    ! no more than inv_settings%max_nr_of_solutions solutions are returned.
    ! (the term 'triplets' is no longer correct since any number of views
    !  larger than or equal to 2 can be used for inversion)
    !
    ! Method:
    !       Given 2 or more values of sigma0:
    !       for each wind direction, the WIND SPEED 
    !       that minimises the distance to the cone is selected. 
    !       Then, 
    !       at these optimal speeds, the WIND DIRECTIONS that locally 
    !       minimise the distance to the cone are selected.
    !       For more accuracy, interpolation between the gridpoints
    !       has been added using a parabolic formula.
    
    !---------------------------------------------------------
    ! input and output parameters for this routine
    !---------------------------------------------------------
    TYPE(inv_input_type)  :: inv_input
    TYPE(inv_output_type) :: inv_output
    
    !---------------------------------------------------------
    ! define some local variables
    !---------------------------------------------------------
    integer                     :: i, k
    integer                     :: nr_of_minima  = -1
    integer                     :: status        = -1 
    integer, dimension(1:max_nr_of_sol) :: indices     =  0   ! (/ 0,0,0,0/)
    real(l_)                    :: cone_dist     = -1.0
    real(l_)                    :: winddirection = -1.0
    real(l_)                    :: windspeed     = -1.0

    ! a dimension dimension(1:nr_of_winddir_steps) would be sufficient
    ! to store this data, but because of the circular nature of the arrays
    ! adding an element at both sides makes it much easier to implement
    ! finding of local minima.
    real(l_), save, dimension(0:nr_of_winddir_steps+1) :: windspeed_array
    real(l_), save, dimension(0:nr_of_winddir_steps+1) :: cone_dist_array
    ! needed for solution sorting
    integer,dimension(max_nr_of_sol)    :: isort

    integer  :: mid_index
    ! only used for parabolic winddir searching
    real(l_) :: bestwinddir, bestconedist

    !---------------------------------------------------------
    ! start of the inversion code
    !---------------------------------------------------------
    IF (.not. inv_settings_are_defined) &
         CALL init_inv_settings_to_default()

    CALL init_inv_output(inv_output)

    ! print some welcome messages 
    if (inv_settings%verbosity .ge. inversion_verbosity_report) &
         CALL print_message(MSG_HELLO)

    ! reset the firstcall_warning flags to make sure a first warning
    ! is called for every WVC again
    if (inv_settings%verbosity .ge. inversion_verbosity_report) &
         CALL print_message(WARN_ERASE_FLAGS)

    ! check the validity of ka, kb and/or kc etc.
    if (inv_settings%do_input_data_check) then
       CALL check_input_data(inv_input)
    endif

    DO i = 1,inv_input%nr_of_views
       ! check if the polarimetric scheme is used, and set a switch if so
       if ( (inv_input%pol(i) .eq. c_po_pol ) .or. &
            (inv_input%pol(i) .eq. ku_po_pol)      ) then
          polarimetric_scheme_used = .true.
       end if
       ! check if the Doppler scheme is used, and set a switch if so
       if ( (inv_input%pol(i) .eq. c_vv_dop ) .or. &
            (inv_input%pol(i) .eq. c_hh_dop)      ) then
          doppler_scheme_used = .true.
       end if
    END DO

    IF (inv_input%nr_of_views .lt. 1) THEN
       ! inversion without data is not possible
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) &
            CALL print_message(WARN_NO_DATA)
       return
    END IF

    ! for inversion a GMF is used which assumes a rel_winddir=0
    ! when the wind blows towards the antenna.
    ! So if the antenna uses oceanographic convention and wind uses
    ! meteorological convention, just subtracting the 2 numbers is
    ! sufficient to get the rel_winddir. This is what is used
    ! in this module.
    ! However, if for one of these angles another convention is used,
    ! the result is 180 degrees off.
    ! Therefore I add 180 degrees to correct for this ......
    ! testing inv_settings%wind_angle_convention to be equal to 
    ! angle_convention_oceanographic convention is not done at the moment
    ! since that setting is not allowed. Output should always be in 
    ! meteorological convention, otherwise big trouble can be expected
    ! in the ambiguity removal schemes
    ! NOTE from Anton: in older versions of the software, this conversion
    ! was done on the wind directions after the inversion. This caused
    ! problems when the ERS/ASCAT cone distance normalisation was
    ! performed after the conversion of the wind directions.
    ! Therefore, I decided to apply the conversion on the antenna azimuth
    ! angles before doing the inversion. It should make no difference...
    !
    if (inv_settings%antenna_angle_convention .eq. &
        angle_convention_meteorological) then
      do i = 1,inv_input%nr_of_views
        inv_input%antenna_dir(i) = &
              mod(inv_input%antenna_dir(i) + 180.0, 360.0)
      enddo
    endif

    IF (inv_settings%use_zspace) &
         CALL convert_sigma_to_zspace(inv_input)

    ! in some cases the norm_factor can be calculated for each beam
    ! in advance before entering the loop, which saves a lot of 
    ! computations (this is only possible if normalisation is independent of 
    ! the found windspeed)
    if (inv_settings%use_meas_sigma0_for_normfactor) then
      CALL calc_normalisation(inv_input)
    endif 

    ! reset the initial wind speed used by find_minimum_cone_dist, 
    ! for every call of the inversion software, not just for the first one:
    ! (to prevent fatal attraction to u,v = 0,0, or sp=50)
    previous_windspeed = inv_settings%ws_initial_guess

    ! needed for skill calculation of ERS
    inv_output%mean_cone_distance = 0.0

    ! scan over all wind directions
    DO i = 1,nr_of_winddir_steps 
      ! calculate the current wind direction
      winddirection = winddirstep * i

      ! find windspeed that has minimum cone_distance for this direction
      ! and save those values for later use.
      CALL find_minimum_cone_dist(inv_input, winddirection,windspeed,cone_dist)

      ! use some local arrays to store the results
      windspeed_array(i) = windspeed
      cone_dist_array(i) = cone_dist

      ! determine mean, minimum and maximum cone distances
      inv_output%mean_cone_distance = inv_output%mean_cone_distance + cone_dist
      if (i .eq. 1) then
        inv_output%min_cone_distance = cone_dist
        inv_output%max_cone_distance = cone_dist
      else
        if (cone_dist .lt. inv_output%min_cone_distance) then
          inv_output%min_cone_distance = cone_dist
        endif
        if (cone_dist .gt. inv_output%max_cone_distance) then
          inv_output%max_cone_distance = cone_dist
        endif
      endif

    END DO ! wind_direction

    inv_output%mean_cone_distance = &
         inv_output%mean_cone_distance / nr_of_winddir_steps

    ! in case only one view is available, return min/max/average windspeed
    ! but no winddir can be derived....
    IF (inv_input%nr_of_views .eq. 1) THEN
       
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) &
            CALL print_message(WARN_ONE_VIEW) ! only one view is available

       if (inv_settings%max_nr_of_solutions .ge. 3) then
          inv_output%foundwindspeed(1)   = &
               my_min(windspeed_array,1,nr_of_winddir_steps)
          inv_output%foundwindspeed(2)   = &
               my_average(windspeed_array,1,nr_of_winddir_steps)
          inv_output%foundwindspeed(3)   = &
               my_max(windspeed_array,1,nr_of_winddir_steps)
          inv_output%nr_of_windsolutions = 3
          inv_output%wind_quality_code   = wqc_notok_nodirfound
       else
          ! with inv_settings%max_nr_of_solutions < 3 this type of output 
          ! is not possible, so flag it
          inv_output%wind_quality_code   = wqc_notok_noresult
       end if

       ! remember which GMF version was used
       inv_output%GMF_version_used = GMF_version_used
       
       return
    END IF

    multviews: IF (inv_input%nr_of_views .gt. 1) THEN
       testallwinddirs: IF (inv_settings%get_all_winddirs) THEN
          ! new case (MSS), with typically MAXSOL=144
          !  #[

          inv_output%nr_of_windsolutions = inv_settings%max_nr_of_solutions
          DO i=1,inv_output%nr_of_windsolutions
             ! just copy the found speed and MLE for each direction
             inv_output%foundwinddir(i)          = winddirstep * i
             inv_output%conedistance_measured(i) = cone_dist_array(i)
             inv_output%foundwindspeed(i)        = windspeed_array(i)
          END DO

          ! set the quality code, to indicate the GET_ALL_WINDDIRS
          ! was used
          ! the other wqc codes are not reported for this case,
          ! the user should check on LUT edge etc.
          inv_output%wind_quality_code = wqc_ok_144sol

          ! we are finished now for the MSS case, but we will also compute the
          ! normal case solutions (up to max_nr_of_sol_n) since they
          ! provide useful information
          ! the indices_solutions_n array contains pointers to those solutions
          ! out of the 144 that correspond to the normal solutions
          CALL get_indices_lowest_local_minima(cone_dist_array,&
                                     windspeed_array,1,nr_of_winddir_steps,&
                                     indices, max_nr_of_sol_n, &
                                     nr_of_minima, status, &
                                     inv_output%minimum_at_lutmin, inv_output%minimum_at_lutmax)

          inv_output%nr_of_windsolutions_n = nr_of_minima
          minima_n: DO i=1,inv_output%nr_of_windsolutions_n
             inv_output%indices_solutions_n(i) = indices(i)
          END DO minima_n

          !  #]
       ELSE
          ! normal case, with typically MAXSOL=4
          !  #[

          CALL get_indices_lowest_local_minima(cone_dist_array,&
                                     windspeed_array,1,nr_of_winddir_steps,&
                                     indices, inv_settings%max_nr_of_solutions, &
                                     nr_of_minima, status, &
                                     inv_output%minimum_at_lutmin, inv_output%minimum_at_lutmax)
          ! result is ok if status=0, 
          ! if more than inv_settings%max_nr_of_solutions minima found
          ! status=99 is reported below, but lowest minima are still reported
          
          inv_output%nr_of_windsolutions = nr_of_minima
          minima: DO i=1,inv_output%nr_of_windsolutions
             !  #[

             mid_index = indices(i)
             IF (inv_settings%do_parabolic_winddir_search) THEN
                CALL do_parabolic_winddir_search(mid_index,cone_dist_array, &
                     bestwinddir,bestconedist)
                !report the found solution back to the calling program
                inv_output%foundwinddir(i)          = bestwinddir
                inv_output%conedistance_measured(i) = bestconedist
             ELSE
                inv_output%foundwinddir(i)          = winddirstep * mid_index
                inv_output%conedistance_measured(i) = cone_dist_array(mid_index)
             ENDIF
             ! since windspeed does not change strongly with winddir
             ! I will not put much effort into doing the same trick for
             ! it, and just take the mid element.
             inv_output%foundwindspeed(i)        = windspeed_array(mid_index)

             !  #]
          END DO minima

          !  #]
       ENDIF testallwinddirs

       ! sort inversion output (also for MSS) by MLE ranking since in
       ! post_inversion the conedistance_measured of the first solution is used
       call GetSortIndex (inv_output%nr_of_windsolutions, &
                          inv_output%conedistance_measured, isort)
       call SortWithIndex(inv_output%nr_of_windsolutions, isort, &
                          inv_output%conedistance_measured)
       call SortWithIndex(inv_output%nr_of_windsolutions, isort, &
                          inv_output%foundwinddir)
       call SortWithIndex(inv_output%nr_of_windsolutions, isort, &
                          inv_output%foundwindspeed)

       ! in the MSS case, we also have an array of pointers to the normal
       ! solutions sort this array as well by MLE ranking
       IF (inv_output%nr_of_windsolutions_n .gt. 0) THEN
          nr_of_minima = 0
          DO i=1,inv_output%nr_of_windsolutions
             DO k=1,inv_output%nr_of_windsolutions_n
                IF (isort(i) .eq. inv_output%indices_solutions_n(k)) THEN
                   nr_of_minima = nr_of_minima + 1
                   ! temporarily store the sorted indices
                   indices(nr_of_minima) = i
                ENDIF
             END DO
             IF (nr_of_minima .ge. inv_output%nr_of_windsolutions_n) exit
          END DO
          DO i=1,inv_output%nr_of_windsolutions_n
             inv_output%indices_solutions_n(i) = indices(i)
          END DO
       ENDIF

       ! remember which GMF version was used
       inv_output%GMF_version_used = GMF_version_used

       ! determine whether the actually measured sigma values are
       ! inside or outside the cone.
       if (inv_settings%add_sign_to_mle) then
          call add_sign_to_mle(inv_input, inv_output, windspeed_array)
       endif

       IF (.not. inv_settings%get_all_winddirs) THEN
          CALL fill_wind_quality_code(inv_output, status)
       endif

    END IF multviews ! check for 1 or more views

    return

  end subroutine invert_one_wvc
  !  #]
  !---------------------------------
  subroutine Get_2D_MLE_array(inv_input,u,v,MLE_array)
    !  #[ 

    ! This routine calculates for given 2D arrays of u,v
    ! windcomponents the resulting MLE using the geometry defined
    ! for the current WVC.
    !
    ! This routine is needed by the rfscat FoM calculation.
    ! Size of the MLE_arrat will typically be: 201*201=39.000 elements
    ! times 4 bytes = 162kb

    ! input and output parameters for this routine
    TYPE(inv_input_type),      intent(inout) :: inv_input ! input
    real(r4_), dimension(:,:), intent(in)    :: u         ! input
    real(r4_), dimension(:,:), intent(in)    :: v         ! input
    real(r4_), dimension(:,:), intent(out)   :: MLE_array ! input
    
    ! local variables
    integer   :: i, j
    real(r4_) :: windspeed, winddir

    ! sanity checks
    if ( (size(u,1) .ne. size(v,1)) .or. &
         (size(u,2) .ne. size(v,2)) .or. &
         (size(u,1) .ne. size(MLE_array,1)) .or. &
         (size(u,2) .ne. size(MLE_array,2))      ) then
       print *,"ERROR while calculating MLE_array"
       print *,"input sizes of array u,v,MLE_array should be identical"
       print *,"but currently they differ !"
       print *,"*** ... quitting ... ***"
       CALL my_exit
    end if

    IF (.not. inv_settings_are_defined) &
         CALL init_inv_settings_to_default()

    ! check the validity of ka, kb and/or kc etc.
    if (inv_settings%do_input_data_check) then
       CALL check_input_data(inv_input)
    endif

    DO i = 1,inv_input%nr_of_views
       ! check if the polarimetric scheme is used, and set a switch if so
       if ( (inv_input%pol(i) .eq. c_po_pol ) .or. &
            (inv_input%pol(i) .eq. ku_po_pol)      ) then
          polarimetric_scheme_used = .true.
       end if
       ! check if the Doppler scheme is used, and set a switch if so
       if ( (inv_input%pol(i) .eq. c_vv_dop ) .or. &
            (inv_input%pol(i) .eq. c_hh_dop)      ) then
          doppler_scheme_used = .true.
       end if
    END DO

    IF (inv_input%nr_of_views .lt. 1) THEN
       ! inversion without data is not possible
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) &
            CALL print_message(WARN_NO_DATA)
       return
    END IF

    if (inv_settings%antenna_angle_convention .eq. &
        angle_convention_meteorological) then
       do i = 1,inv_input%nr_of_views
          inv_input%antenna_dir(i) = &
               mod(inv_input%antenna_dir(i) + 180.0, 360.0)
       enddo
    endif

    IF (inv_settings%use_zspace) &
         CALL convert_sigma_to_zspace(inv_input)

    ! in some cases the norm_factor can be calculated for each beam
    ! in advance before entering the loop, which saves a lot of 
    ! computations (this is only possible if normalisation is independent of 
    ! the found windspeed)
    if (inv_settings%use_meas_sigma0_for_normfactor) then
       CALL calc_normalisation(inv_input)
    endif 

    ! scan over all wind directions
    DO i = 1,size(u,1)
       DO j = 1,size(u,2)
          windspeed = uv_to_speed(u(i,j),v(i,j))
          winddir   = uv_to_dir(u(i,j),v(i,j))
          MLE_array(i,j) = calc_cone_distance(inv_input,windspeed,winddir)
       END DO ! winddir
    END DO ! windspeed

    ! Do we need this?
    ! Currently I will not implement this, since it needs at least one
    ! solution at the cone surface to do the trick. JK, 11-May-2009.

    ! determine whether the actually measured sigma values are
    ! inside or outside the cone.
    !if (inv_settings%add_sign_to_mle) then
    !   call add_sign_to_mle(inv_input, inv_output, windspeed_array)
    !endif

  end subroutine Get_2D_MLE_array
  !  #]
  !---------------------------------
  subroutine fill_wind_quality_code(inv_output, status)
    !  #[
    ! This routine applies a number of quality control tricks to
    ! fill in the wind_quality_code

    !---------------------------------------------------------
    ! input and output parameters for this routine
    !---------------------------------------------------------
    type(inv_output_type), intent(inout) :: inv_output
    integer, intent(in)                  :: status

    ! assume all is OK to start with
    inv_output%wind_quality_code   = wqc_ok

    ! no minima found
    IF (inv_output%nr_of_windsolutions .eq. 0) THEN
       inv_output%wind_quality_code   = wqc_notok_nosol
       return
    END IF

    ! more than inv_settings%max_nr_of_solution found
    IF (status .eq. 99) inv_output%wind_quality_code = wqc_ok_toomany

  end subroutine fill_wind_quality_code
  !---------------------------------
  !  #]
  !---------------------------------
  subroutine save_inv_input(filename,inv_input)
    !  #[
    ! write the inversion input elements to file, for plotting
    ! and debugging purposes (in a format suitable to be read by IDL)
    TYPE(inv_input_type), intent(in) :: inv_input
    character(len=*),intent(in)      :: filename

    integer                      :: fileunit, view

    print *,'opening file: ',trim(filename),' for writing inversion input'
    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='replace',action='write',&
         form="FORMATTED",ERR=99)
    write(fileunit,"(i3,1X,a)") inv_input%nr_of_views,"      ; nr_of_views"

    IF (inv_input%converted_to_zspace) THEN 
       write(fileunit,"(a)") "  1      ; sigma0's are converted to zspace" 
    ELSE
       write(fileunit,"(a)") "  0      ; sigma0's are not converted to zspace" 
    END IF

    DO view=1,inv_input%nr_of_views
       write(fileunit,'(i3,1X,a,i2,a)') inv_input%pol(view),&
            " ; pol(",view,")"
       write(fileunit,'(f8.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%antenna_dir(view)),&
            " ; antenna_dir(",view,")"
       write(fileunit,'(f8.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%theta_angle(view)),&
            " ; theta_angle(",view,")"
       write(fileunit,'(e13.5,1X,a,i2,a)') &
            convert_missing_real(inv_input%sigma_0(view)),&
            " ; sigma0(",view,")"
       write(fileunit,'(e11.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%kp_a(view)),&
            " ; kp_a(",view,")"
       write(fileunit,'(e11.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%kp_b(view)),&
            " ; kp_b(",view,")"
       write(fileunit,'(e11.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%kp_c(view)),&
            " ; kp_c(",view,")"
       write(fileunit,'(e11.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%sigma_0_vh(view)),&
            " ; sigma_0_vh(",view,")"
       write(fileunit,'(e11.3,1X,a,i2,a)') &
            convert_missing_real(inv_input%doppler_shift(view)),&
            " ; doppler_shift(",view,")"
    END DO
    write(fileunit,'(i4,1X,a,i2,a)') &
         convert_missing_integer(inv_input%node_nr)," ; node_nr"
    write(fileunit,'(i4,1X,a,i2,a)') &
         convert_missing_integer(inv_input%year)," ; year"
    write(fileunit,'(i6,1X,a,i2,a)') &
         convert_missing_integer(inv_input%cell_sampling)," ; cell_sampling"

    close(fileunit)
    CALL free_lun(fileunit)
    return

    !*** display error and stop if no file could be made
99  print *,'ERROR while creating the output file:  ',trim(filename)
    print *,'*** ... quitting ... ***'
    CALL my_exit

  end subroutine save_inv_input
  !  #]
  subroutine read_inv_input(filename,inv_input)
    !  #[
    ! read the inversion input elements from file, for plotting
    ! and debugging purposes (in a format suitable to be read by IDL)
    TYPE(inv_input_type), intent(out) :: inv_input
    character(len=*),intent(in)       :: filename
    integer                           :: fileunit, view, zspace_flag

    print *,'opening file: ',trim(filename),' for reading inversion input'
    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),action='read',&
         form="FORMATTED",ERR=99)
    read(fileunit,"(i3)") inv_input%nr_of_views
    read(fileunit,"(i3)") zspace_flag

    inv_input%converted_to_zspace = .false.
    IF (zspace_flag .gt. 0) THEN
       inv_input%converted_to_zspace = .true.
       inv_settings%use_zspace       = .true.
    ENDIF

    DO view=1,inv_input%nr_of_views
       read(fileunit,"(i3)") inv_input%pol(view)
       read(fileunit,"(f8.3)") inv_input%antenna_dir(view)
       read(fileunit,"(f8.3)") inv_input%theta_angle(view)
       read(fileunit,"(e13.5)") inv_input%sigma_0(view)
       read(fileunit,"(e11.3)") inv_input%kp_a(view)
       read(fileunit,"(e11.3)") inv_input%kp_b(view)
       read(fileunit,"(e11.3)") inv_input%kp_c(view)
       read(fileunit,"(e11.3)") inv_input%sigma_0_vh(view)
       read(fileunit,"(e11.3)") inv_input%doppler_shift(view)
    END DO
    read(fileunit,'(i3)') inv_input%node_nr
    read(fileunit,'(i4)') inv_input%year
    read(fileunit,'(i6)') inv_input%cell_sampling

    close(fileunit)
    CALL free_lun(fileunit)
    return

    !*** display error and stop if no file could be read
99  print *,'ERROR while reading the input file:  ',trim(filename)
    print *,'*** ... quitting ... ***'
    CALL my_exit

  end subroutine read_inv_input
  !  #]
  subroutine save_inv_output(filename,inv_output)
    !  #[
    TYPE(inv_output_type) :: inv_output
    character(len=*),intent(in)  :: filename
    integer                      :: fileunit, sol

    print *,'opening file: ',trim(filename),' for writing inversion output'
    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='replace',action='write',&
         form="FORMATTED",ERR=99)
    write(fileunit,*) inv_output%nr_of_windsolutions
    write(fileunit,*) inv_output%wind_quality_code
    write(fileunit,*) inv_output%GMF_version_used
    DO sol=1,inv_output%nr_of_windsolutions
       write(fileunit,*) inv_output%foundwindspeed(sol)
       write(fileunit,*) inv_output%foundwinddir(sol)
       write(fileunit,*) inv_output%conedistance_measured(sol)
       write(fileunit,*) inv_output%probability(sol)
    END DO

    close(fileunit)
    CALL free_lun(fileunit)
    return

    !*** display error and stop if no file could be made
99  print *,'ERROR while creating the output file:  ',trim(filename)
  print *,'*** ... quitting ... ***'
  CALL my_exit

  end subroutine save_inv_output
  !---------------------------------
  !  #]
  subroutine do_parabolic_winddir_search(mid_index, cone_dist_array, &
                                         bestwinddir,bestconedist)
  !  #[
    real(l_), dimension(0:nr_of_winddir_steps+1) :: cone_dist_array
    real(l_),intent(out) :: bestwinddir, bestconedist
    integer  :: mid_index, low_index, high_index
    real(l_) :: winddir0,  winddir1,  winddir2
    real(l_) :: conedist0, conedist1, conedist2

    ! min/max possible values contained in indices(i) 
    !are 1,nr_of_winddir_steps
    low_index  = mid_index - 1
    high_index = mid_index + 1

    ! for winddir values of 0 or 360+step degrees are ok at this point
    winddir0  = winddirstep * low_index
    winddir1  = winddirstep * mid_index
    winddir2  = winddirstep * high_index

    ! for conedist I have to take circularity into account
    cone_dist_array(0) = cone_dist_array(nr_of_winddir_steps)
    cone_dist_array(nr_of_winddir_steps+1) = cone_dist_array(1)
    
    conedist0 = cone_dist_array(low_index)
    conedist1 = cone_dist_array(mid_index)
    conedist2 = cone_dist_array(high_index)
    
    IF ( (conedist0 .ge. conedist1)  .and. &
         (conedist1 .le. conedist2)        ) THEN
       ! now apply the parabolic fit to get a better estimate
       ! of the real minimum.
       CALL get_parabolic_minimum(winddir0,    conedist0,  &
                                  winddir1,    conedist1,  &
                                  winddir2,    conedist2,  &
                                  bestwinddir, bestconedist )
       ! bestwinddir might be between 360.0 and 
       ! 360.0+winddirstep degrees which is not
       ! according to the dir definition, so correct for that:
       !           IF (bestwinddir .le. 0.0) bestwinddir = bestwinddir + 360.0
       IF (bestwinddir .gt. 360.0) bestwinddir = bestwinddir - 360.0
    ELSE
       ! fall back to the old method, for (very) flat areas
       bestwinddir  = winddirstep * mid_index
       bestconedist = cone_dist_array(mid_index)
    ENDIF
  end subroutine do_parabolic_winddir_search
  !---------------------------------
  !  #]
  subroutine calc_normalisation(inv_input)
  !  #[
    ! in this case the norm_factor can be calculated in advance
    ! before entering the loop, which saves a lot of computations

    TYPE(inv_input_type) :: inv_input
    integer :: i

    DO i = 1,inv_input%nr_of_views
        IF ( (inv_input%pol(i) .eq. c_po_pol ) .or. &
             (inv_input%pol(i) .eq. ku_po_pol)      ) THEN

           IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
              ! special case for crosspol/polarimetric measurement
              IF (inv_input%sigma_0_vh(i) .eq. 0.0) THEN
                 print *,'WARNING: sigma_0_vh = 0.0 inside invert_one_wvc!!!'
              END IF
           END if

           ! norm_factor_meas is a global array
           norm_factor_meas(i) = calc_var_s0(inv_input%sigma_0_vh(i),&
                inv_input%kp_a(i),inv_input%kp_b(i),inv_input%kp_c(i))
        ELSE
           norm_factor_meas(i) = calc_var_s0(inv_input%sigma_0(i),&
                inv_input%kp_a(i),inv_input%kp_b(i),inv_input%kp_c(i))
        END IF
    END DO
  end subroutine calc_normalisation
  !---------------------------------
  !  #]
  subroutine add_sign_to_mle(inv_input, inv_output, windspeed_array)
    !  #[

    ! Determine whether the
    ! actually measured sigma values are inside or outside
    ! the cone. This is done by calculating the inner product
    ! of c-s and m-s. Here c-s is the vector from the solution
    ! (on the cone surface) to the center of the cone, and
    ! m-s is the vector from the solution (on the cone surface)
    ! towards the actually measured set of sigmas.
    ! So a positive sign means the measurement lies inside the cone
    ! and a negative sign means the measurement lies outside the cone

    ! input parameters
    TYPE(inv_input_type), intent(in) :: inv_input
    TYPE(inv_output_type), intent(inout) :: inv_output
    real(l_), dimension(0:nr_of_winddir_steps+1), intent(in) :: windspeed_array

    real(l_),dimension(1:max_nr_of_views) :: c_vector
    real(l_),dimension(1:max_nr_of_views) :: m_vector
    real(l_),dimension(1:max_nr_of_views,1:max_nr_of_sol) :: s_vector
    real(l_) :: rel_wind_dir
    real(l_) :: inproduct
    real(l_) :: winddirection
    real(l_) :: windspeed
    integer  :: nr_wind_dirs
    integer  :: i,j,k

    ! - for each view, calculate the average over all wind directions
    !   of the sigma0 for this view, for wind speed on the line
    !   of the minimum valley. This is the cone_center vector c.
    ! - copy the measured sigma vector to m.
    ! - put the sigma-vector for the found solutions in s
    do i=1,inv_input%nr_of_views
      c_vector(i)  = 0.0
      nr_wind_dirs = 0
      ! to speed up computing, only 1/4 of all winddirs is used to compute c
      do j=1,nr_of_winddir_steps,4
        nr_wind_dirs  = nr_wind_dirs + 1
        ! calculate the (relative) current wind direction
        winddirection = winddirstep * j
        rel_wind_dir  = winddirection - inv_input%antenna_dir(i)
        ! take the current wind speed for this direction
        windspeed     = windspeed_array(j)
        c_vector(i)   = c_vector(i) + &
                        calc_sigma0(windspeed, rel_wind_dir,&
                                    inv_input%theta_angle(i), inv_input%pol(i))
      enddo
      c_vector(i) = c_vector(i)/nr_wind_dirs
      m_vector(i) = inv_input%sigma_0(i)

      s_vector(i,:) = 0.0
      do k=1,inv_output%nr_of_windsolutions
        winddirection = inv_output%foundwinddir(k)
        rel_wind_dir  = winddirection - inv_input%antenna_dir(i)
        s_vector(i,k) = calc_sigma0(inv_output%foundwindspeed(k),rel_wind_dir,&
                                    inv_input%theta_angle(i),inv_input%pol(i))
      enddo
    enddo

    ! now calculate the inner product and change the MLE according to
    ! the found sign of the inner product
    do k=1,inv_output%nr_of_windsolutions
      inproduct = 0.0
      do i=1,inv_input%nr_of_views
        inproduct = inproduct + (c_vector(i)-s_vector(i,k)) * &
                                (m_vector(i)-s_vector(i,k))
      enddo

      if (inproduct .le. 0.0) then
        inv_output%conedistance_measured(k) = &
              -1.0 * inv_output%conedistance_measured(k)
      endif
    enddo

  end subroutine add_sign_to_mle
  !---------------------------------

  !  #]
  subroutine print_message(code)
  !  #[
    integer :: code, i
    logical, dimension(0:NR_OF_WARNINGS), save :: firstcall_warning = .true.
    logical, save :: firstcall_msg_hello = .true.

    if (code .eq. WARN_ERASE_FLAGS) then
       do i=1,NR_OF_WARNINGS
          firstcall_warning(i) = .true.
       enddo
    else
       if (.not. inv_settings%first_warning_only .or. firstcall_warning(code)) then
          firstcall_warning(code) = .false.

          SELECT CASE (code)
          CASE (WARN_NO_DATA)
             ! inversion without data is not possible
             print *,'WARNING: inversion without data is not possible'
             print *,'WARNING: returning without results'
          CASE (WARN_ONE_VIEW)
             ! only one view is available
             print *,'WARNING: only one view is available'
             print *,'WARNING: no wind direction can be determined,'
             print *,'WARNING: just a minimum, average and maximum windspeed'
          CASE (WARN_LUT_HIGH_WS)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (windspeed > ',LUT_max_windspeed,')'
          CASE (WARN_LUT_HIGH_WD)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (winddir > ',LUT_max_winddir,')'
          CASE (WARN_LUT_HIGH_TH)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (theta > ',LUT_max_theta,')'
          CASE (WARN_LUT_LOW_WS)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (windspeed < ',LUT_min_windspeed,')'
          CASE (WARN_LUT_LOW_WD)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (winddir < ',LUT_min_winddir,')'
          CASE (WARN_LUT_LOW_TH)
             print *,'WARNING: outside look-up-table, ' // &
                     'giving boundary value as result'
             print *,'WARNING: (theta < ',LUT_min_theta,')'
          CASE (WARN_HIGHNR_MIN)
             print *,'WARNING: more than',inv_settings%max_nr_of_solutions,&
                     ' local minima found in array !'
             print *,'WARNING: ==>Returning only deepest ',&
                  inv_settings%max_nr_of_solutions,' results.'
          CASE (WARN_NO_MINIMA)
             print *,'WARNING: No minima found !!!!!!!'
          CASE (WARN_LOW_VAR_S0)
             print*, 'EUREKA!'
             print *,'WARNING: SD_sq < 1.0e-25 in function calc_var_s0'
             print *,'WARNING: setting it to 1.0e-25 to prevent divide by zero !'
          CASE (WARN_WS_STEP)
             print *,'WARNING: it does not make sence to interpolate the'
             print *,'WARNING: velocity if you choose the minimum step in v'
             print *,'WARNING: equal to the stepsize in the Look-Up-Table !'
             print *,'WARNING: Rather choose INTERPOLATE1D or INTERPOLATE2D'
             print *,'WARNING: then INTERPOLATE3D for these settings.....'
          CASE (MSG_HELLO)
             IF (firstcall_msg_hello) THEN
                firstcall_msg_hello = .false.
                print *,'---------------------------------------------------'
                print *,'Welcome to the new scatterometry inversion software'
                print *,'---------------------------------------------------'
             ENDIF
          CASE DEFAULT
             print *,'WARNING: this warning code has not yet been implemented !'
             print *,'         warning code = ',code
          END SELECT

       endif
    endif

    return
  end subroutine print_message
  !---------------------------------
  !  #]
  subroutine init_inv_input(inv_input)
  !  #[
    TYPE(inv_input_type), intent(out) :: inv_input

    integer :: i
    inv_input%nr_of_views = 0
    inv_input%converted_to_zspace = .false.

    DO i=1,max_nr_of_views
       inv_input%pol(          i) = missing_indicator_integer
       inv_input%antenna_dir(  i) = missing_indicator_real
       inv_input%theta_angle(  i) = missing_indicator_real
       inv_input%sigma_0(      i) = missing_indicator_real
       inv_input%kp_a(         i) = missing_indicator_real
       inv_input%kp_b(         i) = missing_indicator_real
       inv_input%kp_c(         i) = missing_indicator_real
       inv_input%sigma_0_vh(   i) = missing_indicator_real
       inv_input%doppler_shift(i) = missing_indicator_real
    END DO
    inv_input%node_nr       = missing_indicator_integer 
    inv_input%year          = missing_indicator_integer 
    inv_input%cell_sampling = missing_indicator_integer 

  end subroutine init_inv_input
  !---------------------------------
  !  #]
  subroutine init_inv_output(inv_output)
  !  #[
    TYPE(inv_output_type), intent(out) :: inv_output

    ! init output arrays for case of "no solutions"
    inv_output%foundwindspeed(:)        = missing_indicator_real
    inv_output%foundwinddir(:)          = missing_indicator_real
    inv_output%conedistance_measured(:) = missing_indicator_real
    inv_output%probability(:)           = missing_indicator_real

    inv_output%indices_solutions_n(:)   = missing_indicator_integer

    inv_output%skill                 = missing_indicator_real
    inv_output%nr_of_windsolutions   = 0
    inv_output%nr_of_windsolutions_n = 0
    inv_output%minimum_at_lutmin     = .false.
    inv_output%minimum_at_lutmax     = .false.
    inv_output%wind_quality_code     = 999 ! no solutions
    inv_output%GMF_version_used      = -1
    inv_output%mean_cone_distance    = missing_indicator_real
    inv_output%min_cone_distance     = missing_indicator_real
    inv_output%max_cone_distance     = missing_indicator_real
    inv_output%smooth_width          = -1

  end subroutine init_inv_output
    !---------------------------------
    !  #]
  subroutine init_inv_settings_to_default()
    !  #[
    IF (inv_settings_are_defined) THEN
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
          print *,"init_inv_settings_to_default: overriding old settings with defaults"
       endif
    ELSE
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,"init_inv_settings_to_default: initialising settings to defaults"
       endif
    END IF

    ! use a global saved variable for this module to remember that
    ! the settings have been initialised
    inv_settings_are_defined = .true.

    ! set the canary
    inv_settings%struct_was_properly_initialised = inv_settings_canary

    ! setting of verbosity: print errors only is default
    inv_settings%verbosity = inversion_verbosity_error

    ! setting of instrument type: RFSCAT is default
    inv_settings%instrument_type = instrument_type_rfscat

    ! BEWARE: these next 2 settings define whether 180 degrees will be added
    ! to the found winddir to correct for it, or not. So be very sure of
    ! your conventions before changing this.
    ! ERS and ASCAT use angle_convention_meteorological
    ! NSCAT and SEAWINDS use angle_convention_oceanographic
    inv_settings%antenna_angle_convention = angle_convention_meteorological

    ! BEWARE: this should always be meteorological, because ambiguity 
    ! removal needs this. (for example for conversion to u,v)
    inv_settings%wind_angle_convention    = &
         angle_convention_meteorological ! this is true for BUFR products

    inv_settings%use_zspace = .false.

    ! defines whether the norm_factor (MLE) is calculated with some sort
    ! of normalisation or not
    ! set to true for ERS and ASCAT
    inv_settings%no_normalisation = .false.

    ! choose whether or not to calculate if the measurement is inside
    ! or outside the cone
    ! set to true for ERS and ASCAT
    inv_settings%add_sign_to_mle = .false.

    ! always false for all instruments, a user should request it explicitly
    inv_settings%get_all_winddirs = .false.

    ! always true for all instruments (gives best results)
    inv_settings%do_parabolic_minimum_search = .true.
    ! defines if the minimum should be calculated by using 
    ! a parabolic fit to the lowest, and the left and right
    ! neighbouring points, while scanning the windspeed.
    ! if not set the middle point is used as minimum.

    ! defines if the minima along the winddir scan of the minimum valley 
    ! also should use the parabolic fit to determine a better angle.
    ! set to true for all instruments (gives best results)
    ! since this switch causes a small error in windvelocity
    ! for the polarimetric case, it should be switched off for RFSCAT
    inv_settings%do_parabolic_winddir_search = .true.

    ! WARNING: Inversion and QC have been optimized for 
    ! inv_settings%max_nr_of_solutions = 4. No major impact is expected when
    ! this parameter is > 4. However, this is not the case for values < 4.
    ! A thorough validation of inversion & QC would be needed in such case.
    ! WARNING: This parameter can be overridden before inversion, e.g.,
    ! for ERS, this happens in the "init_inversion_module_for_ERS" subroutine
    ! (ers_cmline module), and/or during inversion (in this module), e.g., in 
    ! case the Multiple solution Scheme (MSS) is used (i.e., if 
    ! inv_settings%get_all_winddirs=.true.)
    inv_settings%max_nr_of_solutions = 4

    inv_settings%experimental_ers_wqc_checks = .false.

    inv_settings%use_meas_sigma0_for_normfactor = .false.

    inv_settings%add_log_term_to_cost = .false.

    inv_settings%use_which_fn_to_construct_LUT = dont_try_to_construct_LUT

    ! print warning message only if it occurs for the first time
    inv_settings%first_warning_only = .true.

    ! check the validity of ka, kb and/or kc etc.
    ! RFSCAT: gaat niet samen met READ_PSEUDO_L1b_FILE!
    inv_settings%do_input_data_check = .true.

    ! settings used for stepping in the windspeed in search for a
    ! minimum in MLE. First steps of initial_ws_step are taken, starting
    ! at ws_initial_guess.
    ! Then, when a minimum is found, the steps are reduced by multiplying
    ! the stepsize by refiner_ws_step. This is repeated untill the stepsize
    ! gets below minimum_ws_step and then the found minimum is taken as
    ! minimum in MLE for this azimuth.
    ! For the next search at the next azimuth value the previously found
    ! windspeed is used, rather than the ws_initial_guess value.

    ! 0.2 is a general value, 0.5 was used for 1D/2D interpolation with ERS
    inv_settings%initial_ws_step  = 0.2

#ifdef INTERPOLATE3D
    inv_settings%minimum_ws_step  = 0.02
#else
    inv_settings%minimum_ws_step  = 0.2
#endif

    inv_settings%refiner_ws_step  = 0.1

    ! 8.0 seems appropriate for SeaWinds, 10.0 for other instruments
    ! to be set in calling program
    inv_settings%ws_initial_guess = 10.0

    ! a weight factor to be able to combine polarimetric measurements
     ! with normal VV/HH measurements (tested for RFSCAT only)
    inv_settings%polarimetric_weight = 1.0

    ! a weight factor to be able to combine doppler measurements
    ! with normal VV/HH measurements (tested for RFSCAT only)
    inv_settings%doppler_weight = 1.0e5

  end subroutine init_inv_settings_to_default
    !---------------------------------
    !  #]
  subroutine write_invsettings_to_file(fileunit)
   !  #[
   ! write all settings to a file
    ! inputs
    integer                 :: fileunit

    ! local variable
    character(len=256)      :: textline

    write(fileunit,*) "printing CPP and parameter settings used by inversion:"

    write(fileunit,*) "LUT_FILENAME_C_VV     is set to: ",&
         trim(filename_c_vv)
    write(fileunit,*) "LUT_FILENAME_C_HH     is set to: ",&
         trim(filename_c_hh)
    write(fileunit,*) "LUT_FILENAME_C_POL    is set to: ",&
         trim(filename_c_pol)
    write(fileunit,*) "LUT_FILENAME_KU_VV    is set to: ",&
         trim(filename_ku_vv)
    write(fileunit,*) "LUT_FILENAME_KU_HH    is set to: ",&
         trim(filename_ku_hh)
    write(fileunit,*) "LUT_FILENAME_KU_POL   is set to: ",&
         trim(filename_ku_pol)
    write(fileunit,*) "LUT_FILENAME_C_VV_DOP is set to: ",&
         trim(filename_c_vv_dop)
    write(fileunit,*) "LUT_FILENAME_C_HH_DOP is set to: ",&
         trim(filename_c_hh_dop)

#ifdef INTERPOLATE1D
    write(fileunit,*) "INTERPOLATION TYPE IS 1D"
#else
#ifdef INTERPOLATE2D
    write(fileunit,*) "INTERPOLATION TYPE IS 2D"
#else
#ifdef INTERPOLATE2DV
    write(fileunit,*) "INTERPOLATION TYPE IS 2DV"
#else
#ifdef INTERPOLATE3D
    write(fileunit,*) "INTERPOLATION TYPE IS 3D"
#endif
#endif
#endif
#endif

    write(fileunit,*) "NR_OF_WINDDIRSTEPS is set to ",nr_of_winddir_steps
    write(fileunit,'(A,F8.5)') " INITIAL_WS_STEP  is set to ", &
       inv_settings%initial_ws_step
    write(fileunit,'(A,F8.5)') " MINIMUM_WS_STEP  is set to ", &
       inv_settings%minimum_ws_step
    write(fileunit,'(A,F8.5)') " REFINER_WS_STEP  is set to ", &
       inv_settings%refiner_ws_step
    write(fileunit,'(A,F8.5)') " WS_INITIAL_GUESS is set to ", &
       inv_settings%ws_initial_guess

    if (inv_settings%first_warning_only) then
       write(fileunit,*) "FIRST_WARNING_ONLY           is set"
    else
       write(fileunit,*) "FIRST_WARNING_ONLY           is not set"
    endif

    if (inv_settings%do_input_data_check) then
       write(fileunit,*) "DO_INPUT_DATA_CHECK          is set"
    else
       write(fileunit,*) "DO_INPUT_DATA_CHECK          is not set"
    endif

    if (inv_settings%no_normalisation) then
       write(fileunit,*) "NO_NORMALISATION             is set"
    else
       write(fileunit,*) "NO_NORMALISATION             is not set"
    endif

    if (inv_settings%add_sign_to_mle) then
       write(fileunit,*) "ADD_SIGN_TO_MLE              is set"
    else
       write(fileunit,*) "ADD_SIGN_TO_MLE              is not set"
    endif

#ifdef USE_8BYTE_REALS
    write(fileunit,*) "USE_8BYTE_REALS              is set"
#else
    write(fileunit,*) "USE_8BYTE_REALS              is not set"
#endif

    write(fileunit,*) "POLARIMETRIC_WEIGHT is set to: ",&
         inv_settings%polarimetric_weight

    write(fileunit,*) "DOPPLER_WEIGHT      is set to: ",&
         inv_settings%doppler_weight

    ! write the contents of the inv_settings
    IF (.not. inv_settings%struct_was_properly_initialised .eq. &
              inv_settings_canary) THEN
       print *,"=================================="
       print *,"write_inv_settings_to_file: unable to write the inv_settings"
       print *,"since they have not yet been set"
       print *,"=================================="
       return
    ENDIF

    write(fileunit,*) "Settings for the Inversion module:"

    SELECT CASE (inv_settings%antenna_angle_convention)
    CASE(angle_convention_meteorological)
       textline = "==> antenna_angle_convention: meteorological"
    CASE(angle_convention_oceanographic)
       textline = "==> antenna_angle_convention: oceanographic"
    CASE DEFAULT
       print *,"==>sorry, invalid value for:"
       print *,"inv_settings%antenna_angle_convention = ",&
                inv_settings%antenna_angle_convention
       stop
    END SELECT
    write(fileunit,*) trim(textline)

    SELECT CASE (inv_settings%wind_angle_convention)
    CASE(angle_convention_meteorological)
       textline = "==> wind_angle_convention: meteorological"
    CASE(angle_convention_oceanographic)
       textline = "==> wind_angle_convention: oceanographic"
    CASE DEFAULT
       print *,"==>sorry, invalid value for:"
       print *,"inv_settings%wind_angle_convention = ",&
                inv_settings%wind_angle_convention
       stop
    END SELECT
    write(fileunit,*) trim(textline)

    textline = "==> use_zspace = .false."
    IF (inv_settings%use_zspace) textline = "==> use_zspace = .true."
    write(fileunit,*) trim(textline)

    textline = "==> get_all_winddirs = .false."
    IF (inv_settings%get_all_winddirs) &
         textline = "==> get_all_winddirs = .true."
    write(fileunit,*) trim(textline)

    textline = "==> inv_settings%parabolic_winddir_search: is not set"
    IF (inv_settings%do_parabolic_winddir_search) &
         textline = "==> inv_settings%parabolic_winddir_search: is set"
    write(fileunit,*) trim(textline)

    textline = "==> inv_settings%parabolic_minimum_search: is not set"
    IF (inv_settings%do_parabolic_minimum_search) &
         textline = "==> inv_settings%parabolic_minimum_search: is set"
    write(fileunit,*) trim(textline)

    write(fileunit,*) "==> inv_settings%max_nr_of_solutions = ",&
                           inv_settings%max_nr_of_solutions

    IF (inv_settings%experimental_ers_wqc_checks) THEN 
       textline = "==> using experimental ERS-only wqc checks"
    ELSE
       textline = "==> NOT using experimental ERS-only wqc checks"
    ENDIF
    write(fileunit,*) trim(textline)

    if (inv_settings%use_meas_sigma0_for_normfactor) then
       textline = "==> use_meas_sigma0_for_normfactor is set"
    else
       textline = "==> use_meas_sigma0_for_normfactor is not set"
    endif
    write(fileunit,*) trim(textline)

    SELECT CASE (inv_settings%use_which_fn_to_construct_LUT)
    CASE(dont_try_to_construct_LUT)
       textline = "==> no construction of LUT will be attempted"
    CASE(use_cmod4_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD4"
    CASE(use_cmod5_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD5"
    CASE(use_cmod5_5_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD5_5"
    CASE(use_cmod5_n_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD5_N"
    CASE(use_cmod6_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD6"
    CASE(use_cmod7_to_construct_LUT)
       textline = "==> function_to_construct_LUT: CMOD7"
    CASE DEFAULT
       print *,"==>sorry, invalid value for:"
       print *,"inv_settings%use_which_fn_to_construct_LUT = ",&
                inv_settings%use_which_fn_to_construct_LUT
       stop
    END SELECT
    write(fileunit,*) trim(textline)

  end subroutine write_invsettings_to_file
  !--------------------------------
  !  #]
  subroutine print_invsettings()
    !  #[
    call write_invsettings_to_file(fileunit_stdout)

  end subroutine print_invsettings
    !  #]
  subroutine get_inv_settings(reported_settings,error)
    !  #[
    TYPE(inv_settings_type), intent(out) :: reported_settings
    integer, intent(out) :: error

    error = 0 ! all ok
    IF (inv_settings_are_defined) THEN
       reported_settings = inv_settings
    ELSE
       print *,"WARNING: settings were not yet defined"
       print *,"they shoud be initialised to default or set"
       print *,"before calling get_inv_settings() !!!"
       error = 1
    ENDIF

  end subroutine get_inv_settings
    !---------------------------------
    !  #]
  subroutine set_inv_settings(new_settings)
    !  #[

    TYPE(inv_settings_type), intent(in) :: new_settings

    ! check whether the canary was set, or not....
    if (new_settings%struct_was_properly_initialised .ne. & 
        inv_settings_canary) then
       print *,"ERROR: you are trying to set new settings for the"
       print *,"inversion module with set_inv_settings(new_settings)"
       print *,"but the new_settings structure was not yet initialised !!!"
       print *,"Please initialise it first using the"
       print *,"init_inv_settings_to_default() routine......"
       print *,"Then get the struct using get_inv_settings()"
       print *,"Then change whatever settings you wish to change"
       print *,"and only then call set_inv_settings()"
       stop
    endif

    if (inv_settings_are_defined) then
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,"set_inv_settings: overriding old settings"
       endif
    else
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,"set_inv_settings: activating new settings"
       endif
    endif

    ! copy all settings
    inv_settings = new_settings
    inv_settings_are_defined = .true.
    
    ! do a sanity check on the new settings

    ! BEWARE: this should always be meteorological, because ambiguity 
    ! removal needs this. (for example for conversion to u,v)
    if (inv_settings%wind_angle_convention .ne. &
        angle_convention_meteorological) then
       print *,"ERROR: invalid setting for inv_settings%wind_angle_convention"
       print *,"in set_inv_settings()."
       stop
    endif

    if ((inv_settings%antenna_angle_convention .lt. 1) .or. &
        (inv_settings%antenna_angle_convention .gt. 2)) then
       print *,"ERROR: invalid setting for inv_settings%antenna_angle_convention"
       print *,"in set_inv_settings()."
       stop
    endif

    if (inv_settings%get_all_winddirs) then
       if (inv_settings%max_nr_of_solutions .ne. nr_of_winddir_steps) then
          print *,"WARNING in set_inv_settings"
          print *,"setting inv_settings%max_nr_of_solutions to ", &
             nr_of_winddir_steps
          print *,"because the switch inv_settings%get_all_winddirs is set"
          inv_settings%max_nr_of_solutions = nr_of_winddir_steps
       endif
    endif

    if (inv_settings%max_nr_of_solutions .gt. max_nr_of_sol) then
       print *,"ERROR: invalid setting for inv_settings%max_nr_of_solutions"
       print *,"in set_inv_settings()."
       print *,"It should not be larger than: max_nr_of_sol = ", &
          max_nr_of_sol
       stop
    endif

    if (inv_settings%ws_initial_guess .lt. lower_speed_threshold) then
       print *,"ERROR: invalid setting for inv_settings%initial_ws_step"
       print *,"in set_inv_settings()."
       print *,"It should not be lower than lower_speed_threshold = ", &
          lower_speed_threshold
       stop
    endif

    if (inv_settings%ws_initial_guess .gt. higher_speed_threshold) then
       print *,"ERROR: invalid setting for inv_settings%initial_ws_step"
       print *,"in set_inv_settings()."
       print *,"It should not be lower than higher_speed_threshold = ", &
          higher_speed_threshold
       stop
    endif

  end subroutine set_inv_settings
    !---------------------------------

    !  #]
  subroutine check_input_data(inv_input)
  !  #[

    ! check the validity of ka, kb and/or kc etc.
    ! this check should not be done inside calc_var_s0 because that
    ! function is called a lot of times, and the check only has to
    ! be done once per wvc/node
    TYPE(inv_input_type) :: inv_input
    integer :: i
    logical :: correct_input

    correct_input = .true. ! assume all is ok

    ! check for missing data ...
    IF (inv_input%nr_of_views .le. 0) correct_input = .false.

    DO i=1,inv_input%nr_of_views
       IF (MISSING_REAL(inv_input%antenna_dir(i))) correct_input = .false.
       IF (MISSING_REAL(inv_input%theta_angle(i))) correct_input = .false.
       IF (MISSING_REAL(inv_input%sigma_0(    i))) correct_input = .false.
       IF (MISSING_REAL(inv_input%kp_a(       i))) correct_input = .false.
       IF (MISSING_REAL(inv_input%kp_b(       i))) correct_input = .false.
       IF (MISSING_REAL(inv_input%kp_c(       i))) correct_input = .false.

       ! REMARK: for seawinds kc is provided in dB values in the BUFR product.
       ! This should be converted to normal values before running inversion,
       ! since inversion uses normal values for sigma0 !!!
       ! The following test should catch such mistakes
       IF (inv_input%kp_c(i) .lt. -1.) correct_input = .false.
       IF (inv_input%kp_c(i) .gt.  5.) correct_input = .false.

       IF ( (inv_input%pol(i) .eq. c_po_pol ) .or. &
            (inv_input%pol(i) .eq. ku_po_pol)      )  THEN
          ! this input is only used for the polarimetric measurements
          IF (MISSING_REAL(inv_input%sigma_0_vh( i))) correct_input = .false.
       END IF

       IF ( (inv_input%pol(i)  .le.  0) .or. &
            (inv_input%pol(i)  .gt. largest_pol_code) ) correct_input = .false.

       IF ( (inv_input%pol(i) .eq. c_vv_dop) .or. &
            (inv_input%pol(i) .eq. c_hh_dop)      ) THEN
          IF (MISSING_REAL(inv_input%doppler_shift( i))) correct_input = .false.
       END IF
    END DO

    IF (.not. correct_input) THEN
        print *,'PROBLEM!!! not all inv_input elements are valid ...'
        CALL print_input_data_of_inversion(inv_input)
        CALL my_exit
    END IF

    ! other checks that might be included in the future are:
    !   -sensible sigma values (not negative, not above 1, etc.)

    ! this check was added by Marcos in version 1.3.2 but it does not
    ! work always for me, because I often use tests where kb=0 !
    !   IF (ka .eq. 0 .or. kb .eq. 0) STOP 'KP PROBLEM!!!'

    ! this check is ok for me:
    DO i=1,inv_input%nr_of_views
      if (.not. inv_settings%no_normalisation) then
         IF ( ( (inv_input%kp_a(i) .eq. 0.) .and. &
                (inv_input%kp_b(i) .eq. 0.) .and. &
                (inv_input%kp_c(i) .eq. 0.)      )  .or. &
              (missing_real(inv_input%kp_a(i))   )       ) THEN
            print *,'KP PROBLEM!!! ka,b and c = 0 or ka = DEFAULT !!!'
            CALL my_exit
         END IF
      endif

      IF (inv_settings%use_zspace) THEN
         IF (inv_input%sigma_0(i) .lt. 0.0) THEN
            print *,'ERROR: a negative measured sigma0 should never'
            print *,'ERROR: occur, and will give big problems when'
            print *,'ERROR: using ZSPACE !!!'
            print *,'ERROR: Probably you are using dB values and not'
            print *,'ERROR: real backscatter values !!!!'
            print *,'quitting ....'
            stop
         END IF
      END IF
    END DO

    ! check the sensibility of the interpolation setting
#ifdef INTERPOLATE3D
    if (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
       if (inv_settings%minimum_ws_step .eq. LUT_windspeed_step) then
          CALL print_message(WARN_WS_STEP)
       end if
    end if
#endif

    return
  end subroutine check_input_data
  !---------------------------------

  !  #]
  subroutine find_minimum_cone_dist(inv_input, winddirection, windspeed, &
                                    cone_dist)
  !  #[

    ! scan wind speed for this direction and find velocity
    ! that has a minimum distance to the cone
    ! (there should always be just one minimum ! so no difficulties
    !  with local minima etc.)

    ! input:  winddirection
    ! output: windspeed,cone_dist

    ! strategy: always calc. 3 cone_distances for 3 windspeeds
    !     windspeed_low, windspeed_mid, windspeed_high
    ! repeat:
    !   -if conedist for windspeed_low < windspeed_mid < windspeed_high
    !       then step towards lower windspeed
    !   -if conedist for windspeed_low > windspeed_mid > windspeed_high
    !       then step towards higher windspeed
    !   -if conedist for windspeed_low > windspeed_mid < windspeed_high
    !       then the minimum must be around windspeed_mid,
    !       so set windspeedstep to a smaller value and start scanning
    !       around windspeed_mid. (windspeedstep = windspeedstep/2 or so)
    ! until: windspeedstep would become smaller than minimum_ws_step
    ! now give cone_dist at windspeed_mid as result of this function

    ! scanning boundaries:
    ! -never scan below v = min_windvelocity, or above v = max_windvelocity m/s

    type(inv_input_type) :: inv_input
    real(l_),intent(in)  :: winddirection
    real(l_),intent(out) :: windspeed
    real(l_),intent(out) :: cone_dist

    integer  :: stepdirection
    real(l_) :: windspeedstep
    real(l_) :: windspeed_low, windspeed_mid, windspeed_high
    real(l_) :: cone_dist_low, cone_dist_mid, cone_dist_high

    windspeedstep = inv_settings%initial_ws_step

    ! find wind speed to start with
    !
    ! when scanning over winddirection the found wind velocity
    ! varies only slowly. The last found wind velocity is
    ! therefore a good first guess.
    !
    ! BEWARE: for the polarimetric scheme taking the last found
    ! wind velocity as first guess is not a good idea! Since in this
    ! case the center around u,v=0,0 is a VERY flat area, converging
    ! to the proper windvelocity is not obvious. Escaping the flat area
    ! may depend on tiny numerical roundoff errors, and may succeed on
    ! one machine but break-down on another for the same program code.
    if (polarimetric_scheme_used) then

      ! use a constant since a previous speed should not be used
      ! in the polarimetric scheme
      windspeed_mid = inv_settings%ws_initial_guess
    else

      ! avoid dangerous flat area in MLE surface at lower speeds
      if (previous_windspeed .lt. lower_speed_threshold) then
         windspeed_mid = lower_speed_threshold

      ! avoid dangerous flat area in MLE surface at higher speeds
      ! (or even descending MLE for cmod5)
      elseif (previous_windspeed .gt. higher_speed_threshold) then
         windspeed_mid = higher_speed_threshold

      ! use the last used speed, which will probably 
      ! improve time needed for the search
      else
        windspeed_mid = previous_windspeed

        ! refine the wind speed step as well when this is not the first
        ! call to this routine for a specific wvc
        ! this makes the scanning more efficient
        if (previous_windspeed .ne. inv_settings%ws_initial_guess) then
          windspeedstep = windspeedstep * inv_settings%refiner_ws_step
        endif
      endif
    endif

    ! take two adjacent windspeeds
    windspeed_low  = windspeed_mid - windspeedstep
    windspeed_high = windspeed_mid + windspeedstep

    ! calculate the cone distances for these 3 windspeeds
    cone_dist_low  = calc_cone_distance(inv_input, &
                                        windspeed_low, winddirection)
    cone_dist_mid  = calc_cone_distance(inv_input, &
                                        windspeed_mid, winddirection)
    cone_dist_high = calc_cone_distance(inv_input, &
                                        windspeed_high,winddirection)

    ! implement the scanning logic
    windspeed_search: do

      stepdirection = 99

      ! determine step direction
      if (cone_dist_high .gt. cone_dist_mid .and. &
          cone_dist_mid  .gt. cone_dist_low) then
        stepdirection = -1    ! do a step towards lower windspeed

      elseif (cone_dist_high .lt. cone_dist_mid .and. &
              cone_dist_mid  .lt. cone_dist_low) then
        stepdirection =  1    ! do a step towards higher windspeed

      elseif (cone_dist_high .ge. cone_dist_mid .and. &
              cone_dist_mid  .le. cone_dist_low) then
        stepdirection =  0    ! a local minimum was found so refine step size
      endif

      ! if step size gets too small exit the search
      if (windspeedstep .lt. inv_settings%minimum_ws_step) stepdirection = 0

      select case (stepdirection)
      case (-1)

        ! check for v = min_windvelocity boundary
        if (windspeed_low - windspeedstep .lt. &
            min_windvelocity + inv_settings%minimum_ws_step) then

          ! refine windspeedstep
          windspeedstep = windspeedstep * inv_settings%refiner_ws_step

          ! windspeed_low should not get below min_windvelocity
          if (windspeedstep .gt. windspeed_low - min_windvelocity) then
            windspeedstep = windspeed_low - min_windvelocity
          endif

          ! it should still be possible to approach min_windvelocity
          if (windspeedstep .lt. inv_settings%minimum_ws_step) then
            windspeedstep = windspeed_low - min_windvelocity
          endif

          ! do a step towards lower windspeed, preserve symmetry for
          ! windspeed_low and windspeed_high w.r.t. windspeed_mid
          windspeed_high = windspeed_low + windspeedstep
          windspeed_mid  = windspeed_low
          windspeed_low  = windspeed_low - windspeedstep

          ! two new cone-distances have to be calculated, the mid one
          ! can be copied
          cone_dist_high = calc_cone_distance(inv_input, &
                                              windspeed_high, winddirection)
          cone_dist_mid  = cone_dist_low
          cone_dist_low  = calc_cone_distance(inv_input, &
                                              windspeed_low, winddirection)

        else

          ! do a step towards lower windspeed
          windspeed_high = windspeed_mid
          windspeed_mid  = windspeed_low
          windspeed_low  = windspeed_low - windspeedstep

          ! only one new cone-distance has to be calculated, the others
          ! can be copied
          cone_dist_high = cone_dist_mid
          cone_dist_mid  = cone_dist_low
          cone_dist_low  = calc_cone_distance(inv_input, &
                                              windspeed_low, winddirection) 
        endif
      case (1)

        ! check for v = max_windvelocity boundary
        if (windspeed_high + windspeedstep .gt. &
            max_windvelocity - inv_settings%minimum_ws_step) then

          ! refine windspeedstep
          windspeedstep = windspeedstep * inv_settings%refiner_ws_step

          ! windspeed_high should not get above max_windvelocity
          if (windspeedstep .gt. max_windvelocity - windspeed_high) then
            windspeedstep = max_windvelocity - windspeed_high
          endif

          ! it should still be possible to approach max_windvelocity
          if (windspeedstep .lt. inv_settings%minimum_ws_step) then
            windspeedstep = max_windvelocity - windspeed_high
          endif

          ! do a step towards higher windspeed, preserve symmetry for
          ! windspeed_low and windspeed_high w.r.t. windspeed_mid
          windspeed_low  = windspeed_high - windspeedstep
          windspeed_mid  = windspeed_high
          windspeed_high = windspeed_high + windspeedstep

          ! two new cone-distances have to be calculated, the mid one
          ! can be copied
          cone_dist_low  = calc_cone_distance(inv_input, &
                                              windspeed_low, winddirection)
          cone_dist_mid  = cone_dist_high
          cone_dist_high = calc_cone_distance(inv_input, &
                                              windspeed_high, winddirection)
        else

          ! do a step towards higher windspeed
          windspeed_low  = windspeed_mid
          windspeed_mid  = windspeed_high
          windspeed_high = windspeed_high + windspeedstep

          ! only one new cone-distance has to be calculated, the others
          ! can be copied
          cone_dist_low  = cone_dist_mid
          cone_dist_mid  = cone_dist_high
          cone_dist_high = calc_cone_distance(inv_input, &
                                              windspeed_high, winddirection)
        endif
      case (0)

        ! now cone_dist_mid is the minimum of the three
        ! so refine the stepsize
        windspeedstep = windspeedstep * inv_settings%refiner_ws_step

        ! check whether we have reached the final result
        if (windspeedstep .lt. inv_settings%minimum_ws_step) then

          ! no further refinement necessary, so break the loop
          ! and take windspeed_mid as result
          if (inv_settings%do_parabolic_minimum_search) then

            ! at the edge of the LUT it can occur that cone_dist_mid
            ! is not the minimum. low and mid, or mid and high might be
            ! equal, but have a numerical rounding difference causing
            ! the wrong ordering. This is not compatible with calling the
            ! get_parabolic_minimum routine.
            ! it can also occur at the edge of the LUT that
            ! windspeed low = mid, or mid=high, and determining a parabola
            ! from just 2 points is also not possible.
            ! so just give cone_dist_mid back as result
            if ((cone_dist_low .ge. cone_dist_mid)  .and. &
                (cone_dist_mid .le. cone_dist_high) .and. &
                (windspeed_low .lt. windspeed_mid)  .and. &
                (windspeed_mid .lt. windspeed_high)) then
              call get_parabolic_minimum(windspeed_low, cone_dist_low,  &
                                         windspeed_mid, cone_dist_mid,  &
                                         windspeed_high,cone_dist_high, &
                                         windspeed,     cone_dist)
            else
              windspeed = windspeed_mid
              cone_dist = cone_dist_mid
            endif
          else
            windspeed = windspeed_mid
            cone_dist = cone_dist_mid
          endif

          ! break the windspeed_search loop
          exit windspeed_search
        endif

        ! refine the search with the smaller stepsize
        ! the check for v = min_windvelocity and max_windvelocity boundaries
        ! is not necessary here since we reduced the windspeedstep
        ! and left windspeed_mid unchanged
        windspeed_low  = windspeed_mid - windspeedstep
        windspeed_high = windspeed_mid + windspeedstep

        ! two new cone-distances have to be calculated, the mid-one
        ! has not changed.
        cone_dist_low  = calc_cone_distance(inv_input, &
                                            windspeed_low, winddirection)
        cone_dist_high = calc_cone_distance(inv_input, &
                                            windspeed_high, winddirection)
      case default

        ! sometimes cone_dist_mid is the maximum of the three
        ! this should not happen, but it does sometimes (due to
        ! numerical erors).
        ! refining the stepsize when this happens will
        ! result in escaping the loop. Just hope that this local
        ! maximum (which is given as result now) is in a flat
        ! area (of a broad local minimum).
        windspeedstep = windspeedstep * inv_settings%refiner_ws_step
      end select

    enddo windspeed_search

    ! correct for the speed-dependent logarithmic term which was added in
    ! calc_cone_distance
    if (inv_settings%add_log_term_to_cost) then
      cone_dist = cone_dist + 2.0 * log(windspeed / max_windvelocity)
      cone_dist = abs(cone_dist)
    endif

    ! remember the resulting windspeed, so that we can use it
    ! for the next wind direction
    previous_windspeed = windspeed

  end subroutine find_minimum_cone_dist
  !---------------------------------

  !  #]
  subroutine get_parabolic_minimum(x0,y0,x1,y1,x2,y2,xmin,ymin)
  !  #[
    ! inputs
    real(l_) :: x0,y0,x1,y1,x2,y2

    ! outputs
    real(l_) :: xmin,ymin

    ! working variables
    real(l_) :: a,b,c

#ifndef USE_8BYTE_REALS 
    real(l_), parameter :: conversion     = 1.0e-14
    real(l_), parameter :: backconversion = 1.0/conversion
    logical  :: yvar_converted        = .false.
    logical  :: yvar_converted_2times = .false.
#endif

    ! check the sanity of the input numbers
    IF ((x0 .ge. x1) .or. (x1 .ge. x2)) THEN
      print *,'ERROR: inside subroutine get_parabolic_minimum'
      print *,'ERROR: inputnumbers have to obey: x0 < x1 < x2'
      print *,'ERROR: however, the routine was called using:'
      print *,'ERROR: x0,x1,x2 = ',x0,x1,x2
      CALL my_exit
    END IF

    IF ((y1 .gt. y0) .or. (y1 .gt. y2)) THEN
      print *,'ERROR: inside subroutine get_parabolic_minimum'
      print *,'ERROR: inputnumbers have to obey: y0 > y1 < y2'
      print *,'ERROR: however, the routine was called using:'
      print *,'ERROR: y0,y1,y2 = ',y0,y1,y2
      print *,'ERROR: y1 is not close to a minimum in this case !!!!'
      CALL my_exit
    END IF

    IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
      IF (((y1 .eq. y0) .and. (x1 .eq. x0)) .or. &
           ((y1 .eq. y2) .and. (x1 .eq. x2))      )THEN
        print *,'WARNING: inside subroutine get_parabolic_minimum'
        print *,'WARNING: y1 = y0, or y1 = y2'
        print *,'WARNING: this may lead to a false minimum !!!'
        print *,'WARNING: x0,x1,x2 = ',x0,x1,x2
        print *,'WARNING: y0,y1,y2 = ',y0,y1,y2
        IF ((y1 .eq. y0) .and. (y1 .eq. y2)) THEN
          print *,'WARNING: inside subroutine get_parabolic_minimum'
          print *,'WARNING: y0=y1=y2 so this is a flat region'
          print *,'WARNING: minimum position is not well determined,'
          print *,'WARNING: and there may not be a minimum at all here !!!'
          stop
        END IF
      END IF
    END IF

#ifndef USE_8BYTE_REALS 
    yvar_converted = .false.
    IF ((abs(y0) .gt. backconversion) .or. &
        (abs(y1) .gt. backconversion) .or. &
        (abs(y2) .gt. backconversion)       ) THEN
      ! this happens only for very low norm_factors, so usually
      ! only when no noise is simulated. Then the maximum MLE can be
      ! in the order of 1.0e21. For 4 byte reals the square can no
      ! longer be represented in the 4 bytes, so this trick
      ! becomes necessary:
      y0 = y0*conversion
      y1 = y1*conversion
      y2 = y2*conversion

      yvar_converted = .true.
    END IF

    yvar_converted_2times = .false.
    IF ((abs(y0) .gt. backconversion) .or. &
        (abs(y1) .gt. backconversion) .or. &
        (abs(y2) .gt. backconversion)       ) THEN
      ! this happened for the HH-LUT, for very small windspeeds
      y0 = y0*conversion
      y1 = y1*conversion
      y2 = y2*conversion
      yvar_converted_2times = .true.
    END IF
#endif

    ! for reasoning behind these formulas, see notebook of Jos, page 42.

    a = (((y2   -y0   )/(x2-x0))-((y1   -y0   )/(x1-x0)))/&
        (((x2**2-x0**2)/(x2-x0))-((x1**2-x0**2)/(x1-x0)))

    b = ((y1-y0)-a*(x1**2-x0**2))/(x1-x0)

    c = y0-a*x0**2-b*x0

    IF (a .ne. 0.0) THEN
      xmin = -1.0*b/(2.0*a)
      ymin = c - b**2/(4.0*a)
    ELSE 
      IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
        print *,'WARNING: y0=y1=y2 occurred inside get_parabolic_minimum'
        print *,'WARNING: therefore locating the minimum was impossible'
      END IF
      xmin = x1
      ymin = y1
    END IF

#ifndef USE_8BYTE_REALS 
    IF (yvar_converted)        ymin = ymin*backconversion
    IF (yvar_converted_2times) ymin = ymin*backconversion
#endif

    ! since a negative MLE should never happen, any negative value here
    ! is caused by the parabola going just below zero, even if the
    ! 3 points it is based on are positive. Therefore just keep the
    ! position xmin, but set the ymin value to zero to correct for
    ! this (numeric) artefact.
    IF (ymin .lt. 0.0) ymin = 0.0

    return

  end subroutine get_parabolic_minimum
  !---------------------------------
  !  #]
  function calc_cone_distance(inv_input, windspeed,winddirection) &
       result(cone_dist)
  !  #[
    ! this routine consumes most of the calculating time of the whole
    ! inversion module, so improvements in performance should be made
    ! inside it, or by calling it less frequent
    ! (last time I checked, I had 1136 calls per inverted WVC)

    TYPE(inv_input_type)  :: inv_input
    real(l_) :: windspeed
    real(l_) :: winddirection
    real(l_) :: cone_dist

    real(l_) :: sigma_expected, sigma_measured, sigma_difference
    real(l_) :: sigma_0_vh_expected
    real(l_) :: norm_factor, rel_wind_dir

    integer  :: j     ! counter

    cone_dist = 0.0 ! also known as MLE (max. likelihood estimator)

    do j=1,inv_input%nr_of_views
      ! I wish to scan v_x and v_y in the normal x,y coordinate system
      ! but the calc_sigma0 function wants the windspeed with respect
      ! to the antenna view direction, so a conversion has to be done:
      rel_wind_dir     = winddirection - inv_input%antenna_dir(j)
      sigma_expected   = calc_sigma0(windspeed,rel_wind_dir,&
                                     inv_input%theta_angle(j),&
                                     inv_input%pol(j))
      if ( (inv_input%pol(j) .eq. c_po_pol) .or. &
           (inv_input%pol(j) .eq. ku_po_pol)      ) then
         sigma_measured   = inv_input%doppler_shift(j)
      else
         sigma_measured   = inv_input%sigma_0(j)
      end if
      sigma_difference = sigma_expected - sigma_measured

      if (abs(sigma_difference) .gt. 1.0e15) then
         ! this may happen for certain look-up-tables (especially HH)
         ! for windspeeds close to zero, for which the sigma values in the
         ! LUT can go above 1.0e32. Since sigma_difference is squared below
         ! and divided by norm_factor_meas(j) which has a value in the 
         ! order of 1e-5, this gives an Arithmetic Exception (overflow).
         ! (max. allowed value for 4byte reals is 3.4e38)
         ! the next line fixes this.
         sigma_difference = 1.0e15
         ! (the wanted solutions are never close to these large differences,
         !  so there is no danger to the inversion algoritm.)
      endif
      
      ! BEWARE: for the polarimetric scheme an additional weight may be used
      ! put this weight in the sigma_difference for efficiency reasons
      if (polarimetric_scheme_used) then
         if ( (inv_input%pol(j) .eq. c_po_pol) .or. &
              (inv_input%pol(j) .eq. ku_po_pol)      ) then
            sigma_difference = sigma_difference / &
                               sqrt(inv_settings%polarimetric_weight)
         endif
      endif

      ! ditto for the doppler_weight
      if (doppler_scheme_used) then
         if ( (inv_input%pol(j) .eq. c_vv_dop) .or. &
              (inv_input%pol(j) .eq. c_hh_dop)      ) then
            sigma_difference = sigma_difference / &
                               sqrt(inv_settings%doppler_weight)
         endif
      end if

      if (inv_settings%no_normalisation) then
         cone_dist = cone_dist + sigma_difference**2
      else
         ! for now, this else clause has not yet been adapted
         ! to using the Doppler option. JK.
         if (inv_settings%use_meas_sigma0_for_normfactor) then
            cone_dist = cone_dist + sigma_difference**2/norm_factor_meas(j)
         else 
            if ( (polarimetric_scheme_used) .and. &
                 ( (inv_input%pol(j) .eq. c_po_pol ) .or. &
                   (inv_input%pol(j) .eq. ku_po_pol)       ) ) then
               ! special case for polarimetric measurement
               if (inv_input%pol(j) .eq. c_po_pol) then
                  sigma_0_vh_expected = calc_sigma0(windspeed,rel_wind_dir,&
                                                 inv_input%theta_angle(j),c_vh_pol)
               endif
               if (inv_input%pol(j) .eq. ku_po_pol) then
                  sigma_0_vh_expected = calc_sigma0(windspeed,rel_wind_dir,&
                                                 inv_input%theta_angle(j),ku_vh_pol)
               endif
               norm_factor = calc_var_s0(sigma_0_vh_expected,&
                             inv_input%kp_a(j),inv_input%kp_b(j),inv_input%kp_c(j))
               cone_dist = cone_dist + sigma_difference**2/norm_factor
            else
               norm_factor = calc_var_s0(sigma_expected,&
                    inv_input%kp_a(j),inv_input%kp_b(j),inv_input%kp_c(j))
               cone_dist = cone_dist + sigma_difference**2/norm_factor
            endif
         endif
      endif

    enddo ! loop over all views

    ! add an extra term -2 * natural log(windspeed) to prevent retrieval
    ! failures in case of negative backscatter values
    if (inv_settings%add_log_term_to_cost) then
       cone_dist = cone_dist - 2.0 * log(windspeed / max_windvelocity)
    endif

  end function calc_cone_distance
  !---------------------------------
  !  #]
  function calc_dist_to_cone_center(inv_input,inv_output) &
       result(dist_to_cone_center)
  !  #[
    ! parameters
    TYPE(inv_input_type), intent(in)  :: inv_input
    TYPE(inv_output_type), intent(in) :: inv_output
    ! the result
    real(l_) :: dist_to_cone_center
    ! local variables
    real(l_), dimension(1:max_nr_of_views) :: avg_sigma0
    real(l_) :: rel_wind_dir, windspeed, sigma_difference, winddirection
    integer :: i,j

    ! safety check
    if (.not. inv_settings%no_normalisation) then
       print *,"ERROR: calc_dist_to_cone_center() was only ment to be used"
       print *,"ERROR: with ERS and ASCAT data, with the setting no_normalisation"
       stop
    endif

    ! safety check, this should never happen
    IF (inv_output%nr_of_windsolutions .lt. 1) THEN
       print *,"Warning: calc_dist_to_cone_center() failed because there were"
       print *,"no valid solutions to use"
       dist_to_cone_center = missing_indicator_real
    END IF

    DO i=1,inv_input%nr_of_views
       avg_sigma0(i) = 0.0

       DO j=1,nr_of_winddir_steps
          ! calculate the current (relative) wind direction
          winddirection = winddirstep * j
          rel_wind_dir  = winddirection - inv_input%antenna_dir(i)
          ! take the current windspeed for this direction
          ! use the speed of the 1st rank solution to do the determination
          ! of the conecenter
          windspeed = inv_output%foundwindspeed(1)

          avg_sigma0(i) = avg_sigma0(i) + &
                          calc_sigma0(windspeed,rel_wind_dir,&
                                      inv_input%theta_angle(i),&
                                      inv_input%pol(i))
       END DO ! loop over winddirection
       avg_sigma0(i) = avg_sigma0(i)/nr_of_winddir_steps
    END DO ! loop over views

    ! then calc. the distance between the cone center and the triplet
    dist_to_cone_center = 0.0
    DO i=1,inv_input%nr_of_views
       sigma_difference = inv_input%sigma_0(i) - avg_sigma0(i)
       IF (abs(sigma_difference) .gt. 1.0e15) sigma_difference = 1.0e15
       dist_to_cone_center = dist_to_cone_center + sigma_difference**2
    END DO

  end function calc_dist_to_cone_center
    !  #]
  subroutine convert_sigma_to_zspace(inv_input)
  !  #[
    ! convert sigma_0() in the inv_input struct to zspace, 
    ! and check to make sure it is only done once !
    ! for a definition of zspace, see thesis Ad Stoffelen, p.III-3

    TYPE(inv_input_type) :: inv_input
    integer :: i 

    IF (.NOT. inv_input%converted_to_zspace) THEN
       inv_input%converted_to_zspace = .true.
       DO i = 1,inv_input%nr_of_views
          IF (inv_input%sigma_0(i) .lt. 0.0) then
             inv_input%sigma_0(i) = -1.0 * (abs(inv_input%sigma_0(i)))**Zfactor
          ELSE
             inv_input%sigma_0(i) = inv_input%sigma_0(i)**Zfactor
          END IF
       END DO
    END IF

  end subroutine convert_sigma_to_zspace
  !---------------------------------
  !  #]
  function calc_var_s0(sigma0, ka, kb, kc)  result(SD_sq)
  !  #[
    real(l_) :: sigma0
    real(l_) :: ka, kb, kc
    real(l_) :: SD_sq

    ! GMF is usually zero !
#define GMF 0
    SD_sq = (ka * (1.+GMF) - 1.) * sigma0**2 + kb * sigma0 + kc
#undef GMF
    ! REMARK: for seawinds kc is provided in dB values in the BUFR product.
    ! This should be converted to normal values before running inversion,
    ! since inversion uses normal values for sigma0 !!!

    ! prevent dividing by zero !
    ! the lowest possible numbers for sigma0 (c-vv) are in the order of 1e-18
    ! so after adding 1% noise this is 1e-20, so squared 1e-40 is the 
    ! lowest possible valid result. The lowest normal value, however, is
    ! about 1e-8 or 1e-9, the lower ones are just fixes so that the 
    ! gmf does not go to zero, and have no physical meaning.
    ! So 1% of 1e-9 is 1e-11, and squared this gives 1e-22.
    ! However 1e-25 works fine in practice.
    IF (SD_sq .lt. 1.0e-25)  then

       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          IF (ka .ne. 1.0) THEN
             CALL print_message(WARN_LOW_VAR_S0)
          END IF
          ! for ka = 0 (this can happen for tests of the system without noise)
          ! all measurements should get the same weight, so setting
          ! SD_sq to a constant value 1.0e-25 is OK.
       END IF

       IF (SD_sq .lt. 0.0) THEN
         print *,'ALARM: SD_sq < 0 this should never happen, since this gives'
         print *,'negative weighting to a sigma_0 !!!!!!'
         print *,'sigma0, ka, kb, kc = ',sigma0, ka, kb, kc
         print *,' ***** ..... quitting ..... *****'
         CALL my_exit
       END IF
       SD_sq= 1.0e-25
    ENDIF

  return
  end function calc_var_s0
  !---------------------------------
  !  #]
  function get_dynamic_range(pol,v,theta) result(max_sigma0)
  !  #[
    ! finds, for a given speed v, inc.angle theta, and polarisation type pol,
    ! the maximum sigma0 that occurs in the LUT, and reports
    ! this as the dynamic range.
    ! (needed in the kp calculation)

    integer  :: pol, i
    real(l_) :: v, theta, phi
    real(l_) :: max_sigma0, sigma0
    
    max_sigma0 = 0.0
    DO i=1,36
       phi=10.*i
       sigma0 = calc_sigma0(v,phi,theta,pol) 
       IF (abs(sigma0) .gt. max_sigma0) max_sigma0 = abs(sigma0)
    END DO

  end function get_dynamic_range
  !---------------------------------
  !  #]
  function get_GMF_version_used(pol) result(GMF_version)
  !  #[
    integer :: pol
    integer :: GMF_version

    ! GMF_version is a global variable in this module
    GMF_version = -1
    IF (pol .eq. c_vv_pol) THEN
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod4_to_construct_LUT) THEN
          GMF_version = cmod4_used_as_GMF
       END IF
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod5_to_construct_LUT) THEN
          GMF_version = cmod5_used_as_GMF
       END IF
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod5_5_to_construct_LUT) THEN
          GMF_version = cmod5_5_used_as_GMF
       END IF
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod5_n_to_construct_LUT) THEN
          GMF_version = cmod5_n_used_as_GMF
       END IF
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod6_to_construct_LUT) THEN
          GMF_version = cmod6_used_as_GMF
       END IF
       IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
            use_cmod7_to_construct_LUT) THEN
          GMF_version = cmod7_used_as_GMF
       END IF
    END IF

  end function get_GMF_version_used
  !  #]
  function get_closest_solution(inv_output,ref_speed,ref_dir) result(closest)
  !  #[
    ! finds the local minimum (i.e. non-MSS) solution that is closest to the
    ! given reference wind speed and direction (e.g. a NWP wind)
    ! even if MSS is used, still the closest local minimum will be reported
    ! if the reference wind is missing, the first rank will be returned
    type(inv_output_type) :: inv_output
    real    :: ref_speed
    real    :: ref_dir
    integer :: closest

    real    :: sqr_vec_len,wind_u,wind_v,ref_u,ref_v
    integer :: nsol,isol,iisol

    if (inv_output%nr_of_windsolutions_n .gt. 0) then ! MSS case, use local minima
      nsol = inv_output%nr_of_windsolutions_n
      closest = inv_output%indices_solutions_n(1)
    else                                              ! normal case
      nsol = inv_output%nr_of_windsolutions
      closest = 1
    endif

    if (ref_speed .ne. missing_indicator_real .and. ref_dir .ne. missing_indicator_real) then
      sqr_vec_len = 999999.9
      ref_u = speeddir_to_u(ref_speed,ref_dir)
      ref_v = speeddir_to_v(ref_speed,ref_dir)
      do isol = 1,nsol
        if (inv_output%nr_of_windsolutions_n .gt. 0) then ! MSS case
          iisol = inv_output%indices_solutions_n(isol)
        else                                              ! normal case
          iisol = isol
        endif
        wind_u = speeddir_to_u(inv_output%foundwindspeed(iisol),inv_output%foundwinddir(iisol))
        wind_v = speeddir_to_v(inv_output%foundwindspeed(iisol),inv_output%foundwinddir(iisol))
        if (((wind_u-ref_u)**2 + (wind_v-ref_v)**2) .lt. sqr_vec_len) then
          closest = iisol
          sqr_vec_len = (wind_u-ref_u)**2 + (wind_v-ref_v)**2
        endif
      enddo
    endif

  end function get_closest_solution
  !  #]
  function calc_sigma0(v,phi,theta,pol) result(sigma0)
  !  #[

    !
    !
    ! arguments
    real(l_) :: v        ! wind speed 
    real(l_) :: phi      ! azimuth 
    real(l_) :: theta    ! incidence angle
    integer  :: pol      ! polarisation type  
    real(l_) :: sigma0   ! output
    !local
    real(l_) :: temp_phi

    ! used for checking whether LUT has only positive numbers
    ! (in case of no polarimetry and no Doppler) and for checking whether the
    ! LUT is not centered around +1,-1 (in case of polarimetry)
    logical             :: polarimetric_LUT
    logical             :: doppler_LUT

    logical :: succes
    character(len=256)  :: env_filename

    ! used to scan the filename for cmod4/cmod5
    integer             :: last_char_of_path
    character(len=256)  :: file_without_path
    logical             :: filename_contains_cmod4
    logical             :: filename_contains_cmod5
    logical             :: filename_contains_cmod5_5
    logical             :: filename_contains_cmod5_n
    logical             :: filename_contains_cmod6
    logical             :: filename_contains_cmod7
    integer             :: status

    if (overall_first_call) then
       nullify(c_vv_table,  c_hh_table,  c_pol_table, &
               ku_vv_table, ku_hh_table, ku_pol_table, &
               c_vv_dop_table, c_hh_dop_table)
       overall_first_call = .false.
    end if
    ! these tables are allocated when first used to this dimension:
    ! dimension(0:LUT_nr_windspeed_steps-1,&
    !           0:LUT_nr_winddir_steps-1,  &
    !           0:LUT_nr_theta_steps-1      )

    succes = .true.
    SELECT CASE (pol)
       CASE(c_vv_pol,&  ! is identical to cmod4.dat or cmod5.dat
            c_vh_pol)   ! is identical to cmod4/5.dat attenuated by -15dB
          IF (first_call_c_vv) THEN
             !  #[ load or compose c_vv LUT
             allocate(c_vv_table(0:LUT_nr_windspeed_steps-1, &
                                 0:LUT_nr_winddir_steps-1,   &
                                 0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for c_vv_table'
                call my_exit()
             end if
             filename_contains_cmod4 = .false.
             filename_contains_cmod5 = .false.
             filename_contains_cmod5_5 = .false.
             filename_contains_cmod5_n = .false.
             filename_contains_cmod6 = .false.
             filename_contains_cmod7 = .false.
             CALL getenv("LUT_FILENAME_C_VV",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_c_vv = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_C_VV = ',&
                        trim(env_filename)
                endif
             END IF

             ! try to derive from the filename, 
             ! which cmod version was used
             last_char_of_path = index(filename_c_vv,"/",back=.true.)
             file_without_path = filename_c_vv(last_char_of_path+1:)
             IF (file_without_path(1:5) .eq. "cmod4") THEN
                filename_contains_cmod4 = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                     use_cmod4_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod4 and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF
             IF (file_without_path(1:5) .eq. "cmod5" .AND. &
             &   file_without_path(6:7) .ne. "_5" .AND. file_without_path(6:7) .ne. "_n") THEN
                filename_contains_cmod5 = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                    use_cmod5_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod5 and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF
             IF (file_without_path(1:7) .eq. "cmod5_5") THEN
                filename_contains_cmod5_5 = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                    use_cmod5_5_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod5_5 and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF
             IF (file_without_path(1:7) .eq. "cmod5_n") THEN
                filename_contains_cmod5_n = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                    use_cmod5_n_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod5_n and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF
             IF (file_without_path(1:5) .eq. "cmod6") THEN
                filename_contains_cmod6 = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                    use_cmod6_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod6 and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF
             IF (file_without_path(1:5) .eq. "cmod7") THEN
                filename_contains_cmod7 = .true.
                IF (inv_settings%use_which_fn_to_construct_LUT .ne. &
                    use_cmod7_to_construct_LUT) THEN
                   print *,"ERROR: using a LUT file named cmod7 and"
                   print *,"ERROR: inv_settings%use_which_fn_to_construct_LUT set to ", &
                     inv_settings%use_which_fn_to_construct_LUT
                   print *,"ERROR: this is very confusing, and could easily lead to mistakes"
                   print *,"ERROR: please change the inv_settings or change the LUT file name"
                   stop
                END IF
             END IF

             first_call_c_vv  = .false.
             polarimetric_LUT = .false.
             doppler_LUT      = .false.
             CALL read_LUT(c_vv_table, filename_c_vv,&
                           polarimetric_LUT, doppler_LUT, succes)

             GMF_version_used = -1
             IF (.not. succes) THEN 
                IF ( (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod4_to_construct_LUT) .or. &
                     (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod5_to_construct_LUT) .or. &
                     (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod5_5_to_construct_LUT) .or. &
                     (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod5_n_to_construct_LUT) .or. &
                     (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod6_to_construct_LUT) .or. &
                     (inv_settings%use_which_fn_to_construct_LUT .eq. &
                      use_cmod7_to_construct_LUT) ) THEN
                   CALL create_LUT_C_VV(c_vv_table, filename_c_vv)
                   succes = .true.
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod4_to_construct_LUT) THEN
                      GMF_version_used = cmod4_used_as_GMF
                   END IF
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod5_to_construct_LUT) THEN
                      GMF_version_used = cmod5_used_as_GMF
                   END IF
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod5_5_to_construct_LUT) THEN
                      GMF_version_used = cmod5_5_used_as_GMF
                   END IF
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod5_n_to_construct_LUT) THEN
                      GMF_version_used = cmod5_n_used_as_GMF
                   END IF
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod6_to_construct_LUT) THEN
                      GMF_version_used = cmod6_used_as_GMF
                   END IF
                   IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                       use_cmod7_to_construct_LUT) THEN
                      GMF_version_used = cmod7_used_as_GMF
                   END IF
                ELSE
                   print *,"ERROR: could not find the proper Look Up Table for inversion"
                   print *,"(tried to read file: "//trim(filename_c_vv)//")"
                   print *,"ERROR: and no function to construct one was defined, i.e."
                   print *,"filename did not contain the string cmod4,cmod5,cmod5_5,cmod5_n,"
                   print *,"cmod6 or cmod7."
                   stop
                END IF
             ELSE
                ! try to derive from the filename, which cmod version was used
                ! if this fails, use the value from inv_settings
                IF (filename_contains_cmod4) THEN
                   GMF_version_used = cmod4_used_as_GMF
                ELSE IF (filename_contains_cmod5) THEN
                   GMF_version_used = cmod5_used_as_GMF
                ELSE IF (filename_contains_cmod5_5) THEN
                   GMF_version_used = cmod5_5_used_as_GMF
                ELSE IF (filename_contains_cmod5_n) THEN
                   GMF_version_used = cmod5_n_used_as_GMF
                ELSE IF (filename_contains_cmod6) THEN
                   GMF_version_used = cmod6_used_as_GMF
                ELSE IF (filename_contains_cmod7) THEN
                   GMF_version_used = cmod7_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod4_to_construct_LUT) THEN
                   GMF_version_used = cmod4_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod5_to_construct_LUT) THEN
                   GMF_version_used = cmod5_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod5_5_to_construct_LUT) THEN
                   GMF_version_used = cmod5_5_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod5_n_to_construct_LUT) THEN
                   GMF_version_used = cmod5_n_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod6_to_construct_LUT) THEN
                   GMF_version_used = cmod6_used_as_GMF
                ELSE IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                         use_cmod7_to_construct_LUT) THEN
                   GMF_version_used = cmod7_used_as_GMF
                END IF
             END IF
             call cross_check_for_identical_LUTs(c_vv_pol)
             !  #]
          END IF ! end of check for first_call_c_vv
       CASE(c_hh_pol)
          IF (first_call_c_hh) THEN
             !  #[ load c_hh LUT
             allocate(c_hh_table(0:LUT_nr_windspeed_steps-1, &
                                 0:LUT_nr_winddir_steps-1,   &
                                 0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for c_hh_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_C_HH",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_c_hh = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_C_HH = ',&
                        trim(env_filename)
                endif
             END IF
             first_call_c_hh  = .false.
             polarimetric_LUT = .false.
             doppler_LUT      = .false.
             CALL read_LUT(c_hh_table, filename_c_hh,&
                           polarimetric_LUT, doppler_LUT, succes)
             call cross_check_for_identical_LUTs(c_hh_pol)
             !  #]
          END IF ! end of check for first_call_c_hh
       CASE(c_po_pol)
          IF (first_call_c_pol) THEN
             !  #[ load c_pol LUT
             allocate(c_pol_table(0:LUT_nr_windspeed_steps-1, &
                                  0:LUT_nr_winddir_steps-1,   &
                                  0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for c_pol_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_C_POL",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_c_pol = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_C_POL = ',&
                        trim(env_filename)
                endif
             END IF
             first_call_c_pol = .false.
             polarimetric_LUT = .true.
             doppler_LUT      = .false.
             CALL read_LUT(c_pol_table, filename_c_pol,&
                           polarimetric_LUT, doppler_LUT, succes)
             call cross_check_for_identical_LUTs(c_po_pol)
             !  #]
          END IF ! end of check for first_call_c_pol
       CASE(ku_vv_pol,&
            ku_vh_pol) ! is identical to ku_vv attenuated by -15dB
          IF (first_call_ku_vv) THEN
             !  #[ load ku_vv LUT
             allocate(ku_vv_table(0:LUT_nr_windspeed_steps-1, &
                                  0:LUT_nr_winddir_steps-1,   &
                                  0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for ku_vv_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_KU_VV",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_ku_vv = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_KU_VV = ',&
                           trim(env_filename)
                endif
             END IF
             first_call_ku_vv = .false.
             polarimetric_LUT = .false.
             doppler_LUT      = .false.
             CALL read_LUT(ku_vv_table, filename_ku_vv,&
                           polarimetric_LUT, doppler_LUT, succes)
             call cross_check_for_identical_LUTs(ku_vv_pol)
             !  #]
          END IF ! end of check for first_call_ku_vv
       CASE(ku_hh_pol)
          IF (first_call_ku_hh) THEN
             !  #[ load ku_hh LUT
             allocate(ku_hh_table(0:LUT_nr_windspeed_steps-1, &
                                  0:LUT_nr_winddir_steps-1,   &
                                  0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for ku_hh_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_KU_HH",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_ku_hh = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_KU_HH = ',&
                          trim(env_filename)
                endif
             END IF
             first_call_ku_hh = .false.
             polarimetric_LUT = .false.
             doppler_LUT      = .false.
             CALL read_LUT(ku_hh_table, filename_ku_hh,&
                           polarimetric_LUT, doppler_LUT, succes)
             call cross_check_for_identical_LUTs(ku_hh_pol)
             !  #]
          END IF ! end of check for first_call_ku_hh
       CASE(ku_po_pol)
          IF (first_call_ku_pol) THEN
             !  #[ load ku_pol LUT
             allocate(ku_pol_table(0:LUT_nr_windspeed_steps-1, &
                                  0:LUT_nr_winddir_steps-1,   &
                                  0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for ku_pol_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_KU_POL",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_ku_pol = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_KU_POL = ',&
                           trim(env_filename)
                endif
             END IF
             first_call_ku_pol = .false.
             polarimetric_LUT = .true.
             doppler_LUT      = .false.
             CALL read_LUT(ku_pol_table, filename_ku_pol,&
                           polarimetric_LUT, doppler_LUT, succes)
             call cross_check_for_identical_LUTs(ku_po_pol)
             !  #]
          END IF ! end of check for first_call_ku_pol
       CASE(c_vv_dop)
          IF (first_call_c_vv_dop) THEN
             !  #[ load or compose c_vv_dop LUT
             allocate(c_vv_dop_table(0:LUT_nr_windspeed_steps-1, &
                                     0:LUT_nr_winddir_steps-1,   &
                                     0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for c_vv_dop_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_C_VV_DOP",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_c_vv_dop = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_C_VV_DOP = ',&
                        trim(env_filename)
                endif
             END IF
             first_call_c_vv_dop = .false.
             polarimetric_LUT    = .false.
             doppler_LUT         = .true.
             CALL read_LUT(c_vv_dop_table, filename_c_vv_dop,&
                           polarimetric_LUT, doppler_LUT, succes)

             IF (.not. succes) THEN
                CALL create_LUT_C_VV_DOP(c_vv_dop_table, filename_c_vv_dop)
                succes = .true.
             END IF
             call cross_check_for_identical_LUTs(c_vv_dop)
             !  #]
          END IF ! end of check for first_call_c_vv_dop
       CASE(c_hh_dop)
          IF (first_call_c_hh_dop) THEN
             !  #[ load or compose c_hh_dop LUT
             allocate(c_hh_dop_table(0:LUT_nr_windspeed_steps-1, &
                                     0:LUT_nr_winddir_steps-1,   &
                                     0:LUT_nr_theta_steps-1), &
                      stat=status)
             if (status .ne. 0) then
                print *,'Could not allocate memory for c_hh_dop_table'
                call my_exit()
             end if
             CALL getenv("LUT_FILENAME_C_HH_DOP",env_filename)
             IF (trim(env_filename) .ne. "") THEN
                filename_c_hh_dop = trim(env_filename)
                if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                   print *,'USING ENV VARIABLE: LUT_FILENAME_C_HH_DOP = ',&
                        trim(env_filename)
                endif
             END IF
             first_call_c_hh_dop = .false.
             polarimetric_LUT    = .false.
             doppler_LUT         = .true.
             CALL read_LUT(c_hh_dop_table, filename_c_hh_dop, &
                           polarimetric_LUT, doppler_LUT, succes)

             IF (.not. succes) THEN
                CALL create_LUT_C_hh_DOP(c_HH_dop_table, filename_c_HH_dop)
                succes = .true.
             END IF

             call cross_check_for_identical_LUTs(c_hh_dop)
             !  #]
          END IF ! end of check for first_call_c_hh_dop
       CASE DEFAULT
          print *,'ERROR: polarisation type: ',pol,' is undefined'
          print *,'ERROR: *** ... quitting ... ***'
          CALL my_exit
    END SELECT

    IF (.not. succes) THEN
       print *,'ERROR: reading or composing the look-up-table failed'
       print *,'ERROR: in function calc_sigma0()'
       print *,'ERROR: for view: v,phi,theta,pol = ',v,phi,theta,pol
       print *,'ERROR: *** ... quitting ... ***'
       CALL my_exit
    END IF

    ! ensure that phi (which may be between -720 and 720 degrees)
    ! is in the range 0:360 degrees
    if (phi .gt. 360.0) phi = phi - 360.0
    if (phi .lt.   0.0) phi = phi + 360.0
    if (phi .lt.   0.0) phi = phi + 360.0

    ! phi only spans 0:180 degrees in the table since there is a
    ! symmetry: sigma(v,phi,theta) =  sigma(v,360-phi,theta) 
    !       or: sigma(v,phi,theta) = -sigma(v,360-phi,theta) (polarimetric)

    ! implement symmetry for vv and hh case: 
    !      sigma(v,phi,theta) =  sigma(v,360-phi,theta)
    ! the old inversion software started the phi loop at -180 deg and not
    ! at 0 deg, which resulted in some (small) rounding problems towards
    ! a different integer. To enforce compatibility the 0.00001 was added.
    ! (rounding is performed inside the interpolate subroutine)
    temp_phi = phi 
    
    if (temp_phi .gt. 180.) temp_phi = 360.0 - phi

! the choice between 1d,2d and 3d interpolation should not be done at
! runtime, because this is a wast of time. You will always use just
! one setting at a time. Therefore I replaced the if-thens and the int3d
! switch by a preprocessor function (macro).
! INTERPOLATE(table, v, phi, theta) is a CPP function

    SELECT CASE (pol)
    CASE(c_vv_pol) 
       sigma0 = INTERPOLATE(c_vv_table, v,temp_phi,theta)
    CASE(c_hh_pol) 
       sigma0 = INTERPOLATE(c_hh_table, v,temp_phi,theta) 
    CASE(c_vh_pol) 
       sigma0 = INTERPOLATE(c_vv_table, v,temp_phi,theta)*min15dB_factor
    CASE(c_po_pol)
       sigma0 = INTERPOLATE(c_pol_table, v,temp_phi,theta)
       ! sigma sometimes has to be set negative for polarimetric since:
       !     sigma(v,phi,theta) = -sigma(v,360-phi,theta) (polarimetric)
       IF (phi .gt. 180) sigma0 = -1.0*sigma0
    CASE(ku_vv_pol) 
       sigma0 = INTERPOLATE(ku_vv_table, v,temp_phi,theta)
    CASE(ku_hh_pol) 
       sigma0 = INTERPOLATE(ku_hh_table, v,temp_phi,theta) 
    CASE(ku_vh_pol) 
       sigma0 = INTERPOLATE(ku_vv_table, v,temp_phi,theta)*min15dB_factor
    CASE(ku_po_pol)
       sigma0 = INTERPOLATE(ku_pol_table, v,temp_phi,theta)
       ! sigma sometimes has to be set negative for polarimetric since:
       !     sigma(v,phi,theta) = -sigma(v,360-phi,theta) (polarimetric)
       IF (phi .gt. 180) sigma0 = -1.0*sigma0
    CASE(c_vv_dop)
       !print *,'c_vv_dop case'
       ! these is no real sigma0 backscatter in this case, but we use the
       ! same variable to give back the Doppler shift to keep things simple
       sigma0 = INTERPOLATE(c_vv_dop_table, v,temp_phi,theta)
    CASE(c_hh_dop)
       print *,'c_hh_dop case'
       ! these is no real sigma0 backscatter in this case, but we use the
       ! same variable to give back the Doppler shift to keep things simple
       sigma0 = INTERPOLATE(c_hh_dop_table, v,temp_phi,theta)
    CASE DEFAULT
      print *,'ERROR: polarisation type: ',pol,' is undefined'
      print *,'ERROR: *** ... quitting ... ***'
      CALL my_exit
    END SELECT

    IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
       ! test
       IF ((sigma0 .le. 0.0     ) .and. &
            (pol    .ne. c_po_pol) .and. &
            (pol    .ne. ku_po_pol)      ) THEN
          print *,'calc_sigma0 WARNING: sigma0 <= 0.0 !!!'
          print *,'sigma0 = ',sigma0
       END IF
    END IF

    return
  end function calc_sigma0
  !---------------------------------------------------
  !  #]
  !  #[ Interpolate functions
#ifdef INTERPOLATE1D
  function interpolate1d(table,v,phi,theta) result(sigma0)
  !  #[
    ! do a 1d interpolation of the 3 dimensional table, 
    ! only in theta direction. The other 2 dimensions
    ! are rounded towards the closest value available
    ! in the table.

    real(r4_), dimension(0:LUT_nr_windspeed_steps-1,&
                         0:LUT_nr_winddir_steps-1,  &
                         0:LUT_nr_theta_steps-1       ) :: table
    real(l_) :: v, phi, theta
    real(l_) :: sigma0

    real(l_) :: r,sigma111,sigma112
    integer  :: i,j,k

! BEWARE: since in 1 direction interpolation is done, 
! k should not be allowed to get a value of 
! LUT_nr_..._steps-1 !!!!! LUT_nr_..._steps-2 should be the limit !

    ! do not interpolate v and phi in the table
    i = nint((v - LUT_min_windspeed)*one_over_LUT_windspeed_step)
    j = nint((phi - LUT_min_winddir)*one_over_LUT_winddir_step)

    ! interpolate the table using theta
    ! int rounds -0.001 towards 0, while floor rounds it towards -1 !!!!
    r = (theta - LUT_min_theta)*one_over_LUT_theta_step
    k = floor(r)

    ! k does not need the 0.5 stepvalue addition since the residue will be 
    ! used anyway to interpolate. For i and j it is important, since in these 
    ! directions no interpolation is done, so the roundoff should be correct.
    ! this also explains why the max values of i and j may be up to the edge
    ! of the table, while for k it has to remain 1 below the edge index.
    ! (nint essentially adds one half and then does a floor()!)

    IF (i .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WS)
          print *,'i = nint( (v   - LUT_min_windspeed)*one_over_LUT_windspeed_step )'
          print *,'LUT_min_windspeed = ',LUT_min_windspeed
          print *,'one_over_LUT_windspeed_step = ',one_over_LUT_windspeed_step
          print *,'v=',v
       END IF
       i = 0
    ELSE
       IF (i .gt. LUT_nr_windspeed_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             CALL print_message(WARN_LUT_HIGH_WS)
          END IF
          i = LUT_nr_windspeed_steps-1
       END IF
    END IF

    IF (j .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WD)
       END IF
       j = 0
    ELSE
       IF (j .gt. LUT_nr_winddir_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             CALL print_message(WARN_LUT_HIGH_WD)
          END IF
          j = LUT_nr_winddir_steps-1
       END IF
    END IF

    IF (k .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_TH)
       END IF
       k = 0
       r = 0.0
    ELSE
       IF (k .ge. LUT_nr_theta_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (k .ge. LUT_nr_theta_steps) CALL print_message(WARN_LUT_HIGH_TH)
          END IF
          k = LUT_nr_theta_steps-2
       END IF
       ! calculate the residue, needed for the interpolation in theta direction
       r = r - real(k)
    END IF

! This exception can only occur at the table boundary.
! The negative value causes an extrapolation out of the table
! and in some cases this may result in a negative sigma0.
! The simulation program could then be trapped inside the endless loop 
! that calculates the instrument noise (because it will wait untill
! sigma0+noise gives a positive number which will never happen
! if sigma0 is negative !!!!)
!IF (r .lt. 0.0) r = 0.0
! (to increase the programs speed, this exception is interwoven
!  in the IF-THEN construct above.)

    sigma111 = table(i,j,k  )
    sigma112 = table(i,j,k+1)
    sigma0 = sigma111 + r*(sigma112-sigma111)

  end function interpolate1d
  !---------------------------------------------------
  !  #]
#endif
#ifdef INTERPOLATE2D
  function interpolate2d(table,v,phi,theta) result(sigma0)
  !  #[
    ! do a 2d interpolation of the 3 dimensional table, 
    ! only in theta and phi direction. 
    ! v is rounded towards the closest value available
    ! in the table.

    real(r4_), dimension(0:LUT_nr_windspeed_steps-1,&
                         0:LUT_nr_winddir_steps-1,  &
                         0:LUT_nr_theta_steps-1      ) :: table
    real(l_) :: v, phi, theta
    real(l_) :: sigma0

    integer  :: i,j,k
    real(l_) :: sigma111,sigma121,sigma112,sigma122
    real(l_) :: q,r

! BEWARE: since in 2 directions interpolation is done, 
! j and k should not be allowed to get a value of 
! LUT_nr_..._steps-1 !!!!! LUT_nr_..._steps-2 should be the limit !

    ! do not interpolate v in the table
    i = nint((v - LUT_min_windspeed)*one_over_LUT_windspeed_step)

    ! interpolate the table using phi and theta
    ! int rounds -0.001 towards 0, while floor rounds it towards -1 !!!!
    q = (phi - LUT_min_winddir)*one_over_LUT_winddir_step
    j = floor(q)
    r = (theta - LUT_min_theta)*one_over_LUT_theta_step
    k = floor(r)

    IF (i .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WS)
       END IF
       i = 0
    ELSE
       IF (i .gt. LUT_nr_windspeed_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             CALL print_message(WARN_LUT_HIGH_WS)
          END IF
          i = LUT_nr_windspeed_steps-1
       END IF
    END IF

    IF (j .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WD)
       END IF
       j = 0
       q = 0.0
    ELSE
       IF (j .ge. LUT_nr_winddir_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (j .ge. LUT_nr_winddir_steps) CALL print_message(WARN_LUT_HIGH_WD)
          END IF
          j = LUT_nr_winddir_steps-2
       END IF
       q = q - real(j)
    END IF

    IF (k .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_TH)
       END IF
       k = 0
       r = 0.0
    ELSE
       IF (k .ge. LUT_nr_theta_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (k .ge. LUT_nr_theta_steps) CALL print_message(WARN_LUT_HIGH_TH)
          END IF
          k = LUT_nr_theta_steps-2
       END IF
       r = r - real(k)
    END IF

! These exceptions can only occur at the table boundary.
! The negative values cause an extrapolation out of the table
! and in some cases this may result in a negative sigma0.
! The simulation program could then be trapped inside the endless loop 
! that calculates the instrument noise (because it will wait untill
! sigma0+noise gives a positive number which will never happen
! if sigma0 is negative !!!!)
!IF (q .lt. 0.0) q = 0.0
!IF (r .lt. 0.0) r = 0.0
! (to increase the programs speed, these exceptions are interwoven
!  in the series IF-THEN constructs above.)

    sigma111 = table(i  ,j  ,k  )
    sigma121 = table(i  ,j+1,k  )
    sigma112 = table(i  ,j  ,k+1)
    sigma122 = table(i  ,j+1,k+1)


    sigma0 = sigma111 + &
              q*(sigma121-sigma111) + &
              r*(sigma112-sigma111) + &
              q*r*(sigma122+sigma111-sigma112-sigma121)

  end function interpolate2d
  !---------------------------------------------------
  !  #]
#endif
#ifdef INTERPOLATE2DV
  function interpolate2dv(table,v,phi,theta) result(sigma0)
  !  #[
    ! do an interpolation in a 2 dimensional table of sigma0s
    ! interpolate along theta and v, (not phi as is done in
    ! interpolate2d). This is usefull when phi steps are equal
    ! to the LUT steps, as is often the case.

    real(r4_), dimension(0:LUT_nr_windspeed_steps-1,&
                         0:LUT_nr_winddir_steps-1,  &
                         0:LUT_nr_theta_steps-1      ) :: table
    real(l_) :: v, phi, theta
    real(l_) :: sigma0

    integer  :: i,j,k
    real(l_) :: sigma111,sigma211,sigma112,sigma212
    real(l_) :: p,r

! BEWARE: since in 2 directions interpolation is done, 
! i and k should not be allowed to get a value of 
! LUT_nr_..._steps-1 !!!!! LUT_nr_..._steps-2 should be the limit !

    ! do not interpolate phi in the table
    j = nint((phi - LUT_min_winddir)*one_over_LUT_winddir_step)

    ! interpolate the table using v and theta
    ! int rounds -0.001 towards 0, while floor rounds it towards -1 !!!!
    p = (v - LUT_min_windspeed)*one_over_LUT_windspeed_step
    i = floor(p)
    r = (theta - LUT_min_theta)*one_over_LUT_theta_step
    k = floor(r)

    IF (i .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WS)
       END IF
       i = 0
       p = 0.0
    ELSE
       IF (i .ge. LUT_nr_windspeed_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (i .ge. LUT_nr_windspeed_steps) CALL print_message(WARN_LUT_HIGH_WS)
          END IF
          i = LUT_nr_windspeed_steps-2
       END IF
       p = p - real(i)
    END IF

    IF (j .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WD)
       END IF
       j = 0
    ELSE
       IF (j .gt. LUT_nr_winddir_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             CALL print_message(WARN_LUT_HIGH_WD)
          END IF
          j = LUT_nr_winddir_steps-1
       END IF
    END IF

    IF (k .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_TH)
       END IF
       k = 0
       r = 0.0
    ELSE
       IF (k .ge. LUT_nr_theta_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (k .ge. LUT_nr_theta_steps) CALL print_message(WARN_LUT_HIGH_TH)
          END IF
          k = LUT_nr_theta_steps-2
       END IF
       r = r - real(k)
    END IF

! These exceptions can only occur at the table boundary.
! The negative values cause an extrapolation out of the table
! and in some cases this may result in a negative sigma0.
! The simulation program could then be trapped inside the endless loop 
! that calculates the instrument noise (because it will wait untill
! sigma0+noise gives a positive number which will never happen
! if sigma0 is negative !!!!)
!IF (p .lt. 0.0) p = 0.0
!IF (r .lt. 0.0) r = 0.0
! (to increase the programs speed, these exceptions are interwoven
!  in the series IF-THEN constructs above.)

    sigma111 = table(i  ,j  ,k  )
    sigma211 = table(i+1,j  ,k  )
    sigma112 = table(i  ,j  ,k+1)
    sigma212 = table(i+1,j  ,k+1)

    sigma0 = sigma111 + &
              p*(sigma211-sigma111) + &
              r*(sigma112-sigma111) + &
              p*r*(sigma212+sigma111-sigma112-sigma211)

  end function interpolate2dv
  !---------------------------------------------------
  !  #]
#endif
#ifdef INTERPOLATE3D
  function interpolate3d(table,v,phi,theta) result(sigma0)
  !  #[
    ! do an interpolation in a 3 dimensional table of sigma0s

    real(r4_), dimension(0:LUT_nr_windspeed_steps-1,&
                         0:LUT_nr_winddir_steps-1,  &
                         0:LUT_nr_theta_steps-1      ) :: table
    real(l_) :: v, phi, theta
    real(l_) :: sigma0

    integer  :: i,j,k
    real(l_) :: sigma111,sigma211,sigma121,sigma112
    real(l_) :: sigma222,sigma122,sigma212,sigma221
    real(l_) :: p,q,r

! BEWARE: since in 3 directions interpolation is done, 
! i,j and k should not be allowed to get a value of 
! LUT_nr_..._steps-1 !!!!! LUT_nr_..._steps-2 should be the limit !

    ! interpolate the table using v, phi and theta
    ! int rounds -0.001 towards 0, while floor rounds it towards -1 !!!!
    p = (v - LUT_min_windspeed)*one_over_LUT_windspeed_step
    i = floor(p)
    q = (phi - LUT_min_winddir)*one_over_LUT_winddir_step
    j = floor(q)
    r = (theta - LUT_min_theta)*one_over_LUT_theta_step
    k = floor(r)

    IF (i .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WS)
       END IF
       i = 0
       p = 0.0
    ELSE
       IF (i .ge. LUT_nr_windspeed_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (i .ge. LUT_nr_windspeed_steps) CALL print_message(WARN_LUT_HIGH_WS)
          END IF
          i = LUT_nr_windspeed_steps-2
       END IF
       p = p - real(i)
    END IF

    IF (j .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_WD)
       END IF
       j = 0
       q = 0.0
    ELSE
       IF (j .ge. LUT_nr_winddir_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (j .ge. LUT_nr_winddir_steps) CALL print_message(WARN_LUT_HIGH_WD)
          END IF
          j = LUT_nr_winddir_steps-2
       END IF
       q = q - real(j)
    END IF

    IF (k .lt. 0) THEN 
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
          CALL print_message(WARN_LUT_LOW_TH)
       END IF
       k = 0
       r = 0.0
    ELSE
       IF (k .ge. LUT_nr_theta_steps-1) THEN 
          IF (inv_settings%verbosity .ge. inversion_verbosity_warn) THEN
             IF (k .ge. LUT_nr_theta_steps) CALL print_message(WARN_LUT_HIGH_TH)
          END IF
          k = LUT_nr_theta_steps-2
       END IF
       r = r - real(k)
    END IF

! These exceptions can only occur at the table boundary.
! The negative values cause an extrapolation out of the table
! and in some cases this may result in a negative sigma0.
! The simulation program could then be trapped inside the endless loop 
! that calculates the instrument noise (because it will wait untill
! sigma0+noise gives a positive number which will never happen
! if sigma0 is negative !!!!)
!IF (p .lt. 0.0) p = 0.0
!IF (q .lt. 0.0) q = 0.0
!IF (r .lt. 0.0) r = 0.0
! (to increase the programs speed, these exceptions are interwoven
!  in the series IF-THEN constructs above.)

    sigma111 = table(i  ,j  ,k  )
    sigma211 = table(i+1,j  ,k  )
    sigma121 = table(i  ,j+1,k  )
    sigma112 = table(i  ,j  ,k+1)
    sigma221 = table(i+1,j+1,k  )
    sigma212 = table(i+1,j  ,k+1)
    sigma122 = table(i  ,j+1,k+1)
    sigma222 = table(i+1,j+1,k+1)

    sigma0 = sigma111 + &
              p*(sigma211-sigma111) + &
              q*(sigma121-sigma111) + &
              r*(sigma112-sigma111) + &
              p*q*(sigma221+sigma111-sigma121-sigma211) + &
              p*r*(sigma212+sigma111-sigma112-sigma211) + &
              q*r*(sigma122+sigma111-sigma112-sigma121) + &
              p*q*r*(sigma222+sigma112+sigma121+sigma211-&
                     sigma122-sigma212-sigma221-sigma111)

  end function interpolate3d
  !---------------------------------------------------
  !  #]
#endif
  !  #]
  subroutine read_LUT(table, filename, polarimetric_LUT, &
                      doppler_LUT, succes)
  !  #[
    real(r4_), intent(out), dimension(0:LUT_nr_windspeed_steps-1,&
                                      0:LUT_nr_winddir_steps-1,  &
                                      0:LUT_nr_theta_steps-1      ) :: table
    character(len=256), intent(in) :: filename 
    logical, intent(in)            :: polarimetric_LUT
    logical, intent(in)            :: doppler_LUT
    logical, intent(out)           :: succes

    character(len=256) :: filename_zspace
    integer  :: fileunit
    integer  :: i,j,k
    logical  :: exists
    ! used for checking the LUT 
    ! in case of vv/hh it must have only positive numbers
    ! in case of pol it must not be centered around -1/1 otherwise
    ! the LUT has been converted in a wrong way
    real(l_) :: testaverage = 0.0
    integer  :: testcount   = 0
    integer  :: testfilelength

    succes = .true.
    IF (inv_settings%use_zspace .and. (.not. doppler_LUT) ) THEN
       ! check wheter the needed LUT is available in zspace version
       ! and if so, use it
       filename_zspace = trim(filename) // ".zspace"
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,'read LUT from file: ',trim(filename_zspace)
       endif
       inquire(file=trim(filename_zspace), exist=exists)
       IF (exists) THEN ! read the look-up-table from file
          fileunit = get_lun()

          ! checking big/little endianness of this file
          open(unit=fileunit,file=trim(filename),status='old',&
               access="direct",recl=4,ERR=98)
          read(fileunit,rec=1) testfilelength
          close(unit=fileunit)

          if (testfilelength .ne. LUT_nr_windspeed_steps*&
                                  LUT_nr_winddir_steps*&
                                  LUT_nr_theta_steps*4) then
             !=250*73*51*4
             if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
                print *,"zspace-LUT file has unexpected format...."
                print *,"testfilelength = ",testfilelength
                print *,"probably this is produced on a little endian machine"
                print *,"while this current machine is big-endian, or vice versa"
                print *," "
                print *,"trying to read the regular LUT"
             endif
          else
             open(unit=fileunit,file=trim(filename_zspace),status='old',&
                  form="unformatted",ERR=98)
             read(fileunit) table
             close(unit=fileunit)
             CALL free_lun(fileunit)
             return
          end if
       ELSE
          if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
            print *,'WARNING: no zspace version of the LUT found, try to read a regular LUT'
          endif
       ENDIF
    END IF

    ! check the existence of a regular LUT
    inquire(file=trim(filename), exist=exists)
    IF (exists) THEN ! read the look-up-table from file
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,'read LUT from file: ',trim(filename)
       endif
       fileunit = get_lun()

       ! checking big/little endianness of this file
       open(unit=fileunit,file=trim(filename),status='old',&
            access="direct",recl=4,ERR=98)
       read(fileunit,rec=1) testfilelength
       !print *,"testfilelength = ",testfilelength
       close(unit=fileunit)

       if (testfilelength .ne. LUT_nr_windspeed_steps*&
                               LUT_nr_winddir_steps*&
                               LUT_nr_theta_steps*4) then
          !=250*73*51*4
          print *,"LUT file has unexpected format...."
          print *,"testfilelength = ",testfilelength
          print *,"probably this is produced on a little endian machine"
          print *,"while this current machine is big-endian, or vice versa"
          succes = .false.
          return
       else
          open(unit=fileunit,file=trim(filename),status='old',&
               form="unformatted",ERR=98)
          read(fileunit) table
          close(unit=fileunit)
          CALL free_lun(fileunit)
       end if
    ELSE
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
         print *,'WARNING: look-up table file: ',trim(filename),' could not be found'
       endif
       succes = .false.
       return
    END IF ! end of check for existence of LUT file

    IF (polarimetric_LUT) THEN
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,'Checking the integrity of this polarimetric Look-Up-Table'
       endif
       testaverage = 0.0
       testcount   = 0
       DO i = 0,LUT_nr_windspeed_steps-1
          DO j = 0,LUT_nr_winddir_steps-1
             DO k = 0,LUT_nr_theta_steps-1
                testaverage = testaverage + abs(table(i,j,k))
                testcount = testcount + 1
             END DO
          END DO
       END DO
       testaverage = testaverage / testcount
       IF (testaverage .gt. 0.8) THEN
          print *,'ERROR: LUT contains many values of 0.8 or higher !!!!!!!'
          print *,'ERROR: this indicates that conversion of this'
          print *,'ERROR: polarimetric LUT was not converted in the right'
          print *,'ERROR: way from Linux Unix or vice versa. Probably the'
          print *,'ERROR: conversion from DB to ordinary numbers was'
          print *,'ERROR: performed (which should be left out for the '
          print *,'ERROR: polarimetric LUT conversion!)'
          print *,'quitting .....'
          CALL my_exit
       END IF
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,'Integrity Check complete'
       endif
    ELSE ! vv/hh case
       if (.not. doppler_LUT) then
          if (inv_settings%verbosity .ge. inversion_verbosity_report) then
             print *,'Checking the integrity of this Look-Up-Table .....'
          endif
          DO i = 0,LUT_nr_windspeed_steps-1
             DO j = 0,LUT_nr_winddir_steps-1
                DO k = 0,LUT_nr_theta_steps-1   
                   IF (table(i,j,k) .lt. 0.0) THEN
                      print *,'ERROR: LUT contains a negative number!'
                      print *,'ERROR: i,j,k,table(i,j,k)=',i,j,k,table(i,j,k)
                      print *,'ERROR: this should never happen!'
                      print *,'quitting .....'
                      CALL my_exit
                   END IF
                END DO
             END DO
          END DO
          if (inv_settings%verbosity .ge. inversion_verbosity_report) then
             print *,'Integrity Check complete'
          endif
       end if ! check for Doppler case
    END IF ! check for polarimetric case

    IF (inv_settings%use_zspace .and. (.not. doppler_LUT) ) THEN
       ! convert the complete table to zspace
       ! for a definition of zspace, see thesis Ad Stoffelen, p.III-3
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
         print *,'converting the table of sigma0s to zspace .....'
       endif
       DO i = 0,LUT_nr_windspeed_steps-1
          DO j = 0,LUT_nr_winddir_steps-1
             DO k = 0,LUT_nr_theta_steps-1   
                table(i,j,k) = table(i,j,k)**Zfactor
             END DO
          END DO
       END DO
       if (inv_settings%verbosity .ge. inversion_verbosity_report) then
          print *,'saving the converted table to disk, for later use ...'
          print *,'filename: ',trim(filename_zspace)
       endif
       fileunit = get_lun()
       open(unit=fileunit,file=trim(filename_zspace),status='new',&
            form="unformatted",ERR=98)
       write(fileunit) table
       close(unit=fileunit)
       CALL free_lun(fileunit)
    ENDIF

    return

    !*** display error and stop if no file could be read
98  print *,'ERROR while opening input file:',trim(filename)
    succes = .false.
    return

  END subroutine read_LUT
  !---------------------------------
  !  #]
  subroutine create_LUT_C_VV(table, filename)
  !  #[
    real(r4_), intent(out), dimension(0:LUT_nr_windspeed_steps-1,&
                                      0:LUT_nr_winddir_steps-1,  &
                                      0:LUT_nr_theta_steps-1      ) :: table
    character(len=256), intent(in) :: filename 

    integer  :: fileunit
    integer :: i,j,k
    real(l_) :: windspeed, winddir, theta
    character(len=256)  :: filename_zspace

    if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
      print *,'Creating new look-up table'
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod4_to_construct_LUT) then
         print *,"GMF function used: cmod4"
      endif
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod5_to_construct_LUT) then
         print *,"GMF function used: cmod5"
      endif
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod5_5_to_construct_LUT) then
         print *,"GMF function used: cmod5_5"
      endif
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod5_n_to_construct_LUT) then
         print *,"GMF function used: cmod5_n"
      endif
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod6_to_construct_LUT) then
         print *,"GMF function used: cmod6"
      endif
      if (inv_settings%use_which_fn_to_construct_LUT .eq. &
           use_cmod7_to_construct_LUT) then
         print *,"GMF function used: cmod7"
      endif
    endif

    filename_zspace = trim(filename) // ".zspace"

    DO i = 0,LUT_nr_windspeed_steps-1
       windspeed = LUT_min_windspeed + i*LUT_windspeed_step
       DO j = 0,LUT_nr_winddir_steps-1
          winddir = LUT_min_winddir + j*LUT_winddir_step
          DO k = 0,LUT_nr_theta_steps-1 
             theta = LUT_min_theta + k*LUT_theta_step
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod4_to_construct_LUT) THEN
                table(i,j,k) = calc_sigma0_cmod4(windspeed,winddir,theta)
             END IF
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod5_to_construct_LUT) THEN
                table(i,j,k) = calc_sigma0_cmod5(windspeed,winddir,theta)
             END IF
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod5_5_to_construct_LUT) THEN
                table(i,j,k) = calc_sigma0_cmod5_5(windspeed,winddir,theta)
             END IF
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod5_n_to_construct_LUT) THEN
                table(i,j,k) = calc_sigma0_cmod5_n(windspeed,winddir,theta)
             END IF
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod6_to_construct_LUT) THEN
                table(i,j,k) = calc_sigma0_cmod6(windspeed,winddir,theta)
             END IF
             IF (inv_settings%use_which_fn_to_construct_LUT .eq. &
                  use_cmod7_to_construct_LUT) THEN
                ! cmod7 is only available as lookup table.
                !table(i,j,k) = calc_sigma0_cmod7(windspeed,winddir,theta) 
             END IF
          END DO
       END DO
    END DO

    if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
      print *,'Writing constructed C_VV LUT to file: ',trim(filename), &
              ' for later use'
    endif

    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='replace',&
         form="unformatted",ERR=98)
    write(fileunit) table
    close(unit=fileunit)
    CALL free_lun(fileunit)

    IF (inv_settings%use_zspace) THEN
       ! convert the complete table to zspace
       ! for a definition of zspace, see thesis Ad Stoffelen, p.III-3
       DO i = 0,LUT_nr_windspeed_steps-1
          DO j = 0,LUT_nr_winddir_steps-1
             DO k = 0,LUT_nr_theta_steps-1   
                table(i,j,k) = table(i,j,k)**Zfactor
             END DO
          END DO
       END DO
       if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
         print *,'Writing converted zspace LUT to file: ',trim(filename_zspace), &
                 ' for later use'
       endif
       fileunit = get_lun()
       open(unit=fileunit,file=trim(filename_zspace),status='replace',&
            form="unformatted",ERR=98)
       write(fileunit) table
       close(unit=fileunit)
       CALL free_lun(fileunit)
    END IF
    return

    !*** display error and stop if no file could be read
98  print *,'ERROR while opening output file:',trim(filename)
    print *,'*** ... quitting ... ***'
    CALL my_exit

  END subroutine create_LUT_C_VV
  !---------------------------------
  !  #]
  subroutine create_LUT_C_VV_DOP(table, filename)
    !  #[ compose the C_VV_DOP LUT
    real(r4_), intent(out), dimension(0:LUT_nr_windspeed_steps-1,&
                                      0:LUT_nr_winddir_steps-1,  &
                                      0:LUT_nr_theta_steps-1      ) :: table
    character(len=256), intent(in) :: filename 

    integer  :: fileunit
    integer :: i,j,k
    real(l_) :: windspeed, winddir, theta

    print *,"GMF function used: cdop"

    DO i = 0,LUT_nr_windspeed_steps-1
       windspeed = LUT_min_windspeed + i*LUT_windspeed_step
       DO j = 0,LUT_nr_winddir_steps-1
          winddir = LUT_min_winddir + j*LUT_winddir_step
          DO k = 0,LUT_nr_theta_steps-1 
             theta = LUT_min_theta + k*LUT_theta_step
             table(i,j,k) = calc_cdop(windspeed,winddir,theta,c_vv_dop)
          END DO
       END DO
    END DO

    if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
      print *,'Writing constructed C_VV_DOP LUT to file: ',trim(filename), &
              ' for later use'
    endif

    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='replace',&
         form="unformatted",ERR=98)
    write(fileunit) table
    close(unit=fileunit)
    CALL free_lun(fileunit)

    return

    !*** display error and stop if no file could be written
98  print *,'ERROR while opening output file:',trim(filename)
    print *,'*** ... quitting ... ***'
    CALL my_exit
    
  end subroutine create_LUT_C_VV_DOP
    !  #]
  subroutine create_LUT_C_HH_DOP(table, filename)
    !  #[ compose the C_HH_DOP LUT
    real(r4_), intent(out), dimension(0:LUT_nr_windspeed_steps-1,&
                                      0:LUT_nr_winddir_steps-1,  &
                                      0:LUT_nr_theta_steps-1      ) :: table
    character(len=256), intent(in) :: filename 

    integer  :: fileunit
    integer :: i,j,k
    real(l_) :: windspeed, winddir, theta

    print *,"GMF function used: cdop"

    DO i = 0,LUT_nr_windspeed_steps-1
       windspeed = LUT_min_windspeed + i*LUT_windspeed_step
       DO j = 0,LUT_nr_winddir_steps-1
          winddir = LUT_min_winddir + j*LUT_winddir_step
          DO k = 0,LUT_nr_theta_steps-1 
             theta = LUT_min_theta + k*LUT_theta_step
             table(i,j,k) = calc_cdop(windspeed,winddir,theta,c_hh_dop)
          END DO
       END DO
    END DO

    if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
      print *,'Writing constructed C_HH_DOP LUT to file: ',trim(filename), &
              ' for later use'
    endif

    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='replace',&
         form="unformatted",ERR=98)
    write(fileunit) table
    close(unit=fileunit)
    CALL free_lun(fileunit)

    return

    !*** display error and stop if no file could be written
98  print *,'ERROR while opening output file:',trim(filename)
    print *,'*** ... quitting ... ***'
    CALL my_exit
    
  end subroutine create_LUT_C_HH_DOP
    !  #]
  subroutine cross_check_for_identical_LUTs(pol)
    !  #[ verify that the same table is not used for different cases
    integer, intent(in) :: pol

    ! local variables
    real(r4_), pointer, dimension(:,:,:) :: this_table
    character(len=15) :: this_table_text

    nullify(this_table)

    select case (pol)
    case(c_vv_pol, c_vh_pol)
       this_table => c_vv_table
       this_table_text = 'c_vv_table'
    case(c_hh_pol)
       this_table => c_hh_table
       this_table_text = 'c_hh_table'
    case(c_po_pol)
       this_table => c_pol_table
       this_table_text = 'c_pol_table'
    case(ku_vv_pol, ku_vh_pol)
       this_table => ku_vv_table
       this_table_text = 'ku_vv_table'
    case(ku_hh_pol)
       this_table => ku_hh_table
       this_table_text = 'ku_hh_table'
    case(ku_po_pol)
       this_table => ku_pol_table
       this_table_text = 'ku_pol_table'
    case(c_vv_dop)
       this_table => c_vv_dop_table
       this_table_text = 'c_vv_dop_table'
    case(c_hh_dop)
       this_table => c_hh_dop_table
       this_table_text = 'c_hh_dop_table'
    end select

    IF ( (pol .ne. c_vv_pol) .and. (.not. first_call_c_vv) ) &
         CALL test_for_identical_LUTs(this_table, c_vv_table,&
         trim(this_table_text),"c_vv_table")

    IF ( (pol .ne. c_hh_pol) .and. (.not. first_call_c_hh) ) &
         CALL test_for_identical_LUTs(this_table, c_hh_table,&
         trim(this_table_text),"c_hh_table")

    IF ( (pol .ne. c_po_pol) .and. (.not. first_call_c_pol) ) &
         CALL test_for_identical_LUTs(this_table, c_pol_table,&
         trim(this_table_text),"c_pol_table")

    IF ( (pol .ne. ku_vv_pol) .and. (.not. first_call_ku_vv) ) &
         CALL test_for_identical_LUTs(this_table, ku_vv_table,&
         trim(this_table_text),"ku_vv_table")

    IF ( (pol .ne. ku_hh_pol) .and. (.not. first_call_ku_hh) ) &
         CALL test_for_identical_LUTs(this_table, ku_hh_table,&
         trim(this_table_text),"ku_hh_table")

    IF ( (pol .ne. ku_po_pol) .and. (.not. first_call_ku_pol) ) &
         CALL test_for_identical_LUTs(this_table, ku_pol_table,&
         trim(this_table_text),"ku_pol_table")

    IF ( (pol .ne. c_vv_dop) .and. (.not. first_call_c_vv_dop) ) &
         CALL test_for_identical_LUTs(this_table, c_vv_dop_table,&
         trim(this_table_text),"c_vv_dop_table")

    IF ( (pol .ne. c_hh_dop) .and. (.not. first_call_c_hh_dop) ) &
         CALL test_for_identical_LUTs(this_table, c_hh_dop_table,&
         trim(this_table_text),"c_hh_dop_table")

  end subroutine cross_check_for_identical_LUTs
  !  #]
  subroutine test_for_identical_LUTs(new_table, existing_table,&
                                     name_new_table,name_existing_table)
  !  #[
    character(len=*), intent(in)  :: name_new_table
    character(len=*), intent(in)  :: name_existing_table
    
    real(r4_), dimension(0:LUT_nr_windspeed_steps-1,&
                         0:LUT_nr_winddir_steps-1,  &
                         0:LUT_nr_theta_steps-1      ) :: new_table, &
                                                          existing_table

    integer :: i,j,k

    ! check whether a newly loaded table is identical to an already
    ! avaliable table in memory. If so this points to a serious problem.
    ! probably a mistake in setting the filenames for the LUTs has been
    ! made, but checking the filenames is not sufficient because it is
    ! also possible that by accident one file is copied to another
    ! files name. The only way to find out is looking at the data ....

    if (inv_settings%verbosity .ge. inversion_verbosity_report) then
       print *,"comparing LUT's:"
       print *,"new Look-Up-Table:      ",trim(name_new_table)
       print *,"existing Look-Up-Table: ",trim(name_existing_table)
    endif

    ! first check a few random points
    i=0; j=0; k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=LUT_nr_windspeed_steps-1
    j=0
    k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=0
    j=LUT_nr_winddir_steps-1
    k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=0
    j=0
    k=LUT_nr_theta_steps-1
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=floor(0.5*LUT_nr_windspeed_steps)
    j=0
    k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=0
    j=floor(0.5*LUT_nr_winddir_steps)
    k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=0
    j=0
    k=floor(0.5*LUT_nr_theta_steps)
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=floor(0.5*LUT_nr_windspeed_steps)
    j=floor(0.5*LUT_nr_winddir_steps)
    k=0
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=floor(0.5*LUT_nr_windspeed_steps)
    j=0
    k=floor(0.5*LUT_nr_theta_steps)
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=0
    j=floor(0.5*LUT_nr_winddir_steps)
    k=floor(0.5*LUT_nr_theta_steps)
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    i=floor(0.5*LUT_nr_windspeed_steps)
    j=floor(0.5*LUT_nr_winddir_steps)
    k=floor(0.5*LUT_nr_theta_steps)
    IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return

    ! if no difference has been found yet scan the whole table
    DO i=0,LUT_nr_windspeed_steps-1
       DO j=0,LUT_nr_winddir_steps-1
          DO k=0,LUT_nr_theta_steps-1  
             IF (new_table(i,j,k) .ne. existing_table(i,j,k)) return
          END DO
       END DO
    END DO

    print *,"FATAL ERROR: "
    print *,"new Look-Up-Table:    ",trim(name_new_table)
    print *,"equals Look-Up-Table: ",trim(name_existing_table)
    print *,"which is already loaded into memory."
    print *,"quitting program ...."
    CALL my_exit

  end subroutine test_for_identical_LUTs
  !---------------------------------
  !  #]
  function my_mod(x,y) result(m)
  !  #[
    ! reduce range of x to [0:y]
    real(l_) :: x,y,m

    IF (y .le. 0.0) THEN
      print *,'ERROR: a modulo of below 0 is nonsense !!!!!!!'
      print *,'ERROR: function my_mod has aborted the program ...'
      CALL my_exit
    END IF

    m = x-y*floor(x/y)

    return
  end function my_mod
  !--------------------------------
  !  #]
  function my_min(x,n1,n2) result(mymin)
  !  #[
    ! find the minimum value in array x, between indices n1 and n2

    ! this function cannot be replaced by the standard f90 min()
    ! function, because its arguments are not default reals,
    ! but my own l_ type real.

    integer                        :: n1, n2
    real(l_), dimension(n1-1:n2+1) :: x
    real(l_)                       :: mymin
    integer                        :: i

    mymin = x(n1)
    DO i = n1+1, n2
      IF (x(i) .lt. mymin) mymin = x(i)
    END DO

  end function my_min
  !--------------------------------
  !  #]
  function my_max(x,n1,n2) result(mymax)
  !  #[
    ! find the maximum value in array x, between indices n1 and n2

    ! this function cannot be replaced by the standard f90 max()
    ! function, because its arguments are not default reals,
    ! but my own l_ type real.

    integer                        :: n1, n2
    real(l_), dimension(n1-1:n2+1) :: x
    real(l_)                       :: mymax
    integer                        :: i

    mymax = x(n1)
    DO i = n1+1, n2
      IF (x(i) .gt. mymax) mymax = x(i)
    END DO

  end function my_max
  !--------------------------------
  !  #]
  function my_average(x,n1,n2) result(average)
  !  #[
    ! find the average value of array x, between indices n1 and n2

    integer                        :: n1, n2
    real(l_), dimension(n1-1:n2+1) :: x
    real(l_)                       :: mysum, average
    integer                        :: i

    mysum = 0.0_l_
    DO i = n1, n2
      mysum = mysum + x(i)
    END DO

    average = mysum/(n2-n1+1)

  end function my_average
  !--------------------------------
  !  #]
  subroutine get_indices_lowest_local_minima(xx, vv, n1, n2, indices, max_nr_of_minima, &
                            nr_of_minima, status, minimum_at_lutmin, minimum_at_lutmax)
                             
  !  #[
    ! A subroutine to look for local minima in the array x
    ! It finds all minima but only returns the 
    ! max_nr_of_minima deepest ones.
    ! In stead of keeping all minima in an array, than sorting it
    ! and extracting the max_nr_of_minima deepest ones 
    ! (as was done in the
    ! old software version) I only save the deepest ones, and keep them
    ! unsorted in the indices array. 
    ! Then sort them using the MLE value of each minimum.
    ! If all is well status=0

    ! added option: if the switch ignore_big_minima is set then 
    ! minima larger than a given "fraction" of max(x) are ignored.

    integer                         :: n1,n2
    real(l_), dimension(n1-1:n2+1)  :: xx, x, vv
    integer,  dimension(1:max_nr_of_sol)  :: indices
    integer                         :: max_nr_of_minima,nr_of_minima,status
    logical                         :: minimum_at_lutmin,minimum_at_lutmax
    integer                         :: i, maxindex
    real(l_), dimension(1:max_nr_of_sol)  :: minima_array
    real(l_) :: threshold

    ! this var is used to remember what the largest minimum is, which
    ! is stored in the indices array at any time.
    real(l_) :: highest_min_in_minima_array, max_x
    real(l_), parameter :: thr_fraction = 0.99 ! 0.75 or 0.25 used in the past
    logical, parameter  :: ignore_big_minima = .true.
    logical, parameter  :: reject_lutmax_sol = .false.  ! IF .true. discard sols at speed LUT max

    ! copy xx to x to prevent local changes here to affect the rest of 
    ! the program
    x(n1:n2) = xx(n1:n2)

    ! copy both ends of the array to the 2 axtra array fields
    ! to prevent a lot of extra code for exceptions at the array-edge.
    x(n1-1) = x(n2)
    x(n2+1) = x(n1)

    ! start the search for local minima
    minimum_at_lutmin = .false.
    minimum_at_lutmax = .false.
    status = 0
    nr_of_minima = 0
    highest_min_in_minima_array = x(n1)
    max_x = my_max(x,n1,n2)
    threshold = thr_fraction*max_x
    DO i = n1, n2
      IF (x(i-1) .gt. x(i) .and. x(i) .le. x(i+1)) THEN

        ! local minimum found !
        IF ((.not. ignore_big_minima) .or. &
            (ignore_big_minima .and. (x(i) .le. threshold))) THEN

          ! flag minimum at LUT min & LUT max
          IF (vv(i) .le. min_windvelocity+0.01) minimum_at_lutmin = .true.
          IF (vv(i) .ge. max_windvelocity-0.01) minimum_at_lutmax = .true.
          IF ((.not. reject_lutmax_sol) .or. &
              (reject_lutmax_sol .and. vv(i) .lt. max_windvelocity-0.01)) THEN
            nr_of_minima = nr_of_minima + 1
            IF (nr_of_minima .le. max_nr_of_minima) THEN

              ! add found minimum (in unsorted order) to the indices array.
              indices(nr_of_minima)      = i
              minima_array(nr_of_minima) = x(i)

              ! define new highest_min_in_minima_array
              maxindex = my_index_max(minima_array,1,nr_of_minima)
              highest_min_in_minima_array = minima_array(maxindex)
            ELSE

              ! see if one of the already found indices is above this one
              ! if not, forget about it, if so, throw away the higher one
              ! from the indices array and add this one to the indices array.
              IF (x(i) .lt. highest_min_in_minima_array) THEN

                ! find the array index of the maximum value in the minima_array
                ! and replace it with the new found minimum
                maxindex = my_index_max(minima_array,1,max_nr_of_minima) 
                indices(maxindex)      = i
                minima_array(maxindex) = x(i)

                ! define new highest_min_in_minima_array
                maxindex = my_index_max(minima_array,1,max_nr_of_minima)
                highest_min_in_minima_array = minima_array(maxindex)
              END IF ! new low minimum found ?
            END IF ! nr_of_minima .lt. max_nr_of_minima ?
          END IF ! reject_lutmax_sol ?
        END IF ! ignore_big_minima ?
      END IF ! local minimum found !
    END DO ! i (loop over x array)

    IF (nr_of_minima .gt. max_nr_of_minima) THEN 

       ! too many minima found
       IF (inv_settings%verbosity .ge. inversion_verbosity_warn .and. &
           .not. inv_settings%get_all_winddirs) &
           CALL print_message(WARN_HIGHNR_MIN)
       status = 99
       nr_of_minima = max_nr_of_minima
    END IF

    IF (nr_of_minima .eq. 0) THEN

       ! no minima found
       IF (inv_settings%verbosity .ge. inversion_verbosity_report) &
           CALL print_message(WARN_NO_MINIMA)
       status = -1
    END IF

  end subroutine get_indices_lowest_local_minima
  !--------------------------------
  !  #]
!
! only needed for unsorted cases, so the 144 sol. case??
!  function my_index_min(x,n1,n2) result(minindex)
  !  #[
!    ! find the index of the minimum value in array x, between indices n1 and n2
!
!    real(l_), dimension(n1:n2)     :: x
!    integer                        :: n1, n2
!    integer                        :: minindex
!    real(l_)                       :: min
!    integer                        :: i
!
!    minindex = n1
!    DO i = n1+1, n2
!      IF (x(i) .lt. x(minindex)) minindex = i
!    END DO
!
!    return
!  end function my_index_min
!  !--------------------------------
  !  #]
  function my_index_max(x,n1,n2) result(maxindex)
  !  #[
    ! find the index of the maximum value in array x, between indices n1 and n2

    integer                        :: n1, n2
    real(l_), dimension(n1:n2)     :: x
    integer                        :: maxindex
    integer                        :: i

    maxindex = n1
    DO i = n1+1, n2
      IF (x(i) .gt. x(maxindex)) maxindex = i
    END DO

    return
  end function my_index_max
  !--------------------------------
  !  #]
  subroutine my_exit
  !  #[
  print *,"Exiting the program ......"

#ifdef USE_MPI
  CALL MPI_abort(MPI_COMM_WORLD,0,mpi_err)
#endif
  stop

  end subroutine my_exit
  !--------------------------------
  !  #]
  subroutine print_wind_quality_code(wqc)
  !  #[
  integer :: wqc

  print *,'-----------------------------------------'
  print *,"Explaining wind quality code number ",wqc

  SELECT CASE (wqc)
!
! first all ok codes
!
  CASE(wqc_ok)
    print *,"OK, there are solutions found (at least 1)"
  CASE(wqc_ok_144sol) 
     print *,"ok, but GET_ALL_WINDDIRS switch was used, so usually all 144"
     print *,"solutions are returned UNSORTED"
  CASE(wqc_ok_toomany)
    print *,"More than inv_settings%max_nr_of_solutions = ",&
         inv_settings%max_nr_of_solutions
    print *,"solutions found, probably the result is OK, but not all"
    print *,"solutions (minima) have been reported"
!
! then follow the notok codes
!
  CASE(wqc_notok_nodirfound)
    print *,"No wind direction found, only min,average and max of windspeed"
    print *," which is returned in foundwindspeed(1),(2) and (3)"
  CASE(wqc_notok_noresult)
    print *,"No wind direction found, "
    print *,"but inv_settings%max_nr_of_solutions < 3, so it is not possible"
    print *,"to give min/avg/max as output, therefore no output is available"
  CASE(wqc_notok_nosol)
    print *,"No solutions were found"
  CASE DEFAULT
    IF (missing_int(wqc)) THEN
       print *,'WARNING: wqc is missing/undefined !'
    ELSE
       print *,'wind_quality_code:', wqc
       print *,'WARNING: this wind_quality_code is not set in the inversion module! It must'
       print *,'         be an instrument specific flag and therefore set after inversion.'
    END IF
  END SELECT

  print *,'-----------------------------------------'
  return

  end subroutine print_wind_quality_code
  !--------------------------------
  !  #]
  subroutine print_input_data_of_inversion(inv_input)
  !  #[
    TYPE(inv_input_type)  :: inv_input

    print *,'inversion inputs:'
    print *,'nr_of_views:  ',inv_input%nr_of_views
    print *,'pol:          ',&
         inv_input%pol(        1:inv_input%nr_of_views)
    print *,'antenna_dir:  ',&
         inv_input%antenna_dir(1:inv_input%nr_of_views)
    print *,'theta_angle:  ',&
         inv_input%theta_angle(1:inv_input%nr_of_views)
    print *,'sigma_0:      ',&
         inv_input%sigma_0(    1:inv_input%nr_of_views)
    print *,'kp_a:         ',&
         inv_input%kp_a(       1:inv_input%nr_of_views)
    print *,'kp_b:         ',&
         inv_input%kp_b(       1:inv_input%nr_of_views)
    print *,'kp_c:         ',&
         inv_input%kp_c(       1:inv_input%nr_of_views)
                      
    print *,'sigma_0_vh:   ',&
         inv_input%sigma_0_vh( 1:inv_input%nr_of_views)
    print *,'doppler_shift:',&
         inv_input%doppler_shift( 1:inv_input%nr_of_views)
    print *,''
    print *,"NOTE1: antenna_dir might have been altered by the call to"
    print *,"       invert_one_wvc, depending on your wind direction"
    print *,"       convention settings."
    print *,"NOTE2: sigma_0 might have been altered by the call to"
    print *,"       invert_one_wvc, depending on your z_space settings."
    print *,"Therefore to be sure you get the right numbers printed, run the"
    print *,"print_input_data_of_inversion routine BEFORE running"
    print *,"invert_one_wvc (or run it twice, before and after, to see what"
    print *,"inv_input items actually where modified)"

  end subroutine print_input_data_of_inversion
  !--------------------------------
  !  #]
  subroutine print_output_data_of_inversion(inv_output)
  !  #[
    TYPE(inv_output_type) :: inv_output
    integer :: i

    print *,'KNMI inversion results:'
    print *,'=== nr_of_windsolutions:   ',inv_output%nr_of_windsolutions
    print *,'=== wind_quality_code:     ',inv_output%wind_quality_code
    IF (inv_output%wind_quality_code .ne. wqc_ok) &
         CALL print_wind_quality_code(inv_output%wind_quality_code)
    SELECT CASE (inv_output%GMF_version_used)
    CASE(cmod4_used_as_GMF)
       print *,'=== GMF version used:      CMOD4'
    CASE(cmod5_used_as_GMF)
       print *,'=== GMF version used:      CMOD5'
    CASE(cmod5_5_used_as_GMF)
       print *,'=== GMF version used:      CMOD5_5'
    CASE(cmod5_n_used_as_GMF)
       print *,'=== GMF version used:      CMOD5_N'
    CASE(cmod6_used_as_GMF)
       print *,'=== GMF version used:      CMOD6'
    CASE(cmod7_used_as_GMF)
       print *,'=== GMF version used:      CMOD7'
    CASE DEFAULT
       print *,'=== GMF version used:      UNKNOWN'
    END SELECT
    DO i=1, inv_output%nr_of_windsolutions
       print *,'solution : ',i
       print *,'   === foundwindspeed:        ', inv_output%foundwindspeed(i)
       print *,'   === foundwinddir:          ', inv_output%foundwinddir(i)
       print *,'   === conedistance_measured: ',&
            inv_output%conedistance_measured(i)
       print *,'   === probability:           ', inv_output%probability(i)
    END DO
    print *,'=== skill:                 ', inv_output%skill

  end subroutine print_output_data_of_inversion
  !--------------------------------
  !  #]
  subroutine print_in_out_data_of_inversion(inv_input, inv_output)
  !  #[
    TYPE(inv_input_type)  :: inv_input
    TYPE(inv_output_type) :: inv_output

    print *,'TEST of the inversion subroutine:'
    print *,""
    CALL print_input_data_of_inversion(inv_input)
    CALL print_output_data_of_inversion(inv_output)

  end subroutine print_in_out_data_of_inversion
  !--------------------------------
  !  #]
  function calc_sigma0_cmod4(v,phi,theta,calc_B0) result(sigma_0)
  !  #[
    !-----------------------------------------------------
    ! inputs:
    !   -v,     in [m/s] wind velocity (always >= 0)
    !   -phi,   in [deg] angle between azimuth and wind direction
    !   -theta, in [deg] incidence angle
    ! output:
    !   -sigma_0 [dimensieloos, zie p. I-12, thesis Stoffelen]
    !-----------------------------------------------------
    
!    real(l_) :: f1 !,Get_Br_from_Look_Up_Table
    real(l_) :: v, phi, theta, sigma_0
    real(l_) :: B0, B1, B2, B3, Br
    real(l_) :: f2
    real(l_) :: alfa, gamma, beta
    real(l_) :: p0, p1, p2, x
    real(l_), parameter :: c1  = -2.301523, c2  = -1.632686, c3  =  0.761210, &
                           c4  =  1.156619, c5  =  0.595955, c6  = -0.293819, &
                           c7  = -1.015244, c8  =  0.342175, c9  = -0.500786, &
                           c10 =  0.014430, c11 =  0.002484, c12 =  0.074450, &
                           c13 =  0.004023, c14 =  0.148810, c15 =  0.089286, &
                           c16 = -0.006667, c17 =  3.000000, c18 =-10.000000
  
    logical,optional :: calc_B0

    Br = Get_Br_from_Look_Up_Table(theta)
    x  = (theta-40.0)/25.0
  
    ! voor f1 zie functie definitie onder deze cmod4 functie
    f2 = tanh( 2.5*(x+0.35)) - 0.61*(x+0.35)
    
    p0 = 1.0
    p1 = x
    p2 = (3.0*x*x-1.0)/2.0
    
    alfa  = c1*p0 + c2*p1 + c3*p2
    gamma = c4*p0 + c5*p1 + c6*p2
    beta  = c7*p0 + c8*p1 + c9*p2
    
    B0 = Br * 10**(alfa + gamma*f1(v+beta))
    B1 = c10*p0 +  c11*v +  (c12*p0 + c13*v)*f2
    B2 = c14*p0 +  c15*(1.0+p1)*v
    B3 = 0.42*(1.0 + c16*(c17+x)*(c18+v))
    
    IF (present(calc_B0)) THEN
      IF (calc_B0) THEN
         sigma_0 = B0
         return
      END IF
    END IF



    sigma_0 = B0 * (1.0 + B1*cos(phi*deg2rad) + &
         B3*tanh(B2)*cos(2.0*phi*deg2rad ))**1.6
    
    return
  end function calc_sigma0_cmod4
  !--------------------------------
  function f1(s) result(x) 
    
    real(l_) :: s,x
  
    if (s .le. 1.0e-10) then 
       x = -10.0
    else 
       if (s .le. 5.0) then 
          x = log10(s)
       else 
          x = sqrt(s)/3.2
       end if
    end if
    return
  end function f1
  !--------------------------------
  function Get_Br_from_Look_Up_Table(theta) result(Br)

    integer  :: th_index
    real(l_) :: theta, Br
    real(l_), dimension(16:60), parameter :: br_table = &
         (/ 1.075, 1.075, 1.075, 1.072, 1.069, &
            1.066, 1.056, 1.030, 1.004, 0.979, &
            0.967, 0.958, 0.949, 0.941, 0.934, &
            0.927, 0.923, 0.930, 0.937, 0.944, &
            0.955, 0.967, 0.978, 0.988, 0.998, &
            1.009, 1.021, 1.033, 1.042, 1.050, &
            1.054, 1.053, 1.052, 1.047, 1.038, &
            1.028, 1.016, 1.002, 0.989, 0.965, &
            0.941, 0.929, 0.929, 0.929, 0.929  &
         /)
  
    ! get indices between which to interpolate the Br value
    th_index = int(theta) ! afronden op het interger deel van het getal !
    IF (th_index .lt. 16) th_index = 16
    IF (th_index .ge. 59) th_index = 59
    
    ! interpolate using table ...
    Br = br_table(th_index) + (theta-th_index)* &
         (br_table(th_index+1)-br_table(th_index))
    
    ! the code given in Ads thesis does not do this interpolation!!!
    !  Br = br_table(th_index)
    
    ! for production of the look-up-table, there is no difference !!!
    ! but when using the cmod function directly, inbetween the gridpoints
    ! of the LUT, the results will differ
    
    return
  end function Get_Br_from_Look_Up_Table
  !--------------------------------
  !  #]
  function calc_sigma0_cmod5(v,phi,theta,calc_B0) RESULT(CMOD5)
  !  #[
    !-----------------------------------------------------
    ! *CMOD5* - GEOPHYSICAL MODEL FUNCTION FOR THE ERS C-BAND SCATTEROMETER
    !
    !     A. STOFFELEN              MAY  1991 ECMWF  CMOD4
    !     A. STOFFELEN, S. DE HAAN  DEC  2001 KNMI   CMOD5 PROTOTYPE
    !     H. HERSBACH               JUNE 2002 ECMWF  COMPLETE REVISION
    !     J. de Kloe                JULI 2003 KNMI,  rewritten in fortan90
    !
    !**   PURPOSE
    !     -------
    !     SIMULATION OF BACKSCATTER GIVEN A WIND VECTOR AND ICIDENCE ANGLE.
    !     CMOD5 IS TO BE USED IN AN INVERSION ROUTINE IN ORDER TO OBTAIN
    !     WIND VECTORS FROM BACKSCATTER TRIPLETS.
    !
    !**   INTERFACE
    !     ---------
    !     CMOD5(v, phi, theta)
    !         inputs:
    !              v     in [m/s] wind velocity (always >= 0)
    !              phi   in [deg] angle between azimuth and wind direction
    !                    (= D - AZM)
    !              theta in [deg] incidence angle
    !         output:
    !              CMOD5 NORMALIZED BACKSCATTER            (LINEAR)
    !---------------------------------------------------------------------
    
    ! code gechecked 3-7-2003 met de versie in Technical Memorandom 395
    ! (jan. 2003) van ECMWF door Hans Hersbach, en identiek bevonden
    ! (en tevens getest mbv het bijgeleverde testscript/programma)
    ! 9-7-2003: lijst met parameters gechecked en aangepast aan de nieuwe
    ! getallen in het Technical Memorandom 395.
    
    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6
    real(l_), parameter :: C(28) = &
            (/ -0.6880, -0.793, 0.338, -0.173, 0.00,  0.004, 0.111, &
                0.0162,  6.340, 2.570, -2.180, 0.40, -0.600, 0.045, &
                0.0070,  0.330, 0.012, 22.000, 1.95,  3.000, 8.390, &
               -3.4400,  1.360, 5.350,  1.990, 0.29,  3.800, 1.530   /)

    real(l_) :: CMOD5, v, phi, theta
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S !, AL
    real(l_) :: D1, D2, V0, V2

    logical,optional :: calc_B0

    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
    
    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error) 
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))
    
    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00
    
    X  = (theta - THETM) / THETHR
    XX = X*X
    
    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X
    
    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X
    
    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
    
    B0=(A3**GAM)*10.**(A0+A1*V)

    IF (present(calc_B0)) THEN
      IF (calc_B0) THEN
         CMOD5 = B0
         return
      END IF
    END IF

    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
    
    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X
    
    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
    
    B2 = (-D1+D2*V2)*EXP(-V2)
    
    !  CMOD5: COMBINE THE THREE FOURIER TERMS
    
    CMOD5 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW
    
    RETURN
  END FUNCTION calc_sigma0_cmod5
  !--------------------------------
  !  #]
  function calc_sigma0_cmod5_5(v,phi,theta,calc_B0) RESULT(CMOD5_5)
  !  #[
    !-----------------------------------------------------
    ! *CMOD5_5* - GEOPHYSICAL MODEL FUNCTION FOR THE ERS AND ASCAT C-BAND SCATTEROMETERS
    !
    !     A. STOFFELEN              MAY  1991 ECMWF  CMOD4
    !     A. STOFFELEN, S. DE HAAN  DEC  2001 KNMI   CMOD5 PROTOTYPE
    !     H. HERSBACH               JUNE 2002 ECMWF  COMPLETE REVISION
    !     J. de Kloe                JULI 2003 KNMI,  rewritten in fortan90
    !     J. Verspeek              APRIL 2007 KNMI,  CMOD5 with v=v-0.5m/s
    !
    !**   PURPOSE
    !     -------
    !     SIMULATION OF BACKSCATTER GIVEN A WIND VECTOR AND ICIDENCE ANGLE.
    !     CMOD5_5 IS TO BE USED IN AN INVERSION ROUTINE IN ORDER TO OBTAIN
    !     WIND VECTORS FROM BACKSCATTER TRIPLETS.
    !
    !**   INTERFACE
    !     ---------
    !     CMOD5_5(v, phi, theta)
    !         inputs:
    !              v     in [m/s] wind velocity (always >= 0)
    !              phi   in [deg] angle between azimuth and wind direction
    !                    (= D - AZM)
    !              theta in [deg] incidence angle
    !         output:
    !              CMOD5_5 NORMALIZED BACKSCATTER            (LINEAR)
    !---------------------------------------------------------------------
    
    ! code gechecked 3-7-2003 met de versie in Technical Memorandom 395
    ! (jan. 2003) van ECMWF door Hans Hersbach, en identiek bevonden
    ! (en tevens getest mbv het bijgeleverde testscript/programma)
    ! 9-7-2003: lijst met parameters gechecked en aangepast aan de nieuwe
    ! getallen in het Technical Memorandom 395.
    
    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6
    real(l_), parameter :: C(28) = &
            (/ -0.6880, -0.793, 0.338, -0.173, 0.00,  0.004, 0.111, &
                0.0162,  6.340, 2.570, -2.180, 0.40, -0.600, 0.045, &
                0.0070,  0.330, 0.012, 22.000, 1.95,  3.000, 8.390, &
               -3.4400,  1.360, 5.350,  1.990, 0.29,  3.800, 1.530   /)

    real(l_) :: CMOD5_5, v, phi, theta
    real(l_) :: v_org
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S !, AL
    real(l_) :: D1, D2, V0, V2

    logical,optional :: calc_B0

    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
    
    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error) 
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))


    ! Adapted wind speed:
    v_org = v
    if (v .gt. 0.8) then
      v = v - 0.5
    else
      v = v * 0.375
    endif

    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00
    
    X  = (theta - THETM) / THETHR
    XX = X*X
    
    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X
    
    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X
    
    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
    
    B0=(A3**GAM)*10.**(A0+A1*V)

    IF (present(calc_B0)) THEN
      IF (calc_B0) THEN
         CMOD5_5 = B0
         v = v_org
         return
      END IF
    END IF

    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
    
    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X
    
    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
    
    B2 = (-D1+D2*V2)*EXP(-V2)
    
    !  CMOD5_5: COMBINE THE THREE FOURIER TERMS
    
    CMOD5_5 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW

    v = v_org
    
    RETURN
  END FUNCTION calc_sigma0_cmod5_5
  !--------------------------------
  !  #]
  function calc_sigma0_cmod5_n(v,phi,theta,calc_B0) RESULT(CMOD5_N)
  !  #[
    !-----------------------------------------------------
    ! *CMOD5_N* - GEOPHYSICAL MODEL FUNCTION FOR THE ERS AND ASCAT C-BAND SCATTEROMETERS
    !
    !     A. STOFFELEN              MAY  1991 ECMWF  CMOD4
    !     A. STOFFELEN, S. DE HAAN  DEC  2001 KNMI   CMOD5 PROTOTYPE
    !     H. HERSBACH               JUNE 2002 ECMWF  COMPLETE REVISION
    !     J. de Kloe                JULI 2003 KNMI,  rewritten in fortan90
    !     A. Verhoef                JAN  2008 KNMI,  CMOD5 for neutral winds
    !
    !**   PURPOSE
    !     -------
    !     SIMULATION OF BACKSCATTER GIVEN A WIND VECTOR AND ICIDENCE ANGLE.
    !     CMOD5_N IS TO BE USED IN AN INVERSION ROUTINE IN ORDER TO OBTAIN
    !     WIND VECTORS FROM BACKSCATTER TRIPLETS.
    !
    !**   INTERFACE
    !     ---------
    !     CMOD5_N(v, phi, theta)
    !         inputs:
    !              v     in [m/s] wind velocity (always >= 0)
    !              phi   in [deg] angle between azimuth and wind direction
    !                    (= D - AZM)
    !              theta in [deg] incidence angle
    !         output:
    !              CMOD5_N NORMALIZED BACKSCATTER (LINEAR)
    !---------------------------------------------------------------------

    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6

    ! Coefficients obtained from Hans Hersbach by Email on 5 Feb 2008
    real(l_), parameter :: C(28) = &
            (/ -0.6878, -0.7957,  0.3380, -0.1728, 0.0000,  0.0040, 0.1103, &
                0.0159,  6.7329,  2.7713, -2.2885, 0.4971, -0.7250, 0.0450, &
                0.0066,  0.3222,  0.0120, 22.7000, 2.0813,  3.0000, 8.3659, &
               -3.3428,  1.3236,  6.2437,  2.3893, 0.3249,  4.1590, 1.6930/)

    real(l_) :: CMOD5_N, v, phi, theta
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S
    real(l_) :: D1, D2, V0, V2

    logical,optional :: calc_B0

    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)

    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error)
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))

    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00

    X  = (theta - THETM) / THETHR
    XX = X*X

    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X

    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X

    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))

    B0=(A3**GAM)*10.**(A0+A1*V)

    IF (present(calc_B0)) THEN
      IF (calc_B0) THEN
         CMOD5_N = B0
         return
      END IF
    END IF

    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)

    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X

    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN

    B2 = (-D1+D2*V2)*EXP(-V2)

    !  CMOD5_N: COMBINE THE THREE FOURIER TERMS

    CMOD5_N = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW

    RETURN
  end function calc_sigma0_cmod5_n
  !--------------------------------
  !  #]
  function calcPoly(x, n, coef) result(y)
  !  #[
  !**********************************************************************
  ! Function to calculate y as n-th order polynomial in x.
  !
  ! Author  : Jeroen Verspeek
  !
  ! Input   :
  !           x              -
  !           n              - order of the polynomial
  !           coef           - polynomial coefficients a(0:n)
  !
  ! Output  : y              - y=a(0)+a(1)*x+a(2)*x**2+...a(n)*x**n 
  !**********************************************************************
  real, intent(in) :: x
  integer, intent(in) :: n
  integer :: i
  real, dimension(0:n), intent(in) :: coef
  real :: y
  y = 0.
  do i = 0, n
    y = y+coef(i)*x**i
  enddo
  end function calcPoly
  !  #]
  function inc_B0_corr(theta) result(inc_B0_correction)
  !  #[
  !**********************************************************************
  ! Routine to add a correction to the B0 value
  ! as a polynomial function of the incidence angle.
  ! 
  ! Author  : Jeroen Verspeek
  !
  ! Input : 
  !   theta       - incidence angle (degrees)
  !           
  ! Output  : 
  !   inc_B0_correction- B0 correction (linear)
  !
  ! Notes :  
  ! 1. Used in CMOD6
  !**********************************************************************
    ! Valid for ASCAT incidence angle range:
    ! $NB/ASCAT_GMF_CMOD6/fit_cmod/output_NOC_702/a_coefs.out:
    !!real, parameter :: a_coefs(0:3) = &
    !!(/ 5.7236425879, -0.4226930560, 0.0105605079, -0.0000864832 /)

    ! Valid for combined ERS and ASCAT incidence angle range:
    ! $NB/ASCAT_ERS_GMF_CMOD6/fit_cmod/output_NOC_702/a_coefs.out:
    real, parameter :: a_coefs(0:5) = &
    &(/ 1.00557711e-02, 2.63968952e-02, -1.36487705e-03,&
    &   2.33507248e-05, 1.20736387e-07, -4.60930473e-09 /)

    real, intent(in) :: theta
    integer :: n
    real :: inc_B0_correction, inc, log_B0_correction

    inc = theta
    n = size(a_coefs)-1 ! order of polynomial
    log_B0_correction = calcPoly(inc, n, a_coefs)
    inc_B0_correction = trnsfrm(log_B0_correction, 'log_2_lin')
  end function inc_B0_corr
  !--------------------------------
  !  #]
  function calc_sigma0_cmod6(v,phi,theta,calc_B0) RESULT(CMOD6)
  !  #[
    !-----------------------------------------------------
    ! *CMOD6* - GEOPHYSICAL MODEL FUNCTION FOR THE ERS AND ASCAT C-BAND SCATTEROMETERS
    !
    !     A. STOFFELEN              MAY  1991 ECMWF  CMOD4
    !     A. STOFFELEN, S. DE HAAN  DEC  2001 KNMI   CMOD5 PROTOTYPE
    !     H. HERSBACH               JUNE 2002 ECMWF  COMPLETE REVISION
    !     J. de Kloe                JULI 2003 KNMI,  rewritten in fortan90
    !     A. Verhoef                JAN  2008 KNMI,  CMOD5 for neutral winds
    !     J. Verspeek               AUG  2009 KNMI,  CMOD6 experimental,
    !                                                based on CMOD5n
    !
    !**   PURPOSE
    !     -------
    !     SIMULATION OF BACKSCATTER GIVEN A WIND VECTOR AND ICIDENCE ANGLE.
    !     CMOD6 IS TO BE USED IN AN INVERSION ROUTINE IN ORDER TO OBTAIN
    !     WIND VECTORS FROM BACKSCATTER TRIPLETS.
    !
    !**   INTERFACE
    !     ---------
    !     CMOD6(v, phi, theta)
    !         inputs:
    !              v     in [m/s] wind velocity (always >= 0)
    !              phi   in [deg] angle between azimuth and wind direction
    !                    (= D - AZM)
    !              theta in [deg] incidence angle
    !         output:
    !              CMOD6 NORMALIZED BACKSCATTER (LINEAR)
    !---------------------------------------------------------------------

    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6

    ! Coefficients obtained from Hans Hersbach by Email on 5 Feb 2008
    real(l_), parameter :: C(28) = &
            (/ -0.6878, -0.7957,  0.3380, -0.1728, 0.0000,  0.0040, 0.1103, &
                0.0159,  6.7329,  2.7713, -2.2885, 0.4971, -0.7250, 0.0450, &
                0.0066,  0.3222,  0.0120, 22.7000, 2.0813,  3.0000, 8.3659, &
               -3.3428,  1.3236,  6.2437,  2.3893, 0.3249,  4.1590, 1.6930/)

    real(l_) :: CMOD6, v, phi, theta
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S
    real(l_) :: D1, D2, V0, V2

    logical,optional :: calc_B0

    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)

    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error)
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))

    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00

    X  = (theta - THETM) / THETHR
    XX = X*X

    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X

    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X

    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))

    B0=(A3**GAM)*10.**(A0+A1*V)

    ! Add correction to the B0:
    ! An addition in log-space corresponds to a multiplication in lin-space:  
    B0 = B0*inc_B0_corr(theta)
    !

    IF (present(calc_B0)) THEN
      IF (calc_B0) THEN
         CMOD6 = B0
         return
      END IF
    END IF

    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)

    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE

    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X

    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN

    B2 = (-D1+D2*V2)*EXP(-V2)

    !  CMOD6: COMBINE THE THREE FOURIER TERMS

    CMOD6 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW

    RETURN
  END FUNCTION calc_sigma0_cmod6
  !--------------------------------
  !  #]
  subroutine calc_B_coefs_cmod4(v, phi, theta, sigma_0, B_coef)
  !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD5
  !
  ! Author  : Jeroen Verspeek
  !
  ! Version : 2004-04-26
  !
  ! Input   : 
  !           v        - wind speed (m/s)
  !           phi      - azimuth angle (degrees)
  !           theta    - incidence angle (degrees)
  !           
  ! Output  : 
  !           sigma_0  - sigma_naught value (linear scale)
  !           B_coef   - B0, B1, B2 coefficients
  !
  ! Notes :  
  ! 1. Identical to function calc_sigma0_cmod4, but B coefficients are 
  !    output now.
  !**********************************************************************
  !  real(l_) :: f1 ! ,Get_Br_from_Look_Up_Table
    real(l_) :: v, phi, theta, sigma_0
    real(l_) :: B0, B1, B2, B3, Br
    real(l_), dimension(0:2), intent(out) :: B_coef
    real(l_) :: f2
    real(l_) :: alfa, gamma, beta
    real(l_) :: p0, p1, p2, x
    real(l_), parameter :: c1  = -2.301523, c2  = -1.632686, c3  =  0.761210, &
                           c4  =  1.156619, c5  =  0.595955, c6  = -0.293819, &
                           c7  = -1.015244, c8  =  0.342175, c9  = -0.500786, &
                           c10 =  0.014430, c11 =  0.002484, c12 =  0.074450, &
                           c13 =  0.004023, c14 =  0.148810, c15 =  0.089286, &
                           c16 = -0.006667, c17 =  3.000000, c18 =-10.000000
  
    Br = Get_Br_from_Look_Up_Table(theta)
    x  = (theta-40.0)/25.0
  
    ! voor f1 zie functie definitie
    f2 = tanh( 2.5*(x+0.35)) - 0.61*(x+0.35)
    
    p0 = 1.0
    p1 = x
    p2 = (3.0*x*x-1.0)/2.0
    
    alfa  = c1*p0 + c2*p1 + c3*p2
    gamma = c4*p0 + c5*p1 + c6*p2
    beta  = c7*p0 + c8*p1 + c9*p2
    
    B0 = Br * 10**(alfa + gamma*f1(v+beta))
    B1 = c10*p0 +  c11*v +  (c12*p0 + c13*v)*f2
    B2 = c14*p0 +  c15*(1.0+p1)*v
    B3 = 0.42*(1.0 + c16*(c17+x)*(c18+v))
    
    sigma_0 = B0 * (1.0 + B1*cos(phi*deg2rad) + &
         B3*tanh(B2)*cos(2.0*phi*deg2rad ))**1.6
  
    B_coef(0) = B0
    B_coef(1) = B1
    B_coef(2) = B3*tanh(B2)
    
    return
  end subroutine calc_B_coefs_cmod4
    !---------------------------------
    !  #]
  subroutine calc_B_coefs_cmod5(v, phi, theta, CMOD5, B_coef)
    !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD5
  !
  ! Author  : Jeroen Verspeek
  !
  ! Version : 2004-04-26
  !
  ! Input   : 
  !           v        - wind speed (m/s)
  !           phi      - azimuth angle (degrees)
  !           theta    - incidence angle (degrees)
  !           
  ! Output  : 
  !           CMOD5    - sigma_naught value (linear scale)
  !           B_coef   - B0, B1, B2 coefficients
  !
  ! Notes :  
  ! 1. Identical to function calc_sigma0_cmod5, but B coefficients are 
  !    output now.
  !**********************************************************************
  
    implicit none
    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6
    real(l_), parameter :: C(28) = &
            (/ -0.6880, -0.793, 0.338, -0.173, 0.00,  0.004, 0.111, &
                0.0162,  6.340, 2.570, -2.180, 0.40, -0.600, 0.045, &
                0.0070,  0.330, 0.012, 22.000, 1.95,  3.000, 8.390, &
               -3.4400,  1.360, 5.350,  1.990, 0.29,  3.800, 1.530   /)
  
    real(l_) :: CMOD5, v, phi, theta
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S
    real(l_) :: D1, D2, V0, V2
  
    real(l_), dimension(0:2), intent(out) :: B_coef
  
    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
    
    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error) 
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))
    
    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00
    
    X  = (theta - THETM) / THETHR
    XX = X*X
    
    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X
    
    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X
    
    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
    
    B0=(A3**GAM)*10.**(A0+A1*V)
  
    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
    
    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X
    
    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
    
    B2 = (-D1+D2*V2)*EXP(-V2)
    
    !  CMOD5: COMBINE THE THREE FOURIER TERMS
    
    CMOD5 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW
  
    B_coef(0) = B0
    B_coef(1) = B1
    B_coef(2) = B2
  
    RETURN
  end subroutine calc_B_coefs_cmod5
    !---------------------------------
    !  #]
  subroutine calc_B_coefs_cmod5_5(v, phi, theta, CMOD5_5, B_coef)
    !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD5_5
  !
  ! Author  : Jeroen Verspeek
  !
  ! Version : 2004-04-26
  !
  ! Input   : 
  !           v        - wind speed (m/s)
  !           phi      - azimuth angle (degrees)
  !           theta    - incidence angle (degrees)
  !           
  ! Output  : 
  !           CMOD5_5    - sigma_naught value (linear scale)
  !           B_coef   - B0, B1, B2 coefficients
  !
  ! Notes :  
  ! 1. Identical to function calc_sigma0_cmod5_5, but B coefficients are 
  !    output now.
  !**********************************************************************
  
    implicit none
    real(l_), parameter :: DTOR   = 57.29577951
    real(l_), parameter :: THETM  = 40.
    real(l_), parameter :: THETHR = 25.
    real(l_), parameter :: ZPOW   = 1.6
    real(l_), parameter :: C(28) = &
            (/ -0.6880, -0.793, 0.338, -0.173, 0.00,  0.004, 0.111, &
                0.0162,  6.340, 2.570, -2.180, 0.40, -0.600, 0.045, &
                0.0070,  0.330, 0.012, 22.000, 1.95,  3.000, 8.390, &
               -3.4400,  1.360, 5.350,  1.990, 0.29,  3.800, 1.530   /)
  
    real(l_) :: CMOD5_5, v, phi, theta
    real(l_) :: v_org
    real(l_) :: A0, A1, A2, A3, B0, B1, B2
    real(l_) :: FI, CSFI, CS2FI, X, XX
    real(l_) :: GAM, S0, S
    ! real(l_) :: AL
    real(l_) :: D1, D2, V0, V2
  
    real(l_), dimension(0:2), intent(out) :: B_coef
  
    real(l_), save :: Y0 = C(19)
    real(l_), save :: PN = C(20)
    real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
    
    ! WARNING: in an initialisation constant expression, exponents are
    !          required to be of integer type, so the next line is
    !          illegal fortran90 (SGI allows this, but Linux gives an error) 
    !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
    ! so I replaced c(20) by the constant value of "3"
    real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))
  
    ! Adapted wind speed:
    v_org = v
    if (v .gt. 0.8) then
      v = v - 0.5
    else
      v = v * 0.375
    endif
  
    !  ANGLES
    FI=phi/DTOR
    CSFI = COS(FI)
    CS2FI= 2.00 * CSFI * CSFI - 1.00
    
    X  = (theta - THETM) / THETHR
    XX = X*X
    
    ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
    A1 =C( 5)+C( 6)*X
    A2 =C( 7)+C( 8)*X
    
    GAM=C( 9)+C(10)*X+C(11)*XX
    S0 =C(12)+C(13)*X
    
    S = A2*V
    A3=1./(1.+EXP(-MAX(S,S0)))
    IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
    
    B0=(A3**GAM)*10.**(A0+A1*V)
  
    !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
    B1 = C(14)*(1.+X)- B1
    B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
    
    !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
    
    V0 = C(21) + C(22)*X + C(23)*XX
    D1 = C(24) + C(25)*X + C(26)*XX
    D2 = C(27) + C(28)*X
    
    V2 = (V/V0+1.)
    IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
    
    B2 = (-D1+D2*V2)*EXP(-V2)
    
    !  CMOD5_5: COMBINE THE THREE FOURIER TERMS
    
    CMOD5_5 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW
  
    B_coef(0) = B0
    B_coef(1) = B1
    B_coef(2) = B2
  
    v = v_org
  
    RETURN
  end subroutine calc_B_coefs_cmod5_5
    !---------------------------------
    !  #]
  subroutine calc_B_coefs_cmod5_n(v, phi, theta, CMOD5_N, B_coef)
    !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD5_n
  !
  ! Author  : Jeroen Verspeek
  !
  ! Version : 2008-02-15
  !
  ! Input   : 
  !           v        - wind speed (m/s)
  !           phi      - azimuth angle (degrees)
  !           theta    - incidence angle (degrees)
  !           
  ! Output  : 
  !           CMOD5_N  - CMOD5_N NORMALIZED BACKSCATTER (LINEAR)
  !           B_coef   - B0, B1, B2 coefficients
  !
  ! Notes :  
  ! 1. Identical to function calc_sigma0_cmod5_n, but B coefficients are 
  !    output now.
  !**********************************************************************
      real(l_), parameter :: DTOR   = 57.29577951
      real(l_), parameter :: THETM  = 40.
      real(l_), parameter :: THETHR = 25.
      real(l_), parameter :: ZPOW   = 1.6
  
      ! Coefficients obtained from Hans Hersbach by Email on 5 Feb 2008
      real(l_), parameter :: C(28) = &
              (/ -0.6878, -0.7957,  0.3380, -0.1728, 0.0000,  0.0040, 0.1103, &
                  0.0159,  6.7329,  2.7713, -2.2885, 0.4971, -0.7250, 0.0450, &
                  0.0066,  0.3222,  0.0120, 22.7000, 2.0813,  3.0000, 8.3659, &
                 -3.3428,  1.3236,  6.2437,  2.3893, 0.3249,  4.1590, 1.6930/)
  
      real(l_) :: CMOD5_N, v, phi, theta
      real(l_) :: A0, A1, A2, A3, B0, B1, B2
      real(l_) :: FI, CSFI, CS2FI, X, XX
      real(l_) :: GAM, S0, S
      real(l_) :: D1, D2, V0, V2
  
      real(l_), dimension(0:2), intent(out) :: B_coef
  
      real(l_), save :: Y0 = C(19)
      real(l_), save :: PN = C(20)
      real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
  
      ! WARNING: in an initialisation constant expression, exponents are
      !          required to be of integer type, so the next line is
      !          illegal fortran90 (SGI allows this, but Linux gives an error)
      !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
      ! so I replaced c(20) by the constant value of "3"
      real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))
  
      !  ANGLES
      FI=phi/DTOR
      CSFI = COS(FI)
      CS2FI= 2.00 * CSFI * CSFI - 1.00
  
      X  = (theta - THETM) / THETHR
      XX = X*X
  
      ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
      A1 =C( 5)+C( 6)*X
      A2 =C( 7)+C( 8)*X
  
      GAM=C( 9)+C(10)*X+C(11)*XX
      S0 =C(12)+C(13)*X
  
      S = A2*V
      A3=1./(1.+EXP(-MAX(S,S0)))
      IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
  
      B0=(A3**GAM)*10.**(A0+A1*V)
  
      !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
      B1 = C(14)*(1.+X)- B1
      B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
  
      !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      V0 = C(21) + C(22)*X + C(23)*XX
      D1 = C(24) + C(25)*X + C(26)*XX
      D2 = C(27) + C(28)*X
  
      V2 = (V/V0+1.)
      IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
  
      B2 = (-D1+D2*V2)*EXP(-V2)
  
      !  CMOD5_N: COMBINE THE THREE FOURIER TERMS
  
      CMOD5_N = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW
  
      B_coef(0) = B0; B_coef(1) = B1; B_coef(2) = B2
  
      RETURN
  end subroutine calc_B_coefs_cmod5_n
    !---------------------------------
    !  #]
  subroutine calc_B_coefs_cmod6(v, phi, theta, CMOD6, B_coef)
    !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD6
  !
  ! Author  : Jeroen Verspeek
  !
  ! Version : 2011-03-25
  !
  ! Input   : 
  !           v        - wind speed (m/s)
  !           phi      - azimuth angle (degrees)
  !           theta    - incidence angle (degrees)
  !           
  ! Output  : 
  !           CMOD6  - CMOD6 NORMALIZED BACKSCATTER (LINEAR)
  !           B_coef   - B0, B1, B2 coefficients
  !
  ! Notes :  
  ! 1. Identical to function calc_sigma0_cmod6, but B coefficients are 
  !    output now.
  !**********************************************************************
      real(l_), parameter :: DTOR   = 57.29577951
      real(l_), parameter :: THETM  = 40.
      real(l_), parameter :: THETHR = 25.
      real(l_), parameter :: ZPOW   = 1.6
  
      ! Coefficients obtained from Hans Hersbach by Email on 5 Feb 2008
      real(l_), parameter :: C(28) = &
              (/ -0.6878, -0.7957,  0.3380, -0.1728, 0.0000,  0.0040, 0.1103, &
                  0.0159,  6.7329,  2.7713, -2.2885, 0.4971, -0.7250, 0.0450, &
                  0.0066,  0.3222,  0.0120, 22.7000, 2.0813,  3.0000, 8.3659, &
                 -3.3428,  1.3236,  6.2437,  2.3893, 0.3249,  4.1590, 1.6930/)
  
      real(l_) :: CMOD6, v, phi, theta
      real(l_) :: A0, A1, A2, A3, B0, B1, B2
      real(l_) :: FI, CSFI, CS2FI, X, XX
      real(l_) :: GAM, S0, S
      real(l_) :: D1, D2, V0, V2
  
      real(l_), dimension(0:2), intent(out) :: B_coef
  
      real(l_), save :: Y0 = C(19)
      real(l_), save :: PN = C(20)
      real(l_), save :: A  = C(19)-(C(19)-1.)/C(20)
  
      ! WARNING: in an initialisation constant expression, exponents are
      !          required to be of integer type, so the next line is
      !          illegal fortran90 (SGI allows this, but Linux gives an error)
      !  real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(C(20)-1.))
      ! so I replaced c(20) by the constant value of "3"
      real(l_), save :: B  = 1./(C(20)*(C(19)-1.)**(3-1))
  
      !  ANGLES
      FI=phi/DTOR
      CSFI = COS(FI)
      CS2FI= 2.00 * CSFI * CSFI - 1.00
  
      X  = (theta - THETM) / THETHR
      XX = X*X
  
      ! B0: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      A0 =C( 1)+C( 2)*X+C( 3)*XX+C( 4)*X*XX
      A1 =C( 5)+C( 6)*X
      A2 =C( 7)+C( 8)*X
  
      GAM=C( 9)+C(10)*X+C(11)*XX
      S0 =C(12)+C(13)*X
  
      S = A2*V
      A3=1./(1.+EXP(-MAX(S,S0)))
      IF (S.LT.S0) A3=A3*(S/S0)**( S0*(1.- A3))
  
      B0=(A3**GAM)*10.**(A0+A1*V)
  
      ! Add correction to the B0:
      ! An addition in log-space corresponds to a multiplication in lin-space:  
      B0 = B0*inc_B0_corr(theta)
      !

      !  B1: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      B1 = C(15)*V*(0.5+X-TANH(4.*(X+C(16)+C(17)*V)))
      B1 = C(14)*(1.+X)- B1
      B1 = B1/(EXP( 0.34*(V-C(18)) )+1.)
  
      !  B2: FUNCTION OF WIND SPEED AND INCIDENCE ANGLE
  
      V0 = C(21) + C(22)*X + C(23)*XX
      D1 = C(24) + C(25)*X + C(26)*XX
      D2 = C(27) + C(28)*X
  
      V2 = (V/V0+1.)
      IF   (V2.LT.Y0) V2 = A+B*(V2-1.)**PN
  
      B2 = (-D1+D2*V2)*EXP(-V2)
  
      !  CMOD6: COMBINE THE THREE FOURIER TERMS
  
      CMOD6 = B0*(1.0+B1*CSFI+B2*CS2FI)**ZPOW
  
      B_coef(0) = B0; B_coef(1) = B1; B_coef(2) = B2
  
      RETURN
  end subroutine calc_B_coefs_cmod6
    !---------------------------------
    !  #]
  subroutine calc_B_coefs_cmod(cmod_version, v, phi, theta, &
                               & sigma_0, B_coef, dbgLevel)
    !  #[
  !**********************************************************************
  ! Routine to calculate the B coefficients from CMOD
  !
  ! Author  : Jeroen Verspeek
  !
  ! Input   : 
  !  cmod_version - the version of CMOD
  !  v            - wind speed (m/s)
  !  phi          - azimuth angle (degrees)
  !  theta        - incidence angle (degrees)
  !           
  ! Output  : 
  !  sigma_0      - CMOD NORMALIZED BACKSCATTER (LINEAR)
  !  B_coef       - B0, B1, B2 coefficients
  !**********************************************************************
    ! Interface parameters:
    character(len=3),  intent(in)         :: cmod_version
    real(l_), intent(in)                  :: v, phi, theta
    real(l_), intent(out)                 :: sigma_0
    real(l_), dimension(0:2), intent(out) :: B_coef
    integer, optional, intent(in)         :: dbgLevel
  
    if (present(dbgLevel)) then; if (dbgLevel >= 5) then
      write(6, *) 'calc_B_coefs_cmod - Entering routine...'
    endif; endif
  
    sigma_0 = 0.
  
    select case(cmod_version)
    case('4')
      call calc_B_coefs_cmod4(V, phi, theta, sigma_0, B_coef)
    case('5')
      call calc_B_coefs_cmod5(V, phi, theta, sigma_0, B_coef)
    case('55')
      call calc_B_coefs_cmod5_5(V, phi, theta, sigma_0, B_coef)
    case('5n')
      call calc_B_coefs_cmod5_n(V, phi, theta, sigma_0, B_coef)
    case('6')
      call calc_B_coefs_cmod6(V, phi, theta, sigma_0, B_coef)
    case default
      write(6, *) 'calc_B_coefs_cmod ERROR - Unknown cmod version.'
      write(6, *) ' cmod_version: ', cmod_version
      write(6, *)
      stop
    end select
  
    if (present(dbgLevel)) then; if (dbgLevel >= 5) then
      write(6, *) 'calc_B_coefs_cmod - Exiting routine.'
    endif; endif
  
  end subroutine calc_B_coefs_cmod
     !---------------------------------
     !  #]
  !---------------------------------
  function calc_cdop(u10,wdir,inc,pol) result(cdop)
    !  #[ calculate doppler effect
    !     using a simplified scalar version of cdop.py
    !
    ! calculate CDOP function (Doppler effect for C-band)
    !
    ! Inputs (scalar only):
    ! ==>u10: wind speed at 10 m standard height in [m/s]
    ! ==>wdir: wind direction rel. to antenna look direction in [degrees]
    ! ==>inc (theta): incidence angle of radar beam in [degrees]
    ! ==>pol: polarisation switch (string: 'VV' or 'HH')
    !
    ! Output:
    ! ==>frequency shift in Hz
    ! (see explanation by Alexis Mouche by email to J. de Kloe, 15-Jun-2012)

    ! interface
    real(l_), intent(in)  :: u10,wdir,inc
    integer , intent(in)  :: pol
    real(l_)              :: cdop ! result

    ! local variables
    real(l_), dimension(3)    :: b1, w1
    real(l_), dimension(11)   :: b2, w3, part1
    real(l_), dimension(11,3) :: w2
    real(l_), dimension(3)    :: inputs
    real(l_) :: b3, b4, w4, wdir_clipped

    ! init to zero to suppress compiler warnings about 
    ! possible uninitialized use of variables
    b1(:) = 0.
    b2(:) = 0.
    b3 = 0.
    b4 = 0.
    w1(:) = 0.
    w2(:,:) = 0.
    w3(:) = 0.
    w4 = 0.
    inputs(:) = 0.
    part1(:) = 0.

    ! Get coefficients (W=weights and B=biases)
    ! (coefficient names in mouche2012 are given)
    select case (pol)
    case(c_vv_dop)
       ! lambda[0:2,1]
       b1 = (/-0.343935744939, 0.108823529412, 0.15/)
       ! lambda[0:2,0]
       w1 = (/0.028213254683, 0.0411764705882, .00388888888889/)
       ! omega[i,0]
       b2 = (/14.5077150927, -11.4312028555,  1.28692747109,&
              -1.19498666071,  1.778908726,  11.8880215573,&
               1.70176062351, 24.7941267067, -8.18756617111,&
               1.32555779345, -9.06560116738 /)

       ! omega[i,[3,2,1]]
       w2( 1,:) = (/19.7873046673,  22.2237414308,    1.27887019276  /)
       w2( 2,:) = (/ 2.910815875,   -3.63395681095,  16.4242081101   /)
       w2( 3,:) = (/ 1.03269004609,  0.403986575614,  0.325018607578 /)
       w2( 4,:) = (/ 3.17100261168,  4.47461213024,   0.969975702316 /)
       w2( 5,:) = (/-3.80611082432, -6.91334859293,  -0.0162650756459/)
       w2( 6,:) = (/4.09854466913,  -1.64290475596, -13.4031862615   /)
       w2( 7,:) = (/0.484338480824, -1.30503436654,  -6.04613303002  /)
       w2( 8,:) = (/-11.1000239122, 15.993470129,    23.2186869807   /)
       w2( 9,:) = (/-0.577883159569, 0.801977535733,  6.13874672206  /)
       w2(10,:) = (/0.61008842868,  -0.5009830671,   -4.42736737765  /)
       w2(11,:) = (/-1.94654022702,  1.31351068862,   8.94943709074  /)

       ! gamma[0]
       b3 = 4.07777876994
       ! gamma[1:11]
       w3 = (/ 7.34881153553,  0.487879873912, -22.167664703,  &
               7.01176085914,  3.57021820094,   -7.05653415486,&
              -8.82147148713,  5.35079872715,   93.627037987,  &
              13.9420969201, -34.4032326496 /)
       ! beta
       b4 = -52.2644487109
       ! alpha
       w4 = 111.528184073
    case(c_hh_dop)
       ! lambda[0:2,1]
       b1 = (/-0.342097701547, 0.118181818182, 0.15/)
       ! lambda[0:2,0]
       w1 = (/0.0281843837385, 0.0318181818182, 0.00388888888889/)
       ! omega[i,0]
       b2 = (/ 1.30653883096, -2.77086154074,  10.6792861882, &
              -4.0429666906,  -0.172201666743, 20.4895916824, &
              28.2856865516,  -3.60143441597,  -3.53935574111,&
              -2.11695768022, -2.57805898849 /)

       ! omega[i,[3,2,1]]
       w2( 1,:) = (/-2.61087309812,  -0.973599180956, -9.07176856257 /)
       w2( 2,:) = (/-0.246776181361,  0.586523978839, -0.594867645776/)
       w2( 3,:) = (/17.9261562541,   12.9439063319,   16.9815377306  /)
       w2( 4,:) = (/ 0.595882115891,  6.20098098757,  -9.20238868219 /)
       w2( 5,:) = (/-0.993509213443,  0.301856868548, -4.12397246171 /)
       w2( 6,:) = (/15.0224985357,   17.643307099,     8.57886720397 /)
       w2( 7,:) = (/13.1833641617,   20.6983195925,  -15.1439734434  /)
       w2( 8,:) = (/ 0.656338134446,  5.79854593024,  -9.9811757434  /)
       w2( 9,:) = (/ 0.122736690257, -5.67640781126,  11.9861607453  /)
       w2(10,:) = (/ 0.691577162612,  5.95289490539, -16.0530462     /)
       w2(11,:) = (/ 1.2664066483,    0.151056851685,  7.93435940581 /)
       
       ! gamma[0]
       b3 = 2.68352095337
       ! gamma[1:11]
       w3 = (/ -8.21498722494, -94.9645431048, -17.7727420108, &
              -63.3536337981,   39.2450482271,  -6.15275352542,&
               16.5337543167,   90.1967379935,  -1.11346786284,&
              -17.57689699,      8.20219395141 /)
       ! beta
       b4 = -66.9554922921
       ! alpha
       w4 = 136.216953823
    case default
      print *,'ERROR: unknown polarisation in function cdop'
      call my_exit()
   end select
   
   ! clip wdir in [0,180]
   ! NOTE: its expensive to use acos(cos(x)) just for clipping!
   wdir_clipped = acos(cos(wdir*deg2rad))*rad2deg

   ! Make inputs as matrix
   inputs(1) = w1(1) * inc          + b1(1)
   inputs(2) = w1(2) * u10          + b1(2)
   inputs(3) = w1(3) * wdir_clipped + b1(3)
   
   ! Compute CDOP
   part1 = cdop_func_arr(my_dot_product2(w2,inputs) + b2)
   cdop = w4 * cdop_func(my_dot_product( w3,part1 ) + b3) + b4

  end function calc_cdop
    !  #]
  function cdop_func(x) result(f)
    !  #[ little helper function for calc_cdop
    real(l_), intent(in) :: x
    real(l_)             :: f ! result
    f = 1. / (1. + exp(-x))
  end function cdop_func
    !  #]
  function cdop_func_arr(x) result(f)
    !  #[ ditto, vector version
    real(l_), dimension(:),      intent(in) :: x
    real(l_), dimension(size(x))            :: f ! result
    
    ! local variable
    integer :: i

    do i=1,size(x)
       f(i) = cdop_func(x(i))
    end do

  end function cdop_func_arr
    !  #]
  function my_dot_product(x,y) result(d)
    !  #[ define a dot product of 2 vectors
    ! the intrinsic function dot_product is only available in
    ! fortran95 or above, so define our own copy here
    real(l_), dimension(:), intent(in) :: x, y
    real(l_)                           :: d ! result

    ! local variables
    integer :: i

    d = 0.0
    do i=1,size(x)
       d=d+x(i)*y(i)
    end do

  end function my_dot_product
    !  #]
  function my_dot_product2(m,v) result(d)
    !  #[ ditto, but now between matrix and vector
    real(l_), dimension(:,:), intent(in) :: m
    real(l_), dimension(:),   intent(in) :: v
    real(l_), dimension(size(m,1))       :: d ! result

    ! local variables
    integer :: i,j

    do i=1,size(m,1)
       d(i) = 0.0
       do j=1,size(m,2)
          d(i) = d(i) + m(i,j)*v(j)
       end do
    end do

  end function my_dot_product2
    !  #]
  !---------------------------------
END MODULE inversion
