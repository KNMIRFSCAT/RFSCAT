module post_inversion
!
!  #[ Description:
!
! This module contains routines for post-inversion cone distance normalisation
! and probability calculations.
! Mainly for ERS and ASCAT scatterometers.
!
!  #]
!  #[
! Current Code Owner:  Anton Verhoef
!
! Code Description:
!     Language:           Fortran 90
!     Software Standards: "European Standards for Writing and
!     Documenting Exchangeable Fortran 90 Code"
!
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             post_inversion.F90
!
!     Created on:            2006/06/28
!
!     Last Modified on:      $Date: 2016-09-28 13:36:43 +0200 (Wed, 28 Sep 2016) $
!
!     Modifications Log:
!
! $Id: post_inversion.F90 11841 2016-09-28 11:36:43Z verhoefa $
! 
!  post_inversion.F90 11835 2016-09-26 08:25:53Z vogelzan $
! 
!  post_inversion.F90 11610 2016-05-17 13:01:37Z vogelzan $
! 
!  post_inversion.F90 10425 2015-02-24 10:40:58Z vogelzan $
! 
!  post_inversion.F90 9183 2013-11-11 14:29:11Z vogelzan $
! 
!  post_inversion.F90 9179 2013-11-11 08:52:37Z vogelzan $
! 
!  post_inversion.F90 9096 2013-10-15 09:19:58Z verhoefa $
! 
!  post_inversion.F90 8818 2013-06-07 08:25:55Z vogelzan $
! 
!  post_inversion.F90 8477 2013-01-04 15:08:03Z josdekloe $
! 
!  post_inversion.F90 8162 2012-08-08 13:03:28Z josdekloe $
! 
!  post_inversion.F90 7726 2011-11-14 16:05:37Z verhoefa $
!
! Revision 1.38  2009/11/19 08:36:14  verhoefa
! Allow more than 2 solutions and only flag solutions inside cone
!
! Revision 1.37  2009/05/13 10:38:08  verhoefa
! Removed public statement since everything is public in this module
!
! Revision 1.36  2009/05/11 14:55:59  kloedej
! added optional ExpMeanMLE input parameter to the calc_probabilities()
! subroutine, to allow Quikscat style probability calculations
!
! Revision 1.35  2009/01/14 14:00:00  verspeek
!
! adapted comment on l-parameter for ASCAT
! -
!
! Revision 1.34  2008/12/29 16:15:36  verhoefa
! Removed unused use statement
!
! Revision 1.33  2008/09/30 09:14:03  verhoefa
! Removed factor 9 in calculation of QC threshold, it is in the QC tables now
!
! Revision 1.32  2008/09/08 08:50:35  verhoefa
! Compensate increasing cone distance at low winds in MLE rather than in QC threshold
!
! Revision 1.31  2008/07/15 13:42:26  verhoefa
! Adapted low wind speed QC threshold
!
! Revision 1.30  2008/07/10 13:28:50  verhoefa
! Changes resulting from QC tuning
!
! Revision 1.29  2008/05/21 07:03:17  verhoefa
! Replaced local code by call to generic function get_closest_solution
!
! Revision 1.28  2008/05/20 10:31:53  verhoefa
! Code simplification
!
! Revision 1.27  2008/05/20 09:26:58  verhoefa
! Use closest to background solution for quality control with ASCAT
!
! Revision 1.26  2008/02/27 07:10:31  verhoefa
! Fixed gfortran compiler warning
!
! Revision 1.25  2008/01/17 06:45:22  verhoefa
! Bug fix in check_wind_solutions_ers_ascat
!
! Revision 1.24  2007/07/03 13:01:24  verhoefa
! Equal solution probabilities at low wind speeds only for non-mss case
!
! Revision 1.23  2007/07/02 14:36:14  verspeek
! Adapted calc_probabilities to handle call with %nr_of_windsolutions=0
!
! Revision 1.22  2007/05/24 07:54:50  verhoefa
! Prevent core dumps when converting Kp to Zspace
!
! Revision 1.21  2007/02/27 13:59:26  verhoefa
! Use different geophysical noise tables for ERS and ASCAT
!
! Revision 1.20  2007/02/22 14:40:56  verhoefa
! Some refinements in file namings
!
! Revision 1.19  2007/02/22 07:20:43  verhoefa
! Changed LUT file names and improved error handling when reading them
!
! Revision 1.18  2007/02/21 13:41:44  portabel
! - normalise_conedist_ers_ascat: 1st-rank-based norm (for QC) and sel-rank-based
!   norm (for prob. computation) are now merged in one single norm
!   (mean_ers_25000_mle_vs_wvc.asc) and QC threshold has been adapted to account
!   for this change (node_dependent_ers_QC_threshold.asc)
! - calc_probabilities: for ERS and ASCAT winds below 4 m/s, ambiguous solutions
!   are now set to equal probability. A new argument (inv_settings) has been
!   added in the subroutine call.
!
! Revision 1.17  2006/11/10 11:03:37  verhoefa
! Corrected bug in check on valid node number
!
! Revision 1.16  2006/11/08 14:20:51  verhoefa
! Added probability to inv_output structure
!
! Revision 1.15  2006/11/03 15:42:51  verhoefa
! Removed sorting on probabilities
! Normalise MLEs with a node dependent factor which is read from a file
!
! Revision 1.14  2006/11/02 18:19:51  portabel
! - MLE node dependent norms updated in normalise_conedist_prescat_mode and
!   normalise_conedist_ers_ascat.
! - Subroutine normalise_conedist_ers_ascat: tripletQC thresholds adjusted
!   to mimic rejection rate of PRESCAT tripletQC.
! - Subroutine check_wind_solutions_ers_ascat modified to accomodate changes
!   of the minima search (LUT max sols rejected) in the inversion module (see
!   version 1.80).
!
! Revision 1.13  2006/10/20 06:45:46  verhoefa
! Undone all changes from 19 Oct 2006
!
! Revision 1.12  2006/10/19 09:15:58  verhoefa
! Sorting of wind solutions removed since it is done already in inversion
!
! Revision 1.11  2006/10/17 16:59:11  portabel
! node_dependent_norm_factor table updated in normalise_conedist_prescat_mode
! subroutine.
!
! Revision 1.10  2006/10/17 10:48:52  portabel
! New node_dependent_norm_factor for both normalise_conedist_prescat_mode
! and normalise_conedist_ers_ascat routines. The latter is not yet
! operational though. Triplet QC uses now a normalized (to 1) MLE.
!
! Revision 1.9  2006/10/13 12:48:21  verhoefa
! Changed naming of wqc flags
!
! Revision 1.8  2006/10/11 15:04:33  portabel
! wqc_ok_1sol flag updated.
!
! Revision 1.7  2006/10/10 17:17:51  portabel
! All references to the "allowed_nr_of_sol" parameter have been erased.
! ERS specific wind_quality_codes are now declared and set in here and not in the
! inversion module.
! The check_wind_solutions_ers_ascat subroutine has been updated.
! The cone distance norm is not applied beam by beam anymore. A total norm is used
! to avoid solution rank swapping due to normalization.
!
! Revision 1.5  2006/07/14 08:20:13  verhoefa
! Added subroutine calc_sort_prob (copied from awdp)
!
! Revision 1.4  2006/07/14 06:38:33  verhoefa
! Keep the sign of the cone distance in normalise_conedist_ers_ascat
!
! Revision 1.3  2006/07/12 11:11:13  verhoefa
! Extra factor of nr_of_views in cone distance normalisation and lay-out changes
!
! Revision 1.2  2006/07/03 15:11:40  verhoefa
! Moved large part of fill_wind_quality_code from inversion to post_inversion
!
! Revision 1.1  2006/06/29 08:06:14  verhoefa
! Initial version
!
!
!  #]
!  #[ modules used:

!
  use LunManager,only: get_lun
  use LunManager,only: free_lun

  use numerics,only: l_
  use numerics,only: s_
  use numerics,only: missing_indicator_real
  use numerics,only: missing_indicator_integer

  use inversion,only: inv_settings_type
  use inversion,only: inv_input_type        ! input structure for inversion
  use inversion,only: inv_output_type       ! output structure for inversion
  use inversion,only: max_nr_of_views
  use inversion,only: max_nr_of_sol
  use inversion,only: Zfactor
  use inversion,only: max_windvelocity
  use inversion,only: inversion_verbosity_error
  use inversion,only: inversion_verbosity_warn
  use inversion,only: inversion_verbosity_report
  use inversion,only: instrument_type_ers
  use inversion,only: instrument_type_ascat
  use inversion,only: wqc_ok_toomany
  use inversion,only: wqc_below_this_are_ok
  use inversion,only: wqc_notok_nosol
  use inversion,only: get_closest_solution

  use SortMod,only: GetSortIndex
!

!  #]
!
  implicit none
!
!  #[ private declarations
!

!
!  #]
!  #[ public declarations
!
  !===================================================================
  ! code definition for wind_quality_code:
  ! Most of theses flags are set in the post_inversion subroutine (for ERS).
  ! Solutions at LUT_max are discarded (currently the inversion module is
  ! them, but otherwise they are discarded in here) discardingand such WVCs
  ! flagged accordingly; then, the WVC is reduced to the 1st-rank solution
  ! and the solution (if any) which is closest to 180 deg apart and at least
  ! 90 deg apart from 1st-rank. For winds below 4 m/s a solution 180 deg apart
  ! from 1st-rank is forced if there is no solution more than 90 deg apart from
  ! 1st-rank. Then, if the resulting WVC has 2 solutions it is flagged as "OK"
  ! else (one or none) it is flagged as "NOT OK".

  ! Note that the parameters which are commented out (i.e., wqc_ok, wqc_ok_144sol,
  ! wqc_ok_toomany, wqc_below_this_are_ok, wqc_notok_nodirfound,
  ! wqc_notok_noresult and wqc_notok_nosol) are declared in the inversion module.

! integer, parameter :: wqc_ok = 0
  !    0   = ok, there are solutions found (at least 1)
  integer, parameter :: wqc_ok_lutmin = 2
  !    2   = ok, but some solutions are at min windspeed. A solution
  !          is at least 90 deg apart from 1st-rank.
  integer, parameter :: wqc_ok_lutmax = 3
  !    3   = ok, but some solutions are at max windspeed. A solution
  !          (other than LUT-max sols which are removed) is at least 90 deg
  !          apart from 1st-rank.
! integer, parameter :: wqc_ok_144sol = 4
  !    4   = ok, but get_all_winddirs switch was used, so usually all 144
  !          solutions are returned UNSORTED
! integer, parameter :: wqc_ok_toomany = 6
  !    6   = more than inv_settings%max_nr_of_solutions solutions found, none of them
  !          at LUT edges. The result is OK, although it rarely happens.
  integer, parameter :: wqc_ok_ERSexception = 7
  !    7   = Flag associated with experimental quality checks ("exp_checks")
  !          in the post_inversion routine. NOT used operationally: ERS normally
  !          only produces 2 minima, but in special cases
  !          3 (or 4?) can be found. This especially happens for cmod5
  !          (but also cmod4?).
  integer, parameter :: wqc_ok_discardedsol = 9
  !    9   = one or more solutions have been succesfully removed because
  !          MLE3 was above 40 times MLE1.
  !          Thus the nr of solutions was succesfully reduced to 2.
  integer, parameter :: wqc_ok_ERSlowspeedexception = 10
  !   10   = Flag associated with experimental quality checks ("exp_checks")
  !          in the post_inversion routine. NOT used operationally:
  !          3 or more solutions were found, but windspeed is below
  !          a given minimum (lowspeed_exception=1.0 at the moment).
  !          This is allowed since reporting the very low windspeed
  !          is more important for these cases, than the possible problem
  !          in winddirection determination. Only the 2 solutions closest
  !          to the cone are reported.
  integer, parameter :: wqc_ok_1sol = 11
  !    11  = For wind speed < 4 m/s, cases with one sol or many sols but
  !          none more than 90 deg apart from 1st rank, are set as OK. In both cases,
  !          1st-rank is kept and another solution 180 deg apart from 1st-rank is
  !          artificially generated.
  integer, parameter :: wqc_ok_toomany_lutmin = 12   ! added by Marcos
  !    12  = more than inv_settings%max_nr_of_solutions solutions & some at LUT min
  !          A solution more than 90 deg apart from 1st rank is present.
  integer, parameter :: wqc_ok_toomany_lutmax = 13   ! added by Marcos
  !    13  = more than inv_settings%max_nr_of_solutions solutions & some at LUT max
  !          A solution (other than the LUT_max sols which are removed)
  !          more than 90 deg apart from 1st rank is present.
! integer, parameter :: wqc_below_this_are_ok = 14
  !   14   = this is just the threshold value that separates "ok" from
  !          "notok" cases.
  integer, parameter :: wqc_notok_lutmax = 90
  !    90  = when only one or none sols are below LUT max and the rest (at least
  !          one) are at LUT max, the node is flagged with this number
  integer, parameter :: wqc_notok_1sol = 91
  !    91  = A single solution with speed above 4 m/s and below LUT_max
  integer, parameter :: wqc_notok_no180 = 92
  !    92  = When there is no sol at least 90 apart from 1st rank, the WVC is
  !          flagged with this number.
  integer, parameter :: wqc_notok_no180_lutmax = 93  ! added by Marcos
  !    93  = same as wqc_notok_no180 but for WVCs containing some sols at
  !          LUT max.
  integer, parameter :: wqc_notok_no180_toomany = 94
  !    94  = same as wqc_notok_no180 but for WVCs with more than
  !          inv_settings%max_nr_of_solutions solutions.
  integer, parameter :: wqc_notok_no180_toomany_lutmax = 95
  !    95  = same as wqc_notok_no180 but for WVCs with more than
  !          inv_settings%max_nr_of_solutions solutions & some sols at LUT max.
  integer, parameter :: wqc_notok_toomany_lutmax = 96     ! added by Marcos
  !    96  = more than inv_settings%max_nr_of_solutions solutions found &
  !          inv_settings%max_nr_of_solutions-1 solutions at LUT max.
  integer, parameter :: wqc_notok_toomany = 98
  !    98  = Flag associated with experimental quality checks ("exp_checks")
  !          in the post_inversion routine. NOT used operationally:
  !          more than inv_settings%max_nr_of_solutions solutions found,
  !          probably something is wrong
  integer, parameter :: wqc_notok_closetoconecenter = 99
  !    99  = Flag associated with experimental quality checks ("exp_checks")
  !          in the post_inversion routine. NOT used operationally:
  !          this triplet is probably very close to the cone center,
  !          which means it is not possible to get an accurate
  !          winddirection solution
! integer, parameter :: wqc_notok_nodirfound = 360
  !    360 = no wind direction found, only min,average and max of windspeed
  !          which is returned in foundwindspeed(1),(2) and (3)
! integer, parameter :: wqc_notok_noresult = 361
  !    361 = no wind direction found,
  !          but inv_settings%max_nr_of_solutions < 3, so it is not possible
  !          to give min/avg/max as output, therefore no output is available
  integer, parameter :: wqc_notok_tripletQCfailed = 400
  !    400 = conedistance of 1st rank solution is larger than a certain threshold
  !          (typically around 3 times the estimated noise value for the
  !           corresponding wind speed and WVC number)
! integer, parameter :: wqc_notok_nosol = 999
  !    999 = no solutions
  !===================================================================

  real(l_), dimension(max_nr_of_views) :: kp_total ! total Kp values (instrument + geophysical noise)
  real,parameter :: prob_min = 1.0E-15 ! lowest probability value to avoid zero prob

!
!  #]
!  #[ interfaces
!
!
!  #]
!
contains

  subroutine normalise_conedist_ers_ascat(inv_settings,inv_input,inv_output,model_speed,model_dir)
    !  #[
    ! Description: normalise the cone distance following Marcos' FoM approach.
    ! This can be done only after the inversion is done.
    ! A check of the first rank or closest to background cone distance w.r.t. a
    ! threshold is performed and the wind_quality_code is set if necessary.
    !
    ! arguments
    type(inv_settings_type),intent(in)  :: inv_settings
    type(inv_input_type),intent(in)     :: inv_input
    type(inv_output_type),intent(inout) :: inv_output
    real,intent(in)                     :: model_speed
    real,intent(in)                     :: model_dir
    ! local variables
    ! an additional MLE normalisation factor for ERS and ASCAT
    ! depending on the position of the node in the swath
    ! (so basically on the incidence angle)
    ! the maximum nr of nodes in the 5.6-km product of ASCAT is 198
    real(l_), save, dimension(198) :: mle_norm    ! node dependent normalization of selected MLE
    real(l_), save, dimension(198) :: factor_kpQC ! Node dependent QC factor
    logical, save      :: firstcall = .true.
    character(len=256) :: filename1,filename2,lutsdir
    integer            :: fileunit
    logical            :: exists
    integer            :: iview,isol,inode
    integer            :: num_nodes
    integer            :: closest
    real(l_)           :: kp_total_norm
    real(l_)           :: node_dependent_norm_factor

    real(l_) :: v, speed_fact  ! To mimic prescat QC rejection rate we adjust the MLE at low winds

    if (inv_output%nr_of_windsolutions .ge. 1) then

      ! check inversion settings
      if (inv_settings%do_input_data_check) then
        call check_ers_ascat_inversion_data(inv_settings,inv_input)
      endif

      ! read a node dependent MLE normalisation factor and a QC threshold from two files
      ! different tables are provided for ERS, ASCAT 25-km and ASCAT 12.5-km
      if (firstcall) then
        firstcall = .false.
        call getenv("INVERSION_LUTSDIR",lutsdir)
        if (trim(lutsdir) .ne. "") then
          num_nodes = 0
          if (inv_settings%instrument_type .eq. instrument_type_ers) then
            num_nodes = 19
            filename1 = trim(lutsdir) // "/ers_25000_MLE_norm_vs_wvc.asc"
            filename2 = trim(lutsdir) // "/ers_25000_QC_threshold_vs_wvc.asc"
          else
            select case (inv_input%nr_of_nodes)
            case (42)
              num_nodes = 42
              filename1 = trim(lutsdir) // "/ascat_25000_MLE_norm_vs_wvc.asc"
              filename2 = trim(lutsdir) // "/ascat_25000_QC_threshold_vs_wvc.asc"
            case (82)
              num_nodes = 82
              filename1 = trim(lutsdir) // "/ascat_12500_MLE_norm_vs_wvc.asc"
              filename2 = trim(lutsdir) // "/ascat_12500_QC_threshold_vs_wvc.asc"
            case (162)
              num_nodes = 162
              filename1 = trim(lutsdir) // "/ascat_6250_MLE_norm_vs_wvc.asc"
              filename2 = trim(lutsdir) // "/ascat_6250_QC_threshold_vs_wvc.asc"
            case (198)
              num_nodes = 198
              filename1 = trim(lutsdir) // "/ascat_5655_MLE_norm_vs_wvc.asc"
              filename2 = trim(lutsdir) // "/ascat_5655_QC_threshold_vs_wvc.asc"
            case default
              print *, 'ERROR: Routine normalise_conedist_ers_ascat in module post_inversion.F90:'
              print *, 'ERROR: No MLE normalisation table file for nr of nodes',inv_input%nr_of_nodes
              stop
            end select
          endif

          inquire(file=trim(filename1), exist=exists)
          if (exists) then
            if (inv_settings%verbosity .ge. inversion_verbosity_report) then
              print *,'read MLE normalisation values from file: ',trim(filename1)
            endif

            fileunit  = get_lun()
            open(unit=fileunit, file= trim(filename1), &
                 status='old', access='sequential', form='formatted')
            read(fileunit,*) (mle_norm(inode), inode=1,num_nodes)
            close(fileunit)
            call free_lun(fileunit)
          else
            print *, 'ERROR: MLE normalisation table file: ',trim(filename1),' could not be found'
            print *, 'ERROR: could not read mle_norm table'
            stop
          endif

          inquire(file=trim(filename2), exist=exists)
          if (exists) then
            if (inv_settings%verbosity .ge. inversion_verbosity_report) then
              print *,'read node dependent QC threshold from file: ',trim(filename2)
            endif

            fileunit  = get_lun()
            open(unit=fileunit, file= trim(filename2), &
                 status='old', access='sequential', form='formatted')
            read(fileunit,*) (factor_kpQC(inode), inode=1,num_nodes)
            close(fileunit)
            call free_lun(fileunit)
          else
            print *, 'ERROR: QC threshold table file: ',trim(filename2),' could not be found'
            print *, 'ERROR: could not read factor_kpQC table'
            stop
          endif
        else
          print *, 'ERROR: env variable INVERSION_LUTSDIR for directory containing'
          print *, 'ERROR: inversion lookup tables is empty'
          print *, 'ERROR: could not read mle_norm and factor_kpQC tables'
          stop
        endif
      endif

      ! calculate Kp values
      call calc_kp_ers_ascat(inv_settings,inv_input,inv_output%foundwindspeed(1))

      ! calculate normalisation factor: if we compute a norm factor for each view/beam
      ! solution swap (in ranking) is possible. To avoid this, we compute the total norm
      kp_total_norm = 0.0
      do iview = 1,inv_input%nr_of_views
        kp_total_norm = kp_total_norm + kp_total(iview) * inv_input%sigma_0(iview)**2
      enddo
      if (kp_total_norm .lt. 1.0e-25)  then
          ! for kp_total = 0 (this can happen for tests of the system without noise)
          ! all measurements should get the same weight, so setting
          ! SD_sq to a constant value 1.0e-25 is OK.
         if (inv_settings%verbosity .ge. inversion_verbosity_warn) then
            print *,'WARNING: kp_total_norm < 1.0e-25 in normalise_conedist_ers_ascat'
            print *,'WARNING: setting it to 1.0e-25 to prevent divide by zero !'
         endif
         kp_total_norm = 1.0e-25
      endif

      ! find wind solution closest to the model wind
      ! use local minima that are provided by the inversion in the MSS case
      closest = get_closest_solution(inv_output,model_speed,model_dir)

      ! below 2 m/s, a speed dependent correction factor is applied to the MLE
      ! in order to compensate the increasing cone distances
      v = inv_output%foundwindspeed(closest)

      if (v .lt. 2.0) then
        speed_fact = 8.0*v**2 - 32.0*v + 33.0 ! parabolic function
      else
        speed_fact = 1.0
      endif

      ! this node dependent factor is used to normalize (to 1) the mean
      ! value of the MLE distribution
      node_dependent_norm_factor = mle_norm(inv_input%node_nr) * speed_fact

      ! normalise the cone distances for each solution
      do isol = 1,inv_output%nr_of_windsolutions
        inv_output%conedistance_measured(isol) = inv_output%conedistance_measured(isol) / &
                                                 (kp_total_norm * node_dependent_norm_factor)
      enddo

      ! check to see if measured triplet is within the expected noise value from the cone
      ! the node dependent factor_kpQC accounts for normalization of the
      ! first-rank MLE and an additional factor of 0.946 that is applied to mimic the
      ! prescat triplet QC (see normalise_conedist_prescat_mode routine)
      ! note that triplets can be rejected only if the cone distance is positive, i.e. if
      ! the measurement is inside the cone
      if (inv_output%conedistance_measured(closest) .gt. factor_kpQC(inv_input%node_nr)) then
        inv_output%wind_quality_code = wqc_notok_tripletQCfailed
      endif

      ! for ERS, keep the check on large negative MLEs (outside the cone), it helps to flag
      ! invalid WVCs due to instrument calibration issues which occur from time to time
      if (inv_settings%instrument_type .eq. instrument_type_ers) then
        if (inv_output%conedistance_measured(closest) .lt. -1.0 * factor_kpQC(inv_input%node_nr)) then
          inv_output%wind_quality_code = wqc_notok_tripletQCfailed
        endif
      endif

    endif

  end subroutine normalise_conedist_ers_ascat
  !  #]

  subroutine calc_kp_ers_ascat(inv_settings,inv_input,guess_wspeed)
    !  #[
    ! Description: within the framework of the FoM project, a new Kp (sum of both
    ! the instrument noise and the geophysical noise) dependent on
    ! the wind speed and the incidence angle has been computed for ERS.
    ! The same approach can be used for ASCAT.
    !
    ! arguments
    type(inv_settings_type),intent(in) :: inv_settings
    type(inv_input_type),intent(in)    :: inv_input
    real(l_) :: guess_wspeed
    ! local variables
    integer   i
    logical, save      :: firstcall = .true.
    character(len=256) :: filename, lutsdir
    integer            :: fileunit
    logical            :: exists
    integer, parameter :: num_beams = 3, num_nodes_ers = 19
    real, save, dimension(num_beams, num_nodes_ers) :: mean_instr_noise

    if (inv_settings%instrument_type .eq. instrument_type_ers .and. inv_input%year .lt. 2003) then

      ! For ERS, the BUFR product contains the true instrument noise since 2003,
      ! but the older data contain invalid values for Kp.
      ! Empirical mean Kp values (as a function of beam and node number) are read
      ! from a small ASCII table for the ERS data from before 2003.
      if (firstcall) then
        firstcall = .false.
        call getenv("INVERSION_LUTSDIR",lutsdir)
        if (trim(lutsdir) .ne. "") then
          filename = trim(lutsdir) // "/ers_25000_mean_instr_kp_vs_beam_and_wvc.asc"
          inquire(file=trim(filename), exist=exists)
          if (exists) then
            if (inv_settings%verbosity .ge. inversion_verbosity_report) then
              print *,'read mean_instr_noise from file: ',trim(filename)
            endif

            fileunit  = get_lun()
            open(unit=fileunit, file= trim(filename), &
                 status='old', access='sequential', form='formatted')
            read(fileunit,*) mean_instr_noise
            close(fileunit)
            call free_lun(fileunit)
          else
            print *, 'ERROR: mean_instr_noise table file: ',trim(filename),' could not be found'
            print *, 'ERROR: could not read mean_instr_noise table'
            stop
          endif
        else
          print *, 'ERROR: env variable INVERSION_LUTSDIR for directory containing'
          print *, 'ERROR: inversion lookup tables is empty'
          print *, 'ERROR: could not read mean_instr_noise table'
          stop
        endif
      endif

      ! calculate Kp=sqrt(Kpinstr**2+Kpgeoph**2) in linear scale
      ! Kp_total = kp**2
      do i=1,inv_input%nr_of_views
        kp_total(i) = mean_instr_noise(i,inv_input%node_nr)**2 + &
          calc_geoph_noise_ers_ascat(inv_settings,inv_input,guess_wspeed,inv_input%theta_angle(i))**2
      enddo
    else

      ! for ASCAT and ERS after 2002, we can simply use the kp value of the BUFR for the
      ! instrument noise
      ! calculate Kp=sqrt(Kpinstr**2+Kpgeoph**2) in linear scale
      ! note that the input kp_a (which actually contains the BUFR noise value)
      ! is a percentage which must be converted to a fraction
      ! Kp_total = kp**2
      do i=1,inv_input%nr_of_views
        kp_total(i) = (inv_input%kp_a(i) / 100)**2 + &
          calc_geoph_noise_ers_ascat(inv_settings,inv_input,guess_wspeed,inv_input%theta_angle(i))**2
      enddo
    endif

    ! convert to ZSpace if necessary: Kp_Z=1-(1-Kp)**Z
    ! i.e. Kp_total_Z=(1-(1-sqrt(Kp_total))**Z)**2
    ! limit Kp_total to 1 (100%) in order to prevent numerical problems
    ! WVCs with such high Kp values will be flagged anyway
    if (inv_settings%use_zspace) then
      do i=1,inv_input%nr_of_views
        if (kp_total(i) .gt. 1.0) kp_total(i) = 1.0
        kp_total(i) = (1.0 - (1.0 - sqrt(kp_total(i)))**Zfactor)**2
      enddo
    endif

  end subroutine calc_kp_ers_ascat
  !  #]

  function calc_geoph_noise_ers_ascat(inv_settings,inv_input,wspeed,theta) result(kp)
    !  #[
    ! Description: this function provides the geophysical noise (Kp value)
    ! corresponding to a certain windspeed and incidence angle by interpolating
    ! the Kp table.
    !
    ! arguments
    type(inv_settings_type),intent(in) :: inv_settings
    type(inv_input_type),intent(in)    :: inv_input
    real(l_) wspeed,theta,kp
    ! local variables
    real(l_), parameter :: min_wspeed = 0.0,  max_wspeed = 20.0
    real(l_), parameter :: min_incang = 16.0, max_incang = 66.0
    integer,  parameter :: num_wspeed_steps = 21
    integer,  parameter :: num_incang_steps = 51

    real(l_), save, dimension (0:num_wspeed_steps-1,0:num_incang_steps-1) :: mean_geoph_noise

    real(l_) v
    real(l_) kp11, kp12, kp21, kp22, q, r
    integer  i,j

    real(l_), save     :: bsize_wspeed, bsize_incang
    logical, save      :: firstcall = .true.
    character(len=256) :: filename, lutsdir
    integer            :: fileunit
    logical            :: exists

    if (firstcall) then

      ! read ASCII table containing mean geophysical noise as a function of
      ! wind speed and incidence angle
      firstcall = .false.
      call getenv("INVERSION_LUTSDIR",lutsdir)

      if (trim(lutsdir) .ne. "") then
        if (inv_settings%instrument_type .eq. instrument_type_ers) then
          filename = trim(lutsdir) // "/ers_25000_geoph_kp_vs_speed_and_inc_ang.asc"
        else
          if (inv_input%cell_sampling .eq. 25000) then
            filename = trim(lutsdir) // "/ascat_25000_geoph_kp_vs_speed_and_inc_ang.asc"
          elseif (inv_input%cell_sampling .eq. 12500) then
            filename = trim(lutsdir) // "/ascat_12500_geoph_kp_vs_speed_and_inc_ang.asc"
          elseif (inv_input%cell_sampling .eq. 6250) then
            filename = trim(lutsdir) // "/ascat_6250_geoph_kp_vs_speed_and_inc_ang.asc"
          elseif (inv_input%cell_sampling .eq. 5655) then
            filename = trim(lutsdir) // "/ascat_5655_geoph_kp_vs_speed_and_inc_ang.asc"
          endif
        endif
        inquire(file=trim(filename), exist=exists)
        if (exists) then
          if (inv_settings%verbosity .ge. inversion_verbosity_report) then
            print *,'read mean_geoph_noise from file: ',trim(filename)
          endif

          fileunit  = get_lun()
          open(unit=fileunit, file= trim(filename), &
               status='old', access='sequential', form='formatted')
          read(fileunit,*) mean_geoph_noise
          close(fileunit)
          call free_lun(fileunit)

          ! calculate wind speed and incidence angle bin sizes of table
          bsize_wspeed = (max_wspeed - min_wspeed) / real(num_wspeed_steps - 1)
          bsize_incang = (max_incang - min_incang) / real(num_incang_steps - 1)
        else
          print *, 'ERROR: mean_geoph_noise table file: ',trim(filename),' could not be found'
          print *, 'ERROR: could not read mean_geoph_noise table'
          stop
        endif
      else
        print *, 'ERROR: env variable INVERSION_LUTSDIR for directory containing'
        print *, 'ERROR: inversion lookup tables is empty'
        print *, 'ERROR: could not read mean_geoph_noise table'
        stop
      endif
    endif

    ! check wind speed and incidence angle ranges
    if (wspeed .lt. min_wspeed) then
      print *, 'ERROR: speed value out of range in calc_geoph_noise_ers_ascat', wspeed
      stop
    endif
    if (theta .lt. min_incang .or. theta .gt. max_incang) then
      print *, 'ERROR: incidence angle value out of range in calc_geoph_noise_ers_ascat', theta
      stop
    endif

    v = wspeed
    if (v .gt. max_wspeed) v = max_wspeed

    ! find the table elements to use for the interpolation
    i = int((v     - min_wspeed)/bsize_wspeed)
    j = int((theta - min_incang)/bsize_incang)

    if (i .eq. num_wspeed_steps-1) i = num_wspeed_steps-2
    if (j .eq. num_incang_steps-1) j = num_incang_steps-2

    ! do the interpolation using four surrounding table elements
    q = (v     - min_wspeed - i*bsize_wspeed)/bsize_wspeed
    r = (theta - min_incang - j*bsize_incang)/bsize_incang

    kp11 = mean_geoph_noise(i  ,j)
    kp21 = mean_geoph_noise(i+1,j)
    kp12 = mean_geoph_noise(i  ,j+1)
    kp22 = mean_geoph_noise(i+1,j+1)

    kp = kp11 + &
         q * (kp21-kp11) + &
         r * (kp12-kp11) + &
         q * r * (kp22+kp11-kp12-kp21)

  end function calc_geoph_noise_ers_ascat
  !  #]

  subroutine normalise_conedist_prescat_mode(inv_settings,inv_input,inv_output)
    !  #[
    ! Description: normalise the ERS cone distances using the 'old' prescat approach.
    ! For ERS no Kp normalisation per beam is used, but only a scaling after the
    ! inversion is done.
    ! See thesis Ad Stoffelen, p.II-28 for these formulas.
    ! A check of the first rank cone distance w.r.t. a threshold is performed
    ! and the wind_quality_code is set if necessary.
    !
    ! arguments
    type(inv_settings_type),intent(in)  :: inv_settings
    type(inv_input_type),intent(in)     :: inv_input
    type(inv_output_type),intent(inout) :: inv_output
    ! local variables
    ! an additional MLE normalisation factor for ERS
    ! depending on the position of the node in the swath
    ! (so basically on the incidence angle)
    real(l_), dimension(19), parameter :: mle_norm = &
              (/ 0.278,0.246,0.247,0.255,0.265,0.278,0.344,0.481,0.627,0.720, &
                 0.818,0.883,0.878,0.908,0.983,1.057,1.178,1.349,1.608 /)
    integer  :: iview,isol
    real(l_) :: sum_s0_sq,v,theta,noise_sq,rel_conedist_sq
    real(l_) :: zzSk,zNorm,zSkill
    real(l_) :: node_dependent_norm_factor

    if (inv_output%nr_of_windsolutions .ge. 1) then

      ! check inversion settings
      if (inv_settings%do_input_data_check) then
        call check_ers_ascat_inversion_data(inv_settings,inv_input)
      endif

      theta = inv_input%theta_angle(2) ! inc. angle of the midbeam

      sum_s0_sq = 0.0
      do iview = 1,inv_input%nr_of_views
        sum_s0_sq = sum_s0_sq + inv_input%sigma_0(iview)**2
      enddo

      v = inv_output%foundwindspeed(1) ! only use speed of 1st rank solution
      noise_sq = get_ers_noise_estimate(inv_settings,v, theta, sum_s0_sq)
      node_dependent_norm_factor = mle_norm(inv_input%node_nr)

      ! calculate skill, based on UNnormalised cone distances
      ! note that the skill is used only when the prescat ambrem scheme is applied
      ! the sign has not yet been added to the cone distance here,
      ! so an abs() is not necessary, but in case the order of calls
      ! changes in the future, it is probably better to apply it anyway
      zSkill = max(inv_output%mean_cone_distance - abs(inv_output%conedistance_measured(1)), 0.)
      zNorm  = max(inv_output%conedistance_measured(1), noise_sq)
      zSkill = zSkill / zNorm

      ! see thesis Ad Stoffelen, p. V-5 en V-6 for these equations:
      !     Use 2nd order function in sqrt(skill) to get normalised skill:
      !           f(0)=0, f(a)=1, f'(a)=0  , a = 10.,   zzSk = sqrt(skill)/a
      !           f = -zzSk**2 + 2zzSk  ; save in %
      zzSk = min(sqrt(max(zSkill,.00001)) / 10., 1.)
      inv_output%skill = (-zzSk + 2.) * zzSk * 100.

      ! normalise the cone distances
      rel_conedist_sq = 9999.9
      do isol = 1,inv_output%nr_of_windsolutions
        inv_output%conedistance_measured(isol) = &
             inv_output%conedistance_measured(isol) / (noise_sq*node_dependent_norm_factor)

        ! determine the minimum value of the cone distances of all solutions,
        ! used to check if QC flag needs to be set
        ! for the 4 solutions case, this will be the first solution
        ! for the 144 solutions case not!
        if (abs(inv_output%conedistance_measured(isol)) .lt. rel_conedist_sq) then
          rel_conedist_sq = abs(inv_output%conedistance_measured(isol))
        endif
      enddo

      ! check to see if measured triplet is within 3 times the expected
      ! noise value from the cone
      if (rel_conedist_sq .gt. 3.0**2) then
        inv_output%wind_quality_code = wqc_notok_tripletQCfailed
      endif
    endif

  end subroutine normalise_conedist_prescat_mode
  !  #]

  function get_ers_noise_estimate(inv_settings,v,theta_midbeam,sum_s0_sq) result(noise_sq)
    !  #[
    ! Description: see thesis Ad Stoffelen, p. II-28
    !
    ! arguments
    type(inv_settings_type),intent(in)  :: inv_settings
    real(l_) :: v, theta_midbeam, sum_s0_sq, noise_sq
    ! local variables
    real(l_) :: X

    ! see thesis Ad Stoffelen, p. II-28, eq. D2
    X = 0.02 * (1.0 + (45.0 - theta_midbeam) / 27.0)

    if (inv_settings%use_zspace) then
      noise_sq = sum_s0_sq * &
           (Zfactor * (1.0 + 5.0/v + 1.0/(2.0*v*v) + 5.0/(2.0*v*v*v)) * X)**2
    else
      noise_sq = sum_s0_sq * &
           ((1.0 + 5.0/v + 1.0/(2.0*v*v) + 5.0/(2.0*v*v*v)) * X)**2
    endif

    ! see thesis Ad Stoffelen, p. II-28, equation h^2
    if (v .gt. 15.0) noise_sq = noise_sq * (1.0 + (v - 15.0)**2 / 100.0)

    ! WARNING: the above formulation is exactly as is described in Ad's thesis
    ! taking into account the mistake of sqrt in the h function.
    ! HOWEVER, the original PRESCAT invert.f uses an approximation:
    ! noise_sq = sum_s0_sq * &
    !            ( Zfactor*(1. + 5./v)*X )**2
    ! noise_sq = noise_sq * (1. + 1./v**2)
    !
    ! This is exactly the same for the terms upto 1/v^3, and only the
    ! term 1/v^4 (and higher terms) differ a little.
    !
    ! my implementation yields:   1 + 10/v + 26/v^2 + 10/v^3 + 101/(4v^4) +
    !                             5/(2v^5) + 25/(4v^6)
    ! old prescat implementation: 1 + 10/v + 26/v^2 + 10/v^3 + 100/(4v^4)
    ! therefore both are almost identical. The old one is probably
    ! a little less cpu-time consuming, but on modern machines I dont care
    ! (and also for reprocessing I dont care)

    ! from the next print it follows that the difference between
    ! the old approximation and the new full formula is:
    ! v=0.2 m/s new code has 7 times higher noise_sq
    ! v=1   m/s new code has 15% higher noise_sq
    ! v=2   m/s new code has 1.1% higher noise_sq
    ! v=3   m/s new code has 0.3% higher noise_sq
    ! v=4   m/s new code has 0.1% higher noise_sq
    !

!   print *,"sum_s0_sq = ",sum_s0_sq," noise_sq = ",sum_s0_sq * &
!        ( Zfactor*(1. + 5./v + 1./(2.*v*v) + 5./(2.*v*v*v) )*X )**2,&
!       " noise_sq_prescat = ",&
!        sum_s0_sq * (Zfactor * X * (1. + 5./v) )**2 * (1. + 1./(v*v) ),&
!        " v = ",v

  end function get_ers_noise_estimate
  !  #]

  subroutine check_ers_ascat_inversion_data(inv_settings,inv_input)
    !  #[
    ! Description: checks some inversion settings specific for ERS and ASCAT.
    !
    ! arguments
    type(inv_settings_type),intent(in) :: inv_settings
    type(inv_input_type),intent(in)    :: inv_input
    ! local variables

    if (inv_settings%instrument_type .ne. instrument_type_ers .and. &
        inv_settings%instrument_type .ne. instrument_type_ascat) then
      print *,"ERROR: instrument_type in the inv_settings struct should be set to ERS or ASCAT"
      stop
    endif

    if (inv_settings%instrument_type .eq. instrument_type_ers) then
      if (inv_input%year .eq. missing_indicator_integer) then
        print *,"ERROR: year for ERS seems undefined in the inv_input struct"
        print *,"ERROR: this number is needed for determining the Kp values"
        stop
      endif

      if (inv_input%node_nr .lt. 1 .or. inv_input%node_nr .gt. 19) then
        print *,"ERROR: node_nr for ERS seems out of range in the inv_input struct"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        print *,'inv_input%node_nr = ',inv_input%node_nr
        stop
      endif
    endif

    if (inv_settings%instrument_type .eq. instrument_type_ascat) then
      if (inv_input%cell_sampling .ne. 25000 .and. &
          inv_input%cell_sampling .ne. 12500 .and. &
          inv_input%cell_sampling .ne.  6250 .and. &
          inv_input%cell_sampling .ne.  5655) then
        print *,"ERROR: cell_sampling for ASCAT is not equal to 25000, 12500, 6250, or 5655"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        stop
      endif

      if (inv_input%cell_sampling .eq. 25000 .and. &
          (inv_input%node_nr .lt. 1 .or. inv_input%node_nr .gt. 42)) then
        print *,"ERROR: node_nr for ASCAT seems out of range in the inv_input struct"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        stop
      endif

      if (inv_input%cell_sampling .eq. 12500 .and. &
          (inv_input%node_nr .lt. 1 .or. inv_input%node_nr .gt. 82)) then
        print *,"ERROR: node_nr for ASCAT seems out of range in the inv_input struct"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        stop
      endif

      if (inv_input%cell_sampling .eq. 6250 .and. &
          (inv_input%node_nr .lt. 1 .or. inv_input%node_nr .gt. 162)) then
        print *,"ERROR: node_nr for ASCAT seems out of range in the inv_input struct"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        stop
      endif

      if (inv_input%cell_sampling .eq. 5655 .and. &
          (inv_input%node_nr .lt. 1 .or. inv_input%node_nr .gt. 198)) then
        print *,"ERROR: node_nr for ASCAT seems out of range in the inv_input struct"
        print *,"ERROR: this number is needed for normalisation of the cone distance"
        stop
      endif
    endif

  end subroutine check_ers_ascat_inversion_data
  !  #]

  subroutine check_wind_solutions_ers_ascat(inv_settings, inv_output)
    !  #[
    ! Description: this routine applies a number of quality control tricks
    ! specially for the ERS and ASCAT instruments
    ! an attempt is done to obtain two solutions (if requested) and
    ! the wind_quality_code is filled in
    !
    ! arguments
    type(inv_settings_type),intent(in)  :: inv_settings
    type(inv_output_type),intent(inout) :: inv_output
    ! local variables
    integer  :: i,isol
    logical  :: inside_cone

    ! a setting needed to decide what angle still is close enough to 180
    ! Marcos suggested to use 90. degrees as criterium
    ! (originally I had 155. degrees)
    real(l_), parameter :: dir_criterium = 90.0

    ! maximum ratio |MLE3/MLE1| to reject high-rank solutions
    real(l_), parameter :: MLE_highfactor = 40.0

!  #[ conditionally defined variables

!#define CHECK180DEG
#ifdef CHECK180DEG
    integer  :: sol_to_remove
    real(l_) :: dir_diff23
#endif

#define ALLOW_LOWSPEED_SOL
#ifdef ALLOW_LOWSPEED_SOL
    real(l_), parameter :: lowspeed_exception = 1.0 ! 0.5 ! m/s
    logical :: lowspeed_exception_found
#endif

#define CHECK_MLE_RANGE
#ifdef CHECK_MLE_RANGE
    ! a factor needed to find triplets close to the cone center
    ! a value of 1.5 filters almost nothing
    real(l_), parameter :: MLE_lowfactor = 1.5 ! 4.0
#endif

!#define CHECKCONECENTERDIST
#ifdef CHECKCONECENTERDIST
    ! a factor that defines how close a triplet may be to the cone center
    integer, parameter :: MLE_center_factor = 1./4.
    real(l_) :: dist_to_cone_center
#endif

!#define REDUCE_TO_TWO_SOLS
#ifdef REDUCE_TO_TWO_SOLS
    real(l_) :: dir_diff12,dir_diff13
#endif

    !  #]

    ! these checks don't make sense when get_all_winddirs option (mss) is chosen
    if (inv_settings%get_all_winddirs) then
      return
    endif

    ! filter nodes with no solutions (minima) found (inversion module)
    if (inv_output%wind_quality_code .gt. wqc_below_this_are_ok) then
      return
    endif

    ! WVCs with more than allowed nr of solutions and/or with solutions at
    ! LUT edges are OK (for now). The different cases are flagged accordingly.
    ! a) LUT_min cases:
    if (inv_output%minimum_at_lutmin) then
      if (inv_output%wind_quality_code .eq. wqc_ok_toomany) then
        inv_output%wind_quality_code = wqc_ok_toomany_lutmin
      else
        inv_output%wind_quality_code = wqc_ok_lutmin
      endif
    endif
    ! b) LUT_max cases:
    if (inv_output%minimum_at_lutmax) then
      if (inv_output%wind_quality_code .eq. wqc_ok_toomany) then
        inv_output%wind_quality_code = wqc_ok_toomany_lutmax
      else
        inv_output%wind_quality_code = wqc_ok_lutmax
      endif
    endif

    just1sol: if (inv_output%nr_of_windsolutions .eq. 1) then
      if (inv_settings%verbosity .ge. inversion_verbosity_report) then
        ! in PRESCAT an artificial solution was added in case of 1 sol. of
        ! windspeed < 4 m/s, with 180 degree ambiguity.
        ! I have to check some cases to see if I want to do this as well
        print *,"WARNING: just one solution found"
        print *,"please use this example to study this in detail, saving node data...."
!       CALL save_inv_input( "test1sol_input.dat", inv_input)
!       CALL save_inv_output("test1sol_output.dat",inv_output)
!       stop
      endif

      ! this (almost) never happens, at least in a testrun with 4 days of
      ! data (20-23 jan.1993) I did not find a single example.
      ! So I am not putting any more effort in this at the moment
      ! Marcos: We only trust 1 sol cases at low winds. In such case,
      ! a 2nd-rank sol identical to 1st-rank but 180 deg. apart is generated.
      if (inv_output%foundwindspeed(1) .gt. 4.) then
        inv_output%wind_quality_code = wqc_notok_1sol
      else
        inv_output%wind_quality_code = wqc_ok_1sol
        inv_output%nr_of_windsolutions = 2
        inv_output%foundwindspeed(2) = inv_output%foundwindspeed(1)
        inv_output%foundwinddir(2) = mod(inv_output%foundwinddir(1)+180.+3600.,360.)
        inv_output%conedistance_measured(2) = inv_output%conedistance_measured(1)
      endif
      return
    endif just1sol

    ! an extra check: ERS normally only produces 2 minima, but in special cases
    ! 3 or more can be found.
    ! This happens if the triplet lies within the cone (3 sol).
    ! If the triplet lies close to the center of the cone even 4 sol.
    ! may be found. Cases of broad minima, providing more than 4 sol
    ! are rare but possible. A thorough investigation of such cases show
    ! that when a solution more than 90 deg apart from 1st-rank is found
    ! the WVC is OK. The results also show that solutions at LUT max should
    ! be discarded. If they are not discarded in the inversion module
    ! (see get_indices_lowest_local_minima), they are discarded now.
    morethan1sol: if (inv_output%nr_of_windsolutions .gt. 1) then

      ! remove LUT max solution(s)
      if (inv_output%minimum_at_lutmax) then
        isol = 1
        solloop: do
          if (isol .gt. inv_output%nr_of_windsolutions) exit solloop

          if (inv_output%foundwindspeed(isol) .ge. max_windvelocity - 0.01) then

            ! remove LUT edge solution
            call remove_one_solution(inv_settings,inv_output,isol)
          else

            ! jump to the next solution
            isol = isol + 1
          endif
        enddo solloop

        ! if 1 or none solutions are found below LUT max, WVC is flagged as NOTOK
        if (inv_output%nr_of_windsolutions .eq. 0) then
          inv_output%wind_quality_code = wqc_notok_nosol
          return
        elseif (inv_output%nr_of_windsolutions .eq. 1) then
          if (inv_output%wind_quality_code .eq. wqc_ok_toomany_lutmax) then
            inv_output%wind_quality_code = wqc_notok_toomany_lutmax
          else
            inv_output%wind_quality_code = wqc_notok_lutmax
          endif
          return
        endif
      endif

      ! remove high-rank solutions when MLE1 < 0 or MLE2 < 0 or |MLE3/MLE1| > 40
      ! see Lin, W., M. Portabella, A. Stoffelen and A. Verhoef,
      ! On the characteristics of ASCAT wind direction ambiguities
      ! Atmospheric Measurement Techniques, 2013, 6, 1053-1060,
      ! doi:10.5194/amt-6-1053-2013.
      ! only in case of 3 or 4 wind solutions when 1st rank wind speed > 4m/s
      isol = inv_output%nr_of_windsolutions
      if (isol .eq. 3 .or. isol .eq. 4) then
        if (inv_output%foundwindspeed(1) .gt. 4.0) then
          if (inv_output%conedistance_measured(1) .lt. 0.0 .or. &
              inv_output%conedistance_measured(2) .lt. 0.0 .or. &
              abs(inv_output%conedistance_measured(3)) .gt. &
              abs(inv_output%conedistance_measured(1)) * MLE_highfactor) then

            solloop2: do
              if (isol .le. 2) exit solloop2

              ! discard high MLE solution
              if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                print *,"discarding too high MLE solution nr. ",isol
                print *,"speed = ",inv_output%foundwindspeed(isol),&
                     " dir = ",inv_output%foundwinddir(isol),&
                     " MLE = ",inv_output%conedistance_measured(isol)
              endif
              call remove_one_solution(inv_settings,inv_output,isol)

              ! jump to the next solution
              isol = isol - 1
            enddo solloop2

            if (inv_output%nr_of_windsolutions .eq. 2) then
              if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                print *,"reduction of nr of solutions to 2 was successfull"
              endif
              inv_output%wind_quality_code = wqc_ok_discardedsol
              return
            endif

          endif
        endif
      endif

      exp_checks: if (inv_settings%experimental_ers_wqc_checks) then
        !  #[ a number of experimental quality checks

        if (inv_output%nr_of_windsolutions .eq. 2) then
          ! case with 3 or more solutions that seem valid, and are not
          ! at the LUT edge.
          ! for the moment I will allow this only if the measured triplet
          ! is inside the cone, so if the signs of one or more solutions
          ! are positive
          inv_output%wind_quality_code = wqc_ok_ERSexception

#ifdef ALLOW_LOWSPEED_SOL
          ! for very low windspeeds, the fact that the windspeed is low
          ! is more important than the problems of determining
          ! wind-direction so these should be reported always,
          ! even if more than 2 solutions are found.
          ! Just report the 2 solutions closest to the cone.
          ! (in PRESCAT speed was set to 1 m/s
          !  and winddir to 0 and 180 degrees in this case)
          lowspeed_exception_found = .true.
          do isol = 1,inv_output%nr_of_windsolutions
            if (inv_output%foundwindspeed(isol) .gt. lowspeed_exception) &
                 lowspeed_exception_found = .false.
          enddo
          lowspeedexc: if (lowspeed_exception_found) then
            inv_output%nr_of_windsolutions = 2
            inv_output%foundwindspeed(3:inv_settings%max_nr_of_solutions)=&
                 missing_indicator_real
            inv_output%foundwinddir(3:inv_settings%max_nr_of_solutions) = &
                 missing_indicator_real
            inv_output%wind_quality_code = wqc_ok_ERSlowspeedexception
            return
          endif lowspeedexc
#endif

          ! more than 2 solutions are not allowed for ERS; if triplet falls
          ! outside the cone this may indicate a programming error
          ! or invalid data (REMEMBER this may also happen if inversion is
          ! applied on only 2 beams!)
          !
          ! note that this test can be performed only when the sign of
          ! the cone distance is known
          if (inv_settings%add_sign_to_mle) then
            if (inv_settings%verbosity .ge. inversion_verbosity_report) then
              print *,"number of wind solutions is still more than 2"
              print *,"nr_of_windsolutions = ",inv_output%nr_of_windsolutions
              print *,"checking if triplet is inside the cone ..."
            endif
            inside_cone = .false.
            do i=1,inv_output%nr_of_windsolutions
              if (inv_output%conedistance_measured(i) .ge. 0) inside_cone = .true.
            enddo
            if (.not. inside_cone) then
              if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                print *,"ERS case with too many solutions, and triplet is outside the cone ..."
              endif
              inv_output%wind_quality_code = wqc_notok_toomany
              return
            else
              if (inv_settings%verbosity .ge. inversion_verbosity_report) then
                print *,"yes, it is inside the cone"
              endif
            endif
          endif

#ifdef CHECK_MLE_RANGE
          ! add a check for too close to cone center
          ! use a low variation in MLE with wind direction as criterium
          if (inv_output%max_cone_distance .lt. &
              inv_output%min_cone_distance * MLE_lowfactor) then

            ! this triplet is probably very close to the cone center,
            ! which means it is not possible to get an accurate
            ! winddirection solution, so flag it
            ! (solving the windspeed is still possible, if this is needed
            !  a special (optional) switch should be added to this code
            inv_output%wind_quality_code = wqc_notok_closetoconecenter
            return
          endif
#endif

#ifdef CHECKCONECENTERDIST
          ! besides the low variation in MLE with wind direction
          ! which was checked above, I will also calculate the actual
          ! distance to the cone center, and check how close the triplet
          ! is to it.
          dist_to_cone_center = calc_dist_to_cone_center(inv_input,inv_output)
          do i = 1,inv_output%nr_of_windsolutions
            if (dist_to_cone_center .lt. &
                MLE_center_factor*inv_output%conedistance_measured(i)) then

              ! flag this solution
              inv_output%wind_quality_code = wqc_notok_closetoconecenter
              return
            endif
          enddo
#endif

#ifdef CHECK180DEG
          if (inv_output%nr_of_windsolutions .eq. 3) then

            ! now I can use the ERS property that there should be
            ! 2 valid solutions about 180 degrees apart.
            ! the one that does not fullfill this can be removed from the
            ! list of solutions
            dir_diff12 = abs(inv_output%foundwinddir(1) - inv_output%foundwinddir(2))
            dir_diff23 = abs(inv_output%foundwinddir(2) - inv_output%foundwinddir(3))
            dir_diff13 = abs(inv_output%foundwinddir(1) - inv_output%foundwinddir(3))
            if (dir_diff12 .gt. 180.) dir_diff12 = 360. - dir_diff12
            if (dir_diff23 .gt. 180.) dir_diff23 = 360. - dir_diff23
            if (dir_diff13 .gt. 180.) dir_diff13 = 360. - dir_diff13

            sol_to_remove = -1
            if (dir_diff12 .gt. dir_criterium) sol_to_remove=3
            if (dir_diff23 .gt. dir_criterium) sol_to_remove=1
            if (dir_diff13 .gt. dir_criterium) sol_to_remove=2

            if (sol_to_remove .ne. -1) then
              call remove_one_solution(inv_settings, inv_output, sol_to_remove)
            endif
          endif ! check for 3 minima
#endif

          ! if at this point still more than 2 solutions are present
          ! this is NOT a valid ERS case, so flag it
          if (inv_output%nr_of_windsolutions .gt. 2) then
            inv_output%wind_quality_code = wqc_notok_toomany
            return
          endif

        endif

        !  #]
      endif exp_checks

    endif morethan1sol

#ifdef REDUCE_TO_TWO_SOLS
    ! alternative for the exp_checks
    ! reduce to 2 solutions (by Marcos)
    if (inv_output%nr_of_windsolutions .ge. 2) then
      remove_loop: do

        ! repeat until no more than 2 solutions are left
        if (inv_output%nr_of_windsolutions .le. 2) exit remove_loop

        dir_diff12 = abs(inv_output%foundwinddir(1) - inv_output%foundwinddir(2))
        dir_diff13 = abs(inv_output%foundwinddir(1) - inv_output%foundwinddir(3))

        if (dir_diff12 .gt. 180.) dir_diff12 = 360. - dir_diff12
        if (dir_diff13 .gt. 180.) dir_diff13 = 360. - dir_diff13

        if (dir_diff12 .gt. dir_diff13) then

          ! dir_diff12 is closer to 180 than dir_diff13, so delete sol 3
          call remove_one_solution(inv_settings, inv_output, 3)
        else

          ! dir_diff13 is closer to 180 than dir_diff12, so delete sol 2
          call remove_one_solution(inv_settings, inv_output, 2)
        endif
      enddo remove_loop

      ! check for success
      dir_diff12 = abs(inv_output%foundwinddir(1) - inv_output%foundwinddir(2))

      if (dir_diff12 .gt. 180.) dir_diff12 = 360. - dir_diff12

      if (dir_diff12 .gt. dir_criterium) then
        if (inv_output%wind_quality_code .gt. wqc_below_this_are_ok) then
          print*, 'FATAL ERROR: At this point wqc should be ok!'
          stop
        endif
      else
        ! Only in case wind < 4 m/s, wqc is set as OK, i.e., wqc_ok_1sol
        if (inv_output%foundwindspeed(1) .le. 4.) then
          inv_output%nr_of_windsolutions = 2
          inv_output%foundwindspeed(2) = inv_output%foundwindspeed(1)
          inv_output%foundwinddir(2) = mod(inv_output%foundwinddir(1)+180.+3600.,360.)
          inv_output%conedistance_measured(2) = inv_output%conedistance_measured(1)
          inv_output%wind_quality_code = wqc_ok_1sol
        elseif (inv_output%wind_quality_code .eq. wqc_ok_lutmax) then
          inv_output%wind_quality_code = wqc_notok_no180_lutmax
        elseif (inv_output%wind_quality_code .eq. wqc_ok_toomany) then
          inv_output%wind_quality_code = wqc_notok_no180_toomany
        elseif (inv_output%wind_quality_code .eq. wqc_ok_toomany_lutmax) then
          inv_output%wind_quality_code = wqc_notok_no180_toomany_lutmax
        else
          inv_output%wind_quality_code = wqc_notok_no180
        endif
      endif

    endif
#endif

  end subroutine check_wind_solutions_ers_ascat
  !  #]

  subroutine remove_one_solution(inv_settings, inv_output, sol_nr)
    !  #[
    !
    ! arguments
    type(inv_settings_type),intent(in)  :: inv_settings
    type(inv_output_type),intent(inout) :: inv_output
    integer, intent(in)                 :: sol_nr
    ! local variables
    integer :: i

    if (sol_nr .gt. 0 .and. sol_nr .le. inv_output%nr_of_windsolutions) then
      if (sol_nr .ne. inv_output%nr_of_windsolutions) then

        ! move following solutions one place down
        do i = sol_nr + 1,inv_output%nr_of_windsolutions
          inv_output%foundwindspeed(i-1)        = inv_output%foundwindspeed(i)
          inv_output%foundwinddir(i-1)          = inv_output%foundwinddir(i)
          inv_output%conedistance_measured(i-1) = inv_output%conedistance_measured(i)
        enddo
      endif

      ! make unused solutions empty
      do i = inv_output%nr_of_windsolutions,inv_settings%max_nr_of_solutions
        inv_output%foundwindspeed(i)        = missing_indicator_real
        inv_output%foundwinddir(  i)        = missing_indicator_real
        inv_output%conedistance_measured(i) = missing_indicator_real
      enddo
      inv_output%nr_of_windsolutions = inv_output%nr_of_windsolutions - 1
    else
      print *,"ERROR: sorry, cannot remove not existing solutions ..."
      print *,"ERROR: you tried to remove solution ", sol_nr, " of ", &
        inv_output%nr_of_windsolutions
      stop
    endif

  end subroutine remove_one_solution
  !  #]

  subroutine calc_probabilities(inv_settings,inv_output,l_parameter,ExpMeanMLE)
    !  #[
    !
    ! Description: calculate probabilities taking into account
    ! the MLE and the sector sizes
    !
    ! arguments
    type(inv_settings_type),      intent(in)    :: inv_settings
    type(inv_output_type),        intent(inout) :: inv_output
    real,                         intent(in)    :: l_parameter
    real, optional,               intent(in)    :: ExpMeanMLE

    ! local variables
    real   ,dimension(max_nr_of_sol) :: sector ! contains size of wind direction sector
    integer,dimension(max_nr_of_sol) :: isort  ! sort index
    integer :: isol,nsol
    real    :: prob,prob_sum,raux,reciproke_l_parameter
    real    :: ReciprokeExpMeanMLE
    !

    ! initialise
    do isol = 1,max_nr_of_sol
      inv_output%probability(isol) = missing_indicator_real
    enddo

    ! NOTE: ExpMeanMLE is used by SDP to scale the MLE returned by inversion.
    ! The ExpMeanMLE depends on node-number and windspeed and is read from
    ! a LUT stored in a file. 
    ! Handling this file cannot be easily inserted here, since not all
    ! instruments use this method. Therefore to keep this routine generic, 
    ! the ExpMeanMLE is introduced as optional input parameter. JK, 11-May-2009

    ReciprokeExpMeanMLE = 1.
    if (present(ExpMeanMLE)) ReciprokeExpMeanMLE = 1.0 / ExpMeanMLE

    if (inv_output%nr_of_windsolutions .ge. 1) then
      reciproke_l_parameter = 1.0 / l_parameter
      nsol = inv_output%nr_of_windsolutions ! for shorter notation

      if ((inv_settings%instrument_type .eq. instrument_type_ers .or. &
           inv_settings%instrument_type .eq. instrument_type_ascat) .and. &
          inv_output%foundwindspeed(1) .lt. 4.0) then

        ! exceptional case: for ERS & ASCAT low wind cases (< 4 m/s) anisotropy
        ! is small as compared to noise
        ! as such equal probability is assigned to the ambiguous wind solutions
        do isol = 1,nsol
          inv_output%probability(isol) = 1.0/real(nsol)
        enddo

      else

        ! calculate probabilities
        ! use the relation p = 1/k * exp(-MLE / l), see p.63 of thesis Marcos Portabella.
        ! conedistance_measured must contain the Rn for SeaWinds
        ! l = 1.4 for SeaWinds, 2 for ERS and 1.5 for ASCAT
        do isol = 1,nsol
          if (present(ExpMeanMLE)) then
            raux = abs(inv_output%conedistance_measured(isol)) * ReciprokeExpMeanMLE
          else
            raux = abs(inv_output%conedistance_measured(isol))
          endif
          prob                         = exp(-raux * reciproke_l_parameter)
          inv_output%probability(isol) = max(prob,prob_min)  ! to avoid zero prob in AR cost function
        enddo
      endif

      ! calculate sector size as part of the full circle for each solution
      select case(nsol)
      case(1)
        sector(1) = 1.0
      case(2)
        sector(1) = 0.5
        sector(2) = 0.5
      case default

        ! sort inversion output on wind direction
        call GetSortIndex(nsol,inv_output%foundwinddir,isort)

        ! calculate angle difference between next and previous winddir
        ! this is twice the sector size
        do isol = 2,nsol-1
          sector(isort(isol)) = inv_output%foundwinddir(isort(isol+1)) - &
                                inv_output%foundwinddir(isort(isol-1))
        enddo

        ! begin and end sectors need to be calculated separately
        sector(isort(1))    = inv_output%foundwinddir(isort(   2)) - &
                              inv_output%foundwinddir(isort(nsol))
        sector(isort(nsol)) = inv_output%foundwinddir(isort(     1)) - &
                              inv_output%foundwinddir(isort(nsol-1))

        ! normalisation, such that sum of sectors is 1
        do isol = 1,nsol
          if (sector(isol) .lt. 0.0) sector(isol) = sector(isol) + 360.0
          sector(isol) = sector(isol) / (2.0 * 360.0)
        enddo
      end select

      ! weigh each probability by the size of its sector
      prob_sum  = 0.0
      do isol = 1,nsol
        inv_output%probability(isol) = inv_output%probability(isol) * sector(isol)
        prob_sum = prob_sum + inv_output%probability(isol)
      enddo

      ! normalise the probabilities such that their sum is 1
      if (prob_sum .gt. 0.0) then
        do isol = 1,nsol
          inv_output%probability(isol) = inv_output%probability(isol) / prob_sum
        enddo
      endif
    endif ! inv_output%nr_of_windsolutions .ge. 1

  end subroutine calc_probabilities
  !  #]

end module post_inversion
