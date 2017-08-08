program inv_test
  !  #[ Document info
  !     Written by Jos de Kloe
  !     for testing inversion number 1.5.2
  !     last change 12-06-2007
  !     copyright KNMI
  !  #]
  !  #[ the main test program
  use inversion, only: inv_input_type, inv_output_type, init_inv_input,&
                       write_invsettings_to_file, read_inv_input,&
                       inv_settings_type, get_inv_settings,&
                       set_inv_settings, init_inv_settings_to_default,&
                       instrument_type_ers, instrument_type_seawinds, &
                       angle_convention_oceanographic, &
                       angle_convention_meteorological, &
                       use_cmod5_to_construct_LUT
  use LunManager, only: get_lun, free_lun
  use Compiler_Features, only: getarg_genscat, iargc_genscat

  IMPLICIT NONE             ! no implicit variable typing

! choose instrument type
#ifdef ERS
#undef ERS
#endif
#ifdef SEAWINDS
#undef SEAWINDS
#endif
#define ERS

! settings for the coneplot scans
#define MAXWINDSP   25.0
!#define MAXWINDSP   50.0
#define MINWINDSP    1.0
#define WINDSPSTEPS  100
#define WINDDIRSTEPS 144

  ! data structures needed by the inversion routine
  type(inv_input_type)    :: inv_input
  type(inv_output_type)   :: inv_output
  type(inv_settings_type) :: inv_settings
  character(len=256)      :: filename
  integer                 :: fileunit
  integer                 :: nr_of_arguments, error

  ! set the default
  filename = "testfile_inv_input.dat"

  ! take the filename for testinput from the commandline
  nr_of_arguments = iargc_genscat()
  if (nr_of_arguments .lt.1) then
     print *,"No filename supplied on commandline"
     print *,"using default name: ", trim(filename)
  else
     call getarg_genscat(1,filename)
     print *,"filename from commandline: ",trim(filename)
  endif

  call init_inv_input(inv_input)
  call read_inv_input(filename,inv_input)

  ! set the inv_settings explicitly for Seawinds
  call init_inv_settings_to_default()
  call get_inv_settings(inv_settings,error)

#ifdef SEAWINDS
  inv_settings%instrument_type                = instrument_type_seawinds
  inv_settings%antenna_angle_convention       = angle_convention_oceanographic
  inv_settings%wind_angle_convention          = angle_convention_meteorological
  inv_settings%use_zspace                     = .false.
  inv_settings%no_normalisation               = .false.
  inv_settings%add_sign_to_mle                = .false.
  inv_settings%get_all_winddirs            = .false.
  inv_settings%do_parabolic_minimum_search = .false.
  inv_settings%do_parabolic_winddir_search = .false.
  inv_settings%max_nr_of_solutions         = 4
  inv_settings%experimental_ers_wqc_checks    = .false.
  inv_settings%use_meas_sigma0_for_normfactor = .true.
  inv_settings%first_warning_only             = .true.
  inv_settings%do_input_data_check            = .true.
  inv_settings%ws_initial_guess               = 8.0
#endif
#ifdef ERS
  inv_settings%instrument_type                = instrument_type_ers
  inv_settings%antenna_angle_convention       = angle_convention_meteorological
  inv_settings%wind_angle_convention          = angle_convention_meteorological
  inv_settings%use_zspace                     = .true.
  inv_settings%no_normalisation               = .true. ! = not default
  inv_settings%add_sign_to_mle                = .true. ! = not default
  inv_settings%get_all_winddirs               = .false.
  inv_settings%do_parabolic_winddir_search    = .true. ! = default
  inv_settings%do_parabolic_minimum_search    = .true. ! = default
  inv_settings%max_nr_of_solutions            = 4 ! = default
  ! switch to Marcos's code (is default)
! inv_settings%experimental_ers_wqc_checks    = .true. ! NOT default
  inv_settings%use_meas_sigma0_for_normfactor = .false. !not used by ERS
  inv_settings%use_which_fn_to_construct_LUT  = use_cmod5_to_construct_LUT
  inv_settings%first_warning_only             = .true. ! = default
  inv_settings%do_input_data_check            = .true. ! = default
#endif

  call set_inv_settings(inv_settings)

  call test_one_node(inv_settings, inv_input, inv_output)
  call save_node_results("testfile_inv_result.dat", inv_output)
  call calc_and_save_cone_dist("testfile_conedist1.dat", inv_input, inv_settings)
  call calc_and_save_winddir_conedist("testfile_conedist2.dat", inv_input, inv_settings)

  filename = "inversion_settings.dat"
  fileunit = get_lun()
  open(unit=fileunit,file=trim(filename),status='replace',&
       form="FORMATTED",ERR=99)
  call write_invsettings_to_file(fileunit)
  close(fileunit)
  call free_lun(fileunit)

  stop

  !*** display error and stop if no file could be made
99 print *,'ERROR while creating the output file:  ',trim(filename)
  print *,'there is probably already a file with this name !!'
  print *,'*** ... quitting ... ***'
  stop

end program inv_test
  !  #]
!-----------------------------------------------------------------------
subroutine test_one_node(inv_settings,inv_input,inv_output)
  !  #[

  use inversion, only: inv_input_type, inv_output_type, inv_settings_type
  use inversion, only: invert_one_wvc
  use inversion, only: wqc_ok, print_wind_quality_code
  use inversion, only: print_input_data_of_inversion
  use inversion, only: print_output_data_of_inversion
  use inversion, only: instrument_type_ers, instrument_type_seawinds
  use post_inversion, only: normalise_conedist_prescat_mode
  use numerics, only: l_

  IMPLICIT NONE             ! no implicit variable typing

  ! data structures needed by the inversion routine
  type(inv_settings_type) :: inv_settings
  type(inv_input_type)    :: inv_input
  type(inv_output_type)   :: inv_output

  print *,'************************'
  print *,'***   before invert  ***'
  print *,'************************'
  call print_input_data_of_inversion(inv_input)

  call invert_one_wvc(inv_input, inv_output)
  if (inv_settings%instrument_type .eq. instrument_type_ers) then
    call normalise_conedist_prescat_mode(inv_settings,inv_input,inv_output)
  endif

  print *,'************************'
  print *,'***   after invert   ***'
  print *,'************************'

  call print_output_data_of_inversion(inv_output)

end subroutine test_one_node

  !  #]
!-----------------------------------------------------------------------
subroutine save_node_results(filename,inv_output)
  !  #[ output of this routine is used by my idl plotting program
  use inversion,  only: inv_output_type, my_exit
  use convert,    only: speeddir_to_u, speeddir_to_v
  use LunManager, only: get_lun, free_lun

  IMPLICIT NONE             ! no implicit variable typing

  character(len=*),intent(in)  :: filename
  type(inv_output_type)        :: inv_output
  integer                      :: fileunit, sol

  print *,'opening file: ',trim(filename),' for writing inversion results'
  fileunit = get_lun()
  open(unit=fileunit,file=trim(filename),status='new',action='write',&
       form="FORMATTED",ERR=99)

! these 2 numbers are used to plot a set of blue crossing lines
! in my idl plotting program.
! When needed, set them to the NWP field or the simulated input windvector
! For the moment they are set to bogus values, and not used.
#define TESTWINDSPEED -999.
#define TESTWINDDIR   0.

  write(fileunit,*) &
        speeddir_to_u(TESTWINDSPEED,TESTWINDDIR), &
        speeddir_to_v(TESTWINDSPEED,TESTWINDDIR), &
        inv_output%nr_of_windsolutions, &
        inv_output%wind_quality_code
  do sol = 1,inv_output%nr_of_windsolutions
     write(fileunit,*) &
          speeddir_to_u(inv_output%foundwindspeed(sol),&
                        inv_output%foundwinddir(  sol)  ),&
          speeddir_to_v(inv_output%foundwindspeed(sol),&
                        inv_output%foundwinddir(  sol)  ),&
          inv_output%foundwindspeed(       sol),&
          inv_output%foundwinddir(         sol),&
          inv_output%conedistance_measured(sol)
  enddo

  close(fileunit)
  call free_lun(fileunit)
  return

    !*** display error and stop if no file could be made
99  print *,'ERROR while creating the output file:  ',trim(filename)
  print *,'there is probably already a file with this name !!'
  print *,'*** ... quitting ... ***'
  call my_exit

end subroutine save_node_results
  !  #]
!-----------------------------------------------------------------------
subroutine calc_and_save_cone_dist(filename, inv_input, inv_settings)
  !  #[ output of this routine is used by my idl plotting program
  ! a subroutine which scans the wind velocity and direction
  ! and calculates the conedistance for each of the scanned points,
  ! and than saves the results to file (for plotting purposes)

  use inversion, only: inv_input_type, inv_settings_type
  use inversion, only: calc_cone_distance, my_exit
  use inversion, only: calc_normalisation
  use inversion, only: convert_sigma_to_zspace
  use inversion, only: inv_output_type, find_minimum_cone_dist
  use inversion, only: instrument_type_ers, instrument_type_seawinds
  use post_inversion, only: normalise_conedist_prescat_mode
  use LunManager, only: get_lun, free_lun
  use numerics, only:l_

  IMPLICIT NONE             ! no implicit variable typing

  integer :: fileunit
  character(len=*),intent(in)  :: filename

  ! settings for the calc_and_save_cone_dist subroutine
  ! which determine how the v-phi plane is scanned
  real(l_),parameter :: CPLOT_max_windvelocity = MAXWINDSP
  real(l_),parameter :: CPLOT_min_windvelocity = MINWINDSP
  integer, parameter :: CPLOT_nr_of_windspeed_steps = WINDSPSTEPS
  integer, parameter :: CPLOT_nr_of_winddir_steps = WINDDIRSTEPS
  real(l_),parameter :: CPLOT_windspeedstep  = &
                           (MAXWINDSP-MINWINDSP)/ WINDSPSTEPS ! [m/s]
  real(l_),parameter :: CPLOT_winddirstep    = &
                           360.0_l_/CPLOT_nr_of_winddir_steps ! degrees
  integer            :: i, j
  real(l_)           :: windspeed, winddirection, cone_dist
  type(inv_input_type)    :: inv_input
  type(inv_settings_type) :: inv_settings

  ! for local use only
  type(inv_output_type) :: temp_inv_output

  print *,'opening file: ',trim(filename),' for writing cone_distances'
  fileunit = get_lun()
  open(unit=fileunit,file=trim(filename),status='new',&
       form="FORMATTED",ERR=99)
  ! write to the file
  write(fileunit,"(I5,F10.5)") CPLOT_nr_of_winddir_steps,   &
       CPLOT_winddirstep
  write(fileunit,"(I5,F10.5)") CPLOT_nr_of_windspeed_steps, &
       CPLOT_windspeedstep
  write(fileunit,"(F12.5)") CPLOT_min_windvelocity

  if (inv_settings%use_zspace) then
     call convert_sigma_to_zspace(inv_input)
  endif

  if (inv_settings%use_meas_sigma0_for_normfactor) then
     call calc_normalisation(inv_input)
  endif

  ! needed for skill calculation of ERS
  temp_inv_output%mean_cone_distance = 0.0

  ! calculate mean cone distance first if necessary
  if (inv_settings%instrument_type .eq. instrument_type_ers) then
     ! scan over all wind directions
     do i = 1,CPLOT_nr_of_winddir_steps
        ! calculate the current wind direction
        winddirection = CPLOT_winddirstep * i
        ! find windspeed that has minimum cone_distance for this direction
        ! and save those values for later use.
        call find_minimum_cone_dist(inv_input, winddirection,windspeed,cone_dist)
        temp_inv_output%mean_cone_distance= temp_inv_output%mean_cone_distance + cone_dist
     enddo
     temp_inv_output%mean_cone_distance= temp_inv_output%mean_cone_distance / CPLOT_nr_of_winddir_steps
  endif


  ! scan over all wind directions
  do i = 1,CPLOT_nr_of_winddir_steps
     winddirection = CPLOT_winddirstep * i

     ! scan wind speed for this direction and find minimum distance to cone
     do j=1,CPLOT_nr_of_windspeed_steps
        windspeed = CPLOT_min_windvelocity+CPLOT_windspeedstep * j
        cone_dist = calc_cone_distance(inv_input,windspeed,winddirection)

        ! do the ERS specific scaling
        ! the temp_inv_output data struct is used to communicate with
        ! the normalise_conedist_prescat_mode() subroutine, and has in this case
        ! no relation to the rest of the inversion code.
        if (inv_settings%instrument_type .eq. instrument_type_ers) then
           temp_inv_output%nr_of_windsolutions      = 1
           temp_inv_output%foundwindspeed(1)        = windspeed
           temp_inv_output%conedistance_measured(1) = cone_dist
           call normalise_conedist_prescat_mode(inv_settings,inv_input,temp_inv_output)
           cone_dist = temp_inv_output%conedistance_measured(1)
        endif
        write(fileunit,*) cone_dist ! write result to file
     enddo ! windspeed
  enddo ! wind_direction

  close(fileunit)
  call free_lun(fileunit)
  return

  !*** display error and stop if no file could be made
99 print *,'ERROR while creating the output file:  ',trim(filename)
  print *,'there is probably already a file with this name !!'
  print *,'*** ... quitting ... ***'
  call my_exit

end subroutine calc_and_save_cone_dist
  !  #]
!--------------------------------
subroutine calc_and_save_winddir_conedist(filename, inv_input, inv_settings)
  !  #[ output of this routine is used by my idl plotting program
  ! a subroutine which scans the wind direction
  ! and calculates for each of the scanned directions,
  ! the velocity that has a minimum conedistance
  ! and than saves the results to file (for plotting purposes)

  use inversion, only: inv_input_type, inv_output_type, inv_settings_type
  use inversion, only: my_exit, find_minimum_cone_dist
  use inversion, only: calc_normalisation
  use inversion, only: convert_sigma_to_zspace
  use inversion, only: instrument_type_ers, instrument_type_seawinds
  use post_inversion, only: normalise_conedist_prescat_mode
  use LunManager, only: get_lun, free_lun
  use numerics, only:l_

  IMPLICIT NONE             ! no implicit variable typing

  ! settings for the calc_and_save_cone_dist subroutine
  ! which determine how the v-phi plane is scanned
  real(l_),parameter :: CPLOT_max_windvelocity = MAXWINDSP
  real(l_),parameter :: CPLOT_min_windvelocity = MINWINDSP
  integer, parameter :: CPLOT_nr_of_windspeed_steps = WINDSPSTEPS
  integer, parameter :: CPLOT_nr_of_winddir_steps = WINDDIRSTEPS
  real(l_),parameter :: CPLOT_windspeedstep  = &
                           (MAXWINDSP-MINWINDSP)/ WINDSPSTEPS ! [m/s]
  real(l_),parameter :: CPLOT_winddirstep    = &
                           360.0_l_/CPLOT_nr_of_winddir_steps ! degrees

  character(len=*),intent(in) :: filename
  type(inv_input_type)        :: inv_input
  type(inv_settings_type)     :: inv_settings
  ! for local use only
  type(inv_output_type)   :: temp_inv_output
  integer :: fileunit
  integer            :: i
  real(l_)           :: winddirection, windspeed, cone_dist
  real(l_), dimension(0:CPLOT_nr_of_winddir_steps+1) :: conedistarr
  real(l_), dimension(1:CPLOT_nr_of_winddir_steps  ) :: windspeedarr

  if (inv_settings%use_zspace) then
     call convert_sigma_to_zspace(inv_input)
  endif

  if (inv_settings%use_meas_sigma0_for_normfactor) then
     call calc_normalisation(inv_input)
  endif

  ! needed for skill calculation of ERS
  temp_inv_output%mean_cone_distance = 0.0

  ! calculate mean cone distance first if necessary
  if (inv_settings%instrument_type .eq. instrument_type_ers) then
     ! scan over all wind directions
     do i = 1,CPLOT_nr_of_winddir_steps
        ! calculate the current wind direction
        winddirection = CPLOT_winddirstep * i
        ! find windspeed that has minimum cone_distance for this direction
        ! and save those values for later use.
        call find_minimum_cone_dist(inv_input, winddirection,windspeed,cone_dist)
        temp_inv_output%mean_cone_distance= temp_inv_output%mean_cone_distance + cone_dist
     enddo
     temp_inv_output%mean_cone_distance= temp_inv_output%mean_cone_distance / CPLOT_nr_of_winddir_steps
  endif

  ! scan over all wind directions
  do i = 1,CPLOT_nr_of_winddir_steps
     winddirection = CPLOT_winddirstep * i
     ! find windspeed that has minimum cone_distance for this direction
     ! and save those values for later use.
     call find_minimum_cone_dist(inv_input,winddirection,windspeed,cone_dist)

     ! do the ERS specific scaling
     ! the temp_inv_output data struct is used to communicate with
     ! the normalise_conedist_prescat_mode() subroutine, and has in this case
     ! no relation to the rest of the inversion code.
     if (inv_settings%instrument_type .eq. instrument_type_ers) then
        temp_inv_output%nr_of_windsolutions      = 1
        temp_inv_output%foundwindspeed(1)        = windspeed
        temp_inv_output%conedistance_measured(1) = cone_dist
        call normalise_conedist_prescat_mode(inv_settings,inv_input,temp_inv_output)
        cone_dist = temp_inv_output%conedistance_measured(1)
     endif
     windspeedarr(i) = windspeed
     conedistarr(i) = cone_dist
  enddo ! wind_direction

  ! save the results to file
  print *,'opening file: ',trim(filename),' for writing optimum windvalues'
  fileunit = get_lun()
  open(unit=fileunit,file=trim(filename),status='new',&
       form="FORMATTED",ERR=98)
  write(fileunit,'(I5)') CPLOT_nr_of_winddir_steps
  do i = 1,CPLOT_nr_of_winddir_steps
     write(fileunit,*) windspeedarr(i),' ',&
          conedistarr(i),' ',CPLOT_winddirstep * i
  enddo ! wind_direction
  close(fileunit)
  call free_lun(fileunit)

  return

  !*** display error and stop if no file could be made
98 print *,'ERROR while creating the output file:  ',trim(filename)
  print *,'there is probably already a file with this name !!'
  print *,'*** ... quitting ... ***'
  call my_exit

  end subroutine calc_and_save_winddir_conedist
  !  #]
!-----------------------------------------------------------------------
