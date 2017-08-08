PROGRAM rfscat ! main program
  !  #[ Documentation

  !---------------------------------------------------
  ! a program to simulate the scatterometer satellite
  ! instruments, including the rotating fanbeam (rfscat)
  ! concept, but provided a suitable Pseudo Level 1B is
  ! used it can also simulate Quikscat and ERS/ASCAT like
  ! instruments.
  !---------------------------------------------------
  ! Written 2000-2013 by  Jos de Kloe, KNMI.
  ! 
  ! Modifications are registered in our revision control
  ! systems (SVN upto 2012, mercurial starting 2013)
  !---------------------------------------------------

  !  #]
  !  #[ Modules used

  ! some helper routines from genscat
  USE numerics, only: r4_, missing_real, r8_
  USE lunmanager, only: get_lun, free_lun
  USE errorhandler, only: no_error, program_abort, error_programming, &
       error_reading_file
  USE convert, only: deg2rad, my_arctan, speeddir_to_u, speeddir_to_v, &
       uv_to_speed, uv_to_dir

  ! a module to define the satellite+antenna geometry
  USE geometry, only: wvc_type, init_wvc, delete_wvc, view_type, init_view

  USE pseudo_l1b_module, only: instrument_def_type, init_instrument_def, &
       load_instrument_def_from_pl1b, delete_instrument_def, &
       get_nr_nodes, get_next_wvc_instr_def

  ! a module to produce a simulated windfield
  USE simwind, only: wind_type, &
       init_simwind, close_simwind, get_nr_of_winds, get_next_u_v_lat_lon, &
       calc_noise_due_to_windvar, add_noise_to_windfield, &
       wind_source_from_windfile, wind_source_u_v_grid, &
       wind_source_sp_dir_grid, wind_source_random, &
       wind_source_constant_vector, wind_source_constant_speed

  ! a module containing the inversion routines
  USE inversion, only: &
       one_over_LUT_windspeed_step, one_over_LUT_theta_step, &
       c_po_pol, ku_po_pol, get_dynamic_range, &
       LUT_nr_windspeed_steps, LUT_min_windspeed, LUT_windspeed_step, &
       LUT_nr_theta_steps,     LUT_min_theta,     LUT_theta_step, &
       LUT_nr_winddir_steps, &
       write_invsettings_to_file, calc_sigma0, &
       c_vh_pol, ku_vh_pol, c_vv_dop, c_hh_dop, &
       invert_one_wvc, inv_input_type, &
       print_input_data_of_inversion, print_output_data_of_inversion, &
       print_in_out_data_of_inversion, my_mod, &
       wqc_notok_nodirfound, calc_cone_distance
       !calc_and_save_cone_dist, calc_and_save_winddir_conedist

  ! a module containing some data arrays for storage
  USE keepbinnedvars, only: init_bins, add_wvc_to_bins, write_bins_to_file

  ! a module to handle generation of random numbers
  USE random, only: gaussian_rnd_nr, save_random_state_to_file, &
       load_random_state_from_file, init_random_status, default_seed

  ! a module to define the ascii vector output
  USE ascii_output, only: open_ascii_outputfile, &
       write_to_ascii_outputfile, close_ascii_outputfile, &
       open_minimal_ascii_outputfile, write_to_minimal_ascii_outpfile, &
       close_minimal_ascii_outputfile

  ! a module to hold all simulation settings
  USE simulation_settings, only: &
       random_seed, save_random_state, random_status_file, &
       write_settings_to_file, init_simulation_settings, &
       check_and_set_derived_settings, process_commandline_settings, &
       outputfile_settings, outputfile_binned, &
       outputfile_vector, outputfile_vector_minimal, &
       save_vector_results, save_minimal_vector_results, &
       save_binned_results, save_settings,&
       wvc_resolution, silent, &
       nr_winds_default, set_nr_winds_to_nr_nodes, set_nr_winds_manually, &
       nr_winds_manual, wind_source, wind_distribution, &
       add_noise_to_input_windfield, &
       windfilename, stdev_for_windfield_noise, use_latlon_from_windfile, &
       use_2nd_pseudo_L1B_file, use_3rd_pseudo_L1B_file, &
       pl1b_filename, pl1b_filename2, pl1b_filename3, &
       u_startval,v_startval,u_step,v_step,nr_of_u_steps,nr_of_v_steps,&
       sp_startval,dir_startval,sp_step,dir_step,nr_of_sp_steps,&
       nr_of_dir_steps,gaussian_dist_halfwidth,constant_u_input,&
       constant_v_input,constant_speed_input, &
       geo_noise_model, add_geo_noise, add_instr_noise, noise_add_method, &
       noise_add_method_multiplicative, noise_add_method_sqrt_sum, &
       constant_instr_noise, instr_noise_method, instr_noise_method_constant, &
       ocean_current_constant_u_input, ocean_current_constant_v_input, &
       doppler_base_frequency

!  USE figure_of_merit, only: calc_FoM_and_FoM_amb, calc_FoM_and_FoM_amb2, &
!       calc_FoM_old, generic_MLE_to_probability
  
  USE timers, only: timer_type, init_timer, start_timer, stop_timer, &
                    get_num_calls, get_total_time, get_avg_time
                    !print_timer_result

  !  #]
  !  #[ Variables and parameters
  IMPLICIT NONE     ! no implicit variable typing 

  ! local variables for main
  integer :: i,nws,nws_to_use,nr_nodes
  integer :: error_flag
  integer :: nr_of_simulated_winds
  real(r4_) :: u,v,lat,lon
  integer :: nr_of_views,node_nr,wvc_column_number,wvc_row_number

  ! a counter to see howmany times the first rank equals 
  ! the closest solution.
  integer :: count_succes

  ! counters to track the number of warnings issued
  !          1234567890123456789012345678901
  integer :: num_warnings_missing_wind
  integer :: num_warnings_failed_noise_calc
  integer :: num_warnings_inv_failed
  integer :: num_warnings_inv_result_missing

  ! some settings to reduce the amount of stdout incase of warnings
  integer, parameter :: max_warnings_missing_wind = 10
  integer, parameter :: max_warnings_failed_noise_calc = 10
  integer, parameter :: max_warnings_inv_failed = 10
  integer, parameter :: max_warnings_inv_result_missing = 10

  ! a structure to keep all data for a given wvc
  TYPE(wvc_type) :: wvc

  ! a structure to store the windvectors to be used
  type(wind_type) :: winds   

  ! a pointer to an array that gives the geometry of each antenna view
  type(view_type), dimension(:), pointer :: view

  ! keeps pointers to the pseudo L1B file(s)
  ! and registers the last used node in the simulation
  type(instrument_def_type) :: instrument_def

  ! variable to allow chacking random_status_file existence
  logical :: exists

  ! for timing purposes
  type(timer_type) :: timer_all, timer_fom
  !  #]
  !  #[ Start the main program and do some initialisations
  call init_simulation_settings()
  call process_commandline_settings(error_flag)
  if (error_flag .ne. no_error) &
       call program_abort(error_flag,"process_commandline_settings")

  if (.not. silent) then
     print *,'================================================'
     print *,'Welcome to the KNMI rfscat simulation program   '
     print *,'================================================'
  end if

  ! init timers
  if (.not. silent) then
     call init_timer(timer_all)
     call init_timer(timer_fom)
     call start_timer(timer_all)
  end if

  call check_and_set_derived_settings(error_flag)
  if (error_flag .ne. no_error) &
       call program_abort(error_flag,"check_and_set_derived_settings")

  if (save_settings) then
     CALL write_settings_to_file(outputfile_settings,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"write_settings_to_file")
  end if

  call init_random_status(random_seed)
  IF (save_random_state) THEN
     ! if random state was saved, reload it
     inquire(file=trim(random_status_file),exist=exists)
     IF (exists) THEN
        ! this replaces the random_seed init result
        call load_random_state_from_file(random_status_file)
     END IF
  END IF

  ! initialise binning data structures
  call init_bins()

  ! initialise the instrument definition data structures
  call init_instrument_def(instrument_def)

  ! load the data from the pseudo_L1B file(s) into the
  !  instrument definition data structures
  call load_instrument_def_from_pl1b(instrument_def,&
                                     pl1b_filename,error_flag)
  if (error_flag .ne. no_error) &
       call program_abort(error_flag,"load_instrument_def_from_pl1b")

  if (use_2nd_pseudo_L1B_file) then
     call load_instrument_def_from_pl1b(instrument_def,&
                                        pl1b_filename2,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"load_instrument_def_from_pl1b")
  end if

  if (use_3rd_pseudo_L1B_file) then
     call load_instrument_def_from_pl1b(instrument_def,&
                                        pl1b_filename3,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"load_instrument_def_from_pl1b")
  end if

  ! request nr of nodes defined in the pseudo L1B file
  ! or the minimum nr of nodes defined in all pseudo L1B files
  ! in case several files are used.
  call get_nr_nodes(instrument_def,nr_nodes,error_flag)
  if (error_flag .ne. no_error) &
          call program_abort(error_flag,"get_nr_nodes")

  ! when geometry info is read from the pseudoL1b file
  ! then the windvector file controls the numbers of simulations.
  ! for every windvector a new geometry is read, and when the
  ! end of the geometry file is reached, it is read again
  ! from the beginning, etc.

  ! NOTE: set nr_winds_manual to -1 to let nws_to_use
  !       be read from the input windfile instead
  ! NOTE2: this nws_to_use setting has no effect when you
  !        request winds defined on a regular u,v or sp,dir grid
  nws_to_use = nr_winds_default
  if (set_nr_winds_to_nr_nodes) nws_to_use = nr_nodes
  if (set_nr_winds_manually)    nws_to_use = nr_winds_manual

  select case (wind_source)
  case(wind_source_u_v_grid)
     call init_simwind(winds,&
                       wind_source=wind_source_u_v_grid,&
                       error_flag=error_flag,&
                       u_startval=u_startval,&
                       v_startval=v_startval,&
                       u_step=u_step,&
                       v_step=v_step,&
                       nr_of_u_steps=nr_of_u_steps,&
                       nr_of_v_steps=nr_of_v_steps,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"1:init_simwind")
  case(wind_source_sp_dir_grid)
     call init_simwind(winds,&
                       wind_source=wind_source_sp_dir_grid,&
                       error_flag=error_flag,&
                       sp_startval=sp_startval,&
                       dir_startval=dir_startval,&
                       sp_step=sp_step,&
                       dir_step=dir_step,&
                       nr_of_sp_steps=nr_of_sp_steps,&
                       nr_of_dir_steps=nr_of_dir_steps,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"2:init_simwind")
  case(wind_source_from_windfile)
     call init_simwind(winds,&
                       wind_source=wind_source_from_windfile,&
                       error_flag=error_flag,&
                       num_winds=nws_to_use,&
                       windfilename=windfilename,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"3:init_simwind")
  case(wind_source_random)
     call init_simwind(winds,&
                       wind_source=wind_source_random,&
                       error_flag=error_flag,&
                       wind_distribution=wind_distribution,&
                       num_winds=nws_to_use,&
                       gaussian_dist_halfwidth=gaussian_dist_halfwidth,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"4:init_simwind")
  case(wind_source_constant_vector)
     call init_simwind(winds,&
                       wind_source=wind_source_constant_vector,&
                       error_flag=error_flag,&
                       num_winds=nws_to_use,&
                       constant_u_input=constant_u_input,&
                       constant_v_input=constant_v_input,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"5:init_simwind")
  case(wind_source_constant_speed)
     call init_simwind(winds,&
                       wind_source=wind_source_constant_speed,&
                       error_flag=error_flag,&
                       num_winds=nws_to_use,&
                       constant_speed_input=constant_speed_input,&
                       silent=silent)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"6:init_simwind")
  case default
     print *,"Undefined wind source"
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"rfscat_simulation: main")
  end select

  call get_nr_of_winds(winds,nws,error_flag)
  if (error_flag .ne. no_error) &
       call program_abort(error_flag,"get_nr_of_winds")

  if (save_vector_results) then
     call open_ascii_outputfile(outputfile_vector,nws,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"open_ascii_outputfile")
  end if

  if (save_minimal_vector_results) then
     call open_minimal_ascii_outputfile(outputfile_vector_minimal,&
                                        nws,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"open_minimal_ascii_outputfile")
  end if

  !  #]
  !  #[ init some counters before entering the loop
  count_succes = 0
  nr_of_simulated_winds = 0
  num_warnings_missing_wind = 0
  num_warnings_failed_noise_calc = 0
  num_warnings_inv_failed = 0
  num_warnings_inv_result_missing = 0
  !  #]
  windvectorloop: DO i = 1,nws
     !  #[ Do the actual simulations

     call init_wvc(wvc)
     nullify(view)

     ! get the instrument definition for the current wvc from the
     ! data structure in which the Pseudo L1B files have been stored
     ! (automatically steps to the next cell/node definition,
     !  and wraps around if more winds are simulated than we have 
     !  cell/node definitions)
     call get_next_wvc_instr_def(instrument_def,lat,lon,nr_of_views,&
                                 node_nr,wvc_column_number,wvc_row_number,&
                                 view,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"get_next_wvc_instr_def")

     wvc%lat               = lat
     wvc%lon               = lon
     wvc%nr_of_views       = nr_of_views
     wvc%node_nr           = node_nr
     wvc%wvc_column_number = wvc_column_number
     wvc%wvc_row_number    = wvc_row_number
     wvc%view              => view

     ! get the net wind vector (u,v) to use as input for the simulation,
     ! depending on the settings choosen above.
     ! NOTE that lat,lon are only returned when winds are read from
     ! an ascii windfile. In all other cases they will be set to missing 
     call get_next_u_v_lat_lon(winds,u,v,lat,lon,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"get_next_u_v_lat_lon")
     wvc%inp_u = u
     wvc%inp_v = v
     wvc%inp_speed = uv_to_speed(u,v) ! in [m/s]
     wvc%inp_dir   = uv_to_dir(u,v) ! in degrees

     ! save unmodified values, since the above values might
     ! be modified in a call to add_noise_to_windfield
     wvc%inp_u_orig     = wvc%inp_u
     wvc%inp_v_orig     = wvc%inp_v
     wvc%inp_speed_orig = wvc%inp_speed
     wvc%inp_dir_orig   = wvc%inp_dir

     if (use_latlon_from_windfile) then
        wvc%lat = lat
        wvc%lon = lon
     end if

     IF ( missing_real(wvc%inp_u) .or. missing_real(wvc%inp_v)) THEN
        num_warnings_missing_wind = num_warnings_missing_wind + 1
        if (num_warnings_missing_wind .le. max_warnings_missing_wind) then
           print *,"WARNING: missing input wind vectors for node ",i
        end if
        if (num_warnings_missing_wind .eq. max_warnings_missing_wind) then
           print *,"==>Number of warnings passed maximum, "
           print *,"==>suppressing any following warnings of this type"
        end if
        call delete_wvc(wvc)
        cycle windvectorloop
     END IF



     CALL calc_nrcs_values(wvc)

     CALL add_instr_and_geo_noise_to_nrcs(wvc,error_flag)
     if (error_flag .ne. no_error) then
        error_flag = no_error
        num_warnings_failed_noise_calc = num_warnings_failed_noise_calc + 1
        if (num_warnings_failed_noise_calc .le. max_warnings_failed_noise_calc) then
           print *,"WARNING: call to add_instr_and_geo_noise_to_nrcs failed"
           print *,"skipping further simulation of node: ",i
        end if
        if (num_warnings_failed_noise_calc .eq. max_warnings_failed_noise_calc) then
           print *,"==>Number of warnings passed maximum, "
           print *,"==>suppressing any following warnings of this type"
        end if
        call delete_wvc(wvc)
        cycle windvectorloop
     end if

     if (add_noise_to_input_windfield) &
          CALL add_noise_to_windfield(wvc%inp_u,wvc%inp_v,&
                                      wvc%inp_speed,wvc%inp_dir,&
                                      stdev_for_windfield_noise)


     ! do the actual inversion, and also calculate 
     ! the different Figure-of_Merit values
     CALL invert_nrcs_to_winds(wvc)

     !call start_timer(timer_fom)
     !call calculate_FoM(wvc)
     !call stop_timer(timer_fom)

     ! no result due to inversion failure
     IF (wvc%inv_output%wind_quality_code .eq. wqc_notok_nodirfound) THEN
        num_warnings_inv_failed = num_warnings_inv_failed + 1
        if (num_warnings_inv_failed .le. max_warnings_inv_failed) then
           print *,"WARNING1: inversion failed; no inversion result for node ",i
        end if
        if (num_warnings_inv_failed .eq. max_warnings_inv_failed) then
           print *,"==>Number of warnings passed maximum, "
           print *,"==>suppressing any following warnings of this type"
        end if
        call delete_wvc(wvc)
        cycle windvectorloop
     END IF

     ! no solutions (may happen in cases with just 1 or 0 views!)
     IF (wvc%inv_output%nr_of_windsolutions .eq. 0) THEN
        num_warnings_inv_result_missing = num_warnings_inv_result_missing + 1
        if (num_warnings_inv_result_missing .le. max_warnings_inv_result_missing) then
           print *,"WARNING2: no inversion result for node ",i
        end if
        if (num_warnings_inv_result_missing .eq. max_warnings_inv_result_missing) then
           print *,"==>Number of warnings passed maximum, "
           print *,"==>suppressing any following warnings of this type"
        end if
        call delete_wvc(wvc)
        cycle windvectorloop
     END IF
     
     ! count the number of successfully simulated nodes
     ! (do this AFTER the last cycle statement)
     nr_of_simulated_winds = nr_of_simulated_winds + 1

     call determine_closest_dir(wvc,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"determine_closest_dir")
     IF (wvc%index_closest .eq. 1) count_succes = count_succes + 1

     ! store the data
     if (save_binned_results) call add_wvc_to_bins(wvc)

     if (save_vector_results) then
        call write_to_ascii_outputfile(wvc,error_flag)
        if (error_flag .ne. no_error) &
             call program_abort(error_flag,"write_to_ascii_outputfile")
     end if

     if (save_minimal_vector_results) then
        call write_to_minimal_ascii_outpfile(wvc,error_flag)
        if (error_flag .ne. no_error) &
             call program_abort(error_flag,"write_to_minimal_ascii_outpfile")
     end if

     call delete_wvc(wvc)

     !  #]
  END DO windvectorloop ! loop over wind steps
  !  #[ Save results and close down

  if (.not. silent) then
     print *
     print *,'================================================'
     print *,"nr_of_simulated_winds    : ",nr_of_simulated_winds
     print *,"#times 1st rank==closest : ",count_succes
     print *,'================================================'
  end if

  CALL close_simwind(winds)

  if (save_vector_results) then
     CALL close_ascii_outputfile(nr_of_simulated_winds)
  end if

  if (save_minimal_vector_results) then
     CALL close_minimal_ascii_outputfile(nr_of_simulated_winds)
  end if

  if (save_binned_results) then
     call write_bins_to_file(outputfile_binned,error_flag)
     if (error_flag .ne. no_error) &
          call program_abort(error_flag,"write_bins_to_file")
  end if

  call  delete_instrument_def(instrument_def,error_flag)
  if (error_flag .ne. no_error) &
          call program_abort(error_flag,"delete_instrument_def")

  IF (save_random_state) THEN
     call save_random_state_to_file(random_status_file)
  END IF

  if (.not. silent) then
     call stop_timer(timer_all)
  end if

  if (.not. silent) then
     ! print timer results
     print *,"--------------"
     print *,"Timer results:"
     if (get_num_calls(timer_all) .eq. 0) then
        print "(a)","timer_all: [no data]"
     else
        print "(a,a,i6,a,f8.3,a,f8.3)","timer_all:",&
             " num_calls  = ",get_num_calls(timer_all),&
             " total_time = ",get_total_time(timer_all),&
             " avg_time   = ",get_avg_time(timer_all)
     end if
     if (get_num_calls(timer_fom) .eq. 0) then
        print "(a)","timer_fom: [no data]"
     else
        print "(a,a,i6,a,f8.3,a,f8.3)","timer_fom:",&
             " num_calls  = ",get_num_calls(timer_fom),&
             " total_time = ",get_total_time(timer_fom),&
             " avg_time   = ",get_avg_time(timer_fom)
        print *,"--------------"
     end if
  end if
  !  #]
contains
  !--------------------------------
  subroutine determine_closest_dir(wvc,error_flag)
    !  #[ 
    ! find closest solution using angle difference
    ! result is returned in wvc%index_closest
    TYPE(wvc_type), intent(inout) :: wvc        ! input/output
    integer,        intent(out)   :: error_flag ! output

    ! local variables
    integer   :: sol, cl
    real(r4_) :: closest, angle_diff
    
    ! init
    error_flag = no_error
    closest = 99999.0
    cl = -1
    wvc%index_closest = -1

    IF (wvc%inv_output%nr_of_windsolutions .gt. 0) THEN
       DO sol=1,wvc%inv_output%nr_of_windsolutions
          angle_diff = wvc%inp_dir_orig - &
                       wvc%inv_output%foundwinddir(sol)
          
          ! making sure closest angle difference is used
          if (angle_diff .gt.  180.0) angle_diff=(360.0-angle_diff)
          if (angle_diff .lt. -180.0) angle_diff=(360.0+angle_diff)
          
          IF (abs(angle_diff) .lt. closest) THEN 
             cl = sol
             closest = abs(angle_diff)
          END IF
       END DO

       IF (cl .eq. -1) THEN
          print *,'ALARM: no closest solution found !!!!!'
          print *,'some diagnostic info:'
          print *,'nr_of_windsolutions = ',wvc%inv_output%nr_of_windsolutions
          DO sol=1,wvc%inv_output%nr_of_windsolutions
             print *,'sol=',sol,' windspeed = ',wvc%inv_output%foundwindspeed(sol),&
                  ' winddir = ',wvc%inv_output%foundwinddir(sol)
          END DO
          error_flag = error_programming
          return
       END IF
    ELSE
       cl = -1
    END IF

    ! store the result
    wvc%index_closest = cl
    
    return
    
  end subroutine determine_closest_dir
    !  #]
  !--------------------------------
  subroutine calc_nrcs_values(wvc)
    !  #[
    ! calculate normalised radar cross-sections
    TYPE(wvc_type), intent(inout) :: wvc ! input/output

    integer   :: j   
    real(r4_) :: phi, this_antenna_dir, u_ant, v_ant
    real(r4_) :: doppler_df, mapped_doppler_speed
    real(r4_), parameter :: speed_of_light = 2.998e8 ! [m/s]

    ! remark: we require the angle convention for the antenna direction
    !         to use the oceanographic convention !

    ! For inversion a GMF is used which assumes a rel_winddir=0
    ! when the wind blows towards the antenna.
    ! So if the antenna uses oceanographic convention and wind uses
    ! meteorological convention, just subtracting the 2 numbers is
    ! sufficient to get the rel_winddir. This is what is used
    ! in this subroutine.

    ! Therefore the inversion_settings%antenna_angle_convention
    ! and inversion_settings%wind_angle_convention are forced to have
    ! the proper values in simulation_settings.F90 for now.

    DO j=1,wvc%nr_of_views
       this_antenna_dir = wvc%view(j)%antenna_dir
       phi = wvc%inp_dir - this_antenna_dir
       phi = my_mod(phi,360.)
       wvc%view(j)%sigma_0 = &
            calc_sigma0(wvc%inp_speed, phi,&
                        wvc%view(j)%theta, &
                        wvc%view(j)%polarisation)

       ! in case of Doppler, add the ocean current signal
       ! (assume this one is independant of the wind effect)
       IF  ( (wvc%view(j)%polarisation .eq. c_vv_dop) .or. &
             (wvc%view(j)%polarisation .eq. c_hh_dop)      ) THEN
          ! for this case sigma_0 contains the doppler shift!

          ! take the inproduct of the antenna pointing direction
          ! and the ocean current direction to get the magnitude
          ! of the Doppler caused by the ocean current
          u_ant = speeddir_to_u(1._r4_,this_antenna_dir)
          v_ant = speeddir_to_v(1._r4_,this_antenna_dir)
          mapped_doppler_speed = ocean_current_constant_u_input*u_ant + &
                                 ocean_current_constant_v_input*v_ant
          doppler_df = -2.*mapped_doppler_speed*doppler_base_frequency/&
                       (speed_of_light+2.*mapped_doppler_speed)
          wvc%view(j)%sigma_0 = wvc%view(j)%sigma_0 + doppler_df
          !print *,'DEBUG: wvc%view(j)%sigma_0 = ',wvc%view(j)%sigma_0
          !print *,'DEBUG: mapped_doppler_speed = ',mapped_doppler_speed
          !print *,'DEBUG: doppler_df = ', doppler_df
          !stop 1
       END IF

       ! store sigma_0_vh in the polarimetric case to calculate kp 
       IF (wvc%view(j)%polarisation .eq. c_po_pol) THEN
          wvc%view(j)%sigma_0_vh = &
               calc_sigma0(wvc%inp_speed, phi,&
                           wvc%view(j)%theta, &
                           c_vh_pol)
          ! I used c_vh_pol here, since I had a real polarimetric (i.e. phase
          ! shifting) LUT in the old rfscat project, but the corresponding
          ! KP parametrization depended on the c_vh_pol sigma0.
          ! Maria Belmonte had changed this one to this constant:
          !                c_po_pol)
          ! but I am not sure if she did this intentional.
       END IF
       ! store sigma_0_vh in the polarimetric case to calculate kp 
       IF (wvc%view(j)%polarisation .eq. ku_po_pol) THEN
          wvc%view(j)%sigma_0_vh = &
               calc_sigma0(wvc%inp_speed, phi,&
                           wvc%view(j)%theta, &
                           ku_vh_pol)
       END IF
       ! only needed if the doppler case needs extra
       ! inputs for noise calculations
       !IF (wvc%view(j)%polarisation .eq. c_vv_dop) THEN
       !   
       !END IF
       !IF (wvc%view(j)%polarisation .eq. c_hh_dop) THEN
       !   
       !END IF
    END DO
    
    return
  end subroutine calc_nrcs_values
    !  #]
  !--------------------------------
  subroutine add_instr_and_geo_noise_to_nrcs(wvc,error_flag)
    !  #[ add noise
    TYPE(wvc_type), intent(inout) :: wvc
    integer,        intent(out)   :: error_flag

    ! local variables
    real(r4_) :: geo_noise, instr_noise, combined_noise
    real(r4_) :: dynamic_range
    integer   :: j

    logical :: polarimetric_case

    ! init
    error_flag = no_error
    instr_noise = 0._r8_
    geo_noise = 0._r8_

    ! calc geophysical noise. This is taken to be the same for all views
    if (add_geo_noise) &
         geo_noise = calc_noise_due_to_windvar(wvc%inp_speed,wvc_resolution,&
                                               geo_noise_model)
    
    DO j=1,wvc%nr_of_views

       polarimetric_case = .false.
       if ( (wvc%view(j)%polarisation .eq. c_po_pol) .or. &
            (wvc%view(j)%polarisation .eq. ku_po_pol)     ) then
          polarimetric_case = .true.
       end if

       ! according the email by Sven d.d. 4-7-2002 I switched the order
       ! in witch kp and geo_noise are added. So now geo_noise is first:
       
       ! after the email and telephone discussion d.d. 15-7-2002 
       ! I changed the kp calculation for the polarimetric case.
       ! Now the sigma_0 value for the VH measurement is used to
       ! calculate an estimate of the polarimetric error.
       
       IF (polarimetric_case) &
            dynamic_range = determine_dynamic_range(wvc,j)

       select case(noise_add_method)
       case(noise_add_method_multiplicative)
          !  #[
          ! first add wind variability to sigma0
          if (add_geo_noise) &
               call apply_noise(wvc,j,geo_noise,polarimetric_case,&
                                dynamic_range,wvc%view(j)%sigma_0)
          
          ! then calc. the instr. noise using the sigma0 including geo_noise
          if (add_instr_noise) then
             call calc_instr_noise(wvc,j,polarimetric_case,instr_noise,&
                                   error_flag)
             if (error_flag .ne. no_error) return
          end if
          
          ! then add the instrument noise
          if (add_instr_noise) &
               call apply_noise(wvc,j,instr_noise,polarimetric_case,&
                                dynamic_range,wvc%view(j)%sigma_0)
          !  #]
       case(noise_add_method_sqrt_sum)
          !  #[
          combined_noise = geo_noise
          
          if (add_instr_noise) then
             call calc_instr_noise(wvc,j,polarimetric_case,instr_noise,&
                                   error_flag)
             if (error_flag .ne. no_error) return
             ! combine the noise by summing the squares of the kp
             ! (is the correct method according to Maria)
             combined_noise = sqrt(geo_noise**2+instr_noise**2)
          end if
          
          ! add the total noise to sigma0
          call apply_noise(wvc,j,combined_noise,polarimetric_case,&
                            dynamic_range,wvc%view(j)%sigma_0)
          !  #]
       case default
          !  #[ this code should never be reached
          print *,"ERROR in add_instr_and_geo_noise_to_nrcs:"
          print *,"unrecognised noise addition method code: ",&
               noise_add_method
          error_flag = error_programming
          return
          !  #]
       end select

       ! then fill in kp_a, kp_b, kp_c so that calc_var_s0
       ! can be used inside inversion to retrieve the kp value
       wvc%view(j)%kp_a = instr_noise**2 + 1
       wvc%view(j)%kp_b = 0.0
       wvc%view(j)%kp_c = 0.0
       
    END DO ! j
    return
    
  end subroutine add_instr_and_geo_noise_to_nrcs
    !  #]
  function determine_dynamic_range(wvc,j) result(dynamic_range)
    !  #[ determine dynamic range of LUT for polarimetric case
    TYPE(wvc_type), intent(in) :: wvc
    integer,        intent(in) :: j
    real(r4_) :: dynamic_range ! result

    ! the following arrays store the dynamic range of the LUT
    ! as a function of windspeed and incidence angle, 
    ! for the polarimetric case (needed in the kp calculation)
    real(r4_), save, dimension(0:LUT_nr_windspeed_steps-1,&
                               0:LUT_nr_theta_steps-1     ) :: max_c_pol_arr
    real(r4_), save, dimension(0:LUT_nr_windspeed_steps-1,&
                               0:LUT_nr_theta_steps-1     ) :: max_ku_pol_arr

    real(r4_) :: windspeed, theta
    integer   :: i,k
    logical, save :: first_call_c_pol = .true.
    logical, save :: first_call_ku_pol = .true.

    IF (wvc%view(j)%polarisation .eq. c_po_pol) THEN
       if (first_call_c_pol) then
          DO i=0,LUT_nr_windspeed_steps-1
             DO k=0,LUT_nr_theta_steps-1
                windspeed = LUT_min_windspeed + i*LUT_windspeed_step
                theta     = LUT_min_theta     + k*LUT_theta_step
                max_c_pol_arr(i,k) = &
                     get_dynamic_range(c_po_pol,windspeed,theta)
             END DO
          END DO
          first_call_c_pol = .false.
       end if
    END IF
    
    IF (wvc%view(j)%polarisation .eq. ku_po_pol) THEN
       if (first_call_ku_pol) then
          DO i=0,LUT_nr_windspeed_steps-1
             DO k=0,LUT_nr_theta_steps-1
                windspeed = LUT_min_windspeed + i*LUT_windspeed_step
                theta     = LUT_min_theta     + k*LUT_theta_step
                max_ku_pol_arr(i,k) = &
                     get_dynamic_range(ku_po_pol,windspeed,theta)
             END DO
          END DO
          first_call_ku_pol = .false.
       end if
    END IF
    
    ! get the indices in the LUT for windspeed and theta (inc.angle)
    i = nint( (wvc%inp_speed - LUT_min_windspeed)*one_over_LUT_windspeed_step )
    IF (i .lt. 0) i = 0
    IF (i .gt. LUT_nr_windspeed_steps-1) i = LUT_nr_windspeed_steps-1
    
    k = nint( (wvc%view(j)%theta - LUT_min_theta)*one_over_LUT_theta_step)
    IF (k .lt. 0) k = 0
    IF (k .gt. LUT_nr_theta_steps-1) k = LUT_nr_theta_steps-1
    
    IF (wvc%view(j)%polarisation .eq. c_po_pol) THEN
       dynamic_range = max_c_pol_arr(i,k)
    END IF
    
    IF (wvc%view(j)%polarisation .eq. ku_po_pol) THEN
       dynamic_range = max_ku_pol_arr(i,k)
    END IF

  end function determine_dynamic_range
    !  #]
  subroutine calc_instr_noise(wvc,j,polarimetric_case,&
                              instr_noise,error_flag)
    !  #[ calculate instr_noise from n_eff, n_noise and snr_prime
    TYPE(wvc_type), intent(in)  :: wvc ! input
    integer,        intent(in)  :: j   ! input
    logical,        intent(in)  :: polarimetric_case ! input
    real(r4_),      intent(out) :: instr_noise ! result
    integer,        intent(out) :: error_flag  ! output

    ! local variables
    real(r4_) :: kp_sq, aaa, ref_sigma0
    integer, save :: num_warnings_issued = 0
    integer, parameter :: max_num_warnings = 10
    real(r4_),parameter :: min_allowed_sigma0 = 1.0e-15

    error_flag = no_error

    if (instr_noise_method .eq. instr_noise_method_constant) then
       instr_noise = constant_instr_noise
       return
    end if

    if ( (wvc%view(j)%nr_samples       .le. 0) .or. &
         (wvc%view(j)%nr_noise_samples .le. 0) .or. &
         (wvc%view(j)%snr_prime        .le. 0.0)    ) then
    !  #[ a sanity check
       num_warnings_issued = num_warnings_issued + 1
       if (num_warnings_issued .le. max_num_warnings) then
          print *,"WARNING in add_instr_and_geo_noise_to_nrcs:"
          print *,"Invalid data in noise calculation !!!"
          print *,"nr_samples = ",wvc%view(j)%nr_samples,&
               " nr_noise_samples = ",wvc%view(j)%nr_noise_samples,&
               " snr_prime = ",wvc%view(j)%snr_prime 
       end if
       if (num_warnings_issued .eq. max_num_warnings) then
          print *,"==>Number of warnings passed maximum, "
          print *,"==>suppressing any following warnings of this type"
       end if
       error_flag = error_reading_file
       return
    !  #]
    end if

    IF (polarimetric_case) THEN
       ref_sigma0 = wvc%view(j)%sigma_0_vh
    ELSE 
       ! note that this differs slightly from the old implementation
       ! since there the modified new_sigma0 was used after
       ! applying the geophysical noise
       ref_sigma0 = wvc%view(j)%sigma_0
    END IF          

    ! protect the formula below from zero new_sigma_0 values 
    ! (which may occur if the (geophysical) noise is (by accident) 
    ! equal to the sigma_0 value itself
          
    IF (abs(ref_sigma0) .lt. min_allowed_sigma0) THEN
       aaa = 1./(wvc%view(j)%snr_prime*min_allowed_sigma0)
    ELSE
       aaa = 1./(wvc%view(j)%snr_prime*ref_sigma0)
    END IF
    
    kp_sq = (1.+aaa)**2/wvc%view(j)%nr_samples + &
         (   aaa)**2/wvc%view(j)%nr_noise_samples
    
    instr_noise = sqrt(kp_sq)

  end subroutine calc_instr_noise
    !  #]
  subroutine apply_noise(wvc,j,noise,polarimetric_case,dynamic_range,sigma0)
    !  #[
    TYPE(wvc_type), intent(in)  :: wvc   ! input
    integer,        intent(in)  :: j     ! input
    real(r4_),      intent(in)  :: noise ! input 
    logical,        intent(in)  :: polarimetric_case ! input
    real(r4_),      intent(in)  :: dynamic_range     ! input
    real(r4_),      intent(inout) :: sigma0 ! modified on output

    if (polarimetric_case) then
       sigma0 = sigma0 + &
                dynamic_range * noise * real(gaussian_rnd_nr(),r4_)
    else
       sigma0 = sigma0 * &
                (1.0 + noise * real(gaussian_rnd_nr(),r4_) )
    end if

  end subroutine apply_noise
    !  #]
  !--------------------------------
  subroutine invert_nrcs_to_winds(wvc)
    !  #[ apply inversion
    TYPE(wvc_type), intent(inout) :: wvc ! input/output

    ! local variables
    ! integer :: i

    ! inputs used by the inversion routine
    TYPE(inv_input_type)  :: inv_input

    ! all data for input in this routine is stored in the wvc array
    ! that is defined by the geometry module
    IF (wvc%nr_of_views .gt. 0) THEN
       ! define inv_input
       call fill_inv_input_from_wvc(wvc,inv_input)
       
       ! a debug print
       !call print_input_data_of_inversion(inv_input)

       call invert_one_wvc(inv_input,wvc%inv_output) ! using the data in this_wvc

       ! no solutions (may happen in cases with just 1 or 0 views!)
       ! dont calculate the probability in that case!
       IF (wvc%inv_output%nr_of_windsolutions .eq. 0) return

       ! convert MLE to probabilities
!       do i=1,wvc%inv_output%nr_of_windsolutions
!          call generic_MLE_to_probability(wvc%inv_output%conedistance_measured(i),&
!                                          wvc%nr_of_views-2,&
!                                          wvc%inv_output%probability(i),&
!                                          error_flag)
!       end do
       
       ! a debug print
       !call print_output_data_of_inversion(wvc%inv_output)
       !call print_in_out_data_of_inversion(inv_input,wvc%inv_output)
       !stop 1
    ELSE
       wvc%inv_output%nr_of_windsolutions = 0
    END IF
    
    return
    
  end subroutine invert_nrcs_to_winds
    !  #]
  subroutine calculate_FoM(wvc)
    !  #[ calculate the different Figure-of_Merit values
    TYPE(wvc_type), intent(inout) :: wvc ! input/output

    ! inputs used by the inversion routine
    TYPE(inv_input_type)  :: inv_input

    ! temporary code
    !integer :: i

    ! all data for input in this routine is stored in the wvc array
    ! that is defined by the geometry module
    IF (wvc%nr_of_views .eq. 0) return

    ! define inv_input
    call fill_inv_input_from_wvc(wvc,inv_input)

    ! calculate the different Figure-of_Merit values
    ! [alternative implementation]
    !call calc_FoM_and_FoM_amb2(inv_input,wvc%inp_u_orig,wvc%inp_v_orig,&
    !                           wvc%FoM,wvc%FoM_amb,error_flag)

    ! calculate the different Figure-of_Merit values
    !call calc_FoM_and_FoM_amb(inv_input,wvc%inp_u_orig,wvc%inp_v_orig,&
    !                          wvc%FoM,wvc%FoM_amb,error_flag)
    
    ! a dummy routine for now
    !call calc_FoM_old(wvc%FOM_old)
    
    ! this one is not yet defined. Might be added in the future
    !wvc%FOM_skew
    
  end subroutine calculate_FoM
    !  #]
  subroutine fill_inv_input_from_wvc(wvc,inv_input)
    !  #[ 
    ! copy input data needed by the inversion and FoM routines
    ! from the wvc to the inv_input structure
    TYPE(wvc_type),       intent(in)  :: wvc       ! input
    TYPE(inv_input_type), intent(out) :: inv_input ! output

    ! local variables
    integer           :: i

    ! copy the data
    inv_input%nr_of_views = wvc%nr_of_views

    inv_input%pol(        1:wvc%nr_of_views) = wvc%view(1:wvc%nr_of_views)%polarisation
    inv_input%antenna_dir(1:wvc%nr_of_views) = wvc%view(1:wvc%nr_of_views)%antenna_dir
    inv_input%theta_angle(1:wvc%nr_of_views) = wvc%view(1:wvc%nr_of_views)%theta
    inv_input%sigma_0(    1:wvc%nr_of_views) = wvc%view(1:wvc%nr_of_views)%sigma_0
    inv_input%sigma_0_vh( 1:wvc%nr_of_views) = wvc%view(1:wvc%nr_of_views)%sigma_0_vh

    DO i=1,wvc%nr_of_views
       inv_input%kp_a(i) = wvc%view(i)%kp_a
       inv_input%kp_b(i) = wvc%view(i)%kp_b
       inv_input%kp_c(i) = wvc%view(i)%kp_c

       IF  ( (wvc%view(i)%polarisation .eq. c_vv_dop) .or. &
             (wvc%view(i)%polarisation .eq. c_hh_dop)      ) THEN
          inv_input%doppler_shift(i) = wvc%view(i)%sigma_0
       END IF
    END DO

  end subroutine fill_inv_input_from_wvc
    !  #]
  !--------------------------------
END program rfscat
