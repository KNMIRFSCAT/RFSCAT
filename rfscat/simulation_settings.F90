module Simulation_Settings
  !-------------------------------------------------------
  !  #[ Documentation
  !
  ! This module contains all settings used by the
  ! rfscat-simulation program, and some routines to allow
  ! reading and writing them to/from a file.
  !
  ! Written in 2000/2001/2002 by Jos de Kloe, KNMI
  ! 
  ! Modifications:
  ! 07-May-2009 J. de Kloe  converted include file simulate.h to this module
  ! 12-May-2009 J. de Kloe  added commandline handling and many clean-ups
  ! 19-May-2009 J. de Kloe  introduced settings input file and removed
  !                         most commandline switches
  ! 19-Jun-2009 J. de Kloe  added new setting get_all_winddirs
  ! 11-Sep-2009 J. de Kloe  add switches to allow use of minimal outputfile
  ! 12-Feb-2010 J. de Kloe  add switches related to noise models
  ! 24-Feb-2010 J. de Kloe  added noise_add_method switch
  !
  !  #]
  !-------------------------------------------------------
  !  #[ Modules used
  USE lunmanager, only: get_lun, free_lun, fileunit_stdout
  USE inversion, only: write_invsettings_to_file, &
       filename_c_vv,  filename_c_hh,  filename_c_pol, &
       filename_ku_vv, filename_ku_hh, filename_ku_pol, &
       inv_settings_type, get_inv_settings, set_inv_settings, &
       instrument_type_rfscat, init_inv_settings_to_default, &
       angle_convention_oceanographic, angle_convention_meteorological, &
       use_cmod5_to_construct_LUT
  USE compiler_features, only: iargc_genscat, getarg_genscat
  USE errorhandler, only: no_error, error_cmdline_options, &
       error_opening_file, error_writing_file, error_program_usage, &
       error_reading_file
  USE numerics, only: r4_
  USE stringtools, only: to_lowercase
  USE simwind, only: get_wind_source_code, get_wind_source_str, &
       get_wind_distr_code, get_wind_distr_str, &
       get_geo_noise_model_code, get_geo_noise_model_str, &
       wind_source_random, wind_distribution_Rayleigh, geo_noise_model_old
  !  #]
  !  #[ variables to hold all settings

  implicit none 
  
  !#########################################
  !# module variables to hold all settings #
  !#########################################

  ! defined in the same order as used in the example settings file
  ! (although this is not strictly necessary)

  !################################################
  !# definition of simulation inputs and settings #
  !################################################

  ! random number generation settings
  integer :: random_seed
  logical :: save_random_state
  character(len=256) :: random_status_file

  ! make stdout silent
  logical :: silent

  ! path and name of pseudo_level_1b file
  character(len=256) :: pseudo_L1b_file_path
  character(len=256) :: pseudo_L1b_file_name

  ! for some geometries, it may be convenient to be able to 
  ! combine 2 or 3 geometry files in one simulation:
  logical            :: use_2nd_pseudo_L1B_file
  character(len=256) :: pseudo_L1b_2nd_file_name
  logical            :: use_3rd_pseudo_L1B_file
  character(len=256) :: pseudo_L1b_3rd_file_name

  ! defines what wind source to use
  ! possible values are defined in the simwind module
  integer :: wind_source

  ! define a u,v grid for wind definition
  real(r4_) :: u_startval
  real(r4_) :: v_startval
  real(r4_) :: u_step
  real(r4_) :: v_step
  integer   :: nr_of_u_steps
  integer   :: nr_of_v_steps
  
  ! define a sp,dir grid for wind definition
  real(r4_) :: sp_startval
  real(r4_) :: dir_startval
  real(r4_) :: sp_step
  real(r4_) :: dir_step
  integer   :: nr_of_sp_steps
  integer   :: nr_of_dir_steps
  
  ! Define the Rayleigh distribution of winds
  real(r4_) :: gaussian_dist_halfwidth
  
  ! Define the Weibull distribution of winds:
  ! t.b.d.
  !real(r4_),parameter :: 
  !real(r4_),parameter :: 
  
  ! Define the u,v components in case of constant input wind vector
  real(r4_) :: constant_u_input
  real(r4_) :: constant_v_input

  ! define speed in case of constant input speed
  real(r4_) :: constant_speed_input
  
  ! minimum and maximum allowed wind
  ! these are imposed on all types of windsources
  ! random winds are only generated inside these boundaries
  ! winds from file or defined by a grid are discarded when
  ! they fall outside the allowed range
  ! The number of available winds is automatically 
  ! adapted accordingly.
  real(r4_) :: min_wind
  real(r4_) :: max_wind
  
  ! path and name of file to use for windvectors
  character(len=256) :: windfilename

  ! fill lat-lon info using the windfile as input
  ! (otherwise they are taken from the Pseudo L1B file)
  logical :: use_latlon_from_windfile

  ! take the nr. of nodes from the PseudoL1b file equal to
  ! the number of windvectors to process.
  logical :: set_nr_winds_to_nr_nodes

  ! manually set the number of windvectors to process
  logical :: set_nr_winds_manually

  ! number of winds to use, when setting manually
  integer :: nr_winds_manual

  ! defines what distribution to use for generating random winds
  ! possible values are defined in the simwind module
  integer :: wind_distribution

  ! add Gaussian distributed noise to the 2 wind components
  logical :: add_noise_to_input_windfield

  ! amount of noise to add to the windfield
  real(r4_) :: stdev_for_windfield_noise

  ! filter views with probably useless content, i.e. snr of 0 
  ! (seems to happen sometimes in the Pseudo L1B files of the 
  ! old rfscat project)
  logical :: dont_use_views_with_snr_0

  ! throw away solutions at or very close to the edge of the LUT
  ! since they probably originate from outside the LUT, and ended
  ! up there because of the safety checks inside the interpolate 
  ! routines. 
  ! (at the moment only the low windspeed edge is tested)
  logical :: exclude_LUT_edge_solutions

  ! resolution is used to scale the geophysical noise, which is cause
  ! by the wind variability within the wvc
  real(r4_) :: wvc_resolution

  ! add geophysical noise to the sigma0 values?
  logical :: add_geo_noise

  ! add instrumental noise to the sigma0 values?
  logical :: add_instr_noise

  ! how is the instrument noise determined?
  integer :: instr_noise_method

  ! possible noise_add_method settings
  integer, parameter :: instr_noise_method_undefined = -1
  integer, parameter :: instr_noise_method_kp        =  1
  integer, parameter :: instr_noise_method_constant  =  2
  
  ! default value in case constant instr. noise is choosen
  real(r4_) :: constant_instr_noise

  ! which geophysical noise model should be used?
  ! (see simwind.F90 for available implementations)
  integer :: geo_noise_model

  ! how to add the noise terms? 
  ! 
  ! Note that all studies upto 2010 have used the mutiplicative method. 
  ! The newly added sqrt_sum method has not yet been extensively tested 
  ! or validated. Choose one of [multiplicative|sqrt_sum]
  integer :: noise_add_method

  ! possible noise_add_method settings
  integer, parameter :: noise_add_method_undefined      = -1
  integer, parameter :: noise_add_method_multiplicative = 1
  integer, parameter :: noise_add_method_sqrt_sum       = 2

  !#########################################
  !# definition of inversion module inputs #
  !#########################################

  ! define the path and filename of the LUT files.
  character(len=256) :: LUT_path
  character(len=256) :: LUT_C_VV_filename
  character(len=256) :: LUT_C_HH_filename
  character(len=256) :: LUT_C_POL_filename
  character(len=256) :: LUT_KU_VV_filename
  character(len=256) :: LUT_KU_HH_filename
  character(len=256) :: LUT_KU_POL_filename

  ! when set to true, this forces all 144 probed wind solutions to
  ! be reported in the output file
  ! Beware this will make the output file very large when simulating
  ! a large number of nodes/winds
  logical :: get_all_winddirs

  real(r4_) :: polarimetric_weight
  real(r4_) :: doppler_weight

  ! in case of using doppler measurements, allow adding a fixed
  ! ocean current vector to the simulation to offset the doppler result
  ! ==>constant components for u,v to be used
  real(r4_) :: ocean_current_constant_u_input
  real(r4_) :: ocean_current_constant_v_input

  ! only used to take Doppler effect into account
  real(r4_) :: doppler_base_frequency

  !####################################
  !# definition of simulation outputs #
  !####################################

  ! output filenames
  character(len=256) :: outputfile_vector
  character(len=256) :: outputfile_vector_minimal
  character(len=256) :: outputfile_binned
  character(len=256) :: outputfile_settings
  
  ! enables writing to file of all processed vectors
  ! of one WVC (in ascii format)
  logical :: save_vector_results

  ! enables writing to file a few results of all processed vectors
  ! of one WVC (in ascii format)
  logical :: save_minimal_vector_results

  ! enables sorting the results in bins which are written to
  ! file, and used for making contourplots.
  logical :: save_binned_results

  ! enables writing to file of all settings
  logical :: save_settings

  ! defines that high resulation bins should be used to 
  ! collect the vector results. This gives a higher resolution
  ! for the contourplots that you can make with it. It also means
  ! that much more vectors should be calculated to fill the picture
  logical :: hi_res_bins

  !#########################################################
  !# end of variables used to store settings file settings #
  !#########################################################

  ! a small number, useful for testing during software development
  integer, parameter :: nr_winds_default = 1000

  ! variables to hold derived/composed settings
  character(len=256) :: pl1b_filename
  character(len=256) :: pl1b_filename2    
  character(len=256) :: pl1b_filename3

  ! Note that these LUT filenames are declared inside the inversion module
  ! and should not be declared again here:
  ! filename_c_vv, filename_c_hh, filename_c_pol
  ! filename_ku_vv, filename_ku_hh, filename_ku_pol

  !  #]
  !-------------------------------------------------------
contains
  !------------------------------------------
  subroutine init_simulation_settings()
    !  #[ 

    ! init all settings to default values

    random_seed                  = 123454
    save_random_state            = .false.
    random_status_file           = 'results/random_status_file.dat'

    silent                       = .false.

    pseudo_L1b_file_path         = "./pseudo_l1b_files/"
    pseudo_L1b_file_name         = "CV_a00_01_bestsystem.l1b"
    use_2nd_pseudo_L1B_file      = .false.
    pseudo_L1b_2nd_file_name     = "CH_a00_01_bestsystem.l1b"
    use_3rd_pseudo_L1B_file      = .false.
    pseudo_L1b_3rd_file_name     = "CP_a00_01_bestsystem.l1b"
    dont_use_views_with_snr_0    = .true.

    wind_source                  = wind_source_random
    u_startval                   = -25.0 ! m/s
    v_startval                   = -25.0 ! m/s
    u_step                       =   2.0 ! 0.5 !  2.0 ! 1.0!  m/s
    v_step                       =   2.0 ! 0.5 !  2.0 ! 1.0 ! m/s
    nr_of_u_steps                =    26 ! 101 !   26 !  51
    nr_of_v_steps                =    26 ! 101 !   26 !  51
    sp_startval                  = 0.2 ! m/s
    dir_startval                 = 0.0 ! deg
    sp_step                      = 0.2 ! 0.5 ! 1.0 ! 0.2 ! m/s
    dir_step                     = 1.0 ! 0.5 ! 2.0 ! 1.0 ! deg
    nr_of_sp_steps               = 125 !  50 !  25 ! 125
    nr_of_dir_steps              = 360 ! 720 ! 180 ! 360
    gaussian_dist_halfwidth      = 5.5
    ! Weibull settings
    ! t.b.d.
    constant_u_input             = 7.5 ! [m/s]
    constant_v_input             = 7.5 ! [m/s]
    constant_speed_input         = 7.5 ! [m/s]
    min_wind                     =  0.0 !   0.8 ! [m/s]
    max_wind                     = 25.0 !  25.0 ! [m/s]
    windfilename                 = "./windfile.dat"
    use_latlon_from_windfile     = .true.
    set_nr_winds_to_nr_nodes     = .true.
    set_nr_winds_manually        = .false.
    nr_winds_manual              = 25
    wind_distribution            = wind_distribution_Rayleigh
    add_noise_to_input_windfield = .false.
    stdev_for_windfield_noise    = 0.0 ! [m/s]

    dont_use_views_with_snr_0    = .true.
    exclude_LUT_edge_solutions   = .true.
    wvc_resolution               = 50 ! [km]

    add_geo_noise                = .true.
    add_instr_noise              = .true.
    instr_noise_method           = instr_noise_method_kp
    constant_instr_noise         = 0.05
    geo_noise_model              = geo_noise_model_old
    noise_add_method             = noise_add_method_multiplicative

    LUT_path                     = "nrcs_luts/"
    LUT_C_VV_filename            = "c_vv.dat"
    LUT_C_HH_filename            = "c_hh.dat"
    LUT_C_POL_filename           = "c_pol.dat"
    LUT_KU_VV_filename           = "ku_vv.dat"
    LUT_KU_HH_filename           = "ku_hh.dat"
    LUT_KU_POL_filename          = "ku_pol.dat"

    ! previously used LUT file names for Ku band
    ! LUT_KU_VV_FILENAME  "nscat2_250_73_51_vv.dat"
    ! LUT_KU_HH_FILENAME  "nscat2_250_73_51_hh.dat"
    ! LUT_KU_VV_FILENAME  "ku-vv2.dat"
    ! LUT_KU_VV_FILENAME  "qscat1_250_73_51_vv.dat"
    ! LUT_KU_HH_FILENAME  "ku-hh2.dat"
    ! LUT_KU_HH_FILENAME  "qscat1_250_73_51_hh.dat"

    get_all_winddirs             = .false.

    polarimetric_weight          = 1.0
    doppler_weight               = 1.0e5

    ocean_current_constant_u_input = 0.0
    ocean_current_constant_v_input = 0.0

    doppler_base_frequency       = 5.255e9 ! [Hz] for ascat/C-band

    outputfile_vector            = "result.dat"
    outputfile_vector_minimal    = "result_minimal.dat"
    outputfile_binned            = "result_binned.dat"
    outputfile_settings          = "settings.dat"
    save_vector_results          = .true.
    save_minimal_vector_results  = .true.
    save_binned_results          = .false.
    save_settings                = .true.
    hi_res_bins                  = .false.

    ! initialise the inversion module settings
    call init_inv_settings_to_default()

  end subroutine init_simulation_settings
    !  #]
  subroutine process_commandline_settings(error_flag)
    !  #[
    ! handle command line arguments
    integer, intent(out) :: error_flag ! output

    ! local variables
    character(len=256) :: settings_filename
    integer :: nr_of_arguments, current_argument_nr

    error_flag = no_error
    nr_of_arguments = iargc_genscat()
    if (nr_of_arguments .eq. 1) then
       current_argument_nr = 1
       call getarg_genscat(current_argument_nr,settings_filename)
       call load_rfscat_settings(settings_filename,error_flag)
       if (error_flag .ne. no_error) return
    else
       call print_usage()
       error_flag = error_cmdline_options
       return
    end if

    return

  end subroutine process_commandline_settings
    !  #]
  subroutine load_rfscat_settings(settings_filename,error_flag)
    !  #[
    ! load the input settings file
    character(len=*), intent(in)  :: settings_filename ! input
    integer,          intent(out) :: error_flag        ! output

    ! local variables
    character(len=256) :: line
    character(len=256) :: key
    character(len=256) :: strvalue
    logical            :: is_comment
    logical            :: exists
    integer            :: fileunit

    error_flag = no_error

    !print *,'testing for existence of file: ',trim(settings_filename)
    inquire(file=trim(settings_filename),exist=exists)
    IF (.not. exists) THEN
       print *,"ERROR in load_rfscat_settings:"
       print *,"Input settings file: ",trim(settings_filename)
       print *,"could not be found !!!"
       error_flag = error_reading_file
       return
    END IF

    ! open the settings file
    fileunit = get_lun()
    open(unit=fileunit,file=trim(settings_filename),status='old',&
         form="FORMATTED",ERR=888)
    
    ! loop over all lines in this file
    ! use the EOF condition to break this do loop
    settings_loop: do
       read(fileunit,"(a)",end=111,err=999) line
       
       call split_key_val(adjustl(line),is_comment,key,strvalue,error_flag)
       if (error_flag .ne. no_error) return

       if (is_comment) cycle settings_loop

       !print *,"line       = "//trim(line)
       !print *,"is_comment = ",is_comment
       !print *,"key        = "//trim(key)
       !print *,"strvalue      = "//trim(strvalue)

       select case (to_lowercase(trim(key)))
       case('random_seed')
          read(strvalue,*,err=770) random_seed
       case('save_random_state')
          call convert_str_to_bool(key,strvalue,save_random_state,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("random_status_file")
          random_status_file = strvalue
       case("silent")
          call convert_str_to_bool(key,strvalue,silent,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("pseudo_l1b_file_path")
          pseudo_L1b_file_path = strvalue
       case("pseudo_l1b_file_name")
          pseudo_L1b_file_name = strvalue
       case("use_2nd_pseudo_l1b_file")
          call convert_str_to_bool(key,strvalue,use_2nd_pseudo_L1B_file,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("pseudo_l1b_2nd_file_name")
          pseudo_L1b_2nd_file_name = strvalue
       case("use_3rd_pseudo_l1b_file")
          call convert_str_to_bool(key,strvalue,use_3rd_pseudo_L1B_file,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("pseudo_l1b_3rd_file_name")
          pseudo_L1b_3rd_file_name = strvalue
       case("wind_source")
          call get_wind_source_code(strvalue,wind_source,error_flag)
          if (error_flag .ne. no_error) return
       case("u_startval")
          read(strvalue,*,err=771) u_startval
       case("v_startval")
          read(strvalue,*,err=771) v_startval
       case("u_step")
          read(strvalue,*,err=771) u_step
       case("v_step")
          read(strvalue,*,err=771) v_step
       case("nr_of_u_steps")
          read(strvalue,*,err=770) nr_of_u_steps
       case("nr_of_v_steps")
          read(strvalue,*,err=770) nr_of_v_steps
       case("sp_startval")
          read(strvalue,*,err=771) sp_startval
       case("dir_startval")
          read(strvalue,*,err=771) dir_startval
       case("sp_step")
          read(strvalue,*,err=771) sp_step
       case("dir_step")
          read(strvalue,*,err=771) dir_step
       case("nr_of_sp_steps")
          read(strvalue,*,err=770) nr_of_sp_steps
       case("nr_of_dir_steps")
          read(strvalue,*,err=770) nr_of_dir_steps
       case("gaussian_dist_halfwidth")
          read(strvalue,*,err=771) gaussian_dist_halfwidth
       case("constant_u_input")
          read(strvalue,*,err=771) constant_u_input
       case("constant_v_input")
          read(strvalue,*,err=771) constant_v_input
       case("constant_speed_input")
          read(strvalue,*,err=771) constant_speed_input
       case("min_wind")
          read(strvalue,*,err=771) min_wind
       case("max_wind")
          read(strvalue,*,err=771) max_wind
       case("windfilename")
          windfilename = strvalue
       case("use_latlon_from_windfile")
          call convert_str_to_bool(key,strvalue,use_latlon_from_windfile,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("set_nr_winds_to_nr_nodes")
          call convert_str_to_bool(key,strvalue,set_nr_winds_to_nr_nodes,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("set_nr_winds_manually")
          call convert_str_to_bool(key,strvalue,set_nr_winds_manually,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("nr_winds_manual")
          read(strvalue,*,err=770) nr_winds_manual
       case("wind_distribution")
          call get_wind_distr_code(strvalue,wind_distribution,error_flag)
          if (error_flag .ne. no_error) return
       case("add_noise_to_input_windfield")
          call convert_str_to_bool(key,strvalue,add_noise_to_input_windfield,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("stdev_for_windfield_noise")
          read(strvalue,*,err=771) stdev_for_windfield_noise
       case("dont_use_views_with_snr_0")
          call convert_str_to_bool(key,strvalue,dont_use_views_with_snr_0,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("exclude_lut_edge_solutions")
          call convert_str_to_bool(key,strvalue,exclude_lut_edge_solutions,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("wvc_resolution")
          read(strvalue,*,err=771) wvc_resolution
       case("add_geo_noise")
          call convert_str_to_bool(key,strvalue,add_geo_noise,error_flag)
          if (error_flag .ne. no_error) return
       case("add_instr_noise")
          call convert_str_to_bool(key,strvalue,add_instr_noise,error_flag)
          if (error_flag .ne. no_error) return
       case("instr_noise_method")
          call get_instr_noise_method_code(strvalue,instr_noise_method,error_flag)
          if (error_flag .ne. no_error) return
       case("constant_instr_noise")
          read(strvalue,*,err=770) constant_instr_noise
       case("geo_noise_model")
          call get_geo_noise_model_code(strvalue,geo_noise_model,error_flag)
          if (error_flag .ne. no_error) return
       case("noise_add_method")
          call get_noise_add_method_code(strvalue,noise_add_method,error_flag)
          if (error_flag .ne. no_error) return
       case("lut_path")
          LUT_path = strvalue
       case("lut_c_vv_filename")
          LUT_C_VV_filename = strvalue
       case("lut_c_hh_filename")
          LUT_C_HH_filename =strvalue
       case("lut_c_pol_filename")
          LUT_C_POL_filename = strvalue
       case("lut_ku_vv_filename")
          LUT_KU_VV_filename = strvalue
       case("lut_ku_hh_filename")
          LUT_KU_HH_filename = strvalue
       case("lut_ku_pol_filename")
          LUT_KU_POL_filename = strvalue
       case("get_all_winddirs")
          call convert_str_to_bool(key,strvalue,get_all_winddirs,error_flag)
          if (error_flag .ne. no_error) return
       case("polarimetric_weight")
          read(strvalue,*,err=771) polarimetric_weight
       case("doppler_weight")
          read(strvalue,*,err=771) doppler_weight
       case("ocean_current_constant_u_input")
          read(strvalue,*,err=771) ocean_current_constant_u_input
       case("ocean_current_constant_v_input")
          read(strvalue,*,err=771) ocean_current_constant_v_input
       case("doppler_base_frequency")
          read(strvalue,*,err=771) doppler_base_frequency
       case("outputfile_vector")
          outputfile_vector = strvalue
       case("outputfile_vector_minimal")
          outputfile_vector_minimal = strvalue
       case("outputfile_binned")
          outputfile_binned = strvalue
       case("outputfile_settings")
          outputfile_settings = strvalue
       case("save_vector_results")
          call convert_str_to_bool(key,strvalue,save_vector_results,error_flag)
          if (error_flag .ne. no_error) return
       case("save_minimal_vector_results")
          call convert_str_to_bool(key,strvalue,save_minimal_vector_results,&
                                   error_flag)
          if (error_flag .ne. no_error) return
       case("save_binned_results")
          call convert_str_to_bool(key,strvalue,save_binned_results,error_flag)
          if (error_flag .ne. no_error) return
       case("save_settings")
          call convert_str_to_bool(key,strvalue,save_settings,error_flag)
          if (error_flag .ne. no_error) return
       case("hi_res_bins")
          call convert_str_to_bool(key,strvalue,hi_res_bins,error_flag)
          if (error_flag .ne. no_error) return
       case default
          print *,"Unknown key: "//trim(key)//" found in settings file..."
          call print_usage()
          error_flag = error_reading_file
          return
       end select

       !print *,"Applied: "//trim(key)//" = "//trim(strvalue)
       
    end do settings_loop

111 continue

    close(unit=fileunit)
    call free_lun(fileunit)
    
    return

    ! error handlers
770 print *,"ERROR in load_rfscat_settings:"
    print *,"Could not convert strvalue string to integer: "//trim(strvalue)
    error_flag = error_reading_file
    return

771 print *,"ERROR in load_rfscat_settings:"
    print *,"Could not convert strvalue string to real: "//trim(strvalue)
    error_flag = error_reading_file
    return

888 print *,"ERROR in load_rfscat_settings:"
    print *,"could not open settings file: "//trim(settings_filename)
    error_flag = error_opening_file
    return

999 print *,"ERROR in load_rfscat_settings:"
    print *,"could not read from settings file: "//trim(settings_filename)
    error_flag = error_reading_file
    return

  end subroutine load_rfscat_settings
    !  #]
  subroutine split_key_val(line,is_comment,key,strvalue,error_flag)
    !  #[
    character(len=*), intent(in)  :: line       ! output
    logical,          intent(out) :: is_comment ! output
    character(len=*), intent(out) :: key        ! output
    character(len=*), intent(out) :: strvalue      ! output
    integer,          intent(out) :: error_flag ! output

    ! local variables
    integer :: pos_equals, pos_excl, i, endpos
    character(len=256) :: key_val_string
    character(len=1)   :: start_quote

    ! init
    is_comment = .false.
    key(:)   = ' '
    strvalue(:) = ' '
    error_flag = no_error

    ! the '#' indicates a comment line
    if (line(1:1) .eq. '#') then
       is_comment = .true.
       return
    end if

    ! treat empty lines as comment lines as well (i.e. ignore them)
    if (len_trim(line) .eq. 0) then
       is_comment = .true.
       return
    end if

    ! find any first occurrence of the '=' and '!' character
    pos_equals = -1
    pos_excl   = -1
    scanloop: do i=1,len_trim(line)
       if (pos_equals .eq. -1) then
          if (line(i:i) .eq. '=') pos_equals=i
       end if
       if (pos_excl .eq. -1) then
          if (line(i:i) .eq. '!') pos_excl=i
       end if
    end do scanloop

    ! safety catches
    if (pos_excl .eq. 1) then
       ! assume this is a comment as well
       is_comment = .true.
       return
    end if
    if (pos_equals .eq. -1) then
       print *,"ERROR in split_key_val while parsing input settings file:"
       print *,"key/value pairs in this file should be separated by"
       print *,"a '=' character, but it could not be found ..."
       error_flag = error_reading_file
       return
    end if
    if (pos_equals .eq. 1) then
       print *,"ERROR in split_key_val while parsing input settings file:"
       print *,"lines in this file should never start with a '=' character"
       error_flag = error_reading_file
       return
    end if

    key_val_string = line
    if (pos_excl .ne. -1) key_val_string = line(1:pos_excl-1)

    key   = key_val_string(1:pos_equals-1)
    strvalue = key_val_string(pos_equals+1:len_trim(key_val_string))

    key   = adjustl(key)
    strvalue = adjustl(strvalue)

    ! in case the value is enclosed in quotes (might happen for strings
    ! like filenames) remove the quotes
    start_quote = ' '
    if (strvalue(1:1) .eq. "'") start_quote = "'"
    if (strvalue(1:1) .eq. '"') start_quote = '"'
    if (strvalue(1:1) .eq. "`") start_quote = "`"

    if (start_quote .ne. ' ') then
       endpos = len_trim(strvalue)
       if (endpos .lt. 2) then
          ! safety catch
          print *,"ERROR in split_key_val while parsing input settings file:"
          print *,"the value after '=' seems a single quote ..."
          print *,"Key   = ["//trim(key)//"]"
          print *,"Value = ["//trim(strvalue)//"]"
          error_flag = error_reading_file
       return
       end if
       if (strvalue(endpos:endpos) .eq. start_quote) then
          strvalue = strvalue(2:endpos-1)
       else
          print *,"ERROR in split_key_val while parsing input settings file:"
          print *,"the value after '=' has non-matching quotes..."
          print *,"Key   = ["//trim(key)//"]"
          print *,"Value = ["//trim(strvalue)//"]"
          error_flag = error_reading_file
          return
       end if
    end if

  end subroutine split_key_val
    !  #]
  subroutine print_usage()
    !  #[
    print *,""
    print *,""
    print *,"Usage of the rfscat_simulation program is as follows:"
    print *,""
    print *,"./rfscat_simulation <settings_file>"
    print *,""
    print *,"See the example settings file 'rfscat_settings.dat' for"
    print *,"the allowed keys and an explanation of their meaning and"
    print *,"allowed values."
    print *,""
    print *,""
  end subroutine print_usage
    !  #]
  subroutine check_and_set_derived_settings(error_flag)
    !  #[ 

    ! set derived settings and check for validity of settings
    ! and existence of output files
    integer, intent(out) :: error_flag ! output

    ! local variables
    type(inv_settings_type) :: inversion_settings
  
    error_flag = no_error

    pl1b_filename  = trim(pseudo_L1b_file_path)//trim(pseudo_L1b_file_name)
    pl1b_filename2 = trim(pseudo_L1b_file_path)//trim(pseudo_L1b_2nd_file_name)
    pl1b_filename3 = trim(pseudo_L1b_file_path)//trim(pseudo_L1b_3rd_file_name)

    if (save_binned_results) then
       CALL check_resultfile(outputfile_binned,error_flag)
       if (error_flag .ne. no_error) return
    end if
    
    if (save_vector_results) then
       CALL check_resultfile(outputfile_vector,error_flag)
       if (error_flag .ne. no_error) return
    end if

    if (save_settings) then
       CALL check_resultfile(outputfile_settings,error_flag)
       if (error_flag .ne. no_error) return
    end if

    ! retrieve the current inversion settings
    call  get_inv_settings(inversion_settings,error_flag)
    if (error_flag .ne. no_error) return

    ! modify these settings (if needed)
    !inversion_settings%verbosity = inversion_verbosity_error
    inversion_settings%instrument_type = instrument_type_rfscat

    ! note that this is forces for now to protect the calc_nrcs_values
    ! subroutine
    inversion_settings%antenna_angle_convention = angle_convention_oceanographic
    inversion_settings%wind_angle_convention    = angle_convention_meteorological

    !inversion_settings%use_zspace = .false.
    !inversion_settings%no_normalisation = .false.
    !inversion_settings%add_sign_to_mle = .false.

    ! 144 sol case or not?
    inversion_settings%get_all_winddirs = get_all_winddirs

    inversion_settings%polarimetric_weight = polarimetric_weight
    inversion_settings%doppler_weight      = doppler_weight
    
    !inversion_settings%do_parabolic_minimum_search = .true.
    !inversion_settings%do_parabolic_winddir_search = .true.
    !inversion_settings%max_nr_of_solutions = 4
    !inversion_settings%experimental_ers_wqc_checks
    !inversion_settings%use_meas_sigma0_for_normfactor = .false.
    
    ! use cmod5 to generate a default LUT if no LUT is defined
    inversion_settings%use_which_fn_to_construct_LUT = &
         use_cmod5_to_construct_LUT
    
    !inversion_settings%first_warning_only
    !inversion_settings%do_input_data_check
    !inversion_settings%initial_ws_step  = 0.2
    !inversion_settings%minimum_ws_step  = 0.02
    !inversion_settings%refiner_ws_step  = 0.1
    !inversion_settings%ws_initial_guess = 10.0

    ! set the modified inversion settings
    call set_inv_settings(inversion_settings)

    ! set LUT filenames
    ! note that these variables are declared inside the inversion module
    filename_c_vv     = trim(LUT_path)//trim(LUT_C_VV_filename)
    filename_c_hh     = trim(LUT_path)//trim(LUT_C_HH_filename)
    filename_c_pol    = trim(LUT_path)//trim(LUT_C_POL_filename)
    filename_ku_vv    = trim(LUT_path)//trim(LUT_KU_VV_filename)
    filename_ku_hh    = trim(LUT_path)//trim(LUT_KU_HH_filename)
    filename_ku_pol   = trim(LUT_path)//trim(LUT_KU_POL_filename)

  end subroutine check_and_set_derived_settings
    !  #]
  subroutine write_settings_to_file(filename,error_flag)
    !  #[

    ! write all settings to a file
    ! if filename equals "-" then write to stdout
    character(len=256), intent(in)  :: filename   ! input
    integer,            intent(out) :: error_flag ! output

    ! local variable
    integer            :: fileunit
    character(len=100) :: wind_source_str
    character(len=100) :: wind_distribution_str
    character(len=100) :: instr_noise_method_str
    character(len=100) :: geo_noise_model_str
    character(len=100) :: noise_add_method_str

    error_flag = no_error
    if (trim(filename) .eq. "-") then
       fileunit = fileunit_stdout
    else
       fileunit = get_lun()
       open(unit=fileunit,file=trim(filename),status='replace',&
            form="FORMATTED",ERR=98)
    end if

    write(fileunit,*,err=99) "Listing of settings used by the simulation"
    
    write(fileunit,*,err=99) "random_seed = ",random_seed

    write(fileunit,*,err=99) &
         "save_random_state = "//trim(bool2txt(save_random_state))
    write(fileunit,*,err=99) &
         "random_status_file = "//trim(random_status_file)

    write(fileunit,*,err=99) &
         "silent = "//trim(bool2txt(silent))

    write(fileunit,*,err=99) &
         "pseudo_L1b_file_path = ",trim(pseudo_L1b_file_path)
    write(fileunit,*,err=99) &
         "pseudo_L1b_file_name = ",trim(pseudo_L1b_file_name)
    write(fileunit,*,err=99) "pl1b_filename        = ",trim(pl1b_filename)

    write(fileunit,*,err=99) "use_2nd_pseudo_L1b_file = "//&
         trim(bool2txt(use_2nd_pseudo_L1b_file))
    IF (use_2nd_pseudo_L1b_file) THEN
       write(fileunit,*,err=99) &
            "pseudo_L1b_2nd_file_name = ",trim(pseudo_L1b_2nd_file_name)
       write(fileunit,*,err=99) &
            "pl1b_filename2           = ",trim(pl1b_filename2)
    ENDIF

    write(fileunit,*,err=99) "use_3rd_pseudo_L1b_file = "//&
         trim(bool2txt(use_3rd_pseudo_L1b_file))
    IF (use_3rd_pseudo_L1b_file) THEN
       write(fileunit,*,err=99) &
            "pseudo_L1b_3rd_file_name = ",trim(pseudo_L1b_3rd_file_name)
       write(fileunit,*,err=99) &
            "pl1b_filename3           = ",trim(pl1b_filename3)
    ENDIF

    call get_wind_source_str(wind_source,wind_source_str,error_flag)
    if (error_flag .ne. no_error) return
    write(fileunit,*,err=99) "wind_source = "//trim(wind_source_str)

    write(fileunit,*,err=99) "u_startval    = ",u_startval
    write(fileunit,*,err=99) "v_startval    = ",v_startval
    write(fileunit,*,err=99) "u_step        = ",u_step
    write(fileunit,*,err=99) "v_step        = ",v_step
    write(fileunit,*,err=99) "nr_of_u_steps = ",nr_of_u_steps
    write(fileunit,*,err=99) "nr_of_v_steps = ",nr_of_v_steps

    write(fileunit,*,err=99) "sp_startval     = ",sp_startval
    write(fileunit,*,err=99) "dir_startval    = ",dir_startval
    write(fileunit,*,err=99) "sp_step         = ",sp_step
    write(fileunit,*,err=99) "dir_step        = ",dir_step
    write(fileunit,*,err=99) "nr_of_sp_steps  = ",nr_of_sp_steps
    write(fileunit,*,err=99) "nr_of_dir_steps = ",nr_of_dir_steps

    write(fileunit,*,err=99) "gaussian_dist_halfwidth = ",gaussian_dist_halfwidth

    write(fileunit,*,err=99) "constant_u_input = ",constant_u_input
    write(fileunit,*,err=99) "constant_v_input = ",constant_v_input

    write(fileunit,*,err=99) "constant_speed_input = ",constant_speed_input

    write(fileunit,*,err=99) "min_wind = ",min_wind
    write(fileunit,*,err=99) "max_wind = ",max_wind
    write(fileunit,*,err=99) "windfilename = "//trim(windfilename)

    write(fileunit,*,err=99) "use_latlon_from_windfile = "//&
         trim(bool2txt(use_latlon_from_windfile))

    write(fileunit,*,err=99) "set_nr_winds_to_nr_nodes = "//&
         trim(bool2txt(set_nr_winds_to_nr_nodes))
    
    write(fileunit,*) "set_nr_winds_manually = "//trim(bool2txt(set_nr_winds_manually))
    write(fileunit,*,err=99) "nr_winds_manual = ",nr_winds_manual

    call get_wind_distr_str(wind_distribution,wind_distribution_str,error_flag)
    if (error_flag .ne. no_error) return
    write(fileunit,*,err=99) "wind_distribution = "//trim(wind_distribution_str)

    write(fileunit,*,err=99) "add_noise_to_input_windfield = "//&
         trim(bool2txt(add_noise_to_input_windfield))
    write(fileunit,*,err=99) "stdev_for_windfield_noise = ",stdev_for_windfield_noise
    write(fileunit,*,err=99) "dont_use_views_with_snr_0 = "//&
         trim(bool2txt(dont_use_views_with_snr_0))

    write(fileunit,*,err=99) "exclude_LUT_edge_solutions = "//&
         trim(bool2txt(exclude_LUT_edge_solutions))
    
    write(fileunit,*,err=99) "wvc_resolution = ",wvc_resolution
    write(fileunit,*,err=99) "add_geo_noise = "//trim(bool2txt(add_geo_noise))
    write(fileunit,*,err=99) "add_instr_noise = "//trim(bool2txt(add_instr_noise))

    call get_instr_noise_method_str(instr_noise_method,instr_noise_method_str,&
                                    error_flag)
    if (error_flag .ne. no_error) return
    write(fileunit,*,err=99) "instr_noise_method = "//&
                             trim(instr_noise_method_str)

    write(fileunit,*,err=99) "constant_instr_noise = ",constant_instr_noise

    call get_geo_noise_model_str(geo_noise_model,geo_noise_model_str,error_flag)
    if (error_flag .ne. no_error) return
    write(fileunit,*,err=99) "geo_noise_model = "//trim(geo_noise_model_str)

    call get_noise_add_method_str(noise_add_method,noise_add_method_str,error_flag)
    if (error_flag .ne. no_error) return
    write(fileunit,*,err=99) "noise_add_method = "//trim(noise_add_method_str)

    write(fileunit,*,err=99) "outputfile_vector = "//trim(outputfile_vector)
    write(fileunit,*,err=99) "outputfile_vector_minimal = "//trim(outputfile_vector_minimal)
    write(fileunit,*,err=99) "outputfile_binned = "//trim(outputfile_binned)
    write(fileunit,*,err=99) "outputfile_settings = "//trim(outputfile_settings)

    write(fileunit,*,err=99) "save_vector_results = "//trim(bool2txt(save_vector_results))
    write(fileunit,*,err=99) "save_minimal_vector_results = "//&
                             trim(bool2txt(save_minimal_vector_results))
    write(fileunit,*,err=99) "save_binned_results = "//trim(bool2txt(save_binned_results))
    write(fileunit,*,err=99) "save_settings = "//trim(bool2txt(save_settings))

    write(fileunit,*,err=99) "hi_res_bins = "//trim(bool2txt(hi_res_bins))

    write(fileunit,*,err=99) "ocean_current_constant_u_input = ",&
                              ocean_current_constant_u_input
    write(fileunit,*,err=99) "ocean_current_constant_v_input = ",&
                              ocean_current_constant_v_input
    write(fileunit,*,err=99) "doppler_base_frequency = ",&
                              doppler_base_frequency
    
    ! write the actual settings used by the inversion module
    write(fileunit,*,err=99) "======================================"
    write(fileunit,*,err=99) "Settings used by the inversion module:"
    write(fileunit,*,err=99) "======================================"
    CALL write_invsettings_to_file(fileunit)

    if (trim(filename) .ne. "-") then
       close(fileunit)
       CALL free_lun(fileunit)
    end if

    return
    
    ! error handlers
98  print *,'ERROR while creating the settings file:  ',trim(filename)
    error_flag = error_opening_file
    return
99  print *,'ERROR while writing to the settings file:  ',trim(filename)
    error_flag = error_writing_file
    return
    
  end subroutine write_settings_to_file
  !  #]
  function bool2txt(b) result(txt)
    !  #[ for writing boolean settings to file
    logical, intent(in) :: b   ! input
    character(len=10)   :: txt ! result

    if (b) then
       txt = "true"
    else
       txt = "false"
    end if
    return
  end function bool2txt
    !  #]
  subroutine convert_str_to_bool(key,strvalue,switch,error_flag)
    !  #[
    character(len=*), intent(in)  :: key
    character(len=*), intent(in)  :: strvalue
    logical,          intent(out) :: switch
    integer,          intent(out) :: error_flag

    error_flag = no_error

    select case(to_lowercase(trim(strvalue)))
    case(".true.","true","t")
       switch=.true.
    case(".false.","false","f")
       switch=.false.
    case default
       print *,"Illegal value for key: "//trim(key)
       print *,"Allowed values are: .true./true/t or .false./false/f"
       print *,"Found value was: "//trim(strvalue)
       error_flag = error_reading_file
       return
    end select

  end subroutine convert_str_to_bool
    !  #]
  subroutine check_resultfile(filename,error_flag)
    !  #[ check for file existence
    character(len=256), intent(in)  :: filename   ! input
    integer,            intent(out) :: error_flag ! output

    ! local variable
    logical            :: exists

    error_flag = no_error

    !print *,'testing for existence of file: ',trim(filename)
    inquire(file=trim(filename),exist=exists)
    IF (exists) THEN
       print *,'ERROR: Outputfile: ',trim(filename),' already exists!!!!!'
       print *,'ERROR: please remove it before running this program !'
       error_flag = error_program_usage
    END IF
    return

  end subroutine check_resultfile
    !  #]
  subroutine get_noise_add_method_code(noise_add_method_str,&
                                       noise_add_method,error_flag)
    !  #[
    character(len=*), intent(in)  :: noise_add_method_str
    integer,          intent(out) :: noise_add_method
    integer,          intent(out) :: error_flag

    error_flag = no_error

    select case(noise_add_method_str)
    case("undefined")
       noise_add_method = noise_add_method_undefined
    case("multiplicative")
       noise_add_method = noise_add_method_multiplicative
    case("sqrt_sum")
       noise_add_method = noise_add_method_sqrt_sum
    case default
       print *,"ERROR in get_noise_add_method_code:"
       print *,"unrecognised noise addition method: "//&
            trim(noise_add_method_str)
       print *,"Implemented algorithms are:"
       print *,"   [multiplicative|sqrt_sum]"
       error_flag = error_program_usage
       return

    end select
  end subroutine get_noise_add_method_code
    !  #]
  subroutine get_noise_add_method_str(noise_add_method,&
                                      noise_add_method_str,error_flag)
    !  #[
    integer,          intent(in)  :: noise_add_method
    character(len=*), intent(out) :: noise_add_method_str
    integer,          intent(out) :: error_flag

    error_flag = no_error

    select case(noise_add_method)
    case(noise_add_method_undefined)
       noise_add_method_str = "undefined"
    case(noise_add_method_multiplicative)
       noise_add_method_str = "multiplicative"
    case(noise_add_method_sqrt_sum)
       noise_add_method_str = "sqrt_sum"
    case default
       print *,"ERROR in get_noise_add_method_str:"
       print *,"unrecognised noise addition method code: ",&
                noise_add_method
       error_flag = error_program_usage
       return
    end select

  end subroutine get_noise_add_method_str
    !  #]
  subroutine get_instr_noise_method_code(instr_noise_method_str,&
                                         instr_noise_method,error_flag)
    !  #[
    character(len=*), intent(in)  :: instr_noise_method_str
    integer,          intent(out) :: instr_noise_method
    integer,          intent(out) :: error_flag

    error_flag = no_error

    select case(instr_noise_method_str)
    case("undefined")
       instr_noise_method = instr_noise_method_undefined
    case("kp")
       instr_noise_method = instr_noise_method_kp
    case("constant")
       instr_noise_method = instr_noise_method_constant
    case default
       print *,"ERROR in get_instr_noise_method_code:"
       print *,"unrecognised instr noise method: "//&
            trim(instr_noise_method_str)
       print *,"Implemented algorithms are:"
       print *,"   [kp|constant]"
       error_flag = error_program_usage
       return

    end select
  end subroutine get_instr_noise_method_code
    !  #]
  subroutine get_instr_noise_method_str(instr_noise_method,&
                                        instr_noise_method_str,error_flag)
    !  #[
    integer,          intent(in)  :: instr_noise_method
    character(len=*), intent(out) :: instr_noise_method_str
    integer,          intent(out) :: error_flag

    error_flag = no_error

    select case(instr_noise_method)
    case(instr_noise_method_undefined)
       instr_noise_method_str = "undefined"
    case(instr_noise_method_kp)
       instr_noise_method_str = "kp"
    case(instr_noise_method_constant)
       instr_noise_method_str = "constant"
    case default
       print *,"ERROR in get_instr_noise_method_str:"
       print *,"unrecognised instr noise method code: ",&
                instr_noise_method
       error_flag = error_program_usage
       return
    end select

  end subroutine get_instr_noise_method_str
    !  #]
  !------------------------------------------
end module Simulation_Settings
