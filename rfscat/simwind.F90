MODULE simwind
  !---------------------------------------------------
  !  #[ Documentation
  !
  ! a module to produce a simulated windfield
  ! for each of the wind vector cells (wvcs) along one line. 
  ! 
  ! Written 2000, 2001, 2002 by Jos de Kloe, KNMI.
  ! Modifications:
  ! 07-May-2009 J. de Kloe removed all CPP definitions
  ! 13-May-2009 J. de Kloe reorganised into logical blocks
  ! 19-May-2009 J. de Kloe change parameters hardcoded in init_simwind
  !                        to optional parameters
  !
  !  #]
  !---------------------------------------------------
  !  #[ Modules used

  ! import some genscat modules
  USE random, only: random_nr, gaussian_rnd_nr
  USE numerics, only: r4_, missing_indicator_real_r4
  USE lunmanager, only: get_lun, free_lun
  USE convert, only: uv_to_speed, uv_to_dir, speeddir_to_u, speeddir_to_v
  USE errorhandler, only: no_error, error_opening_file, error_programming, &
       error_allocate, error_reading_file, error_not_yet_implemented, &
       error_program_usage
  USE stringtools, only: to_lowercase
  !  #]
  !  #[ variables and parameters
  IMPLICIT NONE  ! no implicit variable typing
  public         ! make all variables public by default

  ! define a canary parameter, which helps tracking the mistake
  ! of using an uninitialised wind structure
  integer, parameter :: wind_canary = 789456321

  TYPE wind_type
     integer :: num_winds
     real(r4_), dimension(:), pointer :: u
     real(r4_), dimension(:), pointer :: v
     real(r4_), dimension(:), pointer :: lat
     real(r4_), dimension(:), pointer :: lon
     integer :: wind_source
     integer :: wind_distribution
     integer :: index_last_used_wind
     integer :: struct_was_properly_initialised
     logical :: be_silent
  END TYPE wind_type

  ! possible wind sources
  ! for initialisation only
  integer, parameter :: wind_source_undefined       = -1
  ! define winds on a u,v grid
  integer, parameter :: wind_source_u_v_grid        = 0
  ! define winds on a sp,dir grid
  integer, parameter :: wind_source_sp_dir_grid     = 1
  ! read winds from asciifile
  integer, parameter :: wind_source_from_windfile   = 2
  ! generate random winds with some given distribution
  integer, parameter :: wind_source_random          = 3
  ! generate always the same wind vector
  integer, parameter :: wind_source_constant_vector = 4
  ! generate always the same wind speed, but with random direction
  integer, parameter :: wind_source_constant_speed  = 5

  ! possible wind distribution functions, used to generate random winds
  ! for initialisation only
  integer, parameter :: wind_distribution_undefined = -1
  ! use a Weibull distribution to generate random winds
  integer, parameter :: wind_distribution_Weibull   = 1
  ! use a Rayleigh distribution to generate random winds
  integer, parameter :: wind_distribution_Rayleigh  = 2
  ! use a Uniform distribution to generate random winds
  integer, parameter :: wind_distribution_Uniform   = 3

  ! available geo noise models:
  integer, parameter :: geo_noise_model_undefined = -1
  ! the algorithm used by the 2002 rfscat study
  integer, parameter :: geo_noise_model_old       = 1
  ! updated algorithm for use with qscat (Ku band)
  integer, parameter :: geo_noise_model_exp_qscat = 2
  ! updated algorithm for use with ascat (C band)
  integer, parameter :: geo_noise_model_exp_ascat = 3

  !  #]
  !---------------------------------------------------
CONTAINS ! routines to handle the data in this module
  !--------------------------------

  ! TODO: deze settings verhuizen naar de aanroep van de init routine
  !if (set_nr_winds_to_nr_nodes) windnumber = nr_of_nodeblocks
  !if (set_nr_winds_manually) windnumber = nr_winds
  !if (use_random_winds) windnumber= 100000 ! 25000

  subroutine init_simwind(winds,wind_source,error_flag,&
                          wind_distribution,num_winds,&
                          windfilename,&
                          u_startval,v_startval,u_step,v_step,&
                          nr_of_u_steps,nr_of_v_steps,&
                          sp_startval,dir_startval,sp_step,dir_step,&
                          nr_of_sp_steps,nr_of_dir_steps,&
                          gaussian_dist_halfwidth, &
                          constant_u_input,constant_v_input,&
                          constant_speed_input,silent)
    !  #[
    type(wind_type), intent(out) :: winds
    integer,         intent(in)  :: wind_source
    integer,         intent(out) :: error_flag ! output
    integer,optional,intent(in)  :: wind_distribution
    integer,optional,intent(in)  :: num_winds
    character(len=*), optional, intent(in) :: windfilename
    ! parameters to define a u,v grid for wind definition
    real(r4_), optional, intent(in) :: u_startval
    real(r4_), optional, intent(in) :: v_startval
    real(r4_), optional, intent(in) :: u_step
    real(r4_), optional, intent(in) :: v_step
    integer,   optional, intent(in) :: nr_of_u_steps
    integer,   optional, intent(in) :: nr_of_v_steps
    ! define a sp,dir grid for wind definition
    real(r4_), optional, intent(in) :: sp_startval
    real(r4_), optional, intent(in) :: dir_startval
    real(r4_), optional, intent(in) :: sp_step
    real(r4_), optional, intent(in) :: dir_step
    integer,   optional, intent(in) :: nr_of_sp_steps
    integer,   optional, intent(in) :: nr_of_dir_steps
    ! define the Rayleigh distribution of winds
    real(r4_), optional, intent(in) :: gaussian_dist_halfwidth
    ! define the u,v components in case of constant input wind vector
    real(r4_), optional, intent(in) :: constant_u_input
    real(r4_), optional, intent(in) :: constant_v_input
    ! define speed in case of constant input speed
    real(r4_), optional, intent(in) :: constant_speed_input
    logical, optional, intent(in) :: silent


    ! local variables
    integer :: num_winds_limit

    ! Next follow some fixed parameters, that could easily be transformed
    ! to input settings if they need to be become variable

    ! Define the Weibull distribution of winds:
    ! t.b.d.
    !real(r4_),parameter :: 
    !real(r4_),parameter :: 

    ! minimum and maximum allowed wind
    ! these are imposed on all types of windsources
    ! random winds are only generated inside these boundaries
    ! winds from file or defined by a grid are discarded when
    ! they fall outside the allowed range
    ! The number of available winds is automatically 
    ! adapted accordingly.
    real(r4_),parameter :: min_wind =  0.0 !   0.8 ! [m/s]
    real(r4_),parameter :: max_wind = 25.0 !  25.0 ! [m/s]

    ! print *,"Wind initialisation starting"

    error_flag = no_error

    ! init the winds structure
    nullify(winds%u)
    nullify(winds%v)
    nullify(winds%lat)
    nullify(winds%lon)
    winds%num_winds            = 0
    winds%wind_source          = wind_source_undefined
    winds%wind_distribution    = wind_distribution_undefined
    winds%index_last_used_wind = 0
    winds%be_silent            = .false.
    if (present(silent)) then
       winds%be_silent = silent
    end if

    ! then fill the structure for different cases
    select case(wind_source)
    case(wind_source_undefined)
       print *,"ERROR in init_simwind: wind_source seems set to"
       print *,"wind_source_undefined; this is not allowed"
       error_flag = error_programming
       return
    case(wind_source_u_v_grid)
       if (.not. (present(u_startval)    .and. &
                  present(v_startval)    .and. &
                  present(u_step)        .and. &
                  present(v_step)        .and. &
                  present(nr_of_u_steps) .and. &
                  present(nr_of_v_steps)        )) then
          print *,"ERROR in init_simwind: optional parameters"
          print *,"u_startval,v_startval,u_step,v_step,"
          print *,"nr_of_u_steps and nr_of_v_steps"
          print *,"need to be set when defining winds on a u,v grid."
          error_flag = error_programming
          return
       end if
       call define_winds_on_u_v_grid(winds,&
                                     u_startval,v_startval,&
                                     u_step,v_step,&
                                     nr_of_u_steps,nr_of_v_steps,&
                                     min_wind,max_wind,error_flag)
       if (error_flag .ne. no_error) return
    case(wind_source_sp_dir_grid)
       if (.not. (present(sp_startval)    .and. &
                  present(dir_startval)   .and. &
                  present(sp_step)        .and. &
                  present(dir_step)       .and. &
                  present(nr_of_sp_steps) .and. &
                  present(nr_of_dir_steps)      )) then
          print *,"ERROR in init_simwind: optional parameters"
          print *,"sp_startval,dir_startval,sp_step,dir_step,"
          print *,"nr_of_sp_steps and nr_of_dir_steps"
          print *,"need to be set when defining winds on a sp,dir grid."
          error_flag = error_programming
          return
       end if
       call define_winds_on_sp_dir_grid(winds,&
                                        sp_startval,dir_startval,&
                                        sp_step,dir_step,&
                                        nr_of_sp_steps,nr_of_dir_steps,&
                                        min_wind,max_wind,error_flag)
       if (error_flag .ne. no_error) return
    case(wind_source_from_windfile)
       if (.not. present(wind_distribution)) then
          print *,"ERROR in init_simwind: optional parameter windfilename"
          print *,"needs to be set when reading winds from a file."
          error_flag = error_programming
          return
       end if
       num_winds_limit = -1
       if (present(num_winds)) num_winds_limit=num_winds
       if (num_winds_limit .ne. -1) then
          call read_winds_from_windfile(winds,windfilename,&
                                        min_wind,max_wind,error_flag,&
                                        num_winds_limit)
       else
          call read_winds_from_windfile(winds,windfilename,&
                                        min_wind,max_wind,error_flag)
       end if
       if (error_flag .ne. no_error) return
    case(wind_source_random)
       if (.not. present(wind_distribution)) then
          print *,"ERROR in init_simwind: optional parameter wind_distribution"
          print *,"needs to be set when defining random winds."
          error_flag = error_programming
          return
       end if
       if (.not. present(num_winds)) then
          print *,"ERROR in init_simwind: optional parameter num_winds"
          print *,"needs to be set when defining random winds."
          error_flag = error_programming
          return
       end if
       if (wind_distribution .eq. wind_distribution_Rayleigh) then
          if (.not. present(gaussian_dist_halfwidth)) then
             print *,"ERROR in init_simwind: optional parameter"
             print *,"gaussian_dist_halfwidth needs to be set when"
             print *,"defining random winds with Gaussian u,v distribution."
             error_flag = error_programming
             return
          end if
       end if

       call define_random_winds(winds,wind_distribution,num_winds,&
                           min_wind,max_wind,error_flag,&
                           gaussian_dist_halfwidth=gaussian_dist_halfwidth)
       if (error_flag .ne. no_error) return
    case(wind_source_constant_vector)
       if (.not. (present(constant_u_input) .and. &
                  present(constant_v_input)       )) then
          print *,"ERROR in init_simwind: optional parameters"
          print *,"constant_u_input and constant_v_input need to be set when"
          print *,"defining constant winds"
          error_flag = error_programming
          return
       end if
       call define_constant_winds(winds,num_winds,&
                                  constant_u_input,constant_v_input,&
                                  error_flag)
       if (error_flag .ne. no_error) return

    case(wind_source_constant_speed)
       

       if (.not. present(constant_speed_input)) then
          print *,"ERROR in init_simwind: optional parameter"
          print *,"constant_speed_input needs to be set when"
          print *,"defining a wind distribution with constant windspeed"
          error_flag = error_programming
          return
       end if

       if (.not. present(num_winds)) then
          print *,"ERROR in init_simwind: optional parameter num_winds"
          print *,"needs to be set when defining random winds."
          error_flag = error_programming
          return
       end if
      

       call define_constant_speed(winds,num_winds,&
                                 constant_speed_input,error_flag)
                                 

       if (error_flag .ne. no_error) return

    case default
       print *,"ERROR in init_simwind: wind_source seems set to"
       print *,"an unknown value of",wind_source,"; this is not allowed"
       error_flag = error_programming
       return
    end select

    winds%struct_was_properly_initialised = wind_canary
    ! print *,"Wind initialisation done"

  end subroutine init_simwind
    !  #]
  subroutine close_simwind(winds)
    !  #[
    type(wind_type), intent(inout) :: winds

    ! deallocate arrays to clean-up memory
    if (associated(winds%u)) deallocate(winds%u)
    if (associated(winds%v)) deallocate(winds%v)
    if (associated(winds%lat)) deallocate(winds%lat)
    if (associated(winds%lon)) deallocate(winds%lon)
    winds%num_winds            = 0
    winds%wind_source          = wind_source_undefined
    winds%wind_distribution    = wind_distribution_undefined
    winds%index_last_used_wind = 0
    winds%struct_was_properly_initialised = 0
    
    return
  end subroutine close_simwind
    !  #]
  subroutine define_winds_on_u_v_grid(winds,&
                   u_startval,v_startval,u_step,v_step,&
                   nr_of_u_steps,nr_of_v_steps,&
                   min_wind,max_wind,error_flag)
    !  #[
    type(wind_type), intent(inout) :: winds ! will contain the winds
    real(r4_), intent(in) :: u_startval     ! input
    real(r4_), intent(in) :: v_startval     ! input
    real(r4_), intent(in) :: u_step         ! input
    real(r4_), intent(in) :: v_step         ! input
    integer,   intent(in) :: nr_of_u_steps  ! input
    integer,   intent(in) :: nr_of_v_steps  ! input
    real(r4_), intent(in) :: min_wind       ! input
    real(r4_), intent(in) :: max_wind       ! input
    integer,   intent(out) :: error_flag    ! output

    ! local variables
    integer :: count,i,j,max_nr_winds,AllocStatus
    real(r4_) :: min_wind_sq,max_wind_sq
    real(r4_) :: u, u_sq, v, v_sq, windspeed_sq
    real(r4_), dimension(:), pointer :: u_table
    real(r4_), dimension(:), pointer :: v_table

    ! init
    error_flag = no_error
    max_nr_winds = nr_of_u_steps*nr_of_v_steps

    ! allocate temporary storage arrays
    nullify(u_table,v_table)
    allocate(u_table(max_nr_winds),v_table(max_nr_winds),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_winds_on_u_v_grid"
       error_flag = error_allocate
       return
    end if

    min_wind_sq = min_wind**2
    max_wind_sq = max_wind**2
    count=0
    DO i=1,nr_of_u_steps
       u = u_startval + u_step*(i-1)
       u_sq = u**2
       DO j=1,nr_of_v_steps
          v = v_startval + v_step*(j-1)
          v_sq = v**2
          windspeed_sq = u_sq + v_sq
          IF (windspeed_sq .le. max_wind_sq) THEN
             IF (windspeed_sq .ge. min_wind_sq) THEN
                count = count + 1
                u_table(count) = u
                v_table(count) = v
             END IF
          END IF
       END DO
    END DO

    ! allocate the output arrays
    allocate(winds%u(count),winds%v(count),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_winds_on_u_v_grid"
       error_flag = error_allocate
       return
    end if

    ! copy the results to the winds data structure
    winds%u(1:count)  = u_table(1:count)
    winds%v(1:count)  = v_table(1:count)
    winds%num_winds   = count
    winds%wind_source = wind_source_u_v_grid

    ! clean up
    deallocate(u_table,v_table)

  end subroutine define_winds_on_u_v_grid
    !  #]
  subroutine define_winds_on_sp_dir_grid(winds,&
                   sp_startval,dir_startval,sp_step,dir_step,&
                   nr_of_sp_steps,nr_of_dir_steps,&
                   min_wind,max_wind,error_flag)
    !  #[
    type(wind_type), intent(inout) :: winds  ! will contain the winds
    real(r4_), intent(in) :: sp_startval     ! input
    real(r4_), intent(in) :: dir_startval    ! input
    real(r4_), intent(in) :: sp_step         ! input
    real(r4_), intent(in) :: dir_step        ! input
    integer,   intent(in) :: nr_of_sp_steps  ! input
    integer,   intent(in) :: nr_of_dir_steps ! input
    real(r4_), intent(in) :: min_wind        ! input
    real(r4_), intent(in) :: max_wind        ! input
    integer,   intent(out) :: error_flag     ! output

    ! local variables
    integer :: count,i,j,max_nr_winds,AllocStatus
    real(r4_) :: min_wind_sq,max_wind_sq
    real(r4_) :: sp, dir, windspeed_sq
    real(r4_), dimension(:), pointer :: u_table
    real(r4_), dimension(:), pointer :: v_table

    ! init
    error_flag = no_error
    max_nr_winds = nr_of_sp_steps*nr_of_dir_steps

    ! allocate temporary storage arrays
    nullify(u_table,v_table)
    allocate(u_table(max_nr_winds),v_table(max_nr_winds),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_winds_on_sp_dir_grid"
       error_flag = error_allocate
       return
    end if

    min_wind_sq = min_wind**2
    max_wind_sq = max_wind**2
    count=0
    DO i=1,nr_of_sp_steps
       sp = sp_startval + sp_step*(i-1)
       windspeed_sq = sp**2
       DO j=1,nr_of_dir_steps
          dir = dir_startval + dir_step*(j-1)
          IF (windspeed_sq .le. max_wind_sq) THEN
             IF (windspeed_sq .ge. min_wind_sq) THEN
                count = count + 1
                u_table(count) = speeddir_to_u(sp,dir)
                v_table(count) = speeddir_to_v(sp,dir)
             END IF
          END IF
       END DO
    END DO

    ! allocate the output arrays
    allocate(winds%u(count),winds%v(count),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_winds_on_sp_dir_grid"
       error_flag = error_allocate
       return
    end if

    ! copy the results to the winds data structure
    winds%u(1:count)  = u_table(1:count)
    winds%v(1:count)  = v_table(1:count)
    winds%num_winds   = count
    winds%wind_source = wind_source_sp_dir_grid

    ! clean up
    deallocate(u_table,v_table)

  end subroutine define_winds_on_sp_dir_grid
    !  #]
  subroutine read_winds_from_windfile(winds,windfilename,&
                                      min_wind,max_wind,error_flag,&
                                      num_winds_limit)
    !  #[
    type(wind_type),  intent(inout) :: winds        ! will contain the winds
    character(len=*), intent(in)    :: windfilename ! input
    real(r4_),        intent(in)    :: min_wind     ! input
    real(r4_),        intent(in)    :: max_wind     ! input
    integer,          intent(out)   :: error_flag   ! output
    integer, optional, intent(in)   :: num_winds_limit ! optional input

    ! local variables
    integer   :: windfileunit
    integer   :: count, i, num_winds, timeoffset
    integer   :: AllocStatus
    real(r4_) :: u, v, lat, lon
    real(r4_) :: windspeed_sq
    real(r4_) :: min_wind_sq,max_wind_sq
    character(len=256) :: dummyline
    real(r4_), dimension(:), pointer :: u_table
    real(r4_), dimension(:), pointer :: v_table
    real(r4_), dimension(:), pointer :: lat_table
    real(r4_), dimension(:), pointer :: lon_table

    print *,"opening windfile: ",trim(windfilename)

    windfileunit = get_lun()
    open(unit=windfileunit,file=trim(windfilename),&
         status='old',form="FORMATTED",ERR=888)
    read(windfileunit,*,err=997) num_winds
    read(windfileunit,*,err=998) dummyline

    if (present(num_winds_limit)) then
       if (num_winds .gt. num_winds_limit) num_winds = num_winds_limit
    end if

    ! allocate temporary storage arrays
    nullify(u_table,v_table,lat_table,lon_table)
    allocate(u_table(num_winds),v_table(num_winds),&
             lat_table(num_winds),lon_table(num_winds),&
             STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in read_winds_from_windfile"
       error_flag = error_allocate
       return
    end if

    ! read the data
    min_wind_sq = min_wind**2
    max_wind_sq = max_wind**2
    count = 0
    DO i=1,num_winds
       read(windfileunit,*,err=999) u, v, lat, lon, timeoffset
       windspeed_sq = u**2 + v**2
       IF (windspeed_sq .le. max_wind_sq) THEN
          IF (windspeed_sq .ge. min_wind_sq) THEN
             count = count + 1
             u_table(count)   = u
             v_table(count)   = v
             lat_table(count) = lat
             lon_table(count) = lon
          END IF
       END IF
    END DO

    ! properly close the file after reading the last windvector
    close(windfileunit)
    CALL free_lun(windfileunit)

    ! allocate the output arrays
    allocate(winds%u(count),winds%v(count),&
             winds%lat(count),winds%lon(count),&
             STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in read_winds_from_windfile"
       error_flag = error_allocate
       return
    end if

    ! copy the results to the winds data structure
    winds%u(1:count)   = u_table(1:count)
    winds%v(1:count)   = v_table(1:count)
    winds%lat(1:count) = lat_table(1:count)
    winds%lon(1:count) = lon_table(1:count)
    winds%num_winds    = count
    winds%wind_source  = wind_source_from_windfile

    ! clean up
    deallocate(u_table,v_table,lat_table,lon_table)

    return

    ! error handlers
888 print *,"ERROR in read_winds_from_windfile"
    print *,'ERROR while opening input file:',trim(windfilename)
    error_flag = error_opening_file
    return

997 print *,"ERROR in read_winds_from_windfile"
    print *,'ERROR 1 while reading from input file:',trim(windfilename)
    print *,"reading num_winds failed"
    error_flag = error_reading_file
    return

998 print *,"ERROR in read_winds_from_windfile"
    print *,'ERROR 2 while reading from input file:',trim(windfilename)
    print *,"reading dummyline failed"
    ! a dummy print to prevent set but unused warnings by the compiler
    print *,"dummyline = ",dummyline
    error_flag = error_reading_file
    return

999 print *,"ERROR in read_winds_from_windfile"
    print *,'ERROR 3 while reading from input file:',trim(windfilename)
    print *,"reading u, v, lat, lon, timeoffset failed"
    ! a dummy print to prevent set but unused warnings by the compiler
    print *,"u, v, lat, lon, timeoffset = ",u, v, lat, lon, timeoffset
    error_flag = error_reading_file
    return

  end subroutine read_winds_from_windfile
    !  #]

  subroutine define_random_winds(winds,wind_distribution,num_winds,&
                                 min_wind,max_wind,error_flag, &
                                 gaussian_dist_halfwidth)
    !  #[

    type(wind_type),  intent(inout) :: winds        ! will contain the winds
    integer,          intent(in)    :: wind_distribution ! input
    integer,          intent(in)    :: num_winds         ! input
    real(r4_),        intent(in)    :: min_wind          ! input
    real(r4_),        intent(in)    :: max_wind          ! input
    integer,          intent(out)   :: error_flag        ! output
    real(r4_), optional, intent(in) :: gaussian_dist_halfwidth ! optional input

    ! local variables
    real(r4_) :: gaussian_factor
    real(r4_) :: min_wind_sq, max_wind_sq
    real(r4_) :: u, v, u_sq, v_sq, windspeed_sq
    integer   :: AllocStatus, i

    ! init
    min_wind_sq = min_wind**2
    max_wind_sq = max_wind**2

    if (wind_distribution .eq. wind_distribution_Rayleigh) then
       if (.not. present(gaussian_dist_halfwidth)) then
          print *,"ERROR in define_random_winds:"
          print *,"the optional parameter gaussian_dist_halfwidth should be"
          print *,"supplied when using wind_distribution_Rayleigh"
          error_flag = error_programming
          return
       end if
    end if

    gaussian_factor = 0.0
    if (present(gaussian_dist_halfwidth)) then
       ! calculate the normalisation factor for the denomenator
       ! of the exponential term in the gaussian distribution
       gaussian_factor = -1.0/(2.0*gaussian_dist_halfwidth**2)
    end if

    ! allocate the output arrays
    allocate(winds%u(num_winds),winds%v(num_winds),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_random_winds"
       error_flag = error_allocate
       return
    end if

    windloop: DO i=1,num_winds
       ! repeat taking random wind vector components untill the
       ! conditions are fulfilled.
       tryloop: DO
          u = max_wind * (2.0*real(random_nr(),r4_) - 1.0)
          v = max_wind * (2.0*real(random_nr(),r4_) - 1.0)
          u_sq = u**2
          v_sq = v**2
          windspeed_sq = u_sq + v_sq
          select case (wind_distribution)
          case(wind_distribution_undefined)
             print *,"The wind distribution type is set to"
             print *,"wind_distribution_undefined; this should never happen"
             error_flag = error_programming
             return
          case(wind_distribution_Weibull)
             print *,"The Weibul random distribution is NOT yet implemented"
             error_flag = error_programming
             return
          case(wind_distribution_Rayleigh)
             ! only winds with a random number below the Gaussian curve is 
             ! accepted now.
             IF ( (windspeed_sq .le. max_wind_sq) .and. &
                  (windspeed_sq .ge. min_wind_sq)) THEN
                IF ( (random_nr() .lt. exp(gaussian_factor*(u_sq) ) ) .and. &
                     (random_nr() .lt. exp(gaussian_factor*(v_sq) ) )   ) THEN
                   exit tryloop
                END IF
             END IF
          case(wind_distribution_Uniform)
             ! use a uniform distribution 
             ! any wind between min_wind and max_wind is ok
             IF ( (windspeed_sq .le. max_wind_sq) .and. &
                  (windspeed_sq .ge. min_wind_sq)) THEN
                exit tryloop
             END IF
          case default
             print *,"The wind distribution type is set to",wind_distribution
             print *,"this value is not yet defined"
             error_flag = error_programming
             return
          end select
       END DO tryloop
       ! store the result
       winds%u(i)  = u
       winds%v(i)  = v
    END DO windloop

    winds%num_winds   = num_winds
    winds%wind_source = wind_source_random


  end subroutine define_random_winds



!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
subroutine define_constant_speed(winds,num_winds,&
                                 constant_speed_input,error_flag)
    !  #[

    type(wind_type),  intent(inout) :: winds        ! will contain the winds
    
    integer,          intent(in)    :: num_winds         ! input
    real(r4_),        intent(in)    :: constant_speed_input          ! input
    integer,          intent(out)   :: error_flag        ! output


    ! local variables
    real(r4_) :: gaussian_factor, pi, gaussian_dist_halfwidth

    real(r4_) :: u, v, dir, u_sq, v_sq, windspeed_sq
    integer   :: AllocStatus, i
    integer   :: wind_distribution 

    pi = 3.1415926
    wind_distribution = 2 !wind_distribution_Rayleigh
    gaussian_dist_halfwidth = 1.3


    gaussian_factor = 0.0
    
    gaussian_factor = -1.0/(2.0*gaussian_dist_halfwidth**2)
    

    ! allocate the output arrays
    allocate(winds%u(num_winds),winds%v(num_winds),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_random_winds"
       error_flag = error_allocate
       return
    end if

    windloop: DO i=1,num_winds
       ! repeat taking random wind vector components untill the
       ! conditions are fulfilled.
       tryloop: DO
          dir = 2.* pi * (2.0*real(random_nr(),r4_) - 1.0)

       

         IF ( (dir .le. (2.* pi)) .and. &
             (dir .ge. 0.)) THEN
            !IF ( (random_nr() .lt. exp(gaussian_factor*(dir**2) ) )  ) THEN
               
               exit tryloop
            !END IF
         END IF
       
       END DO tryloop
       ! store the result
	winds%u(i)  = constant_speed_input * cos(dir)
	winds%v(i)  = constant_speed_input * sin(dir)
        
    END DO windloop

    winds%num_winds   = num_winds
    winds%wind_source = wind_source_random







end subroutine define_constant_speed
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





    !  #]
  subroutine define_constant_winds(winds,num_winds,&
                                  constant_u_input,constant_v_input,&
                                  error_flag)
    !  #[
    type(wind_type),  intent(inout) :: winds     ! will contain the winds
    integer,          intent(in)    :: num_winds        ! input
    real(r4_),        intent(in)    :: constant_u_input ! input
    real(r4_),        intent(in)    :: constant_v_input ! input
    integer,          intent(out)   :: error_flag       ! output

    ! local variable
    integer :: AllocStatus

    error_flag = no_error

    ! allocate the output arrays
    allocate(winds%u(num_winds),winds%v(num_winds),STAT=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR: Allocation problem in define_constant_winds"
       error_flag = error_allocate
       return
    end if

    ! fill the data structure with constant winds
    winds%u(:)        = constant_u_input
    winds%v(:)        = constant_v_input
    winds%num_winds   = num_winds
    winds%wind_source = wind_source_constant_vector

  end subroutine define_constant_winds








    !  #]
  subroutine get_nr_of_winds(winds,num_winds,error_flag)
    !  #[
    type(wind_type),  intent(in)  :: winds      ! input
    integer,          intent(out) :: num_winds  ! output
    integer,          intent(out) :: error_flag ! output
    
    error_flag = no_error

    ! check if the wind struct was properly initialised
    if (winds%struct_was_properly_initialised .ne. wind_canary) then
       print *,"ERROR in get_nr_of_winds:"
       print *,"winds structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    num_winds = winds%num_winds

  end subroutine get_nr_of_winds
    !  #]
  subroutine get_next_u_v_lat_lon(winds,u,v,lat,lon,error_flag)
    !  #[
    type(wind_type),  intent(inout) :: winds    ! input/output
    real(r4_),        intent(out) :: u          ! output
    real(r4_),        intent(out) :: v          ! output
    real(r4_),        intent(out) :: lat        ! output
    real(r4_),        intent(out) :: lon        ! output
    integer,          intent(out) :: error_flag ! output
    
    ! local variables
    integer :: index

    ! NOTE that lat,lon are only returned when winds are read from
    ! an ascii windfile. In all other cases they will be set to missing

    error_flag = no_error

    ! check if the wind struct was properly initialised
    if (winds%struct_was_properly_initialised .ne. wind_canary) then
       print *,"ERROR in get_next_u_v_lat_lon:"
       print *,"winds structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    index = winds%index_last_used_wind + 1

    if (.not. winds%be_silent) then
       if (winds%num_winds .gt. 0) then
          call print_count_on_same_line(index, winds%num_winds)
       else
          call print_count_on_same_line(index)
       end if
    end if

    if (index .lt. 1) then
       print *,"ERROR in get_next_u_v_lat_lon: illegal index value: ",index
       error_flag = error_programming
       return
    end if

    if (index .gt. winds%num_winds) then
       print *,"ERROR in get_next_u_v_lat_lon: no more winds available"
       print *,"Increase the nr of winds in the initialisation or in the"
       print *,"winds file, if you need more winds"
       error_flag = error_programming
       return
    end if

    u   = winds%u(index)
    v   = winds%v(index)
    lat = missing_indicator_real_r4
    lon = missing_indicator_real_r4
    if (associated(winds%lat)) lat = winds%lat(index)
    if (associated(winds%lon)) lon = winds%lon(index)

    winds%index_last_used_wind = index

  end subroutine get_next_u_v_lat_lon
    !  #]
  subroutine get_wind_source_code(wind_source_str,wind_source_code,error_flag)
    !  #[ convert string to numerical wind source code
    character(len=*), intent(in)  :: wind_source_str  ! input
    integer,          intent(out) :: wind_source_code ! output
    integer,          intent(out) :: error_flag       ! output

    error_flag = no_error
    wind_source_code = wind_source_undefined

    select case (to_lowercase(trim(wind_source_str)))
    case("wind_source_undefined")
       wind_source_code = wind_source_undefined
    case("wind_source_u_v_grid")
       wind_source_code = wind_source_u_v_grid
    case("wind_source_sp_dir_grid")
       wind_source_code = wind_source_sp_dir_grid
    case("wind_source_from_windfile")
       wind_source_code = wind_source_from_windfile
    case("wind_source_random")
       wind_source_code = wind_source_random
    case("wind_source_constant_vector")
       wind_source_code = wind_source_constant_vector
    case("wind_source_constant_speed")
       wind_source_code = wind_source_constant_speed
    case default
       print *,"ERROR in get_wind_source_code:"
       print *,"Undefined wind source string: "//trim(wind_source_str)
       error_flag = error_program_usage
       return
    end select

  end subroutine get_wind_source_code
    !  #]
  subroutine get_wind_source_str(wind_source_code,wind_source_str,error_flag)
    !  #[ convert numerical wind source code to string
    integer,          intent(in)  :: wind_source_code ! input
    character(len=*), intent(out) :: wind_source_str  ! output
    integer,          intent(out) :: error_flag       ! output

    error_flag = no_error
    wind_source_str(:) = ' '

    select case(wind_source_code)
    case(wind_source_undefined)
       wind_source_str = "wind_source_undefined"
    case(wind_source_u_v_grid)
       wind_source_str ="wind_source_u_v_grid"
    case(wind_source_sp_dir_grid)
       wind_source_str ="wind_source_sp_dir_grid"
    case(wind_source_from_windfile)
       wind_source_str ="wind_source_from_windfile"
    case(wind_source_random)
       wind_source_str ="wind_source_random"
    case(wind_source_constant_vector)
       wind_source_str ="wind_source_constant_vector"
    case(wind_source_constant_speed)
       wind_source_str ="wind_source_constant_speed"
    case default
       print *,"ERROR in get_wind_source_str:"
       print *,"Undefined wind source code: ",wind_source_code
       error_flag = error_programming
       return
    end select

  end subroutine get_wind_source_str
    !  #]
  subroutine get_wind_distr_code(wind_distr_str,wind_distr_code,error_flag)
    !  #[ convert string to numerical wind distr. code
    character(len=*), intent(in)  :: wind_distr_str  ! input
    integer,          intent(out) :: wind_distr_code ! output
    integer,          intent(out) :: error_flag      ! output

    error_flag = no_error
    wind_distr_code = wind_distribution_undefined

    select case (to_lowercase(trim(wind_distr_str)))
    case("wind_distribution_undefined")
       wind_distr_code = wind_distribution_undefined
    case("wind_distribution_weibull")
       wind_distr_code = wind_distribution_Weibull
    case("wind_distribution_rayleigh")
       wind_distr_code = wind_distribution_Rayleigh
    case("wind_distribution_uniform")
       wind_distr_code = wind_distribution_Uniform
    case default
       print *,"ERROR in get_wind_distr_code:"
       print *,"Undefined wind distr string: "//trim(wind_distr_str)
       error_flag = error_program_usage
       return
    end select

  end subroutine get_wind_distr_code
    !  #]
  subroutine get_wind_distr_str(wind_distr_code,wind_distr_str,error_flag)
    !  #[ convert numerical wind distr. code to string
    integer,          intent(in)  :: wind_distr_code ! input
    character(len=*), intent(out) :: wind_distr_str  ! output
    integer,          intent(out) :: error_flag      ! output

    error_flag = no_error
    wind_distr_str(:) = ' '

    select case(wind_distr_code)
    case(wind_distribution_undefined)
       wind_distr_str = "wind_distribution_undefined"
    case(wind_distribution_Weibull)
       wind_distr_str = "wind_distribution_weibull"
    case(wind_distribution_Rayleigh)
       wind_distr_str = "wind_distribution_rayleigh"
    case(wind_distribution_Uniform)
       wind_distr_str = "wind_distribution_uniform"
       print *,"ERROR in get_wind_distr_str:"
       print *,"Undefined wind distr code: ",wind_distr_code
       error_flag = error_programming
       return
    end select

  end subroutine get_wind_distr_str
    !  #]
  subroutine get_geo_noise_model_code(geo_noise_model_str,&
                                      geo_noise_model_code,error_flag)
    !  #[ convert string to numerical geo noise model code
    character(len=*), intent(in)  :: geo_noise_model_str  ! input
    integer,          intent(out) :: geo_noise_model_code ! output
    integer,          intent(out) :: error_flag           ! output

    error_flag = no_error
    
    select case(geo_noise_model_str)
    case("undefined")
       geo_noise_model_code = geo_noise_model_undefined
    case("old")
       geo_noise_model_code = geo_noise_model_old
    case("exponential_qscat")
       geo_noise_model_code = geo_noise_model_exp_qscat
    case("exponential_ascat")
       geo_noise_model_code = geo_noise_model_exp_ascat
    case default
       print *,"ERROR in get_geo_noise_model_code:"
       print *,"unrecognised geophysical noise model: "//&
            trim(geo_noise_model_str)
       print *,"Implemented algorithms are:"
       print *,"   [old|exponential_qscat|exponential_ascat]"
       error_flag = error_program_usage
       return
    end select

  end subroutine get_geo_noise_model_code
    !  #]
  subroutine get_geo_noise_model_str(geo_noise_model_code,&
                                     geo_noise_model_str,error_flag)
    !  #[ convert numerical geo noise model code to string
    integer,          intent(in)  :: geo_noise_model_code ! input
    character(len=*), intent(out) :: geo_noise_model_str  ! output
    integer,          intent(out) :: error_flag           ! output

    error_flag = no_error
    geo_noise_model_str(:) = ' '

    select case(geo_noise_model_code)
    case(geo_noise_model_undefined)
       geo_noise_model_str = "undefined"
    case(geo_noise_model_old)
       geo_noise_model_str = "old"
    case(geo_noise_model_exp_qscat)
       geo_noise_model_str = "exponential_qscat"
    case(geo_noise_model_exp_ascat)
       geo_noise_model_str = "exponential_ascat"
    case default
       print *,"ERROR in get_geo_noise_model_str:"
       print *,"unrecognised geophysical noise model code: ",&
                geo_noise_model_code
       error_flag = error_program_usage
       return
    end select

  end subroutine get_geo_noise_model_str
    !  #]
  !--------------------------------
  subroutine print_count_on_same_line(count, total_count)
    !  #[
    ! the following code prints a message for every 250th windvector
    ! to have an indication of the progress of the program.
    integer, intent(in) :: count
    integer, optional, intent(in) :: total_count

    ! move cursor one line up (to overprint the previous printed line)
    ! this works fine but system is not standard fortran,
    ! so it may break down on some computers
    !       CALL system('tput cuu1')

    ! cursor-up    = 27 91 65 
    ! cursor-down  = 27 91 66
    ! cursor-right = 27 91 67
    ! cursor-left  = 27 91 68

    ! this does the same using escape codes and is standard fortran
    IF (250*(count/250) .eq. count) THEN
       if (present(total_count)) then
          print *,' calculating wind nr. ',count,' out of ',total_count
       else
          print *,' calculating wind nr. ',count 
       end if
       write (*,'(4(A1))',ADVANCE='NO') achar(27),achar(91),achar(65),' '
    END IF

  end subroutine print_count_on_same_line
    !  #]
  !--------------------------------
  function calc_noise_due_to_windvar(windspeed,wvc_resolution,geo_noise_model) &
                result(geo_noise)
    !  #[
    ! calculate the noise due to the windvariability, 
    ! which in turn is only a function of windspeed.

    ! adapted 18-9-2002 to handle different wvc resolutions.
    ! (default = 50 km)
    real(r4_), intent(in) :: windspeed       ! input
    real(r4_), intent(in) :: wvc_resolution  ! input
    integer,   intent(in) :: geo_noise_model ! input
    real(r4_)             :: geo_noise       ! result

    ! local parameter
    real(r4_), parameter :: eps=0.01

    geo_noise = 0.0
    select case(geo_noise_model)
    case(geo_noise_model_old)
       ! as has been used for the old 2002 rfscat study
       IF (windspeed .lt. 16.0) THEN
          IF (abs(wvc_resolution-50.) .lt. eps) THEN
             ! only valid for 50km cell resolution
             geo_noise = 0.01 * 0.0644 * (windspeed - 16.0)**2
          ELSE
             ! replaced by generalised form:
             ! (zie mijn waarnemingenboek, p.52)
             geo_noise = (1.0*wvc_resolution/50.0)**(1.0/3.0) * &
                  0.01 * 0.0644 * (windspeed - 16.0)**2
          ENDIF
       ELSE
          geo_noise = 0.0
       ENDIF
    case(geo_noise_model_exp_qscat)
       ! as implemented by Maria Belmonte
       ! reference: p.34 of Maria's FoM report
       IF (abs(wvc_resolution-50.) .lt. eps) THEN
          ! only valid for 50km cell resolution qscat/Ku-band case
          geo_noise = 0.05 + 2.2 * exp(-windspeed/2.0) !QSCAT Kgeo for 50 km
       ELSE
          ! the new algo is not yet tested for other resolutions, 
          ! so this case remains identical to geo_noise_model_old 
          IF (windspeed .lt. 16.0) THEN
             geo_noise = (1.0*wvc_resolution/50.0)**(1.0/3.0) * &
                  0.01 * 0.0644 * (windspeed - 16.0)**2
          ELSE
             geo_noise = 0.0
          END IF
       ENDIF
    case(geo_noise_model_exp_ascat)
       ! as implemented by Maria Belmonte
       ! reference: p.34 of Maria's FoM report
       IF ( (abs(wvc_resolution-50.) .lt. eps) .or. &
            (abs(wvc_resolution-25.) .lt. eps)      ) THEN
          ! valid for 25km and 50km cell resolution ascat/C-band case       
          geo_noise = 0.12 * exp(-windspeed/12.0) ! ASCAT Kgeo for 50 km and 25 km
        ELSE
          ! the new algo is not yet tested for other resolutions, 
          ! so this case remains identical to geo_noise_model_old 
          IF (windspeed .lt. 16.0) THEN
             geo_noise = (1.0*wvc_resolution/50.0)**(1.0/3.0) * &
                  0.01 * 0.0644 * (windspeed - 16.0)**2
          ELSE
             geo_noise = 0.0
          END IF
       ENDIF
    end select

    return

  end function calc_noise_due_to_windvar
    !  #]
  !--------------------------------
  subroutine add_noise_to_windfield(u,v,sp,dir,&
                                    stdev_for_windfield_noise)
    !  #[
    real(r4_), intent(inout) :: u, v, sp, dir
    real(r4_), intent(in)    :: stdev_for_windfield_noise

    IF (stdev_for_windfield_noise > 0.0) THEN
       u = u + stdev_for_windfield_noise * real(gaussian_rnd_nr(),r4_)
       v = v + stdev_for_windfield_noise * real(gaussian_rnd_nr(),r4_)
       sp  = uv_to_speed(u,v)
       dir = uv_to_dir(  u,v)
    END IF

  end subroutine add_noise_to_windfield
    !  #]
  !--------------------------------
END Module simwind

