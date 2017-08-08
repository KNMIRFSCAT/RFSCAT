module figure_of_merit
  !  #[ Documentation
  ! a module to calculate the different Figure-of-Merit numbers
  ! as defined in the rfscat/FoM project
  !
  ! Written by J. de Kloe
  ! calc_FoM and calc_FoM_amb are based on the IDL implementation
  ! by Maria Belmonte Rivas
  ! calc_FoM is based on the IDL implementation by Jos de Kloe
  !
  ! Modifications:
  ! 20-May-2009 J. de Kloe  initial version
  ! 29-May-2009 J. de Kloe  added second FoM implementation
  !  #]
  !  #[ Modules used
  USE numerics, only: r4_, r8_, missing_indicator_real_r4
  USE convert, only: speeddir_to_u, speeddir_to_v
  USE inversion, only: inv_input_type, Get_2D_MLE_array, &
       find_minimum_cone_dist,calc_cone_distance, my_mod
  USE errorhandler, only: no_error, error_programming, error_allocate
  USE gnuplot_module, only: gp_plot_type, gp_init_plot, gp_close_plot,&
       output_type_png, output_type_eps, output_type_ps, &
       gp_color_black, gp_color_blue, gp_line_full, &
       gp_add_dataset, gp_add_3d_dataset, &
       gp_create_plot,gp_set_plot_property,gp_add_hor_line,gp_add_ver_line,&
       gp_color_green,gp_line_dotted, DataStyle_Points,gp_symbol_dot
  USE timers, only: timer_type, init_timer, start_timer, stop_timer, &
                    get_num_calls, get_total_time, get_avg_time

  ! explicitely use 4 byte versions of these constants
  USE constants, only: pi=>pi_r4, &
                       two_pi=>two_pi_r4, &
                       deg2rad=>deg2rad_r4

  !  #]
  !  #[ Module parameters
  implicit none

  ! a type to store a number it items for each wind direction
  ! during calculation inside calc_FoM_and_FoM_amb2
  type storage_type
     real(r4_) :: dir_step
     real(r4_) :: speed_step_coarse
     real(r4_) :: speed_step_fine
     real(r4_) :: windspeed1, windspeed2
     real(r4_), dimension(:), pointer :: u,v
     real(r4_), dimension(:), pointer :: du,dv,vector_diff_sq
     real(r4_), dimension(:), pointer :: MLE
     real(r4_), dimension(:), pointer :: probability_obs
     real(r4_), dimension(:), pointer :: probability_bg
  end type storage_type

  real(r4_), parameter :: mean_MLE = 5.0
  real(r4_), parameter :: std_bg   = 5.0
  real(r4_), parameter :: p_backmax = 1./(two_pi*std_bg)

  !  #]
contains
  !--------------------------
  subroutine calc_FoM_and_FoM_amb(inv_input,u_in,v_in,&
                                  FoM,FoM_amb,error_flag,make_plots)
    !  #[

    TYPE(inv_input_type), intent(inout) :: inv_input  ! input
    real(r4_),            intent(in)    :: u_in       ! input
    real(r4_),            intent(in)    :: v_in       ! input
    real(r4_),            intent(out)   :: FoM        ! result 
    real(r4_),            intent(out)   :: FoM_amb    ! result 
    integer,              intent(out)   :: error_flag ! output
    logical, optional,    intent(in)    :: make_plots ! optional input

    ! This subroutine calculates 2 Figure-of-Merit numbers
    ! ==>1: the FoM based on the the vector RMS error
    ! ==>2: the FoM_amb based on the ambiguities

    ! NOTE1: inv_input basically is an input structure,
    !        but when z_space is applied the sigma0 values will be
    !        altered, therefore it must have intent inout.

    ! NOTE2: this FoM_amb needs knowledge about the input wind used for
    !        the simulation (u_in,v_in) to calculate the background 
    !        probability map

    ! define the search space (u,v)
    real(r4_), parameter :: u_start     = -20.0 ! m/s
    real(r4_), parameter :: v_start     = -20.0 ! m/s
    ! low resolution (reasonably fast): 
    !integer,   parameter :: num_u_steps = 201
    !integer,   parameter :: num_v_steps = 201
    !real(r4_), parameter :: u_step      =   0.2 ! m/s
    !real(r4_), parameter :: v_step      =   0.2 ! m/s
    ! better resolution
    integer,   parameter :: num_u_steps = 401
    integer,   parameter :: num_v_steps = 401
    real(r4_), parameter :: u_step      =   0.1 ! m/s
    real(r4_), parameter :: v_step      =   0.1 ! m/s
    ! medium resolution
    !integer,   parameter :: num_u_steps = 801
    !integer,   parameter :: num_v_steps = 801
    !real(r4_), parameter :: u_step      =   0.05 ! m/s
    !real(r4_), parameter :: v_step      =   0.05 ! m/s
    ! high resolution  (retrieving MLE takes 30 s now; gnuplot takes
    !                   several minutes per plot to draw the contours!):
    !integer,   parameter :: num_u_steps = 2001
    !integer,   parameter :: num_v_steps = 2001
    !real(r4_), parameter :: u_step      =   0.02 ! m/s
    !real(r4_), parameter :: v_step      =   0.02 ! m/s
    ! higher
    !integer,   parameter :: num_u_steps = 4001
    !integer,   parameter :: num_v_steps = 4001
    !real(r4_), parameter :: u_step      =   0.01 ! m/s
    !real(r4_), parameter :: v_step      =   0.01 ! m/s
    ! very high resolution
    !integer,   parameter :: num_u_steps = 8001
    !integer,   parameter :: num_v_steps = 8001
    !real(r4_), parameter :: u_step      =   0.005 ! m/s
    !real(r4_), parameter :: v_step      =   0.005 ! m/s

    real(r4_), dimension(num_u_steps,num_v_steps) :: u
    real(r4_), dimension(num_u_steps,num_v_steps) :: v
    real(r4_), dimension(num_u_steps,num_v_steps) :: vector_diff_sq
    real(r4_), dimension(num_u_steps,num_v_steps) :: MLE_array
    real(r4_), dimension(num_u_steps,num_v_steps) :: probability_obs
    real(r4_), dimension(num_u_steps,num_v_steps) :: probability_bg

    real(r4_) :: integr_prob, FoM_amb_1, FoM_amb_2
    real(r4_) :: du, dv
    integer   :: i,j,deg_freedom
    logical   :: do_make_plots

    ! only for experimental timing, to be removed from final code!
    !logical, parameter :: check_timing = .true.
    logical, parameter :: check_timing = .false.
    type(timer_type) :: timer_fill_uv, timer_get_MLE
    type(timer_type) :: timer_MLE_to_prob, timer_calc_bg
    type(timer_type) :: timer_int1, timer_int2, timer_int3

    ! init
    error_flag = no_error
    fom     = missing_indicator_real_r4
    fom_amb = missing_indicator_real_r4

    do_make_plots = .false.
    if (present(make_plots)) do_make_plots = make_plots
    if (check_timing) then
       call init_timer(timer_fill_uv)
       call init_timer(timer_get_MLE)
       call init_timer(timer_MLE_to_prob)
       call init_timer(timer_calc_bg)
       call init_timer(timer_int1)
       call init_timer(timer_int2)
       call init_timer(timer_int3)
    end if

    if (check_timing) call start_timer(timer_fill_uv)
    ! fill the u and v arrays
    do i=1,num_u_steps
       do j=1,num_v_steps
          u(i,j) = u_start+u_step*(i-1)
          v(i,j) = v_start+v_step*(j-1)
          du = u(i,j) - u_in
          dv = v(i,j) - v_in
          vector_diff_sq(i,j) = du*du+dv*dv
       end do
    end do   
    if (check_timing) call stop_timer(timer_fill_uv)

    if (check_timing) call start_timer(timer_get_MLE)
    call Get_2D_MLE_array(inv_input,u,v,MLE_array)

    ! some kind of normalisation? only for qscat?
    MLE_array = MLE_array/mean_MLE
    if (check_timing) call stop_timer(timer_get_MLE)

    if (do_make_plots) then
       print *,"Input wind: u_in = ",u_in," v_in = ",v_in
       print *,"Scanned wind: u_min=",minval(u)," u_max=",maxval(u),&
            " step: ",u_step
       print *,"Scanned wind: v_min=",minval(v)," v_max=",maxval(v),&
            " step: ",v_step
       print *,"MLE_array_minx/max=",minval(MLE_array),maxval(MLE_array)
       call plot_2d(u,v,MLE_array,"MLE on the u,.v plane",&
                    "MLE_map",error_flag,scale_to_edgeval_and_min=.true.)
       if (error_flag .ne. no_error) return
    end if

    ! inside SDP/sdp/SdpInversion.F90 probability is defined as:
    !Lparameter = 1.4 ! constant needed to turn Rn into a probability
    !                 ! eq. 3.5 page 63  Phd Thesis Marcos Portabella
    !ReciprokeLparameter   = 1.0/1.4
    !  ExpMeanMLE is taken from LUT, as fn. of nodenr and windspeed
    !  stored in file: 
    !    SDP/data/little_endian/mean_bufr_1r_mle_knmi9_50_r5_mm.dat
    !ProbMin = 1.0e-15
    !ReciprokeExpMeanMLE = 1.0 / ExpMeanMLE
    !Rn(k)     = abs(MLE) * ReciprokeExpMeanMLE
    !p         = exp(-Rn(k)*ReciprokeLparameter)
    !prob(k)   = max(p,ProbMin) ! Marcos: to avoid zero prob in AR cost function

    ! for ERS and ASCAT the ExpMeanMLE=1
    ! and the l_parameter = 2.0 for ERS and 1.5 for ASCAT
    !
    ! prob = exp(-abs(MLE)/l_par)

    ! after consulting Maria it appears that the rule depends on the number
    ! of freedoms:
    ! ==>ascat has 3 views per wvc, but the MLE map has only 2 dimensions
    !    so you get a 1D Chi-square distribution
    ! ==>qscat has 4 views per wvc, but the MLE map has only 2 dimensions
    !    so you get a 2D Chi-square distribution
    !
    ! Therefore these 2 lines are translated into a more generic function
    !if mode eq 'ascat' then p_obs=1./sqrt(2.*pi*mlemap2)*exp(-mlemap2/2.)
    !if mode eq 'qscat' then p_obs=0.5*exp(-mlemap2/2.)
    !
    ! For now I will ignore the Lparameter which is an empirical adaptation
    ! to real data, which is (not yet) possible for the general rfscat case

    ! calculate probability_obs from MLE for all u,v values
    ! and at the same time calculate probability_bg
    deg_freedom = inv_input%nr_of_views-2
    do i=1,num_u_steps
       do j=1,num_v_steps
          ! Observation probability
          if (check_timing) call start_timer(timer_MLE_to_prob)
          call generic_MLE_to_probability(MLE_array(i,j),deg_freedom,&
                                          probability_obs(i,j),error_flag)
          if (error_flag .ne. no_error) return
          if (check_timing) call stop_timer(timer_MLE_to_prob)

          ! Background probability
          if (check_timing) call start_timer(timer_calc_bg)
          call calc_background_probability(vector_diff_sq(i,j),std_bg,&
                                           probability_bg(i,j) )
          if (check_timing) call stop_timer(timer_calc_bg)

          !if (probability_obs(i,j) .gt. 0.1) &
          !     print *,"i=",i,",j=",j,",MLE_array(i,j) = ",MLE_array(i,j),&
          !     " probability_obs(i,j) = ",probability_obs(i,j)
       end do
    end do

    if (do_make_plots) then
       print *,"probability_obs minx/max=",&
            minval(probability_obs),maxval(probability_obs)
       call plot_2d(u,v,probability_obs,"probability of the observation",&
                    "probability_obs",error_flag,&
                    num_contours=10,scale_to_edgeval_and_max=.true.)
       if (error_flag .ne. no_error) return
       print *,"probability_bg minx/max=",&
            minval(probability_bg),maxval(probability_bg)
       call plot_2d(u,v,probability_bg,"probability of the background",&
                    "probability_bg",error_flag,&
                    num_contours=10,scale_to_edgeval_and_max=.true.)
       if (error_flag .ne. no_error) return
       call plot_2d(u,v,probability_obs*probability_bg,&
                    "probability_obs * probability_bg",&
                    "probability_obs_times_bg",error_flag,&
                    num_contours=10,scale_to_edgeval_and_max=.true.)
       if (error_flag .ne. no_error) return
       call plot_2d(u,v,probability_obs*(p_backmax-probability_bg),&
                    "probability_obs * (p_backmax-probability_bg)",&
                    "probability_obs_times_bgmax_min_bg",error_flag,&
                    num_contours=10,scale_to_edgeval_and_max=.true.)
       if (error_flag .ne. no_error) return
    end if

    if (check_timing) call start_timer(timer_int1)
    ! Normalization p_obs*p_back by the integral of the
    ! multiplied probability distributions
    call do_2D_integral(probability_obs*probability_bg,&
                        num_u_steps,num_v_steps,u_step,v_step,&
                        integr_prob)

    !print *,"integr_prob = ",integr_prob*1.e4

    probability_obs = probability_obs/integr_prob
    if (check_timing) call stop_timer(timer_int1)

    !  Compute FoM integrals
    FoM = 0
    FoM_amb_1 = 0
    FoM_amb_2 = 0

    if (check_timing) call start_timer(timer_int2)
    call do_2D_integral(probability_obs*probability_bg*vector_diff_sq,&
                        num_u_steps,num_v_steps,u_step,v_step,&
                        FoM)
    if (check_timing) call stop_timer(timer_int2)

    ! This will always be 1 thanks to the normalisation above
    ! so skip this integral
    !call do_2D_integral(probability_obs*probability_bg,&
    !                    num_u_steps,num_v_steps,u_step,v_step,&
    !                    FoM_amb_1)
    FoM_amb_1 = 1.

    if (check_timing) call start_timer(timer_int3)
    call do_2D_integral(probability_obs*(p_backmax-probability_bg),&
                        num_u_steps,num_v_steps,u_step,v_step,&
                        FoM_amb_2)
    if (check_timing) call stop_timer(timer_int3)

    ! Store results
    ! Note that 2*std_bg actually is the value of the 2nd integral
    ! so that one does not need to be calculated explicitely.
    FoM     = sqrt(FoM/(2.*std_bg))
    FoM_amb = fom_amb_2/fom_amb_1

    if (check_timing) then
       ! report timing results
       print *,"--------------"
       print *,"Timer results:"
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_fill_uv    :",&
            " num_calls  = ",get_num_calls( timer_fill_uv),&
            " total_time = ",get_total_time(timer_fill_uv),&
            " avg_time   = ",get_avg_time(  timer_fill_uv)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_get_MLE    :",&
            " num_calls  = ",get_num_calls( timer_get_MLE),&
            " total_time = ",get_total_time(timer_get_MLE),&
            " avg_time   = ",get_avg_time(  timer_get_MLE)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_MLE_to_prob:",&
            " num_calls  = ",get_num_calls( timer_MLE_to_prob),&
            " total_time = ",get_total_time(timer_MLE_to_prob),&
            " avg_time   = ",get_avg_time(  timer_MLE_to_prob)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_calc_bg    :",&
            " num_calls  = ",get_num_calls( timer_calc_bg),&
            " total_time = ",get_total_time(timer_calc_bg),&
            " avg_time   = ",get_avg_time(  timer_calc_bg)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_int1       :",&
            " num_calls  = ",get_num_calls( timer_int1),&
            " total_time = ",get_total_time(timer_int1),&
            " avg_time   = ",get_avg_time(  timer_int1)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_int2       :",&
            " num_calls  = ",get_num_calls( timer_int2),&
            " total_time = ",get_total_time(timer_int2),&
            " avg_time   = ",get_avg_time(  timer_int2)
       print "(a,a,i8,a,f8.3,a,f8.3)","timer_int3       :",&
            " num_calls  = ",get_num_calls( timer_int3),&
            " total_time = ",get_total_time(timer_int3),&
            " avg_time   = ",get_avg_time(  timer_int3)
       print *,"--------------"
       
       stop 1
    end if

  end subroutine calc_FoM_and_FoM_amb

    !  #]
  subroutine calc_FoM_and_FoM_amb2(inv_input,u_in,v_in,&
                                   FoM,FoM_amb,error_flag,make_plots)
    !  #[
    TYPE(inv_input_type), intent(inout) :: inv_input  ! input
    real(r4_),            intent(in)    :: u_in       ! input
    real(r4_),            intent(in)    :: v_in       ! input
    real(r4_),            intent(out)   :: FoM        ! result 
    real(r4_),            intent(out)   :: FoM_amb    ! result 
    integer,              intent(out)   :: error_flag ! output
    logical, optional,    intent(in)    :: make_plots ! optional input

    ! This subroutine calculates 2 Figure-of-Merit numbers
    ! ==>1: the FoM based on the the vector RMS error
    ! ==>2: the FoM_amb based on the ambiguities

    ! NOTE1: inv_input basically is an input structure,
    !        but when z_space is applied the sigma0 values will be
    !        altered, therefore it must have intent inout.

    ! NOTE2: this FoM_amb needs knowledge about the input wind used for
    !        the simulation (u_in,v_in) to calculate the background 
    !        probability map

    ! NOTE3: this alternative implementation has a different approach
    !        in doing the integrations needed for the FoM.
    !        It tries to be smart in finding MLE locations that matter
    !        before actually starting the probability calculations and the
    !        integrations.

    ! define the search space (u,v)
    real(r4_), parameter :: dir_start         = 0.0 ! deg
    integer,   parameter :: num_dir_steps     = 360 ! 720 ! 360
    real(r4_), parameter :: dir_step          = 1.0 ! 0.5 ! 1.0 ! deg
    ! used for finding locations to integrate
    real(r4_), parameter :: speed_step_coarse = 0.1 ! m/s
    ! used for doing the actual integrations
    real(r4_), parameter :: speed_step_fine   = 0.02 ! 0.005 ! m/s

    ! discard locations for which the MLE has a value above
    ! overal_min_MLE*MLE_factor
    real(r4_), parameter :: MLE_factor = 100.

    real(r4_), dimension(num_dir_steps) :: winddir
    real(r4_), dimension(num_dir_steps) :: min_windspeed
    real(r4_), dimension(num_dir_steps) :: min_MLE

    type(storage_type), dimension(num_dir_steps) :: store

    real(r4_) :: integr_prob, FoM_amb_1, FoM_amb_2

    real(r4_) :: windspeed, MLE, overal_min_MLE
    integer   :: i, j, count, num_sp_steps
    integer   :: deg_freedom
    logical   :: do_make_plots

    ! init
    error_flag = no_error
    fom     = missing_indicator_real_r4
    fom_amb = missing_indicator_real_r4

    do_make_plots = .false.
    if (present(make_plots)) do_make_plots = make_plots

    deg_freedom = inv_input%nr_of_views-2
    
    dirloop1: do i=1,num_dir_steps
       winddir(i) = dir_start + dir_step*(i-1)

       ! find windspeed that has minimum cone_distance for this direction
       CALL find_minimum_cone_dist(inv_input, winddir(i), &
                                   min_windspeed(i), min_MLE(i))
    end do dirloop1

    overal_min_MLE = minval(min_MLE)

    dirloop2: do i=1,num_dir_steps
       nullify(store(i)%u,&
               store(i)%v,&
               store(i)%du,&
               store(i)%dv,&
               store(i)%vector_diff_sq,&
               store(i)%MLE,&
               store(i)%probability_obs,&
               store(i)%probability_bg)

       ! some fields for convenience
       ! store these always, even if there is no data to store
       store(i)%dir_step          = dir_step
       store(i)%speed_step_coarse = speed_step_coarse
       store(i)%speed_step_fine   = speed_step_fine

       if ( min_MLE(i) .gt. MLE_factor*overal_min_MLE) then
          store(i)%windspeed1 = min_windspeed(i)
          store(i)%windspeed2 = min_windspeed(i)
          cycle dirloop2
       end if

       !print *,"winddir=",winddir(i)," min_windspeed=",min_windspeed(i),&
       !     " min_MLE=",min_MLE(i)

       ! walk along this dir with steps speed_step_coarse in both 
       ! windspeed directions untill condistance becomes 4*min_MLE
       windspeed = min_windspeed(i)
       count=0
       decr_windspeed:do
          windspeed = windspeed - speed_step_coarse
          count = count + 1
          if (windspeed .lt. 0.2) exit decr_windspeed
          MLE = calc_cone_distance(inv_input, windspeed, winddir(i))
          !if (MLE .gt. 4.*min_MLE(i)) exit decr_windspeed
          if (MLE .gt. MLE_factor*overal_min_MLE) exit decr_windspeed
       end do decr_windspeed
       store(i)%windspeed1 = windspeed
       !print *,"windspeed1 = ",windspeed," count = ",count

       windspeed = min_windspeed(i)
       count=0
       incr_windspeed: do
          windspeed = windspeed + speed_step_coarse
          count = count + 1
          if (windspeed .lt. 0.2) exit incr_windspeed
          MLE = calc_cone_distance(inv_input, windspeed, winddir(i))
          !if (MLE .gt. 4.*min_MLE(i)) exit incr_windspeed
          if (MLE .gt. MLE_factor*overal_min_MLE) exit incr_windspeed
       end do incr_windspeed
       store(i)%windspeed2 = windspeed
       !print *,"windspeed2 = ",windspeed," count = ",count

       num_sp_steps = nint((store(i)%windspeed2-store(i)%windspeed1)/&
                           speed_step_fine)+1
       allocate(store(i)%u(num_sp_steps),&
                store(i)%v(num_sp_steps),&
                store(i)%du(num_sp_steps),&
                store(i)%dv(num_sp_steps),&
                store(i)%vector_diff_sq(num_sp_steps),&
                store(i)%MLE(num_sp_steps),&
                store(i)%probability_obs(num_sp_steps),&
                store(i)%probability_bg(num_sp_steps))

       ! fill the store arrays
       speed_loop1: do j=1,num_sp_steps
          windspeed = store(i)%windspeed1 + speed_step_fine*(j-1)
          store(i)%u(j) = speeddir_to_u(windspeed,winddir(i))
          store(i)%v(j) = speeddir_to_v(windspeed,winddir(i))
          store(i)%du(j) = store(i)%u(j) - u_in
          store(i)%dv(j) = store(i)%v(j) - v_in
          store(i)%vector_diff_sq(j) = store(i)%du(j)*store(i)%du(j)+&
                                       store(i)%dv(j)*store(i)%dv(j)
          store(i)%MLE(j) = &
               calc_cone_distance(inv_input, windspeed, winddir(i))

          ! some kind of normalisation? only for qscat?
          store(i)%MLE(j) = store(i)%MLE(j)/mean_MLE

          ! calculate observation probability
          call generic_MLE_to_probability(store(i)%MLE(j),&
                                          deg_freedom,&
                                          store(i)%probability_obs(j),&
                                          error_flag)
          if (error_flag .ne. no_error) return

          ! calculate background probability
          call calc_background_probability(store(i)%vector_diff_sq(j),&
                                           std_bg,&
                                           store(i)%probability_bg(j) )

       end do speed_loop1
    end do dirloop2

    if (do_make_plots) then
       call plot_integration_domain(winddir,store,speed_step_fine,error_flag)
    end if

    ! Normalization p_obs*p_back by the integral of the
    ! multiplied probability distributions
    call do_2D_integral_restr_domain(&
              "probability_obs*probability_bg",&
              store, integr_prob)

    !print *,"integr_prob = ",integr_prob*1.e4

    ! apply the normalisation on probability_obs
    dir_loop3: do i=1,size(store)
       ! discard directions for which all data is (close to) zero
       if (.not. associated(store(i)%u)) cycle dir_loop3
       speed_loop2: do j=1,size(store(i)%u)
          store(i)%probability_obs(j) = store(i)%probability_obs(j)/integr_prob
       end do speed_loop2
    end do dir_loop3

    !  Compute FoM integrals
    FoM = 0
    FoM_amb_1 = 0
    FoM_amb_2 = 0

    call do_2D_integral_restr_domain(&
              "probability_obs*probability_bg*vector_diff_sq",&
              store, FoM)

    ! This will always be 1 thanks to the normalisation above
    ! so skip this integral
    !call do_2D_integral_restr_domain(&
    !          "probability_obs*probability_bg",&
    !          store, FoM_amb_1)
    FoM_amb_1 = 1.

    call do_2D_integral_restr_domain(&
              "probability_obs*(p_backmax-probability_bg)",&
              store, FoM_amb_2)

    ! Store results
    ! Note that 2*std_bg actually is the value of the 2nd integral
    ! so that one does not need to be calculated explicitely.
    FoM     = sqrt(FoM/(2.*std_bg))
    FoM_amb = fom_amb_2/fom_amb_1

    ! clean up
    dirloop3: do i=1,num_dir_steps
       if (associated(store(i)%u)) deallocate(store(i)%u)
       if (associated(store(i)%v)) deallocate(store(i)%v)
       if (associated(store(i)%du)) deallocate(store(i)%du)
       if (associated(store(i)%dv)) deallocate(store(i)%dv)
       if (associated(store(i)%vector_diff_sq)) &
            deallocate(store(i)%vector_diff_sq)
       if (associated(store(i)%MLE)) deallocate(store(i)%MLE)
       if (associated(store(i)%probability_obs)) &
            deallocate(store(i)%probability_obs)
       if (associated(store(i)%probability_bg)) &
            deallocate(store(i)%probability_bg)
    end do dirloop3

  end subroutine calc_FoM_and_FoM_amb2
    !  #]
  subroutine calc_FoM_old(fom_old)
    !  #[

    ! reference implementation of the FoM for all four versions 
    ! as defined and used in the old rfscat project (2000-2002)
    !
    real(r4_), intent(out) :: fom_old

    ! local variables and parameters
    logical, parameter :: apply_correction_to_weights = .true.
    !logical, parameter :: apply_correction_to_weights = .false.

    fom_old = missing_indicator_real_r4


    !;-------------------------------------------------------------
    !; interface of the ads_fom_all subroutine:
    !;
    !; function call:
    !;    ads_fom_all, FoM, score_r, score_u, score_v, $
    !;                 max_nr_of_wvcs, nws, wvcarr, inp_u, inp_v, $
    !;                 found_u1, found_v1, found_u2, found_v2, $
    !;                 found_u3, found_v3, found_u4, found_v4, $
    !;                 dirs, numsol, resolution, implementation
    !;
    !; outputs:  
    !; 4 real arrays with dimension [0..max_nr_of_wvcs-1]
    !;   FoM     : the FoM value for each WVC
    !;   score_r : the score_r for each WVC
    !;   score_u : the score_u for each WVC
    !;   score_v : the score_v for each WVC
    !;
    !; inputs:
    !; 2 integer numbers
    !;   max_nr_of_wvcs : number of WVC's present in the data
    !;   nws            : number of nodes to process
    !;
    !; an integer array with dimension [0..nws-1]
    !;   wvcarr         : listt the WVC number for each node
    !;
    !; 10 real arrays with dimension [0..nws-1]
    !;   inp_u, inp_v       : the true/background windcomponents for each node
    !;   found_u1, found_v1 : the found windcomponents for solution 1 for each node
    !;   found_u2, found_v2 : the found windcomponents for solution 2 for each node
    !;   found_u3, found_v3 : the found windcomponents for solution 3 for each node
    !;   found_u4, found_v4 : the found windcomponents for solution 4 for each node
    !;
    !; a two dimensional real arrays with dimension [0..nws-1][0..3]
    !;   dirs               : the direction of the windvector for each node
    !;                        for each solution 
    !;
    !; an integer array with dimension [0..nws-1]
    !;   numsol             : number of found solutions for each node
    !;
    !; one real number
    !;   resolution         : the measurement resolution in km
    !;
    !; one integer number
    !;   implementation     : the implementation [1,2,3 or 4]
    !;-------------------------------------------------------------
    !;  two further switches can be set to false/true to 
    !;  return to the versions that contain the two described bugs:
    !;
    !;  -apply_correction_to_implementation_2
    !;  -apply_correction_to_weights 
    !;-------------------------------------------------------------

!pro ads_fom_all, FoM, score_r, score_u, score_v, $
!                 max_nr_of_wvcs, nws, wvcarr, inp_u, inp_v, $
!                 found_u1, found_v1, found_u2, found_v2, $
!                 found_u3, found_v3, found_u4, found_v4, $
!                 dirs, numsol, resolution, implementation
!
!  true = 1 eq 1 & false = 1 eq 0
!
!  apply_correction_to_implementation_2 = false
!
!
!  expected_SD = (1.0*resolution/50)^(1.0/3.0) * 1.5
!  ; = 1.5 [m/s] for 50 km resolution
!
!  ; vars used for storing the distribution of solutions
!  nrbins = 201
!  startval = -50.0
!  stepval  =   0.5
!  binned_u = fltarr(nrbins); -50 to +50, step 0.5
!  binned_v = fltarr(nrbins); -50 to +50, step 0.5
!
!  ; used for output of the results
!  FoM = fltarr(max_nr_of_wvcs)
!  score_u = fltarr(max_nr_of_wvcs)
!  score_v = fltarr(max_nr_of_wvcs)
!  score_r = fltarr(max_nr_of_wvcs)
!
!  ; repeat the procedure for each satellite node
!  FOR wvc = 0,max_nr_of_wvcs-1 DO $
!  BEGIN
!    binned_u(*) = 0.
!    binned_v(*) = 0.
!
!    ; loop over all windvectors and produce the distribution
!    ; function for the diff in components for u and v
!    FOR i=0L,nws-1 DO $
!    IF (wvcarr(i) eq wvc) THEN $
!    BEGIN
!
!      IF (numsol(i) ge 1) THEN $
!      BEGIN
!        weight = FoM_calc_weight(numsol(i),1,$
!                                 dirs(i,0),  dirs(i,1),  $
!                                 dirs(i,2),  dirs(i,3),  $
!                                 implementation           )
!
!        vector_diff_u1 = inp_u(i)-found_u1(i)
!        index = FIX((vector_diff_u1-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_u(index) = binned_u(index)+weight
!
!        vector_diff_v1 = inp_v(i)-found_v1(i)
!        index = FIX((vector_diff_v1-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_v(index) = binned_v(index)+weight
!      END
!
!      IF (numsol(i) ge 2) THEN $
!      BEGIN
!        weight = FoM_calc_weight(numsol(i),2,$
!                                 dirs(i,0),  dirs(i,1),  $
!                                 dirs(i,2),  dirs(i,3),  $
!                                 implementation           )
!
!        vector_diff_u2 = inp_u(i)-found_u2(i)
!        index = FIX((vector_diff_u2-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_u(index) = binned_u(index)+weight
!
!        vector_diff_v2 = inp_v(i)-found_v2(i)
!        index = FIX((vector_diff_v2-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_v(index) = binned_v(index)+weight
!      END
!
!      IF (numsol(i) ge 3) THEN $
!      BEGIN
!        weight = FoM_calc_weight(numsol(i),3,$
!                                 dirs(i,0),  dirs(i,1),  $
!                                 dirs(i,2),  dirs(i,3),  $
!                                 implementation           )
!
!        vector_diff_u3 = inp_u(i)-found_u3(i)
!        index = FIX((vector_diff_u3-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_u(index) = binned_u(index)+weight
!
!        vector_diff_v3 = inp_v(i)-found_v3(i)
!        index = FIX((vector_diff_v3-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_v(index) = binned_v(index)+weight
!      END
!
!      IF (numsol(i) ge 4) THEN $
!      BEGIN
!        weight = FoM_calc_weight(numsol(i),4,$
!                                 dirs(i,0),  dirs(i,1),  $
!                                 dirs(i,2),  dirs(i,3),  $
!                                 implementation           )
!
!        vector_diff_u4 = inp_u(i)-found_u4(i)
!        index = FIX((vector_diff_u4-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_u(index) = binned_u(index)+weight
!
!        vector_diff_v4 = inp_v(i)-found_v4(i)
!        index = FIX((vector_diff_v4-startval)/stepval)
!        index = max([0,min([index,nrbins-1])])
!        binned_v(index) = binned_v(index)+weight
!      END
!
!    END ; end IF (wvcarr(i) eq wvc) 
!
!    ; let the distributions have an integral of 1
!    IF (total(binned_u) ne 0.0) THEN $
!      obs_prob_u = 1.0*binned_u/total(binned_u) $
!    ELSE  obs_prob_u = 0.0*binned_u
!
!    ; let the distributions have an integral of 1
!    IF (total(binned_v) ne 0.0) THEN $
!      obs_prob_v = 1.0*binned_v/total(binned_v) $
!    ELSE obs_prob_v = 0.0*binned_v
!
!    ; init some new vars
!    anal_prob_u = 0.*binned_u
!    anal_prob_v = 0.*binned_v
!    sum_weighted_mean_squares_u = 0.0
!    sum_weighted_mean_squares_v = 0.0
!    sum_anal_prob_u             = 0.0
!    sum_anal_prob_v             = 0.0
!
!    ; produce a new (smaller) distribution by multiplying the
!    ; found distribution by a Gaussian background distribution.
!    exp_factor = -1.0/(2.0*expected_SD^2) 
!    ; = -1.0/4.5 if expected_SD = 1.5
!    ; (4.5 = 2*(1.5^2) so halfwidth = 1.5 m/s )
!
!    vector_diff = startval + stepval*(indgen(nrbins)+0.5)
!    backgr_prob = exp(exp_factor*vector_diff^2)
!
!    anal_prob_u = backgr_prob * obs_prob_u
!    anal_prob_v = backgr_prob * obs_prob_v
!
!    sum_weighted_mean_squares_u = total(anal_prob_u*vector_diff^2)
!    sum_weighted_mean_squares_v = total(anal_prob_v*vector_diff^2)
!
!    sum_anal_prob_u = total(anal_prob_u)
!    sum_anal_prob_v = total(anal_prob_v)
!
!    ; compute the rms from this distribution
!    IF (sum_anal_prob_u ne 0.0) THEN $
!      rms_u = sqrt(sum_weighted_mean_squares_u / sum_anal_prob_u) $
!    ELSE rms_u = 0.0
!
!    ; compute the rms from this distribution
!    IF (sum_anal_prob_v ne 0.0) THEN $
!      rms_v = sqrt(sum_weighted_mean_squares_v / sum_anal_prob_v) $
!    ELSE rms_v = 0.0
!
!    score_u(wvc) = rms_u/expected_SD
!    score_v(wvc) = rms_v/expected_SD
!
!    ; Compute a ranking score using Pa
!    rr = 0.
!    r1 = 0.
!    nvectors = 0L
!    FOR i=0L,nws-1 DO $
!    IF (wvcarr(i) eq wvc) THEN $
!    BEGIN
!
!      IF (apply_correction_to_implementation_2) THEN $
!      BEGIN
!        IF (numsol(i) ge 1) THEN $
!        BEGIN
!          nvectors = nvectors + 1
!          vector_diff_u = abs(inp_u(i)-found_u1(i))
!          vector_diff_v = abs(inp_v(i)-found_v1(i))
!          index_u = FIX((vector_diff_u-startval)/stepval)
!          index_v = FIX((vector_diff_v-startval)/stepval)
!          index_u = max([0,min([index_u,nrbins-1])])
!          index_v = max([0,min([index_v,nrbins-1])])
!          IF ( (implementation eq 1) OR (implementation eq 2) ) THEN $
!            r1 = r1 + obs_prob_u(index_u)*obs_prob_v(index_v)
!          IF ( (implementation eq 3) OR (implementation eq 4) ) THEN $
!            r1 = r1 + anal_prob_u(index_u)*anal_prob_v(index_v)
!        END
!      END ELSE $
!      BEGIN
!        IF (numsol(i) ge 2) THEN $
!        BEGIN
!          nvectors = nvectors + 1
!          vector_diff_u = abs(inp_u(i)-found_u1(i))
!          vector_diff_v = abs(inp_v(i)-found_v1(i))
!          index_u = FIX((vector_diff_u-startval)/stepval)
!          index_v = FIX((vector_diff_v-startval)/stepval)
!          index_u = max([0,min([index_u,nrbins-1])])
!          index_v = max([0,min([index_v,nrbins-1])])
!          IF ( (implementation eq 1) OR (implementation eq 2) ) THEN $
!            r1 = r1 + obs_prob_u(index_u)*obs_prob_v(index_v)
!          IF ( (implementation eq 3) OR (implementation eq 4) ) THEN $
!            r1 = r1 + anal_prob_u(index_u)*anal_prob_v(index_v)
!        END
!      END
!
!      IF (numsol(i) ge 2) THEN $
!      BEGIN
!        vector_diff_u = abs(inp_u(i)-found_u2(i))
!        vector_diff_v = abs(inp_v(i)-found_v2(i))
!        index_u = FIX((vector_diff_u-startval)/stepval)
!        index_v = FIX((vector_diff_v-startval)/stepval)
!        index_u = max([0,min([index_u,nrbins-1])])
!        index_v = max([0,min([index_v,nrbins-1])])
!        IF ( (implementation eq 1) OR (implementation eq 2) ) THEN $
!          rr = rr + obs_prob_u(index_u)*obs_prob_v(index_v)
!        IF ( (implementation eq 3) OR (implementation eq 4) ) THEN $
!          rr = rr + anal_prob_u(index_u)*anal_prob_v(index_v)
!      END
!
!      IF (numsol(i) ge 3) THEN $
!      BEGIN
!        vector_diff_u = abs(inp_u(i)-found_u3(i))
!        vector_diff_v = abs(inp_v(i)-found_v3(i))
!        index_u = FIX((vector_diff_u-startval)/stepval)
!        index_v = FIX((vector_diff_v-startval)/stepval)
!        index_u = max([0,min([index_u,nrbins-1])])
!        index_v = max([0,min([index_v,nrbins-1])])
!        IF ( (implementation eq 1) OR (implementation eq 2) ) THEN $
!          rr = rr + obs_prob_u(index_u)*obs_prob_v(index_v)
!        IF ( (implementation eq 3) OR (implementation eq 4) ) THEN $
!          rr = rr + anal_prob_u(index_u)*anal_prob_v(index_v)
!      END
!
!      IF (numsol(i) ge 4) THEN $
!      BEGIN
!        vector_diff_u = abs(inp_u(i)-found_u4(i))
!        vector_diff_v = abs(inp_v(i)-found_v4(i))
!        index_u = FIX((vector_diff_u-startval)/stepval)
!        index_v = FIX((vector_diff_v-startval)/stepval)
!        index_u = max([0,min([index_u,nrbins-1])])
!        index_v = max([0,min([index_v,nrbins-1])])
!        IF ( (implementation eq 1) OR (implementation eq 2) ) THEN $
!          rr = rr + obs_prob_u(index_u)*obs_prob_v(index_v)
!        IF ( (implementation eq 3) OR (implementation eq 4) ) THEN $
!          rr = rr + anal_prob_u(index_u)*anal_prob_v(index_v)
!      END
!
!    END ; end IF (wvcarr(i) eq wvc)
!
!    IF (r1+rr ne 0.0) THEN score_r(wvc) = (2*rr) / (r1 + rr) $
!                      ELSE score_r(wvc) = 0.0
!
!    FoM(wvc) = 0.2*score_r(wvc) + 0.4*score_u(wvc) + 0.4*score_v(wvc)
!
!  END ; loop over wvc
!
!END ; end of this subroutine


  end subroutine calc_FoM_old

    !  #]
  subroutine FoM_old_calc_weight(num_sol,which_sol,winddirs,&
                                 implementation,weight,sort_solutions,&
                                 apply_correction_to_weights)
    !  #[
    integer,   intent(in)  :: num_sol
    integer,   intent(in)  :: which_sol
    real(r4_), dimension(num_sol), intent(in)  :: winddirs
    integer,   intent(in)  :: implementation
    real(r4_), intent(out) :: weight
    logical, optional, intent(in) :: sort_solutions
    logical, optional, intent(in) :: apply_correction_to_weights

    ! local variables
    logical :: equal_weights
    logical :: do_sort_solutions
    logical :: do_apply_correction_to_weights
    integer :: which_sorted_sol, left_sol, right_sol
    real(r4_) :: sector
    real(r4_), dimension(:), pointer :: sorted_winddirs

    do_apply_correction_to_weights = .false.
    if (present(apply_correction_to_weights)) &
         do_apply_correction_to_weights = apply_correction_to_weights

    equal_weights = .true.
    if ( (implementation .eq. 2) .or. &
         (implementation .eq. 4)      ) equal_weights = .false.

    ! an old version of this code just assumed the solutions where sorted
    ! in counter clock-wise direction (see section 4.2.3 in rfscat report_3a, p.28)
    ! This is not always true, so normally the solutions should be sorted first,
    ! before calculating the sectors. To allow reproducing the old wrong results
    ! the optional sort_solutions flag maybe set to false.
    do_sort_solutions = .true.
    if (present(sort_solutions)) do_sort_solutions = sort_solutions

    weight = missing_indicator_real_r4

    if (num_sol .eq. 1) then
       weight = 1.0
       return
    end if
    
    if (num_sol .eq. 2) then
       weight = 0.5
       return
    end if

    if (equal_weights) then
       weight = 1./num_sol
       return
    end if

    nullify(sorted_winddirs)
    allocate(sorted_winddirs(num_sol))
    if (do_sort_solutions) then
       call sort_winddirs(num_sol,winddirs,which_sol,&
                          sorted_winddirs,which_sorted_sol)
    else
       sorted_winddirs(1:num_sol) = winddirs(1:num_sol)
       which_sorted_sol = which_sol
    end if

    ! get the left and right neighbours of the current solution
    ! (they should exist, since at this point we know there are
    ! at least 3 solutions)
    left_sol  = get_neighbour_solution(num_sol,which_sorted_sol,left=.true.)
    right_sol = get_neighbour_solution(num_sol,which_sorted_sol,right=.true.)
    
    if (do_apply_correction_to_weights) then
       sector = sorted_winddirs(right_sol)-sorted_winddirs(left_sol)
       sector = my_mod(sector,360.)
       sector = 0.5*sector
    else
       sector = 0.5*(sorted_winddirs(right_sol)-sorted_winddirs(left_sol))
       sector = my_mod(sector,360.)
    end if
    weight = sector/360.0
              
  end subroutine FoM_old_calc_weight
    !  #]
  subroutine sort_winddirs(num_sol,winddirs,which_sol,&
                           sorted_winddirs,which_sorted_sol)
    !  #[
    integer,                       intent(in)  :: num_sol
    real(r4_), dimension(num_sol), intent(in)  :: winddirs
    integer,                       intent(in)  :: which_sol
    real(r4_), dimension(num_sol), intent(out) :: sorted_winddirs
    integer,                       intent(out) :: which_sorted_sol

    ! local variables
    integer   :: i, pos_min, closest_index
    real(r4_) :: tmp_val, closest_anglediff, anglediff

    ! sort the 3 directions

    ! init
    sorted_winddirs(1:num_sol) = winddirs(1:num_sol)
    which_sorted_sol = which_sol

    ! make sure the dirs are in the range 0. to 360.
    do i=1,num_sol
       sorted_winddirs(i) = my_mod(sorted_winddirs(i), 360._r4_)
       which_sorted_sol = which_sol
    end do

    if (num_sol .le. 1) return

    do i=1,num_sol-1
       ! find the smallest angle in the remaining solutions
       ! and swap that one with the current value
       pos_min=minloc(sorted_winddirs(i:num_sol),1)+i-1
       if (pos_min .ne. i) then
          ! swap          
          !print *,"swapping ",i," and ",pos_min
          tmp_val                  = sorted_winddirs(i)
          sorted_winddirs(i)       = sorted_winddirs(pos_min)
          sorted_winddirs(pos_min) = tmp_val
       end if
    end do

    ! find the index of which_sorted_sol
    closest_index = -1
    closest_anglediff = 360.
    do i=1,num_sol
       anglediff = abs(sorted_winddirs(i)-my_mod(winddirs(which_sol), 360._r4_))
       if (anglediff .lt. closest_anglediff) then
          closest_anglediff = anglediff
          closest_index = i
       end if
    end do
    which_sorted_sol = closest_index

  end subroutine sort_winddirs
    !  #]
  function get_neighbour_solution(num_sol,which_sorted_sol,left,right) result(index)
    !  #[
    integer,           intent(in) :: num_sol          ! input
    integer,           intent(in) :: which_sorted_sol ! input
    logical, intent(in), optional :: left  ! optional input
    logical, intent(in), optional :: right ! optional input
    integer                       :: index ! result

    ! local variables
    logical :: get_left_neighbour
    logical :: get_right_neighbour

    get_left_neighbour  = .false.
    get_right_neighbour = .false.
    index = -1

    if (present(left))  get_left_neighbour  = left
    if (present(right)) get_right_neighbour = right

    if (get_left_neighbour) then
       index = which_sorted_sol-1
       if (index .lt.1) index = index+num_sol ! wrap around
       return
    end if

    if (get_right_neighbour) then
       index = which_sorted_sol+1
       if (index .gt.num_sol) index = index-num_sol ! wrap around
       return
    end if

    ! this point should never be reached
    print *,"ERROR in get_neighbour_solution:"
    print *,"Either the right or left input parameter must be set to true"
    print *,"but it seems both are missing, or if present they are set to false"
    print *,"aborting program"
    stop 1

  end function get_neighbour_solution
    !  #]
  subroutine calc_background_probability(vector_diff_sq,stdev_bg,probability) 
    !  #[
    real(r4_), intent(in)  :: vector_diff_sq ! input
    real(r4_), intent(in)  :: stdev_bg       ! input
    real(r4_), intent(out) :: probability    ! result

    ! local variables
    real(r4_) :: factor1, factor2

    factor1 =  1./(2.*pi*stdev_bg)
    factor2 = -1./(2.*stdev_bg)
    probability = factor1*exp(factor2*vector_diff_sq)

  end subroutine calc_background_probability
    !  #]
  subroutine generic_MLE_to_probability(MLE,deg_freedom,probability,error_flag)
    !  #[
    real(r4_), intent(in)  :: MLE
    integer,   intent(in)  :: deg_freedom
    real(r4_), intent(out) :: probability
    integer,   intent(out) :: error_flag

    ! Calculate the generic Chi-square probability density function
    ! for a given number of degrees of freedom
    
    ! Generic formula 
    ! (see for example wikipedia 
    ! http://en.wikipedia.org/wiki/Chi-square_distribution) 
    ! for k degrees of freedom is:
    ! 
    !           (1/2)^(k/2)
    ! p(k,x) =  ----------- * x^(k/2 - 1) * exp(-x/2)
    !           Gamma(k/2)
    !
    ! Some Gamma function values are:
    ! see for example wikipedia
    ! http://en.wikipedia.org/wiki/Gamma_function):
    ! 
    ! Gamma(1/2) = sqrt(pi) so approx.  1.77245385091
    ! Gamma(1)   = 1
    ! Gamma(3/2) = sqrt(pi)/2 so approx. 0.886226925453
    ! Gamma(2)   = 1
    ! Gamma(5/2) = 3*sqrt(pi)/4 so approx. 1.32934038818
    ! Gamma(3)   = 2! = 2
    ! Gamma(7/2) = 15*sqrt(pi)/8 so approx. 3.32335097045
    ! Gamma(4)   = 3! = 6
    ! Gamma(9/2) = 105*sqrt(pi)/16 so approx. 11.6317283966
    ! Gamma(5)   = 4! = 24
    ! Gamma(11/2) =945*sqrt(pi)/32 so approx. 52.3427777846
    ! Gamma(6)   = 5! = 120
    !
    ! so we get for the first few values of k:
    !
    ! p( 1,x) = 0.5^0.5  * x^(-1/2)* exp(-x/2)/Gamma( 1/2)
    ! p( 2,x) = 0.5^1    *           exp(-x/2)/Gamma( 2/2) ! qscat case
    ! p( 3,x) = 0.5^1.5  * x^( 1/2)* exp(-x/2)/Gamma( 3/2) ! ascat case
    ! p( 4,x) = 0.5^2    * x^( 2/2)* exp(-x/2)/Gamma( 4/2)
    ! p( 5,x) = 0.5^2.5  * x^( 3/2)* exp(-x/2)/Gamma( 5/2)
    ! p( 6,x) = 0.5^3    * x^( 4/2)* exp(-x/2)/Gamma( 6/2)
    ! p( 7,x) = 0.5^3.5  * x^( 5/2)* exp(-x/2)/Gamma( 7/2)
    ! p( 8,x) = 0.5^4    * x^( 6/2)* exp(-x/2)/Gamma( 8/2)
    ! p( 9,x) = 0.5^4.5  * x^( 7/2)* exp(-x/2)/Gamma( 9/2)
    ! p(10,x) = 0.5^5    * x^( 8/2)* exp(-x/2)/Gamma(10/2)
    ! p(11,x) = 0.5^5.5  * x^( 9/2)* exp(-x/2)/Gamma(11/2)
    ! p(12,x) = 0.5^6    * x^(10/2)* exp(-x/2)/Gamma(12/2)

    ! local variables
    real(r4_) :: exp_x
    real(r4_) :: x,k

    integer, save :: num_warnings_issued = 0

    ! local parameters
    integer, parameter :: max_num_warnings = 10
    real(r4_), parameter :: eps = 0.1

    ! above this MLE the probability drops below 1.e-30
    !real(r4_), parameter :: max_MLE_0 = ???
    real(r4_), parameter :: max_MLE_1 = 132.
    real(r4_), parameter :: max_MLE_2 = 137.
    real(r4_), parameter :: max_MLE_3 = 142.
    real(r4_), parameter :: max_MLE_4 = 146.
    real(r4_), parameter :: max_MLE_5 = 150.
    real(r4_), parameter :: max_MLE_6 = 153.
    real(r4_), parameter :: max_MLE_7 = 157.
    real(r4_), parameter :: max_MLE_8 = 160.
    real(r4_), parameter :: max_MLE_9 = 162.
    real(r4_), parameter :: max_MLE_10 = 166.
    real(r4_), parameter :: max_MLE_11 = 169.
    real(r4_), parameter :: max_MLE_12 = 172.
    ! This piece of temporary code (placed in calc_FoM_and_FoM_amb)
    ! was used to determine these max MLE values for which it is still
    ! usefull to calculate a probability
    !
    !real(r4_) :: mle,p
    !do i=1,200
    !   mle = 1.0*i
    !   deg_freedom = 12
    !   call generic_MLE_to_probability(mle,deg_freedom,p,error_flag)
    !   print *,"MLE=",mle," p=",p
    !end do
    !stop 1

    error_flag = no_error
    probability = missing_indicator_real_r4

    ! don't calculate probability when MLE is too high
    ! just return with zero in stead.
    ! This trick saves about 3 out of 8 seconds running time
    ! when this routines is called 4.004.001 times, to convert the full
    ! high res. MLE map to probability.
    select case (deg_freedom)
    !case(0 ) ; if (MLE .gt. max_MLE_0) probability = 0.
    case(1 ) ; if (MLE .gt. max_MLE_1) probability = 0.
    case(2 ) ; if (MLE .gt. max_MLE_2) probability = 0.
    case(3 ) ; if (MLE .gt. max_MLE_3) probability = 0.
    case(4 ) ; if (MLE .gt. max_MLE_4) probability = 0.
    case(5 ) ; if (MLE .gt. max_MLE_5) probability = 0.
    case(6 ) ; if (MLE .gt. max_MLE_6) probability = 0.
    case(7 ) ; if (MLE .gt. max_MLE_7) probability = 0.
    case(8 ) ; if (MLE .gt. max_MLE_8) probability = 0.
    case(9 ) ; if (MLE .gt. max_MLE_9) probability = 0.
    case(10) ; if (MLE .gt. max_MLE_10) probability = 0.
    case(11) ; if (MLE .gt. max_MLE_11) probability = 0.
    case(12) ; if (MLE .gt. max_MLE_12) probability = 0.
    end select
    if (probability .le. eps) return
    ! no rule yet for deg_freedom>0; if needed that could be added later
    ! for now, always do the full calculation for that case.

    ! don't really know what to do in case of only 2 views
    ! (i.e. deg_freedom will be zero in that case!)
    ! SO JUST RETURN A PROBABILITY OF 1. FOR THESE CASES
    if (deg_freedom .le. 0) then
       probability = 1.
       return
    end if

    exp_x = exp(-MLE/2)
    select case (deg_freedom)
    case(1)
       ! p( 1,x) = 0.5^0.5  * x^(-1/2)* exp(-x/2)/Gamma( 1/2) ! ascat case
       ! Gamma(1/2) = sqrt(pi) so approx.  1.77245385091
       probability = (MLE**(-0.5)) * exp_x * (1./sqrt(two_pi))
    case(2)
       ! p( 2,x) = 0.5^1    *           exp(-x/2)/Gamma( 2/2) ! qscat case
       ! Gamma(1)   = 1
       probability = exp_x * 1./(2*1)
    case(3)
       ! p( 3,x) = 0.5^1.5  * x^( 1/2)* exp(-x/2)/Gamma( 3/2) ! maybe needed 
       ! Gamma(3/2) = sqrt(pi)/2 so approx. 0.886226925453    ! for rfscat
       probability = (MLE**0.5) * exp_x * sqrt(1./(two_pi))
    case(4)
       ! p( 4,x) = 0.5^2    * x^( 2/2)* exp(-x/2)/Gamma( 4/2)
       ! Gamma(2)   = 1
       probability = MLE * exp_x * 1./(4*1)
    case(5)
       ! p( 5,x) = 0.5^2.5  * x^( 3/2)* exp(-x/2)/Gamma( 5/2)
       ! Gamma(5/2) = 3*sqrt(pi)/4 so approx. 1.32934038818
       probability = (MLE**1.5) * exp_x * 1./(1*3*sqrt(two_pi))
    case(6)
       ! p( 6,x) = 0.5^3    * x^( 4/2)* exp(-x/2)/Gamma( 6/2)
       ! Gamma(3)   = 2! = 2
       probability = (MLE**2) * exp_x * 1./(8*2)
    case(7)
       ! p( 7,x) = 0.5^3.5  * x^( 5/2)* exp(-x/2)/Gamma( 7/2)
       ! Gamma(7/2) = 15*sqrt(pi)/8 so approx. 3.32335097045
       probability = (MLE**2.5) * exp_x * 1./(1*3*5*sqrt(two_pi))
    case(8)
       ! p( 8,x) = 0.5^4    * x^( 6/2)* exp(-x/2)/Gamma( 8/2)
       ! Gamma(4)   = 3! = 6
       probability = (MLE**3) * exp_x * 1./(16*6)
    case(9)
       ! p( 9,x) = 0.5^4.5  * x^( 7/2)* exp(-x/2)/Gamma( 9/2)
       ! Gamma(9/2) = 105*sqrt(pi)/16 so approx. 11.6317283966
       probability = (MLE**3.5) * exp_x * 1./(1*3*5*7*sqrt(two_pi))
    case(10)
       ! p(10,x) = 0.5^5    * x^( 8/2)* exp(-x/2)/Gamma(10/2)
       ! Gamma(5)   = 4! = 24
       probability = (MLE**4) * exp_x * 1./(32*24)
    case(11)
       ! p(11,x) = 0.5^5.5  * x^( 9/2)* exp(-x/2)/Gamma(11/2)
       ! Gamma(11/2) =945*sqrt(pi)/32 so approx. 52.3427777846
       probability = (MLE**4.5) * exp_x * 1./(1*3*5*7*9*sqrt(two_pi))
    case(12)
       ! p(12,x) = 0.5^6    * x^(10/2)* exp(-x/2)/Gamma(12/2)
       ! Gamma(6)   = 5! = 120
       probability = (MLE**5) * exp_x * 1./(64*120)
    case default

       num_warnings_issued = num_warnings_issued + 1
       if (num_warnings_issued .le. max_num_warnings) then
          print *,"WARNING in generic_MLE_to_probability:"
          print *,"deg_freedom value is too high, switching to generic formula."
          print *,"This may be slow !"
       end if
       if (num_warnings_issued .eq. max_num_warnings) then
          print *,"==>Number of warnings passed maximum, "
          print *,"==>suppressing any following warnings of this type"
       end if

       ! compare to the generic case
       k = deg_freedom
       x = MLE
       probability = ((0.5)**(k/2)/fm_Gamma(k/2))*&
                     (x**((k/2)-1))*&
                     exp(-x/2)
    end select

    ! TODO: add checks for infinity, or too large probability

    ! this line was uncommented by Maria.
    ! TODO: reconsider this test
    !if (probability .gt. 1.) then
    !   print *,"WARNING: probability too large: ",probability
    !   print *,"reducing it to 1.0"
    !   probability = 1.
    !end if

    if (MLE .lt. 12.) then
       if (probability .lt. 1.e-25) then
          print *,"WARNING: probability too small: ",probability,&
               " MLE was: ",MLE
       end if
    end if

  end subroutine generic_MLE_to_probability
    !  #]
  subroutine do_2D_integral(data,num_u_steps,num_v_steps,u_step,v_step,&
                            integral)
    !  #[
    real(r4_), dimension(num_u_steps,num_v_steps), intent(in) :: data
    integer,   intent(in)  :: num_u_steps
    integer,   intent(in)  :: num_v_steps
    real(r4_), intent(in)  :: u_step
    real(r4_), intent(in)  :: v_step
    real(r4_), intent(out) :: integral ! result

    ! local variables
    integer   :: i,j
    real(r4_) :: avg

    ! do a very simple 2D integral.
    ! just take the average value in a box, multiply with both the
    ! box dimensions, and summarise over all boxes in the grid.
    integral = 0.
    do i=1,num_u_steps-1
       do j=1,num_v_steps-1
          avg = (data(i  ,j) + data(i  ,j+1) + &
                 data(i+1,j) + data(i+1,j+1)   )/4.
          integral = integral + avg*u_step*v_step
       end do
    end do

  end subroutine do_2D_integral
    !  #]
  subroutine do_2D_integral_restr_domain(name, store, integral)
    !  #[
    ! integrated the variable passed in "name" over the small 
    ! integration domain defined in the store data structure. 
    !
    character(len=*),                 intent(in)  :: name     ! input
    type(storage_type), dimension(:), intent(in)  :: store    ! input
    real(r4_),                        intent(out) :: integral ! result

    ! local variables
    integer   :: i,j
    logical   :: case1, case2, case3
    real(r4_) :: dir_step, speed_step, val, windspeed

    ! set some logicals to indicate what integration to do.
    ! The idea here is that comparing logicals inside the inner loop is
    ! probably much more efficient then comparing strings.
    case1=.false.
    case2=.false.
    case3=.false.
    select case(name)
    case("probability_obs*probability_bg")
       case1 = .true.
    case("probability_obs*probability_bg*vector_diff_sq")
       case2 = .true.
    case("probability_obs*(p_backmax-probability_bg)")
       case3 = .true.
    case default
       print *,"ERROR in do_2D_integral_restr_domain: unknown integration name"
       stop 1
    end select

    dir_step   = store(1)%dir_step*deg2rad ! must be in radians!
    speed_step = store(1)%speed_step_fine
    integral   = 0._r8_
    dir_loop: do i=1,size(store)
       ! discard directions for which all data is (close to) zero
       if (.not. associated(store(i)%u)) cycle dir_loop

       speed_loop: do j=1,size(store(i)%u)
          windspeed = store(i)%windspeed1 + speed_step*(j-1)

          ! calculate probability_obs*probability_bg
          if (case1) val = store(i)%probability_obs(j)*&
                           store(i)%probability_bg(j)
          ! calculate probability_obs*probability_bg*vector_diff_sq
          if (case2) val = store(i)%probability_obs(j)*&
                           store(i)%probability_bg(j)*&
                           store(i)%vector_diff_sq(j)
          ! calculate probability_obs*(p_backmax-probability_bg)
          if (case3) val = store(i)%probability_obs(j)*&
                           (p_backmax-store(i)%probability_bg(j))

          ! do the actual integration
          integral = integral + windspeed*speed_step*dir_step*val
       end do speed_loop
    end do dir_loop

  end subroutine do_2D_integral_restr_domain
    !  #]
  function fm_Gamma(z) result(g)
    !  #[
    ! renamed from Gamma to fm_Gamma to prevent shadowing
    ! the fortran intrinsic function with the same name
    ! JK, 27-Mar-2012

    real(r4_), intent(in) :: z ! input
    real(r4_)             :: g ! result

    ! local parameters and variables
    real(r4_), parameter :: eps = 0.001
    integer :: n

    ! check the assumption that z is a multiple of 0.5   
    if (abs(2*z-nint(2*z)) .gt. eps) then
       print *,"Error in function: fm_Gamma"
       print *,"This function assumes z is a multiple of 0.5 but the"
       print *,"current value seems to be: ",z
       stop 1
    end if

    if (abs(z-nint(z)) .lt. eps) then
       ! ok, n is an integer
       if (z .gt. 0.5) then
          g = factorial(nint(z)-1)
          return 
       else
          print *,"Error in function: fm_Gamma"
          print *,"function is not implemented for z<=0"
          print *,"current value seems to be: ",z
          stop 1
       end if
    else
       ! ok, z is half valued, so n is odd
       n=nint(2*z)
       if (abs(n-1) .lt. eps) then
          ! n=1, so z=0.5
          g = sqrt(pi)
          return 
       else
          g = (1.*double_factorial(n-2)/(2**(nint(0.5*(n-1)))))*sqrt(pi)
          return 
       end if
    end if
    
  end function fm_Gamma
    !  #]
  recursive function factorial(n) result(f)
    !  #[
    integer, intent(in) :: n ! input
    integer             :: f ! result

    f=0
    if (n .lt. 1) then
       f=1
    else
       f=n*factorial(n-1)
    end if

  end function factorial
    !  #]
  recursive function double_factorial(n) result(f)
    !  #[
    integer, intent(in) :: n ! input
    integer             :: f ! result

    f=0
    if (n .lt. 2) then
       f=1
    else
       f= n*double_factorial(n-2)
    end if

  end function double_factorial
    !  #]
  !--------------------------
  ! some plot routines
  !--------------------------
  subroutine plot_2d(u,v,data,plottitle,plotfilename,error_flag,&
                     scale_to_edgeval_and_min,scale_to_edgeval_and_max,&
                     num_contours)
    !  #[

    real(r4_), dimension(:,:), intent(in)  :: u            ! input
    real(r4_), dimension(:,:), intent(in)  :: v            ! input
    real(r4_), dimension(:,:), intent(in)  :: data         ! input
    character(len=*),          intent(in)  :: plottitle    ! input
    character(len=*),          intent(in)  :: plotfilename ! input
    integer,                   intent(out) :: error_flag   ! output
    ! optional inputs
    logical, optional,         intent(in)  :: scale_to_edgeval_and_min
    logical, optional,         intent(in)  :: scale_to_edgeval_and_max
    integer, optional,         intent(in)  :: num_contours

    ! local variables
    type(gp_plot_type) :: gp

    integer, parameter :: num_contours_default = 20
    real(r8_), dimension(:), pointer :: c
    real(r8_) :: cmin,cmax,crange
    integer :: i,AllocStat
    logical :: do_scale_to_edgeval_and_min
    logical :: do_scale_to_edgeval_and_max
    integer :: num_contours_to_use

    do_scale_to_edgeval_and_min = .false.
    do_scale_to_edgeval_and_max = .false.
    num_contours_to_use = num_contours_default

    if (present(scale_to_edgeval_and_min)) &
         do_scale_to_edgeval_and_min=scale_to_edgeval_and_min
    if (present(scale_to_edgeval_and_max)) &
         do_scale_to_edgeval_and_max=scale_to_edgeval_and_max
    if (present(num_contours)) num_contours_to_use = num_contours

    nullify(c)
    allocate(c(num_contours_to_use),stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"Allocation error in plot_2d"
       error_flag = error_allocate
       return
    end if

    call gp_init_plot(gp,error_flag,output_type=output_type_ps)
    !call gp_init_plot(gp,error_flag,output_type=output_type_eps)
    !call gp_init_plot(gp,error_flag,output_type=output_type_png)
    if (error_flag .ne. no_error) return

    print *,"min/maxval(data) = ",minval(data),maxval(data)
    print *,"edge min/maxval(data(1,:)) =",minval(data(1,:)),maxval(data(1,:))

    cmin = minval(data)*1.1
    cmax = maxval(data)*0.9

    if (do_scale_to_edgeval_and_min) &
         cmax = maxval(data(1,:))*0.9

    if (do_scale_to_edgeval_and_max) &
         cmin = minval(data(1,:))*1.1

    cmin = sqrt(cmin)
    cmax = sqrt(cmax)
    crange=cmax-cmin

    do i=1,num_contours_to_use
       c(i) = cmin+crange*(i-1)/(num_contours_to_use-1)
       c(i) = c(i)**2
    end do
    !print *,"contours: ",c

    call gp_set_plot_property(gp,error_flag,&
!              xrange=(/-25._r8_,25._r8_/),&
!              zrange=(/0._r8_,4500._r8_/),& ! this overrides the contours!
              xlabel="u-component [m/s]",&
              ylabel="u-component [m/s]",&
              title=plottitle,&
!              Nr_Contour_Levels=24,&
              ContourPlot=.true.,&
              Contour_Levels=c)
!              Contour_Levels=real((/1.,2.,3.,4.,5.,6.,7.,10.,&
!                                    12.,14.,16.,18.,20./)*1000.,r8_) )


    if (error_flag .ne. no_error) return

    call gp_add_3d_dataset(gp,&
                           real(u(:,1),r8_),&
                           real(v(1,:),r8_),&
                           real(data,r8_),&
                           error_flag,&
                           linecolor=gp_color_black,&
                           linestyle=gp_line_full)
    if (error_flag .ne. no_error) return

    call gp_add_hor_line(gp,7.5_r8_,error_flag,&
                         color=gp_color_green,linestyle=gp_line_dotted)
    if (error_flag .ne. no_error) return

    call gp_add_ver_line(gp,7.5_r8_,error_flag,&
                         color=gp_color_green,linestyle=gp_line_dotted)
    if (error_flag .ne. no_error) return


    call gp_create_plot(gp,plotfilename,error_flag)
    if (error_flag .ne. no_error) return

    call gp_close_plot(gp)

    deallocate(c)
    
  end subroutine plot_2d

    !  #]
  subroutine plot_integration_domain(winddir,store,speed_step_fine,error_flag)
    !  #[
    real(r4_),          dimension(:), intent(in)  :: winddir    ! input
    type(storage_type), dimension(:), intent(in)  :: store      ! input
    real(r4_), intent(in)  :: speed_step_fine ! input
    integer,   intent(out) :: error_flag      ! output

    ! local variables
    type(gp_plot_type) :: gp
    character(len=*), parameter :: plotfilename = "integration_domain"
    real(r4_), dimension(:), pointer :: u,v
    integer   :: num_dirs, num_sp_steps, i, j, total_num_points
    integer   :: running_count

    call gp_init_plot(gp,error_flag,output_type=output_type_ps)
    !if (error_flag .ne. no_error) return

    call gp_set_plot_property(gp,error_flag,&
              xrange=(/-20._r8_,20._r8_/),&
              yrange=(/-20._r8_,20._r8_/),&
              xlabel="u-component [m/s]",&
              ylabel="v-component [m/s]",&
              title="locations used for calculating the Figure-of-Merit")

    num_dirs = size(winddir)
    total_num_points = 0
    dirloop1: do i=1,num_dirs
       if (associated(store(i)%u)) then
          num_sp_steps = size(store(i)%u)
          total_num_points = total_num_points + num_sp_steps
       end if
    end do dirloop1

    print *,"total number of locations in the integration domain: ",&
         total_num_points

    nullify(u,v)
    allocate(u(total_num_points),v(total_num_points))

    ! pack all small u,v arrays from the store structure
    ! in one large array, which allows easier plotting
    running_count = 0
    dirloop2: do i=1,num_dirs
       if (associated(store(i)%u)) then
          num_sp_steps = size(store(i)%u)
          do j=1,num_sp_steps
             running_count=running_count+1
             u(running_count) = store(i)%u(j)
             v(running_count) = store(i)%v(j)
          end do
       end if
    end do dirloop2

    call gp_add_dataset(gp,real(u,r8_),real(v,r8_),&
                        error_flag,"",&
                        linecolor=gp_color_blue, &
                        datastyle=DataStyle_Points,&
                        symbol=gp_symbol_dot)
    deallocate(u,v)

    call gp_create_plot(gp,plotfilename,error_flag)

    call gp_close_plot(gp)

  end subroutine plot_integration_domain
    !  #]
  !--------------------------
end module figure_of_merit
