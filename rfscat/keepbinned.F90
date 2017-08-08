MODULE keepbinnedvars
  !--------------------------------
  !  #[ Documentation
  ! a small help module that contains some arrays 
  ! to be used in the rfscat_simulation program.
  !
  ! contents:
  ! data array definitions, and an init subroutine
  !
  ! Written 2000, 2001 by Jos de Kloe, KNMI
  ! 
  ! Modifications:
  ! 07-May-2009 J. de Kloe removed CPP definitions
  ! 07-Sep-2009 J. de Kloe added binning of u(fr) against v(fr)
  !                        and binning of u(cl) against v(cl)
  !
  !  #]
  !  #[ Modules used
  USE numerics, only: r4_
  USE binning, only: binned_2D_data_type, init_2D_bin, &
       add_to_2D_bin, write_2D_bin
  USE convert, only: speeddir_to_u, speeddir_to_v
  USE lunmanager, only: get_lun, free_lun
  USE errorhandler, only: no_error
  USE inversion, only: LUT_min_windspeed
  USE simulation_settings, only: hi_res_bins, exclude_LUT_edge_solutions
  USE geometry, only: wvc_type
  !  #]
  !  #[ Variables
  IMPLICIT NONE  ! no implicit variable typing
  public         ! make all variables public by default

  ! define grids with cells to use to count howmany simulated
  ! case fall within them. This is then written to file and
  ! used for making contour plots

  ! module variables
  integer   :: dirsteps
  integer   :: uv_steps
  integer   :: speedsteps
  !real(r4_) :: dirstep
  !real(r4_) :: speedstep
  !real(r4_) :: uv_step

  type(binned_2D_data_type) :: bin_winddir_cl
  type(binned_2D_data_type) :: bin_winddir_fr
  type(binned_2D_data_type) :: bin_windspeed_cl
  type(binned_2D_data_type) :: bin_windspeed_fr
  type(binned_2D_data_type) :: bin_wind_u_cl
  type(binned_2D_data_type) :: bin_wind_u_fr
  type(binned_2D_data_type) :: bin_wind_v_cl
  type(binned_2D_data_type) :: bin_wind_v_fr
  type(binned_2D_data_type) :: bin_wind_u_v_fr
  type(binned_2D_data_type) :: bin_wind_u_v_cl

  !  #]
  !--------------------------------
contains
  !--------------------------------
  subroutine init_bins()
    !  #[
    
    if (hi_res_bins) then
       ! winddir   ranges from [0-2.5] towards [357.5-360] degrees (144 steps)
       ! windspeed ranges from [0-0.166] towards [24.834-25] m/s (150 steps)
       ! wind_u/v   ranges from [(-25)-(-24.667)] 
       !                   towards [24.667-25] m/s (150 steps)
       dirsteps      = 144
       uv_steps      = 200 ! 150
       speedsteps    = 125
       
       ! beware: if windspeed step = 0.2 m/s in the simulations
       !         then it is not usefull to specify a finer
       !         windspeed step for this binning!!!
       
       !dirstep   = 360.0/dirsteps  ! = 2.5 [deg]
       !speedstep = 25.0/speedsteps ! = 0.2000 ! [m/s]
       !uv_step   = 50.0/uv_steps   ! = 0.3333 ! [m/s]
    else
       ! winddir ranges from [0-5] towards [355-360] degrees (72 steps)
       ! windspeed ranges from [0-0.5] towards [24.5-25] m/s (50 steps)
       ! wind_u/v ranges from [(-25)-(-24)] towards [24-25] m/s (50 steps)
       
       dirsteps   = 72
       speedsteps = 50
       uv_steps   = 50
       !dirstep   = 5.0 ! [deg]
       !speedstep = 0.5 ! [m/s]
       !uv_step   = 1.0 ! [m/s]
    end if

    call init_2D_bin(bin_winddir_cl,   dirsteps,    0.0, 360.0)
    call init_2D_bin(bin_winddir_fr,   dirsteps,    0.0, 360.0)
    call init_2D_bin(bin_windspeed_cl, speedsteps,  0.2,  25.0)
    call init_2D_bin(bin_windspeed_fr, speedsteps,  0.2,  25.0)
    call init_2D_bin(bin_wind_u_cl,    uv_steps,  -20.0,  20.0)
    call init_2D_bin(bin_wind_u_fr,    uv_steps,  -20.0,  20.0)
    call init_2D_bin(bin_wind_v_cl,    uv_steps,  -20.0,  20.0)
    call init_2D_bin(bin_wind_v_fr,    uv_steps,  -20.0,  20.0)
    call init_2D_bin(bin_wind_u_v_fr,  uv_steps,  -20.0,  20.0)
    call init_2D_bin(bin_wind_u_v_cl,  uv_steps,  -20.0,  20.0)

  end subroutine init_bins
    !  #]
  subroutine add_wvc_to_bins(wvc)
    !  #[
    TYPE(wvc_type), intent(in) :: wvc ! input  

    ! local variable
    integer   :: nr_of_edge_solutions, sol, cl, fr
    real(r4_) :: found_u_cl, found_v_cl
    real(r4_) :: found_u_fr, found_v_fr

    if (exclude_LUT_edge_solutions) then
       nr_of_edge_solutions = 0
       DO sol = 1,wvc%inv_output%nr_of_windsolutions
          IF ( wvc%inv_output%foundwindspeed(sol) .le. &
               1.01*LUT_min_windspeed) THEN
             nr_of_edge_solutions = nr_of_edge_solutions + 1
          END IF
       END DO
       
       ! if we are on the LUT edge, skip this result
       IF (nr_of_edge_solutions .gt. 0) return
    end if

    cl = wvc%index_closest
    fr = 1

    ! add winddir
    call add_to_2D_bin(bin_winddir_cl, &
                       wvc%inp_dir_orig, wvc%inv_output%foundwinddir(cl))
    call add_to_2D_bin(bin_winddir_fr, &
                       wvc%inp_dir_orig, wvc%inv_output%foundwinddir(1))

    ! add windspeed
    call add_to_2D_bin(bin_windspeed_cl, &
                       wvc%inp_speed_orig, wvc%inv_output%foundwindspeed(cl))
    call add_to_2D_bin(bin_windspeed_fr, &
                       wvc%inp_speed_orig, wvc%inv_output%foundwindspeed(1))

    ! calculate u,v components
    found_u_cl = speeddir_to_u(wvc%inv_output%foundwindspeed(cl),&
                               wvc%inv_output%foundwinddir(cl))
    found_u_fr = speeddir_to_u(wvc%inv_output%foundwindspeed(fr),&
                               wvc%inv_output%foundwinddir(fr))
    found_v_cl = speeddir_to_v(wvc%inv_output%foundwindspeed(cl),&
                               wvc%inv_output%foundwinddir(cl))
    found_v_fr = speeddir_to_v(wvc%inv_output%foundwindspeed(fr),&
                               wvc%inv_output%foundwinddir(fr))

    ! add u component
    call add_to_2D_bin(bin_wind_u_cl, wvc%inp_u_orig, found_u_cl)
    call add_to_2D_bin(bin_wind_u_fr, wvc%inp_u_orig, found_u_fr)

    ! add v component
    call add_to_2D_bin(bin_wind_v_cl, wvc%inp_v_orig, found_v_cl)
    call add_to_2D_bin(bin_wind_v_fr, wvc%inp_v_orig, found_v_fr)

    ! add u,v (fr/cl) components
    call add_to_2D_bin(bin_wind_u_v_fr, found_u_fr, found_v_fr)
    call add_to_2D_bin(bin_wind_u_v_cl, found_u_cl, found_v_cl)

  end subroutine add_wvc_to_bins
    !  #]
  subroutine write_bins_to_file(filename,error_flag)
    !  #[ 
    character(len=256), intent(in)  :: filename   ! input
    integer,            intent(out) :: error_flag ! output

    ! local variables
    integer :: fileunit
    logical :: silent
    
    error_flag = no_error
    !print *,'Writing data grids for contourplots to file ....'
    
    !print *,'opening file: ',trim(filename),' for writing binned data'
    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='new',action='write',&
         form="FORMATTED",ERR=99)
    
    silent = .true.
    write(fileunit,*) "bin_winddir_cl"
    call write_2D_bin(bin_winddir_cl,  fileunit,silent)
    write(fileunit,*) "bin_winddir_fr"
    call write_2D_bin(bin_winddir_fr,  fileunit,silent)
    write(fileunit,*) "bin_windspeed_cl"
    call write_2D_bin(bin_windspeed_cl,fileunit,silent)
    write(fileunit,*) "bin_windspeed_fr"
    call write_2D_bin(bin_windspeed_fr,fileunit,silent)
    write(fileunit,*) "bin_wind_u_cl"
    call write_2D_bin(bin_wind_u_cl,   fileunit,silent)
    write(fileunit,*) "bin_wind_u_fr"
    call write_2D_bin(bin_wind_u_fr,   fileunit,silent)
    write(fileunit,*) "bin_wind_v_cl"
    call write_2D_bin(bin_wind_v_cl,   fileunit,silent)
    write(fileunit,*) "bin_wind_v_fr"
    call write_2D_bin(bin_wind_v_fr,   fileunit,silent)
    write(fileunit,*) "bin_wind_u_v_fr"
    call write_2D_bin(bin_wind_u_v_fr, fileunit,silent)
    write(fileunit,*) "bin_wind_u_v_cl"
    call write_2D_bin(bin_wind_u_v_cl, fileunit,silent)

    close(unit=fileunit)
    call free_lun(fileunit)

    return

    !*** display error and stop if no file could be made
99  print *,'ERROR while creating the output file:  ',trim(filename)
    print *,'there is probably already a file with this name !!'
    print *,'*** ... quitting ... ***'
    stop 1
    
  end subroutine write_bins_to_file
    !  #]
  !--------------------------------
END MODULE keepbinnedvars

