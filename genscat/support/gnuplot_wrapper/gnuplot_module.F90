module gnuplot_module
  !  #[ documentation

  !
  ! a module to provide a wrapper around gnuplot, allowing to do
  ! direct plotting from your fortran90 code.
  !
  ! Written by: J. de kloe, KNMI 
  !
  ! modifications:
  !   07-Mar-2008 added to genscat (taken from aeolus ocean albedo project)
  !   11-Apr-2008 some small fixes
  !   13-Oct-2008 added some code to automatically use 
  !               /usr/local/free/bin/gnuplot when it has a newer version
  !               then the default gnuplot command
  !   13-Oct-2008 added a switch to choose between the eps and ps output format
  !   29-Oct-2008 added handling of labels
  !   23-Jan-2009 added contour and shaded contour plots
  !   26-Jan-2009 workarounds to make it compatible with the pgf90 compiler
  !   27-Jan-2009 bugfix in gp_fill_chararray, make more portable by 
  !               using bs in stead of "\\"
  !   17-Mar-2009 rearranged some pointers to prevent warnings with g95 v0.91
  !               at ESA's testing machine.
  !   29-Apr-2009 changed to use getenv_aeolus in stead of getenv
  !               (which is an extension of the Fortran90 standard)
  !   13-May-2009 imported recent ADM-Aeolus developments into genscat
  !               and renamed getenv_aeolus to getenv_genscat
  !
  ! WARNING: on the KNMI bgaxhpc mainframe machine an ancient gnuplot v3.7
  !          is available. This module has not yet been tested with this
  !          version, so no idea wheter it works or not.....
  !
  !  #]
  !  #[ modules used

  use tempfile_handling, only: get_temp_filename, free_temp_filename
  use errorhandler, only: no_error, error_allocate, error_opening_file, &
                          error_writing_file, error_reading_file, &
                          error_programming, error_not_yet_implemented
  use numerics, only: r8_, missing_indicator_real, missing_real, &
                      missing_indicator_real_r8
  use lunmanager, only: get_lun, free_lun, fileunit_stdout
  use stringtools, only: chararray2string, string2chararray, to_lowercase
  use c_support, only: get_filesize, system_cmd
  use compiler_features, only: getenv_genscat, newline, bs ! bs=backslash

  !  #]
  !  #[ variables, types and parameters
  implicit none
  integer,          parameter :: gp_maxstrlen = 256
  character(len=1), parameter :: quote = '"'

  ! external executables used by this module are:

  ! possible locations for the gnuplot command:
  character(len=*), parameter :: gnuplot_command1 = "gnuplot"
  character(len=*), parameter :: gnuplot_command2 = "/usr/local/free/bin/gnuplot"

  ! possible display commands are:
  character(len=*), parameter :: display_command_png = "display"     ! suitable for png
  character(len=*), parameter :: display_command_ps  = "gv"          ! suitable for ps
  character(len=*), parameter :: display_command_eps = "gv -scale 3" ! suitable for eps
  ! WARNING: on some systems the gv commands needs to be called
  !          as "gv -scale=3" in stead of "gv -scale 3" !!!!
  ! TODO: add some code to allow the module to detect this at runtime
  !      i.e. by issuing a command line "gv -help | grep scale"
  !      and examining the output.
  !      On my suse werkstation (KNMI) I get this output:
  !              [-monochrome] [-grayscale] [-color]
  !              [-scalebase <n>]
  !              [-scale <n>]

  ! some gnuplot properties
  ! these are filled by the gp_init_plot routine
  character(len=15), save :: gnuplot_patch_level = "[undefined]"
  character(len=15), save :: gnuplot_version     = "[undefined]"

  ! usefull to allow printing debug messages
  logical, save :: debug = .false.

  ! a linked list of gnuplot command line strings
  type gp_command_line_type
     character(len=1), dimension(:), pointer :: command
     type(gp_command_line_type),     pointer :: next_command_line
  end type gp_command_line_type

  ! a linked list of datasets for this plot
  type gp_dataset_type
     character(len=gp_maxstrlen)       :: name
     character(len=gp_maxstrlen)       :: datafilename
     logical                           :: remove_datafile_after_use
     real(r8_), dimension(:),  pointer :: x
     real(r8_), dimension(:),  pointer :: y
     type(gp_dataset_type),    pointer :: next_dataset
     integer                           :: linestyle
     integer                           :: linecolor
     integer                           :: symbol
     integer                           :: datastyle
  end type gp_dataset_type

  ! a linked list of 3d datasets to create contour/surface plots
  type gp_3d_dataset_type
     character(len=gp_maxstrlen)        :: name
     character(len=gp_maxstrlen)        :: datafilename
     character(len=gp_maxstrlen)        :: contourdatafilename
     logical                            :: remove_datafile_after_use
     logical                            :: remove_datafile2_after_use
     real(r8_), dimension(:),   pointer :: x
     real(r8_), dimension(:),   pointer :: y
     real(r8_), dimension(:,:), pointer :: z
     integer                            :: linestyle
     integer                            :: linecolor
     ! I dont think we will use symbols combined with contours
     ! so the datastyle will allways be "Lines" and symbol type is irrelevant
     type(gp_3d_dataset_type),  pointer :: next_3d_dataset
  end type gp_3d_dataset_type

  ! a linked list of lines/bars to be overplotted
  type gp_line_bar_type
     integer   :: type ! is it a hor./ver. line or bar?
     real(r8_) :: val1 ! x or y or x1 or y1
     real(r8_) :: val2 ! x2 or y2
     integer   :: linestyle
     integer   :: color
     type(gp_line_bar_type), pointer :: next_line_bar
  end type gp_line_bar_type

  ! possible line/bar types (internal, not to be used by users)
  integer, parameter :: gp_lb_type_hor_line        = 1
  integer, parameter :: gp_lb_type_ver_line        = 2
  integer, parameter :: gp_lb_type_colored_hor_bar = 3
  integer, parameter :: gp_lb_type_colored_ver_bar = 4

  ! a linked list of labels for this plot
  type gp_label_type
     character(len=gp_maxstrlen)  :: text
     real(r8_)                    :: xpos
     real(r8_)                    :: ypos
     real(r8_)                    :: rotate ! [deg] 0=hor, 90=vert
     integer                      :: justify
     integer                      :: textcolor
     type(gp_label_type), pointer :: next_label
  end type gp_label_type

  ! a type holding all settings, data and commands needed to create a plot
  type gp_plot_type 
     character(len=1), dimension(:), pointer :: gnuplot_command
     character(len=1), dimension(:), pointer :: display_command
     type(gp_command_line_type), pointer :: first_cmd_line     
     type(gp_dataset_type),      pointer :: first_dataset
     type(gp_3d_dataset_type),   pointer :: first_3d_dataset
     type(gp_label_type),        pointer :: first_label
     type(gp_line_bar_type),     pointer :: first_line_bar
     real(r8_) :: x_min ! I need to store these when setting the xrange
     real(r8_) :: x_max ! and/or yrange, otherwise I have no way of 
     real(r8_) :: y_min ! drawing lines over the full height or width
     real(r8_) :: y_max ! of the plot ...
     real(r8_) :: z_min ! 
     real(r8_) :: z_max ! 
     real(r8_), dimension(2) :: xrange ! requested range when autoscale if off
     real(r8_), dimension(2) :: yrange ! requested range when autoscale if off
     real(r8_), dimension(2) :: zrange ! requested range when autoscale if off
     logical   :: x_autoscale
     logical   :: y_autoscale
     logical   :: z_autoscale
     logical   :: ContourPlot
     logical   :: ShadePlot
     integer   :: Nr_Contour_Levels
     logical   :: AddContourAtOne
     real(r8_), dimension(:), pointer :: Contour_Levels
     integer   :: palette
     integer   :: Output_Type
     character(len=1), dimension(:), pointer :: title
     character(len=1), dimension(:), pointer :: xlabel
     character(len=1), dimension(:), pointer :: ylabel
     character(len=1), dimension(:), pointer :: zlabel
     real(r8_) :: AspectRatio
     integer   :: clipping
  end type gp_plot_type

  ! allowed output types (terminals in gnuplot jargon)
  integer, parameter :: output_type_ps    = 1
  integer, parameter :: output_type_eps   = 2
  integer, parameter :: output_type_png   = 3

  ! some allowed colors
  integer, parameter :: gp_color_default = -99
  integer, parameter :: gp_color_black   = 0
  integer, parameter :: gp_color_red     = 1
  integer, parameter :: gp_color_green   = 2
  integer, parameter :: gp_color_blue    = 3
  integer, parameter :: gp_color_magenta = 4
  integer, parameter :: gp_color_cyan    = 5
  integer, parameter :: gp_color_yellow  = 6

  ! some allowed palettes
  integer, parameter :: gp_palette_default           = -99
  integer, parameter :: gp_palette_color             = 1
  integer, parameter :: gp_palette_color_neg         = 2
  integer, parameter :: gp_palette_gray              = 3
  integer, parameter :: gp_palette_gray_neg          = 4
  integer, parameter :: gp_palette_white_blue        = 5
  integer, parameter :: gp_palette_blue_white        = 6
  integer, parameter :: gp_palette_green_red_violet  = 7
  integer, parameter :: gp_palette_green_blue_white  = 8
  integer, parameter :: gp_palette_black_bl_vio_y_wh = 9 
  !                     black-blue-violet-yellow-white

  ! some allowed symbols
  integer, parameter :: gp_symbol_default      = -99
  integer, parameter :: gp_symbol_none         = -1
  integer, parameter :: gp_symbol_dot          = 0
  integer, parameter :: gp_symbol_plus         = 1
  integer, parameter :: gp_symbol_cross        = 2
  integer, parameter :: gp_symbol_star         = 3
  integer, parameter :: gp_symbol_box          = 4
  integer, parameter :: gp_symbol_filledbox    = 5
  integer, parameter :: gp_symbol_circle       = 6
  integer, parameter :: gp_symbol_filledcircle = 7
  integer, parameter :: gp_symbol_triangle     = 8
  integer, parameter :: gp_symbol_filltriangle = 9

  ! some allowd line drawing styles
  integer, parameter :: gp_line_default     = -99
  integer, parameter :: gp_line_full        = 1
  integer, parameter :: gp_line_dashed      = 2
  integer, parameter :: gp_line_shortdashed = 3
  integer, parameter :: gp_line_dotted      = 4
  integer, parameter :: gp_line_dash_dot    = 5
  integer, parameter :: gp_line_dash_ddot   = 8

  ! Data styles for plotting
  integer, parameter :: DataStyle_undefined   = -99
  integer, parameter :: DataStyle_Lines       = 1
  integer, parameter :: DataStyle_LinesPoints = 2
  integer, parameter :: DataStyle_FilledCurve = 3
  integer, parameter :: DataStyle_Steps       = 4
  integer, parameter :: DataStyle_Points      = 5
  integer, parameter :: DataStyle_Default     = 99

  ! possible values for the justify field of gp_label_type
  integer, parameter :: gp_justify_default = -99
  integer, parameter :: gp_justify_left    =  1
  integer, parameter :: gp_justify_center  =  2
  integer, parameter :: gp_justify_right   =  3

  ! possible clipping settings
  integer, parameter :: clipping_noclip_one  = 1
  integer, parameter :: clipping_noclip_two  = 2
  integer, parameter :: clipping_clip_one    = 3
  integer, parameter :: clipping_clip_two    = 4
  integer, parameter :: clipping_clip_points = 5

  ! define the clipping, for details see:
  !    http://t16web.lanl.gov/Kawano/gnuplot/plot2-e.html
  ! set noclip one:  draws no line if one point is inside and one is outside 
  !                  the plot
  ! set noclip two:  draws no line if both points defining the line are 
  !                  outside the plot, but the line partly falls inside 
  !                  the plot
  ! set clip one:    draws a line if one point is inside and one is outside 
  !                  the plot
  ! set clip two:    draws a line if both points defining the line are 
  !                  outside the plot, but the line partly falls inside 
  !                  the plot
  ! set clip points: does not draw points close to the plot window
  !                  (when the symbol might fall partially outside 
  !                   the plot window)

  !  #]
  !  #[ Known bug
  ! A command will be truncated if it becomes longer than: gp_maxstrlen = 256
  ! or inside gp_create_plot if it becomes longer than 5120 characters
  ! which may happen if you overplot really a large number of plots.
  !
  ! Solution would be to allocate the strings when needed, in stead
  ! of using a fixed lenght. See the linkedlist module for an example
  ! of how this is done.

  !  #]
contains
  !------------------------------------
  subroutine gp_init_plot(gp,error_flag,&
                          alt_gnuplot_command,alt_display_command,&
                          output_format,output_type, debug_prints)
    !  #[ init all settings to empty/missing

    type(gp_plot_type), intent(out) :: gp
    integer,            intent(out) :: error_flag
    character(len=*), optional, intent(in) :: alt_gnuplot_command
    character(len=*), optional, intent(in) :: alt_display_command
!TODO: move this one to gp_set_plot_property as well
    character(len=*), optional, intent(in) :: output_format
    ! maybe: "ps" or "eps"
    integer,          optional, intent(in) :: output_type
    logical,          optional, intent(in) :: debug_prints

    ! local variables
    !character(len=gp_maxstrlen) :: command
    character(len=256)          :: env_txt
    character(len=15)           :: gnuplot_version1,gnuplot_version2
    character(len=15)           :: gnuplot_patch_level

    ! init
    error_flag = no_error

    if (present(debug_prints)) debug = debug_prints

    ! fill the display command with the default setting [ps version]
     call gp_fill_chararray(gp%display_command,display_command_ps,error_flag)
     if (error_flag .ne. no_error) return

    ! take the default name first
    !print *,"assuming we can use: "//trim(gnuplot_command1)
    call gp_fill_chararray(gp%gnuplot_command,gnuplot_command1,error_flag)
    if (error_flag .ne. no_error) return

    ! check to see if this is a recent version
    call get_gnuplot_version(gnuplot_command1, &
                             gnuplot_version1,gnuplot_patch_level,error_flag)
    if (error_flag .ne. no_error) return

    call get_gnuplot_version(gnuplot_command2, &
                             gnuplot_version2,gnuplot_patch_level,error_flag)
    if (error_flag .ne. no_error) return

    if (gnuplot_version2 .gt. gnuplot_version1) then
       !print *,"alternative gnuplot command has newer version."
       !print *,"switching to using: "//trim(gnuplot_command2)
       call gp_refill_chararray(gp%gnuplot_command,gnuplot_command2,error_flag)
       if (error_flag .ne. no_error) return
    end if

    ! is an environment setting is present, use it:
    CALL getenv_genscat("GM_GNUPLOT_COMMAND",env_txt)
    IF (trim(env_txt) .ne. "") THEN
!       print *,"replacing default gnuplot command:      "//trim(gnuplot_command)
!       print *,"by the env setting for gnuplot command: "//trim(env_txt)
       call gp_refill_chararray(gp%gnuplot_command,env_txt,error_flag)
       if (error_flag .ne. no_error) return
    endif
 
    CALL getenv_genscat("GM_DISPLAY_COMMAND",env_txt)
    IF (trim(env_txt) .ne. "") THEN
!       print *,"replacing default display command:      "//trim(display_command)
!       print *,"by the env setting for display command: "//trim(env_txt)
       call gp_refill_chararray(gp%display_command,env_txt,error_flag)
       if (error_flag .ne. no_error) return
    endif
 
    ! if a user explicitely alters the system commands to be used,
    ! then these will override the environment setting.
    if (present(alt_gnuplot_command)) then
!       print *,"replacing default gnuplot command:    "//trim(gnuplot_command)
!       print *,"by the user supplied gnuplot command: "//trim(alt_gnuplot_command)
       call gp_refill_chararray(gp%gnuplot_command,&
                                alt_gnuplot_command,error_flag)
       if (error_flag .ne. no_error) return
    end if

    if (present(alt_display_command)) then
!       print *,"replacing default display command:    "//trim(display_command)
!       print *,"by the user supplied display command: "//trim(alt_display_command)
       call gp_refill_chararray(gp%display_command,&
                                alt_display_command,error_flag)
       if (error_flag .ne. no_error) return
    end if

    nullify(gp%first_cmd_line)
    nullify(gp%first_dataset)
    nullify(gp%first_3d_dataset)
    nullify(gp%first_label)
    nullify(gp%first_line_bar)

    gp%x_min = missing_indicator_real
    gp%x_max = missing_indicator_real

    gp%y_min = missing_indicator_real
    gp%y_max = missing_indicator_real

    gp%z_min = missing_indicator_real
    gp%z_max = missing_indicator_real

    gp%xrange(1) = missing_indicator_real
    gp%xrange(2) = missing_indicator_real

    gp%yrange(1) = missing_indicator_real
    gp%yrange(2) = missing_indicator_real

    gp%zrange(1) = missing_indicator_real
    gp%zrange(2) = missing_indicator_real

    gp%x_autoscale = .true.
    gp%y_autoscale = .true.
    gp%z_autoscale = .true.

    gp%ContourPlot       = .false.
    gp%ShadePlot         = .false.
    gp%Nr_Contour_Levels = 10
    nullify(gp%Contour_Levels)
    gp%AddContourAtOne   = .false.
    gp%palette           = gp_palette_default

    ! default is to output to postscript
    gp%Output_Type = output_type_ps

    if (present(output_format)) then
       select case (trim(to_lowercase(output_format)))
       case ("ps")
          ! ps version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_ps,error_flag)
          if (error_flag .ne. no_error) return
          gp%Output_Type = output_type_ps
       case ("eps")
          ! eps version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_eps,error_flag)
          if (error_flag .ne. no_error) return
          gp%Output_Type = output_type_eps
       case ("png")
          ! ps version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_png,error_flag)
          if (error_flag .ne. no_error) return
          gp%Output_Type = output_type_png
       case default
          print *,"Illegal output format: "//trim(output_format)
          print *,"This parameter should contain either ps, eps or png"
          print *,"when present"
          stop 1
       end select
    end if

    if (present(output_type)) then
       gp%Output_Type = output_type
       select case (output_type)
       case (output_type_ps)
          ! ps version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_ps,error_flag)
          if (error_flag .ne. no_error) return
       case (output_type_eps)
          ! eps version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_eps,error_flag)
          if (error_flag .ne. no_error) return
       case (output_type_png)
          ! png version
          call gp_refill_chararray(gp%display_command,&
                                   display_command_png,error_flag)
          if (error_flag .ne. no_error) return
       case default
          print *,"Illegal output type: ",output_type
          print *,"This parameter should contain one of the integers:"
          print *,"output_type_ps   = 1"
          print *,"output_type_eps  = 2"
          print *,"output_type_png  = 3"
          print *,"preferably use the symbolic names ny importing them from"
          print *,"the gnuplot module ..."
          stop 1
       end select
    end if

    nullify(gp%title)
    nullify(gp%xlabel)
    nullify(gp%ylabel)
    nullify(gp%zlabel)

    gp%AspectRatio = missing_indicator_real
    gp%clipping    = clipping_clip_two

  end subroutine gp_init_plot

    !  #]
  subroutine gp_close_plot(gp)
    !  #[ delete all stored data for this plot
    type(gp_plot_type), intent(inout) :: gp
    
    ! local variables
    type(gp_command_line_type), pointer :: this_gp_cmd_line
    type(gp_command_line_type), pointer :: gp_cmd_line_to_delete
    type(gp_dataset_type),      pointer :: this_dataset
    type(gp_dataset_type),      pointer :: dataset_to_delete
    type(gp_3d_dataset_type),   pointer :: this_3d_dataset
    type(gp_3d_dataset_type),   pointer :: dataset_3d_to_delete
    type(gp_label_type),        pointer :: this_label
    type(gp_label_type),        pointer :: label_to_delete
    type(gp_line_bar_type),     pointer :: this_line_bar
    type(gp_line_bar_type),     pointer :: line_bar_to_delete

    if (associated(gp%gnuplot_command)) then
       deallocate(gp%gnuplot_command)
       nullify(gp%gnuplot_command)
    end if

    if (associated(gp%display_command)) then
       deallocate(gp%display_command)
       nullify(gp%display_command)
    end if

    this_gp_cmd_line => gp%first_cmd_line
    do while (associated(this_gp_cmd_line))
       gp_cmd_line_to_delete => this_gp_cmd_line
       this_gp_cmd_line => this_gp_cmd_line%next_command_line
       if (associated(gp_cmd_line_to_delete%command)) &
            deallocate(gp_cmd_line_to_delete%command)
       deallocate(gp_cmd_line_to_delete)
    end do

    nullify(gp%first_cmd_line)

    this_dataset => gp%first_dataset
    do while (associated(this_dataset))
       dataset_to_delete => this_dataset
       this_dataset => this_dataset%next_dataset

       if (associated(dataset_to_delete%x)) &
            deallocate(dataset_to_delete%x)
       if (associated(dataset_to_delete%y)) &
            deallocate(dataset_to_delete%y)
       deallocate(dataset_to_delete)
    end do

    nullify(gp%first_dataset)

    this_3d_dataset => gp%first_3d_dataset
    do while (associated(this_3d_dataset))
       dataset_3d_to_delete => this_3d_dataset
       this_3d_dataset => this_3d_dataset%next_3d_dataset

       if (associated(dataset_3d_to_delete%x)) &
            deallocate(dataset_3d_to_delete%x)
       if (associated(dataset_3d_to_delete%y)) &
            deallocate(dataset_3d_to_delete%y)
       if (associated(dataset_3d_to_delete%z)) &
            deallocate(dataset_3d_to_delete%z)
       deallocate(dataset_3d_to_delete)
    end do

    nullify(gp%first_3d_dataset)

    this_label => gp%first_label
    do while (associated(this_label))
       label_to_delete => this_label
       this_label => label_to_delete%next_label
       deallocate(label_to_delete)
    end do

    nullify(gp%first_label)

    this_line_bar => gp%first_line_bar
    do while (associated(this_line_bar))
       line_bar_to_delete => this_line_bar
       this_line_bar => line_bar_to_delete%next_line_bar
       deallocate(line_bar_to_delete)
    end do

    nullify(gp%first_line_bar)

    if (associated(gp%Contour_Levels)) then
       deallocate(gp%Contour_Levels)
       nullify(gp%Contour_Levels)
    end if

    if (associated(gp%title)) then
       deallocate(gp%title)
       nullify(gp%title)
    end if

    if (associated(gp%xlabel)) then
       deallocate(gp%xlabel)
       nullify(gp%xlabel)
    end if

    if (associated(gp%ylabel)) then
       deallocate(gp%ylabel)
       nullify(gp%ylabel)
    end if

    if (associated(gp%zlabel)) then
       deallocate(gp%zlabel)
       nullify(gp%zlabel)
    end if

  end subroutine gp_close_plot
    !  #]
  subroutine get_gnuplot_version(gnuplot_command_to_check,&
                         gnuplot_version,gnuplot_patch_level,error_flag)
    !  #[ get version number from the gnuplot command
    character(len=*),  intent(in)  :: gnuplot_command_to_check
    character(len=15), intent(out) :: gnuplot_version
    character(len=15), intent(out) :: gnuplot_patch_level
    integer,           intent(out) :: error_flag

    !  local variables
    character(len=256) :: temp_version_filename
    character(len=256) :: temp_location_filename
    character(len=256) :: tmp_gnuplot_command_to_check
    character(len=256) :: line
    character(len=15)  :: word1,word2,word3,word4
    integer            :: fileunit, ios, size
    logical            :: command_found

    ! init
    error_flag = no_error
    word1(:)=' '
    word2(:)=' '
    word3(:)=' '
    word4(:)=' '

    command_found = .true.
    tmp_gnuplot_command_to_check = gnuplot_command_to_check

    if (gnuplot_command_to_check(1:1) .ne. '/') then
       ! if the path does not begin with a slash. check the output of
       ! the "which" command, to find the location of gnuplot

       call get_temp_filename(temp_location_filename, error_flag)
       if (error_flag .ne. no_error) return

       call system_cmd("which "//trim(gnuplot_command_to_check)//" > "//&
                       trim(temp_location_filename),error_flag)
       if (error_flag .ne. no_error) return

       fileunit = get_lun()
       open(unit=fileunit,file=temp_location_filename,status="old",&
            form="FORMATTED",action="read",err=996)
       read(fileunit,"(a)",iostat=ios) line
       if (ios .ne. 0) goto 997

       command_found = .true.
       tmp_gnuplot_command_to_check = line

       if (line(1:1) .ne. '/') then
          command_found = .false.
          tmp_gnuplot_command_to_check(:) =' '
       end if

       close(unit=fileunit)
       call free_lun(fileunit)
       !print *,"location = ["//trim(tmp_gnuplot_command_to_check)//"]"

       call free_temp_filename(temp_location_filename,error_flag)
       if (error_flag .ne. no_error) return
    end if

    if (.not. command_found) then
       ! this gnuplot command seems not to exist, so return
       ! with version number 0.0 to pass this test smoothly
       gnuplot_version     = "0.0"
       gnuplot_patch_level = "0"
       goto 111
    end if

    call get_temp_filename(temp_version_filename, error_flag)
    if (error_flag .ne. no_error) return

    size = get_filesize(tmp_gnuplot_command_to_check)
    !print *,"size=",size

    if (size .le. 0) then
       ! this gnuplot command seems not a proper file, so return
       ! with version number 0.0 to pass this test smoothly
       gnuplot_version     = "0.0"
       gnuplot_patch_level = "0"
       goto 111 
    end if

    call system_cmd(trim(tmp_gnuplot_command_to_check)//" --version > "//&
                    trim(temp_version_filename),error_flag)
    if (error_flag .ne. no_error) return

    fileunit = get_lun()
    open(unit=fileunit,file=temp_version_filename,status="old",&
         form="FORMATTED",action="read",err=999)
    read(fileunit,*,iostat=ios) word1,word2,word3,word4
    if (ios .ne. 0) goto 998
    gnuplot_version = word2
    gnuplot_patch_level = word4
    close(unit=fileunit)
    call free_lun(fileunit)

    call free_temp_filename(temp_version_filename,error_flag)
    if (error_flag .ne. no_error) return

111 continue 
    !print *,"gnuplot_command_to_check=["//trim(gnuplot_command_to_check)//"]"
    !print *,"tmp_gnuplot_command_to_check=["//trim(tmp_gnuplot_command_to_check)//"]"
    !print *,"gnuplot_version=["//trim(gnuplot_version)//"]"
    !print *,"gnuplot_patch_level=["//trim(gnuplot_patch_level)//"]"

    return

996 print *,"ERROR: could not read from file: "//trim(temp_location_filename)
    error_flag = error_reading_file
    return

997 print *,"ERROR: could open file: "//trim(temp_location_filename)
    error_flag = error_opening_file
    return

998 print *,"ERROR: could not read from file: "//trim(temp_version_filename)
    ! this print is just to suppress "variable unused" warnings by the 
    ! gfortran and g95 compilers. I don't expected them to have meaningfull
    ! vaslues at this point.
    print *,"word1,word2,word3,word4 = ",word1,word2,word3,word4
    error_flag = error_reading_file
    return

999 print *,"ERROR: could open file: "//trim(temp_version_filename)
    error_flag = error_opening_file
    return

  end subroutine get_gnuplot_version
    !  #]
  subroutine gp_asciiwrite_cmd_list(gp,fileunit,error_flag)
    !  #[
    type(gp_plot_type), intent(in)  :: gp
    integer,            intent(in)  :: fileunit
    integer,            intent(out) :: error_flag

    type(gp_command_line_type), pointer :: this_gp_cmd_line

    error_flag = no_error
    this_gp_cmd_line => gp%first_cmd_line
    do while (associated(this_gp_cmd_line))
       write(fileunit,*,err=999) chararray2string(this_gp_cmd_line%command)
       this_gp_cmd_line => this_gp_cmd_line%next_command_line
    end do

    return

999 print *,"ERROR in gp_asciiwrite_cmd_list"
    error_flag = error_writing_file
    return

  end subroutine gp_asciiwrite_cmd_list
    !  #]
  subroutine gp_print_cmd_list(gp,error_flag)
    !  #[
    type(gp_plot_type), intent(in)  :: gp
    integer,            intent(out) :: error_flag

    call gp_asciiwrite_cmd_list(gp,fileunit_stdout,error_flag)

  end subroutine gp_print_cmd_list
    !  #]
  subroutine gp_add_command(gp,command,error_flag)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    character(len=*),   intent(in)    :: command
    integer,            intent(out)   :: error_flag

    ! local variables
    type(gp_command_line_type), pointer :: new_gp_cmd, this_gp_cmd
    integer :: AllocStatus
    
    error_flag = no_error

    allocate(new_gp_cmd,stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"allocation problem in gp_add_command"
       error_flag = error_allocate
       return
    end if
    
    ! copy the data
    call gp_fill_chararray(new_gp_cmd%command,command,error_flag)
    if (error_flag .ne. no_error) return

    nullify(new_gp_cmd%next_command_line)

    if (associated(gp%first_cmd_line)) then
       ! more command lines are present already
       this_gp_cmd =>gp%first_cmd_line
       find_list_end: do
          if (associated(this_gp_cmd%next_command_line)) then
             ! more elements are present
             this_gp_cmd => this_gp_cmd%next_command_line
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end

       this_gp_cmd%next_command_line => new_gp_cmd
    else
       ! this is the first command line to be added
       gp%first_cmd_line => new_gp_cmd
    end if

  end subroutine gp_add_command
    !  #]
  subroutine gp_fill_chararray(chararray,txt,error_flag)
    !  #[
    character(len=1), dimension(:), pointer     :: chararray  ! result
    character(len=*),               intent(in)  :: txt        ! input
    integer,                        intent(out) :: error_flag ! output
 
    ! local variables
    integer :: AllocStat, n

    error_flag = no_error

    n = len_trim(txt)
    allocate(chararray(n),stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_fill_chararray"
       error_flag = error_allocate
       return
    end if

    chararray = string2chararray(trim(txt))

  end subroutine gp_fill_chararray
    !  #]
  subroutine gp_refill_chararray(chararray,txt,error_flag)
    !  #[
    character(len=1), dimension(:), pointer     :: chararray  ! result
    character(len=*),               intent(in)  :: txt        ! input
    integer,                        intent(out) :: error_flag ! output
 
    error_flag = no_error

    deallocate(chararray)
    call gp_fill_chararray(chararray,txt,error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_refill_chararray
    !  #]
  function datastyle_to_str(DataStyle) result(datastyle_str)
    !  #[
    integer, intent(in) :: DataStyle
    character(len=20)  :: datastyle_str ! result

    select case (DataStyle)
    case(DataStyle_Lines)
       datastyle_str = "lines"
    case(DataStyle_LinesPoints)
       datastyle_str = "linespoints"
    case(DataStyle_FilledCurve)
       datastyle_str = "filledcurve"
    case(DataStyle_Steps)
       datastyle_str = "steps"
    case(DataStyle_Points)
       datastyle_str = "points"
    case(DataStyle_Default)
       datastyle_str(:) = ' '
    case default
       print *,"ERROR in datastyle_to_str:"
       print *,"unknown DataStyle code: ",DataStyle
       stop 1
    end select
    
  end function datastyle_to_str
    !  #]
  subroutine gp_set_plot_property(gp,error_Flag,&
                                  xrange,yrange,zrange,&
                                  autoscale,&
                                  title,xlabel,ylabel,zlabel,&
                                  AspectRatio,&
                                  ContourPlot,ShadePlot,&
                                  Nr_Contour_Levels, AddContourAtOne, &
                                  Contour_Levels,&
                                  palette)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    integer,            intent(out)   :: error_flag
    ! optional switches to control the scaling of the axes
    real(r8_), dimension(2), optional, intent(in) :: xrange
    real(r8_), dimension(2), optional, intent(in) :: yrange
    real(r8_), dimension(2), optional, intent(in) :: zrange
    logical,                 optional, intent(in) :: autoscale
    character(len=*),        optional, intent(in) :: title
    character(len=*),        optional, intent(in) :: xlabel
    character(len=*),        optional, intent(in) :: ylabel
    character(len=*),        optional, intent(in) :: zlabel
    real(r8_),               optional, intent(in) :: AspectRatio
    logical,                 optional, intent(in) :: ContourPlot
    logical,                 optional, intent(in) :: ShadePlot
    integer,                 optional, intent(in) :: Nr_Contour_Levels
    logical,                 optional, intent(in) :: AddContourAtOne
    real(r8_), dimension(:), optional, intent(in) :: Contour_Levels
    integer,                 optional, intent(in) :: palette

    ! local variables
    integer :: AllocStat

    !  #[ handle title and x/y/z labels
    if (present(title)) then
       if (.not. associated(gp%title)) then
          call gp_fill_chararray(gp%title,title,error_flag)
          if (error_flag .ne. no_error) return
       else
          print *,"ERROR in gp_set_plot_property:"
          print *,"redefining the title is not implemented,"
          print *,"please set the title only once ...."
          error_flag = error_programming
          return
       end if
    end if

    if (present(xlabel)) then
       if (.not. associated(gp%xlabel)) then
          call gp_fill_chararray(gp%xlabel,xlabel,error_flag)
          if (error_flag .ne. no_error) return
       else
          print *,"ERROR in gp_set_plot_property:"
          print *,"redefining the xlabel is not implemented,"
          print *,"please set the xlabel only once ...."
          error_flag = error_programming
          return
       end if
    end if

    if (present(ylabel)) then
       if (.not. associated(gp%ylabel)) then
          call gp_fill_chararray(gp%ylabel,ylabel,error_flag)
          if (error_flag .ne. no_error) return
       else
          print *,"ERROR in gp_set_plot_property:"
          print *,"redefining the ylabel is not implemented,"
          print *,"please set the ylabel only once ...."
          error_flag = error_programming
          return
       end if
    end if

    if (present(zlabel)) then
       if (.not. associated(gp%zlabel)) then
           call gp_fill_chararray(gp%zlabel,zlabel,error_flag)
          if (error_flag .ne. no_error) return
       else
          print *,"ERROR in gp_set_plot_property:"
          print *,"redefining the zlabel is not implemented,"
          print *,"please set the zlabel only once ...."
          error_flag = error_programming
          return
       end if
    end if
    !  #]
    !  #[ handle autoscale and x/y/z ranges
    if (present(autoscale)) then
       gp%x_autoscale = .true.
       gp%y_autoscale = .true.
       gp%z_autoscale = .true.
    end if

    if (present(xrange)) then
       gp%xrange = xrange
       gp%x_autoscale = .false.
    end if

    if (present(yrange)) then
       gp%yrange = yrange
       gp%y_autoscale = .false.
    end if

    if (present(zrange)) then
       gp%zrange = zrange
       gp%z_autoscale = .false.
    end if
    !  #]
    !  #[ handle aspect ratio and clipping
    if (present(AspectRatio)) gp%AspectRatio = AspectRatio

    ! default is set in gp_init_plot
    ! chaning this one is not yet implemented
    !gp%clipping = clipping_clip_two
    !  #]
    !  #[ handle contour and shade plot settings
    gp%ContourPlot = .false.
    if (present(ContourPlot)) gp%ContourPlot = ContourPlot

    gp%Nr_Contour_Levels = 10
    if (present(Nr_Contour_Levels)) gp%Nr_Contour_Levels = Nr_Contour_Levels

    gp%ShadePlot = .false.
    if (present(ShadePlot)) gp%ShadePlot = ShadePlot

    gp%AddContourAtOne = .false.
    if (present(AddContourAtOne)) gp%AddContourAtOne = .true.

    gp%palette = gp_palette_default
    if (present(palette)) gp%palette = palette

    if (present(Contour_Levels)) then
       if (associated(gp%Contour_Levels)) then
          print *,"ERROR: contour levels seem already defined!"
          print *,"ERROR they should be specified only once !!!"
          print *,"ERROR redefining them is not yet implemented..."
          error_flag = error_not_yet_implemented
          return
       end if
       allocate(gp%Contour_Levels(size(Contour_Levels)),stat=AllocStat)
       if (AllocStat .ne. 0) then
          print *,"ERROR: Allocation error in gp_set_plot_property."
          error_flag = error_allocate
          return
       end if
       gp%Contour_Levels(:) = Contour_Levels(:)
       gp%Nr_Contour_Levels = size(Contour_Levels)
    end if
    !  #]

    !  #[ Note on legend position 
    ! this determines the position of the legend
!    command = 'set key left top'
!    command = 'set key outside'
!    call gp_add_command(gp,command,error_flag)
!    if (error_flag .ne. no_error) return
    !  #]
    !  #[ Notes on line styles:
    ! The following command sequnce:
    !   set terminal postscript color
    !   set output 'test.ps'
    !   test
    ! generates a test file, with all default definitions
    ! for line style/color/points
    !
    ! When plotting, "lt 1" will refer to the default line style with index 1
    ! And "ls 1" will refer to the user defined line style with index 1

    ! define custom linestyle 1
    ! -based on the default line type 2
    ! -with a linewidt twice the default
    ! -with a point type from default line style 3
    ! -with point size twice the default size
    ! See p.94 of gnuplot.pdf
    !command = 'set style line 1 lt 2 lw 2 pt 3 ps 2.0'
    !call gp_add_command(gp,command,error_flag)
    !if (error_flag .ne. no_error) return

    ! some defaults for the postscript color device:
    ! index color pointtype
    ! 0     black   dot
    ! 1     red     plus
    ! 2     green   cross
    ! 3     blue    star
    ! 4     purple  box
    ! 5     cyan    filledbox
    ! 6     yellow  circle
    ! etc.
    !  #]

  end subroutine gp_set_plot_property
    !  #]
  subroutine gp_add_line_bar(gp,type,val1,val2,linestyle,color,error_flag)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    integer,            intent(in)    :: type
    real(r8_),          intent(in)    :: val1
    real(r8_),          intent(in)    :: val2
    integer,            intent(in)    :: linestyle
    integer,            intent(in)    :: color
    integer,            intent(out)   :: error_flag

    ! local variables
    type(gp_line_bar_type), pointer :: new_gp_line_bar
    type(gp_line_bar_type), pointer :: this_gp_line_bar
    integer   :: AllocStat

    error_flag = no_error
    allocate(new_gp_line_bar,stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_line_bar"
       error_flag = error_allocate
       return
    end if

    ! copy the data
    new_gp_line_bar%type      = type
    new_gp_line_bar%val1      = val1
    new_gp_line_bar%val2      = val2
    new_gp_line_bar%linestyle = linestyle
    new_gp_line_bar%color     = color

    nullify(new_gp_line_bar%next_line_bar)

    if (associated(gp%first_line_bar)) then
       ! more line_bars are present already
       this_gp_line_bar => gp%first_line_bar
       find_list_end: do
          if (associated(this_gp_line_bar%next_line_bar)) then
             ! more datasets are present
             this_gp_line_bar => this_gp_line_bar%next_line_bar
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end

       this_gp_line_bar%next_line_bar => new_gp_line_bar
    else
       ! this is the first dataset to be added
       gp%first_line_bar => new_gp_line_bar
    end if

  end subroutine gp_add_line_bar
    !  #]
  subroutine gp_add_colored_hor_bar(gp,y1,y2,error_flag,fillcolor)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: y1, y2
    integer,            intent(out)   :: error_flag
    integer,   optional,intent(in)    :: fillcolor

    error_flag = no_error
    call gp_add_line_bar(gp,gp_lb_type_colored_hor_bar,y1,y2,&
                         gp_line_default,fillcolor,error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_add_colored_hor_bar
    !  #]
  subroutine gp_add_colored_ver_bar(gp,x1,x2,error_flag,fillcolor)
    !  #[

    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: x1, x2
    integer,            intent(out)   :: error_flag
    integer,   optional,intent(in)    :: fillcolor

    error_flag = no_error
    call gp_add_line_bar(gp,gp_lb_type_colored_ver_bar,x1,x2,&
                         gp_line_default,fillcolor,error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_add_colored_ver_bar
    !  #]
  subroutine gp_add_hor_line(gp,y,error_flag,color,linestyle)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: y
    integer,            intent(out)   :: error_flag
    integer,   optional,intent(in)    :: color
    integer,   optional,intent(in)    :: linestyle

    ! local variables
    integer :: tmp_color
    integer :: tmp_linestyle

    error_flag = no_error

    tmp_color = gp_color_default
    if (present(color)) tmp_color = color

    tmp_linestyle = gp_line_default
    if (present(linestyle)) tmp_linestyle = linestyle

    call gp_add_line_bar(gp,gp_lb_type_hor_line,y,missing_indicator_real_r8,&
                         tmp_linestyle,tmp_color,error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_add_hor_line
    !  #]
  subroutine gp_add_ver_line(gp,x,error_flag,color,linestyle)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: x
    integer,            intent(out)   :: error_flag
    integer,   optional,intent(in)    :: color
    integer,   optional,intent(in)    :: linestyle

    ! local variables
    integer :: tmp_color
    integer :: tmp_linestyle

    error_flag = no_error

    tmp_color = gp_color_default
    if (present(color)) tmp_color = color

    tmp_linestyle = gp_line_default
    if (present(linestyle)) tmp_linestyle = linestyle

    call gp_add_line_bar(gp,gp_lb_type_ver_line,x,missing_indicator_real_r8,&
                         tmp_linestyle,tmp_color,error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_add_ver_line
    !  #]
  subroutine gp_add_dataset(gp,x,y,error_flag,name,datafilename,&
                            linecolor,linestyle,symbol,datastyle)
    !  #[
    type(gp_plot_type),       intent(inout) :: gp
    real(r8_), dimension(:),  intent(in)    :: x
    real(r8_), dimension(:),  intent(in)    :: y
    integer,                  intent(out)   :: error_flag
    character(len=*),optional,intent(in)    :: name
    character(len=*),optional,intent(in)    :: datafilename
    integer,         optional,intent(in)    :: linecolor
    integer,         optional,intent(in)    :: linestyle
    integer,         optional,intent(in)    :: symbol
    integer,         optional,intent(in)    :: datastyle

    ! local variables
    type(gp_dataset_type), pointer :: new_gp_dataset
    type(gp_dataset_type), pointer :: this_gp_dataset
    integer   :: AllocStat, npoints
    real(r8_) :: x_min, x_max, y_min, y_max

    error_flag = no_error
    allocate(new_gp_dataset,stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_dataset"
       error_flag = error_allocate
       return
    end if

    npoints = size(x)
    if (size(y) .ne. npoints) then
       print *,"Warning in gp_add_dataset:"
       print *,"length of x and y arrays differ!"
       print *,"size(x) = ",size(x)
       print *,"size(y) = ",size(y)
       npoints = min(size(x),size(y))
       print *,"using minimum of both sizes: ",npoints
    end if

    allocate(new_gp_dataset%x(npoints),&
             new_gp_dataset%y(npoints),stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_dataset"
       error_flag = error_allocate
       return
    end if

    ! copy the data
    new_gp_dataset%x(1:npoints) = x(1:npoints)
    new_gp_dataset%y(1:npoints) = y(1:npoints)

    new_gp_dataset%name(:) = ' '
    if (present(name)) &
         new_gp_dataset%name = name

    new_gp_dataset%datafilename(:) = ' '
    if (present(datafilename)) &
         new_gp_dataset%datafilename = datafilename

    nullify(new_gp_dataset%next_dataset)

    new_gp_dataset%linecolor = gp_color_default
    if (present(linecolor)) &
             new_gp_dataset%linecolor = linecolor

    new_gp_dataset%linestyle = gp_line_default
    if (present(linestyle)) &
             new_gp_dataset%linestyle = linestyle

    new_gp_dataset%symbol = gp_symbol_default
    if (present(symbol)) &
             new_gp_dataset%symbol = symbol

    new_gp_dataset%datastyle = datastyle_Default
    if (present(datastyle)) then
       new_gp_dataset%datastyle = datastyle
    end if

    if (associated(gp%first_dataset)) then
       ! more datasets are present already
       this_gp_dataset => gp%first_dataset
       find_list_end: do
          if (associated(this_gp_dataset%next_dataset)) then
             ! more datasets are present
             this_gp_dataset => this_gp_dataset%next_dataset
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end
       
       this_gp_dataset%next_dataset => new_gp_dataset
    else
       ! this is the first dataset to be added
       gp%first_dataset => new_gp_dataset
    end if

    ! handle x scaling
    if (gp%x_autoscale) then
       x_min = minval(x)
       x_max = maxval(x)
       if (missing_real(gp%x_min)) then 
          gp%x_min = x_min
       else
          gp%x_min = min(gp%x_min,x_min)
       end if
       if (missing_real(gp%x_max)) then 
          gp%x_max = x_max
       else
          gp%x_max = max(gp%x_max,x_max)
       end if
    end if

    ! handle y scaling
    if (gp%y_autoscale) then
       y_min = minval(y)
       y_max = maxval(y)
       if (missing_real(gp%y_min)) then 
          gp%y_min = y_min
       else
          gp%y_min = min(gp%y_min,y_min)
       end if
       if (missing_real(gp%y_max)) then 
          gp%y_max = y_max
       else
          gp%y_max = max(gp%y_max,y_max)
       end if
    end if

  end subroutine gp_add_dataset
    !  #]
  subroutine gp_add_3d_dataset(gp,x,y,z,error_flag,name,datafilename,&
                               contourdatafilename,&
                               linecolor,linestyle)
    !  #[
    type(gp_plot_type),       intent(inout) :: gp
    real(r8_), dimension(:),  intent(in)    :: x
    real(r8_), dimension(:),  intent(in)    :: y
    real(r8_), dimension(:,:),intent(in)    :: z
    integer,                  intent(out)   :: error_flag
    character(len=*),optional,intent(in)    :: name
    character(len=*),optional,intent(in)    :: datafilename
    character(len=*),optional,intent(in)    :: contourdatafilename
    ! linecolor/style are only used when plotting contours
    ! not when plotting shades
    integer,         optional,intent(in)    :: linecolor
    integer,         optional,intent(in)    :: linestyle

    ! local variables
    type(gp_3d_dataset_type), pointer :: new_gp_3d_dataset
    type(gp_3d_dataset_type), pointer :: this_gp_3d_dataset
    integer   :: AllocStat, npoints_x,npoints_y
    real(r8_) :: x_min, x_max, y_min, y_max, z_min, z_max

    error_flag = no_error
    allocate(new_gp_3d_dataset,stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_3d_dataset"
       error_flag = error_allocate
       return
    end if

    npoints_x = size(x) ! 1st dim. in z
    npoints_y = size(y) ! 2nd dim. in z

    if (size(z,1) .ne. npoints_x) then
       print *,"Warning in gp_add_3d_dataset:"
       print *,"length of x and z(1st-dim) arrays differ!"
       print *,"size(x)   = ",size(x)
       print *,"size(z,1) = ",size(z,1)
       npoints_x = min(size(x),size(z,1))
       print *,"using minimum of both sizes: ",npoints_x
    end if
    if (size(z,2) .ne. npoints_y) then
       print *,"Warning in gp_add_3d_dataset:"
       print *,"length of y and z(2nd-dim) arrays differ!"
       print *,"size(x)   = ",size(x)
       print *,"size(z,2) = ",size(z,2)
       npoints_y = min(size(y),size(z,2))
       print *,"using minimum of both sizes: ",npoints_y
    end if

    allocate(new_gp_3d_dataset%x(npoints_x),&
             new_gp_3d_dataset%y(npoints_y),&
             new_gp_3d_dataset%z(npoints_x,npoints_y),&
             stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_3d_dataset"
       error_flag = error_allocate
       return
    end if

    ! copy the data
    new_gp_3d_dataset%x(1:npoints_x) = x(1:npoints_x)
    new_gp_3d_dataset%y(1:npoints_y) = y(1:npoints_y)
    new_gp_3d_dataset%z(1:npoints_x,1:npoints_y) = z(1:npoints_x,1:npoints_y)

    new_gp_3d_dataset%name(:) = ' '
    if (present(name)) &
         new_gp_3d_dataset%name = name

    new_gp_3d_dataset%datafilename(:) = ' '
    if (present(datafilename)) &
         new_gp_3d_dataset%datafilename = datafilename

    new_gp_3d_dataset%contourdatafilename(:) = ' '
    if (present(contourdatafilename)) &
         new_gp_3d_dataset%contourdatafilename = contourdatafilename

    new_gp_3d_dataset%linecolor = gp_color_default
    if (present(linecolor)) &
             new_gp_3d_dataset%linecolor = linecolor

    new_gp_3d_dataset%linestyle = gp_line_default
    if (present(linestyle)) &
             new_gp_3d_dataset%linestyle = linestyle

    nullify(new_gp_3d_dataset%next_3d_dataset)

    if (associated(gp%first_3d_dataset)) then
       ! more datasets are present already
       this_gp_3d_dataset => gp%first_3d_dataset
       find_list_end: do
          if (associated(this_gp_3d_dataset%next_3d_dataset)) then
             ! more datasets are present
             this_gp_3d_dataset => this_gp_3d_dataset%next_3d_dataset
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end
       
       this_gp_3d_dataset%next_3d_dataset => new_gp_3d_dataset
    else
       ! this is the first 3d dataset to be added
       gp%first_3d_dataset => new_gp_3d_dataset
    end if

    ! handle x scaling
    if (gp%x_autoscale) then
       x_min = minval(x)
       x_max = maxval(x)
       if (missing_real(gp%x_min)) then 
          gp%x_min = x_min
       else
          gp%x_min = min(gp%x_min,x_min)
       end if
       if (missing_real(gp%x_max)) then 
          gp%x_max = x_max
       else
          gp%x_max = max(gp%x_max,x_max)
       end if
    end if

    ! handle y scaling
    if (gp%y_autoscale) then
       y_min = minval(y)
       y_max = maxval(y)
       if (missing_real(gp%y_min)) then 
          gp%y_min = y_min
       else
          gp%y_min = min(gp%y_min,y_min)
       end if
       if (missing_real(gp%y_max)) then 
          gp%y_max = y_max
       else
          gp%y_max = max(gp%y_max,y_max)
       end if
    end if

    ! handle z scaling
    if (gp%z_autoscale) then
       z_min = minval(z)
       z_max = maxval(z)
       if (missing_real(gp%z_min)) then 
          gp%z_min = z_min
       else
          gp%z_min = min(gp%z_min,z_min)
       end if
       if (missing_real(gp%z_max)) then 
          gp%z_max = z_max
       else
          gp%z_max = max(gp%z_max,z_max)
       end if
    end if

  end subroutine gp_add_3d_dataset
    !  #]
  subroutine gp_add_histogram(gp,x1,x2,y,error_flag,name,datafilename,&
                              linecolor)
    !  #[
    type(gp_plot_type),       intent(inout) :: gp
    real(r8_), dimension(:),  intent(in)    :: x1, x2, y
    integer,                  intent(out)   :: error_flag
    character(len=*),optional,intent(in)    :: name
    character(len=*),optional,intent(in)    :: datafilename
    integer,         optional,intent(in)    :: linecolor

    ! local variables
    type(gp_dataset_type), pointer :: new_gp_dataset
    type(gp_dataset_type), pointer :: this_gp_dataset
    integer   :: AllocStat, npoints, i
    real(r8_) :: x_min, x_max, y_min, y_max

    error_flag = no_error
    allocate(new_gp_dataset,stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_histogram"
       error_flag = error_allocate
       return
    end if

    npoints = size(x1)
    if (size(y) .ne. npoints) then
       print *,"Warning in gp_add_histogram:"
       print *,"length of x1 and y arrays differ!"
       print *,"size(x1) = ",size(x1)
       print *,"size(y)  = ",size(y)
       npoints = min(size(x1),size(y))
       print *,"using minimum of both sizes: ",npoints
    end if
    if (size(x2) .ne. npoints) then
       print *,"Warning in gp_add_histogram:"
       print *,"length of x2 and npoints differ!"
       print *,"size(x2) = ",size(x2)
       print *,"npoints  = ",npoints
       npoints = min(size(x1),npoints)
       print *,"using minimum of both sizes: ",npoints
    end if

    allocate(new_gp_dataset%x(4*npoints),&
             new_gp_dataset%y(4*npoints),stat=AllocStat)
    if (AllocStat .ne. 0) then
       print *,"allocation problem in gp_add_dataset"
       error_flag = error_allocate
       return
    end if

    ! copy the data
    DO i=1,npoints
       new_gp_dataset%x((i-1)*4+1) = x1(i)
       new_gp_dataset%x((i-1)*4+2) = x1(i)
       new_gp_dataset%x((i-1)*4+3) = x2(i)
       new_gp_dataset%x((i-1)*4+4) = x2(i)
       new_gp_dataset%y((i-1)*4+1) = 0._r8_
       new_gp_dataset%y((i-1)*4+2) = y(i)
       new_gp_dataset%y((i-1)*4+3) = y(i)
       new_gp_dataset%y((i-1)*4+4) = 0._r8_
    END DO

    new_gp_dataset%name(:) = ' '
    if (present(name)) &
         new_gp_dataset%name = name

    new_gp_dataset%datafilename(:) = ' '
    if (present(datafilename)) &
         new_gp_dataset%datafilename = datafilename

    nullify(new_gp_dataset%next_dataset)

    new_gp_dataset%linecolor = gp_color_default
    if (present(linecolor)) &
             new_gp_dataset%linecolor = linecolor

    new_gp_dataset%linestyle = gp_line_default
    new_gp_dataset%symbol = gp_symbol_default
    new_gp_dataset%datastyle = datastyle_Steps

    if (associated(gp%first_dataset)) then
       ! more datasets are present already
       this_gp_dataset => gp%first_dataset
       find_list_end: do
          if (associated(this_gp_dataset%next_dataset)) then
             ! more datasets are present
             this_gp_dataset => this_gp_dataset%next_dataset
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end
       
       this_gp_dataset%next_dataset => new_gp_dataset
    else
       ! this is the first dataset to be added
       gp%first_dataset => new_gp_dataset
    end if

    ! handle x scaling
    if (gp%x_autoscale) then
       x_min = minval(x1)
       x_max = maxval(x2)
       if (missing_real(gp%x_min)) then 
          gp%x_min = x_min
       else
          gp%x_min = min(gp%x_min,x_min)
       end if
       if (missing_real(gp%x_max)) then 
          gp%x_max = x_max
       else
          gp%x_max = max(gp%x_max,x_max)
       end if
    end if

    ! handle y scaling
    if (gp%y_autoscale) then
       y_min = minval(y)
       y_max = maxval(y)
       if (missing_real(gp%y_min)) then 
          gp%y_min = y_min
       else
          gp%y_min = min(gp%y_min,y_min)
       end if
       if (missing_real(gp%y_max)) then 
          gp%y_max = y_max
       else
          gp%y_max = max(gp%y_max,y_max)
       end if
    end if

  end subroutine gp_add_histogram
    !  #]
  subroutine gp_add_label(gp,text,xpos,ypos,error_flag,&
                          rotate,justify,textcolor)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    character(len=*),   intent(in)    :: text
    real(r8_),          intent(in)    :: xpos
    real(r8_),          intent(in)    :: ypos
    integer,            intent(out)   :: error_flag
    real(r8_),optional, intent(in)    :: rotate
    integer,  optional, intent(in)    :: justify
    integer,  optional, intent(in)    :: textcolor

    ! local variables
    type(gp_label_type), pointer :: new_label, this_label
    integer :: AllocStatus
    
    error_flag = no_error
    allocate(new_label,stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"allocation problem in gp_add_label"
       error_flag = error_allocate
       return
    end if
    
    ! copy the data
    new_label%text      = text
    new_label%xpos      = xpos
    new_label%ypos      = ypos
    new_label%rotate    = 0._r8_
    new_label%justify   = gp_justify_default
    new_label%textcolor = gp_color_default
    nullify(new_label%next_label)

    ! check the optional inputs
    if (present(rotate))    new_label%rotate    = rotate
    if (present(justify))   new_label%justify   = justify
    if (present(textcolor)) new_label%textcolor = textcolor

    if (associated(gp%first_label)) then
       ! more labels are present already
       this_label =>gp%first_label
       find_list_end: do
          if (associated(this_label%next_label)) then
             ! more labels are present
             this_label => this_label%next_label
          else
             ! end of linked list found
             exit find_list_end
          end if
       end do find_list_end

       this_label%next_label => new_label
    else
       ! this is the first label to be added
       gp%first_label => new_label
    end if

  end subroutine gp_add_label
    !  #]
  subroutine gp_print_datasets(gp)
    !  #[ for debugging only
    type(gp_plot_type), intent(inout) :: gp
    
    ! local variables
    type(gp_dataset_type),    pointer :: this_dataset
    type(gp_3d_dataset_type), pointer :: this_3d_dataset
    integer :: i,j

    this_dataset => gp%first_dataset
    do while (associated(this_dataset))
       print *,"2D dataset: name = "//trim(this_dataset%name)
       do i=1,size(this_dataset%x) 
          print *,"i,x,y=",i,&
               this_dataset%x(i),&
               this_dataset%y(i)
       end do
       this_dataset => this_dataset%next_dataset
    end do

    this_3d_dataset => gp%first_3d_dataset
    do while (associated(this_3d_dataset))
       print *,"3D dataset: name = "//trim(this_3d_dataset%name)
       do i=1,size(this_3d_dataset%x)
          do j=1,size(this_3d_dataset%y)
             print *,"i,j,x,y,z=",i,j,&
                  this_3d_dataset%x(i),&
                  this_3d_dataset%y(j),&
                  this_3d_dataset%z(i,j)
          end do
       end do
       this_dataset => this_dataset%next_dataset
    end do

  end subroutine gp_print_datasets
    !  #]
  subroutine gp_overplot_arrow(gp,x1,y1,x2,y2,error_flag,nohead,&
                               color,linestyle)
    !  #[
    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: x1,y1,x2,y2
    integer,            intent(out)   :: error_flag
    logical, optional,  intent(in)    :: nohead
    integer, optional,  intent(in)    :: color
    integer, optional,  intent(in)    :: linestyle

    ! local variables
    character(len=gp_maxstrlen) :: command
    character(len=gp_maxstrlen) :: x1_str, y1_str
    character(len=gp_maxstrlen) :: x2_str, y2_str
    character(len=6)            :: clr_str
    character(len=6)            :: style_str

    error_flag = no_error

    write(x1_str,*,err=996) x1
    write(y1_str,*,err=997) y1
    write(x2_str,*,err=998) x2
    write(y2_str,*,err=999) y2

    command = "set arrow from "//trim(x1_str)//","//trim(y1_str)//&
                         " to "//trim(x2_str)//","//trim(y2_str)
    if (present(nohead)) then
       if (nohead) command = trim(command)//" nohead"
    end if

    if (present(linestyle)) then
          write(style_str,"(i6)",err=995) linestyle
          command = trim(command)//" lt "//trim(adjustl(style_str))
    end if

    if (present(color)) then
       if (gnuplot_version .ge. "4.2") then
          ! this seems only possible for gnuplot v4.2 or above!
          write(clr_str,"(i6)",err=995) color
          command = trim(command)//" lc "//trim(adjustl(clr_str))
       end if
    end if

    call gp_add_command(gp,command,error_flag)
    if (error_flag .ne. no_error) return

    return

995 print *,"Error in gp_overplot_arrow:"
    print *,"Could not convert linecolor to a proper string"
    error_flag = error_writing_file
    return

996 print *,"ERROR in gp_overplot_arrow"
    print *,"cound not convert number x1 to a string"
    error_flag = error_writing_file
    return

997 print *,"ERROR in gp_overplot_arrow"
    print *,"cound not convert number y1 to a string"
    error_flag = error_writing_file
    return

998 print *,"ERROR in gp_overplot_arrow"
    print *,"cound not convert number x2 to a string"
    error_flag = error_writing_file
    return

999 print *,"ERROR in gp_overplot_arrow"
    print *,"cound not convert number y2 to a string"
    error_flag = error_writing_file
    return

  end subroutine gp_overplot_arrow

    !  #]
  subroutine gp_overplot_line(gp,x1,y1,x2,y2,error_flag,color,linestyle)
    !  #[

    type(gp_plot_type), intent(inout) :: gp
    real(r8_),          intent(in)    :: x1,y1,x2,y2
    integer,            intent(out)   :: error_flag
    integer, optional,  intent(in)    :: color
    integer, optional,  intent(in)    :: linestyle

    error_flag = no_error
    call gp_overplot_arrow(gp,x1,y1,x2,y2,error_flag,nohead=.true.,&
                           color=color,linestyle=linestyle)

  end subroutine gp_overplot_line

    !  #]
  subroutine gp_apply_all_ranges(gp, error_flag)
    !  #[ apply x/y/z ranges
    type(gp_plot_type), intent(inout) :: gp
    integer,            intent(out)   :: error_flag

    ! local variable
    character(len=gp_maxstrlen) :: command

    ! replace the actual min/ax values derived from all datasets
    ! by the user defined values
    if (.not. gp%x_autoscale) then
       if ( (.not. missing_real(gp%xrange(1))) .and. &
            (.not. missing_real(gp%xrange(2)))       ) then
          gp%x_min = gp%xrange(1)
          gp%x_max = gp%xrange(2)
       else
          print *,"ERROR: xrange not fully defined"
          error_flag = error_programming
          return
       end if
    end if
    if (.not. gp%y_autoscale) then
       if ( (.not. missing_real(gp%yrange(1))) .and. &
            (.not. missing_real(gp%yrange(2)))       ) then
          gp%y_min = gp%yrange(1)
          gp%y_max = gp%yrange(2)
       else
          print *,"ERROR: yrange not fully defined"
          error_flag = error_programming
          return
       end if
    end if
    if (.not. gp%z_autoscale) then
       if ( (.not. missing_real(gp%zrange(1))) .and. &
            (.not. missing_real(gp%zrange(2)))       ) then
          gp%z_min = gp%zrange(1)
          gp%z_max = gp%zrange(2)
       else
          print *,"ERROR: zrange not fully defined"
          error_flag = error_programming
          return
       end if
    end if

    write(command,*) "set xrange [",gp%x_min,":",gp%x_max,"]"
    call gp_add_command(gp,adjustl(command),error_flag)
    if (error_flag .ne. no_error) return

    write(command,*) "set yrange [",gp%y_min,":",gp%y_max,"]"
    call gp_add_command(gp,adjustl(command),error_flag)
    if (error_flag .ne. no_error) return

    write(command,*) "set zrange [",gp%z_min,":",gp%z_max,"]"
    call gp_add_command(gp,adjustl(command),error_flag)
    if (error_flag .ne. no_error) return

  end subroutine gp_apply_all_ranges
    !  #]
  subroutine gp_apply_title_and_xyzlabels(gp, error_flag)
    !  #[ apply title and x/y/z labels
    type(gp_plot_type), intent(inout) :: gp
    integer,            intent(out)   :: error_flag

    if (associated(gp%title)) then
       call gp_add_command(gp,'set title "'//&
               trim(chararray2string(gp%title))//'"',error_flag)
       if (error_flag .ne. no_error) return
    end if
    if (associated(gp%xlabel)) then
       call gp_add_command(gp,'set xlabel "'//&
               trim(chararray2string(gp%xlabel))//'"',error_flag)
       if (error_flag .ne. no_error) return
    end if
    if (associated(gp%ylabel)) then
       call gp_add_command(gp,'set ylabel "'//&
               trim(chararray2string(gp%ylabel))//'"',error_flag)
       if (error_flag .ne. no_error) return
    end if
    if (associated(gp%zlabel)) then
       call gp_add_command(gp,'set zlabel "'//&
               trim(chararray2string(gp%zlabel))//'"',error_flag)
       if (error_flag .ne. no_error) return
    end if
  end subroutine gp_apply_title_and_xyzlabels
    !  #]
  subroutine FixContourDatafile(filename,filename2,error_flag)
    !  #[
    character(len=*), intent(in)  :: filename
    character(len=*), intent(in)  :: filename2
    integer,          intent(out) :: error_flag

    ! local variables
    character(len=100) :: line
    integer   :: fileunit
    integer   :: AllocStatus
    integer   :: linecount
    integer   :: c,s,l,num_datalines
    logical   :: start_new_section
    logical   :: do_print

    ! local types
    type Stored_Surface_Type
       character(len=1),dimension(:), pointer :: heading_line
       integer                                :: num_contours
       type(Contour_Type),            pointer :: first_contour
    end type Stored_Surface_Type

    type Contour_Type
       character(len=1),dimension(:), pointer :: heading_line
       logical                                :: isoline_safe
       integer                                :: num_sections
       type(Section_Type),            pointer :: first_section
       type(Contour_Type),            pointer :: next_contour
    end type Contour_Type

    type Section_Type
       integer                      :: num_lines
       type(Dataline_Type), pointer :: first_dataline
       type(Section_Type),  pointer :: next_section
    end type Section_Type

    type Dataline_Type
       character(len=1), dimension(:), pointer :: line
       type(Dataline_Type),            pointer :: next_dataline
    end type Dataline_Type

    ! alternative implementation might be to use linked lists
    ! to sotre this data, since num_secions per contour 
    ! and num_lines per section are variable. This saves memory and
    ! prevents the necessity to scan the file twice.
    
    type(Stored_Surface_Type)    :: Surface
    type(Contour_Type),  pointer :: this_contour, new_contour
    type(Contour_Type),  pointer :: contour_to_delete
    type(Section_Type),  pointer :: this_section, new_section
    type(Section_Type),  pointer :: section_to_delete
    type(Dataline_Type), pointer :: this_dataline, new_dataline
    type(Dataline_Type), pointer :: dataline_to_delete

    ! init
    error_flag = no_error
    linecount     = 0
    !num_datalines = 0
    !num_contours  = 0
    !num_sections  = 0
    !section_seems_safe = .false.
    !max_num_sections   = 0
    !max_num_datalines  = 0
    start_new_section = .false.
    nullify(this_contour,new_contour)
    nullify(this_section,new_section)
    nullify(this_dataline,new_dataline)

    ! init the surface structure
    nullify(Surface%heading_line)
    nullify(Surface%first_contour)
    Surface%num_contours = 0

    fileunit = get_lun()
    open(unit=fileunit,file=filename,status="old",&
         form="FORMATTED",action="read",err=999)
    !  #[ read the file

    readdataloop: do
       read(fileunit,"(a)",err=998,end=111) line
       linecount = linecount + 1

       !print *,"line: "//trim(line)

       if (line(1:8) .eq. "#Surface") then
          ! just one surface expected, so this line should
          ! occur just once !
          if (.not. associated(Surface%heading_line)) then
             call gp_fill_chararray(Surface%heading_line,line,error_flag)
             if (error_flag .ne. no_error) return
             start_new_section = .true.
          else
             print *,"ERROR: just a single #Surface line expected in this file!"
             error_flag = error_reading_file
             return
          end if
          cycle
       end if

       ! new contour found
       if (line(1:9) .eq. "# Contour") then
          !print *,"adding new contour"
          Surface%num_contours = Surface%num_contours + 1
          allocate(new_contour,stat=AllocStatus)
          if (AllocStatus .ne. 0) then
             print *,"allocation problem in FixContourDatafile"
             error_flag = error_allocate
             return
          end if

          !init the new contour
          nullify(new_contour%heading_line)
          new_contour%isoline_safe = .false.
          new_contour%num_sections = 0
          nullify(new_contour%first_section)
          nullify(new_contour%next_contour)
          call gp_fill_chararray(new_contour%heading_line,line,error_flag)
          if (error_flag .ne. no_error) return

          if (associated(Surface%first_contour)) then
             ! add the new one to the linked list
             this_contour%next_contour => new_contour
          else
             ! add the new one as first contour
             Surface%first_contour => new_contour
          end if
          this_contour => new_contour
          start_new_section = .true.
          cycle
       end if

       ! empty line found
       if (len_trim(line) .eq. 0) then
          start_new_section = .true.
          cycle
       end if

       ! ok, we have a dataline
       
       ! start a new datasection if:
       ! 1) the previous line started a new surface or new contour
       ! 2) the previous line was an empty one, or multiple empty ones
       if (start_new_section) then
          !print *,"adding new section"
          start_new_section = .false.
          this_contour%num_sections = this_contour%num_sections + 1
          allocate(new_section,stat=AllocStatus)
          if (AllocStatus .ne. 0) then
             print *,"allocation problem in FixContourDatafile"
             error_flag = error_allocate
             return
          end if
          
          ! init the new section
          new_section%num_lines = 0
          nullify(new_section%first_dataline)
          nullify(new_section%next_section)

          if (associated(this_contour%first_section)) then
             ! add the new one to the linked list
             this_section%next_section => new_section
          else
             ! add the new one as first section
             this_contour%first_section => new_section
          end if
          this_section => new_section
       end if

       ! add the dataline
       !print *,"adding new dataline"
       this_section%num_lines = this_section%num_lines + 1
       allocate(new_dataline,stat=AllocStatus)
       if (AllocStatus .ne. 0) then
          print *,"allocation problem in FixContourDatafile"
          error_flag = error_allocate
          return
       end if

       nullify(new_dataline%line)
       nullify(new_dataline%next_dataline)
       call gp_fill_chararray(new_dataline%line,line,error_flag)
       if (error_flag .ne. no_error) return
       
       if (associated(this_section%first_dataline)) then
          ! add the new one to the linked list
          this_dataline%next_dataline => new_dataline
       else
          ! add the new one as first dataline
          this_section%first_dataline => new_dataline         
       end if
       this_dataline => new_dataline

       !num_datalines=num_datalines+1

       cycle
111    exit readdataloop
    end do readdataloop

    !  #]

    close(unit=fileunit)
    call free_lun(fileunit)
    
    ! check for unsafe sections that might produce isolines and fix them
    !  #[ 

    this_contour => Surface%first_contour
    c=1
    contour_loop1: do while (associated(this_contour))
       num_datalines = -1
       this_contour%isoline_safe = .false.
       this_section => this_contour%first_section
       section_loop1: do while (associated(this_section))
          if (num_datalines .eq. -1) then
             num_datalines = this_section%num_lines
          else
             if (num_datalines .ne. this_section%num_lines) then
                ! ok, the number of lines in this section is variable
                ! so gnuplot will not try to draw isolines
                this_contour%isoline_safe = .true.
             end if
          end if

          !this_dataline => this_section%first_dataline
          !datalineloop1: do while (associated(this_dataline))
          !   ! step to next dataline
          !   this_dataline => this_dataline%next_dataline
          !end do datalineloop1
          ! step to next section
          this_section => this_section%next_section
       end do section_loop1

       if (.not. this_contour%isoline_safe) then
          if (debug) then
             print *,"WARNING: contour ",c," is not safe for isolines"
             print *,"adding a dummy datapoint in 1st section to fix this"
          end if

          ! allocate a new dataline and init
          allocate(new_dataline,stat=AllocStatus)
          if (AllocStatus .ne. 0) then
             print *,"allocation problem in FixContourDatafile"
             error_flag = error_allocate
             return
          end if

          nullify(new_dataline%line)
          nullify(new_dataline%next_dataline)
          ! fill it by copying an existing point
          call gp_fill_chararray(new_dataline%line,&
               chararray2string(this_contour%first_section%first_dataline%line),&
               error_flag)
          if (error_flag .ne. no_error) return
          ! add it to the linked list of datalines
          new_dataline%next_dataline => this_contour%first_section%first_dataline
          this_contour%first_section%first_dataline => new_dataline

       end if
       ! step to the next contour
       this_contour => this_contour%next_contour
       c=c+1
    end do contour_loop1

    !  #]
    
    if (debug) then
       print *,"================================================="
       print *,"Summary, the file "//trim(filename)//" contained:"
       print *,"num_contours = ",Surface%num_contours
       print *,"linecount    = ",linecount
       print *,"================================================="
    end if

    do_print = .false.
    if (do_print) then
    !  #[ print the datastructure

    print *,"================================================="
    print *,"Datastructure:"
    print *,"==>Surface%heading_line  = "//&
         trim(chararray2string(Surface%heading_line))
    print *,"==>Surface%num_contours  = ",Surface%num_contours
    this_contour => Surface%first_contour
    c=1
    contour_loop2: do while (associated(this_contour))
       print *,"==>Surface%contour(",c,")%heading_line = "//&
            trim(chararray2string(this_contour%heading_line))
       print *,"==>Surface%contour(",c,")%isoline_safe = ",&
            this_contour%isoline_safe
       print *,"==>Surface%contour(",c,")%num_sections = ",&
            this_contour%num_sections

       this_section => this_contour%first_section
       s=1
       section_loop2: do while (associated(this_section))
          print *,"==>Surface%contour(",c,")%section(",s,")%num_lines = ",&
               this_section%num_lines
          
          this_dataline => this_section%first_dataline
          l=1
          datalineloop2: do while (associated(this_dataline))

             print *,"==>Surface%contour(",c,")%section(",s,")%line(",l,") = "//&
                  trim(chararray2string(this_dataline%line))

             ! step to next dataline
             this_dataline => this_dataline%next_dataline
             l=l+1
          end do datalineloop2

          ! step to next section
          this_section => this_section%next_section
          s=s+1
       end do section_loop2

       ! step to the next contour
       this_contour => this_contour%next_contour
       c=c+1
    end do contour_loop2
    
    print *,"================================================="
    !  #]
    end if

    !  #[ write the datastructure to file
    fileunit = get_lun()
    open(unit=fileunit,file=filename2,status="replace",&
         form="FORMATTED",action="write",err=997)

    write(fileunit,"(a)") ""
    write(fileunit,"(a)") chararray2string(Surface%heading_line)
    write(fileunit,"(a)") ""

    this_contour => Surface%first_contour
    contour_loop3: do while (associated(this_contour))
       write(fileunit,"(a)") chararray2string(this_contour%heading_line)

       this_section => this_contour%first_section
       section_loop3: do while (associated(this_section))
          
          this_dataline => this_section%first_dataline
          datalineloop3: do while (associated(this_dataline))

             write(fileunit,"(a)") chararray2string(this_dataline%line)

             ! step to next dataline
             this_dataline => this_dataline%next_dataline
          end do datalineloop3

          write(fileunit,"(a)") ""

          ! step to next section
          this_section => this_section%next_section
       end do section_loop3

       write(fileunit,"(a)") ""

       ! step to the next contour
       this_contour => this_contour%next_contour
    end do contour_loop3

    write(fileunit,"(a)") ""

    close(unit=fileunit)
    call free_lun(fileunit)

    !  #]

    !  #[ clean up; delete the datastructure
    deallocate(Surface%heading_line)
    this_contour => Surface%first_contour
    contour_loop4: do while (associated(this_contour))
       ! save this pointer to delete the struct below
       contour_to_delete => this_contour
       deallocate(contour_to_delete%heading_line)

       this_section => contour_to_delete%first_section
       section_loop4: do while (associated(this_section)) 
          ! save this pointer to delete the struct below
          section_to_delete => this_section

          this_dataline => section_to_delete%first_dataline
          datalineloop4: do while (associated(this_dataline))
             ! save this pointer to delete the struct below
             dataline_to_delete => this_dataline
             deallocate(dataline_to_delete%line)

             ! step to next dataline and delete the current one
             this_dataline => dataline_to_delete%next_dataline
             deallocate(dataline_to_delete)
          end do datalineloop4

          ! step to next section and delete the current one
          this_section => section_to_delete%next_section
          deallocate(section_to_delete)
       end do section_loop4

       ! step to the next contour and delete the current one
       this_contour => contour_to_delete%next_contour
       deallocate(contour_to_delete)
    end do contour_loop4
    !  #]

    return

997 print *,"Error in FixContourDatafile:"
    print *,"Could not open file for writing data: "//trim(filename2)
    error_flag = error_opening_file
    return

998 print *,"Error in FixContourDatafile:"
    print *,"Could not read data from file: "//trim(filename)
    error_flag = error_reading_file
    return

999 print *,"Error in FixContourDatafile:"
    print *,"Could not open file for reading data: "//trim(filename)
    error_flag = error_opening_file
    return

  end subroutine FixContourDatafile
    !  #]
  subroutine gp_create_plot(gp,output_file_no_ext,error_flag,&
                            cmdfile,cmdfile_contours,&
                            display)
    !  #[
    type(gp_plot_type),         intent(inout) :: gp
    character(len=*),           intent(in)    :: output_file_no_ext
    integer,                    intent(out)   :: error_flag
    character(len=*), optional, intent(in)    :: cmdfile
    character(len=*), optional, intent(in)    :: cmdfile_contours
    logical,          optional, intent(in)    :: display

    ! local variables
    type(gp_dataset_type),    pointer :: this_dataset
    type(gp_3d_dataset_type), pointer :: this_3d_dataset
    type(gp_label_type),      pointer :: this_label
    type(gp_line_bar_type),   pointer :: this_line_bar

    character(len=gp_maxstrlen) :: command
    character(len=50)   :: contour_level_value
    character(len=256)  :: output_file
    character(len=256)  :: temp_data_filename
    character(len=256)  :: temp_data_filename2
    character(len=256)  :: temp_data_filename2_fixed
    character(len=256)  :: temp_gnuplot_filename
    character(len=256)  :: temp_gnuplot_filename2
    character(len=15120) :: plot_cmd_datasets
    character(len=15120) :: long_command
    character(len=512)  :: system_command
    character(len=25)   :: cnt_str,clr_str,smb_str, lt_str, lvl_str
    character(len=64)   :: rotate_str
    character(len=64)   :: title_str
    character(len=64)   :: val1_str, val2_str, val3_str
    character(len=gp_maxstrlen) :: ratio_str
    integer   :: fileunit, i, j, cnt, cnt_3d
    integer   :: AllocStatus
    real(r8_) :: eps, contour_level_step
    type(gp_plot_type) :: tmp_gp
    logical   :: display_result

    ! NOTE:
    ! in this routine all actual gnuplot commands are issued
    ! based on all settings collected in the gp datastructure

    ! init
    error_flag = no_error
    nullify(this_dataset,this_3d_dataset,this_label,this_line_bar)

    display_result = .true.
    if (present(display)) display_result = display

    ! apply all settings stored in the gp structure

    !  #[ handle the output type setting and add the file extension
    select case(gp%Output_Type)
    case(output_type_ps)
       command = "set terminal postscript color"
       output_file = trim(output_file_no_ext)//".ps"
    case(output_type_eps)
       command = "set terminal postscript color eps"       
       output_file = trim(output_file_no_ext)//".eps"
    case(output_type_png)
       !command = "set terminal png size 640x480" ! default
       ! use a large fontsize
       command = "set terminal png large size 1024,768 "//&
                 "xffffff x000000 x000000 x000000"
       ! the last 4 hexadecimal numbers define 4 colors for:
       ! backgr borders/axes ??? and contours (=color nr 0
       ! This is the way to get black contours when plotting
       ! shades and contours into the same plot with multiplot

       ! use a giant fontsize to be able to use the plot for presentations
       !command = "set terminal png giant size 1280,1024"!my screensize at knmi
       output_file = trim(output_file_no_ext)//".png"
    case default
       print *,"ERROR in SetOutputType:"
       print *,"output_type has unknown value: ",gp%Output_Type
       error_flag = error_programming
       return
    end select
    call gp_add_command(gp,command,error_flag)
    if (error_flag .ne. no_error) return

    command = "set output '"//trim(output_file)//"'"
    call gp_add_command(gp,command,error_flag)
    if (error_flag .ne. no_error) return

    ! for testing only:
    !if (gp%Output_Type .eq. output_type_png) then
    !   ! extra commands for png output only
    !   ! let the background be transparant
    !   command = "set terminal png transparent"
    !   call gp_add_command(gp,command,error_flag)
    !   if (error_flag .ne. no_error) return
    !   command = "set terminal png size 1024,768" ! 1024x768
    !   ! default is 640x480
    !   call gp_add_command(gp,command,error_flag)
    !   if (error_flag .ne. no_error) return
    !end if
    !  #]

    call gp_apply_title_and_xyzlabels(gp, error_flag)
    if (error_flag .ne. no_error) return

    call gp_apply_all_ranges(gp, error_flag)
    if (error_flag .ne. no_error) return

    !  #[ apply aspect ratio setting and clipping
    if (.not. missing_real(gp%AspectRatio)) then
       write(ratio_str,*) gp%AspectRatio
       write(command,*) "set size ratio "//trim(adjustl(ratio_str))
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return
    end if

    if (gp%clipping .eq. clipping_clip_two) then
       command = "set clip two"
       call gp_add_command(gp,command,error_flag)
    else
       print *,"ERROR: sorry, only clip_two has been implemented"
       error_flag = error_not_yet_implemented
       return
    end if
    !  #]
    !  #[ apply contour and shade plot settings
    if (gp%ShadePlot) then
       !  #[ set the palette
       ! for examples of setting the palette see:
       !   http://gnuplot.sourceforge.net/demo/pm3dcolors.html

       select case(gp%palette)
       case(gp_palette_default)
       case(gp_palette_color)
          command = "set palette color"
          !command = "set palette color positive"
       case(gp_palette_color_neg)
          command = "set palette color negative"
       case(gp_palette_gray)
          command = "set palette gray"
       case(gp_palette_gray_neg)
          command = "set palette gray negative"
       case(gp_palette_white_blue)
          ! white-to-blue gradient 
          ! (color lowest value (0) is rgb=111, highest value (1) rgb=001)
          command = "set palette defined (0 1 1 1,1 0 0 1)"
       case(gp_palette_blue_white)
          command = "set palette defined (0 0 0 1,1 1 1 1)"
       case(gp_palette_green_red_violet) ! green-red-violet
          command = "set palette rgbformulae 3,11,6"
       case(gp_palette_green_blue_white) ! green-blue-white
          command = "set palette rgbformulae 23,28,3"
       case(gp_palette_black_bl_vio_y_wh) !black-blue-violet-yellow-white
          command = "set palette rgbformulae 30,31,32"
       end select

       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return     
       !  #]
    end if

    if (gp%ContourPlot .and. gp%ShadePlot) then
       !  #[

       ! see http://www.gnuplot.info/faq/faq.html
       ! section 3.11 for these 2 commands

       ! see: http://newsgroups.derkeiler.com/Archive/Comp/ \
       !      comp.graphics.apps.gnuplot/2005-12/msg00058.html
       ! for the trick of combining shades and contours

       ! display the data as a map, looking at it from above
       command = "set view map"
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return

       ! plot the colors at the base, in stead of at the curved surface itself
       command = "set pm3d at b"
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return     

       ! require the "with pm3d" statement for drawing a shaded surface
       !command = "set pm3d explicit"
       !call gp_add_command(gp,command,error_flag)
       !if (error_flag .ne. no_error) return     

       command = "set multiplot"
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return     

       ! this should place the legend for the contourlines
       ! outside the plot (and outside the legend for the shaded
       ! surface), but for postscript output it doesn't seem to work
       !command = "set key outside"
       !call gp_add_command(gp,command,error_flag)
       !if (error_flag .ne. no_error) return     

       ! without this one, a datapoint is drawn at each grid point
       ! which is not what we want here
       command = "unset surface"
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return            

       !  #]
    else
       if (gp%ContourPlot) then
          !  #[
          ! see http://www.gnuplot.info/faq/faq.html
          ! section 3.11 for these 2 commands
          
          ! display the data as a map, looking at it from above
          command = "set view map"
          call gp_add_command(gp,command,error_flag)
          if (error_flag .ne. no_error) return
          
          ! plot the colors at the base, in stead of at the curved surface
          !command = "set pm3d at b"
          !call gp_add_command(gp,command,error_flag)
          !if (error_flag .ne. no_error) return     

          ! require the "with pm3d" statement for drawing a shaded surface
          !command = "set pm3d implicit"
          !command = "set pm3d explicit"
          !call gp_add_command(gp,command,error_flag)
          !if (error_flag .ne. no_error) return     

          !command = "set key outside"
          !call gp_add_command(gp,command,error_flag)
          !if (error_flag .ne. no_error) return     
          
          ! without this one, a datapoint is drawn at each grid point
          ! which is not what we want here
          !command = "unset surface"
          !call gp_add_command(gp,command,error_flag)
          !if (error_flag .ne. no_error) return            
       !  #]
       end if
       if (gp%ShadePlot) then
       !  #[
          ! see http://www.gnuplot.info/faq/faq.html
          ! section 3.11 for these commands

          ! The `set pm3d map` is an abbreviation for `set pm3d at b`; 
          ! `set view map`;`set style data pm3d`; `set style func pm3d`;.
          command = "set pm3d map"
          call gp_add_command(gp,command,error_flag)
          if (error_flag .ne. no_error) return     

       !  #]
       end if
    end if
    !  #]
    !  #[ add lines using the set arrow method
    this_line_bar =>gp%first_line_bar
    do while (associated(this_line_bar))

       select case (this_line_bar%type)
       case(gp_lb_type_hor_line)
          !  #[ apply gp_add_hor_line
          if (this_line_bar%linestyle .ne. gp_line_default) then
             if (this_line_bar%color .ne. gp_color_default) then
                call gp_overplot_line(gp,&
                                      gp%x_min,this_line_bar%val1,&
                                      gp%x_max,this_line_bar%val1,error_flag,&
                                      color=this_line_bar%color,&
                                      linestyle=this_line_bar%linestyle)
             else
                call gp_overplot_line(gp,&
                                      gp%x_min,this_line_bar%val1,&
                                      gp%x_max,this_line_bar%val1,error_flag,&
                                      linestyle=this_line_bar%linestyle)
             end if
          else
             if (this_line_bar%color .ne. gp_color_default) then
                call gp_overplot_line(gp,&
                                      gp%x_min,this_line_bar%val1,&
                                      gp%x_max,this_line_bar%val1,error_flag,&
                                      color=this_line_bar%color)
             else
                call gp_overplot_line(gp,&
                                      gp%x_min,this_line_bar%val1,&
                                      gp%x_max,this_line_bar%val1,error_flag)
             end if
          end if
          !  #]
       case(gp_lb_type_ver_line)
          !  #[ apply gp_add_ver_line
          if (this_line_bar%linestyle .ne. gp_line_default) then
             if (this_line_bar%color .ne. gp_color_default) then
                call gp_overplot_line(gp,&
                                      this_line_bar%val1,gp%y_min,&
                                      this_line_bar%val1,gp%y_max,error_flag,&
                                      color=this_line_bar%color,&
                                      linestyle=this_line_bar%linestyle)
             else
                call gp_overplot_line(gp,&
                                      this_line_bar%val1,gp%y_min,&
                                      this_line_bar%val1,gp%y_max,error_flag,&
                                      linestyle=this_line_bar%linestyle)
             end if
          else
             if (this_line_bar%color .ne. gp_color_default) then
                call gp_overplot_line(gp,&
                                      this_line_bar%val1,gp%y_min,&
                                      this_line_bar%val1,gp%y_max,error_flag,&
                                      color=this_line_bar%color)
             else
                call gp_overplot_line(gp,&
                                      this_line_bar%val1,gp%y_min,&
                                      this_line_bar%val1,gp%y_max,error_flag)
             end if
          end if
          !  #]
       case(gp_lb_type_colored_hor_bar)
          !  #[ apply gp_add_colored_hor_bar
          if (this_line_bar%color .ne. gp_color_default) then
             call gp_add_dataset(gp,&
                     (/gp%x_min,gp%x_max,gp%x_max,gp%x_min,gp%x_min/),&
                     (/ this_line_bar%val1,this_line_bar%val1,&
                        this_line_bar%val2,this_line_bar%val2,&
                        this_line_bar%val1 /),&
                     error_flag, linecolor=this_line_bar%color,&
                     datastyle=DataStyle_FilledCurve,&
                     linestyle=gp_line_full)
          else
             call gp_add_dataset(gp,&
                     (/gp%x_min,gp%x_max,gp%x_max,gp%x_min,gp%x_min/),&
                     (/ this_line_bar%val1,this_line_bar%val1,&
                        this_line_bar%val2,this_line_bar%val2,&
                        this_line_bar%val1 /),&
                     error_flag, &
                     datastyle=DataStyle_FilledCurve,&
                     linestyle=gp_line_full)             
          end if
          !  #]
       case(gp_lb_type_colored_ver_bar)
          !  #[ apply gp_add_colored_hor_bar
          if (this_line_bar%color .ne. gp_color_default) then
             call gp_add_dataset(gp,&
                     (/ this_line_bar%val1,this_line_bar%val1,&
                        this_line_bar%val2,this_line_bar%val2,&
                        this_line_bar%val1 /),&
                     (/gp%y_min,gp%y_max,gp%y_max,gp%y_min,gp%y_min/),&
                     error_flag, linecolor=this_line_bar%color,&
                     datastyle=DataStyle_FilledCurve,&
                     linestyle=gp_line_full)
          else
             call gp_add_dataset(gp,&
                     (/ this_line_bar%val1,this_line_bar%val1,&
                        this_line_bar%val2,this_line_bar%val2,&
                        this_line_bar%val1 /),&
                     (/gp%y_min,gp%y_max,gp%y_max,gp%y_min,gp%y_min/),&
                     error_flag, &
                     datastyle=DataStyle_FilledCurve,&
                     linestyle=gp_line_full)
          end if
          !  #]
       case default
          print *,"ERROR in gp_create_plot: "//&
               "unknown line/bar typer: ",this_line_bar%type
       end select

       ! handle next line bar
       this_line_bar => this_line_bar%next_line_bar
    end do
    !  #]
    !  #[ add the label definitions
    this_label =>gp%first_label
    eps = 0.001_r8_
    labellist: do
       !  #[

       ! check for validity of the pointer
       if (.not. associated(this_label)) exit labellist
       write(command,*) "set label ",&
            quote,trim(this_label%text),quote,&
            " at ",this_label%xpos,",",this_label%ypos

       ! add the optional justify element
       if (this_label%justify .ne. gp_justify_default) then
          select case(this_label%justify)
          case(gp_justify_left)
             command = trim(command)//" left "
          case(gp_justify_center)
             command = trim(command)//" center "
          case(gp_justify_right)
             command = trim(command)//" right "
          case default
             print *,"ERROR in gp_create_plot:"
             print *,"unknown justify code: ",this_label%justify
             stop 1
          end select
       end if

       ! add the optional rotate element
       if (abs(this_label%rotate) .gt. eps) then
          write(rotate_str,*) this_label%rotate
          command = trim(command)//" rotate by "//&
                    trim(adjustl(rotate_str))
       end if

       ! add the optional textcolor element
       if (this_label%textcolor .ne. gp_color_default) then
          select case (this_label%textcolor)
          case(gp_color_default)
             ! stay at the default, so do nothing
          case(gp_color_black:gp_color_yellow)
             write(clr_str,"(i6)",err=902) this_label%textcolor
             command = trim(command)//" textcolor lt "//trim(adjustl(clr_str))
          case default
             print *,"ERROR in gp_create_plot: "//&
                  "unknown textcolor number: ",this_label%textcolor
          end select
       end if

       ! this font is unknown for the png terminal on my knmi system
       if (gp%output_type .ne. output_type_png) then
          ! hardcode this fontname and fontsize for now
          command = trim(command)//" font "//quote//"Times-Roman,8"//quote
          ! example fonts:  "Times-Roman" 14
       end if

       ! register the command for exporting to the output file
       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return

       ! step to next label
       this_label => this_label%next_label

       !  #]
    end do labellist
    !  #]

    this_dataset =>gp%first_dataset
    this_3d_dataset =>gp%first_3d_dataset
    plot_cmd_datasets(:) = ' '

    !  #[ sanity check: see if there is any data
    if ( (.not. associated(this_dataset)    ).and. &
         (.not. associated(this_3d_dataset) )      ) then
       print *,"ERROR in gp_create_plot: no 2D or 3D datasets defined !"
       error_flag = error_writing_file
       return
    end if
    !  #]

    cnt=0
    do while (associated(this_dataset))
       !  #[ add all 2-D datasets
       cnt=cnt+1
       write(cnt_str,"(i6)",err=901) cnt

       ! define a linetype for each plot
       command = "set style line "//trim(adjustl(cnt_str))

       select case (this_dataset%linestyle)
       case(gp_line_default)
          ! stay at the default, so do nothing
          ! just copy default style n to type n
          command = trim(command)//" lt "//trim(adjustl(cnt_str))
       case(gp_line_full,&
            gp_line_dashed,&
            gp_line_shortdashed,&
            gp_line_dotted,&
            gp_line_dash_dot,&
            gp_line_dash_ddot)
          write(lt_str,"(i6)",err=902) this_dataset%linestyle
          command = trim(command)//" lt "//trim(adjustl(lt_str))
       case default
          print *,"ERROR in gp_create_plot: "//&
               "unknown linestyle number: ",this_dataset%linestyle
       end select

       if (gnuplot_version .ge. "4.2") then
          ! this seems only possible for gnuplot v4.2 or above!
          ! recently we seem to have gone back to version 4.0, so this is no
          ! longer implemented for the default gnuplot version ......
          ! However, installing gnuplot 4.2 in a user account is fairly
          ! easy. Ask me for details if you want to try it.

          select case (this_dataset%linecolor)
          case(gp_color_default)
             ! stay at the default, so do nothing
          case(gp_color_black:gp_color_yellow)
             write(clr_str,"(i6)",err=902) this_dataset%linecolor
             command = trim(command)//" lc "//trim(adjustl(clr_str))
          case default
             print *,"ERROR in gp_create_plot: "//&
                  "unknown linecolor number: ",this_dataset%linecolor
          end select
       end if

       select case (this_dataset%symbol)
       case(gp_symbol_default)
          ! stay at the default, so do nothing
       case(gp_symbol_dot:gp_symbol_filltriangle)
          write(smb_str,"(i6)",err=903) this_dataset%symbol
          command = trim(command)//" pt "//trim(adjustl(smb_str))
       case default
          print *,"ERROR in gp_create_plot: "//&
               "unknown symbol number: ",this_dataset%symbol
       end select

       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return

       ! decide which filename to use for temporarily storing the data
       if (len_trim(this_dataset%datafilename) .gt. 0) then
          print *,"Storing data for dataset: "//&
               trim(this_dataset%name)//" in file: "//&
               trim(this_dataset%datafilename)
          temp_data_filename = this_dataset%datafilename
          this_dataset%remove_datafile_after_use = .false.
       else
          call get_temp_filename(temp_data_filename, error_flag)
          if (error_flag .ne. no_error) return
          this_dataset%datafilename = temp_data_filename
          this_dataset%remove_datafile_after_use = .true.
       end if

       ! write data to datafile
       fileunit = get_lun()
       open(unit=fileunit,file=temp_data_filename,status="replace",&
            form="FORMATTED",action="write",err=999)
       do i=1,size(this_dataset%x)
          write(val1_str,*,err=905) this_dataset%x(i)
          write(val2_str,*,err=906) this_dataset%y(i)
          write(fileunit,*,err=998) &
               trim(adjustl(val1_str))//" "//&
               trim(adjustl(val2_str))
!               this_dataset%x(i),' ',this_dataset%y(i)
       end do
       close(unit=fileunit)
       call free_lun(fileunit)

       ! first add a comma, if a 2nd dataset is added
       if (len_trim(plot_cmd_datasets) .gt. 0) &
            plot_cmd_datasets = trim(plot_cmd_datasets)//","
       
       ! then add the code to add this new dataset to the plot
       if (len_trim(this_dataset%name) .eq. 0) then
          plot_cmd_datasets = trim(plot_cmd_datasets)//&
               " '"//trim(temp_data_filename)//"' using 1:2 notitle "//&
               " ls "//trim(adjustl(cnt_str))
       else
          plot_cmd_datasets = trim(plot_cmd_datasets)//&
               " '"//trim(temp_data_filename)//"' using 1:2 title '"//&
               trim(this_dataset%name)//"' ls "//trim(adjustl(cnt_str))
       end if

       ! note: the notitle keyword suppresses the automatic
       !       legend: /tmp/tempfile.00001144.00000001 --+--
       !       which is very confusing with these tempfiles

       ! handle the datastyle setting
       if (this_dataset%datastyle .ne. DataStyle_Default) then
          plot_cmd_datasets = trim(plot_cmd_datasets)//&
               " with "//datastyle_to_str(this_dataset%datastyle)
       end if

       ! handle next dataset
       this_dataset => this_dataset%next_dataset

       !  #]
    end do

    !  #[ sanity check: make sure 2D and 3D are not mixed
    if (associated(this_3d_dataset)) then
       if (cnt .gt. 0) then
          print *,"ERROR in gp_create_plot: combining 2D abd 3D datasets"
          print *,"in a single plot is not yet implemented"
          error_flag = error_programming
          return
       end if
    end if
    !  #]

    cnt_3d=0
    do while (associated(this_3d_dataset))
       !  #[ add all 3-D datasets
       if ( (.not. gp%ShadePlot) .and. &
            (.not. gp%ContourPlot)     ) then
          print *,"ERROR in gp_create_plot:"
          print *,"When using 3D data, the shade-plot and/or contourplot"
          print *,"switch should be set, but both seem false !!!"
          error_flag = error_programming
          return
       end if

       cnt_3d=cnt_3d+1
       write(cnt_str,"(i6)",err=904) cnt_3d

       if (cnt_3d .gt. 1) then
          print *,"ERROR drawing more than one 3D dataset has not"
          print *,"yet been implemented."
          print *,"Please use this example to do so if you need it..."
          error_flag = error_programming
          return
       end if    

       ! todo: set this for each contour seperately
       !  #[ define a linetype and linecolor for each plot
       command = "set style line "//trim(adjustl(cnt_str))

       select case (this_3d_dataset%linestyle)
       case(gp_line_default)
          ! stay at the default, so do nothing
          ! just copy default style n to type n
          command = trim(command)//" lt "//trim(adjustl(cnt_str))
       case(gp_line_full,&
            gp_line_dashed,&
            gp_line_shortdashed,&
            gp_line_dotted,&
            gp_line_dash_dot,&
            gp_line_dash_ddot)
          write(lt_str,"(i6)",err=902) this_3d_dataset%linestyle
          command = trim(command)//" lt "//trim(adjustl(lt_str))
       case default
          print *,"ERROR in gp_create_plot: "//&
               "unknown linestyle number: ",this_3d_dataset%linestyle
       end select

      if (gnuplot_version .ge. "4.2") then
          ! this seems only possible for gnuplot v4.2 or above!
          ! recently we seem to have gone back to version 4.0, so this is no
          ! longer implemented for the default gnuplot version ......
          ! However, installing gnuplot 4.2 in a user account is fairly
          ! easy. Ask me for details if you want to try it.

          select case (this_3d_dataset%linecolor)
          case(gp_color_default)
             ! stay at the default, so do nothing
          case(gp_color_black:gp_color_yellow)
             write(clr_str,"(i6)",err=902) this_3d_dataset%linecolor
             command = trim(command)//" lc "//trim(adjustl(clr_str))
          case default
             print *,"ERROR in gp_create_plot: "//&
                  "unknown linecolor number: ",this_dataset%linecolor
          end select
       end if

       call gp_add_command(gp,command,error_flag)
       if (error_flag .ne. no_error) return
       !  #]

       !  #[ store the data in a temporary file
       ! decide which filename to use for temporarily storing the data
       if (len_trim(this_3d_dataset%datafilename) .gt. 0) then
          print *,"Storing data for 3d dataset: "//&
               trim(this_3d_dataset%name)//" in file: "//&
               trim(this_3d_dataset%datafilename)
          temp_data_filename = this_3d_dataset%datafilename
          this_3d_dataset%remove_datafile_after_use = .false.
       else
          call get_temp_filename(temp_data_filename, error_flag)
          if (error_flag .ne. no_error) return
          this_3d_dataset%datafilename = temp_data_filename
          this_3d_dataset%remove_datafile_after_use = .true.
       end if

       ! write data to datafile
       fileunit = get_lun()
       open(unit=fileunit,file=temp_data_filename,status="replace",&
            form="FORMATTED",action="write",err=999)
       do i=1,size(this_3d_dataset%x)
          do j=1,size(this_3d_dataset%y)
             ! workaround: write the 3 numbers to a string and glue them
             ! together to force getting them on a single line without
             ! loss of precision (which you get when using an explicit format)
             ! Without this trick pgf90 will break the line and write 
             ! alternating lines of 2 numbers and 1 number
             ! and gnuplot doesn't like that at all....
             write(val1_str,*,err=907) this_3d_dataset%x(i)
             write(val2_str,*,err=908) this_3d_dataset%y(j)
             write(val3_str,*,err=909) this_3d_dataset%z(i,j)
             write(fileunit,*,err=998) &
                  trim(adjustl(val1_str))//" "//&
                  trim(adjustl(val2_str))//" "//&
                  trim(adjustl(val3_str))
!                  this_3d_dataset%x(i),' ',&
!                  this_3d_dataset%y(j),' ',&
!                  this_3d_dataset%z(i,j)
          end do
          ! write an empty line after each column of data
          ! this seems required for gnuplot to recognize 3d data
          write(fileunit,*,err=998) ' '
       end do
       close(unit=fileunit)
       call free_lun(fileunit)
       !  #]

       ! to allow more control on how contours are plotted
       ! (especially linetype, colors etc.) it is best to
       ! write the contour data to a temporary file first
       if (gp%ContourPlot) then
          !  #[ store contourdata in a temporary file

          ! decide which filename to use for temporarily storing the data
          if (len_trim(this_3d_dataset%contourdatafilename) .gt. 0) then
             print *,"Storing data for contours: "//&
                  trim(this_3d_dataset%name)//" in file: "//&
                  trim(this_3d_dataset%contourdatafilename)
             temp_data_filename2_fixed = this_3d_dataset%contourdatafilename
             this_3d_dataset%remove_datafile2_after_use = .false.
          else
             call get_temp_filename(temp_data_filename2_fixed, error_flag)
             if (error_flag .ne. no_error) return
             this_3d_dataset%contourdatafilename = temp_data_filename2_fixed
             this_3d_dataset%remove_datafile2_after_use = .true.
          end if

          call get_temp_filename(temp_data_filename2, error_flag)
          if (error_flag .ne. no_error) return

          call gp_init_plot(tmp_gp,error_flag)

          call gp_set_plot_property(tmp_gp,error_Flag,&
                            xrange=(/gp%x_min,gp%x_max/),&
                            yrange=(/gp%y_min,gp%y_max/),&
                            zrange=(/gp%z_min,gp%z_max/)  )

          command = "set terminal table"
          call gp_add_command(tmp_gp,command,error_flag)
          if (error_flag .ne. no_error) return

          command = "set output '"//trim(temp_data_filename2)//"'"
          call gp_add_command(tmp_gp,command,error_flag)
          if (error_flag .ne. no_error) return

          ! re-apply all range settings on this new tmp_gp struct
          call gp_apply_all_ranges(tmp_gp, error_flag)
          if (error_flag .ne. no_error) return

          command = "set contour base"
          call gp_add_command(tmp_gp,command,error_flag)
          if (error_flag .ne. no_error) return     

          command = "unset surface"
          call gp_add_command(tmp_gp,command,error_flag)
          if (error_flag .ne. no_error) return     

          ! set the levels to be used
          if (.not. associated(gp%Contour_Levels)) then
             if (gp%AddContourAtOne) then
                allocate(gp%Contour_Levels(gp%Nr_Contour_Levels+1),&
                         stat=AllocStatus)
             else
                allocate(gp%Contour_Levels(gp%Nr_Contour_Levels),&
                         stat=AllocStatus)
             end if
             if (AllocStatus .ne. 0) then
                print *,"allocation problem in gp_create_plot"
                error_flag = error_allocate
                return
             end if
             
             contour_level_step = &
                  (gp%z_max-gp%z_min)/gp%Nr_Contour_Levels
             do i=1,gp%Nr_Contour_Levels
                ! remark: contours at min or max are not visible
                ! since they will be outside/on the edge of the plot, 
                ! or consist of a single point only. So add a half
                ! step on both sides
                gp%Contour_Levels(i) = gp%z_min+contour_level_step*(-0.5_r8_+i)
             end do

             if (gp%AddContourAtOne) then
                ! shift the defined levels from 1..n to 2..n+2
                do i=gp%Nr_Contour_Levels,1,-1
                   gp%Contour_Levels(i+1) = gp%Contour_Levels(i) 
                end do
                ! fill the first contour with value one
                gp%Contour_Levels(1) = 1._r8_
                ! adapt the number of contours
                gp%Nr_Contour_Levels = gp%Nr_Contour_Levels + 1
             end if
          end if

          !print *,"gp%z_min/max         = ",gp%z_min,gp%z_max
          !print *,"gp%Contour_Levels    = ",gp%Contour_Levels
          !print *,"gp%Nr_Contour_Levels = ",gp%Nr_Contour_Levels

          long_command = "set cntrparam levels discrete "
          contour_level_value(:) = ' '
          do i=1,gp%Nr_Contour_Levels
             write(contour_level_value,*) gp%Contour_Levels(i)
             if (i .eq. 1) then
                long_command = trim(long_command)//" "//&
                     trim(adjustl(contour_level_value))
             else
                long_command = trim(long_command)//","//&
                     trim(adjustl(contour_level_value))
             end if
          end do
          if (debug) then
             print *,"Selected contour levels: "
             print *,trim(long_command)
          end if

          !command = "set cntrparam levels discrete 10,20,30,50,70,100,150,190"
          !command = "set cntrparam levels "//trim(adjustl(lvl_str))
          call gp_add_command(tmp_gp,long_command,error_flag)
          if (error_flag .ne. no_error) return     

          ! related commands:
          !set cntrparam levels 10
          !set cntrparam levels incremental -1, 0.2, 1
          !set cntrparam levels discrete -0.2, -0.5, 0.2, 0.5
          ! format for writing the text at the contour legend
          ! default is '%8.3g'
          !set clabel {'<format>'}

          command = "splot '"//trim(temp_data_filename)//"' with lines 1"
          call gp_add_command(tmp_gp,command,error_flag)
          if (error_flag .ne. no_error) return

          if (present(cmdfile_contours)) then
             temp_gnuplot_filename2 = cmdfile_contours
             print *,"NOTE: the gnuplot commands used for creating the"
             print *,"datafile that describe the contour lines in the current"
             print *,"plot have been saved as file: "//&
                  trim(cmdfile_contours)
             print *,"You may use the command 'gnuplot "//&
                  trim(cmdfile_contours)//"'"
             print *,"to recreate this file (usefull for debugging or"
             print *,"learning about gnuplot)."
          else
             call get_temp_filename(temp_gnuplot_filename2, error_flag)
             if (error_flag .ne. no_error) return
          end if

          ! write the plot commands to a temp file
          fileunit = get_lun()
          open(unit=fileunit,file=temp_gnuplot_filename2,status="replace",&
               form="FORMATTED",action="write",err=996)
          call gp_asciiwrite_cmd_list(tmp_gp,fileunit,error_flag)
          close(unit=fileunit)
          call free_lun(fileunit)
          
          system_command = trim(chararray2string(tmp_gp%gnuplot_command))//" "//&
               trim(temp_gnuplot_filename2)
          if (debug) print *,"executing: "//trim(system_command)
          call system_cmd(system_command,error_flag)
          if (error_flag .ne. no_error) return

          ! done, clean-up the module
          call gp_close_plot(tmp_gp)

          if (.not. present(cmdfile_contours)) then
             call free_temp_filename(temp_gnuplot_filename2, error_flag)
             if (error_flag .ne. no_error) return
          end if

          ! ok
          ! the problem at this point is, that when following sections 
          ! of a dataset in a multi-dataset file, have exactly the 
          ! same amount of points
          ! THEN gnuplot automatically considers them to be a mesh,
          ! and connects the sections with lines as well .....

          ! modify this file and make sure the different sections
          ! of each contour have different numbers of points
          call FixContourDatafile(temp_data_filename2,&
                                  temp_data_filename2_fixed,&
                                  error_flag)
          if (error_flag .ne. no_error) return

          ! Now it is safe to delete the un-fixed file
          call free_temp_filename(temp_data_filename2,error_flag)
          if (error_flag .ne. no_error) return

          !  #]
       end if

       if (gp%ShadePlot) then
          !  #[ add commands to create a shaded plot
          ! first add a comma, if a 2nd dataset is added
          if (len_trim(plot_cmd_datasets) .gt. 0) &
               plot_cmd_datasets = trim(plot_cmd_datasets)//", "//bs//newline

          ! then add the code to add this new dataset to the plot
          if (len_trim(this_3d_dataset%name) .eq. 0) then
             plot_cmd_datasets = trim(plot_cmd_datasets)//&
                  " '"//trim(temp_data_filename)//"' with pm3d notitle "
          else
             plot_cmd_datasets = trim(plot_cmd_datasets)//&
                  " '"//trim(temp_data_filename)//"' with pm3d title '"//&
                  trim(this_3d_dataset%name)
          end if
          !  #]
       end if

       if (gp%ContourPlot) then
          !  #[ add commands to create contour lines
          if (gp%ShadePlot .and. gp%ContourPlot) then
             ! this is a special case, since we plot data
             ! from 2 different files now. Therefore the multiplot
             ! switch has been inserted above, and an additional
             ! splot command for the contours is needed:
             long_command = "splot "//trim(plot_cmd_datasets)
             call gp_add_command(gp,long_command,error_flag)
             if (error_flag .ne. no_error) return

             plot_cmd_datasets(:) = ' '

             command = "reset"
             call gp_add_command(gp,command,error_flag)
             if (error_flag .ne. no_error) return

             command = "unset key"
             call gp_add_command(gp,command,error_flag)
             if (error_flag .ne. no_error) return

             command = "set surface"
             call gp_add_command(gp,command,error_flag)
             if (error_flag .ne. no_error) return

             command = "set view map"
             call gp_add_command(gp,command,error_flag)
             if (error_flag .ne. no_error) return

             !command = "set palette grey"
             !command = "set palette color negative"
             ! white-te-blue gradient 
             ! (color lowest value (0) is rgb=111, highest value (1) rgb=001)
             !command = "set palette defined (0 1 1 1,1 0 0 1)"
             
             ! make all lines of the contour black
             ! this seems not needed for png plotting
             ! however, it is essential when doing postscript output!
             if (gp%output_type .ne. output_type_png) then
                command = "set palette defined (0 0 0 0,1 0 0 0)"
                call gp_add_command(gp,command,error_flag)
                if (error_flag .ne. no_error) return
             end if
             
             ! this seems a problem when producing png files....
             if (gp%output_type .ne. output_type_png) then
                command = "set style line 1 lt 1 lc 0"
                call gp_add_command(gp,command,error_flag)
                if (error_flag .ne. no_error) return
             end if

             ! enforce identical ranges
             call gp_apply_all_ranges(gp, error_flag)
             if (error_flag .ne. no_error) return
             command = "set clip two"

             ! setting the exact same title and x,y,z labels is required
             ! to get a second plot with exactly the same size!!!!!!!
             call gp_apply_title_and_xyzlabels(gp, error_flag)
             if (error_flag .ne. no_error) return
          else
             ! when only contours are drawn, the legend
             ! might be inside the plot
             !command = "set key 10,88"
             !call gp_add_command(gp,command,error_flag)
             !if (error_flag .ne. no_error) return

          end if

          ! remember gnuplot starts counting levels with 0
          do i=0,gp%Nr_Contour_Levels-1
             ! first add a comma, if a 2nd dataset is added
             if (i .gt. 0) &
                  plot_cmd_datasets = trim(plot_cmd_datasets)//&
                  ", "//bs//newline

             ! REMARK: count downwards, since gnuplot starts with
             ! the largest number when writing the contours to
             ! the temporary datafile, even if the list of defined contours
             ! starts with the smallest value !
             write(lvl_str,*) gp%Nr_Contour_Levels-1-i

             ! fill the title string
             write(title_str,"(f10.3)") gp%Contour_Levels(i+1)

             plot_cmd_datasets = trim(plot_cmd_datasets)//&
                  "'"//trim(temp_data_filename2_fixed)//"' i "//&
                  trim(adjustl(lvl_str))//":"//trim(adjustl(lvl_str))//&
                  "  using 1:2:3  title '"//trim(adjustl(title_str))//&
                  "'  with lines "//&
                  " ls "//trim(adjustl(cnt_str))
          end do
          !  #]
       end if

       ! handle next dataset
       this_3d_dataset => this_3d_dataset%next_3d_dataset
       !  #]
    end do    

    if (cnt .gt. 0) then
       ! add the plot command to the list of commands
       !command = "plot '"//trim(temp_data_filename)//"' using 1:2 notitle"
       long_command = "plot "//trim(plot_cmd_datasets)
       call gp_add_command(gp,long_command,error_flag)
       if (error_flag .ne. no_error) return
    end if

    if (cnt_3d .gt. 0) then
       ! add the splot command to the list of commands
       long_command = "splot "//trim(plot_cmd_datasets)
       call gp_add_command(gp,long_command,error_flag)
       if (error_flag .ne. no_error) return
    end if

    ! write the plot commands to a temp file
    if (present(cmdfile)) then
       temp_gnuplot_filename = cmdfile
       print *,"NOTE: the gnuplot commands used for creating the current"
       print *,"plot have been saved as file: "//trim(cmdfile)
       print *,"You may use the command 'gnuplot "//trim(cmdfile)//"'"
       print *,"to recreate the plot (usefull for debugging or"
       print *,"learning about gnuplot)."
   else
       call get_temp_filename(temp_gnuplot_filename, error_flag)
       if (error_flag .ne. no_error) return
    end if

    fileunit = get_lun()
    open(unit=fileunit,file=temp_gnuplot_filename,status="replace",&
         form="FORMATTED",action="write",err=997)
    call gp_asciiwrite_cmd_list(gp,fileunit,error_flag)
    close(unit=fileunit)
    call free_lun(fileunit)

    ! execute gnuplot
    ! shell commands needed:
    !gnuplot < /tmp/tempfile.0000004873.0000000002 > tmp.ps
    !gv tmp.ps

    system_command = trim(chararray2string(gp%gnuplot_command))//&
         " "//trim(temp_gnuplot_filename)
    if (debug) print *,"executing: "//trim(system_command)
    call system_cmd(system_command,error_flag)
    if (error_flag .ne. no_error) return

    if (display_result) then
       system_command = trim(chararray2string(gp%display_command))//&
            " "//trim(output_file)
       if (debug) print *,"executing: "//trim(system_command)
       call system_cmd(system_command,error_flag)
       if (error_flag .ne. no_error) return
    else
       print *,""
       print *,"Created plot is stored in file: "//trim(output_file)
       print *,""
    end if
    
    ! ok, all work is done now, so all temporary data- and
    ! command-files can be deleted
    this_dataset =>gp%first_dataset
    do while (associated(this_dataset))
       if (this_dataset%remove_datafile_after_use) then
          call free_temp_filename(this_dataset%datafilename,&
                                  error_flag)
          if (error_flag .ne. no_error) return
       end if
       this_dataset => this_dataset%next_dataset
    end do

    this_3d_dataset =>gp%first_3d_dataset
    do while (associated(this_3d_dataset))
       if (this_3d_dataset%remove_datafile_after_use) then
          call free_temp_filename(this_3d_dataset%datafilename,&
                                  error_flag)
          if (error_flag .ne. no_error) return
       end if
       if (this_3d_dataset%remove_datafile2_after_use) then
          call free_temp_filename(this_3d_dataset%contourdatafilename,&
                                  error_flag)
          if (error_flag .ne. no_error) return
       end if

       this_3d_dataset => this_3d_dataset%next_3d_dataset
    end do

    if (.not. present(cmdfile)) then
       call free_temp_filename(temp_gnuplot_filename, error_flag)
       if (error_flag .ne. no_error) return
    end if

    return

    !  #[ error handlers
901 print *,"Error in gp_create_plot:"
    print *,"Could not convert cnt to a proper string"
    error_flag = error_writing_file
    return

902 print *,"Error in gp_create_plot:"
    print *,"Could not convert linecolor to a proper string"
    error_flag = error_writing_file
    return

903 print *,"Error in gp_create_plot:"
    print *,"Could not convert symbol to a proper string"
    error_flag = error_writing_file
    return

904 print *,"Error in gp_create_plot:"
    print *,"Could not convert cnt_3d to a proper string"
    error_flag = error_writing_file
    return

905 print *,"Error in gp_create_plot:"
    print *,"Could not convert this_dataset%x(i) to a proper string"
    error_flag = error_writing_file
    return

906 print *,"Error in gp_create_plot:"
    print *,"Could not convert this_dataset%y(i) to a proper string"
    error_flag = error_writing_file
    return

907 print *,"Error in gp_create_plot:"
    print *,"Could not convert this_3d_dataset%x(i) to a proper string"
    error_flag = error_writing_file
    return

908 print *,"Error in gp_create_plot:"
    print *,"Could not convert this_3d_dataset%y(i) to a proper string"
    error_flag = error_writing_file
    return

909 print *,"Error in gp_create_plot:"
    print *,"Could not convert this_3d_dataset%z(i,j) to a proper string"
    error_flag = error_writing_file
    return

996 print *,"Error in gp_create_plot:"
    print *,"Could not open temporary file for writing commands: "//&
         trim(temp_gnuplot_filename2)
    error_flag = error_opening_file
    return

997 print *,"Error in gp_create_plot:"
    print *,"Could not open temporary file for writing commands: "//&
         trim(temp_gnuplot_filename)
    error_flag = error_opening_file
    return

998 print *,"Error in gp_create_plot:"
    print *,"Could not write to temporary file for data: "//&
         trim(temp_data_filename)
    error_flag = error_writing_file
    return

999 print *,"Error in gp_create_plot:"
    print *,"Could not open temporary file for writing data: "//&
         trim(temp_data_filename)
    error_flag = error_opening_file
    return
    !  #]

  end subroutine gp_create_plot
    !  #]
  !------------------------------------
end module gnuplot_module
