module ErrorHandler
  !  #[ Description
  !     a module to define parameters for the error conditions
  !     in a program and its modules, and some functions
  !     to print information on the nature of the error
  !     and to gracefully abort the code
  !
  !     written by: J. de Kloe, KNMI
  !
  !     Modifications:
  !     19-Apr-2006 J. de Kloe  added errortype error_diff_tool_syntax_error
  !     27-Jul-2006 J. de Kloe  added errors for c_support
  !     19-Sep-2006 J. de Kloe  added error_program_usage error type
  !     24-Nov-2006 J. de Kloe  added error_numerics_charsize
  !     13-Dec-2006 J. de Kloe  added error_numerics_hexval
  !     19-Dec-2007 P. Poli     added routine catch_error
  !     26-Sep-2008 J. de Kloe  added error_numerics_NaN_detected and
  !                             error_numerics_Inf_detected
  !     15-Jan-2009 J. de Kloe  added error_not_yet_implemented
  !     19-Feb-2009 J. de Kloe  added error_bufr_interface
  !     01-Apr-2009 J. de Kloe  add some BUFR specific error messages
  !  #]
  !  #[ parameters
  implicit none

  ! these are not really errors, but conditions that
  ! may be signalled using the error_flag as well
  integer, parameter :: end_of_file_reached     = -2
  integer, parameter :: request_program_to_stop = -1

  ! all seems ok
  integer, parameter :: no_error                = 0

  ! general error codes
  integer, parameter :: error_opening_file        = 1
  integer, parameter :: error_closing_file        = 2
  integer, parameter :: error_reading_file        = 3
  integer, parameter :: error_writing_file        = 4
  integer, parameter :: error_allocate            = 5
  integer, parameter :: error_deallocate          = 6
  integer, parameter :: error_programming         = 7
  integer, parameter :: error_cmdline_options     = 8
  integer, parameter :: error_program_usage       = 9
  integer, parameter :: error_not_yet_implemented = 10
 
  ! error codes used by specific support modules       ! used in which module:
  integer, parameter :: error_invalid_date  = 50001 ! datetime module
  integer, parameter :: error_invalid_time  = 50002 ! datetime module
  integer, parameter :: error_xml           = 50101 ! ee_xml module
  integer, parameter :: error_bufr_encode   = 50201 ! bufr module
  integer, parameter :: error_bufr_decode   = 50202 ! bufr module
  integer, parameter :: error_bufr_corr_msg = 50203 ! bufr module
  integer, parameter :: error_bufr_interface= 50204 ! bufr module
  integer, parameter :: error_bufr_bufren   = 50210 ! bufr module
  integer, parameter :: error_bufr_bufrex   = 50211 ! bufr module
  integer, parameter :: error_bufr_busel    = 50212 ! bufr module
  integer, parameter :: error_bufr_bus012   = 50213 ! bufr module
  integer, parameter :: error_bufr_buxdes   = 50214 ! bufr module
  integer, parameter :: error_bufr_bupkey   = 50215 ! bufr module
  ! warning: these parameters should have the same value as the
  ! ones defined in the module numerics.F90.
  ! the numerics module can not USE this ErrorHandler module, however,
  ! because that would introduce a circular dependency
  ! (and a huge number of changes in other programs/modules, since
  ! almost every program/module that I make uses the numerics module
  ! as a standalone module without further dependencies)
  integer, parameter :: error_numerics_intsize      = 50301 ! numerics module
  integer, parameter :: error_numerics_realsize     = 50311 ! numerics module
  integer, parameter :: error_numerics_charsize     = 50321 ! numerics module
  integer, parameter :: error_numerics_hexval       = 50331 ! numerics module
  integer, parameter :: error_numerics_NaN_detected = 50341 ! numerics module
  integer, parameter :: error_numerics_Inf_detected = 50351 ! numerics module
  integer, parameter :: error_c_interface       = 50401 ! c_support module
  integer, parameter :: error_string_handling   = 50501 ! stringtools module
  integer, parameter :: error_netcdf_lib        = 50601 ! netcdf module

  ! error code used by the difftool
  integer, parameter :: error_diff_tool_syntax_error = 60001

  ! error code used by the mpi wrapper
  integer, parameter :: error_mpi = 70001

  ! save settings to be used throughout the program
  logical, save :: do_abort_on_error = .true.

  !  #]
contains
  !-----------------------------------
  subroutine InitErrorHandler(abort_on_error)
    !  #[
    logical, optional, intent(in) :: abort_on_error

    IF (present(abort_on_error)) do_abort_on_error = abort_on_error

    IF (do_abort_on_error) THEN
       print *,"The Error Handler program_abort routine is set to "
       print *,"abort on first error..."
    ELSE
       print *,"The Error Handler program_abort routine is set to "
       print *,"return after each error,"
       print *,"in order to try and resume the program..."
    END IF

  end subroutine InitErrorHandler
    !  #]
  subroutine report_error(error_flag,subroutine_name)
    !  #[
    integer,          intent(in) :: error_flag
    character(len=*), intent(in) :: subroutine_name

    ! if needed a fileunit could be added to the interface of this
    ! routine, which would enable the choice between stdout and a file. 
    ! (this was used in the error reporting module of the bufr module)

    IF (error_flag .gt. 0) &
         print *,"an error was reported from within subroutine: ",&
         trim(subroutine_name)
    select case(error_flag)
    case(end_of_file_reached)
       print *,"End-of-file reached"
    case(request_program_to_stop)
       !print *,"the program will be stopped"
    case(no_error)
       print *,"there was no error ..."
       !  #[ general error codes
    case(error_opening_file)
       print *,"error while opening a file"
    case(error_closing_file)
       print *,"error while closing a file"
    case(error_reading_file)
       print *,"error while reading data from a file"
    case(error_writing_file)
       print *,"error while writing data to a file"
    case(error_allocate)
       print *,"error while allocating memory"
    case(error_deallocate)
       print *,"error while deallocating memory"
    case(error_programming)
       print *,"probably a programming error"
    case(error_cmdline_options)
       print *,"error while parsing commandline options"
    case(error_program_usage)
       print *,"error incorrect use of this propgram (check your settings!)"
    case(error_not_yet_implemented)
       print *,"error; this feature is not yet implemented"
       !  #]
       !  #[ error codes used by the datetime module
    case(error_invalid_date)
       print *,"a date was detected to have an invalid/impossible value"
    case(error_invalid_time)
       print *,"a time was detected to have an invalid/impossible value"
       !  #]
       !  #[ error codes used by the ee_xml module
    case(error_xml)
       print *,"an error was reported by the ee_cfi_software"
       !  #]
       !  #[ error codes used by the bufr module
    case(error_bufr_encode)
       print *,"error while encoding a bufr message"
    case(error_bufr_decode)
       print *,"error while decoding a bufr message"
    case(error_bufr_corr_msg)
       print *,"error BUFR message seems corrupt"
    case(error_bufr_interface)
       print *,"error BUFR fortran and c interfaces do not match"
    case(error_bufr_bufren)
       print *,"error reported by BUFREN library subroutine"
    case(error_bufr_bufrex)
       print *,"error reported by BUFREX library subroutine"
    case(error_bufr_busel)
       print *,"error reported by BUSEL library subroutine"
    case(error_bufr_bus012)
       print *,"error reported by BUS012 library subroutine"
    case(error_bufr_buxdes)
       print *,"error reported by BUXDES library subroutine"
    case(error_bufr_bupkey)
       print *,"error reported by BUPKEY library subroutine"
       !  #]
       !  #[ error codes used by the numerics module
    case(error_numerics_intsize)
       print *,"an error was reported by the numerics module:"
       print *,"integer size differs from what is expected"
    case(error_numerics_realsize)
       print *,"an error was reported by the numerics module:"
       print *,"real size differs from what is expected"
    case(error_numerics_charsize)
       print *,"an error was reported by the numerics module:"
       print *,"character size differs from what is expected"
    case(error_numerics_hexval)
       print *,"an error was reported by the numerics module:"
       print *,"hexadecimal to integer conversion differs"
       print *,"from what is expected"
    case(error_numerics_NaN_detected)
       print *,"an error was reported by the numerics module:"
       print *,"a real variable was found to contain the NaN value."
       print *,"Usually this points at a programming error..."
    case(error_numerics_Inf_detected)
       print *,"an error was reported by the numerics module:"
       print *,"a real variable was found to contain the Inf value."
       print *,"Usually this points at a programming error..."
       !  #]
       !  #[ error codes used by the c_support module
    case(error_c_interface)
       print *,"an error was reported by the c_support module:"
       print *,"c and fortran code cannot be properly interfaced"
       print *,"with the available datatypes in the"
       print *,"current c and fortran objects..."
       !  #]
       !  #[ error codes used by the stringtools module
    case(error_string_handling)
       print *,"an error was reported within the genscat stringtools module."
       !  #]
       !  #[ error codes used by the netcdf module
    case(error_netcdf_lib)
       print *,"an error was reported by the genscat netcdf wrapper module."
       !  #]
       !  #[ error codes used by the difftool
    case(error_diff_tool_syntax_error)
       print *,"a syntax error was reported by the difftool"
       !  #]
       !  #[ error codes used by the mpi wrapper
    case(error_mpi)
       print *,"an error was reported from within the MPI library"
       !  #]
    case default
       print *,"this error code was not yet defined: ",error_flag
    end select

  end subroutine report_error
    !  #]
  subroutine program_abort(error_flag,subroutine_name)
    !  #[
    integer,          intent(in) :: error_flag
    character(len=*), intent(in) :: subroutine_name

    call report_error(error_flag,subroutine_name)

    IF (error_flag .eq. request_program_to_stop) stop

    IF (do_abort_on_error) THEN
       stop 1
    ELSE
       print *,"==> trying to resume the program ..."
       return
    END IF

  end subroutine program_abort
    !  #]
  subroutine catch_error(error_flag,subroutine_name)
    !  #[
    integer,          intent(in) :: error_flag
    character(len=*), intent(in) :: subroutine_name

    IF (error_flag .ne. no_error) call program_abort(error_flag,subroutine_name)

  end subroutine catch_error
    !  #]
 !-----------------------------------
end module ErrorHandler
