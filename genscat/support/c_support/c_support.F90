module c_support
  !  #[ documentation
  !-------------------------------------------
  ! some routines for easier access to routines
  ! from the standard clib library, that are not
  ! easily available for fortran90
  !
  ! Modifications:
  !   03-Apr-2006 J. de Kloe added wrappers for gethostname and get_process_id
  !   27-Jul-2006 J. de Kloe added wrappers for the remove() function
  !   22-Nov-2006 J. de Kloe added function convert_string_c_to_fortran
  !   10-Oct-2007 J. de Kloe added a few missing intents
  !   16-Jan-2008 J. de Kloe phase out integer kind i_ 
  !   08-Jan-2009 J. de Kloe added is_dir() function
  !   23-Jan-2009 J. de Kloe added system_cmd() subroutine
  !   20-Mar-2009 J. de Kloe adapted to changed get_size_of_long function
  !
  !-------------------------------------------
  !  #]
  !  #[ use statements
  USE numerics, only: i2_, i4_, i8_, nbytes_i4_, nbytes_i8_, &
                      r4_, r8_, nbytes_r4_, nbytes_r8_, &
                      missing_indicator_real_r8
  USE ErrorHandler, only: error_programming, error_c_interface, no_error
  !  #]
  !  #[ variables and parameters 

  implicit none

  ! see the manpage of stat for a description of these elements 
  integer, parameter :: stat_dev     = 1  ! device
  integer, parameter :: stat_ino     = 2  ! inode
  integer, parameter :: stat_mode    = 3  ! protection
  integer, parameter :: stat_nlink   = 4  ! number of hard links
  integer, parameter :: stat_uid     = 5  ! user ID of owner
  integer, parameter :: stat_gid     = 6  ! group ID of owner
  integer, parameter :: stat_rdev    = 7  ! device type (if inode device)
  integer, parameter :: stat_size    = 8  ! total size, in bytes
  integer, parameter :: stat_blksize = 9  ! blocksize for filesystem I/O
  integer, parameter :: stat_blocks  = 10 ! number of blocks allocated
  integer, parameter :: stat_atime   = 11 ! time of last access
  integer, parameter :: stat_mtime   = 12 ! time of last modification
  integer, parameter :: stat_ctime   = 13 ! time of last change
  !  #]
  !  #[ interface statements to external c functions
  ! this replaces the old "external" statement
  ! and provides the compiler a way of checking the interface !!
  interface 
     ! a version that is used when sizeof(long)=4
     function get_stat_result_c4(filename, stat_result) result(err)
       USE numerics, only: i4_
       character(len=*),            intent(in)  :: filename
       integer(i4_), dimension(13), intent(out) :: stat_result
       integer(i4_)                             :: err
     end function get_stat_result_c4
     ! a version that is used when sizeof(long)=8
     function get_stat_result_c8(filename, stat_result) result(err)
       USE numerics, only: i8_
       character(len=*),            intent(in)  :: filename
       integer(i8_), dimension(13), intent(out) :: stat_result
       integer(i8_)                             :: err
     end function get_stat_result_c8
     ! a function to request sizeof(long)
     function get_size_of_long() result(size)
       USE numerics, only:i2_
       integer(i2_) :: size
     end function get_size_of_long
     ! a function to request sizeof(double)
     function get_size_of_double() result(size)
       USE numerics, only:i2_
       integer(i2_) :: size
     end function get_size_of_double
     function gethostname_c4(hostname) result(err)
       USE numerics, only: i4_
       character(len=*), intent(out) :: hostname
       integer(i4_)                  :: err
     end function gethostname_c4
     function gethostname_c8(hostname) result(err)
       USE numerics, only: i8_
       character(len=*), intent(out) :: hostname
       integer(i8_)                  :: err
     end function gethostname_c8
     function get_pid_c4() result(pid)
       USE numerics, only: i4_
       integer(i4_) :: pid
     end function get_pid_c4
     function get_pid_c8() result(pid)
       USE numerics, only: i8_
       integer(i8_) :: pid
     end function get_pid_c8
     function remove_file_c4(filename) result(err)
       USE numerics, only: i4_
       character(len=*), intent(in) :: filename
       integer(i4_)                 :: err
     end function remove_file_c4
     function remove_file_c8(filename) result(err)
       USE numerics, only: i8_
       character(len=*), intent(in) :: filename
       integer(i8_)                 :: err
     end function remove_file_c8
     function is_dir_c4(dirname) result(isdir)
       USE numerics, only: i4_
       character(len=*), intent(in) :: dirname
       integer(i4_)                 :: isdir       
     end function is_dir_c4
     function is_dir_c8(dirname) result(isdir)
       USE numerics, only: i8_
       character(len=*), intent(in) :: dirname
       integer(i8_)                 :: isdir       
     end function is_dir_c8
     function system_c4(command) result(err)
       USE numerics, only: i4_
       character(len=*), intent(in) :: command
       integer(i4_)                 :: err
     end function system_c4
     function system_c8(command) result(err)
       USE numerics, only: i8_
       character(len=*), intent(in) :: command
       integer(i8_)                 :: err
     end function system_c8
     function cclock_r4() result(t)
       USE numerics, only: r4_
       real(r4_) :: t
     end function cclock_r4
     function cclock_r8() result(t)
       USE numerics, only: r8_
       real(r8_) :: t
     end function cclock_r8
  end interface
  !  #]
  !  #[ overload some functions with multiple input interfaces
  ! overload the get_filesize function
  interface get_filesize
     module procedure get_filesize_u, get_filesize_f
  end interface
  !  #]
contains
  !------------------------------------------
  function get_filesize_u(fileunit) result(size)
    !  #[ get the filesize
    integer, intent(in) :: fileunit ! input
    integer             :: size     ! result

    ! local variables
    logical            :: nmd
    character(len=256) :: fname

    ! this trick with inquire to find out the filename is necessary
    ! because the c-fileunits differ from the fortran-fileunits,
    ! so the c-function fstat() cannot directly handle fortran
    ! fileunits as input.

    inquire(unit=fileunit,named=nmd)
    if (nmd) then
       ! init the remainder of the string with spaces
       fname(:) = ' '

       inquire(unit=fileunit,name=fname)

       size = get_filesize_f(fname)
       !print *,"inside c_support.F90[get_filesize_u]: "
       !print *,"the size of this file is ",size," in bytes"
       !print *,"the name of this file is ",trim(fname)
    else
       print *,"Error in get_filesize_u():"
       print *,"The fileunit: ",fileunit," seems not to belong to a"
       print *,"named file. Therefore the wrapper to the stat() function"
       print *,"can not be called...."
       print *,"If this is needed please implement it yourself."
       size = -1 ! report -1 to indicate the error
       return
    end if

    return

  end function get_filesize_u
    !  #]
  function get_filesize_f(filename) result(size)
    !  #[ get the filesize 
    character(len=*), intent(in) :: filename ! input
    integer                      :: size     ! result

    ! local variables
    integer(i2_)                :: longsize
    integer(i4_)                :: err
    integer(i4_)                :: err4
    integer(i8_)                :: err8
    integer(i4_), dimension(13) :: stat_result
    integer(i4_), dimension(13) :: stat_result4
    integer(i8_), dimension(13) :: stat_result8
    character(len=256)          :: filename256

    ! init the string with spaces
    filename256(:)=' '

    ! Beware: the interface with c expects a stringlength
    !         of exactly 256 ! Therefore we have to copy the
    !         implicit length of filename to 256 by copying
    !         it to filename256 !!!
    IF (len_trim(filename) .gt. 256) THEN
       ! print *,"filename too long ....."
       size = -1
       return
    END IF
    filename256(:)=filename

    longsize = get_size_of_long()
    !print *,"inside F90: longsize = ",longsize

    IF (longsize .eq. nbytes_i4_) THEN
       ! call the c-wrapper function for stat()
       err4 = get_stat_result_c4(filename256, stat_result4)
       err = err4
       stat_result(:) = stat_result4(:)
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       ! call the c-wrapper function for stat()
       err8 = get_stat_result_c8(filename256, stat_result8)
       err = int(err8,i4_)
       stat_result(:) = int(stat_result8(:),i4_)
    ELSE
       print *,"ERROR in get_filesize_f():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       size = -1
       return
    END IF

    if (err .ne. 0) then
       !print *,"Error in c_support.F90[get_filesize_f]"
       !print *,"get_fstat_result_c() returned with err = ",err
       size = -1
       return
    end if

    !print *,"inside c_support.F90[get_filesize_f] stat_result = ",stat_result
    size = stat_result(stat_size)

    return

  end function get_filesize_f
    !  #]
  function get_hostname() result(hostname)
    !  #[ get this machines hostname
    character(len=25) :: hostname ! result

    ! local variables
    integer(i2_)      :: longsize
    integer(i4_)      :: err
    integer(i4_)      :: err4
    integer(i8_)      :: err8
    character(len=25) :: hostname_c

    longsize = get_size_of_long()
    IF (longsize .eq. nbytes_i4_) THEN
       err4 = gethostname_c4(hostname_c)
       err = err4
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       err8 = gethostname_c8(hostname_c)
       err = int(err8,i4_)
    ELSE
       print *,"ERROR in get_hostname():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       hostname = "unknown"
       return
    END IF

    if (err .ne. 0) then
       hostname = "unknown"
       return
    end if

    hostname = convert_string_c_to_fortran(hostname_c)

    return

  end function get_hostname
  !  #]
  function get_process_id() result(pid)
    !  #[ get the current pid number
    integer :: pid ! result

    ! local variables
    integer(i2_)      :: longsize
    integer(i4_)      :: pid4
    integer(i8_)      :: pid8

    longsize = get_size_of_long()
    IF (longsize .eq. nbytes_i4_) THEN
       pid4 = get_pid_c4()
       pid = pid4
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       pid8 = get_pid_c8()
       pid = int(pid8,i4_)
    ELSE
       print *,"ERROR in get_process_id():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       pid = -1
       return
    END IF

    return

  end function get_process_id
  !  #]
  subroutine remove_file(filename,error_flag)
    !  #[ remove a file
    character(len=*), intent(in)  :: filename   ! input
    integer,          intent(out) :: error_flag ! output

    ! local variables
    integer(i2_)       :: longsize
    integer(i4_)       :: err4,err
    integer(i8_)       :: err8
    character(len=256) :: filename256

    ! NOTE: an alternative way might be to use the fortran command
    !    close(unit=fileunit,status='delete',err=999)
    ! but this requires opening the file first.
    ! I only found out about this fortran90 option after implementing
    ! this c-workaround, so lets keep it as it is for now.

    error_flag = no_error

    ! init the string with spaces
    filename256(:)=' '

    ! Beware: the interface with c expects a stringlength
    !         of exactly 256 ! Therefore we have to copy the
    !         implicit length of filename to 256 by copying
    !         it to filename256 !!!
    IF (len_trim(filename) .gt. 256) THEN
       print *,"Error in remove_file():"
       print *,"filename too long ..... (max length is 256 chars)"
       error_flag = error_programming
       return
    END IF
    filename256 = filename

    longsize = get_size_of_long()
    IF (longsize .eq. nbytes_i4_) THEN
       err4 = remove_file_c4(filename256)
       err  = err4
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       err8 = remove_file_c8(filename256)
       err = int(err8,i4_)
    ELSE
       print *,"ERROR in remove_file():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       error_flag = error_c_interface
       return
    END IF

    IF (err .ne. 0) THEN
       print *,"ERROR in remove_file():"
       print *,"could not remove file: ",trim(filename)
       print *,"reported c error is: ",err
       ! TODO
       ! later I could add explanations for the possible error codes
       ! that may be returned by the c function
       error_flag = error_programming
       return
    END IF

    !print *,"succesfully removed file: ",trim(filename)

  end subroutine remove_file
    !  #]
  function convert_string_c_to_fortran(txt_c) result(txt)
    !  #[ helper function used by get_hostname

    ! remember the a c-style string is terminated with
    ! a zero, and may possibly have a bunch of undefined chars following it
    ! This function converts it to a proper Fortran90 string
    character(len=*)          :: txt_c ! input
    character(len=len(txt_c)) :: txt   ! result

    ! local variable
    integer                   :: i

    txt(:) = ' '
    copyloop: DO i=1,len(txt_c)
       IF (ichar(txt_c(i:i)) .eq. 0) exit copyloop
       txt(i:i) = txt_c(i:i)
    END DO copyloop

  end function convert_string_c_to_fortran
    !  #]
  function is_dir(dirname) result(flag)
    !  #[ check if a dir with this name exists
    character(len=*), intent(in) :: dirname ! input
    logical                      :: flag    ! result

    ! local variables
    integer(i2_)       :: longsize
    integer(i4_)       :: isdir
    integer(i4_)       :: isdir4
    integer(i8_)       :: isdir8
    character(len=256) :: dirname256

    ! init the string with spaces
    dirname256(:)=' '

    ! Beware: the interface with c expects a stringlength
    !         of exactly 256 ! Therefore we have to copy the
    !         implicit length of filename to 256 by copying
    !         it to filename256 !!!
    IF (len_trim(dirname) .gt. 256) THEN
       ! print *,"dirname too long ....."
       flag = .false.
       return
    END IF
    dirname256(:)=dirname
 
    longsize = get_size_of_long()
    !print *,"inside F90: longsize = ",longsize

    IF (longsize .eq. nbytes_i4_) THEN
       ! call the c-wrapper function for is_dir()
       isdir4 = is_dir_c4(dirname256)
       isdir = isdir4
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       ! call the c-wrapper function for is_dir()
       isdir8 = is_dir_c8(dirname256)
       isdir = int(isdir8,i4_)
    ELSE
       print *,"ERROR in is_dir():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       flag=.false.
       return
    END IF

    ! remark: is_dir_c4/is_dir_c8 return a value of 1 when the name
    !         is a proper directory, a value of 0 when it exists
    !         but is not a directory, and a value of -1 when it does not exist

    flag = .false.
    if (isdir .gt. 0) flag = .true.

    return

  end function is_dir
    !  #]
  subroutine system_cmd(command,error_flag)
    !  #[ execute a shell command
    character(len=*), intent(in)  :: command    ! input
    integer,          intent(out) :: error_flag ! output

    ! local variables
    integer(i2_)       :: longsize
    integer(i4_)       :: err4,err
    integer(i8_)       :: err8
    character(len=256) :: command256

    error_flag = no_error

    ! init the string with spaces
    command256(:)=' '

    ! Beware: the interface with c expects a stringlength
    !         of exactly 256 ! Therefore we have to copy the
    !         implicit length of command to 256 by copying
    !         it to command256 !!!
    IF (len_trim(command) .gt. 256) THEN
       print *,"Error in system_cmd():"
       print *,"command too long ..... (max length is 256 chars)"
       error_flag = error_programming
       return
    END IF
    command256 = command

    longsize = get_size_of_long()
    IF (longsize .eq. nbytes_i4_) THEN
       err4 = system_c4(command256)
       err  = err4
    ELSEIF (longsize .eq. nbytes_i8_) THEN
       err8 = system_c8(command256)
       err = int(err8,i4_)
    ELSE
       print *,"ERROR in system_cmd():"
       print *,"The interface between c and Fortran90 expects the long integer"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",longsize
       error_flag = error_c_interface
       return
    END IF

    IF (err .ne. 0) THEN
       print *,"ERROR in system_cmd():"
       print *,"failed command: ",trim(command)
       print *,"reported c error is: ",err
       error_flag = error_programming
       return
    END IF

    !print *,"succesfully executed command: ",trim(command)

  end subroutine system_cmd
    !  #]
  function wclock() result(result_wclock)
    !  #[ get wall-clock time

    ! --- Get wallclock time from system function 'gettimeofday' via routine
    !     'cclock'.
    !
    ! Remark: this routine relies on the fact that the double type in
    ! c usually is an 8-byte variable type. From old remarks by 
    ! A. van der Steen it appears that on some old convex machines this was
    ! not the case, and a 4-byte variable was used in stead.
    ! Therefore I request the size of the double type before actually
    ! calling the cclock function.
    
    real(r8_)    :: result_wclock    ! result

    ! local variables
    real(r4_)    :: result_wclock_r4
    real(r8_)    :: result_wclock_r8
    integer(i2_) :: doublesize

    doublesize = get_size_of_double()

    result_wclock = missing_indicator_real_r8
    if (doublesize .eq. nbytes_r4_) then
       result_wclock_r4 = cclock_r4()
       result_wclock = result_wclock_r4
    elseif (doublesize .eq. nbytes_r8_) then
       result_wclock_r8 = cclock_r8()
       result_wclock = result_wclock_r8
    else
       print *,"ERROR in wclock():"
       print *,"The interface between c and Fortran90 expects the double"
       print *,"type in c to hold either 4 or 8 bytes, but in stead the"
       print *,"current machine seems to use: ",doublesize," bytes"
       return
    end if
 
  end function wclock
    !  #]
  !------------------------------------------
end module c_support
