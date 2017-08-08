module tempfile_handling
  !  #[ documentation
  !
  ! a module to provide a unique temporary filename
  ! after each call.
  !
  ! written by: J. de Kloe, KNMI
  !
  ! Modifications:
  !   05-Mar-2008 added to genscat (taken from aeolus project)
  !   13-Oct-2008 J. de Kloe  remove a typo
  !   26-Jan-2009 J. de Kloe  added check for existence of the tempfile
  !
  !  #]
  !  #[ modules used
  use c_support, only: get_process_id, remove_file, get_filesize, is_dir
  use errorhandler, only: no_error, error_writing_file
  !  #]
  !  #[ variables and parameters
  implicit none
  character(len=*), parameter :: tempdir1 = "/tmp"
  character(len=*), parameter :: tempdir2 = "/temp"
  integer,          parameter :: max_num_tries = 99
  ! also the typical ecgate use of $SCRATH env setting
  ! to indicate a location for a temporary directory
  ! could be added here
  character(len=256), save :: tempdir
  integer, save :: tmpfile_count=0
  logical, save :: first_call = .true.
  !  #]
contains
  !-----------------------------------
  subroutine InitTempFileHandling()
    !  #[
    
    ! check for the presence of the possible temp directories
    ! to see which is available for use at this machine

    ! to check:
    ! /tmp
    ! $SCRATCH
    ! $TMPDIR

    !print *,"inside: InitTempFileHandling"
    tempdir=tempdir1
    if (is_dir(tempdir)) return
    tempdir=tempdir2
    if (is_dir(tempdir)) return

    print *,"ERROR in InitTempFileHandling:"
    print *,"The implemented temp directories do not exist"
    print *,"please modify the tempfile_handling.F90 module"
    stop 1

  end subroutine InitTempFileHandling
    !  #]
  subroutine get_temp_filename(temp_filename, error_flag)
    !  #[
    character(len=256), intent(out) :: temp_filename
    integer,            intent(out) :: error_flag

    ! local variables
    integer           :: pid_nr, ios, count_tries, size
    character(len=10) :: pid_str
    character(len=10) :: tmpfile_count_str

    error_flag = no_error
    temp_filename(:) = ' '

    ! get the process ID number to construct a unique set of filenames
    ! for the temporary files for the current process.
    ! then use an internal counter inside this module
    ! to make sure each following name is unique.

    if (first_call) then
       call InitTempFileHandling()
       first_call = .false.
    end if

    pid_nr = get_process_id()
    write(pid_str,"(i10.10)",iostat=ios) pid_nr
    IF (ios .ne. 0) THEN
       print *,"ERROR in get_temp_filename(): "
       print *,"could not convert pid nr ",pid_nr," to a proper string ..."
       error_flag = error_writing_file
       return
    END IF

    count_tries = 0
    tryloop: do while (count_tries .lt. max_num_tries)
       tmpfile_count = tmpfile_count+1
       count_tries   = count_tries+1
       write(tmpfile_count_str,"(i10.10)",iostat=ios) tmpfile_count
       IF (ios .ne. 0) THEN
          print *,"ERROR in get_temp_filename(): "
          print *,"could not convert tmpfile_count ",tmpfile_count,&
               " to a proper string ..."
          error_flag = error_writing_file
          return
       END IF

       ! construct the temporary filename
       temp_filename = trim(tempdir)//"/tempfile."//pid_str//&
                                      "."//tmpfile_count_str

       ! see if this file already exists, if so take another name
       ! the filesize should be reported as -1 if the file does not exist
       size = get_filesize(temp_filename)
       if (size .lt. 0) exit tryloop
    end do tryloop

    ! check if we really found a filename that does not yet exist
    ! the filesize should be reported as -1 if the file does not exist
    size = get_filesize(temp_filename)
    if (size .ge. 0) then
       print *,"ERROR in get_temp_filename(): "
       print *,"could not find a filename to be used for the temporary file"
       print *,"even after trying ",max_num_tries," times"
       print *,"Last name tried was: "//trim(temp_filename)
       print *,"Please check the temp_filehandling module ..."
       error_flag = error_writing_file
       return
       
    end if

  end subroutine get_temp_filename
    !  #]
  subroutine free_temp_filename(temp_filename, error_flag)
    !  #[
    character(len=256), intent(in)  :: temp_filename
    integer,            intent(out) :: error_flag

    ! local variable
    integer :: filesize

    error_flag = no_error

    filesize = get_filesize(temp_filename)
    if (filesize .ge.0) then
       call remove_file(temp_filename, error_flag)
    else
       print *,"no tempfile present with name: ",trim(temp_filename)
    end if

  end subroutine free_temp_filename
    !  #]
  !-----------------------------------
end module tempfile_handling
 
