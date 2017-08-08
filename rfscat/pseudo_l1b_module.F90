module pseudo_l1b_module
  !---------------------------------------------------
  !  #[ Documentation
  !
  ! a module to read a pseudo L1B file as defined for the old
  ! rfscat project.
  ! These files contain the satellite and antenna geometry of
  ! the scatterometer to be simulated.
  !
  ! Written 2000,2001,2002 by Jos de Kloe, KNMI.
  !    
  ! Modifications;
  ! 07-May-2009 J. de Kloe  phase out all CPP definitions
  ! 12-May-2009 J. de Kloe  a number of smaller clean-ups
  ! 15-May-2009 J. de Kloe  moved all code from geometry.F90 to this
  !                         new module, except the definitions and
  !                         routines to handle view_type and wvc_type
  !
  !  #]
  !---------------------------------------------------
  !  #[ Modules used

  USE geometry, only: view_type, init_view

  ! a module to define inversion subroutine and datatypes
  USE inversion, only: undefined_pol, c_vv_pol, c_hh_pol, c_po_pol, &
       c_vh_pol, ku_vv_pol, ku_hh_pol, ku_vh_pol, ku_po_pol, no_pol, &
       c_vv_dop, c_hh_dop, &
       max_nr_of_views, LUT_min_theta, LUT_max_theta, LUT_theta_step

  USE lunmanager, only: get_lun, free_lun
  USE numerics, only: r4_, missing_indicator_real_r4, missing_indicator_integer
  USE errorhandler, only: no_error, error_opening_file, &
       error_reading_file, error_allocate, error_programming, &
       error_program_usage
  !  #]
  !  #[ Variables and types
  IMPLICIT NONE  ! no implicit variable typing
  public         ! make all variables public by default

  TYPE instrument_def_type
     integer :: nr_of_nodes ! minimum number of all open pseudo_l1b files
     integer :: last_used_node
     type(pseudo_l1b_data_type), pointer :: pseudo_l1b_data_1
     type(pseudo_l1b_data_type), pointer :: pseudo_l1b_data_2
     type(pseudo_l1b_data_type), pointer :: pseudo_l1b_data_3
     integer :: struct_was_properly_initialised
  END TYPE instrument_def_type

  integer, parameter :: pseudo_l1b_data_canary = 123789456

  ! define the datastructure used to hold the geometry
  TYPE pseudo_l1b_data_type
     integer :: nr_noise_samples
     integer :: nr_of_nodeblocks
     integer :: nr_of_node_columns
     integer :: nr_of_node_rows
     type(pl1b_node_data_type), dimension(:), pointer :: node
  END TYPE pseudo_l1b_data_type

  TYPE pl1b_node_data_type
     integer   :: row_number
     integer   :: column_number
     real(r4_) :: lattitude
     real(r4_) :: longitude
     integer   :: nr_of_views
     type(pl1b_view_data_type), dimension(:), pointer :: view
  END TYPE pl1b_node_data_type

  TYPE pl1b_view_data_type
     real(r4_) :: time_offset
     integer   :: asc_desc_flag
     real(r4_) :: azimuth
     real(r4_) :: inc_angle
     real(r4_) :: nr_samples
     real(r4_) :: snr_prime
     integer   :: polarisation
  END TYPE pl1b_view_data_type

  ! define possible values for asc_desc_flag
  integer, parameter :: asc_desc_flag_undefined  = -1
  integer, parameter :: asc_desc_flag_ascending  =  0
  integer, parameter :: asc_desc_flag_descending =  1

  !  #]
  !---------------------------------------------------
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  subroutine init_instrument_def(instrument_def)
    !  #[

    type(instrument_def_type), intent(out) :: instrument_def

    instrument_def%nr_of_nodes    = -1
    instrument_def%last_used_node = -1
    nullify(instrument_def%pseudo_l1b_data_1)
    nullify(instrument_def%pseudo_l1b_data_2)
    nullify(instrument_def%pseudo_l1b_data_3)
    instrument_def%struct_was_properly_initialised = pseudo_l1b_data_canary

  end subroutine init_instrument_def

    !  #]
  subroutine delete_instrument_def(instrument_def,error_flag)
    !  #[
    type(instrument_def_type), intent(inout) :: instrument_def
    integer,                   intent(out)   :: error_flag

    ! init
    error_flag = no_error

    ! sanity check
    if (instrument_def%struct_was_properly_initialised .ne. pseudo_l1b_data_canary) then
       print *,"ERROR in delete_instrument_def:"
       print *,"instrument_def structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    if (associated(instrument_def%pseudo_l1b_data_1)) then
       call delete_pseudo_l1b_data(instrument_def%pseudo_l1b_data_1)
       deallocate(instrument_def%pseudo_l1b_data_1)
    end if
    if (associated(instrument_def%pseudo_l1b_data_2)) then
       call delete_pseudo_l1b_data(instrument_def%pseudo_l1b_data_2)
       deallocate(instrument_def%pseudo_l1b_data_2)
    end if
    if (associated(instrument_def%pseudo_l1b_data_3)) then
       call delete_pseudo_l1b_data(instrument_def%pseudo_l1b_data_3)
       deallocate(instrument_def%pseudo_l1b_data_3)
    end if

  end subroutine delete_instrument_def
    !  #]
  !--------------------------------
  subroutine init_pseudo_l1b_data(pseudo_l1b_data)
    !  #[
    type(pseudo_l1b_data_type), intent(out) :: pseudo_l1b_data

    pseudo_l1b_data%nr_noise_samples   = -1
    pseudo_l1b_data%nr_of_nodeblocks   = -1
    pseudo_l1b_data%nr_of_node_columns = -1
    pseudo_l1b_data%nr_of_node_rows    = -1
    nullify(pseudo_l1b_data%node)

  end subroutine init_pseudo_l1b_data
    !  #]
  subroutine delete_pseudo_l1b_data(pseudo_l1b_data)
    !  #[
    type(pseudo_l1b_data_type), intent(inout) :: pseudo_l1b_data

    ! local variable
    integer :: i

    if (associated(pseudo_l1b_data%node)) then
       do i=1,size(pseudo_l1b_data%node)
          call delete_pl1b_node_data(pseudo_l1b_data%node(i))
       end do
       deallocate(pseudo_l1b_data%node)
    end if

  end subroutine delete_pseudo_l1b_data
    !  #]
  !--------------------------------
  subroutine init_pl1b_node_data(pl1b_node_data)
    !  #[
    type(pl1b_node_data_type), intent(out) :: pl1b_node_data

    pl1b_node_data%row_number    = -1
    pl1b_node_data%column_number = -1
    pl1b_node_data%lattitude     = missing_indicator_real_r4
    pl1b_node_data%longitude     = missing_indicator_real_r4
    pl1b_node_data%nr_of_views   = -1
    nullify(pl1b_node_data%view)
    
  end subroutine init_pl1b_node_data
    !  #]
  subroutine delete_pl1b_node_data(pl1b_node_data)
    !  #[
    type(pl1b_node_data_type), intent(inout) :: pl1b_node_data

    if (associated(pl1b_node_data%view)) then
       deallocate(pl1b_node_data%view)
    end if

  end subroutine delete_pl1b_node_data
    !  #]
  !--------------------------------
  subroutine init_pl1b_view_data(pl1b_view_data)
    !  #[
    type(pl1b_view_data_type), intent(out) :: pl1b_view_data

    pl1b_view_data%time_offset   = missing_indicator_real_r4
    pl1b_view_data%asc_desc_flag = asc_desc_flag_undefined
    pl1b_view_data%azimuth       = missing_indicator_real_r4
    pl1b_view_data%inc_angle     = missing_indicator_real_r4
    pl1b_view_data%nr_samples    = missing_indicator_real_r4
    pl1b_view_data%snr_prime     = missing_indicator_real_r4
    pl1b_view_data%polarisation  = undefined_pol

  end subroutine init_pl1b_view_data
    !  #]
  ! note: a delete_pl1b_view_data subroutine is not needed since
  ! this datatype does not contain any pointers or allocatable arrays
  !--------------------------------
  subroutine load_pseudoL1b_file(pl1b_filename,pseudo_l1b_data,error_flag)
    !  #[
    character(len=*),           intent(in)    :: pl1b_filename   ! input
    ! this next structure will hold all data read from this file
    type(pseudo_l1b_data_type), intent(inout) :: pseudo_l1b_data ! in/output
    integer,                    intent(out)   :: error_flag      ! output

    ! local variables
    logical   :: exists
    integer   :: fileunit, allocstatus, i, n, v
    real(r4_) :: fileversion
    integer   :: nr_of_lines, nr_of_nodeblocks, nr_noise_samples
    character(len=256) :: dummyline, dataline

    ! vars for node-data block
    integer  :: row_number, column_number, nr_of_views
    real(r4_) :: lattitude, longitude
    real(r4_) :: x1,y1,z1,x2,y2,z2 ! sat. heading vector components

    ! vars for view-data block
    integer  :: view_counter
    real(r4_) :: time_offset, azimuth, inc_angle, nr_samples, snr_prime
    character(len=1) :: asc_desc_flag
    character(len=2) :: polarisation
    integer :: asc_desc_code, polarisation_code

    ! warning counters
    integer, save :: num_warnings_view_counter1
    integer, save :: num_warnings_view_counter2
    integer, save :: num_warnings_azimuth
    integer, save :: num_warnings_inc_angle
    integer, save :: num_warnings_nr_samples
    integer, save :: num_warnings_snr_prime

    integer, parameter :: max_warnings_view_counter1 = 10
    integer, parameter :: max_warnings_view_counter2 = 10
    integer, parameter :: max_warnings_azimuth = 10
    integer, parameter :: max_warnings_inc_angle = 10
    integer, parameter :: max_warnings_nr_samples = 10
    integer, parameter :: max_warnings_snr_prime = 10

    error_flag = no_error

    call init_pseudo_l1b_data(pseudo_l1b_data)

    ! open the PSEUDO_L1b file
    !print *,"opening file: ",trim(pl1b_filename)

    inquire(file=trim(pl1b_filename) ,exist=exists)
    IF (.not. exists) THEN
       print *,"ERROR in load_pseudoL1b_file:"
       print *,'File: ',trim(pl1b_filename),' not found !!!!!'
       error_flag = error_opening_file
       return
    END IF

    fileunit = get_lun()
    open(unit=fileunit,file=trim(pl1b_filename),&
         status='old',form="FORMATTED",ERR=888)

    ! read the General Data Block

     ! # Pseudo_L1B File version
    call read_one_real_number(fileunit,fileversion,error_flag)
    if (error_flag .ne. no_error) return

    ! # Number of lines of the general data block
    call read_one_integer_number(fileunit,nr_of_lines,error_flag)
    if (error_flag .ne. no_error) return

    ! Number of lines of a node data block
    read(fileunit,"(a)",err=999) dummyline
    
    ! # Number of node data blocks in this file
    call read_one_integer_number(fileunit,nr_of_nodeblocks,error_flag)
    if (error_flag .ne. no_error) return

    ! Number of lines of a view data block
    read(fileunit,*,err=999) dummyline
    ! Number of view data blocks
    read(fileunit,*,err=999) dummyline
    ! Time of file creation in seconds
    read(fileunit,*,err=999) dummyline

    ! # Number of noise samples
    call read_one_integer_number(fileunit,nr_noise_samples,error_flag)
    if (error_flag .ne. no_error) return

    IF (fileversion .ne. 1.0) THEN
       print *,"ERROR in load_pseudoL1b_file:"
       print *,'This pseudoL1b files seems to be a newer version'
       print *,'than can be read by this program. Please use a'
       print *,'new program version, or a compatible pseudoL1b file.'
       print *,'Expected fileversion number: 1.00'
       print *,'Found    fileversion number: ',fileversion
       error_flag = error_reading_file
       return
    END IF

    IF (nr_of_nodeblocks .lt. 1) THEN
       print *,"ERROR in load_pseudoL1b_file:"
       print *,"ERROR: nr_of_nodeblocks = ",nr_of_nodeblocks
       print *,"ERROR: this should by > 0 !"
       error_flag = error_reading_file
       return
    END IF

    IF (nr_noise_samples .lt. 1) THEN
       print *,"ERROR in load_pseudoL1b_file:"
       print *,"ERROR: nr_noise_samples = ",nr_noise_samples
       print *,"ERROR: this should by > 0 !"
       error_flag = error_reading_file
       return
    END IF

    !print *,'assuming: nr_noise_samples = 4000'
    !    nr_noise_samples = 4000

    ! discard all other lines in the General Data Block
    DO i=9,nr_of_lines
      read(fileunit,"(a)",err=999) dummyline
      !print *,"Ignoring header line: ",dummyline
    END DO

    !print *,'nr_noise_samples = ',nr_noise_samples,&
    !   '     nr_of_nodeblocks = ',nr_of_nodeblocks

    pseudo_l1b_data%nr_noise_samples = nr_noise_samples
    pseudo_l1b_data%nr_of_nodeblocks = nr_of_nodeblocks

    ! allocate room to store the node data
    allocate(pseudo_l1b_data%node(nr_of_nodeblocks),stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       error_flag = error_allocate
       return
    end if

    node_loop: do n=1,nr_of_nodeblocks
       ! read one node-data block

       read(fileunit,"(a)") dummyline ! read the empty line

       read(fileunit,"(a)",err=998,end=997) dataline

       read(dataline,*,err=996,end=995) row_number, column_number, &
           lattitude, longitude, x1,y1,z1,x2,y2,z2, nr_of_views

       ! store the data in the pseudo_l1b_data_type
       pseudo_l1b_data%node(n)%row_number    = row_number
       pseudo_l1b_data%node(n)%column_number = column_number
       pseudo_l1b_data%node(n)%lattitude     = lattitude
       pseudo_l1b_data%node(n)%longitude     = longitude
       pseudo_l1b_data%node(n)%nr_of_views   = nr_of_views
       ! note: satellite heading x1,y1,z2,x2,y2,z2 is currently
       ! not stored. Seems not usefull for our purposes.

       nullify(pseudo_l1b_data%node(n)%view)
       if (nr_of_views .gt. 0) then
          allocate(pseudo_l1b_data%node(n)%view(nr_of_views),stat=AllocStatus)
          if (AllocStatus .ne. 0) then
             error_flag = error_allocate
             return
          end if
       end if

       view_loop: DO v=1,nr_of_views
          ! read one view data block

          read(fileunit,"(a)",err=994,end=993) dataline

          read(dataline,*,err=992,end=991) &
               view_counter, time_offset, asc_desc_flag, &
               azimuth, inc_angle, nr_samples, snr_prime, polarisation
          
          ! check validity for some of the just read numbers 
          IF (view_counter .lt. 0) THEN
             num_warnings_view_counter1 = num_warnings_view_counter1 + 1
             if (num_warnings_view_counter1 .le. max_warnings_view_counter1) then
                print *,"WARNING: invalid value for view_counter: ",view_counter
             end if
             if (num_warnings_view_counter1 .eq. max_warnings_view_counter1) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF
          IF (view_counter .ne. v-1) THEN
             num_warnings_view_counter2 = num_warnings_view_counter2 + 1
             if (num_warnings_view_counter2 .le. max_warnings_view_counter2) then
                print *,"WARNING: unexpected value for view_counter: ",view_counter
                print *,"Looking at the file structure the number: ",v-1," was expected"
                print *,"maybe this file is corrupt ??"
             end if
             if (num_warnings_view_counter2 .eq. max_warnings_view_counter2) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF

          IF ((azimuth .lt. -360.0) .or. (azimuth .gt. 360.0)) THEN
             num_warnings_azimuth = num_warnings_azimuth + 1
             if (num_warnings_azimuth .le. max_warnings_azimuth) then
                print *,"WARNING: invalid value for azimuth: ",azimuth
             end if
             if (num_warnings_azimuth .eq. max_warnings_azimuth) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF
          
          IF ((inc_angle .lt. LUT_min_theta-LUT_theta_step) .or. &
               (inc_angle .gt. LUT_max_theta+LUT_theta_step))THEN
             num_warnings_inc_angle = num_warnings_inc_angle + 1
             if (num_warnings_inc_angle .le. max_warnings_inc_angle) then
                print *,"WARNING: invalid value for inc_angle: ",inc_angle,&
                     "(value is outside LUT)"
             end if             
             if (num_warnings_inc_angle .eq. max_warnings_inc_angle) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF

          IF (nr_samples .lt. 0) THEN
             num_warnings_nr_samples = num_warnings_nr_samples + 1
             if (num_warnings_nr_samples .le. max_warnings_nr_samples) then
                print *,"WARNING: invalid value for nr_samples: ",nr_samples
             end if
             if (num_warnings_nr_samples .eq. max_warnings_nr_samples) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF

          ! NOTE: the recent ASF test pseudo L1B files have this number in dB
          ! so then negative numbers can be expected !
          IF (snr_prime .le. 0) THEN
             num_warnings_snr_prime = num_warnings_snr_prime + 1
             if (num_warnings_snr_prime .le. max_warnings_snr_prime) then
                print *,"WARNING: invalid value for snr_prime: ",snr_prime
             end if
             if (num_warnings_snr_prime .eq. max_warnings_snr_prime) then
                print *,"==>Number of warnings passed maximum, "
                print *,"==>suppressing any following warnings of this type"
             end if
          END IF

          call get_asc_desc_code(asc_desc_flag,asc_desc_code,error_flag)
          if (error_flag .ne. no_error) return

          call get_polarisation_code(polarisation,polarisation_code,error_flag)
          if (error_flag .ne. no_error) return

!print *,"WARNING: temporary hack: converting snr_prime from dB to linear"
!! temporary hack: convert snr_prime from dB to linear
!snr_prime = 10.**(0.1*snr_prime) ! or use: dB2real() from the numerics module
!! temporary hack: take the reciprocal of snr_prime, since that seems
!! to be what was used in the old PL1B files
!snr_prime = 1./snr_prime

          ! store the data in the pl1b_view_type
          pseudo_l1b_data%node(n)%view(v)%time_offset   = time_offset
          pseudo_l1b_data%node(n)%view(v)%asc_desc_flag = asc_desc_code
          pseudo_l1b_data%node(n)%view(v)%azimuth       = azimuth
          pseudo_l1b_data%node(n)%view(v)%inc_angle     = inc_angle
          pseudo_l1b_data%node(n)%view(v)%nr_samples    = nr_samples
          pseudo_l1b_data%node(n)%view(v)%snr_prime     = snr_prime
          pseudo_l1b_data%node(n)%view(v)%polarisation  = polarisation_code
       END DO view_loop
    END DO node_loop

    pseudo_l1b_data%nr_of_node_columns = maxval(pseudo_l1b_data%node(:)%row_number)
    pseudo_l1b_data%nr_of_node_rows    = maxval(pseudo_l1b_data%node(:)%column_number)

    close(unit=fileunit)
    CALL free_lun(fileunit)

    return

    ! error handlers
888 print *,"ERROR in load_pseudoL1b_file:"
    print *,'ERROR while opening input file:',trim(pl1b_filename)
    error_flag = error_opening_file
    return

991 print *,"ERROR in load_pseudoL1b_file:"
    print *,"end of string found when extracting:"
    print *,"while extracting view ",v," of node ",n
    print *," from the dataline: ["//trim(dataline)//"]"
    ! some dummy prints to prevent "variable is set but never used" warnings by g95
    error_flag = error_reading_file
    return

992 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while extracting view ",v," of node ",n
    print *,"from the dataline: ["//trim(dataline)//"]"
    ! some dummy prints to prevent "variable is set but never used" warnings by g95
    print *,"x1,y1,z1,x2,y2,z2 = ",x1,y1,z1,x2,y2,z2
    error_flag = error_reading_file
    return

993 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading view ",v," of node ",n," from input file:",&
         trim(pl1b_filename)
    print *,"unexpected end of file"
    error_flag = error_reading_file
    return

994 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading view ",v," of node ",n," from input file:",&
         trim(pl1b_filename)
    error_flag = error_reading_file
    return

995 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading node ",n," from input file:",trim(pl1b_filename)
    print *,"end of string found when extracting:"
    print *,"lattitude, longitude, x1,y1,z1,x2,y2,z2, nr_of_views"
    print *,"from the dataline read from this file."
    print *,"dataline = ["//trim(dataline)//"]"
    error_flag = error_reading_file
    return

996 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading node ",n," from input file:",trim(pl1b_filename)
    print *,"could not extract: lattitude, longitude, x1,y1,z1,x2,y2,z2, nr_of_views"
    print *,"from the dataline read from this file."
    print *,"dataline = ["//trim(dataline)//"]"
    error_flag = error_reading_file
    return

997 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading node ",n," from input file:",trim(pl1b_filename)
    print *,"unexpected end of file"
    error_flag = error_reading_file
    return

998 print *,"ERROR in load_pseudoL1b_file:"
    print *,"while reading node ",n," from input file:",trim(pl1b_filename)
    error_flag = error_reading_file
    return

999 print *,"ERROR in load_pseudoL1b_file:"
    print *,'while opening input file:',trim(pl1b_filename)
    ! some dummy prints to prevent "variable is set but never used" warnings by g95
    print *,"dummyline = ",dummyline
    error_flag = error_reading_file
    return
    
  end subroutine load_pseudoL1b_file
    !  #]
  subroutine read_one_real_number(fileunit,value,error_flag)
    !  #[
    integer,   intent(in)  :: fileunit   ! input
    real(r4_), intent(out) :: value      ! output
    integer,   intent(out) :: error_flag ! output

    ! local variables
    character(len=256) :: dataline, cleaned_line
    integer :: i

    ! init
    error_flag = no_error
    value = missing_indicator_real_r4

    ! read the data as a character string
    read(fileunit,"(a)",err=999,end=998) dataline

    cleaned_line = dataline

    ! this line contains no valid data
    if (dataline(1:1) .eq. '#') return

    ! search for a '#' character that indicates a comment
    search_comment: do i=2,len_trim(dataline)
       if (dataline(i:i) .eq. '#') then
          cleaned_line = dataline(1:i-1)
          exit search_comment
       end if
    end do search_comment

    ! extract the actual value
    read(cleaned_line,*,err=997,end=996) value

    return

996 print *,"ERROR in read_one_real_number: unexpected end of string reached"
    print *,"while extracting real value:"
    print *,"dataline = ["//trim(dataline)//"]"
    print *,"cleaned_line = ["//trim(cleaned_line)//"]"
    error_flag = error_reading_file
    return

997 print *,"ERROR in read_one_real_number: could not extract real value"
    print *,"from string:"
    print *,"dataline = ["//trim(dataline)//"]"
    print *,"cleaned_line = ["//trim(cleaned_line)//"]"
    error_flag = error_reading_file
    return

998 print *,"ERROR in read_one_real_number: unexpected EOF reached"
    error_flag = error_reading_file
    return

999 print *,"ERROR in read_one_real_number: could not read line of data"
    error_flag = error_reading_file
    return

  end subroutine read_one_real_number
    !  #]
  subroutine read_one_integer_number(fileunit,value,error_flag)
    !  #[
    integer, intent(in)  :: fileunit   ! input
    integer, intent(out) :: value      ! output
    integer, intent(out) :: error_flag ! output

    ! local variables
    character(len=256) :: dataline, cleaned_line
    integer :: i

    ! init
    error_flag = no_error
    value = missing_indicator_integer

    ! read the data as a character string
    read(fileunit,"(a)",err=999,end=998) dataline

    cleaned_line = dataline

    ! this line contains no valid data
    if (dataline(1:1) .eq. '#') return

    ! search for a '#' character that indicates a comment
    search_comment: do i=2,len_trim(dataline)
       if (dataline(i:i) .eq. '#') then
          cleaned_line = dataline(1:i-1)
          exit search_comment
       end if
    end do search_comment

    ! extract the actual value
    read(cleaned_line,*,err=997,end=996) value

    return

996 print *,"ERROR in read_one_integer_number: unexpected end of string reached"
    print *,"while extracting real value:"
    print *,"dataline = ["//trim(dataline)//"]"
    print *,"cleaned_line = ["//trim(cleaned_line)//"]"
    error_flag = error_reading_file
    return

997 print *,"ERROR in read_one_integer_number: could not extract real value"
    print *,"from string:"
    print *,"dataline = ["//trim(dataline)//"]"
    print *,"cleaned_line = ["//trim(cleaned_line)//"]"
    error_flag = error_reading_file
    return

998 print *,"ERROR in read_one_integer_number: unexpected EOF reached"
    error_flag = error_reading_file
    return

999 print *,"ERROR in read_one_integer_number: could not read line of data"
    error_flag = error_reading_file
    return

  end subroutine read_one_integer_number
    !  #]
  !--------------------------------
  subroutine load_instrument_def_from_pl1b(instrument_def,pl1b_filename,error_flag)
    !  #[
    type(instrument_def_type), intent(inout) :: instrument_def ! input/output
    character(len=*),          intent(in)    :: pl1b_filename  ! input
    integer,                   intent(out)   :: error_flag     ! output

    ! init
    error_flag = no_error

    ! sanity check
    if (instrument_def%struct_was_properly_initialised .ne. pseudo_l1b_data_canary) then
       print *,"ERROR in load_instrument_def_from_pl1b:"
       print *,"instrument_def structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    ! make sure the next node to be used is the first one
    instrument_def%last_used_node = 0

    if (.not. associated(instrument_def%pseudo_l1b_data_1)) then
       allocate(instrument_def%pseudo_l1b_data_1)
       call init_pseudo_l1b_data(instrument_def%pseudo_l1b_data_1)
       call load_pseudoL1b_file(pl1b_filename,&
                                instrument_def%pseudo_l1b_data_1,error_flag)
       return
    end if
    
    if (.not. associated(instrument_def%pseudo_l1b_data_2)) then
       allocate(instrument_def%pseudo_l1b_data_2)
       call init_pseudo_l1b_data(instrument_def%pseudo_l1b_data_2)
       call load_pseudoL1b_file(pl1b_filename,&
                                instrument_def%pseudo_l1b_data_2,error_flag)
       return
    end if
    
    if (.not. associated(instrument_def%pseudo_l1b_data_3)) then
       allocate(instrument_def%pseudo_l1b_data_3)
       call init_pseudo_l1b_data(instrument_def%pseudo_l1b_data_3)
       call load_pseudoL1b_file(pl1b_filename,&
                                instrument_def%pseudo_l1b_data_3,error_flag)
       return
    end if

    ! this point should not be reached!

    print *,"ERROR in load_instrument_def_from_pl1b:"
    print *,"Using more than 3 Pseudo L1B files at the same time"
    print *,"is currently not implemented."
    error_flag = error_program_usage
    return

  end subroutine load_instrument_def_from_pl1b
    !  #]
  subroutine get_nr_nodes(instrument_def,nr_nodes,error_flag)
    !  #[
    type(instrument_def_type), intent(inout) :: instrument_def ! input
    integer,                   intent(out)   :: nr_nodes       ! output
    integer,                   intent(out)   :: error_flag     ! output

    ! init
    error_flag = no_error
    nr_nodes   = -1

    ! sanity check
    if (instrument_def%struct_was_properly_initialised .ne. pseudo_l1b_data_canary) then
       print *,"ERROR in get_nr_nodes:"
       print *,"instrument_def structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    if (associated(instrument_def%pseudo_l1b_data_1)) then
       nr_nodes = instrument_def%pseudo_l1b_data_1%nr_of_nodeblocks
    end if

    if (associated(instrument_def%pseudo_l1b_data_2)) then
       if (nr_nodes .gt. instrument_def%pseudo_l1b_data_2%nr_of_nodeblocks) then
          ! reduce the nr of nodes to make sure all simulated nodes
          ! are defined in both 1st and 2nd pseudo L1B file
          nr_nodes = instrument_def%pseudo_l1b_data_2%nr_of_nodeblocks
       end if
    end if

    if (associated(instrument_def%pseudo_l1b_data_3)) then
       if (nr_nodes .gt. instrument_def%pseudo_l1b_data_3%nr_of_nodeblocks) then
          ! reduce the nr of nodes to make sure all simulated nodes
          ! are defined in the 1st, 2nd and 3rd pseudo L1B file
          nr_nodes = instrument_def%pseudo_l1b_data_3%nr_of_nodeblocks
       end if
    end if

  end subroutine get_nr_nodes
    !  #]
  subroutine get_next_wvc_instr_def(instrument_def,lat,lon,nr_of_views,&
                 node_nr,wvc_column_number,wvc_row_number,view,error_flag)
    !  #[

    type(instrument_def_type), intent(inout) :: instrument_def    ! input/output
    real(r4_),                 intent(out)   :: lat               ! output
    real(r4_),                 intent(out)   :: lon               ! output
    integer,                   intent(out)   :: nr_of_views       ! output
    integer,                   intent(out)   :: node_nr           ! output
    integer,                   intent(out)   :: wvc_column_number ! output
    integer,                   intent(out)   :: wvc_row_number    ! output
    type(view_type), dimension(:), pointer   :: view              ! output
    integer,                   intent(out)   :: error_flag        ! output

    ! local variables
    integer :: nr_nodes, AllocStatus, nr_noise_samples, v
    integer :: nr_of_views1, nr_of_views2, nr_of_views3, actual_view_nr

    ! init
    error_flag = no_error

    ! sanity check
    if (instrument_def%struct_was_properly_initialised .ne. pseudo_l1b_data_canary) then
       print *,"ERROR in get_next_wvc_instr_def:"
       print *,"instrument_def structure must be initialised properly before use!"
       error_flag = error_programming
       return
    end if

    ! decide which node should be used
    node_nr = instrument_def%last_used_node + 1
    call get_nr_nodes(instrument_def,nr_nodes,error_flag)
    if (error_flag .ne. no_error) return
    ! wrap around
    if (node_nr .gt.  nr_nodes) node_nr = 1
    ! save for the next call
    instrument_def%last_used_node = node_nr

    ! use the data from the 1st Pseudo_L1B file, except for view related items
    lat               = instrument_def%pseudo_l1b_data_1%node(node_nr)%lattitude
    lon               = instrument_def%pseudo_l1b_data_1%node(node_nr)%longitude
    wvc_column_number = instrument_def%pseudo_l1b_data_1%node(node_nr)%row_number
    wvc_row_number    = instrument_def%pseudo_l1b_data_1%node(node_nr)%column_number

    nr_of_views1 = instrument_def%pseudo_l1b_data_1%node(node_nr)%nr_of_views
    nr_of_views2 = 0
    if (associated(instrument_def%pseudo_l1b_data_2)) &
         nr_of_views2 = instrument_def%pseudo_l1b_data_2%node(node_nr)%nr_of_views
    nr_of_views3 = 0
    if (associated(instrument_def%pseudo_l1b_data_3)) &
         nr_of_views3 = instrument_def%pseudo_l1b_data_3%node(node_nr)%nr_of_views

    nr_of_views = nr_of_views1 + nr_of_views2 + nr_of_views3
    allocate(view(nr_of_views),stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"Allocation ERROR in get_next_wvc_instr_def"
       error_flag = error_allocate
       return
    end if

    init_loop: do v=1,nr_of_views
       call init_view(view(v))
    end do init_loop

    actual_view_nr = 0
    views1_loop: do v=1,nr_of_views1
       actual_view_nr = actual_view_nr + 1
       nr_noise_samples = instrument_def%pseudo_l1b_data_1%nr_noise_samples
       call copy_pl1b_view_to_wvc_view(&
                instrument_def%pseudo_l1b_data_1%node(node_nr)%view(v),&
                view(actual_view_nr),  nr_noise_samples)
    end do views1_loop

    if (associated(instrument_def%pseudo_l1b_data_2)) then
       views2_loop: do v=1,nr_of_views2
          actual_view_nr = actual_view_nr + 1
          nr_noise_samples = instrument_def%pseudo_l1b_data_2%nr_noise_samples
          call copy_pl1b_view_to_wvc_view(&
               instrument_def%pseudo_l1b_data_2%node(node_nr)%view(v),&
               view(actual_view_nr), nr_noise_samples)
       end do views2_loop
    end if

    if (associated(instrument_def%pseudo_l1b_data_3)) then
       views3_loop: do v=1,nr_of_views3
          actual_view_nr = actual_view_nr + 1
          nr_noise_samples = instrument_def%pseudo_l1b_data_3%nr_noise_samples
          call copy_pl1b_view_to_wvc_view(&
               instrument_def%pseudo_l1b_data_3%node(node_nr)%view(v),&
               view(actual_view_nr), nr_noise_samples)
       end do views3_loop
    end if

    nr_of_views = actual_view_nr
    IF ( (nr_of_views .lt. 0) .or. (nr_of_views .gt. max_nr_of_views)) THEN
       print *,"ERROR in load_pseudoL1b_file:"
       print *,"ERROR: nr_of_views = ",nr_of_views
       print *,"ERROR: this should be in the range 0..",max_nr_of_views
       error_flag = error_reading_file
       return
    END IF

  end subroutine get_next_wvc_instr_def

    !  #]
  subroutine copy_pl1b_view_to_wvc_view(pl1b_view, wvc_view, nr_noise_samples)
    !  #[
    type(pl1b_view_data_type), intent(in)  :: pl1b_view        ! input
    type(view_type),           intent(out) :: wvc_view         ! output
    integer,                   intent(in)  :: nr_noise_samples ! input

    wvc_view%antenna_dir      = pl1b_view%azimuth
    wvc_view%theta            = pl1b_view%inc_angle
    wvc_view%nr_noise_samples = nr_noise_samples
    wvc_view%nr_samples       = pl1b_view%nr_samples
    wvc_view%snr_prime        = pl1b_view%snr_prime
    wvc_view%polarisation     = pl1b_view%polarisation

    ! these 2 are calculated inside calc_nrcs_values and
    ! dont need to be filled here: sigma_0, sigma_0_vh

    ! these 3 are filled inside add_instr_and_geo_noise_to_nrcs and
    ! dont need to be filled here: kp_a, kp_b, kp_c

  end subroutine copy_pl1b_view_to_wvc_view
    !  #]
  !--------------------------------
  subroutine get_asc_desc_code(asc_desc_flag,asc_desc_code,error_flag)
    !  #[
    character(len=1), intent(in)  :: asc_desc_flag
    integer,          intent(out) :: asc_desc_code
    integer,          intent(out) :: error_flag

    asc_desc_code = asc_desc_flag_undefined
    select case(asc_desc_flag)
    case('a')
       asc_desc_code = asc_desc_flag_ascending
    case('d')
       asc_desc_code = asc_desc_flag_descending
    case default
       print *,"ERROR in get_asc_desc_code: asc_desc_flag ",&
            asc_desc_flag," is undefined"
       error_flag = error_reading_file
       return
    end select

  end subroutine get_asc_desc_code
    !  #]
  subroutine get_polarisation_code(polarisation_txt,polarisation_code,error_flag)
    !  #[
    character(len=2), intent(in)  :: polarisation_txt  ! input
    integer,          intent(out) :: polarisation_code ! result
    integer,          intent(out) :: error_flag        ! output

    ! init
    error_flag = no_error
    polarisation_code = undefined_pol

    ! polarisation should contain the string CV,CH,CP,KV,KH or KP
    ! note that the constants c_vv_pol etc. are defined in
    ! the inversion module
    SELECT CASE(polarisation_txt)
    CASE('VV') ! kept for compatibility (refers to c-band)
       polarisation_code = c_vv_pol
    CASE('HH') ! kept for compatibility (refers to c-band)
       polarisation_code = c_hh_pol
    CASE('PO') ! kept for compatibility (refers to c-band)
       polarisation_code = c_po_pol
    CASE('CV')
       polarisation_code = c_vv_pol
    CASE('CH')
       polarisation_code = c_hh_pol
    CASE('CC') ! cross-polarisation
       polarisation_code = c_vh_pol
    CASE('CP')
       polarisation_code = c_po_pol
    CASE('VD')
       polarisation_code = c_vv_dop
    CASE('HD')
       polarisation_code = c_hh_dop
    CASE('KV')
       polarisation_code = ku_vv_pol
    CASE('KH')
       polarisation_code = ku_hh_pol
    CASE('KC') ! cross-polarisation
       polarisation_code = ku_vh_pol
    CASE('KP')
       polarisation_code = ku_po_pol
    CASE DEFAULT
       print *,'ERROR in get_polarisation_code: polarisation type: ',&
            polarisation_txt,' is undefined'
       error_flag = error_reading_file
       return
    END SELECT

  end subroutine get_polarisation_code
    !  #]
  !--------------------------------
end module pseudo_l1b_module
