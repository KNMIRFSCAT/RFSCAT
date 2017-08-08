module ReadUnformatted
  !  #[ Description
  !---------------------------------------------------
  ! a module to read unformatted data with some extra checks to ensure
  ! no problems due to little/big-endianness occur
  !---------------------------------------------------
  !    Written by:  Jos de Kloe.
  !    created:     07-02-2005 
  !    last change: 01-04-2005
  !---------------------------------------------------
  !  #]
  !  #[ USED modules
  USE numerics, only: r4_,i4_
  USE LunManager, only: get_lun, free_lun
  !  #]
  !  #[ Variables and parameters
  IMPLICIT NONE
  integer, public, parameter :: VerbosityError  = 0  ! error     verbosity
  integer, public, parameter :: VerbosityWarn   = 1  ! warning   verbosity
  integer, public, parameter :: VerbosityReport = 2  ! reporting verbosity
  ! set default verbosity level 
  integer, public, save      :: Verbosity = VerbosityError 
  !  #]
contains
  !---------------------------------------------
! TO BE ADDED LATER:
! a check on the filesize
  subroutine read_unformatted_array(filename,realarray,&
       expected_element_size,expected_nr_of_elements,choosen_verbosity)
    !  #[
    character(len=*), intent(in)  :: filename
    integer,          intent(in)  :: expected_element_size
    integer,          intent(in)  :: expected_nr_of_elements
    real(r4_), dimension(expected_nr_of_elements), intent(out) :: realarray
    integer, optional, intent(in) :: choosen_verbosity
    
    ! local variables
    integer      :: fileunit
    integer(i4_) :: headcount, swapped_headcount
    integer      :: i
    logical      :: correct_endianness
    character(len=1), dimension(:), allocatable :: bytearray
    character(len=1) :: tmp_byte

    ! set verbosity if needed
    IF (present(choosen_verbosity)) Verbosity = choosen_verbosity

    ! open the binary input file                      
    IF (Verbosity .ge. VerbosityReport) &
         print *,"opening binary file: ",trim(filename)

    IF (expected_element_size .ne. 4) THEN
       print *,"ERROR in read_unformatted_array:"
       print *,"sorry, this routine only has been written and tested"
       print *,"for reading 4 byte reals at the moment"
       print *,"Adapt the read_unformatted module inside genscat/support/file"
       print *,"if you need something else"
       stop 1
    ENDIF

    ! first open the file with direct access to inspect the first 4 bytes
    fileunit = get_lun()
    open(unit=fileunit,file=filename,status="old",&
         access = "direct",recl=4,ERR=99)

    ! read the first 4 bytes into headcount as an integer
    read(fileunit,rec=1) headcount
    IF (Verbosity .ge. VerbosityReport) &
         print *,"headcount = ",headcount

    ! close the file
    close(unit=fileunit)

    ! test if endianness of this file (little or big-endian)
    ! corresponds to the on this platform expected setting
    IF (headcount .eq. expected_nr_of_elements*expected_element_size) THEN
       correct_endianness = .true.
    ELSE
       correct_endianness = .false.
    ENDIF

    IF (correct_endianness) THEN
       IF (Verbosity .ge. VerbosityReport) &
            print *,"endianness is correct, so do a normal binary read:"

       ! open the file again, now for unformatted reading
       open(unit=fileunit,file=filename,status="old",&
            action="read",form="UNFORMATTED",ERR=99)
       
       ! read the unformatted data into the realarray
       read(fileunit) realarray
       
       ! close the file
       close(unit=fileunit)
       call free_lun(fileunit)
    ELSE
       IF (Verbosity .ge. VerbosityReport) THEN
          print *,"endianness is NOT correct, so do a direct access read,"
          print *,"followed by a byteswap"
       END IF

       ! allocate an array to perform the binary tricks on
       allocate(bytearray(expected_element_size*expected_nr_of_elements+8))

       ! open the file again, now to read the raw bytes in a big array
       open(unit=fileunit,file=filename,status="old",&
            access = "direct",&
            recl=expected_element_size*expected_nr_of_elements+8,ERR=99)

       IF (Verbosity .ge. VerbosityReport) &
            print *,"bytes to read = ",&
                    expected_element_size*expected_nr_of_elements+8

       read(fileunit,rec=1) bytearray
       
       IF (Verbosity .ge. VerbosityReport) &
            print *,"raw bytes have been read"

       headcount = transfer(bytearray(1:4),headcount)
       IF (Verbosity .ge. VerbosityReport) &
            print *,"testjos 1 : ",headcount

       IF (headcount .eq. expected_nr_of_elements*expected_element_size) THEN
          IF (Verbosity .ge. VerbosityReport) THEN
             print *,"byte-by-byte reading already did the trick,"
             print *,"so no byteswap seems to be needed ..."
          END IF
       ELSE
          ! try to do the byteswap
          DO i=1,expected_nr_of_elements+2
             ! swap byte 1 and 4
             tmp_byte             = bytearray(4*(i-1)+1)
             bytearray(4*(i-1)+1) = bytearray(4*(i-1)+4)
             bytearray(4*(i-1)+4) = tmp_byte
             
             ! swap byte 2 and 3
             tmp_byte             = bytearray(4*(i-1)+2)
             bytearray(4*(i-1)+2) = bytearray(4*(i-1)+3)
             bytearray(4*(i-1)+3) = tmp_byte
          END DO

          ! convert the heading counter to an integer
          swapped_headcount = transfer(bytearray(1:4),swapped_headcount)
          IF (Verbosity .ge. VerbosityReport) &
               print *,"testjos 2 : ",swapped_headcount
          
          ! check the heading counter
          IF ( swapped_headcount .ne. &
               expected_nr_of_elements*expected_element_size) THEN
             print *,"ERROR in read_unformatted_array:"
             print *,"content of this binary array seems not to correspond"
             print *,"to the expected contents."
             print *,""
             print *,"without swapping:      headcount = ",headcount
             print *,"with swapping: swapped_headcount = ",swapped_headcount
             print *,"however, the nr of elements in this real(r4_) array"
             print *,"is supposed to be: ",expected_nr_of_elements
             print *,""
             print *,"dont know what to do now ..."
             stop 1
          END IF
          
       END IF

       ! convert the array ro reals
       realarray = transfer(bytearray(5:expected_element_size*&
                                        expected_nr_of_elements+4),realarray)
! this causes a segmentation fault due to a bug in g95 (version 17-2-2005)
! therefore the next workaround is added for now:
!       DO i=1,expected_nr_of_elements
!          realarray(i) = transfer(bytearray(1+i*expected_element_size:&
!                                            4+i*expected_element_size),&
!                                  realarray(i))
!       END DO
! this bug was solved after reporting it on 18-2-2005 
! for the g95 version of 9-3-2005 (very good!)

       ! close the file
       close(unit=fileunit)

       ! free the no longer needed memory
       deallocate(bytearray)
    ENDIF

    return
    
99  print *,"ERROR in read_unformatted_array:"
    print *,"unable to open binary input file: ",trim(filename)
    stop

  end subroutine read_unformatted_array
  !  #]
  !---------------------------------------------
end module ReadUnformatted
