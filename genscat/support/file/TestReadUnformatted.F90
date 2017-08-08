program test_read_unformatted

  use numerics,    only: r_
  use LunManager, only: get_lun, free_lun
  use ReadUnformatted, only: read_unformatted_array

  implicit none
  integer, parameter :: testsize = 10

  character(len=256)           :: filename
  character(len=256)           :: filename_swapped
  real(r_),dimension(testsize) :: testarray
  real(r_),dimension(testsize) :: testarray2
  integer                      :: i, fileunit
  character(len=1), dimension(:), allocatable :: bytearray
  character(len=1)             :: tmp_byte

  ! fill the test variables
  filename         = "testbinaryfile.bin"
  filename_swapped = "testswappedbinaryfile.bin"
  DO i=1,testsize
     testarray(i) = 1.1*real(i)
  END DO

  print *,"creating testfile"

  ! write a test binary file
  fileunit = get_lun()
  open(unit=fileunit,file=filename,status="replace",&
       action="write",form="UNFORMATTED",ERR=99)

  ! write the test data to the file
  write(fileunit) testarray

  ! close the file
  close(unit=fileunit)

  print *,"creating a byteswapped testfile"

  allocate(bytearray(testsize*4+8))

  ! open the file again, now to read single bytes
  open(unit=fileunit,file=filename,status="old",&
       access = "direct",recl=1,ERR=99)

  DO i=1,testsize*4+8
     read(fileunit,rec=i) bytearray(i)
  END DO

  ! close the file
  close(unit=fileunit)

  ! do the byteswap
  DO i=1,testsize+2
     ! swap byte 1 and 4
     tmp_byte             = bytearray(4*(i-1)+1)
     bytearray(4*(i-1)+1) = bytearray(4*(i-1)+4)
     bytearray(4*(i-1)+4) = tmp_byte
     
     ! swap byte 2 and 3
     tmp_byte             = bytearray(4*(i-1)+2)
     bytearray(4*(i-1)+2) = bytearray(4*(i-1)+3)
     bytearray(4*(i-1)+3) = tmp_byte
  END DO

  ! write the byteswapped data to a new testfile

  ! open the file again, now to write single bytes
  open(unit=fileunit,file=filename_swapped,status="replace",&
       access = "direct",recl=1,ERR=99)
  DO i=1,testsize*4+8
     write(fileunit,rec=i) bytearray(i)
  END DO

  ! close the file
  close(unit=fileunit)
  call free_lun(fileunit)

  print *,"trying to read the normal testfile"
  testarray2(:) = 0.
  call read_unformatted_array(filename,testarray2, 4, testsize)

  print *,"testfile was read succesfully"

  DO i =1,testsize
     IF (testarray(i) .ne. testarray2(i)) THEN
        print *,"PROBLEMS in test_read_unformatted:"
        print *,"element nr. ",i," is not as expected:"
        print *,"expected: testarray(i)  = ",testarray(i)
        print *,"but got:  testarray2(i) = ",testarray2(i)
        stop 1
     ENDIF
  END DO
  
  print *,"contents of testfile are as expected"

  print *,"trying to read the byteswapped testfile"
  testarray2(:) = 0.
  call read_unformatted_array(filename_swapped,testarray2, 4, testsize)

  print *,"testfile was read succesfully"

  DO i =1,testsize
     IF (testarray(i) .ne. testarray2(i)) THEN
        print *,"PROBLEMS in test_read_unformatted:"
        print *,"element nr. ",i," is not as expected:"
        print *,"expected: testarray(i)  = ",testarray(i)
        print *,"but got:  testarray2(i) = ",testarray2(i)
        stop 1
     ENDIF
  END DO
  
  print *,"contents of swapped testfile are as expected"

  stop

99 print *,"ERROR in test_read_unformatted:"
  print *,"could not create binary testfile"
  stop

end program test_read_unformatted
