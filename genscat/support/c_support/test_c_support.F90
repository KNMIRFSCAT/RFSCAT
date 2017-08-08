program test_c_support
  !  #[ use statements
  USE c_support, only: get_filesize
  !  #]
  !  #[ variables
  implicit none
  integer :: size, fileunit
  character(len=256) :: filename
  !  #]
  !  #[ program code
  filename = "c_support.F90"

  ! test get_filesize() by supplying the filename
  size = get_filesize(filename)
  print *,"file: ",trim(filename)," has a size of ",size," bytes"

  filename = "F90_c_support.c"
  fileunit = 21

  ! test get_filesize() by supplying the fileunit
  open(unit=fileunit,file=filename,status="old",&
       action="read",form="FORMATTED",ERR=99)
  size = get_filesize(fileunit)
  print *,"file: ",trim(filename)," with fileunit: ",fileunit
  print *,"has a size of ",size," bytes"
  close(unit=fileunit)

  ! added to circumvent the irritating "FORTRAN STOP"
  ! message issued by the pgf90 compiler at this point
  goto 100
  !stop

99 print *,"ERROR: unable to open input file: ",trim(filename)
  stop

100 continue

  !  #]
end program test_c_support
