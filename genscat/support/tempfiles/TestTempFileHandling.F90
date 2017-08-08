program TestTempFileHandling

  use tempfile_handling, only: get_temp_filename, free_temp_filename
  use lunmanager, only: get_lun, free_lun

  implicit none
  character(len=256) :: temp_filename
  integer :: error_flag, fileunit

  call get_temp_filename(temp_filename,error_flag)
  print *,"temp_filename = "//trim(temp_filename)

  fileunit = get_lun()
  open(unit=fileunit,file=temp_filename,status="replace",&
       form="FORMATTED",action="write",err=999)
  write(fileunit,"(a)") "Hello world"
  write(fileunit,"(a)") "Bye cruel world"
  close(unit=fileunit)

  call free_lun(fileunit)
  call free_temp_filename(temp_filename,error_flag)

  call get_temp_filename(temp_filename,error_flag)
  print *,"temp_filename = "//trim(temp_filename)
  call free_temp_filename(temp_filename,error_flag)

  call get_temp_filename(temp_filename,error_flag)
  print *,"temp_filename = "//trim(temp_filename)
  call free_temp_filename(temp_filename,error_flag)

  goto 111

999 print *,"Open command failed"

111 continue 

end program TestTempFileHandling
