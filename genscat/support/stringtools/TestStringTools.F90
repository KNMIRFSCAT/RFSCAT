program TestStringTools
  !  #[ Documentation
  ! a number of simple tests for the subroutines defined
  ! in the stringtools module
  !
  !  #]
  !  #[ Modifications:
  !   07-Sep-2006  J. de Kloe   added tests for Split_path_and_filename()
  !   08-Sep-2006  J. de Kloe   added tests for Join_path_and_filename()
  !   13-Dec-2006  J. de Kloe   added some trim() statements
  !   26-Oct-2007  J. de Kloe   added some new tests for the new subroutine
  !                             Split_filename_and_extension
  !   26-May-2009  J. de Kloe   added tests for string_list_type
  !
  !  #]
  !  #[ Modules used
  USE StringTools
  !  #]
  !  #[ Variables
  implicit none
  character(len=50)               :: txt
  character(len=1), dimension(50) :: chararr
  character(len=1)                :: tempchar
  character(len=50)               :: path_and_filename
  character(len=50)               :: filename_and_extension
  character(len=256)              :: path, filename, extension
  integer                         :: i, strlen, error_flag
  type(string_list_type)          :: sl
  character(len=1), dimension(:), pointer :: chararr_text 
  !  #]
  !  #[ test chararray and case conversions
  ! init and print 
  txt(:)     = ' '
  chararr(:) = ' '
  print *,"txt                   = [",txt,"]"
  print *,"string2chararray(txt) = [",chararr,"]"

  ! fill with a test value and print
  txt = "Hello world"
  print *,"trim(txt)             = [",trim(txt),"]"

  ! test the case functions and print
  print *,"to_lowercase(txt)     = [",to_lowercase(txt),"]"
  print *,"to_uppercase(txt)     = [",to_uppercase(txt),"]"

  ! convert to chararr and print
  chararr = string2chararray(txt)
  print *,"string2chararray(txt) = [",chararr,"]"

  ! reverse the chararr as test
  strlen=len_trim(txt)
  DO i=1,strlen/2
     tempchar            = chararr(i)
     chararr(i)          = chararr(strlen+1-i)
     chararr(strlen+1-i) = tempchar
  END DO

  ! convert back to a string and print
  txt = chararray2string(chararr)
  print *,"trim(txt)             = [",trim(txt),"]"
  !  #]
  !  #[ test contains_spaces
  txt = "This is a test"
  print *,"test string: [",trim(txt),"]"
  print *,"contains_spaces(txt) = ",contains_spaces(txt)
  txt = "Thisisatestwithoutspaces"
  print *,"test string: [",trim(txt),"]"
  print *,"contains_spaces(txt) = ",contains_spaces(txt)
  !  #]
  !  #[ test splitting/joining of path-file-ext strings
  print *,"Testing splitting  of path-file strings"

  path_and_filename = "a_normal_dummy_dir/with_a_file.txt"
  print *,"path_and_filename = ["//trim(path_and_filename)//"]"
  call Split_path_and_filename(path_and_filename,path,filename)
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"

  path_and_filename = "a_dir_without_file/"
  print *,"path_and_filename = ["//trim(path_and_filename)//"]"
  call Split_path_and_filename(path_and_filename,path,filename)
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"

  path_and_filename = "a_file_without_dir.txt"
  print *,"path_and_filename = ["//trim(path_and_filename)//"]"
  call Split_path_and_filename(path_and_filename,path,filename)
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"

  print *,"Testing joining of path-file strings"

  path     = ""
  filename = "dummyfilename"
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"
  print *,"path_and_filename = ",trim(Join_path_and_filename(path,filename))

  path     = "Test"
  filename = "dummyfilename"
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"
  print *,"path_and_filename = ",trim(Join_path_and_filename(path,filename))

  path     = "Test/"
  filename = "dummyfilename"
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"
  print *,"path_and_filename = ",trim(Join_path_and_filename(path,filename))

  path     = "Test/"
  filename = ""
  print *,"path     = ["//trim(path)//"]"
  print *,"filename = ["//trim(filename)//"]"
  print *,"path_and_filename = ",trim(Join_path_and_filename(path,filename))

  filename_and_extension = "Testfile.txt"
  print *,"filename_and_extension = ["//trim(filename_and_extension)//"]"
  call Split_filename_and_extension(filename_and_extension,filename,extension)
  print *,"filename  = ["//trim(filename)//"]"
  print *,"extension = ["//trim(extension)//"]"

  filename_and_extension = "TestfileWithoutExtension"
  print *,"filename_and_extension = ["//trim(filename_and_extension)//"]"
  call Split_filename_and_extension(filename_and_extension,filename,extension)
  print *,"filename  = ["//trim(filename)//"]"
  print *,"extension = ["//trim(extension)//"]"

  filename_and_extension = ".TestfileStartingWithDot"
  print *,"filename_and_extension = ["//trim(filename_and_extension)//"]"
  call Split_filename_and_extension(filename_and_extension,filename,extension)
  print *,"filename  = ["//trim(filename)//"]"
  print *,"extension = ["//trim(extension)//"]"

  filename_and_extension = "Testfile.With2DotsAndTailingSpaces.text       "
  print *,"filename_and_extension = ["//trim(filename_and_extension)//"]"
  call Split_filename_and_extension(filename_and_extension,filename,extension)
  print *,"filename  = ["//trim(filename)//"]"
  print *,"extension = ["//trim(extension)//"]"
  !  #]
  !  #[ test the string_list type
  call init_string_list(sl)
  call add_string_to_list(sl,"Some text ",error_flag)
  call add_string_to_list(sl,"a second piece of text ",error_flag)
  call add_string_to_list(sl,"yet another bit of text ",error_flag)
  call add_string_to_list(sl,"Finally some very very very long text 12345678901234567890123456789012345678901234567890",error_flag)
  nullify(chararr_text)
  chararr_text => get_string_from_list(sl)
  print *,""
  print *,"Size of resulting concatenated string: ",&
       get_total_string_len_from_list(sl)
  print *,"Resulting string: ",chararray2string(chararr_text)
  print *,""
  call delete_string_list(sl)
  deallocate(chararr_text)
  !  #]
end program TestStringTools
