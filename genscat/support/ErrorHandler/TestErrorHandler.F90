program TestErrorHandler

  use ErrorHandler

  implicit none
  
  call InitErrorHandler(abort_on_error = .false.)

  print *,"testing: report_error"
  call report_error(error_allocate,"dummy_module_name1")
  print *,"testing: program_abort (with abort_on_error = .false.)"
  call program_abort(error_allocate,"dummy_module_name2")

  call InitErrorHandler(abort_on_error = .true.)

  print *,"testing: program_abort (with abort_on_error = .true.)"
  call program_abort(error_allocate,"dummy_module_name2")

  print *,"this print should not be reached !!!"

end program TestErrorHandler
