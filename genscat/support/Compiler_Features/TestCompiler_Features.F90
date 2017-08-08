program TestCompiler_Features

  ! a simple test program for this module
  ! written by: J. de Kloe
  ! last change: 08-03-2006

  !    Imported from the ADM L2BP software into Genscat
  !    by J. de Kloe, 08-03-2006

  USE Compiler_Features

  integer :: n, nargs
  character(len=256) :: arg

  print *,"---------------------------"
  print *,"testing special characters:"
  print *,"---------------------------"

  print *,"ichar(tabchar) = ",ichar(tabchar)
  print *,"[tabchar] = [",tabchar,"]"

  print *,"ichar(retchar) = ",ichar(retchar)
  print *,"[retchar] = [",retchar,"]"

  print *,"ichar(newline) = ",ichar(newline)
  print *,"[newline] = [",newline,"]"

  print *,"ichar(bs) = ",ichar(bs)
  print *,"[bs] = [",bs,"]"

  print *,"---------------------------"
  print *,"testing argument handling:"
  print *,"---------------------------"

  nargs = iargc_genscat()
  print *,"nr. of arguments present = ",nargs
  DO n=1,nargs
     call getarg_genscat(n,arg)
     print "('arg(',i2.2,')=[',a,']')",n,trim(arg)
  END DO

end program TestCompiler_Features
