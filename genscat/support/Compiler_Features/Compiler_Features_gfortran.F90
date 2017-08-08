MODULE Compiler_Features
  !  #[ Description

  !---------------------------------------------------
  ! a module to manage all the compiler-dependent problems
  ! This one handles gfortran
  ! When executing the Set_MakeOptions, the file Compiler_Features creates
  ! a link to this module.
  !---------------------------------------------------
  !    Written by:  J. de Kloe.
  !    created:     05-01-2006
  !    last change: 08-03-2006
  !
  !    Imported from the ADM L2BP software into Genscat
  !    by J. de Kloe, 08-03-2006
  !
  ! Modifications:
  !    13-May-2009 J. de Kloe added getenv_genscat
  !
  !---------------------------------------------------

  !  #]
  !  #[ Variables and parameters
  IMPLICIT NONE

  character, parameter :: tabchar = achar(9)
  character, parameter :: retchar = achar(13)
  character, parameter :: newline = achar(10)
  character, parameter :: bs = achar(92)

  !  #]
CONTAINS
  !----------------------------------------
  function iargc_genscat() result(nargs)
    !  #[
    ! Points to the routine which returns the number of arguments
    integer :: nargs
    ! WARNING: gfortran refuses to compile when you use
    !          an explicit interface for iargc() !!!
    nargs = iargc()
  end function iargc_genscat
    !  #]
  !--------------------------------
  subroutine getarg_genscat(i_in,c_out)
    !  #[
    ! Returns the string of arguments in input
    integer,          intent(in)  :: i_in
    character(len=*), intent(out) :: c_out
    ! WARNING: gfortran refuses to compile when you use
    !          an explicit interface for getarg() !!!
    call getarg(i_in,c_out)
  end subroutine getarg_genscat
    !  #]
  !--------------------------------
  subroutine getenv_genscat(name,value)
    !  #[
    ! Returns the value of the environment variable
    character(len=*), intent(in)  :: name
    character(len=*), intent(out) :: value

    ! WARNING: gfortran refuses to compile when you use
    !          an explicit interface for getenv !!!
    call getenv(name,value)

  end subroutine getenv_genscat
    !  #]
  !--------------------------------
END MODULE Compiler_Features
