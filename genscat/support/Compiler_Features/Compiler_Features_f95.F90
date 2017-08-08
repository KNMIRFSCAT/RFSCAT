MODULE Compiler_Features
  !  #[ Description

  !---------------------------------------------------
  ! a module to manage all the compiler-dependent problems
  ! This one handles NAGWare Fortran
  ! When executing the Set_MakeOptions, the file Compiler_Features creates
  ! a link to this module.
  !---------------------------------------------------
  !    Written by:  Paul Poli.
  !    created:     15-09-2005
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

  character(len=1), parameter :: tabchar = achar(9)
  character(len=1), parameter :: retchar = achar(13)
  character(len=1), parameter :: newline = achar(10)
  character(len=1), parameter :: bs = '\'
  !  #]
CONTAINS
  !----------------------------------------
  function iargc_genscat result(nargs)
    !  #[
    ! Points to the routine which returns the number of arguments
    !  #[ modules used
    USE F90_UNIX_ENV, only: iargc
    !  #]
    integer :: nargs
    
    nargs = iargc()
  end function iargc_genscat
    !  #]
  !--------------------------------
  subroutine getarg_genscat(i_in,c_out)
    !  #[
    ! Returns the string of arguments in input
    !  #[ modules used
    USE F90_UNIX_ENV, only: getarg
    !  #]
    integer,          intent(in)  :: i_in
    character(len=*), intent(out) :: c_out

    call getarg(i_in,c_out)
  end subroutine getarg_genscat
    !  #]
  !--------------------------------
  subroutine getenv_genscat(name,value)
    !  #[
    ! Returns the value of the environment variable
    character(len=*), intent(in)  :: name
    character(len=*), intent(out) :: value
    !  #[ Interfaces
    ! specify explicit interface for the fortran extension getenv()
    interface
      subroutine getenv(name, value) !  system fortran routine
        character(len=*), intent(in)  :: name
        character(len=*), intent(out) :: value
      end subroutine getenv
    end interface
    !  #]

    call getenv(name,value)

  end subroutine getenv_genscat
    !  #]
  !--------------------------------
END MODULE Compiler_Features
