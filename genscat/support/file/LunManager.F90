MODULE LunManager
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             LunManager
!
!     Created on:            05-12-2002
!
!     Last Modified on:      $Date: 2011-11-15 07:59:19 +0100 (Tue, 15 Nov 2011) $
!
!     Modifications Log:
!
! $Id: LunManager.F90 7731 2011-11-15 06:59:19Z verhoefa $
!
! Revision 1.6  2006/03/28 10:21:52  vogelzan
! Included NWP SAF acknowledgement.
!
!
  !  #[ Description

  !---------------------------------------------------
  ! a library to manage the fileunits in fortran90 programs
  ! -its purpose is to make it easier to prevent using double unit numbers 
  !  without having to track all unitnumbers in all subprograms/units
  !  yourself.
  !---------------------------------------------------
  !    Written by:  Jos de Kloe.
  !    created:     05-12-2002 (code borrowed form the rfscat simulator)
  !    last change: 06-09-2005
  !---------------------------------------------------

  !  #]
  !  #[ Variables and parameters
  IMPLICIT NONE

  ! some default unit numbers
  integer, parameter :: fileunit_stderr = 0
  integer, parameter :: stderr          = 0
  integer, parameter :: fileunit_stdin  = 5
  integer, parameter :: stdin           = 5
  integer, parameter :: fileunit_stdout = 6
  integer, parameter :: stdout          = 6

  ! definition of the possible fileunits
  integer, parameter, private :: first_lun = 30
  integer, parameter, private :: last_lun  = 39
  logical, dimension(first_lun:last_lun), private :: lun_used     = .false.
  logical, dimension(first_lun:last_lun), private :: lun_disabled = .false.

  ! local debug switch
  logical, parameter :: debug = .false. ! .true.

  !remark: unit numbers must be positive and are usually between 1 and 99
  !        Often used default numbers are:
  !           0?= stderr, 5 = stdin, 6 = stdout
  !           16= pbmesr_fileunit
  !        these should not be used for normal files, 
  !        therefore I choose the range 20-29
  !  #]
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  subroutine inspect_luns
    !  #[

    ! loop over all possible lun's and see if they are available or not
    ! (mainly used for testing some fortran90 features)

    integer :: this_lun
    logical :: exists, is_open, nmd
    character(len=256) :: nam

    print *,"start of inspect_luns"
    DO this_lun = -1,99
       inquire(unit=this_lun,exist=exists,opened=is_open,named=nmd,name=nam)

       IF (exists) THEN
          IF (is_open) THEN
             print *," lun ",this_lun," is open"
             IF (nmd) print *," lun ",this_lun," has a name: ",trim(nam)
!          ELSE
!             print *," lun ",this_lun," is NOT open"
          END IF
       ELSE
          print *," lun ",this_lun," does NOT exist"          
       END IF

    END DO
    print *,"end of inspect_luns"

  end subroutine inspect_luns
  !--------------------------------

  !  #]
  function get_lun() result(fileunit)
    !  #[

    integer :: fileunit ! result

    ! local parameters
    integer :: this_lun
    logical :: exists, is_open !, nmd

    this_lun = first_lun
    find_free_lun_loop: DO
      IF ( (.not. lun_used(    this_lun) ) .and. &
           (.not. lun_disabled(this_lun) )        ) THEN

        ! for extra security, see if this unit exists, and is not yet open
        inquire(unit=this_lun,exist=exists,opened=is_open)
        IF (.not. exists) THEN
           print *,"ERROR in get_lun(), a free unit number seems illegal"
           print *,"probably the first_lun, last_lun parameters in the file"
           print *,"lun_manager.f90 have illegal values..."
           print *,"fileunit = ",this_lun
           ! disable the use of this unit number, and search another one
           CALL disable_lun(this_lun)
        ELSE
           IF (is_open) THEN
              print *,"ERROR in get_lun(), a free unit number seems "
              print *,"to be in use already. Probably by a program unit or"
              print *,"library that does not use this module for unit handling"
              ! disable the use of this unit number, and search another one
              CALL disable_lun(this_lun)
           ELSE
              ! all seems OK, so return with the found unit number
              exit find_free_lun_loop
           END IF
        END IF

      ELSE
        this_lun = this_lun + 1
      END IF

      IF (this_lun .gt. last_lun) THEN
        print *,'ERROR: no more fileunits available in function get_lun !'
        ! exit this routine gracefully, by reporting an error
        fileunit=-1 
        return
        ! replace if needed by the next action to stop program execution
!        print *,'aborting program .......'
!        stop
      END IF
    END DO find_free_lun_loop

    lun_used(this_lun) = .true.
    fileunit = this_lun
    IF (debug) print *,"get_lun(): requested LUN: ",fileunit

    return

  end function get_lun
  !--------------------------------

  !  #]
  subroutine free_lun(fileunit)
    !  #[
    integer, intent(in) :: fileunit

    IF ((fileunit .ge. first_lun) .and. &
        (fileunit .le. last_lun )        ) THEN
      IF (lun_used(fileunit)) THEN
        lun_used(fileunit) = .false.
      ELSE
        print *,'fileunit: ',fileunit,' was not in use !!!'
        print *,'free_lun returns without freeing any fileunit'
      END IF
    ELSE
      print *,'fileunit: ',fileunit,' was not in the range that is handled'
      print *,'by this module ! (',first_lun,' - ',last_lun,')'
      print *,'free_lun returns without freeing any fileunit'
    END IF

    IF (debug) print *,"free_lun(): freed LUN: ",fileunit

    return
  end subroutine free_lun
  !--------------------------------
  !  #]
  subroutine print_used_lun
    !  #[
    integer :: i

    DO i = first_lun, last_lun
      IF (lun_used(i)) THEN
        print *,'fileunit: ',i,' is in use'
      ELSE
        IF (lun_disabled(i)) THEN
          print *,'fileunit: ',i,' is disabled'
        ELSE
          print *,'fileunit: ',i,' is still available'
        END IF
      END IF
    END DO
    return
  end subroutine print_used_lun
  !--------------------------------
  !  #]
  subroutine disable_lun(fileunit)
    !  #[
    integer, intent(in) :: fileunit

    IF ((fileunit .ge. first_lun) .and. &
        (fileunit .le. last_lun )        ) THEN
      IF (lun_used(fileunit)) THEN
        print *,'fileunit: ',fileunit,' is still in use !'
        print *,'disabling it is only possible if it is not used !'
        print *,'disable_lun returns without disabling any fileunit'
      ELSE
        IF (.NOT. lun_disabled(fileunit)) THEN
          lun_disabled(fileunit) = .true.
!          print *,'fileunit: ',fileunit,' has been disabled'
        ELSE
          print *,'fileunit: ',fileunit,' was already disabled !!!'
          print *,'disable_lun returns without disabling any fileunit'
        END IF
      END IF
    ELSE
      print *,'fileunit: ',fileunit,' was not in the range that is handled'
      print *,'by this module ! (',first_lun,' - ',last_lun,')'
      print *,'disable_lun returns without disabling any fileunit'
    END IF

    return
  end subroutine disable_lun
  !--------------------------------
  !  #]
  subroutine enable_lun(fileunit)
    !  #[

    integer, intent(in) :: fileunit

    IF ((fileunit .ge. first_lun) .and. &
        (fileunit .le. last_lun )        ) THEN
      IF (lun_disabled(fileunit)) THEN
        lun_disabled(fileunit) = .false.
!        print *,'fileunit: ',fileunit,' has been enabled'
      ELSE
        print *,'fileunit: ',fileunit,' was already enabled !!!'
        print *,'enable_lun returns without enabling any fileunit'
      END IF
    ELSE
      print *,'fileunit: ',fileunit,' was not in the range that is handled'
      print *,'by this module ! (',first_lun,' - ',last_lun,')'
      print *,'enable_lun returns without enabling any fileunit'
    END IF

    return
  end subroutine enable_lun
  !--------------------------------

    !  #]
  subroutine print_disabled_lun
    !  #[
    integer :: i

    DO i = first_lun, last_lun
      IF (lun_disabled(i)) THEN
        print *,'fileunit: ',i,' is disabled'
      ELSE
        print *,'fileunit: ',i,' is enabled'
      END IF
    END DO
    return

  end subroutine print_disabled_lun
  !--------------------------------
    !  #]
  function inquire_lun_in_use(fileunit) result(in_use)
    !  #[
    ! inquires whether the given fileunit is connected to a file
    ! or not. If in use, true is returned, otherwise false

    ! this is a test of this feature of fortran90,
    ! not using the datastructure in this module
    integer :: fileunit
    logical :: in_use

    in_use = .false.
    inquire(unit=fileunit, opened=in_use)
    return

  end function inquire_lun_in_use
    !  #]
  !--------------------------------
END Module LunManager


