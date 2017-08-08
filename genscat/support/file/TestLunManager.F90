PROGRAM TestLunManager ! main program
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             TestLunManager
!
!     Created on:            01-04-2005
!
!     Last Modified on:      $Date: 2011-11-15 07:59:20 +0100 (Tue, 15 Nov 2011) $
!
!     Modifications Log:
!
! $Id: TestLunManager.F90 7732 2011-11-15 06:59:20Z verhoefa $
!

  ! define modules
  USE LunManager

  ! define variables for main program
  IMPLICIT NONE     ! no implicit variable typing

  integer :: test_unit1, test_unit2
  logical :: in_use

  !---------------------------------------------------
  !  ! normal use of the lun_manager module would be:
  !  !
  !  ! if needed disable unitnumbers that are in use already in other
  !  ! parts of the program:
  !  CALL disable_lun(21)
  !  ! request a fileunit for use:
  !  CALL test_unit1 = get_lun()
  !  !
  !  ! open the file and do something with it
  !  ! then close the file when ready.
  !  !
  !  ! then free this unitnumber to enable use in
  !  ! other parts of the program:
  !  CALL free_lun(test_unit1)
  !  ! only if it is certain that the diabled unitnumber is not
  !  ! needed again, enable it:
  !  CALL enable_lun(21)
  !---------------------------------------------------

  !---------------------------------------------------
  ! start the main program
  !---------------------------------------------------

  ! a number of tests to call all available functions
  write(fileunit_stdout,*) 'Starting fileunit test program'

  print *,'===== lun_manager ======'
  test_unit1 = get_lun()
  test_unit2 = test_unit1+1
  CALL free_lun(test_unit2)
  CALL free_lun(88)
  CALL enable_lun(88)
  CALL disable_lun(88)

  CALL disable_lun(21)

  test_unit2 = get_lun()
  in_use = inquire_lun_in_use(test_unit2)
  print *,"unit: ",test_unit2," is used?: ",in_use
  open(unit=test_unit2, file="TestLunManager.F90",status="old",&
       action="read",err=99)
  in_use = inquire_lun_in_use(test_unit2)
  print *,"unit: ",test_unit2," is used?: ",in_use

  CALL inspect_luns

  close(unit=test_unit2)
  CALL disable_lun(test_unit2)

  CALL free_lun(test_unit1)
  test_unit1 = get_lun()

  CALL print_used_lun
!  CALL print_disabled_lun

  CALL free_lun(test_unit2)
  CALL free_lun(test_unit1)
  
  CALL enable_lun(21)
  CALL enable_lun(22)

!  CALL print_used_lun
  
  stop
  
99 print *,"Sorry, unable to open input testfile ...."
  stop
  
END program TestLunManager
!---------------------------------------------------

