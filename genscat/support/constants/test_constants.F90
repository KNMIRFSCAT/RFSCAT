program test_constants
!  #[ Documentation
! a little test program to demonstrate the effect of defining
! the same constant with different accuracies.
!
! Written by: J. de Kloe, KNMI
!
!  #]
!  #[ Modifications
! 
!  Last modified: $Date: 2011-11-15 07:59:40 +0100 (Tue, 15 Nov 2011) $
!
!  #]
!  #[ Modules used
  USE constants
!  #]
!  #[ program code
  print *,"---"
  print *,"pi_r4 = ",pi_r4
  print *,"pi_r8 = ",pi_r8
  print *,"difference: ",pi_r8-pi_r4
  print *,"relative difference: ",(pi_r8-pi_r4)/pi_r8
  print *,"---"
  print *,"speed_of_light_r4 = ",speed_of_light_r4
  print *,"speed_of_light_r8 = ",speed_of_light_r8
  print *,"difference: ",speed_of_light_r8-speed_of_light_r4
  print *,"relative difference: ",&
       (speed_of_light_r8-speed_of_light_r4)/speed_of_light_r8
  print *,"---"
!  #]
end program test_constants
