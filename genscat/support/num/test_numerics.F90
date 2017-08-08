PROGRAM testnumerics ! main program
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             test_numerics
!
!     Created on:            25-03-2004
!
!     Last Modified on:      $Date: 2011-11-15 07:59:36 +0100 (Tue, 15 Nov 2011) $
!
!     Modifications Log:
!
! $Id: test_numerics.F90 7748 2011-11-15 06:59:36Z verhoefa $
!
! Revision 1.12  2006/03/28 11:00:04  vogelzan
! Included NWP SAF acknowledgement.
!
!

  !---------------------------------------------------
  ! define modules
  !---------------------------------------------------

  USE numerics

  !---------------------------------------------------
  ! define variables for main program
  !---------------------------------------------------

  IMPLICIT NONE     ! no implicit variable typing

  real(r_)     :: real_default_size
  !integer(i_)  :: int_default_size ! not used at the moment
  integer(i1_) :: int1
  integer(i2_) :: int2
  integer(i4_) :: int4
  integer(i8_) :: int8

  real(r4_) :: r4
  real(r8_) :: r8

  integer   :: error_flag

  !---------------------------------------------------
  ! start the main program
  !---------------------------------------------------

  ! a number of tests to call all available functions
  print *,'Starting numerics test program'
  print *,'===== representation tests ======'
  print *,"REALACC(6)"
  print *,"r4: digits     ",digits(r4)
  print *,"r4: epsilon    ",epsilon(r4)
  print *,"r4: huge       ",huge(r4)
  print *,"r4: minexponent",minexponent(r4)
  print *,"r4: maxexponent",maxexponent(r4)
  print *,"r4: precision  ",precision(r4)
  print *,"r4: radix      ",radix(r4)
  print *,"r4: range      ",range(r4)
  print *,"r4: tiny       ",tiny(r4)
  print *,"ENDREALACC"

  print *,"REALACC(12)"
  print *,"r8: digits     ",digits(r8)
  print *,"r8: epsilon    ",epsilon(r8)
  print *,"r8: huge       ",huge(r8)
  print *,"r8: minexponent",minexponent(r8)
  print *,"r8: maxexponent",maxexponent(r8)
  print *,"r8: precision  ",precision(r8)
  print *,"r8: radix      ",radix(r8)
  print *,"r8: range      ",range(r8)
  print *,"r8: tiny       ",tiny(r8)
  print *,"ENDREALACC"

  print *,'===== numerics tests ======'
  int1  = 127
  int2  = 256*128-1 ! = 32767
  int4  = missing_indicator_integer
  int8  = 9223372036854775807_i8_
  r4 = missing_indicator_real
  r8 = missing_indicator_real_r8

  print *,"int1 = ",int1
  print *,"int2 = ",int2
  print *,"int4 = ",int4
  print *,"int8 = ",int8
  print *,"huge(int1) = ",huge(int1)
  print *,"huge(int2) = ",huge(int2)
  print *,"huge(int4) = ",huge(int4)
  print *,"huge(int8) = ",huge(int8)
  print *,"REALACC(6)  r4   = ",r4," ENDREALACC"
  print *,"REALACC(12) r8   = ",r8," ENDREALACC"

  print *,'===== check variable sizes  ======'
  call check_variable_sizes(error_flag)
  IF (error_flag .eq. no_error) print *,"Variable sizes are as expected"

  print *,'===== detect and print variable sizes ======'
  CALL detect_variable_sizes

  print *,'===== dB conversion test ======'
  print *,"REALACC(6)"
  real_default_size = 0.000123
  print *,"input test number:        ",real_default_size
  real_default_size = real2dB(real_default_size)
  print *,"converted to dB:          ",real_default_size
  real_default_size = dB2real(real_default_size)
  print *,"converted back to a real: ",real_default_size
  print *,"ENDREALACC"
  print *,'===== done ======'

END program testnumerics
!---------------------------------------------------
