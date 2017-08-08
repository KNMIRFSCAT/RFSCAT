program SortModTest
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             SortModTest
!
!     Created on:            27-10-2005
!
!     Last Modified on:      $Date: 2011-11-15 07:59:26 +0100 (Tue, 15 Nov 2011) $
!
!     Modifications Log:
!
! $Id: SortModTest.F90 7738 2011-11-15 06:59:26Z verhoefa $
!

  use SortMod

  implicit none

  integer,parameter    :: n = 10
  integer,dimension(n) :: sort_index
  real   ,dimension(n) :: sort_array
  integer i
  !
  print *,"Test program for the SortMod module"

  sort_array( 1) = 10.0
  sort_array( 2) =  9.0
  sort_array( 3) =  8.0
  sort_array( 4) =  7.0
  sort_array( 5) =  6.0
  sort_array( 6) =  5.0
  sort_array( 7) =  4.0
  sort_array( 8) =  3.0
  sort_array( 9) =  2.0
  sort_array(10) =  1.0

  print *,"Unsorted array"
  print '(10F5.1)',(sort_array(i), i=1,n)

  call GetSortIndex(n,sort_array,sort_index)

  print *,"After GetSortIndex"
  print '(10F5.1)',(sort_array(sort_index(i)), i=1,n)

  call SortWithIndex(n,sort_index,sort_array)

  print *,"Sorted array, after SortWithIndex"
  print '(10F5.1)',(sort_array(i), i=1,n)

end program SortModTest
