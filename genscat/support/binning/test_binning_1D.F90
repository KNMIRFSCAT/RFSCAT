  program test_binning_1D
!------------------------------------------------------------------------------!
! test_binning_1D tests the 1D binning routines in genscat/support/binning     !
!                                                                              !
! Version   Date         Comments                                              !
! -------   ----         --------                                              !
!   1.0     24-01-2006   Original version by Jur Vogelzang, KNMI.              !
!------------------------------------------------------------------------------!
  use numerics
  use binning
!------------------------------------------------------------------------------!
! Declarations.                                                                !
!------------------------------------------------------------------------------!
  type(binned_1D_data_type)  :: bin1,bin2   ! Bin structs
  integer  :: NrBins=5                      ! Bin definition
  real(l_) :: MinBin=0.0 , MaxBin=5.0       ! Bin definition

  integer  :: lun=3                         ! File unit for I/O
  integer  :: I                             ! Index
  real(l_) :: X                             ! Variable to be binned
!------------------------------------------------------------------------------!
! Initialise bin1 and fill it with the numbers -1.0 to 6.0 in steps of 0.5.    !
!------------------------------------------------------------------------------!
  call init_1D_bin(bin1 , NrBins,MinBin,MaxBin)

  DataLoop1: do I=-2,12
    X=0.5*float(I)
    call add_to_1D_bin(bin1 , X)
  enddo Dataloop1

  write (*,*) 'Program test_binning_1D '
  write (*,*) ' '
  write (*,*) 'Bin1: Binning -1.0 to 6.0 with step 0.5 in [0,5] with bin size 1'
  call print_1D_bin(bin1)
!------------------------------------------------------------------------------!
! Initialise bin2 and fill it with the numbers -0.9 to 4.5 in steps of 0.3.    !
!------------------------------------------------------------------------------!
  call init_1D_bin(bin2 , NrBins,MinBin,MaxBin)

  DataLoop2: do I=-3,15
    X=0.3*float(I)
    call add_to_1D_bin(bin2 , X)
  enddo Dataloop2

  write (*,*) ' '
  write (*,*) 'Bin2: Binning -0.9 to 4.5 with step 0.3 in [0,5] with bin size 1'
  call print_1D_bin(bin2)
!------------------------------------------------------------------------------!
! Write bin1 and bin2 on tape lun and read them in again in reverse order.     !
!------------------------------------------------------------------------------!
  open (lun , file='test_binning_1D.dat' , status='unknown')

  call write_1D_bin(bin1 , lun)
  call write_1D_bin(bin2 , lun)

  rewind lun

  call read_1D_bin(bin2 , lun)
  call read_1D_bin(bin1 , lun)

  close (lun , status='delete')

  write (*,*) ' '
  write (*,*) 'Bin1 <--> Bin2 by writing on file and reading in again'
  write (*,*) ' '

  write (*,*) 'Bin1: Binning -0.9 to 4.5 with step 0.3 in [0,5] with bin size 1'
  call print_1D_bin(bin1)

  write (*,*) ' '
  write (*,*) 'Bin2: Binning -1.0 to 6.0 with step 0.5 in [0,5] with bin size 1'
  call print_1D_bin(bin2)
!------------------------------------------------------------------------------!
! Merge bin2 into bin1.                                                        !
!------------------------------------------------------------------------------!
  call merge_1D_bin(bin1 , bin2)

  write (*,*) ' '
  write (*,*) 'Bin2 merged into Bin1'
  write (*,*) ' '
  write (*,*) 'Bin1:'
  call print_1D_bin(bin1)

  stop
  end program test_binning_1D
