MODULE binning
  !  #[ Description:
  !---------------------------------------------------------
  ! a collection of datastructures and subroutines
  ! for easy determination of distribution properties
  ! of data
  !
  !           Split from scat_subroutines.f90 file:
  !                  02-09-2004 (version nr. 0.92)
  !
  !           Written by:          Jos de Kloe
  !           Copyright:           KNMI
  !           version number:      0.04
  !           last change:         01-02-2006
  !
  !  This software was developed by KNMI within the context of the
  !  Climate-SAF project.
  !
  !---------------------------------------------------------
!  #]
  !  #[ USE Definitions
  ! a module that defines some generic numerics tricks
  USE NUMERICS, only: missing_real
  !  #]
  !  #[ parameters and types used for binning
  IMPLICIT NONE     ! no implicit variable typing
  private ! everything is private, unless explicitely states public

  integer, parameter  :: MaxNr1DBinSteps = 10001
  integer, parameter  :: MaxNr2DBinSteps = 150
  integer, parameter  :: MaxNr3DBinSteps = 100

  TYPE binned_1D_data_type
     integer :: NrSteps
     real    :: MinVal
     real    :: MaxVal
     real    :: StepSize   ! = MaxVal/NrSteps
     integer :: NrDatavaluesBinned
     integer :: NrDatavaluesOutOfRange
     integer, dimension(MaxNr1DBinSteps) :: data
  END TYPE binned_1D_data_type

  TYPE binned_2D_data_type
     ! first the dimensions of the bin arrays and the stepsizes
     integer :: NrSteps
     real    :: MinVal
     real    :: MaxVal
     real    :: StepSize   ! = MaxVal/NrSteps
     integer :: NrDatavaluesBinned
     integer :: NrDatavaluesOutOfRange
     integer, dimension(MaxNr2DBinSteps,MaxNr2DBinSteps) :: data
  END TYPE binned_2D_data_type

  TYPE binned_3D_data_type
     ! first the dimensions of the xbin arrays and the stepsizes
     integer :: NrStepsx
     real    :: MinValx
     real    :: MaxValx
     real    :: StepSizex   ! = MaxVal/NrSteps
     integer :: NrDatavaluesBinned
     integer :: NrDatavaluesOutOfRange
     TYPE(binned_2D_data_type), dimension(MaxNr3DBinSteps) :: datax
  END TYPE binned_3D_data_type
!  #]
  !  #[ user interface
  public :: binned_1D_data_type, read_1d_bin, write_1d_bin, init_1D_bin, &
            add_to_1d_bin, merge_1D_bin, get_1D_bin, print_1D_bin, &
            binned_2D_data_type, init_2d_bin, sum_2d_bins, &
            write_2d_bin, read_2d_bin, add_to_2d_bin, print_2d_bin, &
            binned_3D_data_type, init_3d_bin, read_3d_bin, add_to_3d_bin, &
            write_3d_bin, write_3D_bin_as_1D_bin, write_3d_bin_as_2d_bin
  !  #]
CONTAINS
  !  #[ 3D binning subroutines
  subroutine init_3D_bin(bin, NrStepsx,  MinValx,  MaxValx,&
                              NrStepsuv, MinValuv, MaxValuv)
    !  #[
    TYPE(binned_3D_data_type), intent(out) :: bin
    integer, intent(in) :: NrStepsx, NrStepsuv
    real, intent(in)    :: MinValx, MaxValx, MinValuv, MaxValuv

    ! local variables
    integer :: i

    IF (NrStepsx .gt. MaxNr3DBinSteps) THEN
       print *,"Error: NrStepsx = ",NrStepsx
       print *,"Error: but should never be larger than: MaxNr3DBinSteps",&
            MaxNr3DBinSteps
       print *,"Please adjust MaxNr3DBinSteps in module scat_subroutines"
       print *,"and try again"
       stop
    END IF
    ! NrStepsuv is checked inside init_2D_bin() !!!

    bin%NrStepsx  = NrStepsx
    bin%MinValx   = MinValx
    bin%MaxValx   = MaxValx
    bin%StepSizex = (MaxValx-MinValx)/real(NrStepsx)
    bin%NrDatavaluesBinned     = 0
    bin%NrDatavaluesOutOfRange = 0
    DO i=1,NrStepsx
       CALL init_2D_bin(bin%datax(i),NrStepsuv, MinValuv, MaxValuv)
    END DO

  end subroutine init_3D_bin
    !  #]
  subroutine add_to_3D_bin(bin,x,u,v)
    !  #[
    real, intent(in)                         :: x,u,v
    TYPE(binned_3D_data_type), intent(inout) :: bin
    integer :: index_x
    IF (missing_real(x)) THEN
       ! print *,"Data not binned because it was missing: ",x
    ELSE
        ! first determine the proper index
       index_x = 1 + floor( (x-bin%MinValx)/bin%StepSizex )

       ! then check if we are within the array boundaries
       IF ( (index_x .ge. 1) .and. (index_x .le. bin%NrStepsx) ) THEN
          ! then add to the 2D subbin
          bin%NrDatavaluesBinned = bin%NrDatavaluesBinned + 1
          CALL add_to_2D_bin(bin%datax(index_x),u,v)
       ELSE
          ! print *,"Data not binned because it was out of range: ",x
          ! print *,"==> index_x = ",index_x
          bin%NrDatavaluesOutOfRange = bin%NrDatavaluesOutOfRange + 1
       END IF
    END IF
  end subroutine add_to_3D_bin
    !  #]
  subroutine read_3D_bin(bin,fileunit)
    !  #[
    TYPE(binned_3D_data_type), intent(out) :: bin
    integer, intent(in)                    :: fileunit

    ! local variables
    integer :: i
    integer :: NrStepsx
    real    :: MinValx, MaxValx, StepSizex

    print *,"reading 3D binned data from file:"

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          binned_2D_data(NrSteps)
    read(fileunit,'(i4,1X,f9.3,1X,f9.3,1X,f9.3)') &
         NrStepsx, MinValx, MaxValx, StepSizex
    bin%NrStepsx  = NrStepsx
    bin%MinValx   = MinValx
    bin%MaxValx   = MaxValx
    bin%StepSizex = StepSizex
    read(fileunit,'(i8,1X,i8)') &
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    DO i = 1,NrStepsx
       CALL read_2D_bin(bin%datax(i),fileunit,silent=.true.)
    END DO

  end subroutine read_3D_bin
  !  #]
  subroutine write_3D_bin(bin,fileunit)
    !  #[
    TYPE(binned_3D_data_type), intent(in) :: bin
    integer, intent(in)                :: fileunit

    ! local variables
    integer :: i

    print *,"writing 3D binned data to file:"
    print *,"bin%NrDatavaluesBinned     = ",bin%NrDatavaluesBinned
    print *,"bin%NrDatavaluesOutOfRange = ",bin%NrDatavaluesOutOfRange

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          binned_2D_data(NrSteps)
    write(fileunit,'(i4,1X,f9.3,1X,f9.3,1X,f9.3)') &
         bin%NrStepsx,bin%MinValx,bin%MaxValx,bin%StepSizex
    write(fileunit,'(i8,1X,i8)') &
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    DO i = 1,bin%NrStepsx
       CALL write_2D_bin(bin%datax(i),fileunit,silent=.true.)
    END DO

  end subroutine write_3D_bin
    !  #]
  subroutine write_3D_bin_as_2D_bin(bin,fileunit)
    !  #[
    TYPE(binned_3D_data_type), intent(in) :: bin
    integer, intent(in)                :: fileunit

    ! local variables
    TYPE(binned_2D_data_type):: bin2D
    integer :: i

    print *,"writing 3D binned data to 2D bin file:"
    print *,"bin%NrDatavaluesBinned     = ",bin%NrDatavaluesBinned
    print *,"bin%NrDatavaluesOutOfRange = ",bin%NrDatavaluesOutOfRange

    CALL init_2D_bin(bin2D, bin%datax(1)%NrSteps, &
                     bin%datax(1)%MinVal, bin%datax(1)%MaxVal)
    bin2D%NrDatavaluesBinned     = 0
    bin2D%NrDatavaluesOutOfRange = bin%NrDatavaluesOutOfRange
    DO i = 1,bin%NrStepsx
       bin2D%NrDatavaluesBinned     = bin2D%NrDatavaluesBinned + &
            bin%datax(i)%NrDatavaluesBinned
       bin2D%NrDatavaluesOutOfRange = bin2D%NrDatavaluesOutOfRange + &
            bin%datax(i)%NrDatavaluesOutOfRange
       bin2D%data(:,:) = bin2D%data(:,:) + bin%datax(i)%data(:,:)
    END DO

    CALL write_2D_bin(bin2D,fileunit)

  end subroutine write_3D_bin_as_2D_bin
    !  #]
  subroutine write_3D_bin_as_1D_bin(bin,fileunit,use_gaussian_wind)
    !  #[
    TYPE(binned_3D_data_type), intent(in) :: bin
    integer, intent(in)                :: fileunit
    logical, optional                  :: use_gaussian_wind

    ! local variables
    TYPE(binned_1D_data_type) :: bin1D
    TYPE(binned_2D_data_type) :: bin2D_uv
    integer :: i,j
    logical :: use_gaussian_wind_distr
    real    :: u,v,u_sq,v_sq,start_width, width_sq, height
    real    :: sum_gaus, sum_distr ! , corr_width (not used anymore)
    real, dimension(:,:), allocatable :: gaus_uv, norm_factor

    print *,"writing 3D binned data to 1D bin file:"
    print *,"bin%NrDatavaluesBinned     = ",bin%NrDatavaluesBinned
    print *,"bin%NrDatavaluesOutOfRange = ",bin%NrDatavaluesOutOfRange

    use_gaussian_wind_distr = .false.
    IF (present(use_gaussian_wind)) THEN
       IF (use_gaussian_wind) THEN
          use_gaussian_wind_distr = .true.
       END IF
    END IF

    allocate(gaus_uv(    bin2D_uv%NrSteps,bin2D_uv%NrSteps))
    allocate(norm_factor(bin2D_uv%NrSteps,bin2D_uv%NrSteps))

    forcegaussian: IF (use_gaussian_wind_distr) THEN
       print *,"forcing the data to have gaussian u,v distribution"
       ! compress the 3rd dimension of the 3D array to get
       ! the uv distribution function
       CALL init_2D_bin(bin2D_uv, bin%datax(1)%NrSteps, &
                        bin%datax(1)%MinVal, bin%datax(1)%MaxVal)
       DO i = 1,bin%NrStepsx
          bin2D_UV%NrDatavaluesBinned     = &
               bin2D_uv%NrDatavaluesBinned + &
               bin%datax(i)%NrDatavaluesBinned
          bin2D_UV%NrDatavaluesOutOfRange = &
               bin2D_uv%NrDatavaluesOutOfRange + &
               bin%datax(i)%NrDatavaluesOutOfRange
          bin2D_uv%data(:,:) = bin2D_uv%data(:,:) + bin%datax(i)%data(:,:)
       END DO
       ! then calculating the correction function
       ! by enforcing uv to be a Gaussian function
       start_width = 5.5
       width_sq = start_width * start_width

       height = real(maxval(bin2D_uv%data(bin2D_uv%NrSteps/2-2:&
                                          bin2D_uv%NrSteps/2+2,&
                                          bin2D_uv%NrSteps/2-2:&
                                          bin2D_uv%NrSteps/2+2) ))
       print *,"using height = ",height
       print *,"distr. maximum = ",maxval(bin2D_uv%data)
       DO i=1,bin2D_uv%NrSteps
          u=bin2D_uv%MinVal + (real(i)-0.5)*bin2D_uv%StepSize
          u_sq = u*u
          DO j=1,bin2D_uv%NrSteps
             ! let u,v point to the center of each bin
             v=bin2D_uv%MinVal + (real(j)-0.5)*bin2D_uv%StepSize
             v_sq = v*v
             gaus_uv(i,j) = height*exp(-1.0*(u_sq+v_sq)/width_sq)
          END DO
       END DO

       ! compare integrals to see if I have to increase the width or height
       sum_gaus  = sum(gaus_uv)
       sum_distr = real(sum(bin2D_uv%data))

      ! corrected width
!       corr_width = start_width * sum_distr / sum_gaus
!       width_sq = corr_width * corr_width
!       print *,"corrected width = ",corr_width

       ! corrected height
       height = height*sum_distr/sum_gaus
       print *,"corrected height = ",height

       ! calculate the corrected gaussian
       DO i=1,bin2D_uv%NrSteps
          u=bin2D_uv%MinVal + (real(i)-0.5)*bin2D_uv%StepSize
          u_sq = u*u
          DO j=1,bin2D_uv%NrSteps
             ! let u,v point to the center of each bin
             v=bin2D_uv%MinVal + (real(j)-0.5)*bin2D_uv%StepSize
             v_sq = v*v
             gaus_uv(i,j) = height*exp(-1.0*(u_sq+v_sq)/width_sq)

             ! use this gaussian and the original distribution mapped
             ! onto the uv plane to determine the normalisation function
             norm_factor(i,j) = 1.0
             IF (bin2D_uv%data(i,j) .gt. 0) &
                  norm_factor(i,j) = gaus_uv(i,j)/real(bin2D_uv%data(i,j))
          END DO
       END DO
    END IF forcegaussian

    CALL init_1D_bin(bin1D, bin%NrStepsx, bin%MinValx, bin%MaxValx)
    bin1D%NrDatavaluesBinned     = 0
    bin1D%NrDatavaluesOutOfRange = bin%NrDatavaluesOutOfRange
    DO i = 1,bin%NrStepsx
       bin1D%NrDatavaluesBinned     = bin1D%NrDatavaluesBinned + &
            bin%datax(i)%NrDatavaluesBinned
       bin1D%NrDatavaluesOutOfRange = bin1D%NrDatavaluesOutOfRange + &
            bin%datax(i)%NrDatavaluesOutOfRange

       IF (use_gaussian_wind_distr) THEN
          ! print *,"testjos: doing the actual normalisation"
          bin1D%data(i) = bin1D%data(i) + &
               nint(sum(norm_factor(1:bin2D_uv%NrSteps,&
                                    1:bin2D_uv%NrSteps)*&
                        real(bin%datax(i)%data(1:bin2D_uv%NrSteps,&
                                          1:bin2D_uv%NrSteps)) ) )
       ELSE
          bin1D%data(i) = bin1D%data(i) + sum(bin%datax(i)%data(:,:))
       END IF
    END DO

    CALL write_1D_bin(bin1D,fileunit)
    deallocate(gaus_uv)
    deallocate(norm_factor)

  end subroutine write_3D_bin_as_1D_bin
    !  #]
  !  #]
  !  #[ 2D binning subroutines
  subroutine init_2D_bin(bin, NrSteps, MinVal, MaxVal)
    !  #[
    TYPE(binned_2D_data_type), intent(out) :: bin
    integer, intent(in) :: NrSteps
    real, intent(in)    :: MinVal, MaxVal

    IF (NrSteps .gt. MaxNr2DBinSteps) THEN
       print *,"Error: NrSteps=",NrSteps
       print *,"Error: but should never be larger than: MaxNr2DBinSteps",&
            MaxNr2DBinSteps
       print *,"Please adjust MaxNr2DBinSteps in module scat_subroutines"
       print *,"and try again"
       stop
    END IF

    bin%NrSteps   = NrSteps
    bin%MinVal    = MinVal
    bin%MaxVal    = MaxVal
    bin%StepSize  = (MaxVal-MinVal)/real(NrSteps)
    bin%data(:,:)   = 0
    bin%NrDatavaluesBinned     = 0
    bin%NrDatavaluesOutOfRange = 0
  end subroutine init_2D_bin
  !  #]
  subroutine sum_2D_bins(sumbin, bin)
    !  #[
    TYPE(binned_2D_data_type), intent(inout) :: sumbin
    TYPE(binned_2D_data_type), intent(in)    :: bin

    ! first compare the dimensions to see if summing is possible
    IF (sumbin%NrSteps .ne. bin%NrSteps) THEN
       print *,"ERROR in sumbins: combining uncompatible bins !"
       stop
    ENDIF
    IF (sumbin%MinVal .ne. bin%MinVal) THEN
       print *,"ERROR in sumbins: combining uncompatible bins !"
       stop
    END IF
    IF (sumbin%MaxVal .ne. bin%MaxVal) THEN
       print *,"ERROR in sumbins: combining uncompatible bins !"
       stop
    END IF

    ! do the actual summation
!    sumbin%data(:,:) = sumbin%data(:,:) + bin%data(:,:)
    sumbin%data = sumbin%data + bin%data
    sumbin%NrDatavaluesBinned     = sumbin%NrDatavaluesBinned + &
                                       bin%NrDatavaluesBinned
    sumbin%NrDatavaluesOutOfRange = sumbin%NrDatavaluesOutOfRange + &
                                       bin%NrDatavaluesOutOfRange
  end subroutine sum_2D_bins
  !  #]
  subroutine add_to_2D_bin(bin,x1, x2)
    !  #[
    real, intent(in) :: x1, x2
    TYPE(binned_2D_data_type), intent(inout) :: bin

    integer :: index_x1, index_x2

    IF (missing_real(x1) .or. missing_real(x2)) THEN
       ! print *,"Data not binned because it was missing: ",x1,x2
    ELSE
       ! first determine the proper indices
       index_x1 = 1 + floor( (x1-bin%MinVal)/bin%StepSize )
       index_x2 = 1 + floor( (x2-bin%MinVal)/bin%StepSize )

       IF (bin%maxval .eq. 360.) THEN
          ! ok, now we have a direction, so make sure 360 is mapped to 0
          IF (index_x1 .gt. bin%NrSteps) index_x1=index_x1-bin%NrSteps
          IF (index_x2 .gt. bin%NrSteps) index_x2=index_x2-bin%NrSteps
       END IF

       ! then check if we are within the array boundaries
       IF ( (index_x1 .ge. 1) .and. (index_x1 .le. bin%NrSteps) .and. &
            (index_x2 .ge. 1) .and. (index_x2 .le. bin%NrSteps)      ) THEN
          ! then increment the counters
          bin%NrDatavaluesBinned      = bin%NrDatavaluesBinned      + 1
          bin%data(index_x1,index_x2) = bin%data(index_x1,index_x2) + 1
       ELSE
          ! print *,"Data not binned because it was out of range: ",x1,x2
          ! print *,"==> index_x1 = ",index_x1," ==> index_x2 = ",index_x2
          bin%NrDatavaluesOutOfRange = bin%NrDatavaluesOutOfRange + 1
       END IF
    END IF
  end subroutine add_to_2D_bin
  !  #]
  subroutine write_2D_bin(bin,fileunit,silent)
    !  #[
    TYPE(binned_2D_data_type), intent(in) :: bin
    integer, intent(in)                :: fileunit
    logical, optional                  :: silent

    ! local variables
    integer :: i,j
    logical :: be_silent

    be_silent = .false.
    IF (present(silent)) THEN
       IF (silent) be_silent = .true.
    END IF

    IF (.not. be_silent) THEN
      print *,"writing binned data to file:"
      print *,"bin%NrDatavaluesBinned     = ",bin%NrDatavaluesBinned
      print *,"bin%NrDatavaluesOutOfRange = ",bin%NrDatavaluesOutOfRange
    END IF

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          data
    write(fileunit,'(i4,1X,f9.3,1X,f9.3,1X,f9.3)') &
         bin%NrSteps,bin%MinVal,bin%MaxVal,bin%StepSize
    write(fileunit,'(i8,1X,i8)') &
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    DO i = 1,bin%NrSteps
       DO j = 1,bin%NrSteps
          write(fileunit,'(i9)',ADVANCE="NO") bin%data(i,j)
       END DO
       write(fileunit,"(/)") ! end the line
    END DO

  end subroutine write_2D_bin
  !  #]
  subroutine read_2D_bin(bin,fileunit,silent)
    !  #[
    TYPE(binned_2D_data_type), intent(out) :: bin
    integer, intent(in)                    :: fileunit
    logical, optional                      :: silent

    ! local variables
    integer :: i,j
    integer :: NrSteps
    real    :: MinVal, MaxVal, StepSize
    logical :: be_silent

    be_silent = .false.
    IF (present(silent)) THEN
       IF (silent) be_silent = .true.
    END IF

    IF (.not. be_silent) THEN
       print *,"reading binned data from file:"
    END IF

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          data
    read(fileunit,'(i4,1X,f9.3,1X,f9.3,1X,f9.3)') &
         NrSteps, MinVal, MaxVal, StepSize
    call init_2D_bin(bin,NrSteps, MinVal, MaxVal)
    bin%NrSteps  = NrSteps
    bin%MinVal   = MinVal
    bin%MaxVal   = MaxVal
    bin%StepSize = StepSize
    read(fileunit,'(i8,1X,i8)') &
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    DO i = 1,NrSteps
       DO j = 1,NrSteps
          read(fileunit,'(i9)',ADVANCE="NO") bin%data(i,j)
       END DO
       read(fileunit,"(/)") ! end the line
    END DO

  end subroutine read_2D_bin
  !  #]
  subroutine print_2D_bin(bin)
    !  #[
    TYPE(binned_2D_data_type), intent(in) :: bin

    print *,"bin definition:"
    print *,"NrSteps, MinVal, MaxVal, StepSize = ",&
         bin%NrSteps, bin%MinVal, bin%MaxVal, bin%StepSize
    print *,"bin collected data thus far:"
    print *,"NrDatavaluesBinned, NrDatavaluesOutOfRange = ",&
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    print *,"bin%data(:,:)  ",bin%data(1:bin%NrSteps,1:bin%NrSteps)

  end subroutine print_2D_bin
  !  #]
  !  #]
  !  #[ 1D binning subroutines
  subroutine init_1D_bin(bin, NrSteps, MinVal, MaxVal)
    !  #[
    TYPE(binned_1D_data_type), intent(out) :: bin
    integer, intent(in) :: NrSteps
    real, intent(in)    :: MinVal, MaxVal

    IF (NrSteps .gt. MaxNr1DBinSteps) THEN
       print *,"Error: NrSteps=",NrSteps
       print *,"Error: but should never be larger than: MaxNr1DBinSteps",&
            MaxNr1DBinSteps
       print *,"Please adjust MaxNr1DBinSteps in module scat_subroutines"
       print *,"and try again"
       stop
    END IF

    bin%NrSteps   = NrSteps
    bin%MinVal    = MinVal
    bin%MaxVal    = MaxVal
    bin%StepSize  = (MaxVal-MinVal)/real(NrSteps)
    bin%data(:)   = 0
    bin%NrDatavaluesBinned     = 0
    bin%NrDatavaluesOutOfRange = 0
  end subroutine init_1D_bin
  !  #]
  subroutine add_to_1D_bin(bin,x)
    !  #[
    real, intent(in) :: x
    TYPE(binned_1D_data_type), intent(inout) :: bin

    integer :: index_x

    IF (missing_real(x)) THEN
       ! print *,"Data not binned because it was missing: ",x
    ELSE
       ! first determine the proper index
       index_x = 1 + floor( (x-bin%MinVal)/bin%StepSize )

       ! then check if we are within the array boundaries
       IF ( (index_x .ge. 1) .and. (index_x .le. bin%NrSteps) ) THEN
          ! then increment the counters
          bin%NrDatavaluesBinned = bin%NrDatavaluesBinned + 1
          bin%data(index_x)      = bin%data(index_x)      + 1
       ELSE
       !  print *,"Data not binned because it was out of range: ",x
       !  print *,"==> index_x = ",index_x," x=",x
          bin%NrDatavaluesOutOfRange = bin%NrDatavaluesOutOfRange + 1
       END IF
    END IF
  end subroutine add_to_1D_bin
  !  #]
  subroutine merge_1D_bin(bin1,bin2)
!------------------------------------------------------------------------------!
! merge_1D_bin adds the data of bin2 to those of bin1, provided that the       !
! binning is identical.                                                        !
!                                                                              !
! Version   Date         Comments                                              !
! -------   ----         --------                                              !
!   1.0     24-01-2006   Original version by Jur Vogelzang, KNMI.              !
!   1.1     01-02-2006   J. de Kloe, corrected wrong use of implicit allocation!
!                                    in parameter list of this subroutine.     !
!------------------------------------------------------------------------------!
    TYPE(binned_1D_data_type), intent(inout) :: bin1
    TYPE(binned_1D_data_type), intent(in)    :: bin2
    integer                                  :: I     ! index
!------------------------------------------------------------------------------!
!   Check binning parameters.                                                  !
!------------------------------------------------------------------------------!
    IF (bin1%NrSteps /= bin2%NrSteps) then
      write (*,*) 'Error in merge_1D_bin: unequal number of steps.'
      write (*,*) 'Number of steps in bin1 and bin2:',bin1%NrSteps,bin2%NrSteps
      stop
    END IF
    IF (bin1%MinVal /= bin2%MinVal) then
      write (*,*) 'Error in merge_1D_bin: unequal minimum values.'
      write (*,*) 'Minimum values in bin1 and bin2:',bin1%MinVal,bin2%Minval
      stop
    END IF
    IF (bin1%MaxVal /= bin2%MaxVal) then
      write (*,*) 'Error in merge_1D_bin: unequal maximum values.'
      write (*,*) 'Maximum values in bin1 and bin2:',bin1%MaxVal,bin2%Maxval
      stop
    END IF
!------------------------------------------------------------------------------!
!   Merge bin2 into bin1.                                                      !
!------------------------------------------------------------------------------!
    bin1%NrDatavaluesBinned     = bin1%NrDatavaluesBinned +       &
                                  bin2%NrDatavaluesBinned
    bin1%NrDatavaluesOutOfRange = bin1%NrDatavaluesOutOfRange +   &
                                  bin2%NrDatavaluesOutOfRange
    DO I=1,bin1%NrSteps
      bin1%data(I) = bin1%data(I) + bin2%data(I)
    ENDDO
  end subroutine merge_1D_bin

  subroutine get_1D_bin(bin , BMin,BMax,BStep,NB,Nin,Nout , B)
!------------------------------------------------------------------------------!
! get_1D_bin returns the data in struct bin.                                   !
!                                                                              !
! Version   Date         Comments                                              !
! -------   ----         --------                                              !
!   1.0     24-01-2006   Original version by Jur Vogelzang, KNMI.              !
!   1.1     26-01-2006   Corrected declaration. Jur Vogelzang.                 !
!------------------------------------------------------------------------------!
    TYPE(binned_1D_data_type), intent(in) :: bin
    real, intent(out)                     :: BMin,BMax  ! Minimum and maximum
    real, intent(out)                     :: BStep      ! Binning step size
    integer, intent(out)                  :: NB         ! Number of bins
    integer, intent(out)                  :: Nin,Nout   ! number binned/not binned
    integer, dimension(:), pointer        :: B          ! counting data

    ! local variable
    integer                               :: I          ! index

    NB = bin%NrSteps
    BMin = bin%MinVal
    BMax = bin%MaxVal
    BStep = bin%StepSize
    Nin   = bin%NrDatavaluesBinned
    Nout  = bin%NrDatavaluesOutOfRange

    IF (associated(B)) THEN
       print *,"WARNING in get_1D_bin():"
       print *,"The pointer to the result array seems already associated."
       print *,"Please make sure it is nullified, and not pointing to any memory"
       print *,"before entering this routine."
       return
    END IF

    allocate(B(NB))
    do I=1,NB
      B(I) = bin%data(I)
    enddo

    return
  end subroutine get_1D_bin

  subroutine write_1D_bin(bin,fileunit)
    !  #[
    TYPE(binned_1D_data_type), intent(in) :: bin
    integer, intent(in)                   :: fileunit

    ! local variables
    integer :: i

    ! print *,"writing binned data to file:"
    ! print *,"bin%NrDatavaluesBinned     = ",bin%NrDatavaluesBinned
    ! print *,"bin%NrDatavaluesOutOfRange = ",bin%NrDatavaluesOutOfRange

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          data
    write(fileunit,*) bin%NrSteps,bin%MinVal,bin%MaxVal,bin%StepSize
    write(fileunit,*) bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    write(fileunit,*) (bin%data(i),i=1,bin%NrSteps)

  end subroutine write_1D_bin
  !  #]
  subroutine read_1D_bin(bin,fileunit)
    !  #[
    TYPE(binned_1D_data_type), intent(out) :: bin
    integer, intent(in) :: fileunit

    ! local variables
    integer :: i
    integer :: NrSteps
    real    :: MinVal, MaxVal, StepSize

    ! print *,"reading binned data from file:"

    ! format:  NrSteps MinVal MaxVal StepSize
    !          NrDatavaluesBinned   NrDatavaluesOutOfRange
    !          data
    read(fileunit,*) NrSteps, MinVal, MaxVal, StepSize
    call init_1D_bin(bin,NrSteps, MinVal, MaxVal)
    bin%NrSteps  = NrSteps
    bin%MinVal   = MinVal
    bin%MaxVal   = MaxVal
    bin%StepSize = StepSize
    read(fileunit,*) bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    read(fileunit,*) (bin%data(i),i=1,NrSteps)

  end subroutine read_1D_bin
  !  #]
  subroutine print_1D_bin(bin)
    !  #[
    TYPE(binned_1D_data_type), intent(in) :: bin

    print *,"bin definition:"
    print *,"NrSteps, MinVal, MaxVal, StepSize = ",&
         bin%NrSteps, bin%MinVal, bin%MaxVal, bin%StepSize
    print *,"bin collected data thus far:"
    print *,"NrDatavaluesBinned, NrDatavaluesOutOfRange = ",&
         bin%NrDatavaluesBinned, bin%NrDatavaluesOutOfRange
    print *,"bin%data(:)  ",bin%data(1:bin%NrSteps)

  end subroutine print_1D_bin
  !  #]
  !  #]
END MODULE binning
