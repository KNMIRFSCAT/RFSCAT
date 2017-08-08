MODULE random
  !-------------------------------------------
  !  #[ Documentation
  !
  !  some routines for random number generation,
  !
  ! contents:
  ! public functions:
  !   -function gaussian_rnd_nr() result(gasdev)
  !   -function random_nr()       result(uni_rand)
  !   -function random_nr2()      result(uni_rand)
  !   -function filed_random_nr() result(rnd_number)
  ! internal private functions:
  !   -none
  !
  ! Modifications:
  ! 07-May-2009 J. de Kloe imported r8_ type to replace l_
  !
  !  #]
  !  #[ Modules used
  USE numerics, only: r8_
  USE lunmanager, only: get_lun, free_lun
  !  #]
  !  #[ variables and parameters
  IMPLICIT NONE  ! no implicit variable typing
  public         ! make all variables and functions public by default

  !integer, parameter  :: default_seed = 123456
  integer, parameter  :: default_seed = 123454

  real(r8_) :: var(12)
  real(r8_) :: p1, p2
  real(r8_) :: s10, s11, s12, s13, s14, s20, s21, s22, s23, s24
  save         s10, s11, s12, s13, s14, s20, s21, s22, s23, s24
  
  equivalence (var(1), s10), (var(2), s11), (var(3), s12), &
       (var(4), s13), (var(5), s14), (var(6), s20), &
       (var(7), s21), (var(8), s22), (var(9), s23), &
       (var(10),s24), (var(11), p1), (var(12), p2)

  logical, save :: state_is_initialised = .false.
  !  #]
  !-------------------------------------------
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  function gaussian_rnd_nr()   result(gasdev)
    !  #[ 
    !-------------------------------------------
    ! added the function gaussian_rnd_nr 28-1-2000
    ! inspired on the routine gasdev from
    ! numerical recipes, 2nd edition, p. 280,
    ! (or numerical recipes, 1st edition, p. 203)
    !-------------------------------------------
    integer iset
    real(r8_) :: v1,v2,rsq,fac,gset,gasdev
    data iset /0/
    save iset,gset
    if (iset.eq.0) then
       check_loop: do
          v1=2.*random_nr()-1.
          v2=2.*random_nr()-1.
          rsq=v1**2+v2**2
          if ((rsq .lt. 1.) .and. (rsq .gt. 0)) exit check_loop
       end do check_loop
       fac=sqrt(-2.*log(rsq)/rsq)
       gset=v1*fac
       gasdev=v2*fac
       iset=1
    else
       gasdev=gset
       iset=0
    end if

    return

  end function gaussian_rnd_nr
  !  #]
  function random_nr2()     result(uni_rand)
    !  #[ test the build-in random generator
    ! use the build-in Fortran random nr generator for test purposes
    ! (warning: the seed has not been set, so the clock is used to seed
    !  the random number generator, and the numbers will not be
    !  reproducible between successive runs)
    real(r8_)::uni_rand
    CALL random_number(uni_rand)
    return
  end function random_nr2
    !  #]
  function random_nr()     result(uni_rand)
    !  #[ a custom random nr generator
    !-------------------------------------------
    ! Written by: Aad van der Steen
    ! copied 25-1-2000 by Jos de Kloe
    ! (with only minor changes to the layout,
    !  and a namechange from rand32 to random_nr)
    !
    ! Aad gave his permission to use and redistribute this code
    ! without any restrictions by email, 15-May-2009.
    !
    ! This code is based on a publication by l'Ecuyer.
    ! It is called MRG32k5a, which is a 32-bit CMRG of 
    ! order 5 with 2 components period lenght ca. 2^319
    ! CMRG= Combined multiple recursive random number generator
    !
    ! The algorithm is described in:
    ! www.iro.umontreal.ca/~lecuyer/myftp/papers/combmrg2.ps
    ! see: http://www.iro.umontreal.ca/~lecuyer/papers.html
    ! article nr: 111.
    ! This website also has a link to a c-code implementation
    ! that can be downloaded.
    !
    ! see table IV at p.10, 2nd item from the bottom):
    ! see also the example c-code on page 15.
    !
    !-------------------------------------------

    ! data die Aad in file bdata.f had staan zet ik maar 
    ! direct hieronder. Het wordt toch alleen hier gebruikt

    real(r8_), parameter :: a12   = 1154721.0_r8_
    real(r8_), parameter :: a15n  = 1108499.0_r8_
    real(r8_), parameter :: a14ml = 7473172652438757.0_r8_
    real(r8_), parameter :: a14   = 1739991.0_r8_
    real(r8_), parameter :: a21   = 1776413.0_r8_
    real(r8_), parameter :: a25n  = 1641052.0_r8_
    real(r8_), parameter :: a23m2 = 3715990064523381.0_r8_
    real(r8_), parameter :: a23   =  865203.0_r8_
    real(r8_), parameter :: m1    = 4294949027.0_r8_
    real(r8_), parameter :: m2    = 4294934327.0_r8_
    real(r8_), parameter :: m1inv = 1.0_r8_/m1
    real(r8_), parameter :: m2inv = 1.0_r8_/m2
    real(r8_), parameter :: norm  = 2.3283163396834613e-10_r8_

    real(r8_) :: uni_rand

    !----------------------------------------------------------
    !--- if seed < 0 then initialise s10 ... s24

    IF (.not. state_is_initialised) THEN 
      call init_random_status(default_seed)
    END IF
    !-----------------------------------------------
    !--- component 1.
 
    p1 = a12*s13 -a15n*s10
    IF (p1 > 0.0_r8_) p1 = p1 - a14*m1
    p1 = p1 + a14*s11
    p1 = p1 - Int( p1*m1inv )*m1
    IF (p1 < 0.0_r8_) p1=p1+m1
    s10 = s11
    s11 = s12
    s12 = s13
    s13 = s14
    s14 = p1

    !-----------------------------------------------
    !--- component 2

    p2 = a21*s24 - a25n*s20
    IF ( p2 > 0.0_r8_ ) p2 = p2 - a23*m2
    p2 = p2 + a23*s22
    p2 = p2 - Int( p2*m2inv )*m2
    IF (p2 < 0.0_r8_ ) p2 = p2 + m2
    s20 = s21
    s21 = s22
    s22 = s23
    s23 = s24
    s24 = p2

    !----------------------------------------------
    !--- combine.

    IF (p1 <= p2 ) THEN
      uni_rand = (p1 - p2 + m1)*norm
    ELSE
      uni_rand = (p1 - p2)*norm
    END IF
    
    return
    !----------------------------------------------
  END function random_nr
    !  #]
  subroutine init_random_status(seed)
    !  #[
    integer, intent(in) :: seed

    ! local variable
    integer   :: iseed(5)

    iseed(1)   =  seed
    iseed(2) = iseed(1) + 219
    iseed(3) = iseed(2) + 219
    iseed(4) = iseed(3) + 219
    iseed(5) = iseed(4) + 219
    s10        = Real( iseed(1), r8_ )
    s11        = Real( iseed(2), r8_ )
    s12        = Real( iseed(3), r8_ )
    s13        = Real( iseed(4), r8_ )
    s14        = Real( iseed(5), r8_ )
    s20        = s10 + 1154721.0_r8_
    s21        = s20 + s11
    s22        = s21 + s12
    s23        = s22 + s13
    s24        = s23 + s14
    
    state_is_initialised = .true.
    ! print *,'initialised random number generator; seed = ',seed

  end subroutine init_random_status
    !  #]
  function filed_random_nr(num_to_generate,filename) result(rnd_number)
    !  #[
    !-------------------------------------------
    ! random routine that reads random numbers from file.
    ! This is very useful to test software on different platforms
    ! in a reproducible way (it ensures using the same random sequence)
    !-------------------------------------------
    real(r8_) :: rnd_number ! result
    integer,          optional, intent(in) :: num_to_generate ! optional input
    character(len=*), optional, intent(in) :: filename        ! optional input

    ! local variables
    integer            :: fileunit
    character(len=256) :: random_nr_file
    integer,   save :: count      = 0
    integer,   save :: num_values = -1
    logical,   save :: firstcall  = .true.
    real(r8_), save, dimension(:), allocatable :: randum_nr_array

    random_nr_file = "random_numbers.dat" ! set default value
    IF (present(filename)) random_nr_file = filename

    IF (present(num_to_generate)) THEN
       num_values = num_to_generate
       allocate(randum_nr_array(1:num_values))
       fileunit = get_lun()
       open(fileunit,file=random_nr_file,status='replace',action="write")
       write(fileunit,*) num_values
       DO count=1,num_values
          randum_nr_array(count) = random_nr()
       END DO
       write(fileunit,*) randum_nr_array
       close(fileunit)
       call free_lun(fileunit)
       rnd_number = 0._r8_ ! return with a dummy value
       ! re-initialize to allow a next call to use the new file
       firstcall  = .true.
       count      = 0
       num_values = -1
       return
    END IF
    
    IF (firstcall) THEN ! load random numbers from file
       firstcall = .FALSE.
       fileunit = get_lun()
       open(fileunit,file=random_nr_file,&
            status='old',action="read")
       read(fileunit,*) num_values
       allocate(randum_nr_array(1:num_values))
       DO count=1,num_values
          read(fileunit,*) randum_nr_array(count)
       END DO
       close(fileunit)
       call free_lun(fileunit)
       count = 1
    END IF
    
    rnd_number = randum_nr_array(count)
    count = count + 1
    IF (count .gt. num_values) THEN
       print *,'max. nr. of random numbers exceeded !'
       stop
    END IF

    return
  END function filed_random_nr
    !  #]
  subroutine save_random_state_to_file(random_status_file)
    !  #[
    character(len=*), intent(in) :: random_status_file
    ! local variable
    integer :: i, fileunit

    fileunit = get_lun()
    open(fileunit,file=random_status_file,&
         status='replace',action="write")
    DO i=1,size(var)
       write(fileunit,*) var(i)
    END DO
    close(fileunit)
    call free_lun(fileunit)
    
  end subroutine save_random_state_to_file
    !  #]
  subroutine load_random_state_from_file(random_status_file)
    !  #[
    character(len=*), intent(in) :: random_status_file

    ! local variables
    integer :: i, fileunit
    
    fileunit = get_lun()
    open(fileunit,file=random_status_file,&
         status='old',action="read")
    DO i=1,size(var)
       read(fileunit,*) var(i)
    END DO
    close(fileunit)
    call free_lun(fileunit)
    state_is_initialised = .true.

  end subroutine load_random_state_from_file
    !  #]
  !--------------------------------
END Module random
