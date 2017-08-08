Module timers
!  #[ Documentation
!
! This module provides some functionality to allow easy
! timing of subroutines, functions, or just selected pieces of
! your fortran code.
!
! Usage:
!
! Written by:  Jos de Kloe, 2000.
!
!  #]
!  #[ Modifications
!
!  Last modified: $Date: 2011-11-15 07:59:39 +0100 (Tue, 15 Nov 2011) $
!
! $Id: timers.F90 7751 2011-11-15 06:59:39Z verhoefa $
!
! Revision 1.3  2009/05/26 13:12:34  kloedej
! added a switch to allow use the Fortran90 system_clock routine for timing
!
!
!  #]
!  #[ Modules used
  use numerics, only: r8_, missing_indicator_real_r8
  use c_support, only: wclock
!  #]
!  #[ Type and parameter definitions
  IMPLICIT NONE
  
  type timer_type
     real(r8_) :: start_time ! in [s]
     real(r8_) :: stop_time  ! in [s]
     real(r8_) :: total_time ! in [s] (total run time) 
     integer   :: num_calls
     integer   :: which_timer
     integer   :: struct_was_properly_initialised
  end type timer_type

! possible values for which_timer are:
  integer, parameter :: timer_undefined        = -1
  integer, parameter :: timer_c_gettimeofday   = 1
  integer, parameter :: timer_F90_system_clock = 2

! a fixed number to allow checking whether the struct was
! properly initialised of not
  integer, parameter :: timer_canary = 64273519
!  #]
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  subroutine init_timer(t,which_timer)
    !  #[ initialise the timer struct
    type(timer_type),  intent(out) :: t
    integer, optional, intent(in)  :: which_timer

    t%start_time  = missing_indicator_real_r8
    t%stop_time   = missing_indicator_real_r8
    t%total_time  = 0._r8_
    t%num_calls   = 0
    t%which_timer = timer_c_gettimeofday
    t%struct_was_properly_initialised = timer_canary

    ! choose which type of timer to use
    if (present(which_timer)) t%which_timer = which_timer

  end subroutine init_timer
    !  #]
  subroutine start_timer(t)
    !  #[ start the timer
    type(timer_type), intent(inout) :: t

    if (t%struct_was_properly_initialised .ne. timer_canary) then
       print *,"ERROR in start_timer: "
       print *,"the timer struct must be initialised before use !!!"
       stop 1
    end if

    t%start_time = get_time(t%which_timer) ! in [s]

  end subroutine start_timer
    !  #]
  subroutine stop_timer(t)
    !  #[ stop the timer, and add result to totals
    type(timer_type), intent(inout) :: t

    ! local variable
    real(r8_) :: this_time

    if (t%struct_was_properly_initialised .ne. timer_canary) then
       print *,"ERROR in start_timer: "
       print *,"the timer struct must be initialised before use !!!"
       stop 1
    end if

    t%stop_time  = get_time(t%which_timer)   ! in [s]
    t%num_calls  = t%num_calls+1
    this_time    = t%stop_time-t%start_time ! in [s]
    t%total_time = t%total_time + this_time ! in [s]

  end subroutine stop_timer
    !  #]
  subroutine print_timer_result(t,name)
    !  #[ print the timing result
    type(timer_type), intent(in) :: t
    character(len=*), intent(in) :: name
    
    print *,"Result for timer: "//trim(name)
    select case(t%which_timer)
    case(timer_c_gettimeofday)
       print *,"using the wrapper to the c-routine gettimeofday"
    case(timer_F90_system_clock)
       print *,"using the F90 system_clock routine"
    case default
       print *,"ERROR in print_timer_result: unknown value for which_timer ",&
            t%which_timer
       stop 1
    end select

    print *,'===> nr of calls: ',t%num_calls 
    IF (t%num_calls  .eq. 0) return

    print *,'===> total time spent in this routine: ',&
         t%total_time,' [s]'
    print *,'===> average time for each call: ',&
         t%total_time/t%num_calls,' [s]'

  end subroutine print_timer_result
    !  #]
  function get_num_calls(t) result(num_calls)
    !  #[
    type(timer_type), intent(in) :: t         ! input
    integer                      :: num_calls ! result

    num_calls = t%num_calls

  end function get_num_calls
    !  #]
  function get_total_time(t) result(total_time)
    !  #[
    type(timer_type), intent(in) :: t          ! input
    real(r8_)                    :: total_time ! result

    total_time = t%total_time

  end function get_total_time
    !  #]
  function get_avg_time(t) result(avg_time)
    !  #[
    type(timer_type), intent(in) :: t        ! input
    real(r8_)                    :: avg_time ! result

    avg_time = missing_indicator_real_r8
    if (t%num_calls .gt. 0) &
         avg_time = t%total_time/t%num_calls
    
  end function get_avg_time
    !  #]
  !--------------------------------
  ! for internal use only
  function get_time(which_timer) result(time)
    !  #[
    integer, intent(in) :: which_timer ! input
    real(r8_)           :: time        ! result

    ! local variables
    integer :: count,count_rate

    select case(which_timer)
    case(timer_c_gettimeofday)
       time = wclock() ! in [s]
    case(timer_F90_system_clock)
       ! warning: this nuber may wrap around when the 
       ! maximum integer value is reached
       call system_clock(count,count_rate)
       time = real(count,r8_)/count_rate
    case default
       print *,"ERROR in start_timer: unknown value for which_timer ",&
            which_timer
       stop 1
    end select

  end function get_time
    !  #]
  !--------------------------------
END Module timers
