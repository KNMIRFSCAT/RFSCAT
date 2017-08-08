program test_timers

  USE timers
  USE numerics, only: r8_

  type(timer_type) :: timer1, timer2, timer3, timer4
  integer   :: i,j
  real(r8_) :: x,y

  call init_timer(timer1,which_timer=timer_c_gettimeofday) ! default
  call init_timer(timer2,which_timer=timer_c_gettimeofday) ! default
  call init_timer(timer3,which_timer=timer_F90_system_clock)
  call init_timer(timer4,which_timer=timer_F90_system_clock)

  do i=1,50000
     call start_timer(timer1)
     do j=1,2
        x=sin(0.005_r8_*i)
     end do
     call stop_timer(timer1)

     call start_timer(timer2)
     do j=1,3
        y=exp(0.007_r8_*j)
     end do
     call stop_timer(timer2)

     call start_timer(timer3)
     do j=1,2
        x=sin(0.005_r8_*i)
     end do
     call stop_timer(timer3)

     call start_timer(timer4)
     do j=1,3
        y=exp(0.007_r8_*j)
     end do
     call stop_timer(timer4)
  end do

  call print_timer_result(timer1,"c:sin()")
  call print_timer_result(timer2,"c:exp()")
  call print_timer_result(timer3,"F90:sin()")
  call print_timer_result(timer4,"F90:exp()")
  print *,"x,y = ",x,y

end program test_timers
