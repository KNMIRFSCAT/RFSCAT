program TestGnuPlotModule
  !  #[ documentation:
  !  a little test program to demonstrate and test
  !  the gnuplot_module
  !
  !  Modifications:
  !  08-Jan-2008 J. de Kloe  Initial version
  !
  !  #]
  !  #[ modules used
  USE gnuplot_module
  USE numerics, only: r8_
  !  #]
  !  #[ variables and parameters
  implicit none

  real(r8_), parameter :: pi=3.14159265358979323846264338328_r8_
  integer, parameter :: n_points = 101
  real(r8_), dimension(n_points) :: x,y1,y2
  integer :: i, error_flag

  ! this plotting object will hold all data and settings
  ! needed to make to plot
  type(gp_plot_type) :: gp
  !  #]
  !  #[ program code
  ! use a simple sine to define some test data for plotting
  do i=1,n_points
     x(i)=-pi+2._r8_*pi*(i-1)/(n_points-1)
     y1(i)=sin(x(i))
     y2(i)=cos(x(i)*2)
  end do

  ! init the plotting environment
  call gp_init_plot(gp,error_flag,output_format="ps")
  
  ! define some properties for this plot
  call gp_set_plot_property(gp,error_flag,&
       title="test for the gnuplot module",&
       xlabel="x", ylabel="sin(x)",&
       xrange=(/-pi,pi/),&
       yrange=(/-1.2_r8_,1.2_r8_/) )
  
  ! add the first dataset to the plot
  call gp_add_dataset(gp,x,y1,error_flag,&
       name="f(x)=sin(x)",&
       linecolor=gp_color_blue,&
       linestyle=gp_line_full,&
       datastyle=DataStyle_Lines)

  ! add the second dataset to the plot
  call gp_add_dataset(gp,x,y2,error_flag,&
       name="f(x)=cos(2x)",&
       linecolor=gp_color_green,&
       linestyle=gp_line_dashed,&
       datastyle=DataStyle_LinesPoints)

  ! add a horizontal line for y=0
  call gp_add_hor_line(gp,0._r8_,&
       error_flag,gp_color_blue,gp_line_dotted)

  ! add a vertical line for x=0
  call gp_add_ver_line(gp,0._r8_,&
       error_flag,gp_color_green,gp_line_dashed)

  ! add a red vertical bar between x=0.5 and x=0.6
  call gp_add_colored_ver_bar(gp,0.5_r8_,0.6_r8_,error_flag,gp_color_red)

  ! add a red horizontal green between y=-0.3 and y=-0.4
  call gp_add_colored_hor_bar(gp,-0.3_r8_,-0.4_r8_,error_flag,gp_color_green)

  ! add an arrow to point at some feature
  call gp_overplot_arrow(gp,&
       -pi/2,-0.7_r8_,&  ! from coordinate
       -pi/2,-1.0_r8_,& ! to coordinate
       error_flag)

  ! add a text label
  call gp_add_label(gp, "some text",&
       -2._r8_,0.8_r8_,& ! x,y position
       error_flag,&
       justify=gp_justify_left,&
       textcolor=gp_color_blue)
  
  ! create the plot and write it to file
  ! note that the file extension is added automatically
  ! depending on the type of output choosen (ps, eps or png for now)
  call gp_create_plot(gp,"Testplot",error_flag)

  ! done, clean-up the module
  call gp_close_plot(gp)

  !  #]
end program TestGnuPlotModule
