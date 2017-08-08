program TestGnuPlotModule2
  !  #[ documentation:
  !  a little test program to demonstrate and test
  !  the gnuplot_module 3d plotting feature
  !
  !  Modifications:
  !  12-Jan-2008 J. de Kloe  Initial version
  !
  !  #]
  !  #[ modules used
  USE gnuplot_module
  USE numerics, only: r8_
  !  #]
  !  #[ variables and parameters
  implicit none

  integer, parameter :: n_points_x = 101
  integer, parameter :: n_points_y = 101
  real(r8_), dimension(n_points_x) :: x
  real(r8_), dimension(n_points_y) :: y
  real(r8_), dimension(n_points_x,n_points_y) :: z
  integer :: i, j, error_flag

  ! this plotting object will hold all data and settings
  ! needed to make to plot
  type(gp_plot_type) :: gp
  !  #]
  !  #[ program code
  ! use a simple sine to define some test data for plotting
  do i=1,n_points_x
     x(i)=-10._r8_+20._r8_*(i-1)/(n_points_x-1)
     do j=1,n_points_y
        y(j)=-10._r8_+20._r8_*(j-1)/(n_points_y-1)
        !z(i,j)=100._r8_ - x(i)*x(i) + y(j)*y(j)
        z(i,j)=0._r8_ + x(i)*x(i) + y(j)*y(j)
     end do
  end do

  ! init the plotting environment
  !call gp_init_plot(gp,error_flag,output_format="ps",&
  !                  debug_prints=.true.)
  call gp_init_plot(gp,error_flag,output_format="png")
  
  ! define some properties for this plot
  call gp_set_plot_property(gp,error_flag,&
       title="f(x,y) = x*x+y+y",&
       xlabel="x", ylabel="y",zlabel="",&
       ContourPlot=.true.,&
       ShadePlot=.true.,&
       AddContourAtOne=.true.,&
       !Nr_Contour_Levels=8,&
       Contour_Levels=real((/1.,2.,5.,6.,10.,20.,30.,50.,&
                             100.,150.,160.,170.,180./),r8_) )
  
  ! add the dataset to the plot
  call gp_add_3d_dataset(gp,x,y,z,error_flag,&
                         name="f(x,y)=100-x^2+y^2",&
                         datafilename="data3d.dat",&
                         contourdatafilename="contourdata.dat",&
                         linecolor=gp_color_black,&
                         linestyle=gp_line_full)

  ! create the plot and write it to file
  ! note that the file extension is added automatically
  ! depending on the type of output choosen (ps, eps or png for now)
  call gp_create_plot(gp,"Testplot3d",error_flag,&
                      cmdfile="commands.txt",&
                      cmdfile_contours="contour_commands.txt")
!                      display=.false.)

  ! done, clean-up the module
  call gp_close_plot(gp)
  !  #]
end program TestGnuPlotModule2
