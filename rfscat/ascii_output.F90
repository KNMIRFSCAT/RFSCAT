MODULE ascii_output
  !--------------------------------
  !  #[ Documentation
  ! a small help module to allow easy creation
  ! of an ascii output file with windvector results
  !
  ! Written 2000, 2001, 2002 by  Jos de Kloe, KNMI.
  !
  ! Modifications:
  ! 08-May-2009 introduced keep type to hold all data for one wvc
  ! 12-May-2009 converted to some simple open,write,close routine
  !             to simplify the code
  ! 12-May-2009 renamed from keep.F90 to ascii_output.F90
  ! 20-May-2009 added FoM_skew and fixed a few typos
  ! 19-Jun-2009 added wvc_col_nr and wvc_row_nr to the output file
  ! 11-Sep-2009 implemented minimal outputfile
  !
  !  #]
  !  #[ Modules used
  USE numerics, only: r4_, missing_indicator_real_r4
  USE lunmanager, only: get_lun, free_lun
  USE inversion, only: max_nr_of_sol ! = 144 currently
  USE geometry, only: wvc_type
  USE errorhandler, only: no_error, error_writing_file, &
       error_opening_file
  USE convert, only: speeddir_to_u, speeddir_to_v
  !  #]
  !  #[ Variables and types
  IMPLICIT NONE  ! no implicit variable typing
  public         ! make all variables public by default

  ! unit for the ascii output file
  integer :: fileunit

  ! unit for the minimal ascii output file
  integer :: fileunit_minimal

  ! save the name in the open routine, to be used in the
  ! error message used in the write routine
  character(len=256) :: stored_filename

  ! save the name in the open routine, to be used in the
  ! error message used in the write routine
  character(len=256) :: stored_filename_minimal
  !  #]
  !--------------------------------
CONTAINS ! routines to handle the data in this module
  !--------------------------------
  subroutine open_ascii_outputfile(filename,nws,error_flag)
    !  #[
    character(len=*), intent(in)  :: filename   ! input
    integer,          intent(in)  :: nws        ! input
    integer,          intent(out) :: error_flag ! output

    error_flag = no_error
    !print *,'opening file: ',trim(filename),&
    !     ' for writing ascii vector results of inversion'

    ! save for later use
    stored_filename = filename

    fileunit = get_lun()
    open(unit=fileunit,file=trim(filename),status='new',action='write',&
         form="FORMATTED",ERR=99)
    
    write(fileunit,*) "format: "
    write(fileunit,*) "nr_of_simulated_wvcs"
    write(fileunit,*) "u v sp dir node_nr wvc_col_nr wvc_row_nr "//&
                      "lat lon nr_sol wqc cl "//&
                      "FOM_old FOM FOM_amb FOM_skew [for each simulated wvc]"
    write(fileunit,*) "sol found_u found_v "//&
                      "foundwindspeed foundwinddirection "//&
                      "MLE prob "//&
                      "[for each solution within a wvc]"
    write(fileunit,*) "actual_nr_of_simulated_wvcs"
    ! wqc = wind_quality_code
    
    write(fileunit,*) nws
    
    return

    ! error handler
99  print *,'ERROR while creating the output file:  ',trim(filename)
    error_flag = error_opening_file
    return

  end subroutine open_ascii_outputfile
    !  #]
  subroutine write_to_ascii_outputfile(wvc,error_flag)
    !  #[ 
    ! write result of one wvc to outputfile
    type(wvc_type), intent(in)  :: wvc
    integer,        intent(out) :: error_flag ! output

    ! local variables
    integer   :: sol
    real(r4_) :: found_u, found_v

    error_flag = no_error

    ! WARNING: on some compilers this line might break at the 80th column
    ! (especially with ifort and on SUN/NEC unix platforms)
    write(fileunit,*,err=99) &
         wvc%inp_u_orig,' ',&
         wvc%inp_v_orig,' ',&
         wvc%inp_speed_orig,' ',&
         wvc%inp_dir_orig,' ',&
         wvc%node_nr,' ',&
         wvc%wvc_column_number,' ',&
         wvc%wvc_row_number,' ',&
         wvc%lat,' ',&
         wvc%lon,' ',&
         wvc%inv_output%nr_of_windsolutions,' ',&
         wvc%inv_output%wind_quality_code,' ',&
         wvc%index_closest,' ',&
         wvc%FOM_old,' ',&
         wvc%FOM,' ',&
         wvc%FOM_amb,' ',&
         wvc%FOM_skew

    DO sol = 1,wvc%inv_output%nr_of_windsolutions
       found_u = speeddir_to_u(wvc%inv_output%foundwindspeed(sol),&
                               wvc%inv_output%foundwinddir(sol))
       found_v = speeddir_to_v(wvc%inv_output%foundwindspeed(sol),&
                               wvc%inv_output%foundwinddir(sol))
       write(fileunit,*,err=99) sol,' ',&
            found_u,' ',&
            found_v,' ',&
            wvc%inv_output%foundwindspeed(sol),' ',&
            wvc%inv_output%foundwinddir(sol),' ',   &
            wvc%inv_output%conedistance_measured(sol),' ', &
            wvc%inv_output%probability(sol)
    END DO
       
    return

    ! error handler
99  print *,'ERROR while writing the output file:  ',trim(stored_filename)
    error_flag = error_writing_file
    return
    
  end subroutine write_to_ascii_outputfile
  !  #]
  subroutine close_ascii_outputfile(nr_of_simulated_winds)
    !  #[
    integer, intent(in) :: nr_of_simulated_winds

    write(fileunit,*) nr_of_simulated_winds
    
    close(fileunit)
    CALL free_lun(fileunit)

  end subroutine close_ascii_outputfile
    !  #]
  !--------------------------------
  subroutine open_minimal_ascii_outputfile(filename_minimal,nws,error_flag)
    !  #[
    character(len=*), intent(in)  :: filename_minimal   ! input
    integer,          intent(in)  :: nws        ! input
    integer,          intent(out) :: error_flag ! output

    error_flag = no_error
    !print *,'opening file: ',trim(filename_minimal),&
    !     ' for writing minimal ascii vector results of inversion'

    ! save for later use
    stored_filename_minimal = filename_minimal

    fileunit_minimal = get_lun()
    open(unit=fileunit_minimal,file=trim(filename_minimal),&
         status='new',action='write',form="FORMATTED",ERR=99)
    
    write(fileunit_minimal,*) "format: "
    write(fileunit_minimal,*) "nr_of_simulated_wvcs"
    write(fileunit_minimal,*) "u v  node_nr wvc_col_nr wvc_row_nr "//&
                              "found_u (1st rank) found_v (1st rank) "//&
                              "conedist [for each simulated wvc]"
    write(fileunit_minimal,*) "actual_nr_of_simulated_wvcs"
   
    write(fileunit_minimal,*) nws
    
    return

    ! error handler
99  print *,'ERROR while creating the output file:  ',trim(filename_minimal)
    error_flag = error_opening_file
    return

  end subroutine open_minimal_ascii_outputfile
    !  #]
  subroutine write_to_minimal_ascii_outpfile(wvc,error_flag)
    !  #[ 
    ! write result of one wvc to outputfile
    type(wvc_type), intent(in)  :: wvc
    integer,        intent(out) :: error_flag ! output

    ! local variables
    integer   :: sol,ios
    real(r4_) :: found_u, found_v
    character(len=25) :: txt1,txt2,txt3,txt4,txt5,txt6,txt7,txt8

    error_flag = no_error

    write(txt1,*,iostat=ios) wvc%inp_u_orig
    if (ios .ne. 0) goto 99
    write(txt2,*,iostat=ios) wvc%inp_v_orig
    if (ios .ne. 0) goto 99
    write(txt3,*,iostat=ios) wvc%node_nr
    if (ios .ne. 0) goto 99
    write(txt4,*,iostat=ios) wvc%wvc_column_number
    if (ios .ne. 0) goto 99
    write(txt5,*,iostat=ios) wvc%wvc_row_number
    if (ios .ne. 0) goto 99

    sol = 1
    if (wvc%inv_output%nr_of_windsolutions .ge. 1) then
       found_u = speeddir_to_u(wvc%inv_output%foundwindspeed(sol),&
                               wvc%inv_output%foundwinddir(sol))
       found_v = speeddir_to_v(wvc%inv_output%foundwindspeed(sol),&
                               wvc%inv_output%foundwinddir(sol))
    else
      found_u = missing_indicator_real_r4
      found_v = missing_indicator_real_r4
    end if

    write(txt6,*,iostat=ios) found_u
    if (ios .ne. 0) goto 99
    write(txt7,*,iostat=ios) found_v
    if (ios .ne. 0) goto 99
    write(txt8,*,iostat=ios) wvc%inv_output%conedistance_measured(sol)
    if (ios .ne. 0) goto 99

    ! WARNING: on some compilers this line might break at the 80th column
    ! (especially with ifort and on SUN/NEC unix platforms)

    write(fileunit_minimal,"(7(a,1x),a)",err=99) &
         trim(adjustl(txt1)),trim(adjustl(txt2)),trim(adjustl(txt3)),&
         trim(adjustl(txt4)),trim(adjustl(txt5)),&
         trim(adjustl(txt6)),trim(adjustl(txt7)),trim(adjustl(txt8))
       
    return

    ! error handler
99  print *,'ERROR while writing the output file:  ',&
         trim(stored_filename_minimal)
    error_flag = error_writing_file
    return
    
  end subroutine write_to_minimal_ascii_outpfile
  !  #]
  subroutine close_minimal_ascii_outputfile(nr_of_simulated_winds)
    !  #[
    integer, intent(in) :: nr_of_simulated_winds

    write(fileunit_minimal,*) nr_of_simulated_winds

    close(fileunit_minimal)
    CALL free_lun(fileunit_minimal)

  end subroutine close_minimal_ascii_outputfile
    !  #]
  !--------------------------------
END MODULE ascii_output
