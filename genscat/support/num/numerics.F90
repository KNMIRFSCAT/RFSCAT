MODULE numerics
!
! Part of this software was developed within the context of the EUMETSAT
! Satellite Application Facility on Numerical Weather Prediction (NWP SAF),
! under the Cooperation Agreement dated 16 December, 2003, between EUMETSAT
! and the Met Office, UK, by one or more partners within the NWP SAF.
! The partners in the NWP SAF are the Met Office, ECMWF, KNMI and Meteo France.
!
!     Unit Name:             numerics
!
!     Created on:            27-11-2003
!
!     Last Modified on:      $Date: 2017-03-21 16:05:49 +0100 (Tue, 21 Mar 2017) $
!
!     Modifications Log:
!
! $Id: numerics.F90 12319 2017-03-21 15:05:49Z verhoefa $
! 
!  numerics.F90 11507 2016-03-25 09:01:58Z josdekloe $
! 
!  numerics.F90 11496 2016-03-21 16:05:53Z verhoefa $
! 
!  numerics.F90 8161 2012-08-08 12:33:05Z josdekloe $
! 
!  numerics.F90 7747 2011-11-15 06:59:35Z verhoefa $
!
! Revision 1.24  2006/03/28 10:57:55  vogelzan
! Included NWP SAF acknowledgement.
!
!
  !  #[ Documentation
  !---------------------------------------------------
  !   -a real type that presumably has the characteristics
  !    of 4 and 8 byte IEEE 754 floating-point types
  !    is defined.
  !
  !    Written by: J. de kloe, inspired on ideas from a module
  !                written by Aad van der Steen, UU
  !    first version 18-10-2001 by Jos de Kloe
  !
  !    Added 27-11-2003:  the code for handling missing_real/integers
  !                       as is usual in the scatterometry software
  !    Added 25-02-2004:  the do_range_check subroutines
  !    Added 16-03-2004:  the subroutine detect_variable_sizes
  !    Added 25-10-2005:  missing_integer definitions for 1, 2 and 4
  !                       byte integers
  !
  !    last change: 25-10-2005
  !---------------------------------------------------
  !  #]
  !  #[ global parameters
  IMPLICIT NONE
  integer, parameter :: i1_ = Selected_Int_Kind( 2)  ! = integer*1 
  integer, parameter :: i2_ = Selected_Int_Kind( 4)  ! = integer*2
  integer, parameter :: i4_ = Selected_Int_Kind( 9)  ! = integer*4
  integer, parameter :: i8_ = Selected_Int_Kind(18)  ! = integer*8
! gives compile errors on linux
!  integer, parameter :: i16_ = Selected_Int_Kind(36) ! = integer*16

  integer, parameter :: r4_ = Selected_Real_Kind( 6, 37)  ! = real*4
  integer, parameter :: r8_ = Selected_Real_Kind(15,307)  ! = real*8
! gives compile errors on linux
!  integer, parameter :: r16_ = Selected_Real_Kind(31,275) ! = real*16

  ! default integer and real types (4 bytes)
  integer, parameter :: r_ = r4_
  integer, parameter :: i_ = i4_
  ! r_ and i_ may be used as a general default for the whole program.
  ! Using these kinds removes the need for -i4 and -r4 options etc. on
  ! the command line, and thus should make programs more portable.

  ! maintained for compatibility reasons
  ! (to be phased out, dont use them any more in new code!)
  integer, parameter :: s_ = r4_
  integer, parameter :: l_ = r4_ ! should be r8_ to keep naming consistent

  integer, parameter :: nbytes_i1_ = 1
  integer, parameter :: nbytes_i2_ = 2
  integer, parameter :: nbytes_i4_ = 4
  integer, parameter :: nbytes_i8_ = 8
  integer, parameter :: nbytes_i_  = nbytes_i4_

  integer, parameter :: nbytes_r4_ = 4
  integer, parameter :: nbytes_r8_ = 8
  integer, parameter :: nbytes_r_  = nbytes_r4_

  ! WARNING: although these numbers usually gives the nr of bytes for
  ! these variable kinds in fortran90 there is no guarantee in the
  ! language definition that selected_int_kind and selected_real_kind
  ! will always return with these numbers !!!
  ! To be absolutely sure, use the check_variable_sizes() routine
  ! to check the sizes during runtime.

  !---------------------------------------------------
  ! SELECTED_INT_KIND(R) selects an integer type able to contain
  ! the range -10^R < n < 10^R
  !
  ! SELECTED_REAL_KIND(P,R) selects a real type able to contain
  ! at least P significant digits, and a decimal exponent range
  ! of at least R
  !---------------------------------------------------

  ! integer and real missing indicators
  ! for integers, use the maximum possible positive value
  ! (which can be obtained by the huge() command)
  integer(i1_), parameter :: missing_indicator_integer_i1_ = 127        ! = 2**7 -1
  integer(i2_), parameter :: missing_indicator_integer_i2_ = 32767      ! = 2**15-1
  integer(i4_), parameter :: missing_indicator_integer_i4_ = 2147483647 ! = 2**31-1
  integer(i8_), parameter :: missing_indicator_integer_i8_ = &
                                                9223372036854775807_i8_ ! = 2**63-1
  integer,      parameter :: missing_indicator_integer = (2**30-1)+2**30! = 2**31-1

  ! for reals use the usual number in scatterometry software
  real(r_),  parameter   :: missing_indicator_real    = 1.7E38     ! old: rMDI
  real(r4_), parameter   :: missing_indicator_real_r4 = 1.7E38_r4_ ! old: rMDI
  real(r8_), parameter   :: missing_indicator_real_r8 = 1.7E38_r8_ ! old: rMDI
  ! use the functions missing_real/missing_int to check these conditions

  ! REMARK: these missing values are HARDCODED into the ECMWF BUFR library
  ! but are adjustable for the GRIB library.
  ! For BUFR, do a grep on "NVIND=" or "RVIND=" to find the values.
  ! NVIND = (2**30-1)+2**30 =  2**31-1 = 2147483647
  ! RVIND = 1.7E38

  ! WARNING: these parameters should have the same values as the ones
  ! defined in the ErrorHandler module, otherwise the error printing
  ! routine will produce incorrect results...
  integer, parameter :: no_error                = 0
  integer, parameter :: error_numerics_intsize  = 50201
  integer, parameter :: error_numerics_realsize = 50211

  !  #]
  !  #[ interfaces for overloading the missing functions
  ! overload the function missing_int
  INTERFACE missing_int
     module procedure missing_int1, missing_int2, missing_int4, missing_int8
  END INTERFACE

  ! overload the function missing_real
  INTERFACE missing_real
     module procedure missing_real4, missing_real8 !, missing_real16
  END INTERFACE
  ! overload the function real2txt
  INTERFACE real2txt
     module procedure real4_2txt, real8_2txt
  END INTERFACE
  !  #]
CONTAINS
  !---------------------------------------------------
  subroutine detect_variable_sizes
    !  #[
    ! determines nr of bytes in a variable of the given kind_type

    integer      :: test_i  ! default_integer
    integer(i1_) :: test_i1
    integer(i2_) :: test_i2
    integer(i4_) :: test_i4
    integer(i8_) :: test_i8
!    integer(i16_) :: test_i16 ! gives compile error, since i16_ = -1
    real         :: test_dr ! default_real
    real(s_)     :: test_s
    real(l_)     :: test_l
    real(r_)     :: test_r
    real(r4_)    :: test_r4
    real(r8_)    :: test_r8
!    real(r16_)   :: test_r16 ! fails on 32 bit linux

    integer :: var_length

    write (*,"(a8,1X,a11,1X,a5,1X,a9)") &
         "var_type","nr_of_words","range","precision"

    inquire(iolength=var_length) test_i
    write (*,"(a8,1X,i11,1X,i5)") "i",var_length,range(test_i)

    inquire(iolength=var_length) test_i1
    write (*,"(a8,1X,i11,1X,i5)") "i1_",var_length,range(test_i1)

    inquire(iolength=var_length) test_i2
    write (*,"(a8,1X,i11,1X,i5)") "i2_",var_length,range(test_i2)

    inquire(iolength=var_length) test_i4
    write (*,"(a8,1X,i11,1X,i5)") "i4_",var_length,range(test_i4)

    inquire(iolength=var_length) test_i8
    write (*,"(a8,1X,i11,1X,i5)") "i8_",var_length,range(test_i8)

! a 16 byte integer does not exist on SGI-IRIX, but maybe on other machines?
!    inquire(iolength=var_length) test_i16
!    write (*,"(a8,1X,i11,1X,i5)") "i16_",var_length,range(test_i16)

    inquire(iolength=var_length) test_dr
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "dr",var_length,range(test_dr),precision(test_dr)

    inquire(iolength=var_length) test_s
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "s_",var_length,range(test_s),precision(test_s)

    inquire(iolength=var_length) test_l
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "l_",var_length,range(test_l),precision(test_l)

    inquire(iolength=var_length) test_r
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "r_",var_length,range(test_r),precision(test_r)

    inquire(iolength=var_length) test_r4
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "r4_",var_length,range(test_r4),precision(test_r4)

    inquire(iolength=var_length) test_r8
    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
         "r8_",var_length,range(test_r8),precision(test_r8)

!    inquire(iolength=var_length) test_r16
!    write (*,"(a8,1X,i11,1X,i5,1X,i9)") &
!         "r16_",var_length,range(test_r16),precision(test_r16)

  end subroutine detect_variable_sizes
  !---------------------------------------------------
  !  #]
  subroutine check_variable_sizes(error_flag)
    !  #[
    integer, intent(out) :: error_flag

    ! local variables
    integer      :: nbytes
    integer(i1_) :: value_i1_
    integer(i2_) :: value_i2_
    integer(i4_) :: value_i4_
    integer(i8_) :: value_i8_
    integer(i_)  :: value_i_
    real(r4_)    :: value_r4_
    real(r8_)    :: value_r8_
    real(r_)     :: value_r_

    ! initialize error flag
    error_flag = no_error
    
    ! see how many bytes there are in the default integer i_ datatype
    inquire(iolength=nbytes) value_i1_
    IF (nbytes .ne. nbytes_i1_) THEN 
       print *,"ERROR: size of integer kind i1_ is not the expected ",&
            nbytes_i1_," byte"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_intsize
    END IF
    inquire(iolength=nbytes) value_i2_
    IF (nbytes .ne. nbytes_i2_) THEN
       print *,"ERROR: size of integer kind i2_ is not the expected ",&
            nbytes_i2_," bytes"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_intsize
    END IF
    inquire(iolength=nbytes) value_i4_
    IF (nbytes .ne. nbytes_i4_) THEN
       print *,"ERROR: size of integer kind i4_ is not the expected ",&
            nbytes_i4_," bytes"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_intsize
    END IF
    inquire(iolength=nbytes) value_i8_
    IF (nbytes .ne. nbytes_i8_) THEN
       print *,"ERROR: size of integer kind i8_ is not the expected ",&
            nbytes_i8_," byte"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_intsize
    END IF
    inquire(iolength=nbytes) value_i_
    IF (nbytes .ne. nbytes_i_) THEN
       print *,"ERROR: size of the default integer kind i_ is not",&
            " the expected ",nbytes_i_," bytes"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_intsize
    END IF

    inquire(iolength=nbytes) value_r4_
    IF (nbytes .ne. nbytes_r4_) THEN
       print *,"ERROR: size of real kind r4_ is not the expected ",&
            nbytes_r4_," byte"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_realsize
    END IF
    inquire(iolength=nbytes) value_r8_
    IF (nbytes .ne. nbytes_r8_) THEN
       print *,"ERROR: size of real kind r8_ is not the expected ",&
            nbytes_r8_," byte"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_realsize
    END IF
    inquire(iolength=nbytes) value_r_
    IF (nbytes .ne. nbytes_r_) THEN
       print *,"ERROR: size of the default real kind r_ is not the expected ",&
            nbytes_r4_," byte"
       print *,"but seems to be ",nbytes," bytes ..."
       error_flag = error_numerics_realsize
    END IF

    IF (error_flag .ne. no_error) THEN
       print *,"ERROR in module numercs, subroutine check_variable_sizes():"
       print *,"One of the variable kinds defined by the numerics module"
       print *,"does not have the expected nr of bytes !!!!"
       print *,"This may have serious consequences for handling"
       print *,"unformatted data !!!! and should be corrected before"
       print *,"using this program on this platform......."
    END IF

  end subroutine check_variable_sizes
    !  #]
  !---------------------------------------------------
  function missing_real4(x) result(m)
    !  #[
    real(r4_) :: x
    logical  :: m
    m=.false.
    if (x .ge. (missing_indicator_real*0.99)) m=.true.
  end function missing_real4
  !---------------------------------------------------
  !  #]
  function missing_real8(x) result(m)
    !  #[
    real(r8_) :: x
    logical  :: m
    m=.false.
    if (x .ge. (missing_indicator_real*0.99)) m=.true.
  end function missing_real8
  !---------------------------------------------------
  !  #]
! not all platforms have real*16 defined!
!  function missing_real16(x) result(m)
    !  #[
!    real(r16_) :: x
!    logical  :: m
!    m=.false.
!    if (x .ge. (missing_indicator_real*0.99)) m=.true.
!  end function missing_real16
  !---------------------------------------------------
    !  #]
  function missing_int1(x) result(m)
    !  #[
    integer(i1_) :: x
    logical :: m
    m=.false.
    if (x .eq. (missing_indicator_integer_i1_)) m=.true.
  end function missing_int1
  !---------------------------------------------------
  !  #]
  function missing_int2(x) result(m)
    !  #[
    integer(i2_) :: x
    logical :: m
    m=.false.
    if (x .eq. (missing_indicator_integer_i2_)) m=.true.
  end function missing_int2
  !---------------------------------------------------
  !  #]
  function missing_int4(x) result(m)
    !  #[
    integer(i4_) :: x
    logical :: m
    m=.false.
    if (x .eq. (missing_indicator_integer_i4_)) m=.true.
  end function missing_int4
  !---------------------------------------------------
  !  #]
  function missing_int8(x) result(m)
    !  #[
    integer(i8_) :: x
    logical :: m
    m=.false.
    if (x .eq. (missing_indicator_integer_i8_)) m=.true.
  end function missing_int8
  !---------------------------------------------------
  !  #]
  !---------------------------------------------------
  function convert_missing_real(x1) result(x2)
    !  #[
    ! convert the missing indicator number to the number -999.0
    ! which is much more convenient for writing to formatted output files
    real(r_)    :: x1,x2
    x2=x1
    if (missing_real(x1)) x2=-999.0
  end function convert_missing_real
  !---------------------------------------------------
  !  #]
  function convert_missing_integer(i1) result(i2)
    !  #[
    ! convert the missing indicator number to the number -999
    ! which is much more convenient for writing to formatted output files
    integer :: i1,i2
    i2=i1
    if (missing_int(i1)) i2=-999
  end function convert_missing_integer
  !  #]
  !---------------------------------------------------
  function int2real(i) result(x)
    !  #[

    ! convert an integer to a real, taking into account that 
    ! the input integer might have the missing value
    integer  :: i
    real(r_) :: x
    x = real(i,r_)
    if (missing_int(i)) x = missing_indicator_real
  end function int2real
  !  #]
  function real2int(x) result(i)
    !  #[
    ! convert a real to an integer, taking into account that 
    ! the input real might have the missing value
    real(r_) :: x
    integer  :: i
    if (missing_real(x)) then 
       i = missing_indicator_integer
    else
       i = nint(x)
    endif
  end function real2int
    !  #]
  function real4_2txt(r,acc) result(txt)
    !  #[
    ! convert a real to an integer, taking into account that
    ! the input real might have the missing value

    ! if needed ad an optional format definition in the call to this function

    real(r4_),         intent(in)  :: r
    integer, optional, intent(in)  :: acc 
    character(len=13)              :: txt ! result

    ! local variable
    character(len=10) :: formatstring
    
    formatstring = "(es13.6e2)"
    IF (present(acc)) THEN
       IF ((acc .ge. 1) .and. (acc .le. 9)) THEN
          write(formatstring(7:7),"(i1)") acc
       END IF
    END IF

    if (missing_real(r)) then
       txt = "[missing]"
    else
       write(txt,formatstring) r
    endif

  end function real4_2txt
    !  #]
  function real8_2txt(r,acc) result(txt)
    !  #[
    ! convert a real to an integer, taking into account that
    ! the input real might have the missing value

    ! if needed ad an optional format definition in the call to this function

    real(r8_),         intent(in)  :: r
    integer, optional, intent(in)  :: acc 
    character(len=13)              :: txt ! result

    ! local variable
    character(len=10) :: formatstring
    
    formatstring = "(es13.6e2)"
    IF (present(acc)) THEN
       IF ((acc .ge. 1) .and. (acc .le. 9)) THEN
          write(formatstring(7:7),"(i1)") acc
       END IF
    END IF

    if (missing_real(r)) then
       txt = "[missing]"
    else
       write(txt,formatstring) r
    endif

  end function real8_2txt
    !  #]
  !---------------------------------------------------
!  subroutine do_range_check_int(val,min,max,inside_range,txt)
    !  #[
!    ! check whether the value is inside the specified range
!    ! if not, change the value of inside_range
!
!    ! WARNING: the * for length of "txt" is essential. On Linux systems
!    ! the program  will not compile if you use a fixed number (256) and
!    ! call the routine with a constant string of different length !!!
!    integer, intent(in)           :: val, min, max
!    character(len=*),  intent(in) :: txt
!    logical, intent(inout)        :: inside_range
!
!    IF (missing_int(val)) return
!
!    IF ( (val .lt. min) .or. (val .gt. max) ) THEN 
!       print *,"range check: ",trim(txt),val
!       inside_range = .false.
!    END IF
!  end subroutine do_range_check_int
  !---------------------------------------------------
    !  #]
!  subroutine do_range_check_real(val,min,max,inside_range,txt)
    !  #[
!    ! check whether the value is inside the specified range
!    ! if not, change the value of inside_range
!
!    ! WARNING: the * for length of "txt" is essential. On Linux systems
!    ! the program  will not compile if you use a fixed number (256) and
!    ! call the routine with a constant string of different length !!!
!    real(r_), intent(in)          :: val, min, max
!    character(len=*),  intent(in) :: txt
!    logical, intent(inout)        :: inside_range
!
!    IF (missing_real(val)) return
!
!    IF ( (val .lt. min) .or. (val .gt. max) ) THEN 
!       print *,"range check: ",trim(txt),val
!       inside_range = .false.
!    END IF
!  end subroutine do_range_check_real
    !  #]
  !---------------------------------------------------
  function real2dB(x) result(val)
    !  #[
    real(r_), intent(in) :: x
    real(r_) :: val
    IF (missing_real(x)) THEN
       val = x
    ELSE
       IF (x .lt. 0.0) THEN
          print *,"ERROR: inside real2dB()"
          print *,"converting negative numbers to dB values is not possible !"
          print *,"possibly this value is already in dB"
          print *,"If needed, first take the abs() value"
          print *," input was: x = ",x
          print *,"returning with missing value"
          val = missing_indicator_real
       ELSE
          val = 10.*LOG10(x)
       END IF
    END IF
  end function real2dB
  !  #]
  function dB2real(x) result(val)
    !  #[
    real(r_), intent(in) :: x
    real(r_) :: val
    IF (missing_real(x)) THEN
       val = x
    ELSE
       val = 10.**(0.1*x)
    END IF
  end function dB2real
  !  #]
  !---------------------------------------------------

  subroutine get_i1(x,ixlen,pos,a)
    !  #[
    !
    ! Description: get unsigned integer value from 1 byte in character array
    !
    character,intent(in) :: x(ixlen)
    integer,intent(in)   :: ixlen
    integer,intent(in)   :: pos
    integer,intent(out)  :: a
    !

    a = ichar(x(pos))

  end subroutine get_i1
    !  #]

  subroutine get_ui2(x,ixlen,pos,a)
    !  #[
    !
    ! Description: get unsigned integer value from 2 bytes in character array
    !
    character,intent(in) :: x(ixlen)
    integer,intent(in)   :: ixlen
    integer,intent(in)   :: pos
    integer,intent(out)  :: a
    !

    a = ichar(x(pos))
    a = a * 256 + ichar(x(pos+1))

  end subroutine get_ui2
    !  #]

  subroutine get_i2(x,ixlen,pos,a)
    !  #[
    !
    ! Description: get integer value from 2 bytes in character array
    !
    character,intent(in) :: x(ixlen)
    integer,intent(in)   :: ixlen
    integer,intent(in)   :: pos
    integer,intent(out)  :: a
    !

    a = ichar(x(pos))
    if (a .ge. 128) a = a - 256
    a = a * 256 + ichar(x(pos+1))

  end subroutine get_i2
    !  #]

  subroutine get_i4(x,ixlen,pos,a)
    !  #[
    !
    ! Description: get integer value from 4 bytes in character array
    !
    character,intent(in) :: x(ixlen)
    integer,intent(in)   :: ixlen
    integer,intent(in)   :: pos
    integer,intent(out)  :: a
    !

    a = ichar(x(pos))
    if (a .ge. 128) a = a - 256
    a = a * 256 + ichar(x(pos+1))
    a = a * 256 + ichar(x(pos+2))
    a = a * 256 + ichar(x(pos+3))

    ! JK: remark, this could also be implemented like this:
    ! a = transfer(x(pos:pos+3),a)
    ! also an optional do_byteswap switch may be usefull here.
    !
    ! read/write routines for 1,2,4,8 byte integers
    ! 8 byte reals and 1 byte logicals are available in the
    ! Aeolus L2B software if you need example code
    
  end subroutine get_i4
    !  #]

END Module numerics
