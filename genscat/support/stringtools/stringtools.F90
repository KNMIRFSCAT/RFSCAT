module StringTools
  !  #[ Description
  !
  ! a module that collects some simple tools
  ! to make string handling easier
  ! Created 02-Feb-2006 by J. de Kloe
  !
  ! Modifications:
  !   01-Mar-2006 J. de Kloe  Added contains_spaces()
  !   08-Jun-2006 J. de Kloe  Added write_complex_number()
  !   07-Sep-2006 J. de Kloe  Added Split_path_and_filename()
  !   08-Sep-2006 J. de Kloe  Added Join_path_and_filename()
  !   13-Dec-2006 J. de Kloe  Bugfix for ifort in Added Join_path_and_filename
  !   26-Oct-2007 J. de Kloe  added new subroutine Split_filename_and_extension
  !   16-Jan-2008 J. de Kloe  phase out integer kind i_ 
  !   10-Mar-2008 J. de Kloe  removed bug in Split_filename_and_extension
  !   26-May-2009 J. de Kloe  added string_list type and handlers
  !  #]
  !  #[ Modules used
  USE ErrorHandler, only: no_error, error_allocate, error_string_handling
  USE Compiler_Features, only: retchar, newline
  USE Numerics, only: r8_
  !  #]
  !  #[ Types, variables and parameters
  implicit none
  character(len=1), parameter :: space   = ' '

  ! 2 simple types to allow implementing a linked list of string-chuncks
  ! usefull when you have to concatenate many small string chuncks together
  ! and don't know in advance what the total string length will be.
  ! (inspired on the list type of python)
  type string_chunck_type
     character(len=1), dimension(:), pointer :: chunck
     type(string_chunck_type),       pointer :: next_chunck
  end type string_chunck_type

  type string_list_type
     type(string_chunck_type), pointer :: first_chunck
     type(string_chunck_type), pointer :: last_chunck
     integer                           :: num_chuncks
  end type string_list_type

  !  #]
contains
  !--------------------------------------------------
  function to_lowercase(txt_in) result(txt_out)
    !  #[
    character(len=*)           :: txt_in  ! input
    character(len=len(txt_in)) :: txt_out ! result

    ! local variables
    integer          :: i,n
    character(len=1) :: c
    integer, parameter :: offset_to_lowercase =  ichar('a') - ichar('A')

    txt_out(:) = ' '
    n = len_trim(txt_in)

    IF (n .gt. 0) THEN
       DO i=1,n
          c = txt_in(i:i)
          IF ( (c .ge. 'A') .and. (c .le. 'Z')) THEN
             c = achar(ichar(c)+offset_to_lowercase)
          END IF
          txt_out(i:i) = c
       END DO
    END IF

  end function to_lowercase
    !  #]
  function to_uppercase(txt_in) result(txt_out)
    !  #[
    character(len=*)           :: txt_in  ! input
    character(len=len(txt_in)) :: txt_out ! result

    ! local variables
    integer          :: i,n
    character(len=1) :: c
    integer, parameter :: offset_to_uppercase =  ichar('A') - ichar('a')

    txt_out(:) = ' '
    n = len_trim(txt_in)

    IF (n .gt. 0) THEN
       DO i=1,n
          c = txt_in(i:i)
          IF ( (c .ge. 'a') .and. (c .le. 'z')) THEN
             c = achar(ichar(c)+offset_to_uppercase)
          END IF
          txt_out(i:i) = c
       END DO
    END IF

  end function to_uppercase
    !  #]
  function chararray2string(chararray) result(text)
    !  #[
    character(len=1), dimension(:) :: chararray ! input
    character(len=size(chararray)) :: text      ! output

    integer :: n, i

    text(:) = ' '
    n = size(chararray)

    ! copy the data from the char array to a string
    ! dont use transfer here, since then the type will be converted
    ! in the same memory location, which is not how a function
    ! should behave
    DO i=1,n
       text(i:i) = chararray(i)
    END DO

  end function chararray2string
    !  #]
  function string2chararray(text) result(chararray)
    !  #[
    character(len=*)                       :: text      ! input
    character(len=1), dimension(len(text)) :: chararray ! output

    integer :: n, i

    chararray(:) = ' '
    n = len_trim(text)

    ! copy the text from the string to the char array
    ! dont use transfer here, since then the type will be converted
    ! in the same memory location, which is not how a function
    ! should behave
    DO i=1,n
        chararray(i) = text(i:i)
    END DO
    
  end function string2chararray
    !  #]
  function remove_eol(txt_in) result(txt_out)
    !  #[ remove cr/newline characters
    character(len=*)           :: txt_in  ! input
    character(len=len(txt_in)) :: txt_out ! result

    ! local variables
    integer          :: i_in, i_out, n
    character(len=1) :: c

    txt_out(:) = ' '
    n = len_trim(txt_in)

    IF (n .gt. 0) THEN
       i_out=0
       DO i_in=1,n
          c = txt_in(i_in:i_in)
          IF ( (c .eq. retchar) .or. (c .eq. newline)) THEN
             ! just ignore this character
          ELSE
             ! copy this character
             i_out = i_out+1
             txt_out(i_out:i_out) = c
          END IF
       END DO
    END IF

  end function remove_eol
    !  #]
  function contains_spaces(text) result(spaces_present)
    !  #[ check if any spaces are present
    character(len=*) :: text           ! input
    logical          :: spaces_present ! result
    
    ! local variable
    integer :: i

    ! I could also use the scan() function to do this
    spaces_present = .false. ! assume no spaces
    scanloop: DO i=1,len_trim(text)
       if (text(i:i) .eq. space) then
          spaces_present = .true.
          exit scanloop
       end if
    END DO scanloop

  end function contains_spaces
    !  #]
  function contains_crnl(text) result(crnl_present)
    !  #[ check if a cr and/or newline character is present
    character(len=*) :: text          ! input
    logical          :: crnl_present ! result
    
    ! local variable
    integer :: i

    ! I could also use the scan() function to do this
    crnl_present = .false. ! assume no cr/nl characters
    scanloop: DO i=1,len_trim(text)
       if ( (text(i:i) .eq. newline) .or. &
            (text(i:i) .eq. retchar)      ) then
          crnl_present = .true.
          exit scanloop
       end if
    END DO scanloop

  end function contains_crnl
    !  #]
  function write_complex_number(c) result(c_string)
    !  #[
    ! REMARK: the L2BP difftool recognises only complex numbers that have
    ! the shape (a,b) without any spaces within the brackets. Most compilers
    ! use this form. However, the gfortran compiler is an exception, because
    ! it does write some extra spaces inside the brackets. Therefore the 
    ! following Write_Complex_Number() function was added. 
    ! For the moment the TestArraytools program is the only one in the L2BP
    ! that actually prints complex numbers. 
    ! Note that also the module RayleighBrillouinProcessing/tentispectrum.F90
    ! uses complex numbers, but these are not printed.
    ! JK, 8-6-2006

    complex(r8_), intent(in) :: c        ! input
    character(len=100)       :: c_string ! result

    ! local variables
    character(len=1), parameter :: space = ' '
    character(len=100) :: tmp_string
    integer            :: i,j

    ! init
    c_string(:)   = space
    tmp_string(:) = space

    ! convert the complex number to a string
    write(tmp_string,*,err=999) c

    ! remove all the spaces from the string
    j=0
    DO i=1,len_trim(tmp_string)
       IF (tmp_string(i:i) .ne. space) THEN
          j=j+1
          c_string(j:j) = tmp_string(i:i)
       END IF
    END DO

    ! done
    return

    ! warning: printing an error is not possible here, because then
    ! you cannot use this function as parameter to a print statement
    ! anymore (you will get recursive io errors)
    ! therefore just report "[conversion error]" if something is wrong...
999 c_string = "[conversion error]"
    return

  end function write_complex_number
    !  #]
  subroutine Split_text(text_in, divider, part1, part2, backward)
    !  #[
    character(len=*),  intent(in)  :: text_in
    character(len=1),  intent(in)  :: divider
    character(len=*),  intent(out) :: part1
    character(len=*),  intent(out) :: part2
    logical, optional, intent(in)  :: backward

    ! local variables
    integer :: i, divider_pos, length
    logical :: search_backward

    search_backward = .false.
    if (present(backward)) search_backward = backward

    part1(:) = ' '
    part2(:) = ' '

    ! find the first or last occurrence of divider
    divider_pos = -1
    length = len_trim(text_in)
    searchloop: do i=1,length
       if (text_in(i:i) .eq. divider) then
          divider_pos=i
          if (.not. search_backward) exit searchloop
       end if
    end do searchloop

    if (divider_pos .eq. -1) then
       part1 = text_in
       return
    else
       part1 = text_in(1:divider_pos)
       part2 = text_in(divider_pos+1:length)
    end if
    
  end subroutine Split_text
    !  #]
  subroutine Split_path_and_filename(path_and_filename,path,filename)
    !  #[
    character(len=*),   intent(in)  :: path_and_filename
    character(len=256), intent(out) :: path
    character(len=256), intent(out) :: filename
    
    ! method: assume we are on a unix/linux like system, so just find
    ! the last slash and split the string at that point
    ! (or if no slashes are present, assume path is empty and just
    ! return the filename)
    ! NOTE: if we have to be compatible with windows (hope that will
    ! never happen), then we need to check backslashes .....

    path(:)     = ' '
    filename(:) = ' '

    ! in case no slash is present, part 1 is the filename
    if (index(path_and_filename,'/') .eq. 0) then
       filename = path_and_filename
    else
       ! find the last slash
       call Split_text(path_and_filename,'/',path,filename,backward=.true.)
    end if

  end subroutine Split_path_and_filename
    !  #]
  subroutine Split_path_and_filename_old(path_and_filename,path,filename)
    !  #[
    character(len=*),   intent(in)  :: path_and_filename
    character(len=256), intent(out) :: path
    character(len=256), intent(out) :: filename
    
    ! method: assume we are on a unix/linux like system, so just find
    ! the last slash and split the string at that point
    ! (or if no slashes are present, assume path is empty and just
    ! return the filename)
    ! NOTE: if we have to be compatible with windows (hope that will
    ! never happen), then we need to check backslashes .....

    ! local variables
    integer :: i, slash_pos, length

    path(:)     = ' '
    filename(:) = ' '

    ! find the last slash
    slash_pos = -1
    length = len_trim(path_and_filename)
    do i=1,length
       if (path_and_filename(i:i) .eq. '/') slash_pos=i
    end do

    if (slash_pos .eq. -1) then
       filename = path_and_filename
       return
    else
       path     = path_and_filename(1:slash_pos)
       filename = path_and_filename(slash_pos+1:length)
    end if
    
  end subroutine Split_path_and_filename_old
    !  #]
  function Join_path_and_filename(path,filename) result(path_and_filename)
    !  #[
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: filename
    character(len=len(path)+len(filename)+1) :: path_and_filename ! result
! WARNING:
! the following line DOES NOT work properly when compiled with the ifort
! compiler (version 8.1). Later versions seem to have fixed this bug, but
! since I have 8.1 installed, please don't use len_trim() to determine
! the size of function results for the moment .....
!    character(len=len_trim(path)+len_trim(filename)+1) :: path_and_filename ! result
    
    ! method: assume we are on a unix/linux like system, so glue path and
    ! filename using a slash.
    ! NOTE: if we have to be compatible with windows (hope that will
    ! never happen), then we need to add a backslash .....

    path_and_filename(:)     = ' '

    if (len_trim(path) .eq. 0) then
       ! no path to add, so just return the filename
       path_and_filename = filename
    else
       if (path(len_trim(path):len_trim(path)) .eq. '/') then
!print *,"test inside: [",trim(path),"]"
!print *,"test inside: [",trim(filename),"]"
          ! the path already ends with a slash, so it doesn't need to be added
          path_and_filename = trim(path)//trim(filename)
!print *,"test inside: [",trim(path_and_filename),"]"
!print *,"test inside: len=",len_trim(path)+len_trim(filename)+1
       else
          ! add a slash in between path and filename
          path_and_filename = trim(path)//'/'//trim(filename)
       end if
    end if

    return

  end function Join_path_and_filename
    !  #]
  subroutine Split_filename_and_extension(path_filename_and_extension,&
                                          path_and_filename,extension)
    !  #[
    character(len=*),   intent(in)  :: path_filename_and_extension
    character(len=256), intent(out) :: path_and_filename
    character(len=256), intent(out) :: extension
    
    ! Method: find the last dot and split the string at that point
    ! (or if no dots are present, assume there is no extension).
    ! A path might be present, which needs to be removed before inspecting
    ! the filename, because it is legal to have dots in directory names,
    ! which will otherwise confuse this algorithm.
    ! Exception: if the only dot in the filename is at the first character, 
    ! the file is a hidden unix/linux file, and no extension is present.

    ! local variable
    integer :: i, dot_pos, length
    character(len=256) :: path
    character(len=256) :: filename
    character(len=256) :: filename_and_extension

    ! init
    path(:)                   = ' '
    filename(:)               = ' '
    filename_and_extension(:) = ' '
    extension(:)              = ' '
    path_and_filename(:)      = ' '


    ! first remove a path, which might be prepended
    ! this is needed, because a pathname may contain dots, which do NOT
    ! indicate the start of an extension, and which will confuse
    ! the simple search below for files without an extension....
    call Split_path_and_filename(path_filename_and_extension,&
                                 path,filename_and_extension)

    ! find the last dot
    dot_pos = -1
    length = len_trim(filename_and_extension)
    do i=1,length
       if (filename_and_extension(i:i) .eq. '.') dot_pos=i
    end do

    if (dot_pos .eq. -1) then
       ! no dots where present in the filename
       path_and_filename = path_filename_and_extension
       return
    else
       if (dot_pos .eq.1) then
          ! the last and only  dot is found at pos 1.
          ! These are the hidden files in unix/linux
          path_and_filename = path_filename_and_extension
          return
       else
          filename  = filename_and_extension(1:dot_pos-1)
          extension = filename_and_extension(dot_pos+1:length)
          ! finally, combine the filename again with the path
          ! before reporting the result
          path_and_filename = Join_path_and_filename(path,filename)
          return
       end if
    end if
    
  end subroutine Split_filename_and_extension
    !  #]
  function string_replace(text, search, replace) result(adapted_text)
    !  #[ replace part of a string
    character(len=*), intent(in) :: text, search, replace
    character(len=len(text)+len(replace)-len(search)) :: adapted_text

    ! local variable
    integer :: i, s1, s2
    
    ! init
    adapted_text(:) = ' '
    
    ! search for the substring
    i = index(text, search)
    
    ! check for success
    if (i .eq. 0) then
       ! string not found, just return unaltered original
       adapted_text = text
       return
    end if

    ! construct the resulting string
    s1 = len(search)
    s2 = len(text)
    adapted_text = text(1:i-1) // trim(replace) // text(i+s1:s2)
    return

  end function string_replace
    !  #]
  subroutine remove_quotes(txt, error_flag)
    !  #[ remove a pair of single or double quotes from a string
    character(len=*), intent(inout) :: txt
    integer, intent(out) :: error_flag

    ! local variable
    integer :: n

    error_flag = no_error
    
    ! assume the quoted string is already left adjusted
    n=len_trim(txt)
    
    ! remove double quotes
    if (txt(1:1) .eq. "'") then
       if (txt(n:n) .eq. "'") then
          txt = txt(2:n-1)
          return
       else
          print *,"ERROR: non-matching quotes in string: "//'['//trim(txt)//']'
          error_flag = error_string_handling
          return
       end if
    end if

    ! remove double quotes
    if (txt(1:1) .eq. '"') then
       if (txt(n:n) .eq. '"') then
          txt = txt(2:n-1)
          return
       else
          print *,"ERROR: non-matching quotes in string: "//'['//trim(txt)//']'
          error_flag = error_string_handling
          return
       end if
    end if

    ! no quotes found, this is also a valid condition
    return

  end subroutine remove_quotes
    !  #]

  !--------------------------------------------------
  ! routines to handle the string_list_type structure
  !--------------------------------------------------
  subroutine init_string_list(str_list)
    !  #[
    type(string_list_type), intent(out) :: str_list
    
    nullify(str_list%first_chunck)
    nullify(str_list%last_chunck)
    str_list%num_chuncks = 0

  end subroutine init_string_list
    !  #]
  subroutine delete_string_list(str_list)
    !  #[
    type(string_list_type), intent(inout) :: str_list
    
    if (associated(str_list%first_chunck)) then
       ! recursively delete all chuncks
       call delete_string_chunck(str_list%first_chunck)
       deallocate(str_list%first_chunck)
       nullify(str_list%first_chunck)
    end if

    nullify(str_list%last_chunck)
    str_list%num_chuncks = 0

  end subroutine delete_string_list
    !  #]
  subroutine add_string_to_list(str_list,str,error_flag)
    !  #[
    type(string_list_type), intent(inout) :: str_list   ! modified
    character(len=*),       intent(in)    :: str        ! input
    integer,                intent(out)   :: error_flag ! output

    ! local variables
    integer :: AllocStatus
    type(string_chunck_type), pointer :: new_chunck

    ! allocate and init the new chunck
    allocate(new_chunck,stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR in add_string_to_list: allocation failure"
       error_flag = error_allocate
       return
    end if
    call init_string_chunck(new_chunck)

    ! fill the new chunck
    allocate(new_chunck%chunck(len(str)),stat=AllocStatus)
    if (AllocStatus .ne. 0) then
       print *,"ERROR in add_string_to_list: allocation failure"
       error_flag = error_allocate
       return
    end if
    new_chunck%chunck = string2chararray(str)

    if (associated(str_list%last_chunck)) then
       ! add after last chunck
       str_list%last_chunck%next_chunck => new_chunck
       str_list%last_chunck             => new_chunck
    else
       ! add as first chunck
       str_list%first_chunck => new_chunck
       str_list%last_chunck  => new_chunck
    end if
    str_list%num_chuncks = str_list%num_chuncks + 1

  end subroutine add_string_to_list
    !  #]
  function get_string_from_list(str_list) result(text)
    !  #[
    type(string_list_type),         intent(in) :: str_list ! input
    character(len=1), dimension(:), pointer    :: text     ! result
    integer :: n

    ! local variables
    type(string_chunck_type), pointer :: this_chunck
    integer :: pos, this_len

    n = get_total_string_len_from_list(str_list)
    allocate(text(n))
    this_chunck => str_list%first_chunck
    pos=1
    chunck_loop: do 
       if (.not. associated(this_chunck)) exit chunck_loop
       this_len = size(this_chunck%chunck)
       !print *,"filling chars ",pos," upto ",pos+this_len-1," with: ",&
       !     chararray2string(this_chunck%chunck)
       text(pos:pos+this_len-1) = this_chunck%chunck
       this_chunck => this_chunck%next_chunck
       pos = pos+this_len
    end do chunck_loop

  end function get_string_from_list
    !  #]
  function get_total_string_len_from_list(str_list) result(n)
    !  #[
    type(string_list_type), intent(in) :: str_list ! input
    integer                            :: n        ! result

    n=0
    if (associated(str_list%first_chunck)) &
         n = get_all_chunck_sizes(str_list%first_chunck)

  end function get_total_string_len_from_list
    !  #]
  !--------------------------------------------------
  subroutine init_string_chunck(str_chunck)
    !  #[
    type(string_chunck_type), intent(out) :: str_chunck

    nullify(str_chunck%chunck)
    nullify(str_chunck%next_chunck)

  end subroutine init_string_chunck
    !  #]
  recursive subroutine delete_string_chunck(str_chunck)
    !  #[
    type(string_chunck_type), intent(inout) :: str_chunck

    if (associated(str_chunck%next_chunck)) then
       call delete_string_chunck(str_chunck%next_chunck)
       deallocate(str_chunck%next_chunck)
       nullify(str_chunck%next_chunck)
    end if

    if (associated(str_chunck%chunck)) then
       deallocate(str_chunck%chunck)
       nullify(str_chunck%chunck)
    end if

  end subroutine delete_string_chunck
    !  #]
  recursive function get_all_chunck_sizes(str_chunck) result(n)
    !  #[
    type(string_chunck_type), intent(in) :: str_chunck ! input
    integer                              :: n          ! result

    n=0
    if (associated(str_chunck%chunck)) &
         n = size(str_chunck%chunck)
    if (associated(str_chunck%next_chunck)) &
         n = n + get_all_chunck_sizes(str_chunck%next_chunck)

  end function get_all_chunck_sizes
    !  #]
  !--------------------------------------------------
end module StringTools
