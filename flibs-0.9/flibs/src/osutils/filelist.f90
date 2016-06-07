! filelist.f90 --
!     Utility to return a list of files
!
!     Note:
!     Temporary solution
module osutils
    implicit none

include 'oscommands.inc'

    private :: file_correct_separator


contains

! file_correct_separator --
!     Corrects the separator in a file/directory name/pattern
!
! Arguments:
!     string          String to be checked
!
subroutine file_correct_separator( string )
    character(len=*) :: string

    integer          :: k
    character(len=1) :: altsep

    if ( dir_separator == '\' ) then
        altsep = '/'
    else
        altsep = '\'
    endif

    do
        k = index( string, altsep )
        if ( k > 0 ) then
            string(k:k) = dir_separator
        else
            exit
        endif
    enddo
end subroutine file_correct_separator

! file_list --
!     Returns a list of files fulfilling a particular pattern
!
! Arguments:
!     pattern         Pattern for the file names (like: *.f90)
!     list            Allocated list of file names (returned)
!
! Note:
!     The list of files is an allocated array, the caller should
!     deallocate it when done.
!     The length of the character strings in list must be long
!     enough for the file names. Use for instance a length of at
!     least 200 characters
!     The list can zero names long, it will still be allocated.
!
! To do:
!     Determine the temporary file dynamically
!
subroutine file_list( pattern, list )
    character(len=*), intent(in)            :: pattern
    character(len=*), pointer, dimension(:) :: list

    character(len=200)                      :: tmpfile
    character(len=200)                      :: cmd
    character(len=1)                        :: line
    character(len=len(pattern))             :: prefix
    integer                                 :: luntmp = 10 ! JUST FOR THE MOMENT!
    integer                                 :: i
    integer                                 :: ierr
    integer                                 :: count

    if ( system_type == 'windows' ) then
        call getenv( 'TEMP', tempfile )
        tmpfile = trim(tempfile) // dir_separator // '__filelist__'
    else
        tmpfile = trim(tempdir) // '__filelist__'
    endif

    cmd = trim(filelist) // ' ' // trim(pattern) // ' ' // trim(redirect) // trim(tmpfile) &
              // ' ' // suppress_msg
    call file_correct_separator( cmd )

    if ( add_dir ) then
        prefix = pattern
        call file_correct_separator( prefix )
        i = index( prefix, dir_separator, .true. )
        prefix(i:) = ' '
    endif

    call system( cmd )

    open( luntmp, file = tmpfile )

    !
    ! First count the number of files, then allocate and fill the array
    !
    count = 0
    do
        read( luntmp, '(a)', iostat = ierr ) line

        if ( ierr == 0 ) then
            count = count + 1
        else
            exit
        endif
    enddo

    rewind( luntmp )

    allocate( list(count) )

    do i = 1,count
        read( luntmp, '(a)' ) list(i)
        if ( add_dir ) then
            list(i) = trim(prefix) // dir_separator // list(i)
        endif
    enddo

    close( luntmp, status = 'delete' )

end subroutine file_list

end module

program test_file_list
    use osutils
    implicit none

    character(len=50), dimension(:), pointer :: list
    integer                                  :: i

    call file_list( '*.f90', list )

    write( *, * ) 'Files: '
    do i =1,size(list)
        write( *, * ) list(i)
    enddo

    deallocate( list )

    !
    ! Note: the pattern is automatically adjusted
    !
    ! Careful with backslashes: gfortran interprets them the C way!
    !
    call file_list( 'some\\no\\existent\\directory', list )

    write( *, * ) 'Files: '
    do i =1,size(list)
        write( *, * ) list(i)
    enddo

end program
