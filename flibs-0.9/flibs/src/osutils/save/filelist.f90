! filelist.f90 --
!     Utility to return a list of files
!
!     Note:
!     Temporary solution
module osutils
    implicit none

include 'oscommands.inc'


contains
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
!     Convert the directory separator to the right one for the platform
!     Add the directory (in the pattern) to the list
!
subroutine file_list( pattern, list )
    character(len=*), intent(in)            :: pattern
    character(len=*), pointer, dimension(:) :: list

    character(len=200)                      :: tmpfile
    character(len=200)                      :: cmd
    character(len=1)                        :: line
    integer                                 :: luntmp = 10 ! JUST FOR THE MOMENT!
    integer                                 :: i
    integer                                 :: ierr
    integer                                 :: count

    open( luntmp, status = 'scratch' )
    inquire( luntmp, name = tmpfile ) ! Hope this is okay
    close( luntmp )

    cmd = trim(filelist) // ' ' // trim(pattern) // ' ' // trim(redirect) // tmpfile

    call system( cmd )

    open( luntmp, file = tmpfile )

    !
    ! First count the number of files, then allocate and fill the array
    !
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

    call file_list( 'some\no\existent\directory', list )

    write( *, * ) 'Files: '
    do i =1,size(list)
        write( *, * ) list(i)
    enddo

end program
