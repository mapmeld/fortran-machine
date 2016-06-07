! Test program for binary streams
!
!     $Id: test_binstreams.f90,v 1.3 2006/06/23 08:04:13 arjenmarkus Exp $
!
program test_binstream
    use binary_streams
    implicit none

    type(BINARY_STREAM) :: stream
    integer             :: i
    logical             :: error
    character(len=3)    :: char

    call binstream_open( stream, 10, 'test_binstream.inp', error )

    if ( error ) then
        write(*,*) 'Problem opening the file!'
        call binstream_close( stream )
        stop 'Error!'
    endif

    do i = 1,3
        call binstream_read_char( stream, char, error )
        write(*,*) 'Read: ', char, ' - ', error
    enddo

    !
    ! Write over the start of the file
    !
    call binstream_seek( stream, .true., 1 )
    call binstream_write_char( stream, 'ABCD', error )
    call binstream_seek( stream, .true., 0 )
    call binstream_read_char( stream, char, error )
    write(*,*) 'Read: ', char, ' - ', error

    call binstream_close( stream )
end program test_binstream
