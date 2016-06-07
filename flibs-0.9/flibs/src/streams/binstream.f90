! binstream.f90 --
!     Module to read and write binary files assuming they act as
!     "streams", not as record-based files.
!
!     Note:
!     The module makes a number of assumptions:
!     - any file can be opened as a direct-acess file with any
!       record length
!     - to avoid complicated code the files are opened with
!       records of 4 bytes long (ordinary the record length is 4
!       or 1 - the length unit is system-dependent!). This means
!       that systems where the unit is not 1 or 4 bytes are not
!       supported - this could include 64-bits systems.
!     - the end of a file may not be accurately detected. This
!       is due to the behaviour of direct-access files: the
!       last record may not be complete, if the file size is
!       a multiple of 4 bytes.
!     - a default integer is assumed to be 4 bytes, as is
!       a default real and a default logical. A double precision
!       real is assumed to be 8 bytes long. There is NO provision
!       for situations where this is not true.
!
!     $Id: binstream.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module binary_streams
    implicit none

    type BINARY_STREAM
        private
        integer          :: lun
        integer          :: record
        integer          :: offset
    end type BINARY_STREAM

    integer, private     :: reclen = 0

    private              :: binstream_init

contains

! binstream_init --
!     Initialisation: Determine the relevant
!     record length
! Arguments:
!     lun            Logical unit number to use
!     ierr           Indiates whether initialisation
!                    succeeded
!
subroutine binstream_init( lun, ierr )
    integer, intent(in)          :: lun
    integer, intent(out)         :: ierr

    character(len=5)             :: c

    open( lun, status = 'scratch', access = 'direct', recl = 1 )
    write( lun, rec=1, iostat = ierr ) c(1:1)
    if ( ierr .ne. 0 ) then
        return
    endif
    !
    ! This will cause an error when the record length
    ! is 1 byte, otherwise we "know" the length is 4 bytes
    !
    write( lun, rec=1, iostat = ierr ) c(1:4)
    if ( ierr .ne. 0 ) then
        reclen = 4
    else
        reclen = 1

        !
        ! One last check: it is indeed 4 bytes?
        !
        write( lun, rec=1, iostat = ierr ) c(1:5)
        if ( ierr .eq. 0 ) then
            ierr = 1
        endif
    endif
    close( lun )
end subroutine binstream_init

! binstream_open --
!     Open a file as a binary stream
! Arguments:
!     stream         Identification of the stream
!     lun            Logical unit number to use
!     filename       Name of the file
!     error          Error or not
!
subroutine binstream_open( stream, lun, filename, error )
    type(BINARY_STREAM)          :: stream
    integer, intent(in)          :: lun
    character(len=*), intent(in) :: filename
    logical, intent(out)         :: error

    integer                      :: ierr

    if ( reclen .eq. 0 ) then
        call binstream_init( lun, ierr )
    endif

    open( lun, file=filename, access='direct', recl=reclen, iostat=ierr )

    if ( ierr .eq. 0 ) then
        error = .false.
        stream%lun    = lun
        stream%record = 1
        stream%offset = 0
    else
        error = .true.
        stream%lun    = 0
    endif

end subroutine binstream_open

! binstream_close --
!     Close a binary stream
! Arguments:
!     stream         Identification of the stream
!
subroutine binstream_close( stream )
    type(BINARY_STREAM)          :: stream

    close( stream%lun )
    stream%lun = 0

end subroutine binstream_close

! binstream_tell --
!     Return the position (in bytes) in the file
! Arguments:
!     stream         Identification of the stream
! Result:
!     The byte at which the next read/write will
!     occur, starting with 0!
! Note:
!     By using 0 as the index of the first byte,
!     you can store the current position and
!     restore it later:
!         current = binstream_tell( stream )
!         call binstream_seek( stream, .true., current )
!
integer function binstream_tell( stream )
    type(BINARY_STREAM)          :: stream

    binstream_tell = 4 * (stream%record-1) + stream%offset

end function binstream_tell

! binstream_seek --
!     Set the position (in bytes) in the file
! Arguments:
!     stream         Identification of the stream
!     start          Offset given from start (true)
!                    or from current position (false)
!     offset         Offset in bytes
! Side effects:
!     The record to read or write next is set
!
subroutine binstream_seek( stream, start, offset )
    type(BINARY_STREAM)          :: stream
    logical, intent(in)          :: start
    integer, intent(in)          :: offset

    integer                      :: count

    if ( start ) then
        stream%record = 1 + offset/4
        stream%offset = mod(offset,4)
    else
        count         = 4*(stream%record-1) + stream%offset
        stream%record = 1 + count/4
        stream%offset = mod(count,4)
    endif
end subroutine binstream_seek

! binstream_read_char --
!     Read a character string
! Arguments:
!     stream         Identification of the stream
!     char           Character string
!     error          Error indication
!
subroutine binstream_read_char( stream, char, error )
    type(BINARY_STREAM)           :: stream
    character(len=*), intent(out) :: char
    logical, intent(out)          :: error

    character(len=4)              :: buffer
    integer                       :: p
    integer                       :: lbuf
    integer                       :: left
    integer                       :: ierr

    error = .false.
    lbuf = len(buffer)

    !
    ! First part: whatever was left from the last
    ! record
    !
    read( stream%lun, rec=stream%record, iostat=ierr ) buffer
    if ( ierr .ne. 0 ) then
        error = .true.
        return
    endif

    char(1:) = buffer(stream%offset+1:)

    if ( len(char) .gt. lbuf-stream%offset ) then
        p = len(buffer) - stream%offset + 1
        stream%offset = 0
    else
        stream%offset = len(char) - stream%offset
        return
    endif

    !
    ! Now read the full records within range
    !
    left = len(char) - p + 1
    do while ( left .gt. lbuf )
        stream%record = stream%record + 1

        read( stream%lun, rec=stream%record, iostat=ierr ) buffer
        if ( ierr .ne. 0 ) then
            error = .true.
            return
        endif

        char(p:p+lbuf-1) = buffer
        p    = p    + lbuf
        left = left - lbuf
    enddo

    !
    ! Finally, read the last part
    !
    stream%offset = left
    stream%record = stream%record + 1

    if ( left .gt. 0 ) then
        read( stream%lun, rec=stream%record, iostat=ierr ) buffer
        if ( ierr .ne. 0 ) then
            error = .true.
            return
        endif

        char(p:) = buffer
    endif

end subroutine binstream_read_char

! binstream_read_int, ... --
!     Read a 4-bytes integer/real/logical or
!     an 8-bytes double precision real
! Arguments:
!     stream         Identification of the stream
!     value          Value to be read
!     error          Error indication
!
subroutine binstream_read_int( stream, value, error )
    type(BINARY_STREAM)           :: stream
    integer, intent(out)          :: value
    logical, intent(out)          :: error

    character(len=4)              :: buffer

    call binstream_read_char( stream, buffer, error )
    if ( error ) return

    value = transfer( buffer, value )

end subroutine binstream_read_int

subroutine binstream_read_real( stream, value, error )
    type(BINARY_STREAM)           :: stream
    real, intent(out)             :: value
    logical, intent(out)          :: error

    character(len=4)              :: buffer

    call binstream_read_char( stream, buffer, error )
    if ( error ) return

    value = transfer( buffer, value )

end subroutine binstream_read_real

subroutine binstream_read_log( stream, value, error )
    type(BINARY_STREAM)           :: stream
    logical, intent(out)          :: value
    logical, intent(out)          :: error

    character(len=4)              :: buffer

    call binstream_read_char( stream, buffer, error )
    if ( error ) return

    value = transfer( buffer, value )

end subroutine binstream_read_log

subroutine binstream_read_double( stream, value, error )
    type(BINARY_STREAM)                 :: stream
    real(kind=kind(1.0d0)), intent(out) :: value
    logical, intent(out)                :: error

    character(len=8)                    :: buffer

    call binstream_read_char( stream, buffer, error )
    if ( error ) return

    value = transfer( buffer, value )

end subroutine binstream_read_double

! binstream_write_char --
!     Write a character string
! Arguments:
!     stream         Identification of the stream
!     char           Character string
!     error          Error indication
!
subroutine binstream_write_char( stream, char, error )
    type(BINARY_STREAM)           :: stream
    character(len=*), intent(in)  :: char
    logical, intent(out)          :: error

    character(len=4)              :: buffer
    integer                       :: p
    integer                       :: lbuf
    integer                       :: left
    integer                       :: ierr

    error = .false.
    lbuf = len(buffer)

    !
    ! First part: whatever was left from the last
    ! record
    !
    read( stream%lun, rec=stream%record, iostat=ierr ) buffer
    if ( ierr .ne. 0 ) then
        error = .true.
        return
    endif

    buffer(stream%offset+1:) = char(1:)

    write( stream%lun, rec=stream%record, iostat=ierr ) buffer
    if ( ierr .ne. 0 ) then
        error = .true.
        return
    endif

    if ( len(char) .gt. lbuf-stream%offset ) then
        p = len(buffer) - stream%offset + 1
        stream%offset = 0
    else
        stream%offset = len(char) - stream%offset
        return
    endif

    !
    ! Now write the full records within range
    !
    left = len(char) - p + 1
    do while ( left .gt. lbuf )
        stream%record = stream%record + 1

        buffer = char(p:p+lbuf-1)
        write( stream%lun, rec=stream%record, iostat=ierr ) buffer
        if ( ierr .ne. 0 ) then
            error = .true.
            return
        endif

        p    = p    + lbuf
        left = left - lbuf
    enddo

    !
    ! Finally, write the last part
    !
    stream%offset = left
    stream%record = stream%record + 1

    if ( left .gt. 0 ) then
        read( stream%lun, rec=stream%record, iostat=ierr ) buffer
        if ( ierr .ne. 0 ) then
            error = .true.
            return
        endif

        buffer(1:left) = char(p:)

        write( stream%lun, rec=stream%record, iostat=ierr ) buffer
        if ( ierr .ne. 0 ) then
            error = .true.
            return
        endif
    endif

end subroutine binstream_write_char

end module binary_streams
