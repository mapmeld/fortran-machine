! textstream.f90 --
!     Module to read ordinary text files assuming they act as
!     "streams", not as record-based files.
!
!     Note:
!     The module uses list-directed input to read the text file.
!     It can not deal with very long input items (notably
!     text strings longer than the length of the buffer strings
!     - the parameter MAXBUF).
!     Using list-directed input, it deals with all the features
!     of that type of input processing, but a / seems to interfer
!     with the detection of the end of input.
!
!     $Id: textstream.f90,v 1.1 2006/06/23 08:04:13 arjenmarkus Exp $
!
module text_streams
    implicit none

    integer, parameter :: MAXBUF = 80

    type TEXT_STREAM
        private
        integer                 :: lun
        integer                 :: offset
        logical                 :: endline
        character(len=MAXBUF)   :: buffer
        character(len=2*MAXBUF) :: dbuffer
    end type TEXT_STREAM

    private :: textstream_readbuffer

    interface textstream_read
        module procedure textstream_read_char
    end interface

contains

! textstream_readbuffer --
!     Read a new part of the file
! Arguments:
!     stream         Identification of the stream
!     append         Append to dbuffer or not
!     error          Error or not
!
subroutine textstream_readbuffer( stream, append, ierr )
    type(TEXT_STREAM)            :: stream
    logical, intent(in)          :: append
    integer, intent(inout)       :: ierr

    stream%buffer = ' '
    do
        stream%endline = .false.
        read( stream%lun, '(a)', advance='no', iostat=ierr, &
            eor=100 ) stream%buffer
        goto 200

100     continue
        ierr = 0
        stream%endline = .true.

200     continue
        if ( ierr /= 0 ) exit
        if ( stream%buffer /= ' ' ) exit

    enddo

    if ( append ) then
        stream%dbuffer = stream%dbuffer(1:maxbuf) // stream%buffer
    else
        stream%dbuffer = stream%buffer
    endif
end subroutine textstream_readbuffer

! textstream_open --
!     Open a file as a text stream
! Arguments:
!     stream         Identification of the stream
!     lun            Logical unit number to use
!     filename       Name of the file
!     error          Error or not
!
subroutine textstream_open( stream, lun, filename, error )
    type(TEXT_STREAM)            :: stream
    integer, intent(in)          :: lun
    character(len=*), intent(in) :: filename
    logical, intent(out)         :: error

    integer                      :: ierr

    open( lun, file=filename, status = 'old', iostat=ierr )

    if ( ierr .eq. 0 ) then
        stream%lun     = lun
        stream%endline = .false.
        stream%buffer  = ' '
        call textstream_readbuffer( stream, .false., ierr )
        error = ierr .ne. 0
    else
        error = .true.
        stream%lun    = 0
    endif

end subroutine textstream_open

! textstream_close --
!     Close a text stream
! Arguments:
!     stream         Identification of the stream
!
subroutine textstream_close( stream )
    type(TEXT_STREAM)          :: stream

    close( stream%lun )
    stream%lun = 0

end subroutine textstream_close

! binstream_read_char --
!     Read a character string
! Arguments:
!     stream         Identification of the stream
!     char           Character string
!     ierr           Error indication (IOSTAT code)
!
subroutine textstream_read_char( stream, char, ierr )
    type(TEXT_STREAM)             :: stream
    character(len=*), intent(out) :: char
    integer, intent(out)          :: ierr

    character(len=1)              :: dummy
    integer                       :: i

    ierr = 0

    do
        !
        ! First step: assume there is a string left in the buffer
        ! (To reliably detect the end of the record, we read one extra)
        !
        read( stream%dbuffer, *, iostat=ierr ) (dummy, i=1,stream%offset), char, dummy
        if ( ierr .gt. 0 ) then
            return
        endif
        if ( ierr .eq. 0 ) then
            stream%offset = stream%offset + 1
            return
        endif

        !
        ! Second step: do we have only one item left in the buffer?
        !
        if ( stream%endline ) then
            read( stream%dbuffer, *, iostat=ierr ) (dummy, i=1,stream%offset), char
            if ( ierr .gt. 0 ) then
                return
            endif
            if ( ierr .eq. 0 ) then
                stream%offset = stream%offset + 1
                return
            endif
        endif

        !
        ! Third step: read a next piece of the line or the next line
        !
        if ( stream%endline ) then
            stream%offset = 0
        else
            stream%dbuffer = stream%buffer    ! Shift maxbuf characters
        endif
        call textstream_readbuffer( stream, (.not. stream%endline), ierr )
        if ( ierr .ne. 0 ) return
    enddo

end subroutine textstream_read_char

end module text_streams
