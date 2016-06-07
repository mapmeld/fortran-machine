! ipc_file.f90 --
!     Simple method for inter-process communication via file I/O
!
! ipc_file --
!     Module for IPC via file I/O
!
! NOTES:
!     Start in separet DOS-boxes
!     Order of starting important - right now!
!
module ipc_file
    use dflib ! Hm, compiler-dependency here!

    implicit none

    type ipc_comm
        character(len=20) :: me          ! Identifying string for calling program
        character(len=20) :: connection  ! Id for connecting program
        character(len=20) :: tag         ! Tag for the data (type)
        integer           :: id          ! Numerical identfier
        integer           :: lun
    end type ipc_comm

    interface ipc_send_data
        module procedure ipc_send_int_scalar
        module procedure ipc_send_int_1d
        module procedure ipc_send_int_2d
        module procedure ipc_send_int_3d
        module procedure ipc_send_real_scalar
        module procedure ipc_send_real_1d
        module procedure ipc_send_real_2d
        module procedure ipc_send_real_3d
        module procedure ipc_send_dbl_scalar
        module procedure ipc_send_dbl_1d
        module procedure ipc_send_dbl_2d
        module procedure ipc_send_dbl_3d
        module procedure ipc_send_char_scalar
        module procedure ipc_send_char_1d
        module procedure ipc_send_char_2d
        module procedure ipc_send_char_3d
        module procedure ipc_send_cmplx_scalar
        module procedure ipc_send_cmplx_1d
        module procedure ipc_send_cmplx_2d
        module procedure ipc_send_cmplx_3d
    end interface

    interface ipc_receive_data
        module procedure ipc_receive_int_scalar
        module procedure ipc_receive_int_1d
        module procedure ipc_receive_int_2d
        module procedure ipc_receive_int_3d
        module procedure ipc_receive_real_scalar
        module procedure ipc_receive_real_1d
        module procedure ipc_receive_real_2d
        module procedure ipc_receive_real_3d
        module procedure ipc_receive_dbl_scalar
        module procedure ipc_receive_dbl_1d
        module procedure ipc_receive_dbl_2d
        module procedure ipc_receive_dbl_3d
        module procedure ipc_receive_char_scalar
        module procedure ipc_receive_char_1d
        module procedure ipc_receive_char_2d
        module procedure ipc_receive_char_3d
        module procedure ipc_receive_cmplx_scalar
        module procedure ipc_receive_cmplx_1d
        module procedure ipc_receive_cmplx_2d
        module procedure ipc_receive_cmplx_3d
    end interface
contains

! ipc_open --
!     Initialise the connection on "this" side
!
! Arguments:
!     me         String identifying the calling process
!
! Result:
!     Initialised structure ready for further communications
!
type(ipc_comm) function ipc_open( me )
    character(len=*) :: me

    ipc_open%me = me
end function ipc_open

! ipc_try_connect --
!     Try to open the connection to another process
!
! Arguments:
!     comm       Initialised connection structure
!     dest       Identifying string for that other process
!     idstring   Unique identifier to check the connection
!     success    Whether the connection was achieved or not
!
! Note:
!     This routine is meant for use in the context of the tuple_space
!     module.
!
subroutine ipc_try_connect( comm, dest, idstring, success )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: idstring
    logical          :: success

    integer                      :: ierr
    character(len=len(idstring)) :: answer

    sucess = .false.

    !
    ! Open the send file - but it must not yet exist
    !
    TODO!!!
    lun = 10

    open( lun, file = trim(comm%me) // "-" // trim(dest) // ".send", &
        form = 'unformatted', status = 'new', iostat = ierr )

    if ( ierr /= 0 ) then
        return
    endif
    close( lun )

    call ipc_send_start(  comm, dest, idstring, 0 )
    call ipc_send_data(   comm, idstring )
    call ipc_send_finish( comm )

    !
    ! Now wait for the _same_ string to be returned
    !

    call ipc_receive_start(  comm, dest, idstring, 0 )
    call ipc_receive_data(   comm, answer )
    call ipc_receive_finish( comm )

    if ( idstring == answer ) then
        sucess = .true.
    endif

end subroutine ipc_try_connect

subroutine ipc_cleanup( comm )
    TODO
end subroutine ipc_cleanup


subroutine ipc_send_start( comm, dest, tag, id )
    type(ipc_comm)   :: comm
    character(len=*) :: dest
    character(len=*) :: tag
    integer          :: id

    integer          :: lun

    lun = 10

    write(*,*) 'Send started'
    !
    ! Clean the send file
    !
    open( lun, file = trim(comm%me) // "-" // trim(dest) // ".send", &
        form = 'unformatted' )
    write( lun )
    close( lun )

    !
    ! Open the data file
    !
    open( lun, file = trim(comm%me) // "-" // trim(dest) // ".data", &
        form = 'unformatted' )

    comm%lun        = lun
    comm%connection = dest
    comm%tag        = tag
    comm%id         = id
end subroutine

subroutine ipc_send_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    integer          :: ierr

    close( comm%lun )
    open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".send", &
        form = 'unformatted' )
    write( comm%lun ) comm%me, comm%connection, comm%tag, comm%id
    close( comm%lun )

    do
        open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".recv", &
            form = 'unformatted', status = 'old', iostat = ierr )
        write(*,*) 'Send finishing: ', ierr
        if ( ierr == 0 ) then
            read( comm%lun, iostat = ierr ) okay
            close( comm%lun )
            write(*,*) 'Send finishing: ', okay
            if ( ierr == 0 .and. okay ) exit
        endif
        call sleepqq( 1000 )
    enddo
    open( comm%lun, file = trim(comm%me) // "-" // trim(comm%connection) // ".recv", &
        form = 'unformatted', status = 'old', iostat = ierr )
    write( comm%lun )
    close( comm%lun )
    write(*,*) 'Send finished'

end subroutine

subroutine ipc_receive_start( comm, src, tag, id )
    type(ipc_comm)   :: comm
    character(len=*) :: src
    character(len=*) :: tag
    integer          :: id
    integer          :: lun

    character(len=20) :: src_
    character(len=20) :: dest_
    character(len=20) :: tag_
    integer           :: id_

    integer           :: ierr

    lun = 10

    comm%lun        = lun
    comm%connection = src
    comm%tag        = tag

    write(*,*) 'Receive started'
    !
    ! Clean the receive file
    !
!    open( lun, file = trim(src) // "-" // trim(comm%me) // ".recv", &
!        form = 'unformatted' )
!    write( lun )
!    close( lun )

    !
    ! Open the send file
    !
    do
        open( lun, file = trim(src) // "-" // trim(comm%me) // ".send", &
            form = 'unformatted', status = 'old', iostat = ierr, &
            position = 'rewind' )
        write(*,*) 'Receive starting: ', ierr
        if ( ierr == 0 ) then
            read( lun, iostat = ierr ) src_, dest_, tag_, id_
            close( lun )
            write(*,*) 'Receive starting: (read) ', ierr
            write(*,*) 'Receive starting: (read) ', src_, src
            write(*,*) 'Receive starting: (read) ', dest_, comm%me
            write(*,*) 'Receive starting: (read) ', tag_, tag

            if ( ierr == 0 .and. &
                 src_ == src .and. dest_ .eq. comm%me .and. &
                 tag_ == tag ) then
               exit
            endif
        endif

        call sleepqq( 1000 ) ! One second
    enddo

    comm%id         = id_
    id              = id_

    open( lun, file = trim(src) // "-" // trim(comm%me) // ".data", &
        form = 'unformatted', status = 'old', iostat = ierr )

end subroutine

subroutine ipc_receive_finish( comm )
    type(ipc_comm)   :: comm
    integer          :: lun
    logical          :: okay

    close( comm%lun )
    open( comm%lun, file = trim(comm%connection) // "-" // trim(comm%me) // ".send", &
        form = 'unformatted' )
    write( comm%lun )
    close( comm%lun )

    open( comm%lun, file = trim(comm%connection) // "-" // trim(comm%me) // ".recv", &
        form = 'unformatted' )
    write( comm%lun ) .true.
    close( comm%lun )
    write(*,*) 'Receive finished'

end subroutine

include "ipc_file_data.f90"

end module ipc_file

