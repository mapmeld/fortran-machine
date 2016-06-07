! tupleserver.f90 --
!      Tuple server based on the IPC modules
!
!      *** UNDER CONSTRUCTION ***
!
!      NOTE:
!      - Synchronous reads/ins must be stored until a tuple appears that
!        matches their pattern
!
program tupleserver
    use tuplespaces

    implicit none

    character(len=80)                                 :: directory_name
    character(len=40)                                 :: server_name
    integer                                           :: buffer_size

    type(ipc_comm)                                    :: comm
    type(tuple_data),       dimension(:), allocatable :: tuple
    type(tuple_connection), dimension(:), allocatable :: connection

    integer :: i

    allocate( tuple(0) )
    allocate( connection(0) )

    open( 10, file = 'tupleserver.inp' )
    read( 10, '(a)' ) directory_name
    read( 10, '(a)' ) server_name
    read( 10, * )     buffer_size
    close( 10 )

    !
    ! Initialise the IPC mechanism
    !
    TODO: set buffer size (mmap), directory (file, mmap)

    comm = ipc_open( server_name )
    call ipc_cleanup( comm )

    !
    ! Infinite loop:
    ! - Look for incoming or closing connections
    ! - Look for new requests from exisintg connections
    !
    do
        call handle_connections

        do i = 1,size(connection)
             call handle_requests
        enddo
    enddo

contains

end program
