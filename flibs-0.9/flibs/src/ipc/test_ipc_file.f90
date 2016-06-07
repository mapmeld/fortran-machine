! test_ipc_file.f90 --
!     Test program for the IPC_FILE module:
!     Send messages from one instantation to the other
!
program test_ipc_file

    use ipc_file

    implicit none

    type(ipc_comm)    :: comm

    integer           :: i
    integer           :: id
    integer           :: lun
    real              :: v
    character(len=20) :: myid
    character(len=20) :: task
    character(len=20) :: keyword
    character(len=20) :: value
    character(len=20) :: tag
    character(len=20) :: connection
    logical           :: error

    !
    ! Read the information from the command-line:
    ! - Id of the instance
    ! - Task and which one to connect to
    !
    do i = 1,2
        read( *, * ) keyword, value
        write( *, * ) 'Read: ', keyword, value
        select case (keyword)
            case('id')
                myid = value
            case('send', 'receive')
                task       = keyword
                connection = value
        end select
    enddo

    comm = ipc_open( myid )
    !
    ! Depending on the task we either send a number or receive it
    ! and print it
    !
    write( *, * ) 'Task: >>',task, '<<'
    if ( task == 'send' ) then
        do i = 1,10
            id = i
            write( *, * ) myid, ': sending ... ', v, id
            call random_number( v )
            call ipc_send_start( comm, connection, 'number', id )
            call ipc_send_data( comm, v, error )
            write( *, * ) myid, ': sent ', v, id
            call ipc_send_finish( comm )
        enddo
    else
        tag = 'number'
        do i = 1,10
            id = i
            write( *, * ) myid, ': receiving ... '
            call ipc_receive_start( comm, connection, tag, id )
            call ipc_receive_data( comm, v, error )
            write( *, * ) myid, ': received ', v, id
            call ipc_receive_finish( comm )
        enddo
    endif
end program
