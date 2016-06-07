
subroutine ipc_send_int_scalar( comm, data, error )
    type(ipc_comm) :: comm
    integer       :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_int_scalar( comm, data, error )
    type(ipc_comm) :: comm
    integer       :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = 0
    length = 0

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_real_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real       :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_real_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real       :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = 0
    length = 0

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_dbl_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0))       :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_dbl_scalar( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0))       :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = 0
    length = 0

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_log_scalar( comm, data, error )
    type(ipc_comm) :: comm
    logical       :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_log_scalar( comm, data, error )
    type(ipc_comm) :: comm
    logical       :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = 0
    length = 0

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_char_scalar( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*)       :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = len(data)

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_char_scalar( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*)       :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = len(data)
    length = len(data)

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_cmplx_scalar( comm, data, error )
    type(ipc_comm) :: comm
    complex       :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, (0, i = 1,3), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_cmplx_scalar( comm, data, error )
    type(ipc_comm) :: comm
    complex       :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = 0
    length = 0

    read( comm%lun, iostat = ierr ) &
        typeid_, (dummy, i = 1,3), length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_int_1d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_int_1d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_int_2d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_int_2d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_int_3d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_int_3d( comm, data, error )
    type(ipc_comm) :: comm
    integer, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 1
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_real_1d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_real_1d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_real_2d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_real_2d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_real_3d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_real_3d( comm, data, error )
    type(ipc_comm) :: comm
    real, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 2
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_dbl_1d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_dbl_1d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_dbl_2d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_dbl_2d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_dbl_3d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_dbl_3d( comm, data, error )
    type(ipc_comm) :: comm
    real(kind(1.0d0)), dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 3
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_log_1d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_log_1d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_log_2d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_log_2d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_log_3d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_log_3d( comm, data, error )
    type(ipc_comm) :: comm
    logical, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 4
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_char_1d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = len(data(1))

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_char_1d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = len(data(1))

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_char_2d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = len(data(1,1))

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_char_2d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = len(data(1,1))

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_char_3d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = len(data(1,1,1))

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_char_3d( comm, data, error )
    type(ipc_comm) :: comm
    character(len=*), dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 5
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = len(data(1,1,1))

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_cmplx_1d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_cmplx_1d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_cmplx_2d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_cmplx_2d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:,:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_send_cmplx_3d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = 0

    write( comm%lun, iostat = ierr ) &
        typeid, shape(data), (0, i = 1,3-size(shape(data))), length
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    write( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine

subroutine ipc_receive_cmplx_3d( comm, data, error )
    type(ipc_comm) :: comm
    complex, dimension(:,:,:) :: data
    logical        :: error
    integer        :: typeid = 6
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = 0

    exp_shape = (/ shape(data), (0, i = 1,3-size(shape(data))) /)

    read( comm%lun, iostat = ierr ) &
        typeid_, shape_, length_
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
    if ( typeid_ /= typeid .or. any(shape_ /= exp_shape) .or. length_ /= length ) then
        error = .true.
        return
    endif

    read( comm%lun, iostat = ierr ) data
    if ( ierr /= 0 ) then
        error = .true.
        return
    endif
end subroutine
