# genipc.tcl --
#     Generate the code for the various specific send/receive routines
#     (A lot of it is completely generic)
#
set outfile [open "ipc_file_data.f90" w]

#
# Scalar variants
#
foreach {typedecl typeid typen length} {
    integer           1 int   0
    real              2 real  0
    real(kind(1.0d0)) 3 dbl   0
    logical           4 log   0
    character(len=*)  5 char  len(data)
    complex           6 cmplx 0
    } {
    set subst [list TYPEDECL $typedecl TYPEID $typeid TYPEN $typen \
                    LENGTH $length]

    puts $outfile [string map $subst {
subroutine ipc_send_TYPEN_scalar( comm, data, error )
    type(ipc_comm) :: comm
    TYPEDECL       :: data
    logical        :: error
    integer        :: typeid = TYPEID
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = LENGTH

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

subroutine ipc_receive_TYPEN_scalar( comm, data, error )
    type(ipc_comm) :: comm
    TYPEDECL       :: data
    logical        :: error
    integer        :: typeid = TYPEID
    integer        :: typeid_
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_
    integer        :: dummy

    error  = .false.
    length = LENGTH
    length = LENGTH

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
end subroutine}]
}

#
# Array variants
#
foreach {typedecl typeid typen dimdecl dimn length} {
    integer           1 int   :     1d 0
    integer           1 int   :,:   2d 0
    integer           1 int   :,:,: 3d 0
    real              2 real  :     1d 0
    real              2 real  :,:   2d 0
    real              2 real  :,:,: 3d 0
    real(kind(1.0d0)) 3 dbl   :     1d 0
    real(kind(1.0d0)) 3 dbl   :,:   2d 0
    real(kind(1.0d0)) 3 dbl   :,:,: 3d 0
    logical           4 log   :     1d 0
    logical           4 log   :,:   2d 0
    logical           4 log   :,:,: 3d 0
    character(len=*)  5 char  :     1d len(data(1))
    character(len=*)  5 char  :,:   2d len(data(1,1))
    character(len=*)  5 char  :,:,: 3d len(data(1,1,1))
    complex           6 cmplx :     1d 0
    complex           6 cmplx :,:   2d 0
    complex           6 cmplx :,:,: 3d 0
    } {
    set subst [list TYPEDECL $typedecl TYPEID $typeid TYPEN $typen \
                    DIMDECL $dimdecl DIMN $dimn LENGTH $length]

    puts $outfile [string map $subst {
subroutine ipc_send_TYPEN_DIMN( comm, data, error )
    type(ipc_comm) :: comm
    TYPEDECL, dimension(DIMDECL) :: data
    logical        :: error
    integer        :: typeid = TYPEID
    integer        :: i
    integer        :: ierr
    integer        :: length

    error  = .false.
    length = LENGTH

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

subroutine ipc_receive_TYPEN_DIMN( comm, data, error )
    type(ipc_comm) :: comm
    TYPEDECL, dimension(DIMDECL) :: data
    logical        :: error
    integer        :: typeid = TYPEID
    integer        :: typeid_
    integer, dimension(3) :: shape_
    integer, dimension(3) :: exp_shape
    integer        :: i
    integer        :: ierr
    integer        :: length
    integer        :: length_

    error  = .false.
    length = LENGTH

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
end subroutine}]
}

close $outfile
