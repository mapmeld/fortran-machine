! readfile.f90 --
!     Sample program to robustly read an input like the one below:
!     A     B     C     D
!     1.0   xyz   1     3
!     1.2   x     2     4
!     3.4x  x     2     4
!     4           2     4
!     556   abc   2.0   6
!
!     Column 1 is a real number, column 2 is a string, columns 3 and 4
!     are integers. The last three lines contain errors but these should
!     be properly detected.
!
!     Note:
!     The purpose of this program is mainly to get the boilerplate code
!     correct for the genreader program.
!
!     TODO:
!     separators - how to detect the end of the record as opposed to an
!     empty field?
!
program readfile
    implicit none

    character(len=10) :: dummy
    integer           :: recno

    open( 10, file = 'data.inp' )
    read( 10, '(a)' ) dummy
    recno = 1
    call read_data( 10, recno )
    close( 10 )

contains

subroutine read_data( lun, recno )
    use tokenize
    integer, intent(in)      :: lun
    integer, intent(inout)   :: recno
    character(len=80)        :: line
    character(len=80)        :: field
    integer                  :: field_length
    integer                  :: fieldno
    integer                  :: ierr_
    logical                  :: error
    logical                  :: report_errors

    integer                  :: i_value
    real                     :: r_value
    real(kind=kind(1.0d0))   :: d_value
    logical                  :: l_value
    complex                  :: c_value
    character(len=len(line)) :: s_value

    type(tokenizer)          :: token

    type data_type
        real              :: a
        character(len=10) :: b
        integer           :: c
        integer           :: d
    end type data_type

    type(data_type)       :: data, data_default = data_type(-999.0, '???', -999, -999)

    call set_tokenizer( token, " ", "", '"' )

    report_errors = .true.
    do
        read( lun, '(a)', iostat = ierr_ ) line
        if ( ierr_ /= 0 ) exit

        recno   = recno + 1
        error   = .false.
        fieldno = 1
        field = first_token( token, line, field_length )

        data = data_default


        do while ( field_length >= 0 )

            select case ( fieldno )
                case ( 1 )
                    read( field, *, iostat = ierr_ ) r_value
                    if ( ierr_ /= 0 ) then
                        error = .true.
                        if ( report_errors ) then
                            write( *, * ) 'Field "',field(1:field_length),'" is &
                                &  not a valid real number - record: ', recno
                        endif
                    else
                        data%a = r_value
                    endif
                case ( 2 )
                    read( field, '(a)', iostat = ierr_ ) s_value
                    if ( ierr_ /= 0 ) then
                        error = .true.
                        if ( report_errors ) then
                            write( *, * ) 'Fields"',field(1:field_length),'" is &
                                &  not a valid string - record: ', recno
                        endif
                    else
                        data%b = s_value
                    endif
                case ( 3 )
                    read( field, *, iostat = ierr_ ) i_value
                    if ( ierr_ /= 0 ) then
                        error = .true.
                        if ( report_errors ) then
                            write( *, * ) 'Fields"',field(1:field_length),'" is &
                                &  not a valid integer - record: ', recno
                        endif
                    else
                        data%c = i_value
                    endif
                case ( 4 )
                    read( field, *, iostat = ierr_ ) i_value
                    if ( ierr_ /= 0 ) then
                        error = .true.
                        if ( report_errors ) then
                            write( *, * ) 'Fields"',field(1:field_length),'" is &
                                &  not a valid integer - record: ', recno
                        endif
                    else
                        data%d = i_value
                    endif
                case default
                    if ( report_errors ) then
                        write( *, * ) 'Record ',recno, ' has more fields than expected'
                    endif
            end select

            field = next_token( token, line, field_length )
            fieldno = fieldno + 1
        end do
        if ( fieldno <= 4 ) then
            if ( report_errors ) then
                write( *, * ) 'Record ',recno, ' has less fields than expected'
            endif
        endif
        write( *, '(i5,f10.3,1x,a,2i10)' ) recno, data
    end do
end subroutine
end program
