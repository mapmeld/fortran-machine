! csv_file.f90 --
!     Small module to facilitate writing CSV-files
!
!     $Id: csv_file.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
!     The module contains the following subroutines:
!     csv_next_record       Advance to the next record
!     csv_write_integer     Write a single integer to the CSV-file
!     csv_write_real        Write a single real to the CSV-file
!     csv_write_dble        Write a single double-precision real to the
!                           CSV-file
!     csv_write_char        Write a single character string to the
!                           CSV-file
!     csv_write_integer_1d  Write a 1D array of integers to the CSV-file
!     csv_write_real_1d     Write a 1D array of reals to the CSV-file
!     csv_write_dble_1d     Write a 1D array of double-precision real to
!                           the CSV-file
!     csv_write_char_1d     Write a 1D array of character strings to the
!                           CSV-file
!     csv_write_integer_2d  Write a 2D array of integers to the CSV-file
!     csv_write_real_2d     Write a 2D array of reals to the CSV-file
!     csv_write_dble_2d     Write a 2D array of double-precision real to
!                           the CSV-file
!     csv_write_char_2d     Write a 2D array of character strings to the
!                           CSV-file
!
!     For convenience, the generic name "csv_write" can be used
!     instead of the individual routines.
!
!     The file to write to must already be opened as a LU-number
!     is passed.
!
!     Layout of the CSV-file:
!     - single items are written to the end of the current record
!     - one-dimensional items are also written to the end of the current
!       record
!     - two-dimensional items are written to separate records, one for
!       each row
!     - except for the two-dimensional versions, all routines allow
!       you to suppress advancing to the next record:
!       - for single items you must indicate whether to advance or not
!       - for one-dimensional items, the argument is optional. Default
!         is to advance.
!
!     Note on the format:
!     CSV-files apparently come in different guises (Kernighan and Pike,
!     The practice of Programming, Addison-Wesley, 1999). This module
!     uses the following rules:
!     - items are always separated by a single comma (,)
!     - string items are delimited by double quotes (")
!     - embedded double quotes are treated by doubling the quote
!     - trailing blanks are considered irrelevant
!
module csv_file
    implicit none
    interface csv_write
        module procedure csv_write_integer
        module procedure csv_write_integer_1d
        module procedure csv_write_integer_2d
        module procedure csv_write_char
        module procedure csv_write_char_1d
        module procedure csv_write_char_2d
        module procedure csv_write_real
        module procedure csv_write_real_1d
        module procedure csv_write_real_2d
        module procedure csv_write_dble
        module procedure csv_write_dble_1d
        module procedure csv_write_dble_2d
    end interface
contains

! csv_next_record --
!     Go to the next record (convenience routine)
! Arguments:
!     lun        LU-number of the CSV-file
! Result:
!     The current record is closed, the next write will be to the
!     new record
! Note:
!     This is a convenience routine, it may result in a superfluous
!     comma at the end of the previous record. This does not seem to
!     be problematic, though, for MS Excel
!
subroutine csv_next_record( lun )
    integer, intent(in)   :: lun

    write(lun,'(a)') ''
end subroutine csv_next_record

! csv_write_integer/real/dble --
!     Write a single integer/real/double precision real to the CSV-file
! Arguments:
!     lun        LU-number of the CSV-file
!     value      Value to write
!     advance    Advance (.true.) or not, so that more items can be
!                written to the same record
! Result:
!     The value is written to the current record of the CSV-file
!
subroutine csv_write_integer( lun, value, advance )
    integer, intent(in)   :: lun
    integer, intent(in)   :: value
    logical, intent(in)   :: advance

    character(len=40)     :: buffer
    write( buffer, '(I10)' ) value
    buffer = adjustl(buffer)
    if ( advance ) then
       write(lun,'(a)') trim(buffer)
    else
       ! Most probably: write the comma only when needed
       ! - depends on other actions
       write(lun,'(a,a)',advance='no') trim(buffer), ','
    endif
end subroutine csv_write_integer

subroutine csv_write_real( lun, value, advance )
    integer, intent(in)   :: lun
    real, intent(in)      :: value
    logical, intent(in)   :: advance

    character(len=40)     :: buffer
    write( buffer, '(G14.6)' ) value
    buffer = adjustl(buffer)
    if ( advance ) then
       write(lun,'(a)') trim(buffer)
    else
       ! Most probably: write the comma only when needed
       ! - depends on other actions
       write(lun,'(a,a)',advance='no') trim(buffer), ','
    endif
end subroutine csv_write_real

subroutine csv_write_dble( lun, value, advance )
    integer, intent(in)                    :: lun
    real(kind=kind(1.0d0)), intent(in)     :: value
    logical, intent(in)                    :: advance

    character(len=40)     :: buffer
    write( buffer, '(G20.12)' ) value
    buffer = adjustl(buffer)
    if ( advance ) then
       write(lun,'(a)') trim(buffer)
    else
       ! Most probably: write the comma only when needed
       ! - depends on other actions
       write(lun,'(a,a)',advance='no') trim(buffer), ','
    endif
end subroutine csv_write_dble

! csv_write_char --
!     Write a single character string to the CSV-file
! Arguments:
!     lun        LU-number of the CSV-file
!     value      Value to write
!     advance    Advance (.true.) or not, so that more items can be
!                written to the same record
! Result:
!     The value is written to the current record of the CSV-file
!
subroutine csv_write_char( lun, value, advance )
    integer, intent(in)            :: lun
    character(len=*), intent(in)   :: value
    logical, intent(in)            :: advance

    integer                        :: k
    integer                        :: pos
    integer                        :: posb
    character(len=2*len(value))    :: buffer

    buffer = value

    !
    ! Check for nasty characters (")
    !
    k    = index( value,'"')
    pos  = 1
    posb = 1
    do while ( k .ge. 1 )
        buffer(posb:)   = value(pos:)
        buffer(posb+k:) = '"' // value(pos+k:)
        pos             = pos  + k + 1
        posb            = posb + k + 2
        k               = index( value(pos:),'"')
    enddo

    if ( advance ) then
       write(lun,'(3a)') '"',trim(buffer),'"'
    else
       write(lun,'(3a,a)',advance='no') '"',trim(buffer), '"', ','
    endif
end subroutine csv_write_char

! csv_write_integer/real/dble_1d --
!     Write a one-dimensional array of items to the CSV-file
! Arguments:
!     lun        LU-number of the CSV-file
!     array      Array to write
!     advance    Advance (.true.) or not, so that more items can be
!                written to the same record
! Result:
!     The array is written to the current record of the CSV-file
! Note:
!     Because the four routines of this type differ only in
!     the data type, we use an include file for the body.
!
subroutine csv_write_integer_1d( lun, array, advance )
    integer, dimension(:), intent(in)   :: array

    include 'csv_file_1d.f90'

end subroutine csv_write_integer_1d

subroutine csv_write_real_1d( lun, array, advance )
    real, dimension(:), intent(in)   :: array

    include 'csv_file_1d.f90'

end subroutine csv_write_real_1d

subroutine csv_write_dble_1d( lun, array, advance )
    real(kind=kind(1.0d0)), dimension(:), intent(in)   :: array

    include 'csv_file_1d.f90'

end subroutine csv_write_dble_1d

subroutine csv_write_char_1d( lun, array, advance )
    character(len=*), dimension(:), intent(in)   :: array

    include 'csv_file_1d.f90'

end subroutine csv_write_char_1d

! csv_write_integer/real/dble_2d --
!     Write a two-dimensional array of items to the CSV-file
! Arguments:
!     lun        LU-number of the CSV-file
!     array      Array to write
! Result:
!     The array is written to the current record of the CSV-file
! Note:
!     Because the four routines of this type differ only in
!     the data type, we use an include file for the body.
!
subroutine csv_write_integer_2d( lun, array )
    integer, dimension(:,:), intent(in)   :: array

    include 'csv_file_2d.f90'

end subroutine csv_write_integer_2d

subroutine csv_write_real_2d( lun, array )
    real, dimension(:,:), intent(in)   :: array

    include 'csv_file_2d.f90'

end subroutine csv_write_real_2d

subroutine csv_write_dble_2d( lun, array )
    real(kind=kind(1.0d0)), dimension(:,:), intent(in)   :: array

    include 'csv_file_2d.f90'

end subroutine csv_write_dble_2d

subroutine csv_write_char_2d( lun, array )
    character(len=*), dimension(:,:), intent(in)   :: array

    include 'csv_file_2d.f90'

end subroutine csv_write_char_2d

end module csv_file
