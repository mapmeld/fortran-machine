! Check if a string is a valid integer
!
program chk_integer
    implicit none

    integer :: ivalue
    integer :: ierror
    character(len=20) :: string

    ivalue = -1
    string = '12.34'
    read( string, *, iostat = ierror ) ivalue
    write( *, * ) 'Value: ', ivalue, ' - iostat: ', ierror, ' - ', okay(string)
    ivalue = -1
    string = '12.74'
    read( string, *, iostat = ierror ) ivalue
    write( *, * ) 'Value: ', ivalue, ' - iostat: ', ierror, ' - ', okay(string)
    ivalue = -1
    string = '12-74'
    read( string, *, iostat = ierror ) ivalue
    write( *, * ) 'Value: ', ivalue, ' - iostat: ', ierror, ' - ', okay(string)
    string = ' 12074 '
    read( string, *, iostat = ierror ) ivalue
    write( *, * ) 'Value: ', ivalue, ' - iostat: ', ierror, ' - ', okay(string)
    string = ' 12,74 '
    read( string, *, iostat = ierror ) ivalue
    write( *, * ) 'Value: ', ivalue, ' - iostat: ', ierror, ' - ', okay(string)


contains

logical function okay(string)
    character(len=*) :: string
    character(len=len(string)) :: first
    character(len=20)          :: form
    integer                    :: ierr
    integer                    :: ival

    read( string, * ) first
    write(form,'(a,i0,a)') '(i', len(string), ')'
    read(first, form, iostat = ierr ) ival
    okay = ierr == 0
end function okay

end program
