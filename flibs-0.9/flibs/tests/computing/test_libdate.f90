! test_libdate.f90 --
!     Simple tests for the libdate module
!
program test_diff
    use libdate

    implicit none

    type(DATETYPE)    :: date1, date2, date3
    integer           :: i
    logical           :: found

    character(len=20) :: datestring

    date1 = datetype( 2008, 1, 1, 0, 0 ) ! 1 january 2008, 0:00)
    date2 = datetype( 2009, 1, 1, 0, 0 ) ! 1 january 2009, 0:00)
    date3 = datetype( 2009, 2, 1, 0, 0 ) ! 1 february 2009, 0:00)

    write(*,*) 'Day of the year: ', doy(date1)
    write(*,*) 'Day of the year: ', doy(date2)
    write(*,*) 'Day of the year: ', doy(date3)
    write(*,*) '1 january 2008 > 1 january 2009? ', date1 > date2
    write(*,*) '1 january 2008 < 1 january 2009? ', date1 < date2
    write(*,*) 'First of 1 january 2009, 1 february 2009? ', mindate(date1 , date2)
    write(*,*) 'Last of 1 january 2009, 1 february 2009?  ', maxdate(date1 , date2)

    date3 = datetype( 2009, 2, 1, 10, 1 ) ! 2 february 2009, 10:01)

    write(*,*) 'Time between 1 february 0:00 and 2 february 10:01: ', &
        timelag(date2 , date3)

    call format_date( date3, 'yyyy/mm/dd HH:MM', datestring )
    write(*,*) 'Date: ', datestring
    call format_date( date3, 'yyyy/ms/ds HS:MS', datestring )
    write(*,*) 'Date: ', datestring
end program
