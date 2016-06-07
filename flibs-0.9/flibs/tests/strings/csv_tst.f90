! csv_tst.f90 --
!     Program to test the csv_file module to facilitate writing CSV-files
!
!     $Id: csv_tst.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
program csv_tst
    use csv_file

    integer :: i
    integer :: v
    integer :: lun
    logical :: adv
    real, dimension(3,4)    :: rand2d
    integer, dimension(3,4) :: array2d

    lun = 10
    open( lun, file = 'csv_test.csv' )

    do i = 0,8
        v = 10**i
        call csv_write( lun, v, .false. )
    enddo

    adv = .false.
    call csv_next_record( lun )
    do i = 0,8
        v = 10**i
        call csv_write( lun, v, adv )
        if ( i .gt. 5 ) adv = .true.
    enddo
    call csv_write( lun, 'Aha',     .false. )
    call csv_write( lun, '"Aha"',   .false. )
    call csv_write( lun, 'Aha "!"', .true.  )
    call csv_write( lun, 1.234, .true.  )
    call csv_write( lun, 1.234d0, .true.  )
    call csv_write( lun, (/1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /) )
    call random_number( rand2d )
    array2d = int( rand2d*100.0)
    write(*,*) array2d
    call csv_write( lun, array2d )
end program csv_tst
