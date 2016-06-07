! An implementation of QuickSort in functional programming style
!
program sortdata
    real, dimension(1000) :: data

    call random_number( data )

    write(*,*) 'First 20: '
    write(*,'(5f10.4)') data(1:20)
    write(*,*) 'Last 20: '
    write(*,'(5f10.4)') data(1000-21:1000)

    data = qsort_reals( data )
    write(*,*) 'Sorted:'
    write(*,*) 'First 20: '
    write(*,'(5f10.4)') data(1:20)
    write(*,*) 'Last 20: '
    write(*,'(5f10.4)') data(1000-21:1000)

contains
recursive function qsort_reals( data ) result( sorted )
    real, dimension(:), intent(in) :: data
    real, dimension(1:size(data))  :: sorted

    if ( size(data) > 1 ) then
        sorted = (/ qsort_reals( pack( data(2:), data(2:) > data(1) ) ), &
                    data(1),                                             &
                    qsort_reals( pack( data(2:), data(2:) <= data(1) ) ) /)
    else
        sorted = data
    endif
end function
end program
