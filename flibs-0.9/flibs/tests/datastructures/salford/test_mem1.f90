! test_mem1.f90 --
!     Define a module with included source
!
module mem1
    include 'mem1.f90'
end module

program test_mem1
    use mem1

    call printline( 'Hm' )

end program test_mem1
