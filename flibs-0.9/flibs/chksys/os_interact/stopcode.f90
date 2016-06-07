! stopcode.f90 --
!     Small program to check if the STOP statement can be used to
!     transmit a return code to the operating system
!
!     The code is to be received by the calling shell script or
!     batch file. The correct value is 1
!
program stopcode
    write(*,*) 'Stopping with return code 1'
    stop 1
end program
