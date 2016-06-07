! csvdata.f90 --
!    Program to generate a simple CSV file with fictitious data
!
!    $Id: csvdata.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
program csvdata
   implicit none

   integer                         :: lun = 10
   integer                         :: i
   real, dimension(5)              :: r
   character(len=100)              :: line
   character(len=40)               :: string
   character(len=20), dimension(6) :: station = &
      (/ 'NW1', 'NW2', 'OS30', 'DH', 'DO', 'Ah111' /)

   open( lun, file = 'somedata.csv' )
   write( lun, * ) 'station,date,salinity,temperature'

   do i = 1,100
      call random_number( r )

      line = station(1+int(5.0*r(1)))

      write( string, '(i0,a,i0,a,i0)' ) 2005, '-', 1+int(12.0*r(2)), '-', 1+int(28.0*r(3))
      line = trim(line) // ',' // string

      write( string, '(f10.2)' ) 28.0+6.0*r(4)
      line = trim(line) // ',' // adjustl(string)

      write( string, '(f10.2)' ) 15.0+5.0*r(5)
      line = trim(line) // ',' // adjustl(string)

      write( lun, '(a)' ) trim(line)
   enddo

end program
