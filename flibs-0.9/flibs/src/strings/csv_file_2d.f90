! cvs_file_2d.f90 --
!     Include file for csv_file.f90:
!     contains the body of the two-dimensional version of the
!     writing routines.
!
!     $Id: csv_file_2d.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
    integer, intent(in)                 :: lun

    logical                             :: adv
    integer                             :: i

    adv = .true.

    do i = 1,size(array,2)
        call csv_write( lun, array(:,i), adv )
    enddo
!
! end of body
