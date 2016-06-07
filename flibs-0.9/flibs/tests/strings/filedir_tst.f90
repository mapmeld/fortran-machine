! filedir_tst.f90 --
!     Test the module to manipulate file and directory names
!
!     $Id: filedir_tst.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!

program filedir_tst
    use filedir

    character(len=40), dimension(5,10) :: filename
    integer                            :: i
    integer                            :: errors

    filename(1,1) = 'd:\some\file\file.inp' ! Full name
    filename(2,1) = 'inp'                   ! Extension
    filename(3,1) = 'file.inp'              ! Base name
    filename(4,1) = 'd:\some\file\file'     ! Root name
    filename(5,1) = 'd:\some\file\'         ! Directory

    filename(1,2) = 'd:\some\file\file'     ! Full name
    filename(2,2) = ''                      ! Extension
    filename(3,2) = 'file'                  ! Base name
    filename(4,2) = 'd:\some\file\file'     ! Root name
    filename(5,2) = 'd:\some\file\'         ! Directory

    filename(1,3) = 'd:\some\file\file.'    ! Full name
    filename(2,3) = ''                      ! Extension
    filename(3,3) = 'file.'                 ! Base name
    filename(4,3) = 'd:\some\file\file'     ! Root name
    filename(5,3) = 'd:\some\file\'         ! Directory

    filename(1,4) = 'd:\some\file\.file'    ! Full name
    filename(2,4) = ''                      ! Extension
    filename(3,4) = '.file'                 ! Base name
    filename(4,4) = 'd:\some\file\.file'    ! Root name
    filename(5,4) = 'd:\some\file\'         ! Directory

    filename(1,4) = 'd:\some\file\file..inp' ! Full name
    filename(2,4) = 'inp'                    ! Extension
    filename(3,4) = 'file..inp'              ! Base name
    filename(4,4) = 'd:\some\file\file.'     ! Root name
    filename(5,4) = 'd:\some\file\'          ! Directory

    errors = 0
    do i = 1,4
        if ( filedir_extension(filename(1,i)) .ne. filename(2,i) ) then
           write(*,*) &
               'Expected: ', filename(2,i),&
               ' - got: ', filedir_extension(filename(1,i))
           errors = errors + 1
        endif
        if ( filedir_basename(filename(1,i)) .ne. filename(3,i) ) then
           write(*,*) &
               'Expected: ', filename(3,i),&
               ' - got: ', filedir_basename(filename(1,i))
           errors = errors + 1
        endif
        if ( filedir_rootname(filename(1,i)) .ne. filename(4,i) ) then
           write(*,*) &
               'Expected: ', filename(4,i),&
               ' - got: ', filedir_rootname(filename(1,i))
           errors = errors + 1
        endif
        if ( filedir_dirname(filename(1,i)) .ne. filename(5,i) ) then
           write(*,*) &
               'Expected: ', filename(5,i),&
               ' - got: ', filedir_dirname(filename(1,i))
           errors = errors + 1
        endif
    enddo
    write(*,*) 'Number of errors: ', errors

    filename(1,6) = 'file'
    write(*,*) 'Expected: file.inp - got: ', &
        filedir_add_extension(filename(1,6), 'inp')

    filename(1,6) = 'file'
    write(*,*) 'Expected: file.inp - got: ', &
        filedir_add_extension(filename(1,6), '.inp')

    filename(1,6) = 'file.'
    write(*,*) 'Expected: file.inp - got: ', &
        filedir_add_extension(filename(1,6), '.inp')

    filename(1,6) = 'file.'
    write(*,*) 'Expected: file..inp - got: ', &
        filedir_add_extension(filename(1,6), '..inp')

    filename(1,7) = 'd:\some\file'           ! Directory
    filename(2,7) = 'inp'                    ! File name
    filename(3,7) = 'd:\some\file\inp'       ! Result

    filename(1,8) = '/some/file'             ! Directory
    filename(2,8) = 'inp'                    ! File name
    filename(3,8) = '/some/file/inp'         ! Result

    filename(1,9) = 'file'                   ! Directory
    filename(2,9) = 'inp'                    ! File name
    filename(3,9) = 'file/inp'               ! Result

    errors = 0
    do i = 7,9
        if ( filedir_concat(filename(1,i),filename(2,i)) .ne. &
                filename(3,i) ) then
           write(*,*) &
               'Expected: ', filename(3,i),&
               ' - got: ', filedir_concat(filename(1,i),filename(2,i))
           errors = errors + 1
        endif
    enddo
    write(*,*) 'Number of errors: ', errors

end program filedir_tst
