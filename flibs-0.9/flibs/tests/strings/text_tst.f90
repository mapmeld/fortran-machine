!  text_tst.f90 --
!      Test the modules manipulating arbitrary-length strings
!
!      $Id: text_tst.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
program text_tst
    use multiple_line_text

    type(MULTILINE_TEXT)  :: file
    type(TEXT_STRING)     :: text
    type(TEXT_STRING), pointer :: textp
    character(len=70)     :: string1
    character(len=170)    :: string2
    character(len=10)     :: string
    logical               :: eof
    integer               :: i

    call txt_from_string( text, &
'123456789_123456789_123456789_123456789_123456789_&
&123456789_123456789_123456789_' )

    call txt_to_string( text, string1 )
    write(*,*) '>', trim(string1), '<'
    call txt_to_string( text, string2 )
    write(*,*) '>', trim(string2), '<'

    call chklength(text)

    !
    ! Check delete/insert
    !
    string = '1234567890'
    call edit_delete( string, 1, 2 )
    write(*,*) 'Deleted: ', string, ' - expected: 34567890'
    string = '1234567890'
    call edit_delete( string, 0, 2 )
    write(*,*) 'Deleted: ', string, ' - expected: 234567890'

    string = '1234567890'
    call edit_insert( string, 3, 'AB' )
    write(*,*) 'Inserted: ', string, ' - expected: 123AB45678'

    string = '1234567890'
    call edit_insert( string, 0, 'AB' )
    write(*,*) 'Inserted: ', string, ' - expected: AB12345678'

    open( 10, file = 'textstr.f90' )
    do i = 1,4
        call txt_read_from_file( 10, text, eof )
        call txt_insert_string( text, 1, '<>' )
        call txt_write_to_file( 0, text )
        call mltxt_insert( file, MLTXT_END, text )
    enddo

    call mltxt_delete( file, 2 )
    do i = 1,mltxt_number( file )
        call mltxt_get( file, i, textp )
        call txt_write_to_file( 0, textp )
    enddo

    stop
contains
subroutine chklength(text)
    type(TEXT_STRING) :: text
    character(len=txt_length(text)) :: string
    call txt_to_string( text, string )
    write(*,*) '>', string, '<'
end subroutine
end program
