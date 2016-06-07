! test_manip.f90 --
!     Program to test the string manipulation routines
!
!     $Id: test_manip.f90,v 1.2 2008/03/25 05:23:36 arjenmarkus Exp $
!
program test_manip
    use string_manipulation

    character(len=20) :: string = "Abcdef ghij"
    character(len=50) :: test_line = "the quick brown fox jumped over the lazy dog"

    write(*,*) 'String:   >', string, '<'
    write(*,*) 'Reversed: >', string_reverse(string), '<'

    write(*,*) 'String:    >', test_line, '<'
    write(*,*) 'Uppercase: >', string_toupper(test_line), '<'

    test_line = string_toupper(test_line)
    write(*,*) 'String:    >', test_line, '<'
    write(*,*) 'Uppercase: >', string_tolower(test_line), '<'

    write(*,*) 'Random strings:'
    do i = 1,10
       call random_word( string )
       write(*,*) '    >', string, '<'
    enddo

    write(*,*) 'Insert/delete/replace substrings:'

    test_line = 'ABCDEFGH'
    write(*,*) 'String:      >',test_line,'<'
    call string_insert( test_line, 1, 'abc' )
    write(*,*) 'Insert at 1: >',test_line,'<'
    test_line = 'ABCDEFGH'
    call string_insert( test_line, 2, 'abc' )
    write(*,*) 'Insert at 2: >',test_line,'<'

    test_line = 'ABCDEFGH'
    call string_delete( test_line, 1, 2 )
    write(*,*) 'Delete at 1: >',test_line,'<'
    test_line = 'ABCDEFGH'
    call string_delete( test_line, 2, 2 )
    write(*,*) 'Delete at 2: >',test_line,'<'

    test_line = 'ABCDEFGH'
    call string_replace( test_line, 'AB', 'abc' )
    write(*,*) 'Replace AB: >',test_line,'<'
    test_line = 'ABCDEFGH'
    call string_replace( test_line, 'XY', 'abc' )
    write(*,*) 'Replace XY: >',test_line,'<'

    test_line = 'ABABABBA'
    call string_map( test_line, 'AB', 'abc' )
    write(*,*) 'Map AB:     >',test_line,'<'

end program test_manip
