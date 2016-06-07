! globmatch.f90 --
!     Match strings according to (simplified) glob patterns
!
!     The pattern matching is limited to literals, * and ?
!     (character classes are not supported). A backslash escapes
!     any character.
!
!     $Id: globmatch.f90,v 1.5 2006/03/26 19:03:53 arjenmarkus Exp $
!
module glob_matching
    implicit none

    character(len=1), parameter, private :: backslash = '\\'
    character(len=1), parameter, private :: star      = '*'
    character(len=1), parameter, private :: question  = '?'

contains

! string_match --
!     Tries to match the given string with the pattern
! Arguments:
!     string     String to be examined
!     pattern    Glob pattern to be used for the matching
! Result:
!     .true. if the entire string matches the pattern, .false.
!     otherwise
! Note:
!     Trailing blanks are ignored
!
recursive function string_match( string, pattern ) result(match)
    character(len=*), intent(in) :: string
    character(len=*), intent(in) :: pattern
    logical                      :: match

    character(len=len(pattern))  :: literal
    integer                      :: ptrim
    integer                      :: p
    integer                      :: k
    integer                      :: ll
    integer                      :: method
    integer                      :: start
    integer                      :: strim

    match  = .false.
    method = 0
    ptrim  = len_trim( pattern )
    strim  = len_trim( string )
    p      = 1
    ll     = 0
    start  = 1

    !
    ! Split off a piece of the pattern
    !
    do while ( p <= ptrim )
        select case ( pattern(p:p) )
            case( star )
                if ( ll .ne. 0 ) exit
                method = 1
            case( question )
                if ( ll .ne. 0 ) exit
                method = 2
                start  = start + 1
            case( backslash )
                p  = p + 1
                ll = ll + 1
                literal(ll:ll) = pattern(p:p)
            case default
                ll = ll + 1
                literal(ll:ll) = pattern(p:p)
        end select

        p = p + 1
    enddo

    !
    ! Now look for the literal string (if any!)
    !
    if ( method == 0 ) then
        !
        ! We are at the end of the pattern, and of the string?
        !
        if ( strim == 0 .and. ptrim == 0 ) then
            match = .true.
        else
            !
            ! The string matches a literal part?
            !
            if ( ll > 0 ) then
                if ( string(start:min(strim,start+ll-1)) == literal(1:ll) ) then
                    start = start + ll
                    match = string_match( string(start:), pattern(p:) )
                endif
            endif
        endif
    endif

    if ( method == 1 ) then
        !
        ! Scan the whole of the remaining string ...
        !
        if ( ll == 0 ) then
            match = .true.
        else
            do while ( start <= strim )
                k     = index( string(start:), literal(1:ll) )
                if ( k > 0 ) then
                    start = start + k + ll - 1
                    match = string_match( string(start:), pattern(p:) )
                    if ( match ) then
                        exit
                    endif
                endif

                start = start + 1
            enddo
        endif
    endif

    if ( method == 2 .and. ll > 0 ) then
        !
        ! Scan the whole of the remaining string ...
        !
        if ( string(start:min(strim,start+ll-1)) == literal(1:ll) ) then
            match = string_match( string(start+ll:), pattern(p:) )
        endif
    endif
    return
end function string_match

end module glob_matching

program test_match
    use glob_matching

    character(len=20) :: string
    character(len=20) :: pattern

    string  = 'abcdefghijk' ; pattern = '?b*'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'abcdefghijk' ; pattern = '*c*'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'abcdefghijk' ; pattern = '*c'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'abcdefghijk' ; pattern = '*c*k'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'LS' ; pattern = '?OW'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string(1:2),pattern)

    string  = 'teztit' ; pattern = 'tez*t*t'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    !
    ! Two pattern match problems that might pose difficulties
    !
    string  = 'abcde ' ; pattern = '*e *'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string(1:6),pattern)

    string  = 'baaaaa' ; pattern = 'b*a'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'bababa' ; pattern = 'b*ba'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'baaaaax' ; pattern = 'b*ax'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'baaaaa' ; pattern = 'b*ax'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = 'baaaaax' ; pattern = 'b*a'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = '' ; pattern = 'b*'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string,pattern)

    string  = '3' ; pattern = '??'
    write(*,*) 'String: ', string, '- pattern: ', pattern, ' - match: ', &
        string_match(string(1:1),pattern)
end program
