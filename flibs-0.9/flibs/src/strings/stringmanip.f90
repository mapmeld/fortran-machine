! stringmanip.f90 --
!     Module for simple string manipulations:
!
!     string_reverse         Return the reverted string
!     string_tolower         Return the string with upper-case letters translated to lower-case
!     string_toupper         Return the string with lower-case letters translated to upper-case
!     random_word            Construct a random word
!
!     The functions actually perform fairly simple string manipulations.
!     It is just that these manipulations occur frequently.
!
!     $Id: stringmanip.f90,v 1.4 2008/03/27 20:07:25 arjenmarkus Exp $
!
module string_manipulation
    implicit none

    character(len=26), parameter, private :: upper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=26), parameter, private :: lower = 'abcdefghijklmnopqrstuvwxyz'
contains

! string_reverse --
!     Return a string that has all characters in reverse order
! Arguments:
!     string     String to be reversed
! Result:
!     Reversed string
!
function string_reverse( string ) result (rev)
    character(len=*)           :: string

    character(len=len(string)) :: rev

    integer                    :: i
    integer                    :: length

    length = len(string)
    do i = 1,length
        rev(i:i) = string(length-i+1:length-i+1)
    enddo
end function string_reverse

! string_toupper --
!     Return a string that has all _letters_ in upper case
! Arguments:
!     string     String to be treated
! Result:
!     String with letters turned into upper case
!
function string_toupper( string ) result (new)
    character(len=*)           :: string

    character(len=len(string)) :: new

    integer                    :: i
    integer                    :: k
    integer                    :: length

    length = len(string)
    new    = string
    do i = 1,length
        k = index( lower, string(i:i) )
        if ( k > 0 ) then
           new(i:i) = upper(k:k)
        endif
    enddo
end function string_toupper

! string_tolower --
!     Return a string that has all _letters_ in lower case
! Arguments:
!     string     String to be treated
! Result:
!     String with letters turned into lower case
!
function string_tolower( string ) result (new)
    character(len=*)           :: string

    character(len=len(string)) :: new

    integer                    :: i
    integer                    :: k
    integer                    :: length

    length = len(string)
    new    = string
    do i = 1,length
        k = index( upper, string(i:i) )
        if ( k > 0 ) then
           new(i:i) = lower(k:k)
        endif
    enddo
end function string_tolower

! random_word --
!     Fill a string with a random sequence of letters
! Arguments:
!     string     String to be filled
! Result:
!     String with random letters
!
subroutine random_word( string )
    character(len=*)           :: string

    integer                    :: i
    integer                    :: k
    integer                    :: length
    real                       :: r

    string = ' '
    length = len(string)
    call random_number( r )
    length = 2 + (length-2) * r
    do i = 1,length
        call random_number( r )
        k = 1 + 26 * r
        if ( k > 26 ) k = 1
        string(i:i) = lower(k:k)
    enddo
end subroutine random_word

! string_insert --
!     Insert a string in another string
! Arguments:
!     string     String to be changed
!     pos        Position to insert it
!     second     String to be inserted
! Result:
!     String with second string inserted
! Note:
!     - If the receiving string is not long enough, the result is truncated
!     - The entire second string is inserted (including any trailing blanks)
!
subroutine string_insert( string, pos, second )
    character(len=*), intent(inout) :: string
    integer, intent(in)             :: pos
    character(len=*), intent(in)    :: second

    integer                         :: length

    length = len( second )
    string(pos+length:)      = string(pos:)
    string(pos:pos+length-1) = second

end subroutine string_insert

! string_delete --
!     Delete a substring in another string
! Arguments:
!     string     String to be changed
!     pos        Position to delete
!     length     Number of characters to delete
! Result:
!     String with substring removed
!
subroutine string_delete( string, pos, length )
    character(len=*), intent(inout) :: string
    integer, intent(in)             :: pos
    integer, intent(in)             :: length

    string(pos:)             = string(pos+length:)

end subroutine string_delete

! string_replace --
!     Replace one substring by another substring
! Arguments:
!     string     String to be changed
!     substr     Substring to be removed
!     replace    Substring to replace it with
! Result:
!     String with substring replaced
! Note:
!     The entire replacing string is used and the result may be
!     truncated. Only the first occurrence is replaced
!
subroutine string_replace( string, substr, replace )
    character(len=*), intent(inout) :: string
    character(len=*), intent(in)    :: substr
    character(len=*), intent(in)    :: replace

    integer                         :: k

    k = index( string, substr )
    if ( k > 0 ) then
        call string_delete( string, k, len(substr) )
        call string_insert( string, k, replace )
    endif

end subroutine string_replace

! string_map --
!     Replace all occurrences of a substring by another substring
! Arguments:
!     string     String to be changed
!     substr     Substring to be removed
!     replace    Substring to replace it with
! Result:
!     String with substring replaced
! Note:
!     The entire replacing string is used and the result may be
!     truncated. All occurrences are replaced
!
subroutine string_map( string, substr, replace )
    character(len=*), intent(inout) :: string
    character(len=*), intent(in)    :: substr
    character(len=*), intent(in)    :: replace

    integer                         :: k
    integer                         :: start

    start = 1
    do
        k = index( string(start:), substr )
        if ( k > 0 ) then
            start = start + k - 1
            call string_delete( string, start, len(substr) )
            call string_insert( string, start, replace )
            start = start + len(replace)
        else
            exit
        endif
    enddo

end subroutine string_map

end module string_manipulation
