! stringquery.f90 --
!     Module for simple string queries:
!
!     string_count_substring   Return the number of times a substring occurs
!     string_longest_repeated  Find the longest substring that occurs at least twice
!     string_longest_common    Find the longest substring that occurs in two different strings
!
!     $Id$
!
module string_queries
    implicit none
contains

! string_count_substring --
!     Return the number of times a substring occurs
! Arguments:
!     string     String to be examined
!     substr     Substring in question
! Result:
!     Number of occurrences
! Note:
!     Trailing blanks _are_ taken into account.
!     Possible overlaps are ignored:
!     string = 'ababab' and substr = 'abab'
!     will give the answer 1, not 2
!
function string_count_substring( string, substr ) result( count )
    character(len=*)           :: string
    character(len=*)           :: substr

    integer                    :: start
    integer                    :: count

    count  = 0
    start  = 0
    do
        start = index( string(start+1:), substr )
        count = count + 1
        if ( start == 0 ) exit
    enddo
end function string_count_substring

end module string_queries
