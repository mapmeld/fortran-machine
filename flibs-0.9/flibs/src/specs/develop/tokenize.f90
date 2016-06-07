! tokenize.f90 --
!    Module for tokenizing a string
!
! Note:
! This module contains procedures to initialise a tokenizer
! and to use such a data structure to split up strings
!
! The tokenization takes care of two types of separator
! characters:
! 1. Characters like blanks that can occur in a whole sequence
!    but count as one (called "gap characters" for want of a better
!    name)
! 2. Characters like commas that each separately form a boundary
!    between tokens (called "separators").
! 3. Characters like quotes that delimit tokens
!
! The interaction between gaps and separators is ill-defined
! (for instance: "A#,#B" - if commas and hashes are separators and
! gaps, what tokens do you expect? A and B or A, "", "" and B?)
! therefore they should not both be used in the same tokenizer.
!
! $Id: tokenize.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!

! TOKENIZE --
!    Module for tokenizing strings
!
module TOKENIZE
   implicit none

   private
!
! -------- Define some convenient parameters:
!          whitespace - blanks only (gaps)
!          tsv        - tabs (typically as separators)
!          csv        - commas (typically as separators)
!          quotes     - " and ' (typically as delimiters)
!          empty      - no separators of this type
!
   character(len=1), public :: token_whitespace = ' '
   character(len=1), public :: token_tsv        = char(9)
   character(len=1), public :: token_csv        = ','
   character(len=2), public :: token_quotes     = '"'''
   character(len=0), public :: token_empty

!
! -------- Data structure defining the tokenizer
!
   type tokenizer
      integer               :: position
      character(len=10)     :: gaps
      character(len=10)     :: separators
      character(len=10)     :: delimiters
      integer               :: len_gaps
      integer               :: len_separators
      integer               :: len_delimiters
   end type tokenizer

!
! -------- Other public items
!
   public  tokenizer
   public  set_tokenizer
   public  first_token
   public  next_token

contains

! set_tokenizer
!    Initialise a tokenizer
!
! Arguments:
!    token          The tokenizer structure
!    gaps           Which characters form a gap
!    separators     Which characters are separators
!    delimiters     Which characters delimit tokens
!
! Note:
!    Gaps and separators are mutually exclusive. If both are
!    defined (not "empty"), then gaps take precedence.
!
subroutine set_tokenizer( token, gaps, separators, delimiters )
   type(tokenizer), intent(inout) :: token
   character(len=*), intent(in)   :: gaps
   character(len=*), intent(in)   :: separators
   character(len=*), intent(in)   :: delimiters

   token%position       = 1
   token%gaps           = gaps
   token%separators     = separators
   token%delimiters     = delimiters
   token%len_gaps       = len(gaps)
   token%len_separators = len(separators)
   token%len_delimiters = len(delimiters)

end subroutine set_tokenizer

! first_token --
!    Return the first token
!
! Arguments:
!    token       Tokenizer structure to use
!    string      String to parse
!    length      (Output) Length of the returned token
! Result:
!    substring that forms a single token
!
function first_token( token, string, length )
   type(tokenizer), intent(inout) :: token
   character(len=*), intent(in)   :: string
   integer, intent(out)           :: length

   character(len=len(string))     :: first_token

   token%position = 1
   first_token = next_token( token, string, length )
end function first_token

! next_token --
!    Return the next token
!
! Arguments:
!    token       Tokenizer structure to use
!    string      String to parse
!    length      (Output) Length of the returned token
! Result:
!    substring that forms a single token
!    the length is the length of the token that is returned,
!    a value -1 indicates the end of the string - no more tokens,
!    not even empty ones.
!
function next_token( token, string, length )
   type(tokenizer), intent(inout) :: token
   character(len=*), intent(in)   :: string
   integer, intent(out)           :: length

   character(len=len(string))     :: next_token
   !
   ! Hasty implementation: only gaps
   !
   if ( token%len_gaps .ne. 0 ) then
      next_token = next_token_gaps( token, string, length )
   else
      next_token = next_token_separs( token, string, length )
   endif

end function next_token

! next_token_gaps --
!    Return the next token, using gaps
!
! Arguments:
!    token       Tokenizer structure to use
!    string      String to parse
!    length      (Output) Length of the returned token
! Result:
!    substring that forms a single token
!
function next_token_gaps( token, string, length )
   type(tokenizer), intent(inout) :: token
   character(len=*), intent(in)   :: string
   integer, intent(out)           :: length

   character(len=len(string))     :: next_token_gaps

   integer                        :: lenstr
   integer                        :: pos
   integer                        :: pos1
   integer                        :: pos2

   lenstr = len(string)
   pos    = token%position
   pos1   = lenstr + 1

!   if ( token%len_delimiters > 0 ) then
!      if ( index(token%gaps(1:token%len_gaps), &
!                  string(pos:pos)) .gt. 0 ) then


starttoken: &
   do while ( pos .le. lenstr )
      if ( index(token%gaps(1:token%len_gaps), &
                 string(pos:pos)) .gt. 0 ) then
         pos = pos + 1
      else
         pos1 = pos
         exit starttoken
      endif
   enddo starttoken

endtoken: &
   do while ( pos .le. lenstr )
      if ( index(token%gaps(1:token%len_gaps), &
                 string(pos:pos)) .le. 0 ) then
         pos = pos + 1
      else
         pos2 = pos - 1
         exit endtoken
      endif
   enddo endtoken

   if ( pos1 .le. lenstr ) then
      next_token_gaps = string(pos1:pos2)
      length          = pos2-pos1+1
   else
      next_token_gaps = ' '
      length          = -1
   endif

   token%position = pos
end function next_token_gaps

! next_token_separs --
!    Return the next token, using separators
!
! Arguments:
!    token       Tokenizer structure to use
!    string      String to parse
!    length      (Output) Length of the returned token
! Result:
!    substring that forms a single token
!
function next_token_separs( token, string, length )
   type(tokenizer), intent(inout) :: token
   character(len=*), intent(in)   :: string
   integer, intent(out)           :: length

   character(len=len(string))     :: next_token_separs

   integer                        :: lenstr
   integer                        :: pos
   integer                        :: pos1
   integer                        :: pos2

   lenstr = len(string)
   pos    = token%position

   if ( index(token%separators(1:token%len_separators), &
              string(pos:pos)) .gt. 0 ) then
      pos = pos + 1
   endif

   pos1 = pos

endtoken: &
   do while ( pos .le. lenstr )
      if ( index(token%separators(1:token%len_separators), &
                 string(pos:pos)) .le. 0 ) then
         pos = pos + 1
      else
         pos2 = pos - 1
         exit endtoken
      endif
   enddo endtoken

   if ( pos1 .le. lenstr ) then
      next_token_separs = string(pos1:pos2)
      length            = pos2-pos1+1
   else
      next_token_separs = ' '
      length            = -1
   endif

   token%position = pos
end function next_token_separs

end module TOKENIZE
