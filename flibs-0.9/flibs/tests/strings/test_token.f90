! test_token.f90 --
!    Incomplete test program for the tokenize module
!
!    $Id: test_token.f90,v 1.3 2008/04/02 08:30:55 arjenmarkus Exp $
!
program test_token
   use tokenize
   implicit none

   type(tokenizer)   :: token
   character(len=80) :: string
   character(len=80) :: substring
   integer           :: length

   !
   ! Plain tokenizer: whitespace
   !
   call set_tokenizer( token, token_whitespace, token_empty, token_empty )
   string = '  tokenizer(     token, whitespace, empty, empty )'
   substring = first_token( token, string, length )

   write(*,*) 'Split on spaces: '
   write(*,*) '>>', trim(string), '<<'
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   !
   ! Comma-separated values
   !
   call set_tokenizer( token, token_empty, token_csv, token_empty )
   string = 'tokenizer(     token,, whitespace, empty, empty )'

   write(*,*) 'Split on commas: '
   write(*,*) '>>', trim(string), '<<'
   substring = first_token( token, string, length )
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   !
   ! Include delimited strings
   !
   call set_tokenizer( token, token_whitespace, token_empty, '"' )
   string = ' Just say "Hello, world!", as an example'

   write(*,*) 'Split on spaces, delimiters'
   write(*,*) '>>', trim(string), '<<'
   substring = first_token( token, string, length )
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   !
   ! Include delimited strings
   !
   call set_tokenizer( token, token_empty, token_csv, '"' )
   string = ' Just say,"Hello, world!", as an example'

   write(*,*) 'Split on commas, delimiters'
   write(*,*) '>>', trim(string), '<<'
   substring = first_token( token, string, length )
   do while ( length .ge. 0 )
      write(*,*) length, '>>', substring(1:length), '<<'
      substring = next_token( token, string, length )
   enddo

   stop
end program
