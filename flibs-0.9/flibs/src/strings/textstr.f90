!  textstr.f90 --
!      Modules manipulating arbitrary-length strings
!
!  Arjen Markus
!
!  General information:
!  These modules define a set of subroutines and functions to manipulate
!  strings and collections of strings:
!  edit_text       - module for simple editing actions on strings
!  singleline_text - module for single, arbitrary-length strings
!  multiline_text  - module for collections of such strings (like a
!                    text file)
!
!  Note:
!  A replace-string routine does not seem very useful, one
!  can create it by combining the delete and insert functions
!
!  $Id: textstr.f90,v 1.3 2006/03/26 19:03:53 arjenmarkus Exp $
!
module edit_text
   implicit none
contains

! edit_delete --
!    Delete a substring from a string
! Arguments:
!    string      String from which a substring must be removed
!    pos         First position to be removed
!    length      Length of the substring to be removed
!
subroutine edit_delete( string, pos, length )
    character(len=*), intent(inout)  :: string
    integer, intent(in)              :: pos
    integer, intent(in)              :: length

    integer                          :: firstpos
    integer                          :: lastpos

    firstpos = min( max( 1, pos ), len(string) )
    lastpos  = min( max( 1, pos+length-1 ), len(string))
    string   = string(1:firstpos-1) // string(lastpos+1:)
end subroutine edit_delete

! edit_insert --
!    Insert a substring into a string
! Arguments:
!    string      String into which another substring must be inserted
!    pos         Position after which to insert the substring
!    substring   Substring to be inserted
!
subroutine edit_insert( string, pos, substring )
    character(len=*), intent(inout)  :: string
    integer, intent(in)              :: pos
    character(len=*), intent(in)     :: substring

    integer                          :: insertpos

    insertpos = min( max( 0, pos ), len(string) )
    string    = string(1:insertpos) // substring // string(insertpos+1:)
end subroutine edit_insert

end module edit_text

module singleline_text
   use edit_text
   implicit none

   integer, parameter, private :: SEGMENT_LENGTH = 40

   type TEXT_STRING
       private
       integer :: length = 0
       character(len=SEGMENT_LENGTH), pointer, dimension(:) :: text => null()
   endtype TEXT_STRING

contains

! txt_length --
!    Returns the length of the string in the text_string type
! Arguments:
!    text        Text string to be examined
! Prerequisite:
!    Text string is properly initialised
!
pure integer function txt_length( text )
    type(TEXT_STRING), intent(in) :: text
    txt_length = text%length
end function txt_length

! txt_cleanup --
!    Cleans up a text_string type
! Arguments:
!    text        Text string to be cleaned up
! Prerequisite:
!    Text string is properly initialised
!
subroutine txt_cleanup( text )
    type(TEXT_STRING), intent(inout) :: text

    text%length = 0
    if ( associated(text%text) ) then
        deallocate(text%text)
    endif
end subroutine txt_cleanup

! txt_index --
!    Returns the index of a substring in the text_string type
! Arguments:
!    text        Text string to be examined
!    substring   Substring to be found
!    back        (Optional) backward?
! Prerequisite:
!    Text string is properly initialised
!
integer function txt_index( text, substring, back )
    type(TEXT_STRING), intent(in)   :: text
    character(len=*), intent(in)    :: substring
    logical, optional, intent(in)   :: back

    character(len=txt_length(text)) :: copy
    logical                         :: backw

    backw = .false.
    if ( present(back) ) backw = back

    call txt_to_string( text, copy )
    txt_index  = index( copy, substring, backw )
end function txt_index

! txt_from_string --
!    Store a string in a text_string type
! Arguments:
!    text        Text string to be created/filled
!    string      Ordinary character string to be copied
! Side effects:
!    Text string is properly initialised
!
subroutine txt_from_string( text, string )
    type(TEXT_STRING), intent(inout) :: text
    character(len=*), intent(in)     :: string

    integer :: i
    integer :: nosegm

    nosegm = ( len(string) + segment_length - 1 ) / segment_length

    if ( associated(text%text) ) then
        if ( size(text%text) .lt. nosegm ) then
            deallocate(text%text)
        endif
    endif

    if ( .not. associated(text%text) ) then
        allocate( text%text(1:nosegm) )
    endif

    do i = 1,nosegm-1
        text%text(i) = string(1+(i-1)*segment_length:i*segment_length)
    enddo
    text%text(nosegm) = string(1+(nosegm-1)*segment_length:)
    text%length       = len(string)

end subroutine txt_from_string

! txt_to_string --
!    Store the contents of a text_string type in an ordinary string
! Arguments:
!    text        Text string to be created
!    string      Ordinary character string to contain the contents
! Prerequisite:
!    Text string is properly initialised
!
subroutine txt_to_string( text, string )
    type(TEXT_STRING), intent(in)    :: text
    character(len=*), intent(out)    :: string

    integer :: i
    integer :: nosegm

    nosegm = ( min(len(string),text%length) + segment_length - 1 ) / &
               segment_length

    do i = 1,nosegm-1
        string(1+(i-1)*segment_length:i*segment_length) = text%text(i)
    enddo
    string(1+(nosegm-1)*segment_length:) = text%text(nosegm)

end subroutine txt_to_string

! txt_read_from_file --
!    Read a text string from a file (one complete line)
! Arguments:
!    lun         LU-number of the file to be read
!    text        Text string to be created/filled
!    eof         Whether end-of-file was reached or not
! Side effects:
!    Text string is properly initialised
!
subroutine txt_read_from_file( lun, text, eof )
    integer, intent(in)              :: lun
    type(TEXT_STRING), intent(inout) :: text
    logical, intent(out)             :: eof

    character(len=SEGMENT_LENGTH)    :: segment
    character(len=SEGMENT_LENGTH), pointer, dimension(:) :: parts
    character(len=SEGMENT_LENGTH), pointer, dimension(:) :: parts_save
    integer                          :: reclen
    integer                          :: noparts
    logical                          :: eor

    eof     = .false.
    noparts = 1
    allocate( parts(1:noparts) )

    !
    ! Read the record in segments
    !
    do
        read( lun, end = 180, eor = 170, fmt = '(a)', &
              size = reclen, advance='no') segment
        !
        ! Not at the end yet, so increase the size
        !
        parts_save => parts
        parts      => null()
        noparts    =  noparts + 1
        allocate( parts(1:noparts) )
        parts(1:noparts-1) = parts_save
        parts(noparts-1)   = segment     ! The last segment is stored
                                         ! when the end-of-record is
                                         ! reached
        deallocate( parts_save )
    enddo

    !
    ! End of record (end of file will give an empty text string)
    !
170 continue
    parts(noparts) = segment
    if ( associated(text%text) ) then
        deallocate( text%text )
    endif
    text%text   => parts
    text%length =  (noparts-1) * segment_length + reclen
    return

    !
    ! End of file
    !
180 continue
    if ( associated(text%text) ) then
        deallocate( text%text )
    endif
    text%length = 0
    eof = .true.
    return

    !
    ! Errors are not handled explicitly ...
    !
end subroutine txt_read_from_file

! txt_write_to_file --
!    Write a text string to a file (one complete line)
! Arguments:
!    lun         LU-number of the file to be written to
!                (if LU <= 0, write to screen)
!    text        Text string to be written
! Prerequisites:
!    Text string is properly initialised
!
subroutine txt_write_to_file( lun, text )
    integer, intent(in)              :: lun
    type(TEXT_STRING), intent(inout) :: text

    character(len=txt_length(text))  :: string

    call txt_to_string( text, string )
    if ( lun .gt. 0 ) then
        write( lun, '(a)' ) string
    else
        write( *, '(1x,a)' ) string
    endif
end subroutine txt_write_to_file

! txt_delete_string --
!    Delete a substring from a text_string type
! Arguments:
!    text        Text string to be treated
!    pos         First position to be removed
!    length      Length of the substring to be removed
! Prerequisite:
!    Text string must be properly initialised
!
subroutine txt_delete_string( text, pos, length )
    type(TEXT_STRING), intent(inout) :: text
    integer, intent(in)              :: pos
    integer, intent(in)              :: length

    character(len=txt_length(text)) :: newstring

    call txt_to_string( text, newstring )
    call edit_delete( newstring, pos, length )
    call txt_from_string( text, newstring )

end subroutine txt_delete_string

! txt_insert_string --
!    Insert a substring into a text_string type
! Arguments:
!    text        Text string to be treated
!    pos         Position after which to insert the string
!    string      Ordinary character string to be inserted
! Prerequisite:
!    Text string must be properly initialised
!
subroutine txt_insert_string( text, pos, string )
    type(TEXT_STRING), intent(inout) :: text
    integer, intent(in)              :: pos

    character(len=*), intent(in)     :: string

    character(len=txt_length(text)+len(string)) :: newstring

    call txt_to_string( text, newstring )
    call edit_insert( newstring, pos, string )
    call txt_from_string( text, newstring )

end subroutine txt_insert_string

end module singleline_text

module multiple_line_text
   use singleline_text
   implicit none

   type MULTILINE_TEXT
       private
       integer :: max_length = 0
       type(TEXT_STRING), pointer, dimension(:) :: text => null()
   end type MULTILINE_TEXT

   interface mltxt_insert
       module procedure mltxt_insert_string
       module procedure mltxt_insert_text
   end interface

   integer, parameter  :: MLTXT_END = -123456

contains

! mltxt_length --
!    Returns the (maximum) length of the strings in the multiline text
! Arguments:
!    mltext      Multiline text to be examined
! Prerequisite:
!    Multiline text is properly initialised
!
pure integer function mltxt_length( text )
    type(MULTILINE_TEXT), intent(in) :: text
    mltxt_length = text%max_length
end function mltxt_length

! mltxt_number --
!    Returns the number of strings in the multiline text
! Arguments:
!    mltext      Multiline text to be examined
! Prerequisite:
!    Multiline text is properly initialised
!
pure integer function mltxt_number( text )
    type(MULTILINE_TEXT), intent(in) :: text
    mltxt_number = size(text%text)
end function mltxt_number

! mltxt_cleanup --
!    Cleans up a multiline text
! Arguments:
!    text        Multiline text to be cleaned up
! Prerequisite:
!    Multiline text is properly initialised
!
subroutine mltxt_cleanup( text )
    type(MULTILINE_TEXT), intent(inout) :: text

    integer                             :: i

    text%max_length = 0
    if ( associated(text%text) ) then
        do i = 1,size(text%text)
            call txt_cleanup(text%text(i))
        enddo
        deallocate(text%text)
    endif
end subroutine mltxt_cleanup

! mltxt_insert --
!    Insert a new line into a multiline text
! Arguments:
!    text        Multiline text to be used
!    pos         Position after which to insert the line
!    line        New line to be added
!
subroutine mltxt_insert_string( text, pos, line )
    type(MULTILINE_TEXT), intent(inout) :: text
    integer, intent(in)                 :: pos
    character(len=*), intent(in)        :: line

    integer                             :: posn
    type(TEXT_STRING), pointer, dimension(:) :: old_text

    if ( pos .eq. MLTXT_END ) then
       posn = 0
       if ( associated(text%text) ) then
           posn = size(text%text)
       endif
    else
       posn = max( 0, pos )
    endif
    if ( associated(text%text) ) then
        posn = min( posn, size(text%text) )
        old_text  => text%text
        text%text => null()
        allocate( text%text(1:size(old_text)+1) )
    else
        posn = 0
        old_text  => null()
        allocate( text%text(1:1) )
    endif

    text%max_length = max( text%max_length, len(line) )

    if ( associated( old_text ) ) then
        text%text(1:posn) = old_text(1:posn)
        if ( posn .lt. size(old_text) ) then
            text%text(posn+2:) = old_text(posn+1:)
        endif
        deallocate( old_text )
    endif
    call txt_from_string( text%text(posn+1), line )
end subroutine mltxt_insert_string

subroutine mltxt_insert_text( text, pos, text_str )
    type(MULTILINE_TEXT), intent(inout) :: text
    integer, intent(in)                 :: pos
    type(TEXT_STRING)                   :: text_str

    character(len=txt_length(text_str)) :: buffer

    call txt_to_string( text_str, buffer )
    call mltxt_insert_string( text, pos, buffer )
end subroutine mltxt_insert_text

! mltxt_delete --
!    Delete a line from a multiline text
! Arguments:
!    text        Multiline text to be used
!    pos         Position of the line that is to be thrown away
!
subroutine mltxt_delete( text, pos )
    type(MULTILINE_TEXT), intent(inout) :: text
    integer, intent(in)                 :: pos

    integer                                  :: i
    type(TEXT_STRING), pointer, dimension(:) :: old_text

    if ( .not. associated(text%text) ) then
        return ! We ignore calls with uninitialised items
    endif

    if ( pos .lt. 1 .or. pos .gt. size(text%text) ) then
        return ! We ignore calls with out-of-bound positions
    endif

    old_text  => text%text
    text%text => null()
    allocate( text%text(1:size(old_text)-1) )

    text%text(1:pos-1) = old_text(1:pos-1)
    text%text(pos:)    = old_text(pos+1:)

    deallocate( old_text )

    text%max_length = 0
    do i = 1,size(text%text)
        text%max_length = max( text%max_length, txt_length(text%text(i)) )
    enddo
end subroutine mltxt_delete

! mltxt_get --
!    Returns a pointer to the nth line
! Arguments:
!    text        Multiline text to be used
!    pos         Position of the line to be returned
!    line        Pointer to the line
!
subroutine mltxt_get( text, pos, line )
    type(MULTILINE_TEXT), intent(in)        :: text
    integer, intent(in)                     :: pos
    type(TEXT_STRING), pointer              :: line

    if ( pos .ge. 1 .and. pos .le. size(text%text) ) then
        line => text%text(pos)
    else
        line => null()
    endif
end subroutine mltxt_get

end module multiple_line_text
