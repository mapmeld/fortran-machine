! report_html.f90 --
!     Module for writing reports in HTML format. The routines here
!     are used exclusively via the reporting module.
!
!     $Id: csv_file_1d.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module reporting_html
    implicit none

    type report_data_html
        integer            :: lun
    end type report_data_html

contains

! report_open_html --
!     Open the report file and write the header
!
! Arguments:
!     report          Data structure identifying the file
!     filename        Name of the file to open
!     title           Title for the document
!     papersize       What paper size to use (A4 or Letter)
!                     (ignored!)
!
subroutine report_open_html( report, filename, title, papersize )
    type(report_data_html) :: report
    character(len=*)      :: filename
    character(len=*)      :: title
    character(len=*)      :: papersize

    report%lun = 10   ! Just for now!

    open( report%lun, file = filename )

    write( report%lun, '(A)' ) &
        '<html><head>',&
        '<title>'// trim(title) // '</title></head>',&
        '<body>'

end subroutine report_open_html

! report_close_html --
!     Properly close the report file
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_close_html( report )
    type(report_data_html) :: report

    write( report%lun, '(A)' ) &
        '</body></html>'

end subroutine report_close_html

! report_text_html --
!     Write plain text to the file
!
! Arguments:
!     report          Data structure identifying the file
!     text            Array of text strings
!
subroutine report_text_html( report, text )
    type(report_data_html)          :: report
    character(len=*), dimension(:) :: text

    integer                        :: i

    ! NOTE: No escaping special characters yet

    write( report%lun, '(A)' ) &
        ( trim(text(i)) ,i=1,size(text) )

end subroutine report_text_html

! report_newpar_html --
!     Start a new paragraph
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_newpar_html( report )
    type(report_data_html)          :: report

    write( report%lun, '(A)' ) &
        '<p>'

end subroutine report_newpar_html

! report_chapter_html --
!     Start a new chapter
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the chapter
!
subroutine report_chapter_html( report, title )
    type(report_data_html)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '<h1>' // trim(title) // '</h1>'

end subroutine report_chapter_html

! report_section_html --
!     Start a new section
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the section
!
subroutine report_section_html( report, title )
    type(report_data_html)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '<h2>' // trim(title) // '</h2>'

end subroutine report_section_html

! report_subsection_html --
!     Start a new subsection
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the subsection
!
subroutine report_subsection_html( report, title )
    type(report_data_html)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '<h3>' // trim(title) // '</h3>'

end subroutine report_subsection_html

! report_emph_html --
!     Write emphasized text
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text to be emphasized
!
! Note:
!     MSIE seems to ignore the tag <emph> - so use <i> instead
!
subroutine report_emph_html( report, text )
    type(report_data_html)          :: report
    character(len=*)                 :: text

    write( report%lun, '(A)' ) &
        '<i>' // trim(text) // '</i>'

end subroutine report_emph_html

! report_verbatim_html --
!     Turn text formatting on or off
!
! Arguments:
!     report          Data structure identifying the file
!     verbatim        Turn formatting off (verbatim=true) or on (false)
!
subroutine report_verbatim_html( report, verbatim )
    type(report_data_html)          :: report
    logical                          :: verbatim

    if ( verbatim ) then
        write( report%lun, '(A)' ) &
            '<pre>'
    else
        write( report%lun, '(A)' ) &
            '</pre>'
    endif

end subroutine report_verbatim_html

! report_table_begin_html --
!     Start a new table by writing the headers
!
! Arguments:
!     report          Data structure identifying the file
!     header          Array of texts for the header (determines number
!                     of columns)
!
subroutine report_table_begin_html( report, header )
    type(report_data_html)          :: report
    character(len=*), dimension(:)   :: header

    integer                          :: i

    write( report%lun, '(A)' ) &
        '<table border><tr>'
    write( report%lun, '(A)' ) &
        ( '<td><i>' // trim(header(i)) // '</i></td>', i = 1,size(header) )
    write( report%lun, '(A)' ) &
        '</tr>'

end subroutine report_table_begin_html

! report_table_row_html --
!     Write a new row for a table
!
! Arguments:
!     report          Data structure identifying the file
!     data            Array of texts for the new row
!
subroutine report_table_row_html( report, data )
    type(report_data_html)          :: report
    character(len=*), dimension(:)   :: data

    integer                          :: i

    write( report%lun, '(A)' ) &
        '<tr>', ( '<td>' // trim(data(i)) // '</td>' ,i=1,size(data) )
    write( report%lun, '(A)' ) &
        '</tr>'

end subroutine report_table_row_html

! report_table_end_html --
!     Close the current table
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_table_end_html( report )
    type(report_data_html)          :: report

    write( report%lun, '(A)' ) &
        '</table>'

end subroutine report_table_end_html

! report_list_begin_html --
!     Start a new list (or new list level)
!
! Arguments:
!     report          Data structure identifying the file
!
! Note:
!     Currently only bullet lists, that may change
!
subroutine report_list_begin_html( report )
    type(report_data_html)          :: report

    write( report%lun, '(A)' ) &
        '<ul>'

end subroutine report_list_begin_html

! report_list_item_html --
!     Write a new list item
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text for the list item
!
subroutine report_list_item_html( report, text )
    type(report_data_html)          :: report
    character(len=*), dimension(:)   :: text

    integer                          :: i

    write( report%lun, '(A)' ) &
        '<li>', ( trim(text(i)), i=1,size(text) ), '</li>'

end subroutine report_list_item_html

! report_list_end_html --
!     Close the current list
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_list_end_html( report )
    type(report_data_html)          :: report

    write( report%lun, '(A)' ) &
        '</ul>'

end subroutine report_list_end_html

end module reporting_html

!
! Test program
!
!program test_report
!     use reporting_html
!
!     type(report_data_html) :: report
!
!     call report_open_html( report, 'test_report.html', 'This is a test!', 'A4' )
!
!     call report_text_html(   report, (/ 'Hm, I should provide a scalar version too.'/) )
!     call report_text_html(   report, (/ 'And a longer text to find out what parapgraphs really look like.'/) )
!     call report_newpar_html( report )
!     call report_text_html(   report, (/ 'Yes, definitely!'/) )
!
!     call report_subsection_html(  report, 'How about a table?' )
!     call report_table_begin_html( report, (/ 'Column 1', 'Column 2', 'Column 3' /) )
!     call report_table_row_html(   report, (/ 'Value 1 ', 'Value 2 ', 'Value 3 ' /) )
!     call report_table_row_html(   report, (/ 'A'       , 'B'       , 'C'        /) )
!     call report_table_end_html(   report )
!
!     call report_list_begin_html( report )
!     call report_list_item_html(  report, (/ 'A first item'/) )
!     call report_list_item_html(  report, (/ 'A second item ', &
!                                              'with more text' /) )
!     call report_list_end_html(   report )
!
!     call report_close_html(  report )
!
!end program test_report
