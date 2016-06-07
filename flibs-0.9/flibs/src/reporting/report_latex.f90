! report_latex.f90 --
!     Module for writing reports in LaTex format. The routines here
!     are used exclusively via the reporting module.
!
!     $Id: csv_file_1d.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module reporting_latex
    implicit none

    type report_data_latex
        integer            :: lun
    end type report_data_latex

contains

! report_open_latex --
!     Open the report file and write the header
!
! Arguments:
!     report          Data structure identifying the file
!     filename        Name of the file to open
!     title           Title for the document
!     papersize       What paper size to use (A4 or Letter)
!
subroutine report_open_latex( report, filename, title, papersize )
    type(report_data_latex) :: report
    character(len=*)      :: filename
    character(len=*)      :: title
    character(len=*)      :: papersize

    report%lun = 10   ! Just for now!

    open( report%lun, file = filename )

    if ( papersize == 'Letter' ) then
        write( report%lun, '(A)' ) &
            '\documentclass[letterpaper,11pt]{report}'
    elseif ( papersize == 'A4' ) then
        write( report%lun, '(A)' ) &
            '\documentclass[a4paper,11pt]{report}'
    else
        write( report%lun, '(A)' ) &
            '\documentclass[a4paper,11pt]{report}'
    endif

    write( report%lun, '(A)' ) &
        '\title{'// trim(title) // '}'
    write( report%lun, '(A)' ) &
        '\begin{document}',&
        '\maketitle'

end subroutine report_open_latex

! report_close_latex --
!     Properly close the report file
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_close_latex( report )
    type(report_data_latex) :: report

    write( report%lun, '(A)' ) &
        '\end{document}'

end subroutine report_close_latex

! report_text_latex --
!     Write plain text to the file
!
! Arguments:
!     report          Data structure identifying the file
!     text            Array of text strings
!
subroutine report_text_latex( report, text )
    type(report_data_latex)          :: report
    character(len=*), dimension(:) :: text

    integer                        :: i

    ! NOTE: No escaping special characters yet

    write( report%lun, '(A)' ) &
        ( trim(text(i)) ,i=1,size(text) )

end subroutine report_text_latex

! report_newpar_latex --
!     Start a new paragraph
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_newpar_latex( report )
    type(report_data_latex)          :: report

    write( report%lun, '(A)' ) &
        ' '

end subroutine report_newpar_latex

! report_chapter_latex --
!     Start a new chapter
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the chapter
!
subroutine report_chapter_latex( report, title )
    type(report_data_latex)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '\chapter{' // trim(title) // '}'

end subroutine report_chapter_latex

! report_section_latex --
!     Start a new section
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the section
!
subroutine report_section_latex( report, title )
    type(report_data_latex)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '\section{' // trim(title) // '}'

end subroutine report_section_latex

! report_subsection_latex --
!     Start a new subsection
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the subsection
!
subroutine report_subsection_latex( report, title )
    type(report_data_latex)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '\subsection{' // trim(title) // '}'

end subroutine report_subsection_latex

! report_emph_latex --
!     Write emphasized text
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text to be emphasized
!
subroutine report_emph_latex( report, text )
    type(report_data_latex)          :: report
    character(len=*)                 :: text

    write( report%lun, '(A)' ) &
        '\emph{' // trim(text) // '}'

end subroutine report_emph_latex

! report_verbatim_latex --
!     Turn text formatting on or off
!
! Arguments:
!     report          Data structure identifying the file
!     verbatim        Turn formatting off (verbatim=true) or on (false)
!
subroutine report_verbatim_latex( report, verbatim )
    type(report_data_latex)          :: report
    logical                          :: verbatim

    if ( verbatim ) then
        write( report%lun, '(A)' ) &
            '\begin{verbatim}'
    else
        write( report%lun, '(A)' ) &
            '\end{verbatim}'
    endif

end subroutine report_verbatim_latex

! report_table_begin_latex --
!     Start a new table by writing the headers
!
! Arguments:
!     report          Data structure identifying the file
!     header          Array of texts for the header (determines number
!                     of columns)
!
subroutine report_table_begin_latex( report, header )
    type(report_data_latex)          :: report
    character(len=*), dimension(:)   :: header

    integer                          :: i

    write( report%lun, '(A)' ) &
        '\begin{tabular}{' // repeat('|l', size(header)) // '|}',&
        '\hline'
    write( report%lun, '(A)' ) &
        ( '\emph{' // trim(header(i)) // '} &' ,i=1,size(header)-1 )
    i = size(header)
    write( report%lun, '(A)' ) &
        '\emph{' // trim(header(i)) // '} \\'
    write( report%lun, '(A)' ) &
        '\hline'

end subroutine report_table_begin_latex

! report_table_row_latex --
!     Write a new row for a table
!
! Arguments:
!     report          Data structure identifying the file
!     data            Array of texts for the new row
!
subroutine report_table_row_latex( report, data )
    type(report_data_latex)          :: report
    character(len=*), dimension(:)   :: data

    integer                          :: i

    write( report%lun, '(A)' ) &
        ( trim(data(i)) // ' &' ,i=1,size(data)-1 )
    i = size(data)
    write( report%lun, '(A)' ) &
        trim(data(i)) // '\\'

end subroutine report_table_row_latex

! report_table_end_latex --
!     Close the current table
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_table_end_latex( report )
    type(report_data_latex)          :: report

    write( report%lun, '(A)' ) &
        '\hline', '\end{tabular}'

end subroutine report_table_end_latex

! report_list_begin_latex --
!     Start a new list (or new list level)
!
! Arguments:
!     report          Data structure identifying the file
!
! Note:
!     Currently only bullet lists, that may change
!
subroutine report_list_begin_latex( report )
    type(report_data_latex)          :: report

    write( report%lun, '(A)' ) &
        '\begin{itemize}'

end subroutine report_list_begin_latex

! report_list_item_latex --
!     Write a new list item
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text for the list item
!
subroutine report_list_item_latex( report, text )
    type(report_data_latex)          :: report
    character(len=*), dimension(:)   :: text

    integer                          :: i

    write( report%lun, '(A)' ) &
        '\item{', ( trim(text(i)), i=1,size(text) ), '}'

end subroutine report_list_item_latex

! report_list_end_latex --
!     Close the current list
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_list_end_latex( report )
    type(report_data_latex)          :: report

    write( report%lun, '(A)' ) &
        '\end{itemize}'

end subroutine report_list_end_latex

end module reporting_latex

!
! Test program
!
!program test_report
!     use reporting_latex
!
!     type(report_data_latex) :: report
!
!     call report_open_latex( report, 'test_report.latex', 'This is a test!', 'A4' )
!
!     call report_text_latex(   report, (/ 'Hm, I should provide a scalar version too.'/) )
!     call report_text_latex(   report, (/ 'And a longer text to find out what parapgraphs really look like.'/) )
!     call report_newpar_latex( report )
!     call report_text_latex(   report, (/ 'Yes, definitely!'/) )
!
!     call report_subsection_latex(  report, 'How about a table?' )
!     call report_table_begin_latex( report, (/ 'Column 1', 'Column 2', 'Column 3' /) )
!     call report_table_row_latex(   report, (/ 'Value 1 ', 'Value 2 ', 'Value 3 ' /) )
!     call report_table_row_latex(   report, (/ 'A'       , 'B'       , 'C'        /) )
!     call report_table_end_latex(   report )
!
!     call report_list_begin_latex( report )
!     call report_list_item_latex(  report, (/ 'A first item'/) )
!     call report_list_item_latex(  report, (/ 'A second item ', &
!                                              'with more text' /) )
!     call report_list_end_latex(   report )
!
!     call report_close_latex(  report )
!
!end program test_report
