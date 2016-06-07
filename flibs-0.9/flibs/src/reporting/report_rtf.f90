! report_rtf.f90 --
!     Module for writing reports in RTF format. The routines here
!     are used exclusively via the reporting module.
!
!
!     Note:
!     The preamble was published on the comp.lang.fortran newsgroup
!     by Terence Wright (may 2007). He kindly gave his permission
!     to use it here.
!
!     Original text, including comments:
! There have been sporaduic mentions of writing RTF files and HTML as
! methods of excellent control over report writing. I noticed a comment
! on RTF as needing far too much header preparation work to be really
! useful,
!
! The following is what I find is about the useful minimum header
! information shown ad literals for writing strings.
!
!       DATA CCA/'{\rtf1\ansi \deff1\deflang1033{\fonttbl'/
! C I use Courier, but put font style here as next line
!       DATA CCB/'{\f1\fmodern\fcharset0\fprq1 Courier
! New;}}'/
!       DATA CCD/'{\stylesheet{\f1\fs20 \snext0 Normal;}'/
!       DATA CCE/'{\*\cs10 \additive Default Paragraph Font;}}'/
!       DATA CCF/'{\info{\title (actual title) }}'/
! C chooses either LETTER size
!       DATA CLP/'\paperw12242\paperh15842\paperw15842\paperh12242'/
! C or A4 size
! paper
!       DATA C4P/'\paperw11907\paperh16840\paperw16840\paperh11907'/
! C then
!       DATA
! CCM/'\margl1134\margr0851\margt0851\margb0851\lndscpsxn'/
!       DATA CCN/'\deftab708\widowctrl
! \hyphcaps0\fet0\sectd'/
!       DATA CCQ/'\endnhere\pard\plain\sl-148 \slmult1'/
!       DATA
! CCO/'\linex0\headery709\footery709\colsx709'/
! C SELECT DEFAULT SIZE AS fsNN
!       DATA CCR/'{\f1\fs16\lang1033 '/
!
! C REPORT GOES HERE
!
!       DATA CCX/'}{\par }}'/
!
!     $Id: csv_file_1d.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
module reporting_rtf
    implicit none

    type report_data_rtf
        integer            :: lun
    end type report_data_rtf

contains

! report_open_rtf --
!     Open the report file and write the header
!
! Arguments:
!     report          Data structure identifying the file
!     filename        Name of the file to open
!     title           Title for the document
!     papersize       What paper size to use (A4 or Letter)
!
subroutine report_open_rtf( report, filename, title, papersize )
    type(report_data_rtf) :: report
    character(len=*)      :: filename
    character(len=*)      :: title
    character(len=*)      :: papersize

    report%lun = 10   ! Just for now!

    open( report%lun, file = filename )

    write( report%lun, '(A)' ) &
        '{\rtf1\ansi \deff1\deflang1033{\fonttbl',&
        '{\f1\froman\fcharset0\fprq1 Times New Roman;}',&
        '{\f2\fmodern\fcharset0\fprq1 Courier New;}}',&
        '{\stylesheet{\f1\fs22 \snext0 Normal;}',&
        '{\*\cs10 \additive Default Paragraph Font;}',&
        '{\*\cs21 \f1\fs32 Chapter Font;}',&
        '{\*\cs22 \f1\fs28 \additive Section Font;}',&
        '{\*\cs23 \f1\fs24 \additive Subsection Font;}',&
        '{\*\cs30 \f2\fs22 \additive Verbatim Font;}}',&
        '{\info{\title ' // trim(title) // '}}'
    if ( papersize == 'Letter' ) then
        write( report%lun, '(A)' ) &
            '\paperw12242\paperh15842\paperw15842\paperh12242'
    elseif ( papersize == 'A4' ) then
        write( report%lun, '(A)' ) &
            '\paperw11907\paperh16840\paperw16840\paperh11907'
    else
        write( report%lun, '(A)' ) &
            '\paperw11907\paperh16840\paperw16840\paperh11907'
    endif

    write( report%lun, '(A)' ) &
        '\margl1134\margr0851\margt0851\margb0851\lndscpsxn',&
        '\deftab708\widowctrl \hyphcaps0\fet0\sectd',&
        '\endnhere\pard\plain',&
        '\linex0\headery709\footery709\colsx709'

    write( report%lun, '(A)' ) '{\pard'

end subroutine report_open_rtf

! report_close_rtf --
!     Properly close the report file
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_close_rtf( report )
    type(report_data_rtf) :: report

    write( report%lun, '(A)' ) &
        '}{\par }}'

end subroutine report_close_rtf

! report_text_rtf --
!     Write plain text to the file
!
! Arguments:
!     report          Data structure identifying the file
!     text            Array of text strings
!
subroutine report_text_rtf( report, text )
    type(report_data_rtf)          :: report
    character(len=*), dimension(:) :: text

    integer                        :: i

    ! NOTE: No escaping special characters yet

    write( report%lun, '(A)' ) &
        ( trim(text(i)) ,i=1,size(text) )

end subroutine report_text_rtf

! report_newpar_rtf --
!     Start a new paragraph
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_newpar_rtf( report )
    type(report_data_rtf)          :: report

    write( report%lun, '(A)' ) &
        '\par\par }', '{\pard'

end subroutine report_newpar_rtf

! report_chapter_rtf --
!     Start a new chapter
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the chapter
!
subroutine report_chapter_rtf( report, title )
    type(report_data_rtf)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '\page {\pard\f1\fs40\b ' // trim(title) // '\par\par}'

end subroutine report_chapter_rtf

! report_section_rtf --
!     Start a new section
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the section
!
subroutine report_section_rtf( report, title )
    type(report_data_rtf)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '{\pard\par\par\f1\fs34\b ' // trim(title) // '\par}'

end subroutine report_section_rtf

! report_subsection_rtf --
!     Start a new subsection
!
! Arguments:
!     report          Data structure identifying the file
!     title           Title of the subsection
!
subroutine report_subsection_rtf( report, title )
    type(report_data_rtf)          :: report
    character(len=*)                 :: title

    write( report%lun, '(A)' ) &
        '{\pard\par\par\f1\fs28\b ' // trim(title) // '\par}'

end subroutine report_subsection_rtf

! report_emph_rtf --
!     Write emphasized text
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text to be emphasized
!
! Note:
!     MSIE seems to ignore the tag <emph> - so use <i> instead
!
subroutine report_emph_rtf( report, text )
    type(report_data_rtf)          :: report
    character(len=*)                 :: text

    write( report%lun, '(A)' ) &
        '\i' // trim(text) // '\i0'

end subroutine report_emph_rtf

! report_verbatim_rtf --
!     Turn text formatting on or off
!
! Arguments:
!     report          Data structure identifying the file
!     verbatim        Turn formatting off (verbatim=true) or on (false)
!
subroutine report_verbatim_rtf( report, verbatim )
    type(report_data_rtf)          :: report
    logical                          :: verbatim

    if ( verbatim ) then
        write( report%lun, '(A)' ) &
            '{\cs30'
    else
        write( report%lun, '(A)' ) &
            '}'
    endif

end subroutine report_verbatim_rtf

! report_table_begin_rtf --
!     Start a new table by writing the headers
!
! Arguments:
!     report          Data structure identifying the file
!     header          Array of texts for the header (determines number
!                     of columns)
!
subroutine report_table_begin_rtf( report, header )
    type(report_data_rtf)            :: report
    character(len=*), dimension(:)   :: header

    integer                          :: i
    integer                          :: offset

    write( report%lun, '(A)' ) &
        '{\pard \par \trowd \trql '

    offset = 0
    do i = 1,size(header)
        offset = offset + len_trim(header(i)) * 11 * 20
        write( report%lun, '(A,I0)' ) '\cellx', offset
    enddo

    write( report%lun, '(A)' ) &
        '\i', ( '\intbl  ' // header(i) // '\cell', i = 1,size(header) ), '\i0\row'

end subroutine report_table_begin_rtf

! report_table_row_rtf --
!     Write a new row for a table
!
! Arguments:
!     report          Data structure identifying the file
!     data            Array of texts for the new row
!
subroutine report_table_row_rtf( report, data )
    type(report_data_rtf)          :: report
    character(len=*), dimension(:)   :: data

    integer                          :: i

    write( report%lun, '(A)' ) &
        ( '\intbl  ' // data(i) // '\cell', i = 1,size(data) ), '\row'

end subroutine report_table_row_rtf

! report_table_end_rtf --
!     Close the current table
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_table_end_rtf( report )
    type(report_data_rtf)          :: report

    write( report%lun, '(A)' ) &
        '\par}'

end subroutine report_table_end_rtf

! report_list_begin_rtf --
!     Start a new list (or new list level)
!
! Arguments:
!     report          Data structure identifying the file
!
! Note:
!     Currently only bullet lists, that may change
!
subroutine report_list_begin_rtf( report )
    type(report_data_rtf)          :: report

    write( report%lun, '(A)' ) &
        '{\pard\par\li220'

end subroutine report_list_begin_rtf

! report_list_item_rtf --
!     Write a new list item
!
! Arguments:
!     report          Data structure identifying the file
!     text            Text for the list item
!
subroutine report_list_item_rtf( report, text )
    type(report_data_rtf)          :: report
    character(len=*), dimension(:)   :: text

    integer                          :: i

    write( report%lun, '(A)' ) &
        '\par *\~', ( trim(text(i)) // ' ', i=1,size(text) )

end subroutine report_list_item_rtf

! report_list_end_rtf --
!     Close the current list
!
! Arguments:
!     report          Data structure identifying the file
!
subroutine report_list_end_rtf( report )
    type(report_data_rtf)          :: report

    write( report%lun, '(A)' ) &
        '\par}'

end subroutine report_list_end_rtf

end module reporting_rtf

!
! Test program
!
!program test_report
!     use reporting_rtf
!
!     type(report_data_rtf) :: report
!
!     call report_open_rtf( report, 'test_report.rtf', 'This is a test!', 'A4' )
!
!     call report_text_rtf(   report, (/ 'Hm, I should provide a scalar version too.'/) )
!     call report_text_rtf(   report, (/ 'And a longer text to find out what parapgraphs really look like.'/) )
!     call report_newpar_rtf( report )
!     call report_text_rtf(   report, (/ 'Yes, definitely!'/) )
!
!     call report_chapter_rtf(  report, 'How about a table?' )
!     call report_section_rtf(  report, 'How about a table?' )
!     call report_subsection_rtf(  report, 'How about a table?' )
!     call report_table_begin_rtf( report, (/ 'Column 1', 'Column 2', 'Column 3' /) )
!     call report_table_row_rtf(   report, (/ 'Value 1 ', 'Value 2 ', 'Value 3 ' /) )
!     call report_table_row_rtf(   report, (/ 'A'       , 'B'       , 'C'        /) )
!     call report_table_end_rtf(   report )
!
!     call report_list_begin_rtf( report )
!     call report_list_item_rtf(  report, (/ 'A first item'/) )
!     call report_list_item_rtf(  report, (/ 'A second item ', &
!                                              'with more text' /) )
!     call report_list_end_rtf(   report )
!
!     call report_close_rtf(  report )
!
!end program test_report
