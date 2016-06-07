! report.f90 --
!     Main source file for the reporting module:
!     write files with various formats (HTML, RTF and LaTex).
!
!     $Id: report.f90,v 1.1 2008/03/25 05:23:36 arjenmarkus Exp $
!
module reporting
    use reporting_rtf
    use reporting_html
    use reporting_latex

    integer, parameter :: report_rtf   = 1
    integer, parameter :: report_html  = 2
    integer, parameter :: report_latex = 3

    type report_data
        integer                 :: type
        type(report_data_rtf)   :: rtf
        type(report_data_html)  :: html
        type(report_data_latex) :: latex
    end type report_data

contains

! report_open --
!     Open the report file
!
! Arguments:
!     report          Data structure identifying the file
!     filename        Name of the file to open
!     title           Title for the document
!     format          Type of document (RTF, HTML or LaTex;
!                     use one of the parameters)
!     papersize       What paper size to use (A4 or Letter)
!
! Note:
!     Capitalisation of the document format is important
!     The papersize is ignored for HTML files
!
subroutine report_open( report, filename, title, format, papersize )

    type(report_data) :: report
    character(len=*)  :: filename
    character(len=*)  :: title
    integer           :: format
    character(len=*)  :: papersize

    select case( format )
        case( report_rtf )
            report%type = format
            call report_open_rtf( report%rtf, filename, title, papersize )
        case( report_html )
            report%type = format
            call report_open_html( report%html, filename, title, papersize )
        case( report_latex )
            report%type = format
            call report_open_latex( report%latex, filename, title, papersize )
        case default
            report%type = 0
            write(*,*) 'Unknown document format: ', format
    end select
end subroutine report_open

include 'report_generated.f90'

end module reporting
