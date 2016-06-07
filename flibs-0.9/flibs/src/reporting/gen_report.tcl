# gen_report.tcl --
#     Small script to generate the repetitive code for
#     the reporting module
#
proc mkroutine {name arguments declarations} {
    global outfile

    if { $arguments != "" } {
        set arguments ", $arguments"
    }

    puts $outfile "
subroutine ${name}( report $arguments )

    type(report_data) :: report

    $declarations

    select case( report%type )
        case( report_rtf )
            call ${name}_rtf( report%rtf $arguments )
        case( report_html )
            call ${name}_html( report%html $arguments )
        case( report_latex )
            call ${name}_latex( report%latex $arguments )
    end select
end subroutine $name"
}

#
# Write the code
#
set outfile [open "report_generated.f90" w]

foreach {name arguments declarations} {

report_close {} {}

report_text text
    {character(len=*), dimension(:) :: text}

report_newpar {} {}

report_chapter title
    {character(len=*)                 :: title}

report_section title
    {character(len=*)                 :: title}

report_subsection title
    {character(len=*)                 :: title}

report_emph text
    {character(len=*)                 :: text}

report_verbatim verbatim
    {logical                          :: verbatim}

report_table_begin_auto header
    {character(len=*), dimension(:)   :: header}

report_table_begin_width {header, width}
    {character(len=*), dimension(:)   :: header
     integer, dimension(:)            :: width}

report_table_row data
    {character(len=*), dimension(:)   :: data}

report_table_end {} {}

report_list_begin {} {}

report_list_end {} {}

report_list_item text
    {character(len=*), dimension(:)   :: text}
} {
   mkroutine $name $arguments $declarations
}
