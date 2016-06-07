
subroutine report_close( report  )

    type(report_data) :: report

    

    select case( report%type )
        case( report_rtf )
            call report_close_rtf( report%rtf  )
        case( report_html )
            call report_close_html( report%html  )
        case( report_latex )
            call report_close_latex( report%latex  )
    end select
end subroutine report_close

subroutine report_text( report , text )

    type(report_data) :: report

    character(len=*), dimension(:) :: text

    select case( report%type )
        case( report_rtf )
            call report_text_rtf( report%rtf , text )
        case( report_html )
            call report_text_html( report%html , text )
        case( report_latex )
            call report_text_latex( report%latex , text )
    end select
end subroutine report_text

subroutine report_newpar( report  )

    type(report_data) :: report

    

    select case( report%type )
        case( report_rtf )
            call report_newpar_rtf( report%rtf  )
        case( report_html )
            call report_newpar_html( report%html  )
        case( report_latex )
            call report_newpar_latex( report%latex  )
    end select
end subroutine report_newpar

subroutine report_chapter( report , title )

    type(report_data) :: report

    character(len=*)                 :: title

    select case( report%type )
        case( report_rtf )
            call report_chapter_rtf( report%rtf , title )
        case( report_html )
            call report_chapter_html( report%html , title )
        case( report_latex )
            call report_chapter_latex( report%latex , title )
    end select
end subroutine report_chapter

subroutine report_section( report , title )

    type(report_data) :: report

    character(len=*)                 :: title

    select case( report%type )
        case( report_rtf )
            call report_section_rtf( report%rtf , title )
        case( report_html )
            call report_section_html( report%html , title )
        case( report_latex )
            call report_section_latex( report%latex , title )
    end select
end subroutine report_section

subroutine report_subsection( report , title )

    type(report_data) :: report

    character(len=*)                 :: title

    select case( report%type )
        case( report_rtf )
            call report_subsection_rtf( report%rtf , title )
        case( report_html )
            call report_subsection_html( report%html , title )
        case( report_latex )
            call report_subsection_latex( report%latex , title )
    end select
end subroutine report_subsection

subroutine report_emph( report , text )

    type(report_data) :: report

    character(len=*)                 :: text

    select case( report%type )
        case( report_rtf )
            call report_emph_rtf( report%rtf , text )
        case( report_html )
            call report_emph_html( report%html , text )
        case( report_latex )
            call report_emph_latex( report%latex , text )
    end select
end subroutine report_emph

subroutine report_verbatim( report , verbatim )

    type(report_data) :: report

    logical                          :: verbatim

    select case( report%type )
        case( report_rtf )
            call report_verbatim_rtf( report%rtf , verbatim )
        case( report_html )
            call report_verbatim_html( report%html , verbatim )
        case( report_latex )
            call report_verbatim_latex( report%latex , verbatim )
    end select
end subroutine report_verbatim

subroutine report_table_begin( report , header )

    type(report_data) :: report

    character(len=*), dimension(:)   :: header

    select case( report%type )
        case( report_rtf )
            call report_table_begin_rtf( report%rtf , header )
        case( report_html )
            call report_table_begin_html( report%html , header )
        case( report_latex )
            call report_table_begin_latex( report%latex , header )
    end select
end subroutine report_table_begin

subroutine report_table_row( report , data )

    type(report_data) :: report

    character(len=*), dimension(:)   :: data

    select case( report%type )
        case( report_rtf )
            call report_table_row_rtf( report%rtf , data )
        case( report_html )
            call report_table_row_html( report%html , data )
        case( report_latex )
            call report_table_row_latex( report%latex , data )
    end select
end subroutine report_table_row

subroutine report_table_end( report  )

    type(report_data) :: report

    

    select case( report%type )
        case( report_rtf )
            call report_table_end_rtf( report%rtf  )
        case( report_html )
            call report_table_end_html( report%html  )
        case( report_latex )
            call report_table_end_latex( report%latex  )
    end select
end subroutine report_table_end

subroutine report_list_begin( report  )

    type(report_data) :: report

    

    select case( report%type )
        case( report_rtf )
            call report_list_begin_rtf( report%rtf  )
        case( report_html )
            call report_list_begin_html( report%html  )
        case( report_latex )
            call report_list_begin_latex( report%latex  )
    end select
end subroutine report_list_begin

subroutine report_list_end( report  )

    type(report_data) :: report

    

    select case( report%type )
        case( report_rtf )
            call report_list_end_rtf( report%rtf  )
        case( report_html )
            call report_list_end_html( report%html  )
        case( report_latex )
            call report_list_end_latex( report%latex  )
    end select
end subroutine report_list_end

subroutine report_list_item( report , text )

    type(report_data) :: report

    character(len=*), dimension(:)   :: text

    select case( report%type )
        case( report_rtf )
            call report_list_item_rtf( report%rtf , text )
        case( report_html )
            call report_list_item_html( report%html , text )
        case( report_latex )
            call report_list_item_latex( report%latex , text )
    end select
end subroutine report_list_item
