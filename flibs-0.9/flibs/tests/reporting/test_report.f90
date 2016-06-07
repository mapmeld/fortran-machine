! test_report.f90 --
!     Small test program for the report module
!
!     $Id: test_report.f90,v 1.1 2008/03/25 05:23:36 arjenmarkus Exp $
!
program test_report
     use reporting

     type(report_data) :: report

     call report_open( report, 'test_report.html', 'This is a test!', &
              report_html, 'A4' )

     call report_text(   report, (/ 'Hm, I should provide a scalar version too.'/) )
     call report_text(   report, (/ 'And a longer text to find out what parapgraphs really look like.'/) )
     call report_newpar( report )
     call report_text(   report, (/ 'Yes, definitely!'/) )

     call report_subsection(  report, 'How about a table?' )
     call report_table_begin( report, (/ 'Column 1', 'Column 2', 'Column 3' /) )
     call report_table_row(   report, (/ 'Value 1 ', 'Value 2 ', 'Value 3 ' /) )
     call report_table_row(   report, (/ 'A'       , 'B'       , 'C'        /) )
     call report_table_end(   report )

     call report_list_begin( report )
     call report_list_item(  report, (/ 'A first item'/) )
     call report_list_item(  report, (/ 'A second item ', &
                                              'with more text' /) )
     call report_list_end(   report )

     call report_close(  report )

end program test_report
