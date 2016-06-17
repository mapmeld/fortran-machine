! sample model for Fortran.io and SQLite database
! table should already be created

module marsupial
  use sqlite

  implicit none

  type(SQLITE_DATABASE)                       :: db
  type(SQLITE_STATEMENT)                      :: stmt
  type(SQLITE_COLUMN), dimension(:), pointer  :: column
  integer                                     :: i
  logical                                     :: finished

  contains

  subroutine insert(name, latinName, wikiLink, description)
    ! columns
    character(len=50)             :: name, latinName, wikiLink, description

    allocate( column(4) )
    call sqlite3_set_column( column(1), name )
    call sqlite3_set_column( column(2), latinName )
    call sqlite3_set_column( column(3), wikiLink )
    call sqlite3_set_column( column(4), description )
    call sqlite3_insert( db, 'marsupials', column )
  endsubroutine

  subroutine getOneMarsupial(query, name, latinName, wikiLink, description)
    ! columns
    character(len=*)		        :: query
    character(len=50)			:: name, latinName, wikiLink, description

    call sqlite3_open('marsupials.sqlite3', db)

    allocate( column(4) )
    call sqlite3_column_query( column(1), 'name', SQLITE_CHAR )
    call sqlite3_column_query( column(2), 'latinName', SQLITE_CHAR )
    call sqlite3_column_query( column(3), 'wikiLink', SQLITE_CHAR )
    call sqlite3_column_query( column(4), 'description', SQLITE_CHAR )

    call sqlite3_prepare_select( db, 'marsupials', column, stmt, "WHERE name = '" // trim(query) // "' LIMIT 4")

    i = 1
    do
      call sqlite3_next_row(stmt, column, finished)
      if (finished) exit

      call sqlite3_get_column(column(1), name)
      call sqlite3_get_column(column(2), latinName)
      call sqlite3_get_column(column(3), wikiLink)
      call sqlite3_get_column(column(4), description)
      i = i + 1
    end do
  endsubroutine
endmodule
