! myfprog.f90 --
!    Small test program for the Fortran-wrapper to Sqlite
!
!    $Id: myfprog.f90,v 1.2 2006/03/26 19:03:53 arjenmarkus Exp $
!
program myfprog
   use sqlite

   character(len=20)                 :: fname
   type(SQLITE_DATABASE)             :: db
   type(SQLITE_STATEMENT)            :: stmt
   type(SQLITE_COLUMN), dimension(2) :: columns
   type(SQLITE_COLUMN), dimension(:), pointer :: column_names
   logical                           :: finished


   fname = 'arjen.db'
   call sqlite3_open( fname, db )
   if ( sqlite3_error(db) ) then
      write(*,*) 'Error: ', sqlite3_errmsg(db)
   else
      call sqlite3_do( db, "create table hm (i int)" )
   endif

 ! columns(1)%name = 'key'
 ! columns(1)%type = 'INT'
 ! columns(2)%name = 'string'
 ! columns(2)%type = 'CHAR(20)'
   call sqlite3_column_props( columns(1), 'key', SQLITE_INT )
   call sqlite3_column_props( columns(2), 'string', SQLITE_CHAR, 20 )
   call sqlite3_create_table( db, 'key_value', columns, 'key' )

   nullify( column_names )
   call sqlite3_query_table( db,'key_value', column_names )

   do i = 1,size(column_names)
      write(*,*) i, column_names(i)%name, ' ', column_names(i)%type
   enddo

   call sqlite3_set_column( column_names(1), 1 )
   call sqlite3_set_column( column_names(2), 'my string' )
   call sqlite3_insert( db, 'key_value', column_names )

   call sqlite3_set_column( column_names(1), 2 )
   call sqlite3_set_column( column_names(2), 'my second string' )
   call sqlite3_insert( db, 'key_value', column_names )

   call sqlite3_do( db, "commit" ) ;

   call sqlite3_prepare_select( db, 'key_value', column_names, stmt )
   finished = .false.
   do
      call sqlite3_next_row( stmt, column_names, finished )
      if ( finished ) exit
      write(*,*) column_names(1)%name, column_names(1)%int_value
      write(*,*) column_names(2)%name, trim(column_names(2)%char_value)
   enddo

   call sqlite3_close( db )
end program
