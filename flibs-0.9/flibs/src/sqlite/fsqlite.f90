! fsqlite.f90 --
!    Module for interacting with SQLite3
!
!    Arjen Markus
!
!    General information:
!    This module and the associated C code is
!    inspired by work by Al Danial (http://danial.org).
!    The purpose is to provide a high-level means
!    for Fortran programmers to use the SQLite
!    library by Richard Hipp (http://www.sqlite.org)
!
!    Detailed documentation can be found in
!    the file fsqlite.html.
!
!    TODO:
!    - Arbitrary length character strings in set_column
!      and get_column
!    - Think about finalize and reset: error code?
!    - Support BLOBs
!    - Support DATE
!    - Support NULL
!    - More elegant selection of functions of columns
!
!    Implementation notes:
!    The handles to the database or prepared statements
!    are stored in an array of two integers to take
!    care of 64-bits platforms.
!    With the appropriate compilation options (if needed)
!    the code should be thread-safe, as no data are shared.
!
!    $Id: fsqlite.f90,v 1.4 2008/11/20 05:35:11 arjenmarkus Exp $
!
module sqlite_types

   integer, parameter         :: dp = kind(1.0d00)

   integer, parameter         :: SQLITE_INT    = 1
   integer, parameter         :: SQLITE_REAL   = 2
   integer, parameter         :: SQLITE_DOUBLE = 3
   integer, parameter         :: SQLITE_CHAR   = 4

   integer, parameter         :: SQLITE_OK     = 0
   integer, parameter         :: SQLITE_ERROR  = 1
   integer, parameter         :: SQLITE_MISUSE = 21
   integer, parameter         :: SQLITE_ROW    = 100
   integer, parameter         :: SQLITE_DONE   = 101


   type SQLITE_STATEMENT
      integer, dimension(2)   :: stmt_handle
   end type SQLITE_STATEMENT

   type SQLITE_DATABASE
      integer, dimension(2)   :: db_handle
      integer                 :: error
      character(len=80)       :: errmsg
   end type SQLITE_DATABASE

   type SQLITE_COLUMN
      character(len=40)       :: name     = ' '
      character(len=40)       :: type     = ' '
      character(len=40)       :: function = ' '
      integer                 :: type_set
      integer                 :: int_value
      real(kind=dp)           :: double_value
      character(len=80)       :: char_value
   end type SQLITE_COLUMN
end module sqlite_types

module sqlite
   use sqlite_types

   implicit none

   private :: stringtof
   private :: stringtoc
   private :: typename
   private :: column_func

   !
   ! Convenient interfaces
   !
   interface sqlite3_set_column
      module procedure sqlite3_set_column_int
      module procedure sqlite3_set_column_real
      module procedure sqlite3_set_column_double
      module procedure sqlite3_set_column_char
   end interface
   interface sqlite3_get_column
      module procedure sqlite3_get_column_int
      module procedure sqlite3_get_column_real
      module procedure sqlite3_get_column_double
      module procedure sqlite3_get_column_char
   end interface


contains


! typename --
!    Construct the type and attributes of a column
!    in a new table
! Arguments:
!    column        Column information
!    primary       Name of the primary key
!
character(len=40) function typename( column, primary )
   type(SQLITE_COLUMN), intent(in) :: column
   character(len=*), intent(in)    :: primary

   if ( column%name .ne. primary ) then
      typename = column%type
   else
      !write( typename, '(2a)' ) trim(column%type), ' primary key'
      typename = trim(column%type) // ' primary key'
   endif

end function typename


! column_func --
!    Construct the name and function of a column
!    in a new table
! Arguments:
!    column        Column information
!
character(len=80) function column_func( column )
   type(SQLITE_COLUMN), intent(in) :: column

   if ( column%function .ne. ' ' ) then
      column_func = trim(column%function) // '(' // trim(column%name) // ')'
   else
      column_func = column%name
   endif

end function column_func


! stringtof --
!    Convert a C string to Fortran
! Arguments:
!    string        String to be converted
!
subroutine stringtof( string )
   character(len=*) :: string

   integer          :: last
   last = index( string, char(0) )
   if ( last .gt. 0 ) then
      string(last:) = ' '
   endif

end subroutine stringtof


! stringtoc --
!    Convert a Fortran string to C
! Arguments:
!    string        String to be converted
! Note:
!    It is assumed that the last character
!    is a space. As this is a private
!    routine, this should have been taken
!    care of in the caller.
!
subroutine stringtoc( string )
   character(len=*) :: string

   integer          :: last

   last = 1 + len_trim(string)
   string(last:last) = char(0)

end subroutine stringtoc


! sqlite3_column_props --
!    Convenience routine to set the properties of a column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string
! Side effects:
!    Fields in column filled
!
subroutine sqlite3_column_props( column, name, type, length )
   type(SQLITE_COLUMN), intent(inout) :: column
   character(len=*), intent(in)       :: name
   integer, intent(in)                :: type
   integer, intent(in), optional      :: length

   integer                            :: length_
   character(len=40)                  :: type_expr

   length_ = 20
   if ( present(length) ) then
      length_ = length
   endif

   column%name     = name
   column%type_set = type

   select case ( type )
   case (SQLITE_INT)
      column%type = 'INT'
   case (SQLITE_REAL)
      column%type = 'FLOAT'
   case (SQLITE_DOUBLE)
      column%type = 'DOUBLE'
   case (SQLITE_CHAR)
      write( column%type, '(a,i0,a)' ) 'CHAR(', length_, ')'
   case default
      column%type = 'UNKNOWN!'
   end select

end subroutine sqlite3_column_props


! sqlite3_column_query --
!    Convenience routine to query a column or a function of that column
! Arguments:
!    column        Column structure
!    name          Name of the column
!    type          Type of the column
!    length        Length if a string (optional)
!    function      Name of the function to apply (if any)
! Side effects:
!    Fields in column filled
!
subroutine sqlite3_column_query( column, name, type, length, function )
   type(SQLITE_COLUMN), intent(inout)     :: column
   character(len=*), intent(in)           :: name
   integer, intent(in)                    :: type
   integer, intent(in), optional          :: length
   character(len=*), intent(in), optional :: function

   column%function = ' '
   if ( present(function) ) then
      column%function = function
   endif
   if ( present(length) ) then
      call sqlite3_column_props( column, name, type, length )
   else
      call sqlite3_column_props( column, name, type )
   endif

end subroutine sqlite3_column_query


! sqlite3_set_column_int    --
! sqlite3_set_column_real   --
! sqlite3_set_column_double --
! sqlite3_set_column_char   --
!    Convenience routines to set the value of a column
! Arguments:
!    column        Column structure
!    value         The value to be set
! Side effects:
!    Appropriate value field in column set
!
subroutine sqlite3_set_column_int( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   integer, intent(in)                :: value

   column%int_value = value
   column%type_set  = SQLITE_INT
end subroutine sqlite3_set_column_int

subroutine sqlite3_set_column_real( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   real, intent(in)                   :: value

   column%double_value = value
   column%type_set  = SQLITE_DOUBLE
end subroutine sqlite3_set_column_real

subroutine sqlite3_set_column_double( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   real(kind=dp), intent(in)               :: value

   column%double_value = value
   column%type_set  = SQLITE_DOUBLE
end subroutine sqlite3_set_column_double

subroutine sqlite3_set_column_char( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   character(len=*), intent(in)       :: value

   column%char_value = value
   column%type_set  = SQLITE_CHAR
end subroutine sqlite3_set_column_char


! sqlite3_get_column_int    --
! sqlite3_get_column_real   --
! sqlite3_get_column_double --
! sqlite3_get_column_char   --
!    Convenience routines to get the value of a column
! Arguments:
!    column        Column structure
!    value         Value on return
! Side effects:
!    Value argument will be set
! Note:
!    No attempt is made to convert the value
!    to the requested value. You will have to
!    check this yourself
!
subroutine sqlite3_get_column_int( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   integer, intent(out)               :: value

   value = column%int_value
end subroutine sqlite3_get_column_int

subroutine sqlite3_get_column_real( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   real, intent(out)                  :: value

   value = column%double_value
end subroutine sqlite3_get_column_real

subroutine sqlite3_get_column_double( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   real(kind=dp), intent(out)              :: value

   value = column%double_value
end subroutine sqlite3_get_column_double

subroutine sqlite3_get_column_char( column, value )
   type(SQLITE_COLUMN), intent(inout) :: column
   character(len=*), intent(out)      :: value

   value = column%char_value
end subroutine sqlite3_get_column_char


! sqlite3_error --
!    Return the last error code
! Arguments:
!    db            Structure for the database
! Returns:
!    Last SQLite error code for this database
!
logical function sqlite3_error( db )
   type(SQLITE_DATABASE) :: db

   sqlite3_error = db%error .ne. 0
end function sqlite3_error


! sqlite3_errmsg --
!    Return the last error message
! Arguments:
!    db            Structure for the database
! Returns:
!    Last SQLite error message for this database
!
character(len=80) function sqlite3_errmsg( db )
   type(SQLITE_DATABASE) :: db

   sqlite3_errmsg = db%errmsg
end function sqlite3_errmsg


! sqlite3_open --
!    Open a database file
! Arguments:
!    fname         Name of the file
!    db            Structure for the database
! Side effects:
!    The database file is opened and can be
!    used via the db argument
!
subroutine sqlite3_open( fname, db )
   character(len=*)      :: fname
   type(SQLITE_DATABASE) :: db

   character(len=len(fname)+1) :: fnamec

   interface
      integer function sqlite3_open_c( fnamec, handle )
         character(len=*)      :: fnamec
         integer, dimension(*) :: handle
      end function sqlite3_open_c
   end interface

   db%db_handle   = 0
   db%error       = 0
   db%errmsg       = ' '

   fnamec = fname
   call stringtoc( fnamec )

   db%error = sqlite3_open_c( fnamec, db%db_handle )
end subroutine sqlite3_open


! sqlite3_close --
!    Close a database file
! Arguments:
!    db            Structure for the database
! Side effects:
!    The database file is closed and can no
!    longer be accessed
!
subroutine sqlite3_close( db )
   type(SQLITE_DATABASE) :: db

   interface
      integer function sqlite3_close_c( handle )
         integer, dimension(*) :: handle
      end function sqlite3_close_c
   end interface

   db%error = sqlite3_close_c( db%db_handle )
   db%db_handle   = 0

end subroutine sqlite3_close


! sqlite3_do --
!    Run a single SQL command
! Arguments:
!    db            Structure for the database
!    command       Complete SQL command
! Side effects:
!    Whatever effects the command has. Note
!    that no output is reported back to the
!    caller (except for error codes and
!    messages if any)
!    longer be accessed
!
subroutine sqlite3_do( db, command )
   type(SQLITE_DATABASE) :: db
   character(len=*)      :: command

   interface
      integer function sqlite3_do_c( handle, command, errmsg )
         integer, dimension(*) :: handle
         character(len=*)      :: command
         character(len=*)      :: errmsg
      end function sqlite3_do_c
   end interface

   character(len=len(command)+1) :: commandc
   integer                       :: k

   commandc = command
   call stringtoc( commandc )

   db%errmsg = ' '
   db%error  = sqlite3_do_c( db%db_handle, commandc, db%errmsg )

end subroutine sqlite3_do


! sqlite3_begin --
!    Start a transaction on the given database
! Arguments:
!    db            Structure for the database
! Note:
!    Should be accompanied by a call to either
!    sqlite3_commit or sqlite3_rollback
!
subroutine sqlite3_begin( db )
   type(SQLITE_DATABASE) :: db

   call sqlite3_do( db, "BEGIN TRANSACTION" )

end subroutine sqlite3_begin


! sqlite3_commit --
!    Commits a transaction on the given database
! Arguments:
!    db            Structure for the database
! Note:
!    Accompanies sqlite3_begin
!
subroutine sqlite3_commit( db )
   type(SQLITE_DATABASE) :: db

   call sqlite3_do( db, "COMMIT TRANSACTION" )

end subroutine sqlite3_commit


! sqlite3_rollback --
!    Rolls back any changes to the database since the last commit
! Arguments:
!    db            Structure for the database
! Note:
!    Accompanies sqlite3_begin
!
subroutine sqlite3_rollback( db )
   type(SQLITE_DATABASE) :: db

   call sqlite3_do( db, "ROLLBACK" )

end subroutine sqlite3_rollback


! sqlite3_delete_table --
!    Delete a table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table to be deleted
! Note:
!    The table can not be recovered, unless this
!    is part of a transaction
!
subroutine sqlite3_delete_table( db, tablename )
   type(SQLITE_DATABASE) :: db
   character(len=*)      :: tablename

   character(len=20+len(tablename)) :: command

   write( command, "(2A)" ) "DELETE TABLE ", tablename
   call sqlite3_do( db, command )

end subroutine sqlite3_delete_table


! sqlite3_create_table --
!    Create a new table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Properties of the columns
!    primary       Name of the primary key (if any)
! Side effects:
!    The new table is created
!
subroutine sqlite3_create_table( db, tablename, columns, primary )
   type(SQLITE_DATABASE)              :: db
   character(len=*)                   :: tablename
   type(SQLITE_COLUMN), dimension(:)  :: columns
   character(len=*), optional         :: primary

   character(len=20+80*size(columns)) :: command
   character(len=40)                  :: primary_
   integer                            :: i
   integer                            :: ncols

   primary_ = ' '
   if ( present(primary) ) then
      primary_ = primary
   endif

   ncols = size(columns)
   write( command, '(100a)' ) 'create table ', tablename, ' (', &
      ( trim(columns(i)%name), ' ', trim(typename(columns(i), primary_)), ', ', &
           i = 1,ncols-1 ), &
      trim(columns(ncols)%name), ' ', trim(typename(columns(ncols),primary_)), ')'

   call sqlite3_do( db, command )
end subroutine sqlite3_create_table


! sqlite3_prepare_select --
!    Prepare a selection of data from the database
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns to be returned
!    stmt          Prepared statement (returned)
!    extra_clause  Extra clause for SELECT statement (appended)
! Side effects:
!    A new selection is prepared
!
subroutine sqlite3_prepare_select( db, tablename, columns, stmt, extra_clause )
   type(SQLITE_DATABASE)                       :: db
   character(len=*)                            :: tablename
   type(SQLITE_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!
   character(len=*), optional                  :: extra_clause
   type(SQLITE_STATEMENT), intent(out)         :: stmt

   character(len=20+80*size(columns))          :: command
   integer                                     :: nocols
   integer                                     :: i

   !
   ! Prepare the select statement for this table
   !
   ! TODO: expand the syntax!!
   !
   nocols = size(columns)
   write( command, '(100a)' ) 'select ', &
      (trim(column_func(columns(i))), ',', i = 1,nocols-1), &
       trim(column_func(columns(nocols))), &
      ' from ', trim(tablename)

   !
   ! Hm, appending a string of arbitrary length is tricky ...
   !
   if ( present(extra_clause) ) then
      command = trim(command) // ' ' // extra_clause
   endif

   call stringtoc( command )
   call sqlite3_prepare( db, command, stmt, columns )

end subroutine sqlite3_prepare_select

! sqlite3_insert --
!    Insert a row into the given table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns whose value is to be inserted
! Side effects:
!    A new row is written to the database
!
subroutine sqlite3_insert( db, tablename, columns )
   type(SQLITE_DATABASE)                       :: db
   character(len=*)                            :: tablename
   type(SQLITE_COLUMN), dimension(:), target   :: columns
   character(len=20+80*size(columns))          :: command

   type(SQLITE_COLUMN), dimension(:), pointer  :: prepared_columns
   type(SQLITE_STATEMENT)                      :: stmt
   integer                                     :: i
   integer                                     :: rc

   interface
      subroutine sqlite3_errmsg_c( handle, errmsg )
         integer, dimension(*) :: handle
         character(len=*)      :: errmsg
      end subroutine sqlite3_errmsg_c
   end interface

   interface
      integer function sqlite3_bind_int_c( handle, colidx, value )
         integer, dimension(*) :: handle
         integer               :: colidx
         integer               :: value
      end function sqlite3_bind_int_c
   end interface

   interface
      integer function sqlite3_bind_double_c( handle, colidx, value )
         use sqlite_types
         integer, dimension(*) :: handle
         integer               :: colidx
         real(kind=dp)         :: value
      end function sqlite3_bind_double_c
   end interface

   interface
      integer function sqlite3_bind_text_c( handle, colidx, value )
         integer, dimension(*) :: handle
         integer               :: colidx
         character(len=*)      :: value
      end function sqlite3_bind_text_c
   end interface

   !
   ! Prepare the insert statement for this table
   !
   write( command, '(100a)' ) 'insert into ', trim(tablename), ' values(', &
      ('?,', i = 1,size(columns)-1), '?)'

   call stringtoc( command )
   prepared_columns => columns
   call sqlite3_prepare( db, command, stmt, prepared_columns )

   !
   ! Bind the values
   !
   do i = 1,size(columns)
      select case (columns(i)%type_set)
      case (SQLITE_INT)
         rc = sqlite3_bind_int_c( stmt%stmt_handle, i, columns(i)%int_value )
      case (SQLITE_DOUBLE)
         rc = sqlite3_bind_double_c( stmt%stmt_handle, i, columns(i)%double_value )
      case (SQLITE_CHAR)
         rc = sqlite3_bind_text_c( stmt%stmt_handle, i, trim(columns(i)%char_value) )
      end select
      if ( rc .ne. 0 ) then
         db%error = rc
         call sqlite3_errmsg_c( db%db_handle, db%errmsg )
         call stringtof( db%errmsg )
      endif
   enddo

   !
   ! Actually perform the insert command
   !
   call sqlite3_step( stmt, rc )
   call sqlite3_finalize( stmt )

end subroutine sqlite3_insert


! sqlite3_next_row --
!    Gets the next row of data from a selection
! Arguments:
!    stmt          Prepared statement
!    columns       Columns to be returned
!    finished      Indicates there are no more data
!
subroutine sqlite3_next_row( stmt, columns, finished )
   type(SQLITE_STATEMENT)            :: stmt
   type(SQLITE_COLUMN), dimension(:) :: columns
   logical                           :: finished

   interface
      integer function sqlite3_column_int_c( handle, colidx, value )
         integer, dimension(*) :: handle
         integer               :: colidx
         integer               :: value
      end function sqlite3_column_int_c
   end interface

   interface
      integer function sqlite3_column_double_c( handle, colidx, value )
         use sqlite_types
         integer, dimension(*) :: handle
         integer               :: colidx
         real(kind=dp)         :: value
      end function sqlite3_column_double_c
   end interface

   interface
      integer function sqlite3_column_text_c( handle, colidx, value )
         integer, dimension(*) :: handle
         integer               :: colidx
         character(len=*)      :: value
      end function sqlite3_column_text_c
   end interface

   integer                           :: rc
   integer                           :: i

   call sqlite3_step( stmt, rc )

   if ( rc .eq. SQLITE_ROW ) then
      finished = .false.

      !
      ! Get the values
      !
      ! TODO: check validity of "type_set"
      !
      do i = 1,size(columns)
         select case (columns(i)%type_set)
         case (SQLITE_INT)
            rc = sqlite3_column_int_c( stmt%stmt_handle, i-1, columns(i)%int_value )
         case (SQLITE_REAL,SQLITE_DOUBLE)
            rc = sqlite3_column_double_c( stmt%stmt_handle, i-1, columns(i)%double_value )
         case (SQLITE_CHAR)
            rc = sqlite3_column_text_c( stmt%stmt_handle, i-1, columns(i)%char_value )
            call stringtof( columns(i)%char_value )
         end select
        ! if ( rc .ne. 0 ) then
        !    db%error = rc
        !    call sqlite3_errmsg_c( db%db_handle, db%errmsg )
        !    call stringtof( db%errmsg )
        ! endif
      enddo
   else
      finished = .true.
   endif

end subroutine sqlite3_next_row


! sqlite3_query_table --
!    Retrieve the column names and types from a table
! Arguments:
!    db            Structure for the database
!    tablename     Name of the table
!    columns       Columns (allocated array)
! Side effects:
!    The columns array is allocated and filled
! Note:
!    On entry the columns argument must not be
!    associated. On exit, it will point to a
!    freshly allocated array of column names/types
!
subroutine sqlite3_query_table( db, tablename, columns )
   type(SQLITE_DATABASE)                       :: db
   character(len=*)                            :: tablename
   type(SQLITE_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!

   type(SQLITE_STATEMENT)                      :: stmt
   character(len=20+len(tablename))            :: command

   write( command, '(2a)' ) 'select * from ',tablename

   !
   ! Note, we must free the columns, but we can not be sure
   ! they are no longer used. So simply disassociate.
   if ( associated(columns) ) then
      nullify( columns )
   endif
   call sqlite3_prepare( db, command, stmt, columns )
   call sqlite3_finalize( stmt )

end subroutine sqlite3_query_table


! sqlite3_finalize --
!    Finalize the prepared SQL statement
! Arguments:
!    stmt          Handle to the prepared statement
!
subroutine sqlite3_finalize( stmt )
   type(SQLITE_STATEMENT)                      :: stmt

   call sqlite3_finalize_c( stmt%stmt_handle )

end subroutine sqlite3_finalize


! sqlite3_reset --
!    Reset the prepared SQL statement so that it can
!    be used again
! Arguments:
!    stmt          Handle to the prepared statement
!
subroutine sqlite3_reset( stmt )
   type(SQLITE_STATEMENT)                      :: stmt

   call sqlite3_reset_c( stmt%stmt_handle )

end subroutine sqlite3_reset


! sqlite3_step --
!    Run the prepared SQL statement
! Arguments:
!    stmt          Handle to the prepared statement
!    completion    Return code, indicating if the command is complete or
!                  not (SQLITE_DONE, SQLITE_MISUSE or SQLITE_ERROR)
!
subroutine sqlite3_step( stmt, completion )
   type(SQLITE_STATEMENT)                      :: stmt
   integer, intent(out)                        :: completion

   interface
      subroutine sqlite3_step_c( stmt, completion )
         integer, dimension(*) :: stmt
         integer               :: completion
      end subroutine sqlite3_step_c
   end interface


   call sqlite3_step_c( stmt%stmt_handle, completion )

end subroutine sqlite3_step


! sqlite3_prepare --
!    Reset the prepared SQL statement so that it can
!    be used again
! Arguments:
!    stmt          Handle to the prepared statement
!
subroutine sqlite3_prepare( db, command, stmt, columns )
   type(SQLITE_DATABASE), intent(inout)        :: db
   character(len=*), intent(in)                :: command
   type(SQLITE_STATEMENT), intent(out)         :: stmt
   type(SQLITE_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!

   interface
      integer function sqlite3_prepare_c( db, command, stmt )
         integer, dimension(*) :: db
         character(len=*)      :: command
         integer, dimension(*) :: stmt
      end function sqlite3_prepare_c
   end interface

   interface
      subroutine sqlite3_column_count_c( handle, count )
         integer, dimension(*) :: handle
         integer               :: count
      end subroutine sqlite3_column_count_c
   end interface

   interface
      subroutine sqlite3_column_name_type_c( handle, colidx, name, type )
         integer, dimension(*) :: handle
         integer               :: colidx
         character(len=*)      :: name
         character(len=*)      :: type
      end subroutine sqlite3_column_name_type_c
   end interface

   integer                                     :: count
   integer                                     :: i
   character(len=len(command)+1)               :: commandc

   commandc = command
   call stringtoc( commandc )
   db%error = sqlite3_prepare_c( db%db_handle, commandc, stmt%stmt_handle )

   if ( db%error .eq. 0 ) then
      if ( associated(columns) ) return ! Assumption: they are already known

      call sqlite3_column_count_c( stmt%stmt_handle, count )

      allocate( columns(1:count) )

      do i = 1,count
         call sqlite3_column_name_type_c( stmt%stmt_handle, i-1, &
            columns(i)%name, columns(i)%type )
         call stringtof( columns(i)%name )
         call stringtof( columns(i)%type )

         select case (columns(i)%type(1:4) )
         case( 'INT ', 'INTE' )
            columns(i)%type_set = SQLITE_INT
         case( 'FLOA', 'DOUB' )
            columns(i)%type_set = SQLITE_DOUBLE
         case( 'CHAR', 'VARC' )
            columns(i)%type_set = SQLITE_CHAR
         end select

      enddo
   else
      call sqlite3_errmsg_c( db%db_handle, db%errmsg )
   endif

end subroutine sqlite3_prepare


! sqlite3_get_table --
!    Call sqlite3_exec() and return the result in an
!    array of strings
! Arguments:
!    db            Handle to the database
!    command       SQL comman to be executed
!    result        Two-dimensional array of strings (pointer)
!    errmsg        Error message (if any)
! Note:
!    The result array is _nullified_ first, then allocated
!    to hold the resulting table (within the limits of the
!    character strings). It is up to the user to deallocate
!    this array when done.
! Further note:
!    Because we have to split the process into two parts,
!    to allocate an array that is large enough to hold all
!    strings, use is made of a static variable. As a consequence
!    this routine is _not_ thread-safe.
!
subroutine sqlite3_get_table( db, command, result, errmsg )
   type(SQLITE_DATABASE), intent(inout)        :: db
   character(len=*), intent(in)                :: command
   character(len=*), pointer, dimension(:,:)   :: result
   character(len=*), intent(out)               :: errmsg

   character(len=len(command)+1)               :: commandc
   integer                                     :: ncol
   integer                                     :: nrow

   interface
      integer function sqlite3_get_table_1_c( handle, commandc, ncol, &
         nrow, errmsg )
         integer, dimension(*) :: handle
         character(len=*)      :: commandc
         integer               :: ncol
         integer               :: nrow
         character(len=*)      :: errmsg
      end function sqlite3_get_table_1_c
   end interface

   interface
      subroutine sqlite3_get_table_2_c( ncol, nrow, result )
         integer                       :: ncol
         integer                       :: nrow
         character(len=*),dimension(*) :: result
      end subroutine sqlite3_get_table_2_c
   end interface


   commandc = command
   call stringtoc( commandc )

   db%error  = sqlite3_get_table_1_c( db%db_handle, commandc, ncol, nrow, db%errmsg )

   nullify( result )
   if ( db%error == 0 ) then
       allocate( result(ncol,nrow+1) )
       call sqlite3_get_table_2_c( ncol, nrow, result )
   endif

   errmsg = db%errmsg

end subroutine sqlite3_get_table

end module
