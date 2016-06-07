! flempar.f90 --
!     Driver template for the LEMON parser generator.
!     Derived from lempar.c
!
!     Original copyright text:
!     ** The author disclaims copyright to this source code.
!
! First off, code is include which follows the "include" declaration
! in the input file.
!
module Parse_mod

%%

private
public :: Parse
public :: Parsealloc
public :: Parsetrace
public :: Parsefree
public :: yyparser       ! (AM, TODO) This will not be influenced by %name
public :: Parsetokentype

! Next is all token values, in a form suitable for use by makeheaders.
! This section will be null unless lemon is run with the -m switch.
!
!
! These constants (all generated automatically by the parser generator)
! specify the various kinds of tokens (terminals) that the parser
! understands.
!
! Each symbol here is a terminal symbol in the grammar.
!
%%
! Make sure the INTERFACE macro is defined.
!
! #ifndef INTERFACE
! # define INTERFACE 1
! #endif
!
! The next thing included is series of defines which control
! various aspects of the generated parser.
!    YYCODETYPE         is the data type used for storing terminal
!                       and nonterminal numbers.  "unsigned char" is
!                       used if there are fewer than 250 terminals
!                       and nonterminals.  "int" is used otherwise.
!    YYNOCODE           is a number of type YYCODETYPE which corresponds
!                       to no legal terminal or nonterminal number.  This
!                       number is used to fill in empty slots of the hash
!                       table.
!    YYFALLBACK         If defined, this indicates that one or more tokens
!                       have fall-back values which should be used if the
!                       original value of the token will not parse.
!    YYACTIONTYPE       is the data type used for storing terminal
!                       and nonterminal numbers.  "unsigned char" is
!                       used if there are fewer than 250 rules and
!                       states combined.  "int" is used otherwise.
!    ParseTOKENTYPE     is the data type used for minor tokens given
!                       directly to the parser from the tokenizer.
!    YYMINORTYPE        is the data type used for all minor tokens.
!                       This is typically a union of many types, one of
!                       which is ParseTOKENTYPE.  The entry in the union
!                       for base tokens is called "yy0".
!    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
!                       zero the stack is dynamically sized using realloc()
!    ParseARG_SDECL     A static variable declaration for the %extra_argument
!    ParseARG_PDECL     A parameter declaration for the %extra_argument
!    ParseARG_STORE     Code to store %extra_argument into yypParser
!    ParseARG_FETCH     Code to extract %extra_argument from yypParser
!    YYNSTATE           the combined number of states.
!    YYNRULE            the number of rules in the grammar
!    YYERRORSYMBOL      is the code number of the error symbol.  If not
!                       defined, then do no error processing.
!
%%

integer, parameter :: YY_NO_ACTION     = (YYNSTATE+YYNRULE+2)
integer, parameter :: YY_ACCEPT_ACTION = (YYNSTATE+YYNRULE+1)
integer, parameter :: YY_ERROR_ACTION  = (YYNSTATE+YYNRULE)

! The yyzerominor constant is used to initialize instances of
! YYMINORTYPE objects to zero. !
type(YYMINORTYPE), save :: yyzerominor

! Next are that tables used to determine what action to take based on the
! current state and lookahead token.  These tables are used to implement
! functions that take a state number and lookahead value and return an
! action integer.
!
! Suppose the action integer is N.  Then the action is determined as
! follows
!
!   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
!                                      token onto the stack and goto state N.
!
!   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
!
!   N == YYNSTATE+YYNRULE              A syntax error has occurred.
!
!   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
!
!   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
!                                      slots in the yy_action[] table.
!
! The action table is constructed as a single large table named yy_action[].
! Given state S and lookahead X, the action is computed as
!
!      yy_action[ yy_shift_ofst[S] + X ]
!
! If the index value yy_shift_ofst[S]+X is out of range or if the value
! yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
! is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
! and that yy_default[S] should be used instead.
!
! The formula above is for computing the action when the lookahead is
! a terminal symbol.  If the lookahead is a non-terminal (as occurs after
! a reduce action) then the yy_reduce_ofst[] array is used in place of
! the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
! YY_SHIFT_USE_DFLT.
!
! The following are the tables generated in this section:
!
!  yy_action[]        A single table containing all actions.
!  yy_lookahead[]     A table containing the lookahead for each entry in
!                     yy_action.  Used to detect hash collisions.
!  yy_shift_ofst[]    For each state, the offset into yy_action for
!                     shifting terminals.
!  yy_reduce_ofst[]   For each state, the offset into yy_action for
!                     shifting non-terminals after a reduce.
!  yy_default[]       Default action for each state.
!
%%

character(len=40), allocatable, dimension(:) :: yyTokenName
character(len=80), allocatable, dimension(:) :: yyRuleName

type yyRuleInfoType
    integer :: lhs
    integer :: nrhs
end type
type(yyRuleInfoType), dimension(:), allocatable :: yyruleinfo

!
! (AM,TODO) Setting NDEBUG to .true. should make the parser
! faster but there is some bug that makes it fail - see yy_reduce
logical, parameter :: NDEBUG = .false.

integer, parameter :: YY_SZ_ACTTAB = size(yy_action)

! The next table maps tokens into fallback tokens.  If a construct
! like the following:
!
!      %fallback ID X Y Z.
!
! appears in the grammer, then ID becomes a fallback token for X, Y,
! and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
! but it does not parse, the type of the token is changed to ID and
! the parse is retried before an error is thrown.
!
%%

! The following structure represents a single element of the
! parser's stack.  Information stored includes:
!
!   +  The state number for the parser at this level of the stack.
!
!   +  The value of the token stored at this level of the stack.
!      (In other words, the "major" token.)
!
!   +  The semantic value stored at this level of the stack.  This is
!      the information used by the action routines in the grammar.
!      It is sometimes called the "minor" token.
!
type yyStackEntry
  integer :: stateno          ! The state-number
  integer :: major            ! The major token value.  This is the code
                              ! number for the token at this stack level
  type(YYMINORTYPE) :: minor  ! The user-supplied minor token value. This
                              ! is the value of the token
end type

! The state of the parser is completely contained in an instance of
! the following structure
!
type yyParser
    private
    integer :: yyidx                     ! Index of top element in stack
    integer :: yyerrcnt                  ! Shifts left before out of the error
!   ParseARG_SDECL                       ! A place to hold %extra_argument
    integer yystksz                      ! Current side of the stack
    type(yyStackEntry), dimension(:), pointer :: yystack ! The parser's stack
endtype

integer, save     :: yyTraceFILE = -1
character(len=20) :: yyTracePrompt = '>>'

contains
!
! Turn parser tracing on by giving a stream to which to write the trace
! and a prompt to preface each trace message.  Tracing is turned off
! by making either argument NULL
!
! Inputs:
! <ul>
! <li> A FILE* to which trace output should be written.
!      If NULL, then tracing is turned off.
! <li> A prefix string written at the beginning of every
!      line of trace output.  If NULL, then tracing is
!      turned off.
! </ul>
!
! Outputs:
! None.
!
subroutine ParseTrace( TraceFILE, TracePrompt )
    integer, intent(in)          :: TraceFILE
    character(len=*), intent(in) :: TracePrompt

    yyTraceFILE   = TraceFILE
    yyTracePrompt = TracePrompt

    if ( yyTraceFILE < 0 ) then
        yyTracePrompt = ''
    endif
end subroutine

! For tracing shifts, the names of all terminals and nonterminals
! are required.  The following table supplies these names !
!
subroutine yySetTokenNames

%%

end subroutine

! For tracing reduce actions, the names of all rules are required.
!
subroutine yySetRuleNames
%%

end subroutine

!
! Try to increase the size of the parser stack.
!
subroutine yyGrowStack(p)
    type(yyParser) :: p

    integer                                   :: newSize
    type(yyStackEntry), dimension(:), pointer :: pNew

    if ( YYSTACKDEPTH <= 0 ) then
        newSize = p%yystksz*2 + 100
        allocate( pNew(1:newSize) )
        pNew(1:p%yystksz) = p%yystack(1:p%yystksz)
        deallocate( p%yystack )
        p%yystack => pNew        !?? How to make it zero-based?
        p%yystksz = newSize;

        if ( yyTraceFILE >= 0 ) then
            write(yyTraceFILE, "(A,'Stack grows to ',i0.0,' entries!')" ) &
              yyTracePrompt, p%yystksz
        endif
    endif
end subroutine

!
! This subroutine (was: function) allocates a new parser.
! The only argument is a pointer to a function which works like
! malloc.
!
! Inputs:
! A pointer to the function used to allocate memory.
!
! Outputs:
! A pointer to a parser.  This pointer is used in subsequent calls
! to Parse and ParseFree.
!
subroutine ParseAlloc( pParser )
    type(yyParser) :: pParser

    logical, save  :: init = .true.

    if ( init ) then
        init = .false.
        call yySetTokenNames
        call yySetRuleNames
        call yySetRuleInfo
    endif

    pParser%yyidx = -1

    if ( YYSTACKDEPTH > 0 ) then
        allocate( pParser%yystack(1:YYSTACKDEPTH) )
        pParser%yystksz = YYSTACKDEPTH
    else
        call yyGrowStack(pParser)
    endif

end subroutine

! The following function deletes the value associated with a
! symbol.  The symbol can be either a terminal or nonterminal.
! "yymajor" is the symbol code, and "yypminor" is a pointer to
! the value.
!
subroutine yy_destructor( yymajor, yypminor)
    integer           :: yymajor
    type(YYMINORTYPE) :: yypminor

    write(*,*) 'AM: yy_destructor', yymajor

    select case( yymajor )
      ! Here is inserted the actions which take place when a
      ! terminal or non-terminal is destroyed.  This can happen
      ! when the symbol is popped from the stack during a
      ! reduce or during error processing or when a parser is
      ! being destroyed before it is finished parsing.
      !
      ! Note: during a reduce, the only symbols destroyed are those
      ! which appear on the RHS of the rule, but which are not used
      ! inside the C code.
      !
%%
      case default
          return   ! If no destructor action specified: do nothing

    end select
end subroutine

!
! Pop the parser's stack once.
!
! If there is a destructor routine associated with the token which
! is popped from the stack, then call it.
!
! Return the major token number for the symbol popped.
!
integer function yy_pop_parser_stack( pParser )
    type(yyParser), target      :: pParser

    integer                     :: yymajor
    type(yyStackEntry), pointer :: yytos

    write(*,*) 'AM: yy_pop_parser_stack'

    yytos => pParser%yystack(pParser%yyidx+1)

    yy_pop_parser_stack = 0
    if( pParser%yyidx < 0  ) return

    if ( yyTraceFILE >= 0 .and. pParser%yyidx >= 0 ) then
        write( yyTraceFILE, "(A,'Popping ',A)" ) &
            trim(yyTracePrompt),                 &
            trim(yyTokenName(yytos%major))
    endif

    yymajor = yytos%major
    call yy_destructor( yymajor, yytos%minor)
    pParser%yyidx = pParser%yyidx - 1

    yy_pop_parser_stack = yymajor
end function

!
! Deallocate and destroy a parser.  Destructors are all called for
! all stack elements before shutting the parser down.
!
! Inputs:
! <ul>
! <li>  A pointer to the parser.  This should be a pointer
!       obtained from ParseAlloc.
! <li>  A pointer to a function used to reclaim memory obtained
!       from malloc.
! </ul>
!
subroutine ParseFree( pParser )
    type(yyParser) :: pParser             ! The parser to be deleted

    integer        :: dummy

    if ( .not. associated( pParser%yystack ) ) then
        return
    else
        do while( pParser%yyidx >= 0 )
            dummy = yy_pop_parser_stack( pParser )
        enddo

        deallocate( pParser%yystack )
    endif
end subroutine
!
! Find the appropriate action for a parser given the terminal
! look-ahead token iLookAhead.
!
! If the look-ahead token is YYNOCODE, then check to see if the action is
! independent of the look-ahead.  If it is, return the action, otherwise
! return YY_NO_ACTION.
!
recursive integer function yy_find_shift_action( pParser, iLookAhead ) result(sh)
    type(yyParser) :: pParser        ! The parser
    integer        :: iLookAhead     ! The look-ahead token

    integer i
    integer j
    integer stateno
    integer iFallback            ! Fallback token

    stateno = pParser%yystack(pParser%yyidx+1)%stateno

    write(*,*) 'AM: yy_find_shift_action', stateno

    if ( stateno > YY_SHIFT_MAX .or. yy_shift_ofst(stateno) == YY_SHIFT_USE_DFLT ) then
        sh = yy_default(stateno)
        return
    endif

    call assert( iLookAhead /= YYNOCODE, "iLookAhead /= YYNOCODE" )

    i = yy_shift_ofst(stateno) + iLookAhead

    if ( i < 0 .or. i >= YY_SZ_ACTTAB .or. yy_lookahead(i) /= iLookAhead ) then
        if ( iLookAhead > 0 ) then
            if ( YYHASFALLBACK ) then
                if ( iLookAhead < size(yyFallback) ) then
                    iFallback = yyFallback(iLookAhead)
                    if ( iFallback /= 0 ) then
                        if ( yyTraceFILE >= 0 ) then
                            write( yyTraceFILE, "(A,'FALLBACK ',A, ' => ',A)" ) &
                                trim(yyTracePrompt), trim(yyTokenName(iLookAhead)), &
                                trim(yyTokenName(iFallback))
                         endif
                         sh = yy_find_shift_action(pParser, iFallback)
                         return
                    endif
                endif
            endif ! YYFALLBACK

            if ( YYWILDCARD >= 0 ) then
                j = i - iLookAhead + YYWILDCARD
                if ( j >= 0 .and. j < YY_SZ_ACTTAB .and. yy_lookahead(j)==YYWILDCARD ) then
                    if ( yyTraceFILE >= 0 ) then
                        write( yyTraceFILE, "(A,'WILDCARD ',A,' => ',A)") &
                            trim(yyTracePrompt), trim(yyTokenName(iLookAhead)), &
                            trim(yyTokenName(YYWILDCARD))
                    endif
                    sh = yy_action(j)
                    return
                endif
            endif

            sh = yy_default(stateno)
            return
        endif
    else
        sh = yy_action(i)
        return
    endif
end function

!
! Find the appropriate action for a parser given the non-terminal
! look-ahead token iLookAhead.
!
! If the look-ahead token is YYNOCODE, then check to see if the action is
! independent of the look-ahead.  If it is, return the action, otherwise
! return YY_NO_ACTION.
!
integer function yy_find_reduce_action( stateno, iLookAhead )
    integer :: stateno               ! Current state number !
    integer :: iLookAhead            ! The look-ahead token !

    integer :: i

    write(*,*) 'AM: yy_find_reduce_action', stateno

    if ( YYERRORSYMBOL >= 0) then
        if ( stateno > YY_REDUCE_MAX ) then
            yy_find_reduce_action = yy_default(stateno)
            return
        endif
    else
        call assert( stateno <= YY_REDUCE_MAX, "stateno <= YY_REDUCE_MAX " )
    endif

    i = yy_reduce_ofst(stateno);
    call assert( i /= YY_REDUCE_USE_DFLT, "i /= YY_REDUCE_USE_DFLT" )
    call assert( iLookAhead /=YYNOCODE, "iLookAhead /=YYNOCODE" )

    i = i + iLookAhead

    if ( YYERRORSYMBOL >= 0 ) then
        if( i < 0 .or. i >= YY_SZ_ACTTAB .or. yy_lookahead(i) /= iLookAhead ) then
            yy_find_reduce_action = yy_default(stateno)
            return
        endif
    else
        call assert( i >= 0 .and. i < YY_SZ_ACTTAB, "i>=0 .and. i<YY_SZ_ACTTAB" )
        call assert( yy_lookahead(i)==iLookAhead, "yy_lookahead(i)==iLookAhead" )
    endif

    write(*,*) 'AM: find_reduce result:', i, yy_action(i)
    yy_find_reduce_action = yy_action(i)
    return
end function

!
! The following routine is called if the stack overflows.
!
subroutine yyStackOverflow( yypParser, yypMinor )
    type(yyParser)    :: yypParser
    type(YYMINORTYPE) :: yypMinor

    integer           :: dummy

!   ParseARG_FETCH
   yypParser%yyidx = yypParser%yyidx - 1

   if ( yyTraceFILE >= 0 ) then
       write( yyTraceFILE, "(A,'Stack Overflow!')" ) trim(yyTracePrompt)
   endif

   do while( yypParser%yyidx>=0 )
       dummy = yy_pop_parser_stack( yypParser )
   enddo
   ! Here code is inserted which will execute if the parser
   ! stack every overflows !
%%

!   ParseARG_STORE  ! Suppress warning about unused %extra_argument var
end subroutine

!
! Perform a shift action.
!
subroutine yy_shift( yypParser, yyNewState, yyMajor, yypMinor )

    type(yyParser), target :: yypParser          ! The parser to be shifted
    integer                :: yyNewState         ! The new state to shift in
    integer                :: yyMajor            ! The major token to shift in
    type(YYMINORTYPE)      :: yypMinor           ! Pointer ot the minor token to shift in

    type(yyStackEntry), pointer :: yytos
    integer                     :: i

    write(*,*) 'AM: yy_shift', yyNewState, yypParser%yyidx

    yypParser%yyidx = yypParser%yyidx + 1

    if ( YYSTACKDEPTH > 0 ) then
        if ( yypParser%yyidx >= YYSTACKDEPTH ) then
            call yyStackOverflow(yypParser, yypMinor)
            return
        endif
    else
        if ( yypParser%yyidx >= yypParser%yystksz ) then
            call yyGrowStack(yypParser)
            if ( yypParser%yyidx >= yypParser%yystksz ) then
                call yyStackOverflow(yypParser, yypMinor)
                return
            endif
        endif
    endif

    yytos => yypParser%yystack(yypParser%yyidx+1)
    yytos%stateno = yyNewState
    yytos%major = yyMajor
    yytos%minor = yypMinor

    write(*,*) 'AM: yy_shift - idx: ', yypParser%yyidx
    if ( yyTraceFILE >= 0 .and. yypParser%yyidx>0 ) then
        write( yyTraceFILE,"(A,'Shift ',I0)" ) trim(yyTracePrompt), yyNewState
        write( yyTraceFILE,"(A,'Stack: ')" ) trim(yyTracePrompt)
        do i = 1,yypParser%yyidx
            write( yyTraceFILE,"(3X,A)" ) yyTokenName(yypParser%yystack(i+1)%major)
        enddo
        write(yyTraceFILE,*)
    endif
end subroutine

! The following table contains information about every rule that
! is used during the reduce.
!
subroutine yySetRuleInfo

%%

end subroutine

!
! Perform a reduce action and the shift that must immediately
! follow the reduce.
!
subroutine yy_reduce( yypParser, yyruleno )
    type(yyParser), target :: yypParser          ! The parser
    integer                :: yyruleno           ! Number of the rule by which to reduce

    integer                :: yygoto             ! The next state
    integer                :: yyact              ! The next action
    type(YYMINORTYPE)      :: yygotominor        ! The LHS of the rule reduced

    type(yyStackEntry), pointer :: yymsp    ! The top of the parser's stack
    type(yyStackEntry), pointer :: yymsp2
    integer                     :: yysize   ! Amount to pop the stack

!   ParseARG_FETCH

    write(*,*) 'AM: yy_reduce', yyruleno

    yymsp => yypParser%yystack(yypParser%yyidx+1)

    if ( yyTraceFILE >= 0 .and. yyruleno >= 0 &
          .and. yyruleno < size(yyRuleName) ) then

        write( yyTraceFILE, "(A,'Reduce [',A,'].')" ) trim(yyTracePrompt), &
            trim(yyRuleName(yyruleno))
    endif

    ! Silence complaints from purify about yygotominor being uninitialized
    ! in some cases when it is copied into the stack after the following
    ! switch.  yygotominor is uninitialized when a rule reduces that does
    ! not set the value of its left-hand side nonterminal.  Leaving the
    ! value of the nonterminal uninitialized is utterly harmless as long
    ! as the value is never used.  So really the only thing this code
    ! accomplishes is to quieten purify.
    !
    ! 2007-01-16:  The wireshark project (www.wireshark.org) reports that
    ! without this code, their parser segfaults.  I'm not sure what there
    ! parser is doing to make this happen.  This is the second bug report
    ! from wireshark this week.  Clearly they are stressing Lemon in ways
    ! that it has not been previously stressed...  (SQLite ticket #2172)
    !
    !memset(&yygotominor, 0, sizeof(yygotominor));!

    yygotominor = yyzerominor


    select case( yyruleno )
    ! Beginning here are the reduction cases.  A typical example
    ! follows:
    !   case 0:
    !  #line <lineno> <grammarfile>
    !     { ... }           // User supplied code
    !  #line <lineno> <thisfile>
    !     break;
    !
%%
    end select
    yygoto = yyRuleInfo(yyruleno)%lhs
    yysize = yyRuleInfo(yyruleno)%nrhs
    yypParser%yyidx = yypParser%yyidx - yysize

    write(*,*) 'AM:', yysize, yyruleno, yygoto

    yymsp2 => yypParser%yystack(yypParser%yyidx+1)
    yyact = yy_find_reduce_action( yymsp2%stateno, yygoto )

    if ( yyact < YYNSTATE ) then
        !
        ! NOTE AM:
        ! NDEBUG = .true. causes a strange error. I have not
        ! figured out yet where the bug is located. It must be
        ! in this piece of the program though.
        !
        if ( NDEBUG ) then
            ! If we are not debugging and the reduce action popped at least
            ! one element off the stack, then we can push the new element back
            ! onto the stack here, and skip the stack overflow test in yy_shift().
            ! That gives a significant speed improvement. !
            !
            ! AM: Fairly complex computation of the correct index!
            if ( yysize > 0 ) then
                yypParser%yyidx = yypParser%yyidx + 1
                yymsp => yypParser%yystack(yypParser%yyidx-1-(yysize-1)+1)
                yymsp%stateno = yyact
                yymsp%major   = yygoto
                yymsp%minor   = yygotominor
            else
                call yy_shift( yypParser, yyact, yygoto, yygotominor )
            endif
        else
            call yy_shift( yypParser, yyact, yygoto, yygotominor )
        endif
    else
        call assert( yyact == YYNSTATE + YYNRULE + 1, "yyact == YYNSTATE + YYNRULE + 1" )
        call yy_accept( yypParser )
    endif
end subroutine

!
! The following code executes when the parse fails
!
subroutine yy_parse_failed( yypParser )
    type(yyParser)  :: yypParser           ! The parser !

    integer        :: dummy

!   ParseARG_FETCH

    if ( yyTraceFILE >= 0 ) then
        write( yyTraceFILE, "(A,'Fail!')" ) trim(yyTracePrompt)
    endif

    do while( yypParser%yyidx >= 0 )
        dummy = yy_pop_parser_stack( yypParser )
    enddo
    ! Here code is inserted which will be executed whenever the
    ! parser fails !
%%
!   ParseARG_STORE  ! Suppress warning about unused %extra_argument variable !
end subroutine

!
! The following code executes when a syntax error first occurs.
!
subroutine yy_syntax_error( yypParser, yymajor, yyminor )
    type(yyParser)    :: yypParser           ! The parser
    integer           :: yymajor             ! The major type of the error token
    type(YYMINORTYPE) :: yyminor             ! The minor type of the error token


!   ParseARG_FETCH
!!>>>
!#define TOKEN (yyminor.yy0)
!!<<<
%%

!   ParseARG_STORE  ! Suppress warning about unused %extra_argument variable
end subroutine

!
! The following is executed when the parser accepts
!
subroutine yy_accept( yypParser )
    type(yyParser) :: yypParser           ! The parser !

    integer        :: dummy

!   ParseARG_FETCH

    if ( yyTraceFILE >= 0 ) then
        write( yyTraceFILE, "(A,'Accept!')" ) trim(yyTracePrompt)
    endif

    do while( yypParser%yyidx>=0 )
        dummy = yy_pop_parser_stack( yypParser )
    enddo
    ! Here code is inserted which will be executed whenever the
    ! parser accepts
%%

!   ParseARG_STORE  ! Suppress warning about unused %extra_argument variable
end subroutine

! The main parser program.
! The first argument is a pointer to a structure obtained from
! "ParseAlloc" which describes the current state of the parser.
! The second argument is the major token number.  The third is
! the minor token.  The fourth optional argument is whatever the
! user wants (and specified in the grammar) and is available for
! use by the action routines.
!
! Inputs:
! <ul>
! <li> A pointer to the parser (an opaque structure.)
! <li> The major token number.
! <li> The minor token number.
! <li> An option argument of a grammar-specified type.
! </ul>
!
! Outputs:
! None.
!
subroutine Parse( yypParser, yymajor, yyminor, extra_arg )
    type(yyParser)         :: yypParser             ! The parser
    integer                :: yymajor               ! The major token code number
    type(ParseTOKENTYPE)   :: yyminor               ! The value for the token
    integer, optional      :: extra_arg
!   ParseARG_PDECL               ! Optional %extra_argument parameter

    type(YYMINORTYPE)      :: yyminorunion

    integer                :: yyact             ! The parser action.
    integer                :: dummy
    logical                :: yyendofinput      ! True if we are at the end of input
    logical                :: yyerrorhit        ! True if yymajor has invoked an error
    integer                :: yymx
    type(YYMINORTYPE)      :: u2

    yyerrorhit = .false.

    if ( yypParser%yyidx < 0 ) then
        if ( YYSTACKDEPTH<=0 ) then
            if ( yypParser%yystksz <=0 ) then
                !memset(&yyminorunion, 0, sizeof(yyminorunion));!
                yyminorunion = yyzerominor;
                call yyStackOverflow(yypParser, yyminorunion)
                return
            endif
        endif

        yypParser%yyidx = 0
        yypParser%yyerrcnt = -1
        yypParser%yystack(1)%stateno = 0
        yypParser%yystack(1)%major = 0
    endif

    yyminorunion%yy0 = yyminor
    yyendofinput = (yymajor==0)

!   ParseARG_STORE

    if ( yyTraceFILE >= 0 ) then
        write( yyTraceFILE, "(A,'Input ',A)" ) trim(yyTracePrompt), trim(yyTokenName(yymajor))
    endif

    do
        yyact = yy_find_shift_action( yypParser, yymajor )

    write(*,*) 'AM: Parse', yyact

        if ( yyact < YYNSTATE ) then
            call assert( .not. yyendofinput, ".not. yyendofinput" ) ! Impossible to shift the $ token !
            call yy_shift( yypParser, yyact, yymajor, yyminorunion )
            yypParser%yyerrcnt = yypParser%yyerrcnt - 1
            yymajor = YYNOCODE
        else if ( yyact < YYNSTATE + YYNRULE ) then
            call yy_reduce( yypParser, yyact-YYNSTATE )
        else
            call assert( yyact == YY_ERROR_ACTION, "yyact == YY_ERROR_ACTION" )

            if ( yyTraceFILE >= 0 ) then
                write( yyTraceFILE, "(A,'Syntax Error!')" ) trim(yyTracePrompt)
            endif

            if ( YYERRORSYMBOL >= 0 ) then
                ! A syntax error has occurred.
                ! The response to an error depends upon whether or not the
                ! grammar defines an error token "ERROR".
                !
                ! This is what we do if the grammar does define ERROR:
                !
                !  * Call the %syntax_error function.
                !
                !  * Begin popping the stack until we enter a state where
                !    it is legal to shift the error symbol, then shift
                !    the error symbol.
                !
                !  * Set the error count to three.
                !
                !  * Begin accepting and shifting new tokens.  No new error
                !    processing will occur until three tokens have been
                !    shifted successfully.
                !
                !
                if ( yypParser%yyerrcnt < 0 ) then
                    call yy_syntax_error( yypParser, yymajor, yyminorunion )
                endif
                yymx = yypParser%yystack(yypParser%yyidx+1)%major

                if ( yymx == YYERRORSYMBOL .or. yyerrorhit ) then
                    if ( yyTraceFILE >= 0 ) then
                        write( yyTraceFILE, "(A,'Discard input token ',A)" ) &
                            trim(yyTracePrompt), trim(yyTokenName(yymajor))
                    endif

                    call yy_destructor( yymajor, yyminorunion )
                    yymajor = YYNOCODE
                else
                    do while ( yypParser%yyidx >= 0 .and. yymx /= YYERRORSYMBOL )
                        yyact = yy_find_reduce_action( &
                                     yypParser%yystack(yypParser%yyidx+1)%stateno, &
                                     YYERRORSYMBOL)
                        if (yyact < YYNSTATE ) exit

                        dummy = yy_pop_parser_stack( yypParser )
                    enddo

                    if ( yypParser%yyidx < 0 .or. yymajor == 0 ) then
                        call yy_destructor( yymajor, yyminorunion )
                        call yy_parse_failed( yypParser )
                        yymajor = YYNOCODE
                    else if( yymx /=YYERRORSYMBOL ) then
                        u2%YYERRSYMDT = 0
                        call yy_shift(yypParser, yyact, YYERRORSYMBOL, u2 )
                    endif
                endif
                yypParser%yyerrcnt = 3
                yyerrorhit = .true.
            else
                ! This is what we do if the grammar does not define ERROR:
                !
                !  * Report an error message, and throw away the input token.
                !
                !  * If the input token is $, then fail the parse.
                !
                ! As before, subsequent error messages are suppressed until
                ! three input tokens have been successfully shifted.
                !
                if ( yypParser%yyerrcnt <= 0 ) then
                    call yy_syntax_error( yypParser, yymajor, yyminorunion )
                endif
                yypParser%yyerrcnt = 3
                call yy_destructor( yymajor, yyminorunion)
                if ( yyendofinput ) then
                   call yy_parse_failed( yypParser )
                endif
                yymajor = YYNOCODE
            endif
        endif
        if ( yymajor == YYNOCODE .or. yypParser%yyidx < 0 ) exit

    enddo
    return
end subroutine

! assert routine: mimick the C assert() macro (up to a point)
!
subroutine assert( expr, text )
    logical          :: expr
    character(len=*) :: text

    if ( .not. expr ) then
        write( *, * ) 'Assertion failed - terminating program: '
        write( *, * ) trim(text), ' is NOT true'
        stop
    endif
end subroutine

end module
