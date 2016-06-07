// Simple parser:
// expr = vaue + value + value + ...

%include {
type ParseTOKENTYPE
    character(len=10) :: token
end type
type StateTYPE
    integer :: sum
end type
}

%extra_argument {type(statetype) state}
%token_type {type(parsetokentype)}
%type term {integer}

expr ::= firstterm plusterms . {
    write(*,*) "Result: ", state%sum
}

firstterm ::= term(T) . {
    state%sum = T
    write(*,*) "First term: ", T ;
}

term(V) ::= NUMBER(N) . {
    read( N%token, *, V )
    write(*,*) "Term: ", V, "-- ",  N->token
}

plusterms ::= .
plusterms ::= plusterms plusterm .

plusterm ::= PLUS term(T) . {
    state%sum = state%sum + T
    write(*,*) "Result so far: ", state%sum
}
