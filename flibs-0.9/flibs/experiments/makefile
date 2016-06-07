TODO: Experiments and proofs of concept for flibs.
--------------------------------------------------

One idea: Diophantine equations

call integer_set_create( set, 2 * x**2 - y**2 + 1 == 0 )
call integer_set_signed( set, 1, .false. )
call integer_set_signed( set, 2, .false. )
call integer_set_limit( set, 1000 )
call integer_set_first( set, pair )

do while( integer_set_has_next(set, pair) )
    write(*,*) integer_set_get(pair,1), integer_set_get(pair,2)
    call integer_set_next( set, pair )
enddo

x and y are "special" variables, the expression returns
a parse tree that can be evaluated to a logical value.
The iteration routines (*_limit, *_first and *_next)
know how to traverse 2D space (with or without signs).

The set variable completely defines the set of (x,y)
pairs that satisfy the above equation.


Second idea: quantum search

Sample program available (as well as an article
on the subject).
