
sumsq_neg([],0).
sumsq_neg([H|T],Sum):-
    H < 0,
    sumsq_neg(T,Y),
    Sum is H*H+Y.
sumsq_neg([H|T],Sum):-
        H >= 0 ,
        sumsq_neg(T,Sum).