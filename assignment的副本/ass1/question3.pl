sqrt_table(Start,End,Result):-
    Start >= End,
    par(Start,B),
    C is Start-1,
    sqrt_table(C,End,Y),
    append([[Start,B]],Y,Result).
sqrt_table(Start,End,Result):-
        Start < End,
        Result=[].

        
par(Start,B):-
    B is sqrt(Start).
