
tree_eval(Value,tree(empty,z,empty), Value) :- !.

tree_eval(_, tree(empty, Operator,empty), Operator).

tree_eval(Value,tree(Frontnum,Operator,Latternum),Eval):-
    Frontnum \= empty,
    Latternum \= empty,
    tree_eval(Value,Frontnum,E1),
    tree_eval(Value,Latternum,E2),
    Expression =..[Operator,E1,E2],
    Eval is Expression.

