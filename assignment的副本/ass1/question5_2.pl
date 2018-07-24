tree_eval(Value, tree(empty, z, empty), Value):- !.

tree_eval(_, tree(empty, Num, empty), Num).

tree_eval(Value, tree(LeftTree, Operator, RightTree), Eval) :-
	tree_eval(Value, LeftTree, L),
	tree_eval(Value, RightTree, R),
	Expression =.. [Operator, L, R],
	Eval is Expression.