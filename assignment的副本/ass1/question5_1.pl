% base case, empty tree evaluates to zero
tree_eval(_, empty, 0).
tree_eval(_, tree(empty), 0).

% Leaf case: z as value
tree_eval(Value, tree(empty, Num, empty), Eval) :-
  Num = z,
  Eval is Value.

% Leaf case: number (non z) as value
tree_eval(_, tree(empty, Num, empty), Eval) :-
  Num \= z,
  Eval is Num.

% Tree Case: for + operator
tree_eval(Value, tree(L, +, R), Eval) :-
  tree_eval(Value, L, LEval),
  tree_eval(Value, R, REval),
  Eval is LEval + REval.
  
% Tree Case: for - operator
tree_eval(Value, tree(L, -, R), Eval) :-
  tree_eval(Value, L, LEval),
  tree_eval(Value, R, REval),
  Eval is LEval - REval.
  
% Tree Case: for * operator
tree_eval(Value, tree(L, *, R), Eval) :-
  tree_eval(Value, L, LEval),
  tree_eval(Value, R, REval),
  Eval is LEval * REval.
  
% Tree Case: for / operator
tree_eval(Value, tree(L, /, R), Eval) :-
  tree_eval(Value, L, LEval),
  tree_eval(Value, R, REval),
  Eval is LEval / REval.
