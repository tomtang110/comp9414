% Assignment name: Assignement1
% Name: Nanyang Tang
% Student Number: z5103095
% Date: 27/03/2018

% Question1
% Just judge whether the head of list is less than 0. if it is less than 0, it would be recursive.
% if it is more than 0,
% it would leave out it. The predicate will stop recursion untill list equal to empty and the sum would be 0.

% This predicate is the termination condition that when the list is empty, the sum is 0
sumsq_neg([],0).
%This predicate is that when H<0, H*2 would be added into sum.
sumsq_neg([H|T],Sum):-
    H < 0,
    sumsq_neg(T,Y),
    Sum is H*H+Y.
%This predicate is that when H>0, This list would be skiped and nothing is added into sum.
sumsq_neg([H|T],Sum):-
        H >= 0 ,
        sumsq_neg(T,Sum).

% Question2
% In all_like_all predicate, first use predicate-findall to obtain a list including all fruits that
% first one in people list like, then I write a car predicate
% to check whether each in fruits list given is in all fruits list.


% likes(mary, apple).
% likes(mary, pear).
% likes(mary, grapes).
% likes(tim, mango).
% likes(tim, apple).
% likes(jane, apple).
% likes(jane, mango).

% This predicate is to check whether H2 is in Fruitlist.
car(Fruitlist,[H2|T2]):-
    member(H2,Fruitlist),
    car(Fruitlist,T2).
% This predicate is the termination condition that when Fruitlist is empty, the first parameter does not matter.
car(_,[]).

% This predicate is to find all fruits that a person , and check wheter the assigned fruits is in
% the favoriate fruits list of the person.
all_like_all([Head|Tail],What_list) :-
    findall(Fruit,likes(Head,Fruit),Fruitlist),
    car(Fruitlist,What_list),
    all_like_all(Tail,What_list),
    Tail \= [].

%This predicate is the termination condition of all_like_all([Head|Tail],What_list)
%that when people list is one element, the predicate terminate.
all_like_all([Head],What_list):-
    findall(Fruit,likes(Head,Fruit),Fruitlist),
    car(Fruitlist,What_list).


% This predicate is that when people list is empty, we check whether the fruits of What_list are liked
% If it is liked, it would return false.
all_like_all([],[H|T]):-
    not(likes(_,H)),
    all_like_all([],T).
% This predicate is the termination condition that when people list is empty, the What_list is empty.
all_like_all([],[]).






% Question3
%  In sqrt_table predicate, when N >= M, I write a car predicate to obtain the result of sqrt(N),then C=N-1 and it and M would be recursive untill
%  M<N, the recursion would end. 
% This predicate is that when start number > end number, compute the sqrt of start number and add it and
% start number into result list.
sqrt_table(Start,End,Result):-
    Start > End,
    par(Start,B),
    C is Start-1,
    sqrt_table(C,End,Y),
    append([[Start,B]],Y,Result).
% This predicate is termination condition that when start number < end number, Result list is added a empty list
sqrt_table(Start,End,Result):-
        Start = End,
        par(Start,B),
        Result=[[Start,B]].

% This predicate is to compute the sqrt of start number.
par(Start,B):-
    B is sqrt(Start).

% Question4
% In chop_up predicate, judge predicate is to judge whether first number and second number in list is 1.
% kuang predicate is to obtain the list of successive number
% compare predicate is to judge whether first number and second number in list is 1 and if it is not 1 or empty,
% it would return the first number and record current position which is p in list.
% xunhuan is to identify position of successive number, fenkai is to identify list needed to be obtained and 
% the function of feikuang predicate is same as the function of compare predicate, it just change X=H to X \=H
% nokuang predicate is to obtian the inconsecutive list



%This predicate is that when H1 are the successor of H, H is added into Littlelist.
compare([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1 -1,
    X=H,
    compare(T,T,L_c,P),
    Littlelist=[H|L_c].
% This predicate is that when H1 are not the successor of H, H is added into Littlelist, and the predicate
% would not be recursive any more.
compare([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X \=H,
    Littlelist=[H],
    P=T.
% This predicate is that When T is empty, H is added into Littlelist and the predicate would not be recursive
% any more.
compare([H|T],[_],Littlelist,P):-
    T = [],
    Littlelist=[H],
    P=[].
% This predicate is that when H is not the predecessor of H1, H is added into Littlelist.
feikuang([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X \= H,
    feikuang(T,T,L_c,P),
    Littlelist=[H|L_c].
% This predicate is that when H is the predecessor of H1, H is added into Littlelist and the predicate
% would not be recursive any more.
feikuang([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X = H,
    Littlelist=[],
    P=[H|T].
% This predicate is that when T is empty, H is added into Littlelist and the predicate would not be recursive
% and p is empty.
feikuang([H|T],[_],Littlelist,P):-
    T = [],
    Littlelist=[H],
    P=[].
% Thie predicate is to let Firsth = Hfc and Filist = Tfc
finalcompare([Hfc|Tfc],Firsth,Filist):-
    Firsth=Hfc,
    Filist=Tfc.
% This predicate is that when Tfen is not empty, the predicate would be in recursion.
xunhuan([_|Tfen],P1):-
    Tfen \= [],
    xunhuan(Tfen,P1).
% This predicate is that when Tfen is empty, the predicate would not be in recursion and P1 is Hfen.
xunhuan([Hfen|Tfen],P1):-
    Tfen = [],
    P1=[Hfen].
% This predicate is to add Filist into Rfianl list.
fenkai(Filist,Firsth,Rfinal):-
    xunhuan(Filist,P1),    
    Rfinal=[Firsth|P1].
% This predicate is to check whether Head - Head1 = 0
judge([Head|_],[_,Head1|_]):-
    1 is Head1-Head.
% This predicate to make Kuanglist = Rfinal underlying the condition that the number in list is successive.
kuang(List,Kuanglist,Position):-
    compare(List,List,Littlelist,P),
    finalcompare(Littlelist,Firsth,Filist),
    fenkai(Filist,Firsth,Rfinal),
    Kuanglist=Rfinal,
    Position=P.
% This predicate to make Littlelist = Noklist based on the condition that the number in list is not successive.
nokuang(List,Noklist,Position):-
    feikuang(List,List,Littlelist,P),
    Noklist=Littlelist,
    Position=P.
% This predicate is that When H and H1 are successive, Kuang list is created and H is added into it, and
% Kuanglist is added into Newlist.
chop_up(List,Newlist):-
    List \= [],
    judge(List,List),
    kuang(List,Kuanglist,Position),
    chop_up(Position,Y),
    append([Kuanglist],Y,Newlist).
% This predicate is that when H and H1 are unsuccessive, Noklist list is created and H is added into it,
% and Noklist is added into Newlist.
chop_up(List,Newlist):-
    List \= [],
    not(judge(List,List)),
    nokuang(List,Noklist,Position),
    chop_up(Position,Y),
    append(Noklist,Y,Newlist).
% This predicate is the termination condition of chop_up that when List is empty, Newlist is empty.
chop_up(List,Newlist):-
    List=[],
    Newlist=[].

% Question 5
% In tree_eval predicate, when mid-term is tree(empty,z,empty), Eval =Value, when mid-term is tree(empty, Operator,empty)
% Eval = Operator, when first term in tree(-,-,-) and second term don't equal empty, tree_eval would recur first term and second term
% In addition, in tree_eval(Value,tree(empty,z,empty), Value), ! is necessary . this is because we should prune otherwise it would occur problem.

%This predicate is that when tree(empty,z,empty) occur, the Eval is Value and a pruning is necessary at here.
tree_eval(Value,tree(empty,z,empty), Value) :- !.
%This predicate is that when tree(empty,Operator,empty) occur, the Eval is Operator.
tree_eval(_, tree(empty, Operator,empty), Operator).
% This predicate is that, when Frontnum is not empty and Latternum is not empty, Frontnum and Latternum
% would be in recursion. Eval would be computed.
tree_eval(Value,tree(Frontnum,Operator,Latternum),Eval):-
    Frontnum \= empty,
    Latternum \= empty,
    tree_eval(Value,Frontnum,E1),
    tree_eval(Value,Latternum,E2),
    Expression =..[Operator,E1,E2],
    Eval is Expression.
