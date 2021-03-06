% Path search
% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).

%UCSDIJKSTRA
% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N,1)  :-
     % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N,1).

solve(Start, Solution, G, N,2)  :-
    % insert_legs(), head_member(), build_path()
   ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N,2).
% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N,1)  :-
    goal(Node,1),
    build_path([[Node,Pred]|Expanded], Path).

ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N,2)  :-
        goal(Node,2),
        build_path([[Node,Pred]|Expanded], Path).
% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N,1) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N,1).

ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N,2) :-
        extend(Node, G, Expanded, NewLegs),
        M is L + 1,
        insert_legs(Generated, NewLegs, Generated1),
        ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N,2).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

        


% find the friange of grid.[Max,Max] is the friange.
find_coord(X,Y,Coordinate_x,Coordinate_y,Coordinates):-
    findall(X,land(X,Y),Coordinate_x),
    findall(Y,land(X,Y),Coordinate_y),
    append(Coordinate_x,Coordinate_y,Coordinates).
max_element(Max,[X|L]):-
        max_element_3(Max,X,L).
max_element_3(Max, Max, []):-!.
max_element_3(Max, Max0, [X|L]):-
    X > Max0, !,
    max_element_3(Max, X, L).
max_element_3(Max, Max0, [_|L]):-
    max_element_3(Max, Max0, L).
friange(Max):-
    find_coord(_,_,_,_,Coordinates),
    max_element(Max,Coordinates).
% two positions are both lands.
% right
s((X,Y),(X1,Y1),1):-
    friange(Max),
    X1 is X + 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    land(X1,Y1).
% up
s((X,Y),(X1,Y1),1):-
    friange(Max),
    X1 is X,
    Y1 is Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    land(X1,Y1).
% down
s((X,Y),(X1,Y1),1):-
    friange(Max),
    X1 is X,
    Y1 is Y - 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    land(X1,Y1).
% left
s((X,Y),(X1,Y1),1):-
    friange(Max),
    X1 is X - 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    land(X1,Y1).
% one position is land and the other is water.
% right land to water
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X + 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    not(land(X1,Y1)).
%加
% left land to water
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X - 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    not(land(X1,Y1)).
% dowm land to water
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X,
    Y1 is Y - 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    not(land(X1,Y1)).
%加
%up land to water
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X,
    Y1 is Y+1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    land(X,Y),
    not(land(X1,Y1)).

%right water to land
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 = X + 1,
    Y1 = Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    land(X1,Y1).
%left water to land
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 = X - 1,
    Y1 = Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    land(X1,Y1).

%up water to land
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 = X,
    Y1 = Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    land(X1,Y1).
% dowm water to land
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 = X,
    Y1 = Y-1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    land(X1,Y1).
% two positions are both water.
% right swim
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X + 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    not(land(X1,Y1)).
% left swim
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X - 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    not(land(X1,Y1)).
% down swim
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X,
    Y1 is Y - 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    not(land(X1,Y1)).
% up swim
s((X,Y),(X1,Y1),20):-
    friange(Max),
    X1 is X,
    Y1 is Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max,
    Y1 =< Max,
    not(land(X,Y)),
    not(land(X1,Y1)).
% the final aim.
goal((X,Y),1):-
    monster(X,Y).

goal((X,Y),2):-
    bb(X,Y),
    land_or_dropped(X,Y).

% put these pathes needing to be dropped stone togather.
sway([(X,Y)|T],Res):-
    not(land(X,Y)),
    sway(T,Res1),
    Res=[(X,Y)|Res1].

sway([(X,Y)|T],Res):-
    land_or_dropped(X,Y),
    sway(T,Res).
sway([],[]).
% Find these pathes needing to be dropped stone.
ini_path(Result):-

    solve((1,1),Sol,_,_,1),
    sway(Sol,Result).
% reverse the path list
reverse(List, Rev) :-
        reverse(List, Rev, []).
reverse([], L, L).
reverse([H|T], L, SoFar) :-
        reverse(T, L, [H|SoFar]).
intention_path(Rev):-
    ini_path(Result),
    reverse(Result,Rev).
% add goal into each coordinates
goal_path([(X,Y)|T],Result):-
    goal_path(T,Result1),
    Result = [[goal(X,Y),[]]|Result1].
goal_path([],[]).
% goal format standardation.
intention_goal_path(Result):-
    intention_path(Rev),
    goal_path(Rev,Result).

% initial_intentions.
initial_intentions(Intentions):-
    intention_goal_path(Result),
    Intentions = intents(Result,[]).


% trigger(Percepts,Goal)
trigger([],[]).

trigger([stone(X,Y)|T],Goal):-
    trigger(T,Goal2),
    Goal = [goal(X,Y)|Goal2].
%删掉

%base case Goal become empty and Intentions1 become [].
include_2([],_,[]).
%case: goal(X,Y) is in initial intentions.
include_2([[goal(X,Y),_]|T],Pick,Intentions1):-
    member([goal(X,Y),_],Pick),
    include_goal(T,Pick,Intentions1).

%case: goal(X,Y) is not in initial intentions so we need calculation distance and order it.
include_2([[goal(X,Y),Plan]|T],Pick,Intentions1):-
        land_or_dropped(X,Y),
        not(member([goal(X,Y),_],Pick)),
        agent_at(X1,Y1),
        distance((X1,Y1),(X,Y),D),
        include_2(T,Pick,Intensions11),
        Intentions1 = [[goal(X,Y),Plan,D]|Intensions11].

include_goal(Goal,intents(Drop,Pick),Intentions1):-
    merge_include_goal(Goal,intents(Drop,Pick),Intent0),
    include_2(Intent0,Pick,Intentions1).


% put new Goals and intentions together. 
merge_include_goal(Goal,intents(_,G),Intentions1):-
    %................
    find_goal(Goal,Goal_Goal),
    i_goal(Goal_Goal,Goal1),
    append(G, Goal1, Intentions1).

find_goal([goal(X,Y)|T],[goal(X,Y)|Res2]):-
    agent_at(X1,X2),
    assert(bb(X,Y)),
    goal((X,Y),2),
    solve((X1,X2),Find_path,_,_,2),
    retractall(bb(X,Y)),
    sway(Find_path,Final_path),
    is_Empty(Final_path),
    find_goal(T,Res2).
find_goal([],[]).
find_goal([goal(X,Y)|T],Res1):-
        agent_at(X1,Y1),
        assert(bb(X,Y)),
        goal((X,Y),2),
        solve((X1,Y1),Find_path,_,_,2),
        retractall(bb(X,Y)),
        sway(Find_path,Final_path),
        not(is_Empty(Final_path)),
        find_goal(T,Res1).


i_goal([H|T],Goal1):-
    i_goal(T,Goal2),
    append([[H,[]]],Goal2,Goal1).
i_goal([],[]).

% decreasing order of the length of the shortest valid path from the agent's current position.
min_order(Goal,Intentions,Sortedlist):-
    include_goal(Goal,Intentions,Intentions1),
    take(Intentions1,Result1,Result2),
    quick(Result1,Result3),
    compare_quick(Intentions1,Result3,Sortedlist).

compare_quick([[A,B,C]|T],[P1|PT2],Shuchu):-
    P1 = A,
    append(T,[[A,B,C]],Ada),
    compare_quick(Ada,PT2,Shuchu2),
    Shuchu = [[A,B]|Shuchu2].
compare_quick(_,[],[]).
compare_quick([[A,B,C]|T],[P1|PT2],Shuchu):-
    P1 \= A,
    append(T,[[A,B,C]],Ada),
    compare_quick(Ada,[P1|PT2],Shuchu).


take([[A,B,C]|T],Result1,Result2):-
    take(T,R1,R2),
    Result1 = [[A,C]|R1],
    Result2 = [B|R2].
take([],[],[]).

quick([], []).
quick([[Aim1,Pivot]|Rest], SortedList) :-
    findall([Aim,N], (member([Aim,N],Rest), N =< Pivot), LessList),
   findall([Aim,N], (member([Aim,N],Rest), N > Pivot), GreaterList),
   quick(LessList, SortedLessList),
   quick(GreaterList, SortedGreaterList),
   append(SortedLessList, [Aim1|SortedGreaterList], SortedList).

%merge the intentions with new goals.
merge_goal(intents(H,_),Sorted,In1):-
    In1 = intents(H,Sorted).

incorporate_goals(Goal,Intentions,Intentions1):-
    min_order(Goal,Intentions,Sortedlist),
    merge_goal(Intentions,Sortedlist,Intentions1).

is_Empty([]).
get_action(intents(Init_drop,Init_pick),intents(Init_drop,Init_pick),move(X1,Y1)):-
    agent_at(X1,Y1),
    agent_stones(0),
    is_Empty(Init_pick).

get_action(intents(Init_drop,[[goal(X,Y),Pickfirst_plan]|Pick_plan]),intents(Init_drop,[[goal(X,Y),Next_pickfirst_plan]|Pick_plan]),Next_action):-
    agent_stones(0),
    agent_at(X1,Y1),
    (is_Empty(Pickfirst_plan);Pickfirst_plan = [AV|_],\+applicable(AV)),
    design_plan([pick(X,Y)],X1,Y1,X,Y,[Next_action|Next_pickfirst_plan]).

get_action(intents(Init_drop,[[goal(X,Y),[First_action|Next_action]]|Pick_plan]),intents(Init_drop,[[goal(X,Y),Next_action]|Pick_plan]),First_action):-
    agent_stones(0).

get_action(intents([[goal(X,Y),Dropfirst_plan]|Drop_plan],Int_pick),intents([[goal(X,Y),Next_dropfirst_plan]|Drop_plan],Int_pick),Next_action):-

    agent_stones(1),
    (is_Empty(Dropfirst_plan);Dropfirst_plan = [AV|_],\+applicable(AV)),
    agent_at(X1,Y1),
    design_plan([drop(X,Y)],X1,Y1,X,Y,[Next_action|Next_dropfirst_plan]).

get_action(intents([[goal(X,Y),[First_action|Next_action]]|Drop_plan],Int_pick),intents([[goal(X,Y),Next_action]|Drop_plan],Int_pick),First_action):-
    agent_stones(1).




design_plan(MA,X1,Y1,X,Y,[move(X2,Y1)|Next_plan]):-
    X1 > X,
    X2 is X1 - 1,
    design_plan(MA,X2,Y1,X,Y,Next_plan).
design_plan(MA,X1,Y1,X,Y,[move(X2,Y1)|Next_plan]):-
    X1 < X,
    X2 is X1 + 1,
    design_plan(MA,X2,Y1,X,Y,Next_plan).
design_plan(MA,X1,Y1,X,Y,[move(X1,Y2)|Next_plan]):-
    Y1 > Y,
    Y2 is Y1 - 1,
    design_plan(MA,X1,Y2,X,Y,Next_plan).
design_plan(MA,X1,Y1,X,Y,[move(X1,Y2)|Next_plan]):-
    Y1 < Y,
    Y2 is Y1 + 1,
    design_plan(MA,X1,Y2,X,Y,Next_plan).
design_plan(MA,X1,Y1,X,Y,MA):-
    (X1=X,Z is abs(Y-Y1),Z=1);
    (Y1=Y,K is abs(X-X1),K=1).

update_intentions(at(_,_), Intentions2, Intentions2).
update_intentions(dropped(_,_), intents([_|Init_drop],Init_pick), intents(Init_drop,Init_pick)).
update_intentions(picked(_,_),intents(Drop_plan,[_|Init_pick]),intents(Drop_plan,Init_pick)).


land_or_dropped(X,Y) :-
        land(X,Y).