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

        


% find the friange of grid.[Max_x,Max_y] is the friange of this grid.
% this predicate would find all coordinate_x and coordinate_y.
find_coord(X,Y,Coordinate_x,Coordinate_y):-
    findall(X,land_or_dropped(X,Y),Coordinate_x),
    findall(Y,land_or_dropped(X,Y),Coordinate_y).
% max_element predicate would find the max value in the input list.  
max_element(Max,[X|L]):-
        max_element_3(Max,X,L).
max_element_3(Max, Max, []):-!.
max_element_3(Max, Max0, [X|L]):-
    X > Max0, !,
    max_element_3(Max, X, L).
max_element_3(Max, Max0, [_|L]):-
    max_element_3(Max, Max0, L).
% friange predicate would found Max_x and Max_y.
friange(Max_x,Max_y):-
    find_coord(_,_,Coordinate_x,Coordinate_y),
    max_element(Max_x,Coordinate_x),
    max_element(Max_y,Coordinate_y).
% s((X,Y),(X1,Y1),1) predicate is to 
% indicate the cost from land(X,Y) to land(X1,Y1).
% two positions are both lands.
% right walk
s((X,Y),(X1,Y1),1):-
    friange(Max_x,Max_y),
    X1 is X + 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max_x,
    Y1 =< Max_y,
    land_or_dropped(X,Y),
    land_or_dropped(X1,Y1).
% left walk
s((X,Y),(X1,Y1),1):-
        friange(Max_x,Max_y),
        X1 is X - 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        land_or_dropped(X,Y),
        land_or_dropped(X1,Y1).
% up walk
s((X,Y),(X1,Y1),1):-
    friange(Max_x,Max_y),
    X1 is X,
    Y1 is Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max_x,
    Y1 =< Max_y,
    land_or_dropped(X,Y),
    land_or_dropped(X1,Y1).
% down walk
s((X,Y),(X1,Y1),1):-
        friange(Max_x,Max_y),
        X1 is X,
        Y1 is Y - 1,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        land_or_dropped(X,Y),
        land_or_dropped(X1,Y1).
% s((X,Y),(X1,Y1),20) is to indicate the cost 
% from land(X,Y) to water(X1,Y1) or water(X1,Y1)
% to land(X,Y) or water(X,Y) to water(X1,Y1).
% one position is land and the other is water.
% left land to water
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X - 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        land_or_dropped(X,Y),
        not(land_or_dropped(X1,Y1)).
% left water to land
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X - 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        land_or_dropped(X1,Y1).

% left swim
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X - 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        not(land_or_dropped(X1,Y1)).

% right land to water
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X + 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        land_or_dropped(X,Y),
        not(land_or_dropped(X1,Y1)).
% right water to land
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X + 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        land_or_dropped(X1,Y1).
% right swim
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X + 1,
        Y1 is Y,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        not(land_or_dropped(X1,Y1)).
% up land to water
s((X,Y),(X1,Y1),20):-
    friange(Max_x,Max_y),
    X1 is X,
    Y1 is Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max_x,
    Y1 =< Max_y,
    land_or_dropped(X,Y),
    not(land_or_dropped(X1,Y1)).
% up water to land
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X,
        Y1 is Y + 1,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        land_or_dropped(X1,Y1).
% up swim
s((X,Y),(X1,Y1),20):-
    friange(Max_x,Max_y),
    X1 is X,
    Y1 is Y + 1,
    X1 > 0,
    Y1 > 0,
    X1 =< Max_x,
    Y1 =< Max_y,
    not(land_or_dropped(X,Y)),
    not(land_or_dropped(X1,Y1)).
% down land to water
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X,
        Y1 is Y - 1,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        land_or_dropped(X,Y),
        not(land_or_dropped(X1,Y1)).
% down water to land
s((X,Y),(X1,Y1),20):-
        friange(Max_x,Max_y),
        X1 is X,
        Y1 is Y - 1,
        X1 > 0,
        Y1 > 0,
        X1 =< Max_x,
        Y1 =< Max_y,
        not(land_or_dropped(X,Y)),
        land_or_dropped(X1,Y1).
% down swim
s((X,Y),(X1,Y1),20):-
    friange(Max_x,Max_y),
    X1 is X + 1,
    Y1 is Y,
    X1 > 0,
    Y1 > 0,
    X1 =< Max_x,
    Y1 =< Max_y,
    not(land_or_dropped(X,Y)),
    not(land_or_dropped(X1,Y1)).


    
% the final aim.
% goal((X,Y),1) only indicate the position of monster.
goal((X,Y),1):-
    monster(X,Y).
% goal((X,Y),2) indicate the goal(x,y) assigned.
% bb(X,Y) would be asserted and give the accurate aim position.
goal((X,Y),2):-
    bb(X,Y).

% put these pathes that would be to be dropped stone togather.
% if postion(x,y) is not on land_or_drop, it would be added into list
sway([(X,Y)|T],Res):-
    not(land_or_dropped(X,Y)),
    sway(T,Res1),
    Res=[(X,Y)|Res1].
% if position(x,y) is on land_or_drop, it would not be included.
sway([(X,Y)|T],Res):-
    land_or_dropped(X,Y),
    sway(T,Res).
% base case.
sway([],[]).
% if found several paths, this predicate would 
% only choose the least cost consumed.
first_result([H|_],H).
% this predicate would find the path reaching monster
% In this way, UCS algorithm would employed to find the 
% shortest path.
ini_path(Result):-
    agent_at(X,Y),
    findall(Sol,solve((X,Y),Sol,_,_,1),Result_1),
    first_result(Result_1,Result2),
    sway(Result2,Result).
% reverse the path list
reverse(List, Rev) :-
        reverse(List, Rev, []).
reverse([], L, L).
reverse([H|T], L, SoFar) :-
        reverse(T, L, [H|SoFar]).
% by reverse predicate, the path reaching monster
% would be reversed.
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

% initial_intentions(Intentions)
% this predicate would list the positions of water needing to be 
% dropped and the position of monster.
initial_intentions(Intentions):-
    intention_goal_path(Result),
    Intentions = intents(Result,[]).


% trigger(Percepts,Goal)
% this predicate would change stone(X,Y) into goal(X,Y).
trigger([],[]).

trigger([stone(X,Y)|T],Goal):-
    trigger(T,Goal2),
    Goal = [goal(X,Y)|Goal2].

%base case Goal become empty and Intentions1 would receive initial Intetions.
include_2([],Inten_list,Inten_list).
%case: goal(X,Y) is in initial intentions, it would be discared.
include_2([[goal(X,Y),_]|T],Inten_list,Intentions1):-
    member([goal(X,Y),_],Inten_list),
    include_2(T,Inten_list,Intentions1).

%case: goal(X,Y) is not in initial pick intentions so we add it into pick intentions.
include_2([[goal(X,Y),Plan]|T],Inten_list,Intentions1):-
        not(member([goal(X,Y),_],Inten_list)),
        include_2(T,Inten_list,Intensions11),
        Intentions1 = [[goal(X,Y),Plan]|Intensions11].

% base case
sorted_goals([],[]).
% this predicate would check whether goal(X,Y) at agent_at(X,Y),
% if it is at agent_at(X,Y), it would be thrown out pick plan list.
sorted_goals([[goal(X,Y),_]|T],Intentions1):-
        agent_at(X1,Y1),
        X1 = X,
        Y1 = Y,
        sorted_goals(T,Intentions1).
% if the goal is not at agent_at(x,y), it would be added into intentions1.
sorted_goals([[goal(X,Y),Plan]|T],Intentions1):-
        land_or_dropped(X,Y),
        agent_at(X1,Y1),
        assert(bb(X,Y)),
        findall(G,solve((X1,Y1),_,G,_,2),Cost_value),
        retractall(bb(X,Y)),
        first_result(Cost_value,Least_value),
        sorted_goals(T,Intentions2),
        Intentions1 = [[goal(X,Y),Plan,Least_value]|Intentions2].

        
% this predicate would ouput the result of sorted_goals.
include_goal(Goal,intents(Inten_list,G),Intentions1):-
    merge_include_goal(Goal,intents(Inten_list,G),Intent0),
    include_2(Intent0,G,Intentions2),
    sorted_goals(Intentions2,Intentions1).


% put new Goals with and original pick plans together. 
merge_include_goal(Goal,intents(_,G),Intentions1):-
    %................
    find_goal(Goal,Goal_Goal),
    i_goal(Goal_Goal,Goal1),
    append(G, Goal1, Intentions1).
% find_goal(new_goal,out_put goal)
% this predicate would find the each path from current position to each goal(X,Y)
% if this path only goes through land_or_dropped, it would be added into results
find_goal([goal(X,Y)|T],[goal(X,Y)|Res2]):-
    agent_at(X1,Y1),
    assert(bb(X,Y)),
    findall(Find_path,solve((X1,Y1),Find_path,_,_,2),Path_set),
    retractall(bb(X,Y)),
    first_result(Path_set,First_path),
    sway(First_path,Final_path),
    is_Empty(Final_path),
    find_goal(T,Res2).
% if goal(x,y) is at agent_at(x,y), it would be discarded.
find_goal([goal(X,Y)|T],Res2):-
    agent_at(X1,Y1),
    X1 = X,
    Y1 = Y,
    find_goal(T,Res2).
%base case
find_goal([],[]).
% if this path would go through water, this path would be discarded.
find_goal([goal(X,Y)|T],Res1):-
        agent_at(X1,Y1),
        assert(bb(X,Y)),
        findall(Find_path,solve((X1,Y1),Find_path,_,_,2),Path_set),
        retractall(bb(X,Y)),
        first_result(Path_set,First_path),
        sway(First_path,Final_path),
        not(is_Empty(Final_path)),
        find_goal(T,Res1).

%base case
i_goal([],[]).
% this predicate would add plan[] for each new goal(X,Y).
i_goal([H|T],Goal1):-
    i_goal(T,Goal2),
    append([[H,[]]],Goal2,Goal1).


% decreasing order of the length of the shortest valid path from the agent's current position.
min_order(Goal,Intentions,Sortedlist):-
    include_goal(Goal,Intentions,Intentions1),
    take(Intentions1,Result1,_),
    quick(Result1,Result3),
    compare_quick(Intentions1,Result3,Sortedlist).

% compare_quick([Drop|Pick],shuchu)
% if a new goal(X,Y) is same as the goal(X,Y) existing in Pick,
% this predicate would not add this new goal(X,Y) into Pick. otherwise,
% it would add it into Pick.

%base cae
compare_quick(_,[],[]).
% if a new goal(X,Y) is same as the goal(X,Y) existing in Pick,
% this predicate would not add this new goal(X,Y) into output list.
compare_quick([[A,B,C]|T],[P1|PT2],Shuchu):-
    P1 = A,
    append(T,[[A,B,C]],Ada),
    compare_quick(Ada,PT2,Shuchu2),
    Shuchu = [[A,B]|Shuchu2].

% if a new goal(X,Y) is not same as the goal(X,Y) existing in Pick,
% this predicate would add this new goal(X,Y) into output list.
compare_quick([[A,B,C]|T],[P1|PT2],Shuchu):-
    P1 \= A,
    append(T,[[A,B,C]],Ada),
    compare_quick(Ada,[P1|PT2],Shuchu).

% base case.
take([],[],[]).
% this predicate would take away the pick plan of goal(X,Y)
% this is because in quick predicate, the pick plan would 
% affect the output result.
take([[A,B,C]|T],Result1,Result2):-
    take(T,R1,R2),
    Result1 = [[A,C]|R1],
    Result2 = [B|R2].

% this predicate uses the quick sort algorithm to decreasing sort
% the (goal(X1,Y1),D), D is the cost of path from agent_at(X,Y)
% to goal(X1,Y1).
%base case.
quick([], []).
quick([[Aim1,Pivot]|Rest], SortedList) :-
    findall([Aim,N], (member([Aim,N],Rest), N =< Pivot), LessList),
   findall([Aim,N], (member([Aim,N],Rest), N > Pivot), GreaterList),
   quick(LessList, SortedLessList),
   quick(GreaterList, SortedGreaterList),
   append(SortedLessList, [Aim1|SortedGreaterList], SortedList).



% this predicate would identify whether the goal(x,y) 
% is on the agent_at(x,y). If it is on the agent_at(X,Y),
% this goal(x,y) would be discarded.
%base case.
panduan_stand([],[]).
% goal(x,y) is at agent_at(x,y), so the goal would be discarded.
panduan_stand([[goal(X1,Y1),_]|T],T):-
    agent_at(X,Y),
    X1 = X,
    Y1 = Y.
% goal(x,y) is not at agent_at(x,y) so this goal would be added into Sortedlist
panduan_stand([[goal(X1,Y1),N]|T],Sortedlist):-
    Sortedlist = [[goal(X1,Y1),N]|T].

%merge the intentions with new goals.
merge_goal(intents(H,_),Sorted,In1):-
    In1 = intents(H,Sorted).
% incorporate_goals(Goals,Intentions,Intentions1)
% this predicate would add the new [goal(X,Y),[]] occuring into the 
% intentions.
incorporate_goals(Goal,Intentions,Intentions1):-
    min_order(Goal,Intentions,Sortedlist_1),
    panduan_stand(Sortedlist_1,Sortedlist),
    merge_goal(Intentions,Sortedlist,Intentions1).

% this predicate check whether input list is empty.
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



paixu([_|T],T).
add_move(MA,[(X1,Y1)|T],X,Y,Result5):-
    X1 \= X,
    Y1 \= Y,
    add_move(MA,T,X,Y,Result6),
    Result5 = [move(X1,Y1)|Result6].
add_move(MA,[(X1,Y1)|T],X,Y,Result5):-
    X1 \= X,
    Y1 = Y,
    add_move(MA,T,X,Y,Result6),
    Result5 = [move(X1,Y1)|Result6].
add_move(MA,[(X1,Y1)|T],X,Y,Result5):-
    X1 = X,
    Y1 \= Y,
    add_move(MA,T,X,Y,Result6),
    Result5 = [move(X1,Y1)|Result6].
add_move(MA,[(X1,Y1)|[]],X,Y,Result5):-
    X1 = X,
    Y1 = Y,
    Result5=MA.


design_plan(MA,X1,Y1,X,Y,Subplan):-
    assert(bb(X,Y)),
    findall(Sol,solve((X1,Y1),Sol,_,_,2),Result_1),
    retractall(bb(X,Y)),
    first_result(Result_1,Result2),
    reverse(Result2,Result3),
    paixu(Result3,Result4),
    add_move(MA,Result4,X,Y,Subplan).



update_intentions(at(_,_), Intentions2, Intentions2).
update_intentions(dropped(_,_), intents([_|Init_drop],Init_pick), intents(Init_drop,Init_pick)).
update_intentions(picked(_,_),intents(Drop_plan,[_|Init_pick]),intents(Drop_plan,Init_pick)).

