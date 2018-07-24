likes(mary, apple).
likes(mary, pear).
likes(mary, grapes).
likes(tim, mango).
likes(tim, apple).
likes(jane, apple).
likes(jane, mango).



car(Fruitlist,[H2|T2]):-
    member(H2,Fruitlist),
    car(Fruitlist,T2).
car(_,[]).


all_like_all([Head|Tail],What_list):-
    findall(Fruit,likes(Head,Fruit),Fruitlist),
    car(Fruitlist,What_list),
    all_like_all(Tail,What_list).
all_like_all([],_).





    