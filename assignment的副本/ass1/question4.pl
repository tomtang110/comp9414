
%second1 function   get daxiao()
compare([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1 -1,
    X=H,
    compare(T,T,L_c,P),
    Littlelist=[H|L_c].

compare([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X \=H,
    Littlelist=[H],
    P=T.
compare([H|T],[_],Littlelist,P):-
    T = [],
    Littlelist=[H],
    P=[].
% second2 function get daxiao()
feikuang([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X \= H,
    feikuang(T,T,L_c,P),
    Littlelist=[H|L_c].
feikuang([H|T],[_,H1|_],Littlelist,P):-
    T \= [],
    X is H1-1,
    X = H,
    Littlelist=[H],
    P=T.
feikuang([H|T],[_],Littlelist,P):-
    T = [],
    Littlelist=[H],
    P=[].
% third fuction  get compare()
finalcompare([Hfc|Tfc],Firsth,Filist):-
    Firsth=Hfc,
    Filist=Tfc.
% fifth function
xunhuan([_|Tfen],P1):-
    Tfen \= [],
    xunhuan(Tfen,P1).
xunhuan([Hfen|Tfen],P1):-
    Tfen = [],
    P1=[Hfen].
% fourth function get finalcompare()
fenkai(Filist,Firsth,Rfinal):-
    xunhuan(Filist,P1),    
    Rfinal=[Firsth|P1].
%sixth function
judge([Head|_],[_,Head1|_]):-
    1 is Head1-Head.
% Last function
kuang(List,Kuanglist,Position):-
    compare(List,List,Littlelist,P),
    finalcompare(Littlelist,Firsth,Filist),
    fenkai(Filist,Firsth,Rfinal),
    Kuanglist=Rfinal,
    Position=P.
nokuang(List,Noklist,Position):-
    feikuang(List,List,Littlelist,P),
    Noklist=Littlelist,
    Position=P.
% mian function
chop_up(List,Newlist):-
    List \= [],
    judge(List,List),
    kuang(List,Kuanglist,Position),
    chop_up(Position,Y),
    append([Kuanglist],Y,Newlist).
chop_up(List,Newlist):-
    List \= [],
    not(judge(List,List)),
    nokuang(List,Noklist,Position),
    chop_up(Position,Y),
    append(Noklist,Y,Newlist).
chop_up(List,Newlist):-
    List=[],
    Newlist=[].





    