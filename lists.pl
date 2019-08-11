tran(eins,one).
tran(zwei,two).
tran(drei,three).
tran(vier,four).
tran(fuenf,five).
tran(sechs,six).
tran(sieben,seven).
tran(acht,eight).
tran(neun,nine).

listtran([],[]).
listtran([H|T],[R|RT]) :- tran(H,R), listtran(T,RT).

%---------------------------------------

twice([],[]).
twice([H|T], [H,H|R]) :- twice(T,R).

%--------------------------------------

combine1([H|T],[],[H|T]).
combine1([H1|T1],L2,[H1|R]) :- combine1(L2,T1,R).

%--------------------------------------

combine2([],[],[]).
combine2([H1|T1], [H2|T2], [[H1,H2]|R]) :- combine2(T1,T2,R).

%--------------------------------------

combine3([],[],[]).
combine3([H1|T1], [H2|T2], [join(H1,H2)|R]) :- combine3(T1,T2,R).

%-------------------------------------

not(Goal) :- \+ call(Goal).

mysubset(L1,L2) :- not((member(X,L1), not(member(X,L2)))).
mysuperset(L1,L2) :- mysubset(L2,L1).

mysubset2([],_).
mysubset2([H|T],L2) :- member(H,L2), mysubset2(T,L2).

%-------------------------------------

myAppend([],L,L).
myAppend([H|T],L2,[H|R]) :- myAppend(T,L2,R).

doubled(L) :- myAppend(X,X,L).

%-------------------------------------

myReverse([],[]).
myReverse([H|T],R) :- reverse(T,R1), myAppend(R1,[H],R).

palindrome(L) :- myReverse(L,L).

%-------------------------------------

second(X,[_,X|_]).

swap12([H1,H2|T],[H2,H1|T]).

final(X,L) :- myReverse(L,RL), RL = [X|_].

toptail(L1,L2) :- myAppend([_|L2],[_],L1).

swapf1(L1,L2) :- myAppend([First|M],[Last],L1), myAppend([Last|M],[First],L2).

%-------------------------------------

zebra_owner(ZebraOwner) :-
    Street = [_, _, _],
    member(house(red, englishman, _), Street),
    member(house(_, spanish, jaguar), Street),
    member(house(_, ZebraOwner, zebra), Street),
    sublist([house(_, _, snail), house(_, japanese, _)], Street),
    sublist([house(_, _, snail), house(blue, _, _)], Street).

%-------------------------------------

myMember(X,L) :- myAppend(_,[X|_],L).

%------------------------------------

set([],[]).
set([H|T],[H|R]) :- set(T,R), not(member(H,R)).
set([H|T],R) :- set(T,R), member(H,R).

%------------------------------------

isList([]).
isList([_|_]).

myFlatten([],[]).
myFlatten(X,[X]) :- not(isList(X)).
myFlatten([H|T],R) :- myFlatten(H,Hr), myFlatten(T,Tr), myAppend(Hr,Tr,R).

myFlatten2(L,R) :- flattenAgg(L,[],R).

flattenAgg([],A,A).
flattenAgg(X,A,[X|A]) :- not(isList(X)).
flattenAgg([H|T],A,R) :- flattenAgg(T,A,Ra), flattenAgg(H,Ra,R).
