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
