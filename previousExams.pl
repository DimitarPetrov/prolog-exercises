not(Goal) :- \+ call(Goal).

%31.08.2018
%Да се дефинира на пролог предикат q(X), който при преудовлетворяване
%генерира в X всички списъци, които представляват крайни:
%I.1 аритметични прогресии от съставни естесвени числа.
%I.2 геометрични прогресии, нито един член на които не е квадрат на естесвени число.

nat(0).
nat(N) :- nat(M), N is M + 1.

prime(X) :- primeHelper(X,2).
primeHelper(X,I) :- I < X, X mod I =\= 0, Y is I + 1, primeHelper(X,Y).
primeHelper(X,X).

myBetween(A,B,A) :- A =< B.
myBetween(A,B,C) :- A < B, A1 is A + 1, myBetween(A1,B,C).

genList(1,Sum,[Sum]).
genList(Len,Sum,[N|X]) :- Len > 1, NewLen is Len - 1, myBetween(0,Sum,N), NewSum is Sum - N,
                          genList(NewLen,NewSum,X).

genArithmeticProgression(_,0,_,[]).
genArithmeticProgression(Current,Len,D,[Current|R]) :- Len > 0, NewLen is Len - 1, NewCurrent is Current + D, genArithmeticProgression(NewCurrent,NewLen,D,R).

q1([]).
q1(X) :- nat(N), genList(3,N,[Start,Len,D]), Len > 0, D > 0, 
                  genArithmeticProgression(Start,Len,D,X), not((member(E,X), prime(E))).

