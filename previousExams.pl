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

genKS(1,Sum,[Sum]).
genKS(Len,Sum,[N|X]) :- Len > 1, NewLen is Len - 1, myBetween(0,Sum,N), NewSum is Sum - N,
                          genList(NewLen,NewSum,X).

genArithmeticProgression(_,0,_,[]).
genArithmeticProgression(Current,Len,D,[Current|R]) :- Len > 0, NewLen is Len - 1, NewCurrent is Current + D, genArithmeticProgression(NewCurrent,NewLen,D,R).

q1([]).
q1(X) :- nat(N), genKS(3,N,[Start,Len,D]), Len > 0, D > 0, 
                  genArithmeticProgression(Start,Len,D,X), not((member(E,X), prime(E))).

isNotSquare(X) :- isNotSquare(X,0).
isNotSquare(X,X) :- X =\= 0, X =\= 1.
isNotSquare(X,I) :- I < X, I1 is I + 1, X =\= I * I, isNotSquare(X,I1).

genGeometricProgression(_,0,_,[]).
genGeometricProgression(Start,Len,Q,[Start|X]) :- Len > 0, NewLen is Len - 1, NewStart is Start * Q, genGeometricProgression(NewStart,NewLen,Q,X).

q2([]).
q2(X) :- nat(N), genKS(3,N,[Start,Len,Q]), Len > 0, Q > 1, genGeometricProgression(Start,Len,Q,X), not((member(E,X), not(isNotSquare(E)))).

/*
* I.1 Да се дефинират на пролог едноместни предикати p1, p2, p3 и p4.
* такива че даден списък X:
* (1) p1 разпознава дали празният списък е елемент на X,
* (2) p2 разпознава дали X съдържа елементи Y и Z, такива че не
* всички елементи на Y са елементи на Z,
* (3) p3 разпознава дали X съдържа елемент Y, който съдържа всички
* елемeнти на всички елементи на X,
* (4) p4 разпознава дали за всеки елемент Y на X съществува такъв
* елемент Z на X, че не всички елементи на Z са елементи на Y.
*/

my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

p1(X) :- my_member([],X).

isSubsetOf(A,B) :- not((my_member(X,A), not((my_member(X,B))))).

p2(X) :- my_member(Y,X), my_member(Z,X), not(isSubsetOf(Y,Z)).

isList([]).
isList([_|_]).

elementsOf(X,[X]) :- not(isList(X)).
elementsOf(X,X) :- isList(X).

my_append([],B,B).
my_append([H|T],B,[H|R]) :- my_append(T,B,R).

elementsOfelementsOf([],[]).
elementsOfelementsOf([H|T],L) :- elementsOf(H,E), elementsOfelementsOf(T,E2), my_append(E,E2,L). 


p3(X) :- member(Y,X), elementsOfelementsOf(X,EX), isSubsetOf(EX,Y).
% or p3(L) :− member(Y,L), not((member(Z,L),not(isSubsetOf(Z,Y)))).

p4(X) :- not((my_member(Y,X), not((my_member(Z,X), not(isSubsetOf(Z,Y)))))).

/*
* I.2 Да се дефинират на пролог едноместни предикати q1, q2, q3 и q4.
* такива че даден списък X:
* (1) q1 разпознава дали празният списък е елемент на X,
* (2) q2 разпознава дали X съдържа елементи Y и Z, които нямат общи
* елементи,
* (3) q3 разпознава дали X съдържа елемент Y, чиито елементи са
* еленти на всички елемeнти на X,
* (4) q4 разпознава дали за всеки елемент Y на X съществува такъв
* елемент Z на X, че Y и Z нямат общи елемeнти.
*/

noCommonElements(A,B) :- not(my_member(X,A), my_member(X,B)).

q2(X) :- my_member(Y,X), my_member(Z,X), noCommonElements(Y,Z).

q3(X) :- my_member(Y,X), not((my_member(Z,X), not(isSubsetOf(Y,Z)))).

q4(X) :- not((my_member(Y,X), not(my_member(Z,X), noCommonElements(Y,Z)))).



