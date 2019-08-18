not(Goal) :- \+ call(Goal).

% 31.08.2018г.

/*
 * Да се дефинира на пролог предикат q(X), който при преудовлетворяване
 * генерира в X всички списъци, които представляват крайни:
 * I.1 аритметични прогресии от съставни естесвени числа.
 * I.2 геометрични прогресии, нито един член на които не е квадрат на естесвени число.
*/

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
 *I.1 Да се дефинират на пролог едноместни предикати p1, p2, p3 и p4.
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

% 24.01.2018г.

/* Диаметър на списък наричаме разликата между броя срещания на най-често
 * срещание елемнт на списъка и броя срещания на най-рядко срещания
 * елемент на списъка. 
 *
 * a) Да се дефинира на prolog едноместен предикат p,
 * който по даден списък от списъци L разпознава дали всички елементи на
 * L имат един и същи диаметър.
 *
 * b) Да се дефинира на prolog едноместен предикат q,
 * който по даден списък от списъци L разпознава дали има елемент на L,
 * чийто диаметър е различен от диаметъра на всеки друг елемент на L.
*/

numOfOccurrances(_,[],0).
numOfOccurrances(X,[X|T],N) :- numOfOccurrances(X,T,M), N is M + 1.
numOfOccurrances(X,[H|T],N) :- X \= H, numOfOccurrances(X,T,N).

mostCommon(L,X) :- my_member(X,L), numOfOccurrances(X,L,N1), not((my_member(Y,L), numOfOccurrances(Y,L,N2), N2 > N1)).

leastCommon(L,X) :- my_member(X,L), numOfOccurrances(X,L,N1), not((my_member(Y,L), numOfOccurrances(Y,L,N2), N2 < N1)).

diameter(L,X) :- mostCommon(L,Y), numOfOccurrances(Y,L,N1),  leastCommon(L,Z), numOfOccurrances(Z,L,N2), X is N1 - N2.

p([H|T]) :- diameter(H,X), not((my_member(Y,L), diameter(Y,X2), X =\= X2)).

q(L) :- my_member(X,L), diameter(X,D1), not((my_member(Y,L), Y \= X, diameter(Y,D2), D1 =:= D2)).

/* Фенски списък е краен списък, всеки елемент на който е някоя от буквите 1, 2
 * или е фенски списък, като никои два съседни елемента не са еднакви букви.
 * Например [1,[2,1],2,1,[],[[1,2],[1,2],2,1,2],1] е фенски списък, а
 * [1,[2,1],2,[],[[1,2],[2,2],2,1,2],1] не е фенски списък.
 * Да се дефинира на пролог едноместен предикат p(X), който при 
 * преудоволетворяване генерира в Х всички фенски списъци,
 * които се записват на пролог с краен брой "]".
*/

split([],[]).
split(L,[A|SB]) :- my_append(A,B,L), A \= [], split(B,SB).

splitN(0,L,L).
splitN(N,L,SL) :- N > 0, N1 is N - 1, splitN(N1,L,X), split(X,SL).

genFenskiList(0,[]).
genFenskiList(N,[E|X]) :- N > 0, my_member(E,[1,2,[]]), N1 is N - 1, genFenskiList(N1,X).

my_flatten([],[]).
my_flatten(X,[X]) :- not(isList(X)).
my_flatten([H|T],FL) :- my_flatten(H,FH), my_flatten(T,FT), my_append(FH,FT,FL).

notEqualNeighbours([]).
notEqualNeighbours([_]).
notEqualNeighbours([H1,H2|T]) :- H1 \= H2, notEqualNeighbours([H2|T]).

fen(X) :- nat(N), myBetween(0,N,A), B is N - A, genFenskiList(A,X1), splitN(B,X1,X),
              my_flatten(X,FX),notEqualNeighbours(FX).

