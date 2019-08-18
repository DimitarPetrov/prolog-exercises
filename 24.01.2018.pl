not(Goal) :- \+ call(Goal).
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
