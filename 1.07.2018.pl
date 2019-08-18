not(Goal) :- \+ call(Goal).
% 1.07.2018г.

/* Да се дефинира на пролог предикат p(X,Y,A,B), който по даден списък Х и
 * положителни естествени числа А и В, генерира в Y всички възможни списъци от
 * списъци, такива че Х е конкатенация на елементите на Y и броят на елементите на
 * всеки от елементите на Y е четно число, което е по-малко от А и не по-малко от B ->[B,A).
 */

my_append([],B,B).
my_append([H|T],B,[H|R]) :- my_append(T,B,R).

my_member(X,[X|_]).
my_member(X,[H|T]) :- my_member(X,T).

len([],0).
len([_|T],N) :- len(T,M), N is M + 1.

split([],[]).
split(L,[A|SB]) :- my_append(A,B,L), A \= [], split(B,SB).

condition(N,A,B) :- N mod 2 =:= 0, N < A, N >= B.

p(X,Y,A,B) :- split(X,Y), not((my_member(Z,Y), len(Z,N), not(condition(N,A,B)))).

/* Нека L = [a1,a2,...,an] и M = [b1,b2,...,bk] са списъци от естествени числа.
 * Казваме, че списъкът L покрива списъка M, ако съществува такава редица
 * от естествени числа i1,...ik, че 1 =< i1 < i2 < ... < in = n и
 * b1 =< ai1, b2 =< bi2, ..., bk =< aik. Да се дефинира на пролог двуместен
 * предикат q, който по даден списък от списъци от естествени числа К
 * и списък от естествени числа М разпонзава дали списъка М покрива всеки елемент на К.
 */


sublist(_,0,[]).
sublist([H|T],N,[H|R]) :- N > 0, N1 is N - 1, sublist(T,N1,R).
sublist([H|T],N,R) :- N > 0, sublist(T,N,R).

coverHelp([],[]).
coverHelp([H1|T1],[H2|T2]) :- H2 =< H1, coverHelp(T1,T2).

cover(L,M) :- len(M,K), sublist(L,K,S), coverHelp(S,M).

q(K,M) :- not(my_member(X,K), not(cover(M,X))).
