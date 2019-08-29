not(Goal) :- \+ call(Goal).

/* Ще използваме списъци от символи, за да представяме думи. Да се дефинира на пролог
 * предикат p(A,B,C,K), който по дадени думи А,B и C проверява дали думата К може да се
 * представи като конкатенация на от вида wC, където думата w е или равна на C, или е
 * конкатенация на четен брой думи, всяка от които е равна на А или B. Забележка
 * условието за K може да се запише като следния регулярен израз: (C⋃((A⋃B)(A⋃B))*)C
 */

my_between(A,B,A) :- A =< B.
my_between(A,B,C) :- A < B, A1 is A + 1, my_between(A1,B,C).

my_append([],B,B).
my_append([H|T],B,[H|R]) :- my_append(T,B,R).

my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

my_length([],0).
my_length([_|T],N) :- my_length(T,M), N is M + 1.

genList(_,_,0,Agg,Agg).
genList(A,B,N,Agg,R) :- N > 0, my_member(X,[A,B]), N1 is N - 1, my_append(Agg,X,Agg1),
                              genList(A,B,N1,Agg1,R).

genEvenLengthListOfAandBSmallerThanN(A,B,N,L) :- my_between(0,N,C), C mod 2 =:= 0,
                                                genList(A,B,C,[],L).

equalLists([],[]).
equalLists([H|T1],[H|T2]) :- equalLists(T1,T2).

p(A,B,C,K) :- my_append(C,C,K).
p(A,B,C,K) :- my_length(K,N), genEvenLengthListOfAandBSmallerThanN(A,B,N,W),
                    my_append(W,C,K1), equalLists(K,K1).

/* Записваме безкванторни формули посредством списъци, в които всяка подформула се
 * записва в отделен списък, който се състои от основната логическа операция в
 * подформулата и нейните аргументи. Списъците се записват в инфиксен стил, като
 * атомарните формули се записват с номера. Например формулата (f1 ∨ ¬f2) ∧ (f2 ⇒ f4)
 * се записва като списъка [[f1,∨,[¬,f2]],∧,[f2,⇒,f4]]. Да се дефинира на пролог
 * предикат s(F,N), където F е формула с атомарни формули f1,f2,...,fn записани като
 * списък, а N е списък от n стойности t или f. Предикатът трябва да проверява дали
 * записаната с F формула е вярна, ако първият елемент на N е стойността на f1, вторият
 * на f2 и т.н.
 */

isList([]).
isList([_|_]).

getNthElem([H|T],0,H).
getNthElem([_|T],N,R) :- N > 0, N1 is N - 1, getNthElem(T,N1,R).

s(X,N) :- not(isList(X)), getNthElem(N,X,R), R = t.
s([X],N) :- not(isList(X)), getNthElem(N,X,R), R = t.
s([not,T],N) :- not(s(T,N)).
s([H,or,T],N) :- s(H,N); s(T,N).
s([H,and,T],N) :- s(H,N), s(T,N).
s([H,implication,T],N) :- not(s(H,N)); s(T,N).
s([H,equivalence,T],N) :- ((s(H,N),s(T,N)); (not(s(H,N)),not(s(T,N)))).
              
