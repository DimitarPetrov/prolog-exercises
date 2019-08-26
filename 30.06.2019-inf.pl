not(Goal) :- \+ call(Goal).

/* Нека за всяко положително число i с ξ(i) и с η(i) означим съответно броя
 * на простите числа от вида 6k + 1 и 6k + 5, които са по-малки от i.
 * Да се дефинират на пролог еднометсни предикати su(X) и mu(X), които
 * по дадено цяло число X разпознават дали за някое положително цяло
 * число i е в сила равенството X = i+ξ(i) за su(X) и X = i−η(i) за mu(X).
 */

ksi(I,R) :- ksiAgg(I,0,1,0,R).
n(I,R) :- ksiAgg(I,0,5,0,R).

ksiAgg(Max,K,Var,Agg,Agg) :- (6 * K + Var) >= Max.
ksiAgg(Max,K,Var,Agg,R) :- X is (6 * K + Var), X < Max, isPrime(X),
                                  Agg1 is Agg + 1, K1 is K + 1, ksiAgg(Max,K1,Var,Agg1,R).
ksiAgg(Max,K,Var,Agg,R) :- X is (6 * K + Var), X < Max, not(isPrime(X)),
                                  K1 is K + 1, ksiAgg(Max,K1,Var,Agg,R).

isPrime(X) :- isPrimeAgg(X,2).
isPrimeAgg(X,X).
isPrimeAgg(X,N) :- N < X, X mod N =\= 0, N1 is N + 1, isPrimeAgg(X,N1).

su(X) :- suAgg(X,0).
suAgg(X,I) :- I =< X, ksi(I,R), I + R =:= X.
suAgg(X,I) :- I =< X, ksi(I,R), I + R =\= X, I1 is I + 1, suAgg(X,I1).

mu(X) :- muAgg(X,0).
muAgg(X,I) :- I =< X*X, n(I,R), I - R =:= X.
muAgg(X,I) :- I =< X*X, n(I,R), I - R =\= X, I1 is I + 1, suAgg(X,I1).

/* G-списък ще наричаме списък, всички елементи, на който са двуместни списъци от
 * естествени числа. Нека L е G-списък. За всяко естествено
 * число k с do(L, K) да означим броя на онези елементи на L, чийто първи
 * елемент k ,а с di(L, k) да означим броя на онези елемнти от L, чийто
 * втори елемент е k.
 * Да се дефинират на пролог еднометсни предикати e1g(L) и e2g(L), които
 * при преудовлетворяване генерират в L всички G-списъци, такива че за
 * всяко естествено число k е в сила неравенството:
 * |do(L, k) − di(L, k)| ≤ j, j ∈ {1 → e1g, 2 → e2g}
 */

nat(0).
nat(N) :- nat(M), N is M + 1.

my_between(A,B,A) :- A =< B.
my_between(A,B,C) :- A < B, A1 is A + 1, my_between(A1,B,C).

genKS(0,0,[]).
genKS(1,S,[S]).
genKS(K,S,[H|R]) :- K > 1, K1 is K - 1, my_between(0,S,H), S1 is S - H, genKS(K1,S1,R). 

my_length([],0).
my_length([_|T],N) :- my_length(T,M), N is M + 1.

packPairs([],[]).
packPairs([H1,H2|T],[[H1,H2]|R]) :- packPairs(T,R).

my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

max(L,X) :- my_member(X,L), not((my_member(Y,L), Y > X)).

genGlist(X,Max) :- nat(N), my_between(0,N,A), B is N - A, genKS(A,B,L), length(L,Len),
                Len mod 2 =:= 0, max(L,Max), packPairs(L,X).

do([],_,0).
do([[K,H2]|T],K,N) :- do(T,K,M), N is M + 1.
do([[H1,H2]|T],K,N) :- H1 \= K, do(T,K,N).

di([],_,0).
di([[H1,K]|T],K,N) :- di(T,K,M), N is M + 1.
di([[H1,H2]|T],K,N) :- H2 \= K, di(T,K,N).

condition(L,Max) :- conditionAgg(L,0,Max).
conditionAgg(L,Agg,Max) :- Agg > Max.
conditionAgg(L,Agg,Max) :- Agg =< Max, do(L,Agg,A), di(L,Agg,B), abs(A-B) =< 1,
                            Agg1 is Agg + 1, conditionAgg(L,Agg1,Max).

e1g([]).
e1g(L) :- genGlist(L,Max), condition(L,Max).
