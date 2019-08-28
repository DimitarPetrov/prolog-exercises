not(Goal) :- \+ call(Goal).

/* Ще казваме, че списък от цифри [a1,a2,...,an] представлява числото, което 
 * записано в десетична бройна система изглежда a1a2a3...an. Например списъкът
 * [1,2,3], представлява числото 123. Без да се използва аритметика на Пролог
 * за числа, по-големи от 20, да се дефинира триместен предикат p(X,Y,Z), който
 * по дадени списъци от цифри X и Y намира в Z представяне на сбора на числата,
 * представени със списъците X и Y.
 */

my_length([],0).
my_length([_|T],N) :- my_length(T,M), N is M + 1.

addNZeroes(0,L,L).
addNZeroes(N,L,[0|R]) :- N > 0, N1 is N - 1, addNZeroes(N1,L,R).

equalLengths(X,Y,X,Y1) :- my_length(X,N1), my_length(Y,N2), N1 > N2, Diff is N1 - N2,
                            addNZeroes(Diff,Y,Y1).           
equalLengths(X,Y,X1,Y) :- my_length(X,N1), my_length(Y,N2), N1 < N2, Diff is N2 - N1,
                            addNZeroes(Diff,X,X1).
equalLengths(X,Y,X,Y) :- my_length(X,N1), my_length(Y,N2), N1 =:= N2.

my_reverse([],Agg,Agg).
my_reverse([H|T],Agg,R) :- my_reverse(T,[H|Agg],R).

my_reverse(L,RL) :- my_reverse(L,[],RL).

sum(X,Y,Z) :- sum(X,Y,0,Z).

sum([],[],0,[]).
sum([],[],P,[P]).
sum([H1|T1],[H2|T2],P,[R2|Z]) :- sumDigits(H1,H2,P,[R1,R2]), sum(T1,T2,R1,Z).

sumDigits(H1,H2,P,[R1,R2]) :- S is H1 + H2 + P, R2 is S mod 10, R1 is S div 10.

p(X,Y,Z) :- equalLengths(X,Y,X1,Y1), my_reverse(X1,RX), my_reverse(Y1,RY), sum(RX,RY,RZ),
                      my_reverse(RZ,Z).

/* Ще казваме, че един списък е кодичен, ако първият му елемент е 0 или 1,
 * последният е 2 и всички останали елементи на списъка са кодични списъци.
 * Ще казваме, че един списък от нули, еденици и двойки X е кодирането на даден
 * кодичен списък Y, ако при изтриването на квадратните скоби в записа на елементите
 * на кодичния списък Y се получава Х. Например списъкът [1,0,1,2,1,0,2,2,2,0,2,2]
 * е кодирането на кодичния списък [1,[0,[1,2],[1,[0,2],2],2],[0,2],2] да се дефинира
 * на пролог предиката q(X,Y), който по дадено кодиране Х намира кодичния списък Y,
 * чието кодиране е Х.
 */

my_reverse(L,RL) :- my_reverse(L,[],RL).

my_reverse([],Agg,Agg).
my_reverse([H|T],Agg,RL) :- my_reverse(T,[H|Agg],RL).

q(X,Y) :- q(X,[],Y).

q([2],[Stack],R) :- my_reverse([2|Stack],R).
q([1|T],Stack,Y) :- q(T,[[1]|Stack],Y).
q([0|T],Stack,Y) :- q(T,[[0]|Stack],Y).
q([2|T],[H,H2|StackT],Y) :- my_reverse([2|H],R), q(T,[[R|H2]|StackT],Y).
