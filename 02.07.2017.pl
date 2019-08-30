not(Goal) :- \+ call(Goal).

/* Списък от тройки [X,Y,W] интерпретираме като граф с тежести по ребрата. Всяка
 * тройка представлява ребро от Х до Y с тежест W (тежестта е цяло число). Да се
 * дефинира предикат p(G,L), който по даден списък от тройки G генерира при
 * преудоволетворяване в L всички ациклични пътища (последователности от върхове)
 * в съответния граф, които имат нулева сума на тежестите по ребрата от пътя.
 */

my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

my_reverse(L,RL) :- my_reverse(L,[],RL).
my_reverse([],Agg,Agg).
my_reverse([H|T],Agg,RL) :- my_reverse(T,[H|Agg],RL).

addIfNotPresented(X,L,L) :- my_member(X,L).
addIfNotPresented(X,L,[X|L]) :- not(my_member(X,L)).

extractVertices([],[]).
extractVertices([[X,Y,_]|T],V) :- extractVertices(T,V1), addIfNotPresented(X,V1,V2),
                                      addIfNotPresented(Y,V2,V).

nextVertice(G,X,Stack,Visited,Next,W) :- my_member([X,Next,W],G), not(my_member(Next,Stack)),
                                            not(my_member(Next,Visited)).

path(G,X,Y,P,S) :- extractVertices(G,V), my_member(X,V), my_member(Y,V),
                                            path(G,X,Y,[],[],P,0,S).

path(G,Y,Y,Stack,_,P,SAgg,SAgg) :- my_reverse([Y|Stack],P).
path(G,X,Y,Stack,Visited,P,SAgg,S) :- nextVertice(G,X,[X|Stack],Visited,Next,W),
                                      SAgg1 is SAgg + W,
                                      path(G,Next,Y,[X|Stack],Visited,P,SAgg1,S).
path(G,X,Y,[StackH|StackT],Visited,P,SAgg,S) :- 
                                      not(nextVertice(G,X,[X|[StackH|StackT]],Visited,Next,W)),
                                      path(G,StackH,Y,StackT,[X|Visited],P,SAgg,S).

p(G,L) :- extractVertices(G,V), my_member(X,V), my_member(Y,V), X \= Y,
                                          path(G,X,Y,L,S), S =:= 0.

% [[x,y,2],[y,z,-2],[z,p,-3],[p,x,3]]

/* Потенциал на елемент на списък от цели числа ще наричаме произведението на всички по-големи
 * от него елементи, които са преди него в списъка. Да се напише предикат p(L,X), който
 * по даден списък от цели числа, генерира с преудоволетворяване в Х елементите на L с
 * най-голям потенциал.
 */

potential(L,X,N) :- my_member(X,L), potential(L,X,1,N).
potential([X|T],X,Agg,Agg).
potential([H|T],X,Agg,N) :- H > X, Agg1 is Agg * H, potential(T,X,Agg1,N).
potential([H|T],X,Agg,N) :- H < X, potential(T,X,Agg,N).

q(L,X) :- my_member(X,L), potential(L,X,N), not((my_member(Y,L), potential(L,Y,N1), N1 > N)).
