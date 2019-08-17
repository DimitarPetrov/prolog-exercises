not(Goal) :- \+ call(Goal).

% Definition of list
isList([]).
isList([_|_]).

% append(L1, L2, L3) where L3 = L1 + L2
my_append([],B,B).
my_append([H|T],B,[H|R]) :- my_append(T,B,R).

% Is X a member of the list L
my_member(X,[X|_]).
my_member(X,[_|T]) :- my_member(X,T).

% Insert X in the list L arbitrary
insert(X,L,R) :- my_append(A,B,L), my_append(A,[X|B],R).

% Remove X in the list L (Only from one position, if X is contained more than once in L)
remove(X,L,R) :- my_append(A,[X|B],L), my_append(A,B,R).

% Permute the list L -> TODO: !!!
perm([], []).
perm([H|T], P):- permutation(T, Q), insert(H, Q, P).

% Check if sorted
sorted([]).
sorted([_]).
sorted([H1,H2|T]) :- H1 =< H2, sorted([H2|T]).

% The slowest sort algorithm, but valid
random_sort(L,SL) :- perm(L,SL), sorted(SL).

% Length of list
my_length(0,[]).
my_length(N,[_|T]) :- my_length(M,T), N is M + 1.

% Minimal/Maximal element of list
my_max(X,L) :- my_member(X,L), not((my_member(Y,L), Y > X)).
my_min(X,L) :- my_member(X,L), not((my_member(Y,L), Y < X)).

% Prefix and suffix of list
my_prefix(X,L) :- my_append(X,_,L).
my_suffix(X,L) :- my_append(_,X,L).

% Sublist of list(a slice/chunk of the list)
subl(L,SL) :- my_suffix(S,L), my_prefix(SL,S).

% Subset of L
subs([],[]).
subs([_|T],SL) :- subs(T,SL).
subs([H|T],[H|SL]) :- subs(T,SL).

% Get element at position N
getByIndex([H|_],0,H).
getByIndex([_|T],N,X) :- getByIndex(T,M,X), N is M + 1.

% Reverse of lists
my_reverse(L,RL) :- reverseAgg(L,[],RL).
reverseAgg([],RL,RL).
reverseAgg([H|T],Agg,RL) :- reverseAgg(T,[H|Agg],RL).

% Erase duplicates in list L
unique([],[]).
unique([H|T],[H|UL]) :- unique(T,UL), not(my_member(H,UL)).
unique([H|T],UL) :- unique(T,UL), my_member(H,UL). 

% Given the set of ribs E, get V the set of vertices
insertIfNotPresented(X,L,[X|L]) :- not((my_member(X,L))).
insertIfNotPresented(X,L,L) :- my_member(X,L).

vertices([],[]).
vertices([[A,B]|T],V) :- vertices(T,V1), insertIfNotPresented(A,V1,V2), insertIfNotPresented(B,V2,V).

% Find an acyclic path in the graph
next(E,Current,Visited,Next) :- my_member([Current,Next],E), not(my_member(Next,Visited)).

path(E,A,B,Path) :- path(E,A,B,[],Path).

path(E,X,X,_,[X]) :- vertices(E,V), my_member(X,V).
path(E,A,B,Visited,[A|Path]) :- next(E,A,Visited,Next), path(E,Next,B,[A|Visited],Path).

% When is there a cycle in the graph?
cycle(E,C) :- path(E,X,Y,P), my_member([Y,X],E), my_append(P,[X],C).

% When is the graph connected?
connected(E) :- vertices(E,V), not((my_member(X,V), my_member(Y,V),  not(path(E,X,Y,_)))).

% Generate numbers in interval [A, B].
my_between(A,B,A) :- A =< B.
my_between(A,B,R) :- A < B, N is A + 1, between(N,B,R).

% Split L in two subsets
splitInTwo([],[],[]).
splitInTwo([H|T],[H|L],R) :- splitInTwo(T,L,R).
splitInTwo([H|T],L,[H|R]) :- splitInTwo(T,L,R).

% Generate natural numbers
nat(0).
nat(N) :- nat(M), N is M + 1.

% Generate integers
int(0).
int(X) :- nat(Y), Y > 0, (X is Y; X is -Y).

% Generate a list of numbers in interval [A, B]
range(A, A, [A]).
range(A, B, [A|R]):- A < B, A1 is A + 1, range(A1, B, R).

listBetween(A,B,R) :- nat(N), listBetween(N,A,B,R).

listBetween(0,_,_,[]).
listBetween(N,A,B,[C|L]) :- N > 0, N1 is N - 1, my_between(A,B,C), listBetween(N1,A,B,L).

% Generate a pai of naturals
pair(X,Y) :- nat(N), my_between(0,N,X), Y is N - X.

% Generate K numbeers with sum S
genKS(1,S,[S]).
genKS(K,S,[C|L]) :- K > 1 ,K1 is K - 1, my_between(0,S,C), NewS is S - C, genKS(K1,NewS,L).

% Generate all finite lists of natural numbers
listOfNumbers([]).
listOfNumbers(L) :- nat(N), my_between(1,N,A), B is N - A, genKS(A,B,L).

% Divide list in smaller lists
split([],[]).
split(L, [A|R]) :- my_append(A,B,L), A \= [], split(B,R).
/*
?- split([1,2,3],L).
L = [[1], [2], [3]] ;
L = [[1], [2, 3]] ;
L = [[1, 2], [3]] ;
L = [[1, 2, 3]] ;
false.
*/

% Opposite of divide
my_flatten([],[]).
my_flatten(X,[X]) :- not(isList(X)).
my_flatten([H|T],R) :- my_flatten(H,FH), my_flatten(T,FT), my_append(FH,FT,R).

% Count occurrences of Y in L
countOccurrences(X,L,C) :- countOccurrences(X,L,0,C).
countOccurrences(_,[],Agg,Agg).
countOccurrences(X,[X|T],Agg,C) :- Agg1 is Agg + 1, countOccurrences(X,T,Agg1,C).
countOccurrences(X,[H|T],Agg,C) :- X \= H, countOccurrences(X,T,Agg,C).

count([],_,0).
count([H|T],H,N) :- count(T,H,M), N is M + 1.
count([H|T],Y,N) :- H \= Y, count(T,Y,N).

% Find the GCD of two numbers
dividor(X,Y) :- Y mod X =:= 0.

smaller(X,Y,X) :- X =< Y.
smaller(X,Y,Y) :- X > Y.

commonDividor(X,Y,D) :- smaller(X,Y,M), between(1,M,D), dividor(D,X), dividor(D,Y).

gcd(0,Y,Y).
gcd(X,0,X).
gcd(X,Y,D) :- commonDividor(X,Y,D), not((commonDividor(X,Y,D1), D1 > D)).

/* or
gcd(A, 0, A).
gcd(A, B, G):- B > 0, C is A mod B, gcd(B, C, G).
*/
