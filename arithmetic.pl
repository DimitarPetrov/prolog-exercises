increment(X,Y) :- Y is X + 1.

sum(X,Y,Z) :- Z is X + Y.

addOne([],[]).
addOne([H|T],[Hr|Tr]) :- Hr is H + 1, addOne(T,Tr).

min([H|T],R) :- accMin(T,H,R).

accMin([H|T],Acc,R) :- H < Acc, accMin(T,H,R).
accMin([H|T],Acc,R) :- H >= Acc, accMin(T,Acc,R).
accMin([],Acc,Acc).

scalarMultiplication(_,[],[]).
scalarMultiplication(Scalar,[H|T],[Hr|Tr]) :- Hr is Scalar * H,
                                              scalarMultiplication(Scalar,T,Tr).

dotProduct([],[],0).
dotProduct([H1|T1],[H2|T2],R) :- dotProduct(T1,T2,R1), R is H1 * H2 + R1.
