directlyIn(katarina,olga).
directlyIn(olga,natasha).
directlyIn(natasha,irina).

in(X,Y) :- directlyIn(X,Y).
in(X,Y) :- directlyIn(X,Z), in(Z,Y).

%-----------------------------------

numeral(0).
numeral(succ(X)) :- numeral(X).

greater_than(succ(_),0).
greater_than(succ(X),succ(Y)) :- greater_than(X,Y).

%-----------------------------------

leaf(_).
tree(T1,T2) :- (T1 = leaf(_); T1 = tree(_, _)), (T2 = leaf(_); T2 = tree(_, _)).

% Mirror T1 in T2
swap(leaf(L1),leaf(L1)).
swap(tree(T1,T2), tree(R2,R1)) :- swap(T1,R1), swap(T2,R2).

%----------------------------------

directTrain(forbach,saarbruecken).
directTrain(freyming,forbach).
directTrain(fahlquemont,stAvold).
directTrain(stAvold,forbach).
directTrain(saarbruecken,dudweiler).
directTrain(metz,fahlquemont).
directTrain(nancy,metz).

travelBetween(X,Y) :- directTrain(X,Y).
travelBetween(X,Y) :- directTrain(X,Z), travelBetween(Z,Y).

%----------------------------------

connected(1,2).
connected(3,4).
connected(5,6).
connected(7,8).
connected(9,10).
connected(12,13).
connected(13,14).
connected(15,16).
connected(17,18).
connected(19,20).
connected(4,1).
connected(6,3).
connected(4,7).
connected(6,11).
connected(14,9).
connected(11,15).
connected(16,12).
connected(14,17).
connected(16,19).

path(X,Y) :- connected(X,Y).
path(X,Y) :- connected(X,Z), path(Z,Y).

%---------------------------------

byCar(auckland,hamilton).
byCar(hamilton,raglan).
byCar(valmont,saarbruecken).
byCar(valmont,metz).

byTrain(metz,frankfurt).
byTrain(saarbruecken,frankfurt).
byTrain(metz,paris).
byTrain(saarbruecken,paris).

byPlane(frankfurt,bangkok).
byPlane(frankfurt,singapore).
byPlane(paris,losAngeles).
byPlane(bangkok,auckland).
byPlane(losAngeles,auckland).

travel(X,Y) :- byCar(X,Y); byTrain(X,Y); byPlane(X,Y).
travel(X,Y) :- (byCar(X,Z); byTrain(X,Z); byPlane(X,Z)), travel(Z,Y).

travel(X,Y,go(X,Y)) :- byCar(X,Y); byTrain(X,Y); byPlane(X,Y).
travel(X,Y,go(X,Z,Path)) :- (byCar(X,Z); byTrain(X,Z); byPlane(X,Z)), travel(Z,Y,Path).

travelByTransport(X,Y,byCar(X,Y)) :- byCar(X,Y).
travelByTransport(X,Y,byTrain(X,Y)) :- byTrain(X,Y).
travelByTransport(X,Y,byPlane(X,Y)) :- byPlane(X,Y).

travelByTransport(X,Y,byCar(X,Z,Path)) :- byCar(X,Z), travelByTransport(Z,Y,Path).
travelByTransport(X,Y,byTrain(X,Z,Path)) :- byTrain(X,Z), travelByTransport(Z,Y,Path).
travelByTransport(X,Y,byPlane(X,Z,Path)) :- byPlane(X,Z), travelByTransport(Z,Y,Path).

