count([],0).
count([_|T],N):-count(T,M), N is M+1.

sum([],0).
sum([H|T],S):-sum(T,S1),S is S1+H.

mean(L,N):-sum(L,S),count(L,C),N is S/C.

myLast([X],X).
myLast([_|T],N):-myLast(T,N).

firstAndLast([X],X,X).
firstAndLast([H|T],H,Z):-myLast(T,Z).

nextToLast([A,_],A).
nextToLast([_|T],E):-nextToLast(T,E).

odd(N):- 1 is mod(N,2).
countOdd([],0).
countOdd([H|T],N):-odd(H),countOdd(T,M),N is M+1.
countOdd([_|T],N):-  countOdd(T,N).

myAppend([],L,L).
myAppend([H|T],L2,[H|L3]):-myAppend(T,L2,L3).

max([],M,M).
max([H|T],MT,M):-H>MT,max(T,H,M).
max([_|T],MT,M):-max(T,MT,M).
max([H|T],M):-max(T,H,M).

negNums([],[]).
negNums([H|T],[H|T2]):-H<0,negNums(T,T2).
negNums([_|T],T2):-negNums(T,T2).


sunflower.
dog.
cat.
wolverine.
pineapple.
plant(sunflower).
plant(pineapple).
animal(dog).
animal(cat).
animal(wolverine).
aniPlants([],[],[]).
aniPlants([H|T],[H|A],P):-animal(H),aniPlants(T,A,P).
aniPlants([H|T],A,[H|P]):-plant(H),aniPlants(T,A,P).
aniPlants([_|T],A,P):-aniPlants(T,A,P).
