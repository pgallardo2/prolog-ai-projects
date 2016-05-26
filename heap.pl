% heap.pl
% Paola Gallardo, Fardos Aboargob, Michelle Afravi, James Bias

% ADT Heap
% Basically the same as ADTstack and ADTqueue except for the priority concept

% newHeap/1
% newHeap(Heap) returns true if Queue is instantiated to a new queue
newHeap([ ]).

% isEmptyHeap/1
% isEmptyHeap(Heap) returns true if Queue is empty
isEmptyHeap([ ]).

% insert/3
% insert(Element,OldHeap,NewHeap) 
% inserts element based on "priority", the priority function is defined in astar.pl because it uses predicates found in astar.pl
insert(E, [ ], [E]).
%if the element to be inserted has a lower priority value (since min-heap, in this case the manhattan distance), it is placed as the head
insert(E,[H|T],[E,H|T]):-priority(H,PH), priority(E,EH),EH=<PH.
%to continue traversing until the element finds it's place
insert(E, [H|Q], [H|Q2]) :- insert(E, Q, Q2).

% delete/3
% delete(Element,OldHeap,NewHeap) removes head of list 
delete(E, [E|Q], Q).

% peekHeap/2
% peek(Element,Heap) returns true if Element is the next item in Heap
peekHeap(E,[E|_]).