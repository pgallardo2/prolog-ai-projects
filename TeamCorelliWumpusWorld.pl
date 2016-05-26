%Author: Paola Gallardo
%Date Created: January 28th 2016
%Date Updated: February 8th 2016
%This is an incomplete version of the Wumpus World agent
%I still need to implement: looping decision-making,
%Overall, I dont have a strategy yet

%Maze
%room/2
%room(Coordinate,[stench,breeze,glitter,wumpus,pit]).
%
:-dynamic room/2.
%room(coord(1,1),[none,none,none,none,none]).
%room(coord(1,2),[stench,none,none,none,none]).
%room(coord(1,3),[none,none,none,wumpus,none]).
%room(coord(1,4),[stench,none,none,none,none]).

%room(coord(2,1),[none,breeze,none,none,none]).
%room(coord(2,2),[none,none,none,none,none]).
%room(coord(2,3),[stench,breeze,glitter,none,none]).
%room(coord(2,4),[none,none,none,none,none]).

%room(coord(3,1),[none,none,none,none,pit]).
%room(coord(3,2),[none,breeze,none,none,none]).
%room(coord(3,3),[none,none,none,none,pit]).
%room(coord(3,4),[none,breeze,none,none,none]).

%room(coord(4,1),[none,breeze,none,none,none]).
%room(coord(4,2),[none,none,none,none,none]).
%room(coord(4,3),[none,breeze,none,none,none]).
%room(coord(4,4),[none,none,none,none,pit]).

%room(coord(1,1),[none,breeze,none,none,none]).
%room(coord(2,1),[none,none,none,none,pit]).
%room(coord(3,1),[none,breeze,none,none,none]).
%room(coord(4,1),[none,none,none,none,none]).
%room(coord(1,2),[none,none,none,none,none]).
%room(coord(2,2),[none,breeze,none,none,none]).
%room(coord(3,2),[stench,none,glitter,none,none]).
%room(coord(4,2),[none,none,none,none,none]).
%room(coord(1,3),[none,breeze,none,none,none]).
%room(coord(2,3),[none,none,none,none,pit]).
%room(coord(3,3),[none,breeze,none,wumpus,none]).
%room(coord(4,3),[stench,none,none,none,none]).
%room(coord(1,4),[none,none,none,none,none]).
%room(coord(2,4),[none,breeze,none,none,none]).
%room(coord(3,4),[stench,none,none,none,none]).
%room(coord(4,4),[none,none,none,none,none]).

room(coord(1,1),[none,none,none,none,none]).
room(coord(1,2),[none,breeze,none,none,none]).
room(coord(1,3),[none,none,none,none,pit]).
room(coord(1,4),[none,breeze,none,none,none]).
room(coord(2,1),[none,none,none,none,none]).
room(coord(2,2),[none,none,none,none,none]).  
room(coord(2,3),[stench,breeze,none,none,none]).
room(coord(2,4),[none,none,none,none,pit]).
room(coord(3,1),[none,breeze,none,none,none]).
room(coord(3,2),[stench,none,none,none,none]).
room(coord(3,3),[none,none,none,wumpus,none]).
room(coord(3,4),[stench,breeze,none,none,none]).
room(coord(4,1),[none,none,none,none,pit]).
room(coord(4,2),[none,breeze,none,none,none]).
room(coord(4,3),[stench,none,none,none,none]).
room(coord(4,4),[none,none,glitter,none,none]).

%setting up needed variables

:-dynamic agent_status/1.
:-dynamic has_gold/1.
:-dynamic arrows/1.
:-dynamic facing/1.
:-dynamic path/1.
:-dynamic in_room/2.
:-dynamic wumpus_status/1.
:-dynamic visited/1.
:-dynamic smelly/1.
:-dynamic breezy/1.

agent_status(alive).
has_gold(no).
arrows(1).
facing(north).
in_room(1,1).
path([]).

visited([]).
smelly([]).
breezy([]).



agentUpdate(X):-
	retract(agent_status(_)),
	assertz(agent_status(X)).


agent_run:-
	agent_status(S),
	S = alive,
	in_room(X,Y),
	write('in room:	'), write(X), write(', '),write(Y),	nl,
	addVisited(X,Y),
	room(coord(X,Y),Percept),
	action(Percept),
	agent_run.

agent_run:-
	agent_status(X),
	X = won,
	format("you won").

agent_run:-
	agent_status(X),
	X = dead,
	format("you are dead").


%setting up direction and how to change direction
isDirection(north).
isDirection(south).
isDirection(west).
isDirection(east).

%get opposite direction
oppDir(north,south).
oppDir(east,west).
oppDir(west,east).
oppDir(south,north).

changeDir(Dir):-facing(X),
	isDirection(Dir),
	retract(facing(X)),
	assertz(facing(Dir)).

getGold:- 
	in_room(X,Y),
	room(coord(X,Y),[S, B, G, W, P]),
	G = glitter,
	retract(room(coord(X,Y),[S, B, G, W, P])),
	assertz(room(coord(X,Y),[S, B, none, W, P])),
	retract(has_gold(_)),
	assertz(has_gold(yes)),
	format("you have the gold!"),nl.

addVisited(X,Y):-visited(L), \+member([X,Y],L),append(L,[X,Y],NL), retract(visited(L)),assertz(visited(NL)).
addVisited(X,Y):-visited(L),member([X,Y],L).
addBreezy(X,Y):-breezy(L), \+member([X,Y],L),append(L,[X,Y],NL), retract(breezy(L)),assertz(breezy(NL)).
addBreezy(X,Y):-breezy(L),member([X,Y],L).
addSmelly(X,Y):-smelly(L), \+member([X,Y],L),append(L,[X,Y],NL), retract(smelly(L)),assertz(smelly(NL)).
addSmelly(X,Y):-smelly(L),member([X,Y],L).

action([_,_,_,_,_]):-move,facing(X),savePath(X),format("why igotta").
%action([none,none,none,none,none]):-facing(X),oppDir(X,NX),changeDir(NX),move,changeDir(east),savePath(east),format("wait what").
%action([none,none,none,none,none]):-changeDir(east),move,savePath(east),format("why doesn't thiswork").

action([_,_,glitter,none,none]):- getGold,retrace.
action([_,_,_,wumpus,_]):- agentUpdate(dead).
action([_,_,_,_,pit]):- format("pit"),agentUpdate(dead).
/**
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(north), oppDir(north,NX), changeDir(NX), move, changeDir(east), move, facing(DX), savePath(DX), changeDir(north).
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(north), oppDir(north,NX), changeDir(NX), move, changeDir(west), move, facing(DX), savePath(DX), changeDir(north).
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(east), oppDir(east,NX), changeDir(NX), move, changeDir(north), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(east), oppDir(east,NX), changeDir(NX), move, changeDir(south), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(west), oppDir(west,NX), changeDir(NX), move, changeDir(north), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), smelly(L), \+member([X,Y],L),addSmelly(X,Y),facing(west), oppDir(west,NX), changeDir(NX), move, changeDir(south), move, facing(DX), savePath(DX).

action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(north), oppDir(north,NX), changeDir(NX), move, changeDir(east), move, facing(DX), savePath(DX),format("never").
action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(north), oppDir(north,NX), changeDir(NX), move, changeDir(west), move, facing(DX), savePath(DX),format("gonna").
action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(east), oppDir(east,NX), changeDir(NX), move, changeDir(north), move, facing(DX), savePath(DX),format("give").
action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(east), oppDir(east,NX), changeDir(NX), move, changeDir(south), move, facing(DX), savePath(DX),format("you").
action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(west), oppDir(west,NX), changeDir(NX), move, changeDir(north), move, facing(DX), savePath(DX),format("up").
action([none,breeze,none,none,none]):-in_room(X,Y), breezy(L), \+member([X,Y],L),addBreezy(X,Y),facing(west), oppDir(west,NX), changeDir(NX), move, changeDir(south), move, facing(DX), savePath(DX),format("john").


action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(north), oppDir(north,NX), changeDir(NX), move, facing(DX), savePath(DX),format("paul").
action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(north), move, facing(DX), savePath(DX),format("george").
action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(east), oppDir(east,NX), changeDir(NX), move,facing(DX), savePath(DX),format("ringo").
action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(east),move, facing(DX), savePath(DX),format("robert").
action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(west), oppDir(west,NX), changeDir(NX), move,facing(DX), savePath(DX),format("jimmy").
action([stench,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y),addSmelly(X,Y),facing(west),  move, facing(DX), savePath(DX),format("jonesy").


action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthWest(X,Y),facing(north), changeDir(east), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthWest(X,Y),facing(north), changeDir(west), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthWest(X,Y),facing(east), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthWest(X,Y),facing(west), changeDir(east), move, facing(DX), savePath(DX).

action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthWest(X,Y),facing(north), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthWest(X,Y),facing(north),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthWest(X,Y),facing(east), changeDir(north),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthWest(X,Y),facing(west), changeDir(north), move, facing(DX), savePath(DX).

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthWest(X,Y), facing(north), changeDir(east), move, facing(DX), savePath(DX),format("bonzo").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthWest(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("moon").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthWest(X,Y), facing(east), move, facing(DX), savePath(DX),format("daltrey").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthWest(X,Y), facing(west), changeDir(east),move, facing(DX), savePath(DX),format("townsend").

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthWest(X,Y), facing(north), move, savePath(north),format("enwistle").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthWest(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("hendrix").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthWest(X,Y), facing(east), changeDir(north), move, facing(DX), savePath(DX),format("joplin").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthWest(X,Y), facing(west), changeDir(north),move, facing(DX), savePath(DX),format("fuck").


action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthEast(X,Y),facing(north), changeDir(west), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthEast(X,Y),facing(north), changeDir(east), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthEast(X,Y),facing(west), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellyNorthEast(X,Y),facing(east), changeDir(west), move, facing(DX), savePath(DX).

action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthEast(X,Y),facing(north), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthEast(X,Y),facing(north),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthEast(X,Y),facing(east), changeDir(north),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellyNorthEast(X,Y),facing(west), changeDir(north), move, facing(DX), savePath(DX).

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthEast(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("this sucks").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthEast(X,Y), facing(north), changeDir(east), move, facing(DX), savePath(DX),format("why").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthEast(X,Y), facing(west), move, facing(DX), savePath(DX),format("idk").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezyNorthEast(X,Y), facing(east), changeDir(west),move, facing(DX), savePath(DX),format("more rockstars").

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthEast(X,Y), facing(north), move, savePath(north),format("goddammit").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthEast(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("igive").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthEast(X,Y), facing(east), changeDir(north), move, facing(DX), savePath(DX),format("nofucks").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezyNorthEast(X,Y), facing(west), changeDir(north),move, facing(DX), savePath(DX),format("shet").


action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthWest(X,Y),facing(north), changeDir(east), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthWest(X,Y),facing(north), changeDir(west), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthWest(X,Y),facing(east), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthWest(X,Y),facing(west), changeDir(east), move, facing(DX), savePath(DX).

action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthWest(X,Y),facing(north), changeDir(south), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthWest(X,Y),facing(north), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthWest(X,Y),facing(east), changeDir(south),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthWest(X,Y),facing(west), changeDir(south), move, facing(DX), savePath(DX).

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthWest(X,Y), facing(north), changeDir(east), move, facing(DX), savePath(DX),format("somany").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthWest(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("dear god").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthWest(X,Y), facing(east), move, facing(DX), savePath(DX),format("more?").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthWest(X,Y), facing(west), changeDir(east),move, facing(DX), savePath(DX),format("wow").

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthWest(X,Y), facing(north), cgabgeDir(south),move, savePath(south),format("pls").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthWest(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("stop").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthWest(X,Y), facing(east), changeDir(south), move, facing(DX), savePath(DX),format("now").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthWest(X,Y), facing(west), changeDir(south),move, facing(DX), savePath(DX),format("cries").


action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthEast(X,Y),facing(north), changeDir(west), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthEast(X,Y),facing(north), changeDir(east), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthEast(X,Y),facing(west), move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), checkSmellySouthEast(X,Y),facing(east), changeDir(west), move, facing(DX), savePath(DX).

action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthEast(X,Y),facing(north),changeDir(south), move, facing(DX), savePath(DX).
%action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthEast(X,Y),facing(north),changeDir(south),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthEast(X,Y),facing(east), changeDir(south),move, facing(DX), savePath(DX).
action([stench,none,none,none,none]):-in_room(X,Y), addSmelly(X,Y), \+checkSmellySouthEast(X,Y),facing(west), changeDir(south), move, facing(DX), savePath(DX).

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthEast(X,Y), facing(north), changeDir(west), move, facing(DX), savePath(DX),format("lastones").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthEast(X,Y), facing(north), changeDir(east), move, facing(DX), savePath(DX),format("hopefully").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthEast(X,Y), facing(west), move, facing(DX), savePath(DX),format("lelz").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), checkBreezySouthEast(X,Y), facing(east), changeDir(west),move, facing(DX), savePath(DX),format("breh").

action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthEast(X,Y), facing(north), changeDir(south),move, savePath(south),format("jjajaja").
%action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthEast(X,Y), facing(north), changeDir(south), move, facing(DX), savePath(DX),format("finally").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthEast(X,Y), facing(east), changeDir(south), move, facing(DX), savePath(DX),format("por fin").
action([none,breeze,none,none,none]):-in_room(X,Y), addBreezy(X,Y), \+checkBreezySouthEast(X,Y), facing(west), changeDir(south),move, facing(DX), savePath(DX),format("the end").
*/

checkSmellyNorthWest(X,Y):-smelly(L),NWX is X-1, NWY is Y+1, member([NWX,NWY],L).
checkSmellyNorthEast(X,Y):-smelly(L),NEX is X+1, NEY is Y+1, member([NEX,NEY],L).
checkSmellySouthWest(X,Y):-smelly(L),SWX is X-1, SWY is Y-1, member([SWX, SWY],L).
checkSmellySouthEast(X,Y):-smelly(L),XSE is X+1, YSE is Y-1, member([XSE,YSE],L).

checkBreezyNorthWest(X,Y):-breezy(L),NWX is X-1, NWY is Y+1, member([NWX,NWY],L).
checkBreezyNorthEast(X,Y):-breezy(L),NEX is X+1, NEY is Y+1, member([NEX,NEY],L).
checkBreezySouthWest(X,Y):-breezy(L),SWX is X-1, SWY is Y-1, member([SWX, SWY],L).
checkBreezySouthEast(X,Y):-breezy(L),XSE is X+1, YSE is Y-1, member([XSE,YSE],L).

%moving
move:-facing(north),
	in_room(X,Y),
	Y<4,
	YN is Y+1,
	retract(in_room(X,Y)),
	assertz(in_room(X,YN)).
move:-facing(south),
	in_room(X,Y),
	Y>1,
	YN is Y-1,
	retract(in_room(X,Y)),
	assertz(in_room(X,YN)).
move:-facing(west),
	in_room(X,Y),
	X>1,
	XN is X-1,
	retract(in_room(X,Y)),
	assertz(in_room(XN,Y)).
move:-facing(east),
	in_room(X,Y),
	X<4,
	XN is X+1,
	retract(in_room(X,Y)),
	assertz(in_room(XN,Y)).

savePath(Dir):-
	path(X),
	retract(path(X)),
	assertz(path([Dir|X])).

getDir(Dir):-path([Dir|X]),
	retract(path([Dir|X])),
	assertz(path(X)).

%to retrace
retrace:-
	firstRoom,
	in_room(X,Y),
	write('in room:	'), write(X), write(', '),write(Y),	nl,
	agentUpdate(won).


retrace:-
	\+firstRoom,
	getDir(Dir),
	oppDir(Dir,NewDir),
	changeDir(NewDir),
	move,
	in_room(X,Y),
	write('in room:	'), write(X), write(', '),write(Y),	nl,
	retrace.

firstRoom:-
	in_room(X,Y),
	X=1,
	Y=1.
