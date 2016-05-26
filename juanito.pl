%------------------------------------------------------------------------------
% Remove warnings for singleton variables
:- style_check(-singleton).

% Declaring dynamic methods
:- module(juanito).

:- [map].
:- [obj].
:- [driver].
:- [paquito].

:- dynamic ([
				in/2,
				items/1,
				usedItems/1,
				vampireRooms/1,
				rooms/1,
            	secretItems/1,
            	agentStatus/1,
            	roomsInMap/1,
            	acts/1
			]).

%------------------------------------------------------------------------------

contains(_,[]):- false,!.
contains(V, [V|_]) :- true,!.
contains(V, [_|T]):- contains(V,T).

is_empty([]):- true.
acts([]).

juanito([run]):-start,run.

juanito(Agent2,Acts2,[greet(juanito,Agent2)]):-
	is_empty(Acts2),
	retract(acts(_)),assertz(acts([greet(juanito,Agent2)])),!.

juanito(Agent2,Acts2,[(greet(juanito,Agent2))]):-
	contains(greet(Agent2,juanito),Acts2),
	acts(Acts1),
	\+contains(greet(juanito,Agent2),Acts1),
	retract(acts(_)),assertz(acts([greet(juanito,Agent2)])),!.

juanito(Agent2,Acts2,[request(juanito,Agent2,move(Room))]):-
	acts(Acts1),
	contains(greet(juanito,Agent2),Acts1),
	((contains(greet(Agent2,juanito),Acts2)) ; (contains(agree(request(juanito,Agent2,move(_))),Acts2))),
	in(juanito,R),
	\+R=win,
	move,
	in(juanito,Room),
	retract(acts(_)),assertz(acts([request(juanito,Agent2,move(Room))|Acts1])),!.

juanito(Agent2,Acts2,[agree(request(Agent2,juanito,move(Room)))]):-
	acts(Acts1),
	contains(greet(juanito,Agent2),Acts1),
	contains(request(Agent2,juanito,move(Room)),Acts2),
	moveJuanito(Room),
	in(juanito,Room),
	retract(acts(_)),assertz(acts([agree(request(Agent2,juanito,move(Room)))|Acts1])),!.

juanito(Agent2,Acts2,[inform(Agent2,juanito,'we escaped')]):-
	acts(Acts1),
	in(juanito,win),
	in(Agent2,win),
	retract(game(_)),assertz(game(done)),
	retract(acts(_)),assertz(acts([inform(Agent2,juanito,'we escaped')|Acts1])),!.






%------------------------------------------------------------------------------

start:-
format("Start game!~n~n"),
	retractall(in(juanito,_)), assert(in(juanito,frontEntrance)),
	retractall(rooms(_) ), assert(rooms([])),
	retractall(items(_) ), assert(items([])),
	retractall(usedItems(_)),assert(usedItems([])),
	retractall(vampireRooms(_)),assert(vampireRooms([])),
	retractall(secretItems(_)),assert(secretItems([])),
	retractall(agentStatus(_)),assert(agentStatus(alive)),
	retractall(roomsInMap(_)),assert(roomsInMap([])),
	retractall(game(_)),assert(game(playing)),
	retractall(acts(_)),assert(acts([])),
	retractall(greeted(_,_)),assert(greeted(false,false)),
	addRoom(frontEntrance).

reset_game:-
	retractall(in(juanito,_)), assert(in(juanito,frontEntrance)),
	retractall(rooms(_) ), assert(rooms([])),
	retractall(roomsInMap(_)),assert(roomsInMap([])),
	retractall(items(_) ), assert(items([])),
	retractall(usedItems(_)),assert(usedItems([])),
	retractall(secretItems(_)),assert(secretItems([])),
	retractall(agentStatus(_)),assert(agentStatus(alive)),
	retractall(game(_)),assert(game(playing)),
	retractall(acts(_)),assert(acts([])),
	addRoom(frontEntrance),
	resetMap.

resetMap:-
	retract(room(lobby,A,B,_,_)), assertz(room(lobby,A,B,[brassKey,woodenStake],[exit(westStairs,visible,unlocked),exit(eastStairs,visible,unlocked),exit(courtyard,visible,unlocked)])),
	retract(room(clockTower,C,D,_,_)), assertz(room(clockTower,C,D,[],[exit(treasureRoom,visible,locked),exit(royalChamber,visible,unlocked)])),
	retract(room(royalChamber,E,F,_,_)), assertz(room(royalChamber,E,F,[candle],[exit(clockTower,visible,unlocked),exit(storage,hidden,locked), exit(bedroom,visible,unlocked), exit(westStairs,visible,unlocked)])),
	retract(room(storage,G,H,_,_)), assertz(room(storage,G,H,[helmet],[exit(royalChamber,visible,unlocked)])),
	retract(room(westStairs,I,J,_,_)), assertz(room(westStairs,I,J,[],[exit(lobby,visible,unlocked),exit(royalChamber,hidden,unlocked)])),
	retract(room(eastStairs,K,L,_,_)), assertz(room(eastStairs,K,L,[],[exit(lobby,visible,unlocked),exit(basement,visible,unlocked)])),
	retract(room(courtyard,M,N,_,_)), assertz(room(courtyard,M,N,[cloak],[exit(diningHall, visible, unlocked), exit(lobby, visible, unlocked), exit(chapel, visible, unlocked)])),
	retract(room(chapel,O,P,_,_)), assertz(room(chapel,O,P,[crucifix],[exit(artRoom, visible, unlocked), exit(courtyard, visible, unlocked)])),
	retract(room(artRoom,Q,R,_,_)), assertz(room(artRoom,Q,R,[],[exit(chapel, visible, unlocked), exit(library, hidden, locked)])),
	retract(room(library,S,T,_,_)), assertz(room(library,S,T,[ancientBook],[exit(artRoom, visible, unlocked)])),
	retract(room(bedroom,U,V,_,_)), assertz(room(bedroom,U,V,[],[exit(royalChamber,visible,unlocked),exit(secretStairs,hidden,locked)])),
	retract(room(secretStairs,W,X,_,_)), assertz(room(secretStairs,W,X,[],[exit(bedroom,visible,unlocked),exit(throneRoom,hidden,unlocked)])),
	retract(room(throneRoom,Y,Z,_,_)), assertz(room(throneRoom,Y,Z,[],[exit(secretStairs, visible,unlocked),exit(win,hidden,locked)])),
	retract(room(pantry,AA,BB,_,_)), assertz(room(pantry,AA,BB,[magicPotion],[exit(kitchen,visible,unlocked)])),
	retract(room(kitchen,CC,DD,_,_)), assertz(room(kitchen,CC,DD,[garlic],[exit(pantry,visible,unlocked), exit(diningHall,visible,unlocked)])),
	retract(room(diningHall,EE,FF,_,_)), assertz(room(diningHall,EE,FF,[],[exit(kitchen,visible,unlocked),exit(weaponRoom,hidden,locked), exit(courtyard,visible,unlocked)])),
	retract(room(weaponRoom,GG,HH,_,_)), assertz(room(weaponRoom,GG,HH,[rustySword],[exit(diningHall,visible,unlocked)])),
	retract(room(wineCellar,II,JJ,_,_)), assertz(room(wineCellar,II,JJ,[holyWater],[exit(basement,visible,unlocked)])),
	retract(room(basement,KK,LL,_,_)), assertz(room(basement,KK,LL,[stone],[exit(eastStairs,visible,unlocked),exit(wineCellar,visible,unlocked),exit(crypt,visible,unlocked),exit(prison,visible,unlocked)])),
	retract(room(crypt,MM,NN,_,_)), assertz(room(crypt,MM,NN,[silverKey],[exit(basement,visible,unlocked)])),
	retract(room(bigCell,OO,PP,_,_)), assertz(room(bigCell,OO,PP,[dragonKey],[exit(prison,visible,unlocked)])),
	retract(room(prison,QQ,RR,_,_)), assertz(room(prison,QQ,RR,[],[exit(basement,visible,unlocked),exit(bigCell,visible,locked), exit(smallCell,visible,locked)])),
	retract(room(smallCell,SS,TT,_,_)), assertz(room(smallCell,SS,TT,[gear],[exit(prison,visible,unlocked)])),
	retract(room(treasureRoom,UU,VV,_,_)), assertz(room(treasureRoom,UU,VV,[crown],[exit(clockTower,visible, unlocked)])),
	retract(room(frontEntrance,HELPME,HELPMEPLS,_,_)), assertz(room(frontEntrance,HELPME,HELPMEPLS,[],[exit(lobby,visible,unlocked)])).

print_description:-
	in(juanito,N),
	room(N,D,A,O,E),
	items(I),
	format("Room is: ~p~n", N).
	%format("Description: ~p~n", D),
	%(is_empty(I) -> format("No items ~n"); format("Items: ~p~n", [I])),
	%format("Exits: ~p~n", [E]).

%------------------------------------------------------------------------------


run:-
	in(juanito,win),
	retract(game(_)),assertz(game(done)).
run:-
	move,
	run.


moveJuanito(Room):-
	retract((in(juanito,R))),
	assertz((in(juanito,Room))).

checkAgents(L1):-
	room(L1,_,A,_,_),
	contains(vampire,A),
	addVampireRoom(L1).
checkAgents(L1):-
	room(L1,_,A,_,_),
	\+contains(vampire,A).

checkItems([],_).
checkItems([H|T],L):-
	\+contains(H,L),
	checkItems(T,L).

check_status:-
	agentStatus(dead),
	reset_game.
check_status:-
	agentStatus(alive).
check_status:-
	agentStatus(won),
	format("Agent won!").

addRoom(L1) :-
	rooms(L2),
	addMap(L1),
	retract(rooms(_)), assertz(rooms([L1|L2])).

addMap(R):-
	roomsInMap(L),
	\+contains(R,L),
	retract(roomsInMap(L)),assertz(roomsInMap([R|L])),!.
addMap(R):-
	roomsInMap(L),
	contains(R,L).

addVampireRoom(L1):-
	vampireRooms(L2),
	retractall(vampireRooms(_)), assert(vampireRooms([L1|L2])),
	format("Agent dies at: ~p~n~n", L1),
	retractall(agentStatus(_)), assert(agentStatus(dead)).

get_items:-
	in(juanito,N),
	room(N,_,_,I,_),
	getVisibleItems(N,I).

getVisibleItems(_,[]).
getVisibleItems(N,[H|T]):-
	object(H,_,V,_,_,_),
	V=visible,
	add_item(H),
	retract(room(N,D,A,_,E)),assertz(room(N,D,A,T,E)),
	getVisibleItems(N,T).
	getVisibleItems(_,[H|T]):-
	object(H,_,V,_,_,_),
	V=hidden,
	getVisibleItems(_,T).

add_item(I) :-
	items(L2),
	append([I],L2,L3),
	retractall(items(_)), assert(items(L3)).

has(_Agent,Item):-
	items(L),
	contains(Item,L).

makeVisible(Item,Room):-
	room(Room,_,_,O,_),
	retractall(room(Room,_,_,O,_)),assert(room(Room,_,_,[Item|O],_)).

hasVampireItem:-
	items(I),
	(contains(holyWater,I);
	contains(crucifix,I);
	contains(garlic,I);
	contains(woodenStake,I)).

%------------------------------------------------------------------------------
% Moving Predicates

vampireItem(garlic).
vampireItem(crucifix).
vampireItem(holyWater).
vampireKingItem(rustySword).


%gets room in map that needs an item that juanito has
getItemRoom([],nope).
getItemRoom([H|T],R):-needsItem(H,R),\+R=nope,!.
getItemRoom([H|T],R):-needsItem(H,RM),RM=nope,getItemRoom(T,R).

%checks if any of the rooms needs an item I
needsItem(I,Room):-
	roomsInMap(R),
	checkRooms(R,I,Room).

%if there is no rooms the items is not yet needed
checkRooms([],_,nope).
%checks if a room needs an item I
checkRooms([H|T],I,H):- \+vampireItem(I),\+vampireKingItem(I),object(I,_,_,_,C,_),contains(in(A,H),C).
checkRooms([H|T],I,H):- vampireKingItem(I),object(I,_,_,_,C,_),contains(in(A,H),C),H=throneRoom,items(Items),contains(crown,Items).
checkRooms([H|T],I,H):- vampireKingItem(I),object(I,_,_,_,C,_),contains(in(A,H),C),H=artRoom,room(H,_,_,_,E),contains(exit(library,hidden,locked),E).
checkRooms([H|T],I,RM):-vampireItem(I), \+vampireKingItem(I),hasExit([H|T],RM).
checkRooms([H|T],I,R):-object(I,_,_,_,C,_),checkRooms(T,I,R).

%needed to get to vampire rooms. checks if rooms has exit to room with vampire
hasExit([],nope).
hasExit([H|T],H):-vampireRooms(R), checkExit(R,H,RM),RM=H.
hasExit([H|T],M):-vampireRooms(R), checkExit(R,H,RM),\+RM=H,hasExit(T,M).

checkExit([],_,noRoom).
checkExit([H|T],R,R):-room(R,_,_,_,E),contains(exit(H,visible,unlocked),E).
checkExit([H|T],R,RM):-room(R,_,_,_,E),\+contains(exit(H,visible,unlocked),E),checkExit(T,R,RM).

%returns list of unvisited exits in current room
unvisitedExits(R,E):-room(R,_,_,_,Exits),getUnvisitedExits(Exits,E).
getUnvisitedExits([],[]).
getUnvisitedExits([],R):- \+is_empty(R).
getUnvisitedExits([exit(H,V,L)|T],[H|T2]):-rooms(R),\+contains(H,R),V=visible,L=unlocked,getUnvisitedExits(T,T2).
getUnvisitedExits([exit(H,V,L)|T],UE):-rooms(R),(contains(H,R);(\+contains(H,R),(V=hidden;L=locked))),getUnvisitedExits(T,UE).

%returns room that does not contain a vampire
getSafeRoom([],nope).
getSafeRoom([H|T],H):-vampireRooms(L), (\+contains(H,L); (contains(H,L),hasVampireItem)).
getSafeRoom([H|T],RM):-vampireRooms(L), contains(H,L),\+hasVampireItem,getSafeRoom(T,RM).

%finds a room that have unvisited exits
findRoom([],nel).
findRoom([H|T],H):-unvisitedExits(H,E),\+is_empty(E).
findRoom([H|T],R):-unvisitedExits(H,[]),findRoom(T,R).

%moves to a room that needs an item
move:-
	in(juanito,N),
	(unvisitedExits(N,[]);(unvisitedExits(N,E),\+is_empty(E),getSafeRoom(E,RM),RM=nope)),
	rooms([_|T]),
	roomsInMap(L),
	findRoom(L,Room),
	Room=nel,
	items(Items),
	getItemRoom(Items,ItemRoom),
	go_back(ItemRoom,T),
	print_description,
	check_status,!.

%moves to unvisited room
move:-
	in(juanito,N),
	unvisitedExits(N,E),
	getSafeRoom(E,RM),
	\+RM=nope,
	addRoom(RM),
	retract(in(juanito,_)),assertz(in(juanito,RM)),
	get_items,
	use_items,
	checkAgents(RM),
	print_description,
	check_status,!.

%moves to room that has unvisited exits
move:-
	in(juanito,N),
	(unvisitedExits(N,[]);(unvisitedExits(N,E),\+is_empty(E), getSafeRoom(E,RM),RM=nope)),
	rooms([_|T]),
	roomsInMap(L),
	findRoom(L,Room),
	\+Room=nel,
	go_back(Room,T),
	print_description,
	check_status,!.



use_items:-
	useVampireItem,
	pour,
	useBook,
	placeHelmet,
	placeCloak,
	placeGear,
	useSword,
	useBrassKey,
	useSilverKey,
	useDragonKey,
	useStone,
	useCandle,
	winGame.

useVampireItem:-
	in(juanito,N),
	((hasVampireRoom,hasVampireItem) ->
			items(I),
			room(N,_,A,_,_),
			getVampireItem(V), 
			format("Kill the vampire with ~p~n", V),
			delete(A,vampire,L),
			retract((room(N,D,_,O,E))),
			assertz((room(N,D,L,O,E))),
			vampireRooms(RM),
			delete(RM,N,NL),
			retract(vampireRooms(RM)),
			assertz(vampireRooms(NL));
			F is 1
		).

hasVampireRoom:-
	in(juanito,N),
	room(N,_,A,_,_),
	contains(vampire,A).

getVampireItem(garlic):-items(L),contains(garlic,L),remove_item(garlic),!.
getVampireItem(crucifix):-items(L),contains(crucifix,L),remove_item(crucifix),!.
getVampireItem(holyWater):-items(L),contains(holyWater,L),remove_item(holyWater),!.
getVampireItem(woodenStake):-items(L),contains(woodenStake,L),remove_item(woodenStake),!.

go_back([]). 
go_back(R,[R|T]):- retractall(in(juanito,_)),assert(in(juanito,R)),addRoom(R),use_items.
go_back(Room,[H|T]):- retractall(in(juanito,_)),assert(in(juanito,H)),addRoom(H),use_items, go_back(Room,T).

pour:-
	in(juanito,N),
	items(I),
	((N=courtyard,contains(magicPotion,I)) ->
		format("Use magic potion into fountain~n"),
		retract(object(cloak,D,H,A,C,E)),
		assertz(object(cloak,D,visible,A,C,E)),
		get_items,
		remove_item(magicPotion),
		retractall(secretItems(L)),assert(secretItems([cloak|L]));
		F is 1
	).

useBook:- 
	in(juanito,N),
	items(I),
	((N=crypt,contains(ancientBook,I)) ->
		format("Use ancient book to get silver key~n"),
	
		retract(object(silverKey,D,H,unlock(silverKey,bigCellDoor),A,E)),
		assertz(object(silverKey,D,visible,unlock(silverKey,bigCellDoor),A,E)),

		retract(object(silverKey,DE,HI,unlock(silverKey,smallCellDoor),AG,EX)),
		assertz(object(silverKey,DE,visible,unlock(silverKey,smallCellDoor),AG,EX)),

		get_items,
		remove_item(ancientBook),
		retract(secretItems(L)),assertz(secretItems([cloak|L]));
		F is 1
	).

placeHelmet:-
	in(juanito,N),
	items(I),
	((N=diningHall,contains(helmet,I)) ->
		format("Place helmet on armor - unlock weapon room~n"),
		unlock(weaponRoom),
		remove_item(helmet);
		F is 1
	).

placeCloak:-
	in(juanito,N),
	items(I),
	((N=westStairs,contains(cloak,I)) ->
		format("Place cloak on statue~n"),
		unlock(westStairs),
		remove_item(cloak);
		F is 1
	).

placeGear:-
	in(juanito,N),
	items(I),
	((N=clockTower,contains(gear,I)) ->
		format("Place gear in clock ~n"),
		unlock(treasureRoom),
		remove_item(gear);
		F is 1
	).

useSword:-
	in(juanito,N),
	items(I),
	((N=artRoom,contains(rustySword,I)) ->
		format("Cut tapestry with rusty sword ~n"),
		unlock(library);
		F is 1
	).

useBrassKey:-
	in(juanito,N),
	items(I),
	((N=royalChamber,contains(brassKey,I)) ->
		format("Open door with brass key~n"),
		unlock(storage),
		remove_item(brassKey);
		F is 1
	).

useSilverKey:-
	in(juanito,N),
	items(I),
	((N=prison,contains(silverKey,I)) ->
		format("Unlock small cell & big cell door~n"),
		unlock(cells),
		remove_item(silverKey);
		F is 1
	).

useDragonKey:-
	in(juanito,N),
	items(I),
	((N=lobby,contains(dragonKey,I)) ->
		format("Unlock chest with dragon key~n"),
		retract(object(woodenStake,D,H,A,C,E)),
		assertz(object(woodenStake,D,visible,A,C,E)),
		get_items,
		remove_item(dragonKey),
		retract(secretItems(L)),assertz(secretItems([cloak|L])) ;
		F is 1
	).

useStone:-
	in(juanito,N),
	items(I),
	((N=bedroom,contains(stone,I)) ->
		format("Break mirror with stone~n"),
		unlock(secretStairs),
		remove_item(stone);
		F is 1
	).

useCandle:-
	in(juanito,N),
	items(I),
	((N=secretStairs,contains(candle,I)) ->
		format("Use candle to unlock throne room~n"),
		unlock(throneRoom),
		remove_item(candle);
		F is 1
	).


winGame:-
	in(juanito,N),
	items(I),
	((N=throneRoom,contains(crown,I),contains(rustySword,I)) ->
		format("Place crown on body~n"),
		format("The Vampire King comes to life!~n"),
		format("You use the rusty sword to shred the curtains!~n"),
		format("The Vampire King is DEAD!~n"),
		remove_item(crown),
		unlock(win);
		F is 1
	).

unlock(weaponRoom):-
	retract(room(diningHall,D,A,O,E)),
	assertz(room(diningHall,D,A,O,[exit(kitchen,visible,unlocked),exit(weaponRoom,visible,unlocked), exit(courtyard,visible,unlocked)])).
	
unlock(westStairs):-
	retract(room(westStairs,D,A,O,E)),
	assertz(room(westStairs,D,A,O,[exit(lobby,visible,unlocked),exit(royalChamber,visible,unlocked)])).

unlock(library):-
	retract(room(artRoom,D,A,O,E)),
	assertz(room(artRoom,D,A,O,[exit(chapel, visible, unlocked), exit(library, visible, unlocked)])).

unlock(treasureRoom):-
	retract(room(clockTower,D,A,O,E)),
	assertz(room(clockTower,D,A,O,[exit(treasureRoom,visible,unlocked), exit(royalChamber,visible,unlocked)])).

unlock(storage):-
	retract(room(royalChamber,D,A,O,E)),
	assertz(room(royalChamber,D,A,O,[exit(clockTower,visible,unlocked),exit(storage,visible,unlocked), exit(bedroom,visible,unlocked), exit(westStairs,visible,unlocked)])).

unlock(secretStairs):-
	retract(room(bedroom,D,A,O,E)),
	assertz(room(bedroom,D,A,O,[exit(royalChamber,visible,unlocked),exit(secretStairs,visible,unlocked)])).

unlock(throneRoom):-
	retract(room(secretStairs,D,A,O,E)),
	assertz(room(secretStairs,D,A,O,[exit(bedroom,visible,unlocked),exit(throneRoom,visible,unlocked)])).

unlock(win):-
	retract(room(throneRoom,D,A,O,E)),
	assertz(room(throneRoom,D,A,O,[exit(secretStairs,visible,unlocked),exit(win,visible,unlocked)])).

unlock(cells):-
	retract(room(prison,D,A,O,E)),
	assertz(room(prison,D,A,O,[exit(basement,visible,unlocked),exit(bigCell,visible,unlocked), exit(smallCell,visible,unlocked)])).

unlock(diningHall):-
	retract(room(diningHall,D,A,O,E)),
	assertz(room(diningHall,D,A,O,[exit(kitchen,visible,unlocked),exit(weaponRoom,visible,unlocked), exit(courtyard,visible,unlocked)])).

remove_item(RI):-
	items(I),
	delete(I,RI,L),
	retract(items(I)), assertz(items(L)).

%------------------------------------------------------------------------------