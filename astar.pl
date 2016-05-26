% astar.pl
% Paola Gallardo, Michelle Afravi, Fardos Aboargob, James Bias
% 3-14-2016
% A* implememntation that solves 8 puzzle
% to run: astar([1,0,3,4,2,5,7,8,6],[1,2,3,4,5,6,7,8,0])

% load ADT Heap
:- [heap].

% astar/2
% astar(StartState,GoalState).
% A* search from StartState to GoalState (the same from dfs and bfs)
%
% This clause sets up search with fringe containing node for StartState, list of explored states 
% containing StartState, a variable for the EndNode, and the GoalState. When dfs/4 succeeds, 
% astar/2 prints the path from the StartState to the GoalState.
% A node contains a state, the action that produced the state, and node's parent node.
%

astar(StartState,GoalState) :-
  astar([node(StartState,start,nil)],[StartState],EndNode,GoalState),
  printPath(EndNode).

% astar/4
% astar(Fringe,ExploredStates,EndNode,GoalState).
%
% Main driver for the A* search. Successful search instantiates EndNode to the node
% where the search reached the goal.
%
% If the fringe is empty, there are no more nodes to expand, and thus the search has failed.
%

astar(Fringe,_Explored,node(search_failed,nil),_GoalState) :-
  isEmptyHeap(Fringe).

%
% If the oldest node in the fringe is the GoalState, then the search succeeds.
%

astar(Fringe,_Explored,Node,GoalState) :-
  peekHeap(Node,Fringe),
  isGoalState(Node,GoalState).

%
% Otherwise, expand the oldest node in the fringe and continue the search.
%
astar(Fringe,Explored,EndNode,GoalState) :-
  delete(Node,Fringe,Fringe2),
  expand(Node,Explored,NewExplored,Fringe2,NewFringe),
  astar(NewFringe,NewExplored,EndNode,GoalState).

% isGoalState/2
% isGoalState(CurrentNode,GoalState).
% Succeed if node's state is the goal state
%

isGoalState(node(State,_Action,_Parent),State).

% expand/5
% Given a node and list of explored states, find the children of the node, and update the list
% of unexplored nodes and the fringe.
% 

expand(Node,Explored,NewExplored,Fringe,NewFringe) :-
  findall(N,parent(Node,N),NewNodes),
  addNewNodes(NewNodes,Explored,NewExplored,Fringe,NewFringe).

% addNewNodes/5
% Given a list of newly generated nodes, update the list of explored states and the fringe
%
% If there are no new nodes, the list of explored states and the fringe both stay the same.
%

addNewNodes([],Explored,Explored,Fringe,Fringe).

%
% If a node represents a state that has already been explored, do not add it to the list of explored
% states or to the fringe, and add any remaining nodes.
%

addNewNodes([Node|Nodes],Explored,NewExplored,Fringe,NewFringe) :-
	stateAlreadyExplored(Node,Explored),
	addNewNodes(Nodes,Explored,NewExplored,Fringe,NewFringe).

%
% Otherwise the node represents a new state, so add the state to the list of explored states,
% add the node to the fringe, and add any remaining nodes.
%

addNewNodes([Node|Nodes],Explored,NewExplored,Fringe,NewFringe) :-
	addStateToExplored(Node,Explored,Explored2),
	insert(Node,Fringe,Fringe2),
	addNewNodes(Nodes,Explored2,NewExplored,Fringe2,NewFringe).

stateAlreadyExplored(node(State,_Action,_Parent),Explored) :-
	member(State,Explored).

% addStateToExplored/3
%
% Given a node, add the node's state to the list of explored states.
%
addStateToExplored(node(State,_Actions,_Parent),Explored,[State|Explored]).

% parent/2
%
% Returns true if the second argument is a child node of the first argument. A child node is a 
% node that has a valid state transition from the parent to the child.
% Each node has two arguments: the state, and the node's parent node. So the parent of the
% the current node's child is the current node.
%9

parent(node(State,OldAction,OldParent),node(NewState,Action,node(State,OldAction,OldParent))) :-
	transition(State,NewState,Action).

% transition/3
% transition(ParentState,ChildState, Action).
% Presents the legal transitions between states. This depends on the specifics of the problem.

%
% YOUR CODE FOR THE SPECIFIC PROBLEM GOES HERE
%

%Transitions are based on the location of the blank space, in this case denoted by a 0
%You can either move right, left, up, or down, with six options based on the location of the blank

transition([0,A,B,C,D,E,F,G,H],[A,0,B,C,D,E,F,G,H],moveright).
transition([A,0,B,C,D,E,F,G,H],[A,B,0,C,D,E,F,G,H],moveright).
transition([A,B,C,0,D,E,F,G,H],[A,B,C,D,0,E,F,G,H],moveright).
transition([A,B,C,D,0,E,F,G,H],[A,B,C,D,E,0,F,G,H],moveright).
transition([A,B,C,D,E,F,0,G,H],[A,B,C,D,E,F,G,0,H],moveright).
transition([A,B,C,D,E,F,G,0,H],[A,B,C,D,E,F,G,H,0],moveright).

transition([A,0,B,C,D,E,F,G,H],[0,A,B,C,D,E,F,G,H],moveleft).
transition([A,B,0,C,D,E,F,G,H],[A,0,B,C,D,E,F,G,H],moveleft).
transition([A,B,C,D,0,E,F,G,H],[A,B,C,0,D,E,F,G,H],moveleft).
transition([A,B,C,D,E,0,F,G,H],[A,B,C,D,0,E,F,G,H],moveleft).
transition([A,B,C,D,E,F,G,0,H],[A,B,C,D,E,F,0,G,H],moveleft).
transition([A,B,C,D,E,F,G,H,0],[A,B,C,D,E,F,G,0,H],moveleft).

transition([A,B,C,0,D,E,F,G,H],[0,B,C,A,D,E,F,G,H],moveup).
transition([A,B,C,D,0,E,F,G,H],[A,0,C,D,B,E,F,G,H],moveup).
transition([A,B,C,D,E,0,F,G,H],[A,B,0,D,E,C,F,G,H],moveup).
transition([A,B,C,D,E,F,0,G,H],[A,B,C,0,E,F,D,G,H],moveup).
transition([A,B,C,D,E,F,G,0,H],[A,B,C,D,0,F,G,E,H],moveup).
transition([A,B,C,D,E,F,G,H,0],[A,B,C,D,E,0,G,H,F],moveup).

transition([0,A,B,C,D,E,F,G,H],[D,A,B,C,0,E,F,G,H],movedown).
transition([A,0,B,C,D,E,F,G,H],[A,D,B,C,0,E,F,G,H],movedown).
transition([A,B,0,C,D,E,F,G,H],[A,B,F,C,D,E,0,G,H],movedown).
transition([A,B,C,0,D,E,F,G,H],[A,B,C,F,D,E,0,G,H],movedown).
transition([A,B,C,D,0,E,F,G,H],[A,B,C,D,G,E,F,0,H],movedown).
transition([A,B,C,D,E,0,F,G,H],[A,B,C,D,E,H,F,G,0],movedown).


%get the current coordinate of the tile
%getCoord(T,L,X,Y)
getCoord(T,[T,_,_,_,_,_,_,_,_],1,1).
getCoord(T,[_,T,_,_,_,_,_,_,_],1,2).
getCoord(T,[_,_,T,_,_,_,_,_,_],1,3).
getCoord(T,[_,_,_,T,_,_,_,_,_],2,1).
getCoord(T,[_,_,_,_,T,_,_,_,_],2,2).
getCoord(T,[_,_,_,_,_,T,_,_,_],2,3).
getCoord(T,[_,_,_,_,_,_,T,_,_],3,1).
getCoord(T,[_,_,_,_,_,_,_,T,_],3,2).
getCoord(T,[_,_,_,_,_,_,_,_,T],3,3).

%get the coordinate where the tile belongs,
% expectedCoord(X,Y,T) 
expectedCoord(1,1,1).
expectedCoord(1,2,2).
expectedCoord(1,3,3).
expectedCoord(2,1,4).
expectedCoord(2,2,5).
expectedCoord(2,3,6).
expectedCoord(3,1,7).
expectedCoord(3,2,8).
expectedCoord(3,3,0).

%Manhattan distance for the blank space is zero
getManhattanDistance(T,_,0):-T=0.


%Manhattan distance calculation of a tile.
%gets the number, the list (board), and returns the manhattan distance,
%does this by getting the current coordinate of the tile and comparing it to the expected coordinate
getManhattanDistance(T,L,D):-
  \+T=0,
  getCoord(T,L,X,Y),
  expectedCoord(EX,EY,T),
  DiffX is X-EX,
  DiffY is Y-EY,
  abs(DiffX,AX),
  abs(DiffY,AY),
  TSum is AX+AY,
  D is TSum.

%adds the distances of all tiles
getTotalManhattanDistance(L,TD):-
  getManhattanDistance(1,L,AD),
  getManhattanDistance(2,L,BD),
  getManhattanDistance(3,L,CD),
  getManhattanDistance(4,L,DD),
  getManhattanDistance(5,L,ED),
  getManhattanDistance(6,L,FD),
  getManhattanDistance(7,L,GD),
  getManhattanDistance(8,L,HD),
  TD is AD+BD+CD+DD+ED+FD+GD+HD. 

%returns the manhattan distance total of a current state
priority(node(S,_,_),P):-getTotalManhattanDistance(S,P1),P is P1.





% printPath/1
% Recursively prints the search path represented by the chain of parent nodes, printing start 
% node first and ending with the goal node.
%

printPath(nil).
printPath(node(State,Action, Parent)) :- printPath(Parent), write(Action), write(' --> '), writeln(State).

% printList/1
printList([]).
printList([H|T]) :- writeln(H), printList(T).
