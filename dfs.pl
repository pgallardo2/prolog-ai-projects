% dfs.pl
% David Novick
% 2-18-16
% breadth-first search

% load ADT Stack
:- [adtStack].

% dfs/2
% dfs(StartState,GoalState).
% Breadth-first search from StartState to GoalState
%
% This clause sets up search with fringe containing node for StartState, list of explored states 
% containing StartState, a variable for the EndNode, and the GoalState. When dfs/4 succeeds, 
% dfs/2 prints the path from the StartState to the GoalState.
% A node contains a state, the action that produced the state, and node's parent node.
%

dfs(StartState,GoalState) :-
  dfs([node(StartState,start,nil)],[StartState],EndNode,GoalState),
  printPath(EndNode).

% dfs/4
% dfs(Fringe,ExploredStates,EndNode,GoalState).
%
% Main driver for the breadth-first search. Successful search instantiates EndNode to the node
% where the search reached the goal.
%
% If the fringe is empty, there are no more nodes to expand, and thus the search has failed.
%

dfs(Fringe,_Explored,node(search_failed,nil),_GoalState) :-
  isEmptyStack(Fringe).

%
% If the oldest node in the fringe is the GoalState, then the search succeeds.
%

dfs(Fringe,_Explored,Node,GoalState) :-
  peekStack(Node,Fringe),
  isGoalState(Node,GoalState).

%
% Otherwise, expand the oldest node in the fringe and continue the search.
%
dfs(Fringe,Explored,EndNode,GoalState) :-
  pop(Node,Fringe,Fringe2),
  expand(Node,Explored,NewExplored,Fringe2,NewFringe),
  dfs(NewFringe,NewExplored,EndNode,GoalState).

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
	push(Node,Fringe,Fringe2),
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
%

parent(node(State,OldAction,OldParent),node(NewState,Action,node(State,OldAction,OldParent))) :-
	transition(State,NewState,Action).

% transition/3
% transition(ParentState,ChildState, Action).
% Presents the legal transitions between states. This depends on the specifics of the problem.

%
% YOUR CODE FOR THE SPECIFIC PROBLEM GOES HERE
%
transition([[H1,O1],[H2,O2],w],[[NH1,O1],[NH2,O2],e],[-1,0]):-
  NH1 is H1-1,
  NH2 is H2+1,
  legal([[NH1,O1],[NH2,O2],e]).

transition([[H1,O1],[H2,O2],w],[[H1,NO1],[H2,NO2],e],[0,-1]):-
  NO1 is O1-1,
  NO2 is O2+1,
  legal([[H1,NO1],[H2,NO2],e]).

transition([[H1,O1],[H2,O2],w],[[NH1,NO1],[NH2,NO2],e],[-1,-1]):-
  NH1 is H1-1,
  NO1 is O1-1,
  NH2 is H2+1,
  NO2 is O2+1,
  legal([[NH1,NO1],[NH2,NO2],e]).

transition([[H1,O1],[H2,O2],w],[[NH1,O1],[NH2,O2],e],[-2,0]):-
  NH1 is H1-2,
  NH2 is H2+2,
  legal([[NH1,O1],[NH2,O2],e]).

transition([[H1,O1],[H2,O2],w],[[H1,NO1],[H2,NO2],e],[0,-2]):-
  NO1 is O1-2,
  NO2 is O2+2,
  legal([[H1,NO1],[H2,NO2],e]).

transition([[H1,O1],[H2,O2],e],[[NH1,O1],[NH2,O2],w],[1,0]):-
  NH1 is H1+1,
  NH2 is H2-1,
  legal([[NH1,O1],[NH2,O2],e]).

transition([[H1,O1],[H2,O2],e],[[H1,NO1],[H2,NO2],w],[0,1]):-
  NO1 is O1+1,
  NO2 is O2-1,
  legal([[H1,NO1],[H2,NO2],e]).

transition([[H1,O1],[H2,O2],e],[[NH1,O1],[NH2,O2],w],[2,0]):-
  NH1 is H1+2,
  NH2 is H2-2,
  legal([[NH1,O1],[NH2,O2],e]).

transition([[H1,O1],[H2,O2],e],[[H1,NO1],[H2,NO2],w],[0,2]):-
  NO1 is O1+2,
  NO2 is O2-2,
  legal([[H1,NO1],[H2,NO2],e]).

transition([[H1,O1],[H2,O2],e],[[NH1,NO1],[NH2,NO2],w],[1,1]):-
  NH1 is H1+1,
  NO1 is O1+1,
  NH2 is H2-1,
  NO2 is O2-1,
  legal([[NH1,NO1],[NH2,NO2],e]).

legal([[H1,O1],[H2,O2],_]):-
  (H1>=O1;H1=0),
  (H2>=O2;H2=0),
  H1=<3,H1>=0,
  O1=<3,O1>=0,
  H2=<3,H2>=0,
  O2=<3,O2>=0.



% printPath/1
% Recursively prints the search path represented by the chain of parent nodes, printing start 
% node first and ending with the goal node.
%

printPath(nil).
printPath(node(State,Action, Parent)) :- printPath(Parent), write(Action), write(' --> '), writeln(State).

% printList/1
printList([]).
printList([H|T]) :- writeln(H), printList(T).
