% -*- Mode: Prolog -*-
:- use_module(juanito,[juanito/3]).
:- use_module(paquito,[paquito/3]).
:- use_module(library(writef)).
% agent is in a different module.
% need predicate with name of agent(MyAgent). agentNameOf(variable, otheragentsvariable, actiongoingtotake)
% store memory within module so doesn't clash with someone else's memory 


% Driver for final project: Dialog between two agents
% David Novick, March 30, 2016

% agents/2
% Set up main driver for dialog between Agent1 and Agent2.
% List of acts from prior turn is empty at start of game.
%
agents(Agent1, Agent2) :-
	agents([ ],Agent1,Agent2).

% agents/3
% Main driver for dialog between Agent1 and Agent2
% Alternately calls on agents to act.
%
% Each agent should have a predicate callable by the driver using the name of the agent as the predicate.
% For example, an agent named fred would have a predicate fred(OtherAgent, OtherAgentsActs, FredsActs),
% where on this turn Fred, based on the the other agent's acts, both physical and dialog, from the preceeding
% turn, plus the state of the game, and produces new acts FredsActs, which is a list of physical acts
% and/or dialog acts.
% game/1
% agents/3 halts when game(playing) is changed to game(done).
%
% Note that Acts1 and Acts2 should be lists

agents(_Acts, _Agent1, _Agent2) :-
	game(done).
agents(Acts2, Agent1, Agent2) :-
	call(Agent1, Agent2, Acts2, Acts1), !,   % e.g., === myAgent(Agent2, Acts2, Acts1)
	printActs(Agent1, Acts1),
	% If the agents win the game, the agent should assert 'game(done)'.
	sleep(1),
	agents(Acts1,Agent2,Agent1).
game(playing).

% printActs/2
% Given the name of an agent and a list of acts, prints each act, line by line. For example:
% fred: light(candle)
% fred: greet(fred, barney)
%
printActs(_Agent, []).
printActs(Agent, [Act|Acts]) :-
	writef("%w: %w\n", [Agent, Act]),
	printActs(Agent, Acts).
	
:-dynamic room/5.
:-dynamic object/6.