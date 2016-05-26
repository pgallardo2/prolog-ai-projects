% -*- Mode: Prolog -*-
:- use_module(juanito,[juanito/1]).
:- use_module(library(writef)).
% The agent is in a different module.
% Need predicate with name of agent(MyAgent).
% Store memory within module so doesn't clash with someone else's memory 
:-[map].
:-[obj].
:- dynamic game/1.

game(playing).

% Driver for Final Project
% David Novick, March 30, 2016

% play/1
% Main driver for play with one agent

% The agent should have a predicate callable by the driver
% using the name of the agent as the predicate.
% For example, an agent named fred would have a predicate fred(FredsActs),
% where on this turn Fred, based on the preceeding
% move, plus the state of the game, and produces new acts
% FredsActs, which is a list of physical acts.
%
% play2 halts when game(playing) is changed to game(done).
%
play(_Agent) :-
	game(done).
play(Agent) :-
	call(Agent, Acts), !,   % e.g., === myAgent(Acts)
	printActs(Agent, Acts),
	% If the agents win the game, the agent should assert 'game(done)'.
	play(Agent).

% printActs/2
% Given the name of an agent and a list of acts, prints each act, line by line. For example:
% fred: light(candle)
%
printActs(_Agent, []).
printActs(Agent, [Act|Acts]) :-
	writef("%w: %w\n", [Agent, Act]),
	printActs(Agent, Acts).