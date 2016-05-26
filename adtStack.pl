% adtStack.pl
% David Novick
% 2-01-16

% ADT Stack

% newStack/1
% newStack(Stack) returns true if Stack is instantiated to a new stack
newStack([ ]).

% isEmptyStack/1
% isEmptyStack(Stack) returns true if Stack is empty
isEmptyStack([ ]).

% push/3
% push(Element, OldStack, NewStack) returns true if NewStack is the
% result of pushing Element onto OldStack
push(E, S, [E|S]).

% pop/3
% pop(Element, OldStack, NewStack) returns true if NewStack is the
% result of popping Element from OldStack
pop(E, [E|S], S).

% peekStack/2
% peekStack(Element, Stack) returns true if Element is at the top of
% Stack
peekStack(E,[E|_]).