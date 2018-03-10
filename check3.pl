opposite(left, right).
opposite(right, left).

%% A state is an indication of which riverbank (left or right) holds the four entities:
%% farmer, wolf, goat, cabbage
state(_, _, _, _).

print_history([]).
print_history([arc(take(Obj, B0, B1), _, State1)|T]) :-
	print_history(T),
	print('take '), print(Obj), print(' from '), print(B0), print(' to '), print(B1), nl,
	print('\t'), print(State1), nl.

%% A move succeeds if
%% - its initial state is a legal state
%% - its terminal state is a legal state
%% - its proposed take is not a member of its history	.
move(State0, State1, History) :-
	safe(State0),
	Arc0 = arc(take(_,_,_), State0, StateIntermediate),
	\+ member(Arc0, History),
	Arc0,
	move(StateIntermediate, State1, [Arc0|History]).
	
move(State0, State1, History) :-
	safe(State0),
	A = arc(_, State0, State1),
	A,
	\+ member(A, History),
	print_history([A|History]),
	!.


safe(state(F, _, G, _)) :- F = G, !.
safe(state(_, W, G, C)) :- opposite(W,G), opposite(G,C).
unsafe(A) :- \+ safe(A).

take(Obj, A, B) :- opposite(A, B).

arc(take(wolf, F0, F1), state(F0, F0, G, C), state(F1, F1, G, C)) :-
	take(wolf, F0, F1),
	safe(state(F1, F1, G, C)).
arc(take(goat, F0, F1), state(F0, W, F0, C), state(F1, W, F1, C)) :-
	take(goat, F0, F1),
	safe(state(F1, W, F1, C)).
arc(take(cabbage, F0, F1), state(F0, W, G, F0), state(F1, W, G, F1)) :-
	take(cabbage, F0, F1),
	safe(state(F1, W, G, F1)).
arc(take(farmer, F0, F1), state(F0, W, G, C), state(F1, W, G, C)) :-
	take(farmer, F0, F1),
	safe(state(F1, W, G, C)).

solve :- move(state(left, left, left, left), state(right, right, right, right), []), !.
