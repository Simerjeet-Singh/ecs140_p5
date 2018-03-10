%first argument in state(farmer, wolf, goat, cabbage)
state(_,_,_,_).
oppBanks(A,B,X,Y) :-
  (
  A=X -> B=Y
  ;
  A=Y,B=X
  ),!.

sameBank(A,B,X,Y) :-
  (
    A=X -> B=X
    ;
    A=Y,B=Y
  ),!.

sameSide(A,B):-
  sameBank(A,B,left,right),!.

opposite(A,B) :-
  oppBanks(A,B,left,right),!.

unsafe(state(F,W,G,C)) :-
    F \= G, sameSide(G,C); F\=G, sameSide(G,W)
    ,!.

safe(S) :- \+unsafe(S).

take(X,A,B) :- opposite(A,B),!.


arc(take(wolf, A, B), state(A, A, C, D), state(B, B, C, D)):-
   opposite(A, B),
   safe(state(A,A,C,D)),
   safe(state(B,B,C,D)).

arc(take(goat, A, B), state(A, C, A, D), state(B, C, B, D)):-
  opposite(A, B),
  safe(state(A,C,A,D)),
  safe(state(B,C,B,D)).

arc(take(farmer, A, X), state(A, B, C, D), state(X, B, C, D)):-
  opposite(A, X),
  safe(state(A,B,C,D)),
  safe(state(X,B,C,D)).

arc(take(cabbage, A, B), state(A, C, D, A), state(B, C, D, B)):-
  opposite(A,B),
  safe(state(A,C,D,A)),
  safe(state(B,C,D,B)).

printing(S1,S2,State0,State1,State2,State3,State4,State5,State6,State7,
  State8,State9,State10,State11,State12,State13,State14,State15):-

    (

      (S1=State0,S2=State9) -> write('Take(Cabbage,left,right)'),nl;
      ((S1=State0,S2=State10) -> write('Take(Goat,left,right)'),nl;
      ((S1=State0,S2=State12) -> write('Take(Fox,left,right)'),nl;

      ((S1=State10,S2=State2) -> write('Take(None,right,left)'),nl;
      ((S1=State2,S2=State14) -> write('Take(Wolf,left,right)'),nl;
      ((S1=State14,S2=State4) -> write('Take(Goat,right,left)'),nl;
      ((S1=State4,S2=State13) -> write('Take(Cabbage,left,right)'),nl;
      ((S1=State13,S2=State5) -> write('Take(None,right,left)'),nl;
      ((S1=State5,S2=State15) -> write('Take(Goat,right,left)'),nl;
      ((S1=State2,S2=State11) -> write('Take(Cabbage,left,right)'),nl;
      ((S1=State11,S2=State01) -> write('Take(Goat,right,left)'),nl;
      ((S1=State1,S2=State13) -> write('Take(Fox,left,right)'),nl;
      write('Shouldnt enter here for best case!!'),nl)))))))))))
    ),!.

printCheck([]).
printCheck([H|[]]).
printCheck([State0|T]) :-
    [State1|T1] = T,
    %write(' S0 '),write(State0),write(' S1 '),write(State1),nl,
    printing(State0,State1,state(left,left,left,left),
    state(left,left,left,right),state(left,left,right,left),
    state(left,left,right,right),state(left,right,left,left),
    state(left,right,left,right),state(left,right,right,left),
    state(left,right,right,right),state(right,left,left,left),
    state(right,left,left,right),state(right,left,right,left),
    state(right,left,right,right),state(right,right,left,left),
    state(right,right,left,right),state(right,right,right,left),
    state(right,right,right,right)),
    printCheck(T),!.

reverseList([],[]).
reverseList([H|T],L1) :-
  reverseList(T,L2), append(L2,[H],L1).


printFunction([]).

printFunction([State0|T]) :-
  write(State0),nl,printFunction(T),!.


go(State2,State2,ListOfMoves) :-%base case, we have reached state2
  safe(State2),
  Y=[State2|ListOfMoves],
  reverseList(Y,X),
  printCheck(X).%print list of moves

go(State1,State2,ListOfMoves) :-
  safe(State1), safe(State2),
  arc(_,State1,LocalState),
  \+(member(State1,ListOfMoves)),
  safe(LocalState),
  go(LocalState,State2,[State1|ListOfMoves]).


solve :-
  go(state(left, left, left, left), state(right, right, right, right),[]),!.
