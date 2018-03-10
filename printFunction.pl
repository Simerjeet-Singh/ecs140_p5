printing(S1,S2,State0,State1,State2,State3,State4,State5,State6,State7,
State8,State9,State10,State11,State12,State13,State14,State15):-

  (

    (S1=State0,S2=State9) -> write('Take(Cabbage,left,right)');
    ((S1=State0,S2=State10) -> write('Take(Goat,left,right)');
    ((S1=State0,S2=State12) -> write('Take(Fox,left,right)');

    ((S1=State10,S2=State2) -> write('Take(Farmer,right,left)');
    ((S1=State2,S2=State14) -> write('Take(Wolf,left,right)');
    ((S1=State14,S2=State4) -> write('Take(Goat,right,left)');
    ((S1=State4,S2=State13) -> write('Take(Cabbage,left,right)');
    ((S1=State13,S2=State5) -> write('Take(Farmer,right,left)');
    ((S1=State5,S2=State15) -> write('Take(Goat,right,left)');

    write('Shouldnt enter here for best case!!')))))))))
  ),!.

printCheck([]).
printCheck([State0|T]) :-
  [State1|T1] = T,
  printing(State0,State1,state(left,left,left,left),
  state(left,left,left,right),state(left,left,right,left),
  state(left,left,right,right),state(left,right,left,left),
  state(left,right,left,right),state(left,right,right,left),
  state(left,right,right,right),state(right,left,left,left),
  state(right,left,left,right),state(right,left,right,left),
  state(right,left,right,right),state(right,right,left,left),
  state(right,right,left,right),state(right,right,right,left),
  state(right,right,right,right)),
  printCheck(T).
