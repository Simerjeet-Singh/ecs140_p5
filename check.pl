findpresent(L1,[],LastAtom,FinalList,OriginalL1,UnRecordedL2,Final) :-
    3=4,
    write('Entered to prove false'),nl,
    !.%return false if list is empty now

findpresent([] , T,LastAtom,FinalList,OriginalL1,UnRecordedL2,Final) :-
  append(OriginalL1,FinalList,P),
  append(T,P,Final)
  .


findpresent([H1|T1],[H2|T2],LastAtom,FinalList,OriginalL1,UnRecordedL2,Final) :- %checkout the list one byone
  (
    write('entered default case reloaded hogya'),nl,
    (
      H1=H2 -> write('entered H1=H2'),nl,
      append(UnRecordedL2,[H2],P),
      write('FinalList '),write(FinalList),write(' H2 '),write(H2),write(' UnRecordedL2 '), write(UnRecordedL2), write(' p '),write(P),nl,
      write(' '), write(H1),write(' '), write(T1), write(' '),write(H2),write(' '),write(T2), nl,nl,nl,
      findpresent(T1,T2,H1,FinalList,OriginalL1,P,Final)
      ;
      LastAtom = H2 -> write('entered LastAtom = h2'),nl,
      append(FinalList,UnRecordedL2,P),
      write('FinalList '),write(FinalList),write(' H2 '),write(H2),nl,
      write(' '), write(H1),write(' '), write(T1), write(' '),write(H2),write(' '),write(T2), nl,nl,nl,
      findpresent(OriginalL1,[H2|T2],[],P,OriginalL1,[],Final)
      ;
      write('else') ,
      write(' '), write(H1),write(' '), write(T1), write(' '),write(H2),write(' '),write(T2), nl,
      append(FinalList,[H2],S),
      append(UnRecordedL2,S,P),
      write('FinalList '),write(FinalList),write(' H2 '),write(H2), write(' p '),write(P), nl,nl,nl,
      findpresent(OriginalL1,T2,[],P,OriginalL1,[],Final)
    )

  ),!.





  sublist( [], _ ).
  sublist( [X|XS], [X|XSS] ) :- sublist( XS, XSS ).
  sublist( [X|XS], [_|XSS] ) :- sublist( [X|XS], XSS ).

  swap(L1,[H|T],L2,Final) :-
     suffix(X,L2),
     prefix(Y,L2),
     append(L1,N,X),
     append(Y,X,L2),
     append(N,L1,G),
     append(G,Y,Final).
