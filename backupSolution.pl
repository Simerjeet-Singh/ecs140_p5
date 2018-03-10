
  aggeturo(K,[H|T],Final,LocalHolder) :-
    %write('Entered agge turo'),nl,
    %write(' K '),write(K),write(' T '),write(T),write(' Final '),write(Final),
    %write(' localholder '),write(LocalHolder),write(' H '),write(H),nl,
    (
      \+(isItTrue(K,[H|T],Final,LocalHolder)) ->
        %write(' K '),write(K),write(' T '),write(T),write(' Final '),write(Final),
        %write(' localholder '),write(LocalHolder),nl,
        append(LocalHolder,[H],X),
        aggeturo(K,T,Final,X)
      ;
      isItTrue(K,[H|T],Final,LocalHolder)
   ).



  isItTrue(K, T, Final,LocalHolder) :-
      %write(' K '),write(K),write(' T '),write(T),write(' Final '),write(Final),
      %write(' localholder '),write(LocalHolder),nl,
    	append(K, L, T),
    	append(K, LocalHolder, X),
    	append(L, X, Final).

    swap_prefix_suffix(L1,L2,S) :-
       aggeturo(L1,L2,S,[]) .
