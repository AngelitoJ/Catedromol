-module(propertest).

-include_lib("proper/include/proper.hrl").    


prop_delete() ->
  ?FORALL(L,list(int()), 
    ?IMPLIES(L /= [],
       ?FORALL(I,elements(L), 
          not lists:member(I,lists:delete(I,L))))).