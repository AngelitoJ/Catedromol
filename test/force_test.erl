-module(force_test).
-compile(export_all).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. 

-include("constants.hrl").
-include("enumerate.hrl").
-include("records.hrl").

reverse_enumerate(List) ->
   lists:zip(List,lists:seq(1,length(List))).

sort(Data,OrderedKeys) ->
   Dict   = dict:from_list(reverse_enumerate(OrderedKeys)),
   TaggedData = lists:map(fun({A,_}=E) ->
                                    {ok,Key} = dict:find(A,Dict), {Key,E} end
                           ,Data),
   lists:map(fun({_,E}) -> E end, lists:keysort(1, TaggedData)).

conex() ->
   [{bond,2,1},{bond,3,1},{bond,4,1},{bond,5,1},{angle,3,1,2},{angle,4,1,3},{angle,5,1,2},{angle,5,1,4},{dihedral,4,1,3,2},{dihedral,5,1,4,2}].

unsorted() ->
   [
      {{angle,3,1,2},-9.9669e-7}, {{angle,5,1,2},-1.418337e-7}, {{bond,4,1},0.005291442},
      {{angle,5,1,4},-3.267653e-7}, {{dihedral,5,1,4,2},2.259142e-7}, {{dihedral,4,1,3,2},2.287642e-7},
      {{bond,3,1},0.005290661},{{angle,4,1,3},1.400879e-7}, {{bond,2,1},0.005290983}, {{bond,5,1},0.005291442}].

sorted() ->
   [
      {{bond,2,1},0.005290983}, {{bond,3,1},0.005290661}, {{bond,4,1},0.005291442}, {{bond,5,1},0.005291442}
      ,{{angle,3,1,2},-9.9669e-7}, {{angle,4,1,3},1.400879e-7}, {{angle,5,1,2},-1.418337e-7}, {{angle,5,1,4},-3.267653e-7}
      ,{{dihedral,4,1,3,2},2.287642e-7}, {{dihedral,5,1,4,2},2.259142e-7}
   ].

sort_test() ->
   ?assertEqual(sorted()
                ,sort(unsorted(),conex())).