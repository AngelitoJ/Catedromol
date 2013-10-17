-module(dynamics_test).
-compile(export_all).

% -include("records.hrl").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-include("records.hrl").

group1() -> #molecule{ 
                     mass=[1,1,1,1]
                     ,vel=[[1,0,0],[1,0,0],[1,0,0],[1,0,0]]
                     ,members={[1],[2,3,4],[]}
                  }.
group2() -> #molecule{
                     mass=[1,1,1,1]
                     ,vel=[[1,0,0],[1,0,0],[1,0,0],[1,0,0]]
                     ,members={[1],[],[2,3,4]}
                  }.

kinetic_energy_1__test() ->
   M = group2(),
    ?assertEqual({1,0.5},nosehoover_main:group_kinetic_energy(M)).

kinetic_energy_2_test() ->
   M = group1(),
    ?assertEqual({4,2.0},nosehoover_main:group_kinetic_energy(M)).


-endif.