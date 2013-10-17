-module(conex_generator_test).


-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.


metano_test_data() ->
   [
      {4,3,2,1}
      ,{5,4,2,1}
   ].


metano_generator_test() ->
   ?assertEqual(
               [
                  {bond,2,1}
                  ,{bond,3,1}
                  ,{bond,4,1}
                  ,{bond,5,1}
                  ,{angle,3,1,2}
                  ,{angle,4,1,3}
                  ,{angle,5,1,2}
                  ,{angle,5,1,4}
                  ,{dihedral,4,1,3,2}
                  ,{dihedral,5,1,4,2}
               ]
               ,conex_generator:generate_conex(metano_test_data())
               ).