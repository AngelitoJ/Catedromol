-module(printing_test).
-author("angel@uah.es").
-compile([export_all]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
 
-include("enumerate.hrl").
-include("print.hrl").

enumerate0_test() ->
    ?assertEqual([{0,pedro},{1,juan},{2,pablo}],
                enumerate0([pedro,juan,pablo])).

enumerate_test() ->
    ?assertEqual([{1,pedro},{2,juan},{3,pablo}],
                enumerate([pedro,juan,pablo])).
calcular_nivel_reporte_test() ->
    ?assertEqual( [error],
                nosehoover_core:calcular_nivel_reporte([{debug,0}]) ),
    ?assertEqual( [error,warning],
                nosehoover_core:calcular_nivel_reporte([{debug,1}]) ),
    ?assertEqual( [error,warning,info],
                nosehoover_core:calcular_nivel_reporte([{debug,2}]) ),
    ?assertEqual( [error,warning,info,debug],
                nosehoover_core:calcular_nivel_reporte([{debug,3}]) ).
-endif.