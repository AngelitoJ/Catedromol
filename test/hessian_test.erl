-module(hessian_test).
-import(nosehoover_main,[build_2d_index/1,enumerate_2d/1,span_row_entries/2,span_submatrix/1,extract_hessian/2]).


-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.


