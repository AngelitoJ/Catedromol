-module(membership_test).
-compile(export_all).
-import(group_membership,[init_membership/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. 



mb_dict_init_test() ->
    TestDict = dict:from_list([
                             {phase,reclaim}
                            ,{1,private}
                            ,{2,private}
                            ,{3,private}
                            ,{4,private}
                            ]),
    ?assertEqual(
                dict:to_list(TestDict)
                ,dict:to_list(init_membership([[1,2,3,4]]))
                ).

mb_dict_init_2_test() ->
    TestDict = dict:from_list([
                             {phase,reclaim}
                            ,{1,shared}
                            ,{2,private}
                            ,{3,private}
                            ,{4,private}
                            ,{5,private}
                            ,{6,private}
                            ,{7,private}
                            ]),
    ?assertEqual(
                dict:to_list(TestDict)
                ,dict:to_list(init_membership([[1,2,3,4],[1,5,6,7]]))
                ).