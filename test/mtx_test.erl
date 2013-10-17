-module(mtx_test).
-import(nosehoover_tools,[mtx_transpose/1, mtx_add/2, mtx_mult/2, mtx_mult_vec/2]).
-import(nosehoover_main,[wilson_matrix/1,vector_to_square_matrix/1]).

-ifdef(TEST).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("records.hrl").



vector_to_square_matrix_test() ->
   ?assertEqual([[1,2,3],[4,5,6],[7,8,9]],vector_to_square_matrix([1,2,3,4,5,6,7,8,9])).

mtx_add_test() ->
   ?assertEqual([[3,3],[3,3]]
               ,mtx_add([[1,1],[1,1]],[[2,2],[2,2]])).

mtx_add_2_test() ->
   ?assertEqual([[3.0,3.1,10.0],[3.0,3.2,10.0]]
               ,mtx_add([[1.0,1.1,2.0],[1.0,1.2,8.0]],[[2.0,2.0,8.0],[2.0,2.0,2.0]])).

mtx_transpose_test()->
  ?assertEqual([[1.0,0.0],[0.0,1.0]],mtx_transpose([[1.0,0.0],[0.0,1.0]])).

mtx_mul_test() ->
  ?assertEqual([[1.0,2.0],[0.0,3.0]],mtx_mult([[1.0,2.0],[0.0,3.0]],[[1.0,0.0],[0.0,1.0]])).

mtx_mul_vec_test() ->
  ?assertEqual([1.0,0.0],mtx_mult_vec([[1.0,2.0],[0.0,3.0]],[1.0,0.0])).

is_equal_value(X,Y) when is_float(X),is_float(Y) -> math:sqrt((X-Y)*(X-Y)) < 0.000001 ;
is_equal_value(X,Y) -> X =:= Y.

is_equal(A,B) when is_list(A), is_list(B) ->
  (length(A) == length(B)) andalso ( true == lists:foldl(fun(_,false) -> false; ({X,Y},true) -> is_equal_value(X,Y) end, true, lists:zip(A,B))).

is_equal_list(A,B) when is_list(A), is_list(B) ->
  (length(A) == length(B)) andalso ( true == lists:foldl(fun(_,false) -> false; ({X,Y},true) -> is_equal(X,Y) end, true, lists:zip(A,B))).
  
mtx_wilson_test() ->
    A = #molecule{ position_cart=[[0.0,0.0,0.0],[0.00000000e+00,  0.00000000e+00, 2.05791176e+00],[1.94021962e+00,  0.00000000e+00, -6.85963027e-01],[-9.70109808e-01, -1.68027834e+00, -6.85970586e-01]],
    conex =[{bond,2,1},{bond,3,1},{bond,4,1},{angle,3,1,2},{angle,4,1,3},{dihedral,4,1,3,2}]},
    B =  [ 
 [  0.000000e+00,  0.000000e+00,  -1.000000e+00,  0.000000e+00,  0.000000e+00,  1.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00  ] ,
 [  -9.428103e-01,  0.000000e+00,  3.333298e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00,  9.428103e-01,  0.000000e+00,  -3.333298e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00  ] ,
 [  4.714048e-01,  8.164965e-01,  3.333332e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  -4.714048e-01,  -8.164965e-01,  -3.333332e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00 ] ,
 [  6.479043e-01,  -0.000000e+00,  4.581395e-01,  -4.859295e-01,  0.000000e+00,  2.775558e-17,  -1.619748e-01,  0.000000e+00,  -4.581395e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00,  0.000000e+00  ] ,
 [  3.239542e-01,  -5.611044e-01,  -4.581373e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00,  8.098726e-02,  4.208277e-01,  2.290693e-01,  -4.049415e-01,  1.402767e-01,  2.290680e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00 ] ,
 [  -1.983783e-01,  1.030809e+00,  -5.611053e-01,  -0.000000e+00,  -5.154054e-01,  -0.000000e+00,  4.959484e-02,  -2.577015e-01,  1.402771e-01,  1.487835e-01,  -2.577026e-01,  4.208283e-01,  0.000000e+00,  0.000000e+00,  0.000000e+00 ]
   ],
	is_equal_list(B,wilson_matrix(A)).