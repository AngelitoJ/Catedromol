-module(random_gauss_test).
-ifdef(TEST).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-include("random_gauss.hrl").

% distribution stats (for 68-95-99 rule)
normal_stats(Values,M,S) ->
    T = length(Values),
    {S1,R1} = lists:partition(fun(Z1) -> (Z1 > M-S) and (Z1 < M+S) end,Values),
    {S2,R2} = lists:partition(fun(Z2) -> (Z2 > M-2.0*S) and (Z2 < M+2.0*S) end,R1), 
    {S3,_}  = lists:partition(fun(Z2) -> (Z2 > M-3.0*S) and (Z2 < M+3.0*S) end,R2),  
    A = length(S1)/T,
    B = A + length(S2)/T,
    C = B + length(S3)/T,
    {A,B,C}.

boxmuller_test() ->
    boxmuller_test(100000,1.0,0.5).

% Simple 68,95,99 rule test for normality
boxmuller_test(N,DM,DS) ->
    Values = [bm_gauss(DM,DS) || _ <-lists:seq(1,N)],
    M = lists:sum(Values) / N,
    S = math:sqrt(lists:sum([(X - M)*(X - M) || X <- Values]) / N),
    MT = math:sqrt((M-DM)*(M-DM)),
    ST = math:sqrt((S-DS)*(S-DS)),
    {A,B,C} = normal_stats(Values,M,S),
    (MT < 0.1) and (ST < 0.1) 
    and (A > 0.68) % expected a 0.68 coverage ratio 
    and (B > 0.95) % expected a 0.95 coverage ratio
    and (C > 0.99).% expected a 0.99 coverage ratio

polar_test() ->
    polar_test(100000,1.0,0.5).

% Simple 68,95,99 rule test for normality
polar_test(N,DM,DS) ->
    Values = [polar_gauss(DM,DS) || _ <-lists:seq(1,N)],
    M = lists:sum(Values) / N,
    S = math:sqrt(lists:sum([(X - M)*(X - M) || X <- Values]) / N),
    MT = math:sqrt((M-DM)*(M-DM)),
    ST = math:sqrt((S-DS)*(S-DS)),
    {A,B,C} = normal_stats(Values,M,S),
    (MT < 0.1) and (ST < 0.1) 
    and (A > 0.68) % expected a 0.68 coverage ratio 
    and (B > 0.95) % expected a 0.95 coverage ratio
    and (C > 0.99).% expected a 0.99 coverage ratio

% % gauss_test_() ->
% %     {inparallel,[fun () -> polar_test() end, fun () -> boxmuller_test() end] }.

-endif.