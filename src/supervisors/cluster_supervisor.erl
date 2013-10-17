-module(cluster_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Opts) ->
   supervisor:start_link({local,cluster_supervisor}, ?MODULE, Opts).

-include("print.hrl").

init(Opts) ->

   put(reporting_level,proplists:get_value(console_level,Opts)),
   case proplists:get_value(algo,Opts) of
   reference ->
      ?PINFO("[CLUSTER] Nothing to do.\n",[]),
      ignore;
   serial ->
      ?PINFO("[CLUSTER] local serial operations solicited, spawning local computing tree.\n",[]),

      % there will be one thermo server only
      ChildrenOpts = [{thermo_fsm,1}|Opts],

      % we return direcly a simulation supervisor child spec as we dont have to spwan remote node right now
      {ok,
            {
               {one_for_one, 1, 60}
               ,[{simulation_sup,    {simulation_supervisor, start_link, [ChildrenOpts]},  permanent, infinity, supervisor, [simulation_supervisor]}]
            }
      };
   parallel ->
      ?PINFO("[CLUSTER] local concurrent operations solicited, spawning local computing tree.\n",[]),
      % we return direcly a simulation supervisor child spec as we dont have to spwan remote node right now
      {ok,
            {
               {one_for_one, 1, 60}
               ,[{simulation_sup,    {simulation_supervisor, start_link, [Opts]},  permanent, infinity, supervisor, [simulation_supervisor]}]
            }
      };
   cluster ->
      ?PINFO("[CLUSTER] distributed operation solicited, spawning designated computing nodes.\n",[]),
      ignore;
   Other ->
      ?PWARNING("[CLUSTER] WTF! i dont manage ~w mode\n",[Other]),
      {stop, cluster_failed}
   end.
