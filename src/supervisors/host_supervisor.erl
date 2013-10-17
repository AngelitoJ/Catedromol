-module(host_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Args) ->
   supervisor:start_link(host_supervisor, [Args]).

init([_Opts]) ->
   io:format("[HOST] Initiating Host controller at node [~p]\n",[node()]),
   {ok, {
         {one_for_all, 1, 60},
         [
            {host_server,        {host,                  start_link, []},  permanent, brutal_kill, worker,     [host]}
            ,{sim_controller,    {simulation_supervisor, start_link, []},  permanent, brutal_kill, supervisor, [simulation_supervisor]} % supervises computing processes
         ]
         }
   }.