-module(simulation_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Opts) ->
   supervisor:start_link(simulation_supervisor, Opts).

-include("print.hrl").

init(Opts) ->
   put(reporting_level,proplists:get_value(console_level,Opts)),
   ?PINFO("[SIMULATION SUP] Initiating simulation supervisor process [~p]\n",[self()]),

   {ok, {
         {one_for_all, 1, 60},
         [
%             {thermo_srv,  {thermo_server,      start_link, [Opts]},  permanent, brutal_kill, worker,     [thermo_server]}
            {thermo_fsm,  {thermo_fsm,         start_link, [Opts]},  transient, 1000,          worker,     [thermo_fsm]}
            ,{force_srv,  {force_server,       start_link, [Opts]},  permanent, brutal_kill, worker,     [force_server]}
             ,{group_sup,  {group_supervisor,   start_link, [Opts]},  permanent, brutal_kill, supervisor, [group_supervisor]}
         ]
         }
   }.
