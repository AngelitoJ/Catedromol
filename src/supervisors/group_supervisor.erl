-module(group_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Opts) ->
   supervisor:start_link({local,muc_supervisor}, ?MODULE, Opts).

-include("print.hrl").

init(Opts) ->
   ConsoleLevel = proplists:get_value(console_level,Opts),
   put(reporting_level,ConsoleLevel),
   ?PINFO("[GROUP SUP] Ready to manage MuC\'s\n",[]),

   ChildrenOpts = [{console_level,ConsoleLevel}],
   {ok, {
         {simple_one_for_one, 1, 60},
         [
            {mucfsm,      {sim_group_fsm,      start_link, [ChildrenOpts] },  permanent, brutal_kill, worker, [sim_group]}
         ]
         }
   }.
