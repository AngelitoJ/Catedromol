-module(top_supervisor).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("print.hrl").

start_link(Opts) ->
   supervisor:start_link(top_supervisor, Opts).

init(Opts) ->

   put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting level into process dictionary

%    io:format("[APPLICATION SUP] Initialising [~p]: with ~w options\n",[self(),length(Opts)]),

   {ok, {
         {rest_for_one, 1, 60},
         [
            % Console ouput services for printing stuff
            {console_srv,     {console,            start_link, [Opts]},   permanent, 1000,      worker,     [console]}
            % write logs about many things
            ,{writer_srv,     {writer,             start_link, [Opts]},   permanent, 1000,      worker,     [writer]}
            % local and remote computing nodes management
            ,{cluster_sup,    {cluster_supervisor, start_link, [Opts]},   permanent, infinity,  supervisor, [cluster_supervisor]}
            % simulator core 
            ,{simulator_srv,  {simulator,          start_link, [Opts]},   permanent, 1000,      worker,     [simulator]}
         ]
         }
   }.