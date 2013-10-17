%% Erlang generic script wrapper  
%% Argument parsin and app loading functions
%% @ 2012 Angel Alvarez  The ResMol Group
%%

-module(application_core).
-compile(export_all).

-include("enumerate.hrl").
-include("print.hrl").


% Entry point for application starting path
start(_Type, StartArgs) ->

   {ok, CmdlineOpts} = application:get_env(cmdline_options),
   AppOptions = [{start_args,StartArgs}|CmdlineOpts],

   case top_supervisor:start_link(AppOptions) of
   {ok, Pid} ->
%       io:format("\n\nApplication started...\n\n"),
      {ok, Pid};
   {error,Other} ->
      io:format("Application crashed! Error: ~p\n",[Other]),
      {error, Other}
   end.

stop(_State) ->
ok.


-spec app_main(atom(),[tuple()]) -> none().
app_main(AppName,Options) ->
   {A1,A2,A3} = now(),         % get random seed from current time
   random:seed(A1, A2, A3),    % feed the standar random number generator

   Info = calcular_nivel_reporte(Options), 
   put(reporting_level,Info),                                                                % store reporting level in process dictionary

%    io:format("[MAIN] Allowed Reporting Levels:~w\n",[Info]),
%    ok = application:set_env(AppName,console_level,Info,5000),

   FinalOptions = [{console_level,Info}|Options],                                            % Store console level in proplists
   ok = application:set_env(AppName,cmdline_options,FinalOptions,5000),                      % Store options so app will get them on startup

   Target  = proplists:get_value(target,Options),
   ok = application:set_env(AppName,target,Target,5000),

   case application:start(AppName) of
   ok ->
      try gen_server:call(simulator, run_sim, infinity) of
         ok -> ok
      catch
         Other:Reason ->
         io:format("\n\n\n*** Application terminated badly ****\n\twith:\n\t~w\n\tcaused by\n\t~w\n\n\n",[Other,Reason])
      end,
      ok = application:stop(AppName);
   Other ->
      io:format("[MAIN] Application ~w terminated badly, with cause: ~p\n",[AppName,Other])
   end.

calcular_nivel_reporte(Args) ->                                                 
    Idx           = proplists:get_value(debug, Args, 0),                        
    Levels        = [error,warning,info,debug],                                 
    OrderedLevels = enumerate0(Levels),                                         
    {I,_}         = lists:keyfind(Idx,1,OrderedLevels),                         
    Permited      = lists:filter(fun({X,_}) -> X =< I end,OrderedLevels),       
    Selected      = [X || {_,X} <- Permited ],                                  
    Selected.                                                                   

