-module(config).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-include("print.hrl").

-record(mystate,
            {
              info = [error]  %% current info levels
              ,other = "VOID" %% just kidding...
            }
   ).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, Info} = application:get_env(console_level),
%    print(info, Info, "Hello! This is the ~w server, Come On!\n",[?MODULE]),
    {ok, #mystate{info=Info }, 1000}.

% handle_call(_Request, _From, State) ->
%     {reply, ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
   Info = State#mystate.info,
   print(debug,"Im the config server and im boring...\n",[]),
   {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

