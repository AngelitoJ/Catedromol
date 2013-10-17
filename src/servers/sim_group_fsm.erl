-module(sim_group_fsm).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,start_link/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
   start_link(Opts,[]).

start_link(Opts,Opts2) ->
   io:format("sim_group_fsm: ~p ~p",[Opts,Opts2]),
    gen_fsm:start_link(?MODULE, Opts2 ++ Opts, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

-include("print.hrl").

init(Opts) ->

%    put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting elevel into process dictionary
%    ?PINFO("MuC is alive\n",[]),
    {ok, idle, [],3000}.

idle(timeout,_State) ->
%    ?PINFO("MuC is bored",[]),
   {ok,idle,[],3000};

idle(_Event,_State) ->
   {ok,idle,[],3000}.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    {reply, ok, state_name, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

