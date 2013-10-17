-module(force_server).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-include("print.hrl").
-include("integer_math.hrl").
-include("utils.hrl").
-include("enumerate.hrl").

-record(force_state,{ 
                     conex = []
                  }).

start_link(Opts) ->
    gen_server:start_link({local, forcemaster}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->
   put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting elevel into process dictionary

   ?PINFO("[FORCES] Force server initialised\n",[]),
    {ok, #force_state{}}.

handle_call({notify,system_conex, Conex}, _From, State) ->
   ?PINFO("[FORCES]: Learned whole molecule conectivity: ~p\n",[length(Conex)]),
   ?PDEBUG("[FORCES]: Conectivity: ~p\n",[Conex]),
    {reply, ok, State#force_state{ conex = Conex }};

handle_call({sort,Data}, _From, #force_state{ conex = Conex }=State) ->
   ?PDEBUG("[FORCES]: Sorting ~w elements in conex order \n",[length(Data)]),
   Results = sort(Data,Conex),
   {reply, {ok, Results}, State};

handle_call(Request, _From, State) ->
   ?PDEBUG("[FORCES]: WTF!! Some guy send me this! ~w...\n",[Request]),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

reverse_enumerate(List) ->
   lists:zip(List,lists:seq(1,length(List))).

sort(Data,OrderedKeys) ->
   Dict   = dict:from_list(reverse_enumerate(OrderedKeys)),
   TaggedData = lists:map(fun({A,_}=E) ->
                                    {ok,Key} = dict:find(A,Dict), {Key,E} end
                           ,Data),
   lists:map(fun({_,E}) -> E end, lists:keysort(1, TaggedData)).
