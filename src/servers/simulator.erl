-module(simulator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(simulator_state,{ opts = undefined}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,option_specs/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, simulator}, ?MODULE, [Args], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-include("print.hrl").

init([Opts]) ->
   put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting elevel into process dictionary
    ?PINFO("[SIMULATOR] Initialisation complete!\n",[]),
    {ok, #simulator_state{ opts = Opts }}.

handle_call(run_sim, _From, State) ->

   ?PINFO("[SIMULATOR] Starting simulation!\n",[]),

   case proplists:get_value(algo,State#simulator_state.opts) of 
   reference ->
      ?PINFO("[SIMULATOR] Reference algorithm, serial execution, local only operations\n",[]),
      % Refence simulator:
      % all groups are handled in a list
      % force server is started manually
      % we dont care about markets and other mother fuc...
      reference_nosehoover:main(State#simulator_state.opts);
   serial ->
      ?PINFO("[SIMULATOR] Serialized algorithm, serial execution, local only operations\n",[]),
      % Serial simulator:
      % All groups are handle by processes, simulator talks to them one by one
      % all daemons are available, 
      % Cluster services are not available.
      serial_nosehoover:main(State#simulator_state.opts);
   parallel ->
      ?PINFO("[SIMULATOR] Parallel algorithm, concurrent execution, local only operations\n",[]),
      % parallel simulator:
      % All groups are handle in processes, simulator let them run independtly and concurrently
      % all daemon are available as well.
      % cluster service cover local only operations.
      parallel_nosehoover:main(State#simulator_state.opts)
   end,
   ?PINFO("[SIMULATOR] CATEDROMOL Server finished simulation!\n",[]),
   {reply, ok, State};

handle_call(_Request, _From, State) ->
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

check_energy(Opts) ->
    case proplists:get_value(energy,Opts) < 0.0 of
            true -> { error, {"Energy must be positive.", "E > 0.0 (Kcal/Mol)"}};
            false -> Opts
    end.

check_temperature(Opts) ->
    case proplists:get_value(energy,Opts) < 0.0 of
            true -> { error, {"Temperature must be positive.", "T > 0.0 (Kelvin)"}};
            false -> Opts
    end.

check_timestep(Opts) ->
    case proplists:get_value(timestep,Opts) < 0.0 of
            true -> { error, {"Time Step must be positive.", "(in femtoseconds)"}};
            false -> Opts
    end.

check_relaxation(Opts) ->
    case proplists:get_value(relaxation,Opts) < (proplists:get_value(iterations,Opts) div 100 ) of
            true -> { error, {"Relaxation must be positive and above 1% of total iterations.", "(in femtoseconds)"}};
            false -> Opts
    end.

check_constructor(Opts) ->
   Algo = proplists:get_value(constructor,Opts),
   case (Algo < 1) or (Algo > 2) of
            true -> { error, {"Group constructor algorithm must be in [1,2]", ""}};
            false -> Opts
    end.

option_specs() ->
    {
%%      {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
        [
             {energy,      $E,        "Energy",      {float,0.0},                  "Energy of the electronic state (in kcal/mol))." }
            ,{temperature, $T,        "Temperature", {float,300.0},                "Initial Temperature (in Kelvin)." }
            ,{timestep,    $t,        "TimeStep",    {float,4.134137344773891},    "Integration step in au of time,"}
            ,{iterations,  $I,        "Iterations",  {integer,1000},               "Iterations to do in sim stage,"}
            ,{relaxation,  undefined, "Relaxation",  {integer,10},                 "Iterations to relaxation (warm up)"}
            ,{constructor, undefined, "Constructor", {integer,1},                  "Use a diferent algorithm for group construction (Default number 1)."}
         ]
        ,[
             fun check_energy/1         % Check positive values of energy
            ,fun check_temperature/1    % Check positive values of Temperature
            ,fun check_timestep/1       % Check positive values of timestep
            ,fun check_relaxation/1     % Check relaxation amounts at let 1% of total iterations
            ,fun check_constructor/1     % Check relaxation amounts at let 1% of total iterations
        ]
    }.
