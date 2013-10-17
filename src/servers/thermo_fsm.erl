-module(thermo_fsm).
-behaviour(gen_fsm).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([
         init/1
         ,setup/2, setup/3
         ,ready/2, ready/3
         ,state_name/2, state_name/3
         ,handle_event/3
         ,handle_sync_event/4, handle_info/3
         ,terminate/3
         ,code_change/4
         ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
%    io:format("Trying to start this fucking FSM ~p...\n",[Opts]),
   gen_fsm:start_link({local, thermo_fsm}, thermo_fsm, Opts, []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

-include("print.hrl").
-include("constants.hrl").
-include("nose_hoover_thermostate.hrl").

% -define(TEST,true).

-record(thermo_state,{
                        options                 = []
                        ,state                  = undefined
                        ,children               = 0
                        ,children_pids          = []
                        ,pending_children       = 0
                        ,siblings               = 0
                        ,siblings_pids          = []
                        ,pending_siblings       = 0
                        ,numatoms               = 0
                        ,temperature            = 0.0         %
                        ,timestep               = 0.0          % For doing thermoregulation
                        ,children_energy        = 0.0          % how much energy i got from children
                        ,siblings_energy        = 0.0          % how much energy i got from siblings
                        ,thermostate            = undefined
                     }).

init(Opts) ->
%    io:format("Init of this fucking FSM ~p...\n",[Opts]),
   process_flag(trap_exit,true),                                                 % catch signals to allow clean shutdown ie: closing open files ...

   put(reporting_level,proplists:get_value(console_level,Opts)),                 % get reporting elevel into process dictionary
   ThermoGang     = proplists:get_value(thermo_fsm,Opts),                        % get how many thermo guys are out there (including me)
   TimeStep       = proplists:get_value(timestep,Opts),                          % get step
   Temperature    = proplists:get_value(temperature,Opts),                       % intial temperature

   NewState = #thermo_state{
                              state                =  setup
                              ,timestep            =  TimeStep
                              ,temperature         =  Temperature
                              ,siblings            =  ThermoGang - 1             % how many thermo fsm's not counting this
                           },
   ?PINFO("[THERMO FSM] (INIT)->(SETUP) Starting up, expecting ~w siblings\n",[ThermoGang-1]),
   {ok, setup, NewState}.


setup(Event,State) ->
   ?PINFO("[THERMO FSM] (SETUP) Received: ~p\n",[Event]),
   {next_state, setup, State}.                                              % Unknown async event in setup state

%  Children notify us upon startup, we store how many of them we will send us energies till we can go to thermal bathing
setup(new_child,From,#thermo_state{ children = Children } = State) ->
   NewState = State#thermo_state{ children = Children + 1 },
   ?PINFO("[THERMO FSM] (SETUP) New child is up, now they are: ~w\n",[Children+1]),
   {reply,ok,setup,NewState};

% Simulator tell us how many atom the system has, now we are ready
setup({atoms,NumAtoms},_From, State) when is_integer(NumAtoms) andalso NumAtoms > 0 ->
   NewState    = State#thermo_state{ numatoms = NumAtoms },
   ?PINFO("[THERMO FSM] (SETUP) Total number of atoms: ~w\n",[NumAtoms]),
   {reply, ok, setup, NewState};

setup(go_ready, _From, #thermo_state{ numatoms = NumAtoms, temperature = Temp, timestep = TimeStep, children = Children, siblings = Siblings } = State ) ->
   NewThermo   = thermo_init(Temp, NumAtoms),
   NewState    = State#thermo_state{
                                       state = ready
                                       ,thermostate = NewThermo
                                       ,pending_children = Children
                                       ,pending_siblings = Siblings
                                    },
   ?PINFO("[THERMO FSM] (SETUP)->(READY) Thermostate ready (~w atoms at ~w K for integration at ~w a.u of time)\n",[NumAtoms,Temp,TimeStep]),
   {reply, ok, ready, NewState};

setup(Event, _From, State) ->
   ?PINFO("[THERMO FSM] (SETUP) Received: ~p sending WTF!...\n",[Event]),
   {reply, what_the_fuck_while_ready, setup, State }.                                         % Unknown sync event in setup state

ready(Event, State) ->
   ?PINFO("[THERMO FSM] (READY) Received: ~p\n",[Event]),
   {next_step, ready, State }.                                                % Unknown async event in ready state

% Messages in ready state when no siblings
ready(Event, From, #thermo_state{ siblings = 0 } = State) ->
   no_siblings_ready(Event,From,State);

% Messages in ready state when one or more siblings
% ready(Event, From, #thermo_state{ siblings = _Siblings } = State) ->
%    any_siblings_ready(Event,From,State);


ready(Event, _From, State) ->
   ?PINFO("[THERMO FSM] (READY) Received: ~p sending WTF!...\n",[Event]),
   {reply, what_the_fuck_while_ready, ready, State }.                                         % Unknown sync event in ready state


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

terminate(Reason, StateName, _State) ->

   ?PINFO("[THERMO FSM] Termination due to: ~w while in state: ~w\n",[Reason,StateName]),

    ok.

code_change(_OldVsn, StateName, State, _Extra) ->

    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

no_siblings_ready({child_energy,Ek},From, #thermo_state{ children = Children, children_pids = ChildrenPids, pending_children = Pending, children_energy = ChildrenEk} = State) ->

   NewPending        = Pending - 1,
   NewChildrenPids   = [From|ChildrenPids],
   NewChildrenEk     = ChildrenEk + Ek,
   case NewPending > 0 of
   true ->
      ?PINFO("[THERMO FSM] (READY)->(ACCUMULATING) Received child ~p contribution: ~w\n",[From,Ek]),
      NewState = State#thermo_state{ 
                                       state             = accumulating
                                       ,children_pids    = NewChildrenPids
                                       ,pending_children = NewPending
                                       ,children_energy  = NewChildrenEk
                                    },
      {next_state, accumulating, NewState };
   false ->
      ?PINFO("[THERMO FSM] (READY)->(READY) Received last child ~w contribution: ~w\n",[From,Ek]),

      {NewThermo,ScalingFactor} = thermal_scaling(State,NewChildrenEk),
      ?PINFO("[THERMO FSM] (READY)->(READY) Scaling factor for my children: ~w \n",[ScalingFactor]),
      children_response(NewChildrenPids,ScalingFactor),

      NewState = State#thermo_state{ 
                                       state             = ready
                                       ,children_pids    = []
                                       ,pending_children = Children
                                       ,children_energy  = 0.0
                                       ,thermostate      = NewThermo
                                    },
      {next_state, ready, NewState }
  end.

children_response(Pids,Data) ->
   lists:foreach(fun(Child) -> gen_fsm:reply(Child,Data) end,Pids).
%    lists:foreach(fun(Child) -> Status = gen_fsm:reply(Child,Data)
%                            ,io:format("Sending replies got status = ~p\n",[Status]) end,Pids).

thermal_scaling(#thermo_state{ thermostate = Thermo, timestep = TimeStep , temperature = Temp, numatoms = Atoms },Ek) ->
   NewThermo = bath(Thermo,TimeStep,Temp,Ek,Atoms),
   {NewThermo,NewThermo#thermo.scale}.

















% _ready({sibling_energy,Ek}, From, #thermo_state{  pending_siblings = Pending, siblings_energy = SEk}) ->
%    NewPending = Pending - 1,
%    case NewPending > 0 of
%    true ->
%       NewState = State#thermo_state{ 
%                                        state             = accumulating2
%                                        ,pending_siblings = NewPending
%                                        ,siblings_energy  = SEK + Ek
%                                     },
%       ?PINFO("[THERMO FSM] (READY)->(ACCUMULATE) Received sibling ~p contribution: ~w\n",[From,Ek]),
%       {next_state, accumulating2, NewState };
%    false ->
%       notify_siblings(SiblingPids),
%       {next_state, accumulating2, NewState };
%    end;
% #thermo_state{ sibling_pids = SiblingPids, children_pids = ChildrenPids, pending_children = Pending, children_energy = CEk}) ->
%    NewPending = Pending - 1,
%    SoFarPids  = [From|ChildrenPids],
%    case NewPending > 0 of
%    true ->
%       NewState = State#thermo_state{ 
%                                        state             = accumulating
%                                        ,children_pids    = SoFarPids
%                                        ,pending_children = NewPending
%                                        ,children_energy  = CEK +Ek
%                                     },
%       ?PINFO("[THERMO FSM] (READY)->(ACCUMULATING) Received child ~p contribution: ~w\n",[From,Ek]),
%       {next_state, accumulating, NewState };
%    false ->
%       NewState = State#thermo_state{ 
%                                        state             = accumulating2
%                                        ,children_pids    = SoFarPids
%                                        ,pending_children = NewPending
%                                        ,children_energy  = CEK + Ek
%                                     },
%       ?PINFO("[THERMO FSM] (READY)->(ACCUMULATING2) Received last child ~p contribution: ~w\n",[From,Ek]),
%       notify_siblings(SiblingPids),
%       {next_state, accumulating2, NewState };
%    end;
