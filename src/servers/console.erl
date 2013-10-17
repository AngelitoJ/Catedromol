-module(console).
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

-import(lists,[foreach/2,zip/2,zip3/3]).

-include("records.hrl").
-include("enumerate.hrl").
% -include("print.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Opts) ->
    gen_server:start_link({local, sim_console}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Opts) ->

   put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting elevel into process dictionary
   Info2      = proplists:get_value(console_level,Opts),

   io:format("[CONSOLE] Output services initialised, current reporting levels: ~w\n",[Info2]),
   {ok, Opts}.


handle_call({print,Data}, _From, State) ->
   io:format(Data),
   {reply, ok, State};

handle_call({winfo,Data}, _From, State) ->
   io:format("\tWilson matrix:\n"),
   foreach(fun(Wi) -> 
                     L=length(Wi)
                     ,io:format("\t(~w)~w\n",[L,Wi])end,Data),
   io:format("\n"),
   {reply, ok, State};

handle_call({ginfo, Group}, _From, State) ->
   group_info(Group),
   {reply, ok, State};

handle_call({mucinfo, MuC}, _From, State) ->
   muc_info(MuC),
   {reply, ok, State};

% handle_call({ginfo,#molecule{ index=Index, labels=Labels, position_cart = Coords, position_int = Internas, conex = ConexData, fullconex = FullConexData, globalconex = GlobalConexData
%                               ,vel=Velocities, mass=Masses, forces=Forces, hess = Hess, grad_in_Q0 = GradQ0, grad_in_Qi = GradQi, members = {Private,Managed,Enslaved} }}, _From, State) ->
%    io:format("\tGroup number ~w Private Atoms ~w, Managed Atoms ~w, Enslaved Atoms: ~w \n",[Index,Private,Managed,Enslaved]),
%    AtomData0 = zip3(Coords,Velocities,Masses),
%    AtomData1 = zip(AtomData0,Forces),
%    AtomData  = zip(Labels,AtomData1),
%    foreach(fun({L,{{C,V,M},F}}) -> 
%                               io:format("\tAtom ~s\tMass: ~w (relative to electron)\n",[L,M])
%                               ,io:format("\t\t\tCartesian coordinates: ~w\n",[C])
%                               ,io:format("\t\t\tVelocity:              ~w\n",[V])
%                               ,io:format("\t\t\tForce:                 ~w\n",[F])
%                               end
%                ,AtomData),
%    io:format("\tFull     conectivity         (~w):\t~w\n",[length(FullConexData),FullConexData]),
%    io:format("\tReduced  conectivity         (~w):\t~w\n",[length(ConexData),ConexData]),
%    io:format("\tReduced (global) conectivity (~w):\t~w\n",[length(GlobalConexData),GlobalConexData]),
%    io:format("\tInternal coordinates         (~w):\t~w\n",[length(Internas),Internas]),
%    io:format("\tInitial  Gradient at Q0      (~w):\t~w\n",[length(GradQ0),GradQ0]),
%    io:format("\tCurrent  Gradient (sofar)    (~w):\t~w\n",[length(GradQi),GradQi]),
%    io:format("\tGroup    Hessian             (~w):\n"    ,[length(Hess)]),
%    foreach(fun(X) -> io:format("\t\t\t\t\t~w\n",[X]) end, Hess),
%     {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(Info, State) ->
   io:format(Info),
   {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

group_info(#molecule{ index=Index, labels=Labels, position_cart = Coords, position_int = Internas, conex = ConexData, fullconex = FullConexData, globalconex = GlobalConexData
                              ,vel=Velocities, mass=Masses, forces=Forces, hess = Hess, grad_in_Q0 = GradQ0, grad_in_Qi = GradQi, members = {Private,Managed,Enslaved} }) ->
   io:format("\tGroup number ~w Private Atoms ~w, Managed Atoms ~w, Enslaved Atoms: ~w \n",[Index,Private,Managed,Enslaved]),
   AtomData0 = zip3(Coords,Velocities,Masses),
   AtomData1 = zip(AtomData0,Forces),
   AtomData  = zip(Labels,AtomData1),
   foreach(fun({L,{{C,V,M},F}}) -> 
                              io:format("\tAtom ~s\tMass: ~w (relative to electron)\n",[L,M])
                              ,io:format("\t\t\tCartesian coordinates: ~w\n",[C])
                              ,io:format("\t\t\tVelocity:              ~w\n",[V])
                              ,io:format("\t\t\tForce:                 ~w\n",[F])
                              end
               ,AtomData),
   io:format("\tFull     conectivity         (~w):\t~w\n",[length(FullConexData),FullConexData]),
   io:format("\tReduced  conectivity         (~w):\t~w\n",[length(ConexData),ConexData]),
   io:format("\tReduced (global) conectivity (~w):\t~w\n",[length(GlobalConexData),GlobalConexData]),
   io:format("\tInternal coordinates         (~w):\t~w\n",[length(Internas),Internas]),
   io:format("\tInitial  Gradient at Q0      (~w):\t~w\n",[length(GradQ0),GradQ0]),
   io:format("\tCurrent  Gradient (sofar)    (~w):\t~w\n",[length(GradQi),GradQi]),
   io:format("\tGroup    Hessian             (~w):\n"    ,[length(Hess)]),
   foreach(fun(X) -> io:format("\t\t\t\t\t~w\n",[X]) end, Hess).

muc_info(#muc{index = Number, atomdata = AtomData, gconex = Gconex, lconex = Lconex, qi = Qi, grad_qi = GradQi } = MuC) ->

   io:format("\tMuC(~w) ~w atoms \n",[Number,length(AtomData)]),

   io:format("\tAtom Data for this MuC:\n\n"),
   lists:foreach(fun atom_info/1,AtomData),

   io:format("\n\tConectivity Info for this MuC:\n\n"),

   io:format("\t\tRed. local conectivity  (~w): ~w\n",[length(Lconex),Lconex]),
   io:format("\t\tRed. global conectivity (~w): ~w\n",[length(Gconex),Gconex]),

   io:format("\n\tInternal Coordinates Info for this MuC:\n\n"),

   io:format("\t\tCurrent Qi             (~w): ~w\n",[length(Qi),Qi]),
   io:format("\t\tCurrent gradient at Qi (~w): ~w\n",[length(GradQi),GradQi]).

atom_info(#atominfo{ localidx = Lidx, globalidx = Gidx, status = S, label = Label, position= C, velocity = V , force = F, mass = Mass}) ->
   io:format("\tAtom: ~s (~w)-->(~w) ~w\n",[Label,Lidx,Gidx,S]),
   io:format("\t\tMass:                  ~w (relative to electron)\n",[Mass]),
   io:format("\t\tCartesian coordinates: ~w\n",[C]),
   io:format("\t\tVelocity:              ~w\n",[V]),
   io:format("\t\tForce:                 ~w\n",[F]).

