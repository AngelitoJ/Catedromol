-module(writer).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

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

-include("print.hrl").

-record(sim_log_info,{
                        filename = undefined,
                        handle   = undefined,
                        status   = close
                     }).

-record(writer_state,{
                        initialized     = false,
                        temperature_log = undefined,
                        molden_log      = undefined
                     }).

start_link(Opts) ->
    gen_server:start_link({local, sim_writer}, ?MODULE, Opts, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

% initialize log files for:
% Molden output:  just atoms positions and forces
% Temperature log: all steps temperature
%
init(Opts) ->
%    [Opts]       = Args,
   process_flag(trap_exit,true),                                   % catch signals to allow clean shutdown ie: closing open files ...
   put(reporting_level,proplists:get_value(console_level,Opts)),  % get reporting level into process dictionary

   TargetName   = proplists:get_value(target, Opts),

   TempLog      = open_log(#sim_log_info{ filename = TargetName ++ "_temperature.log" }),
   MoldenLog    = open_log(#sim_log_info{ filename = TargetName ++ "_molden.log" }),

   Status = lists:foldl(fun(_Any,error) -> error;
                           (#sim_log_info{ status = error, filename = _Name },ok) -> error;
                           (#sim_log_info{ status = open, filename = Name},ok) -> ?PINFO("[WRITER] Opening log ~s for writing.\n",[Name]), ok end
                        ,ok
                        ,[TempLog,MoldenLog]),

   case Status of
   ok ->
      ?PINFO("[WRITER] Initialisation complete: \n",[]),
      {ok, #writer_state{ temperature_log = TempLog, molden_log = MoldenLog} };
   error ->
      {stop,error_opening_logs}
   end.

handle_call({write,temperature,Temp}, _From, #writer_state{ temperature_log = LogFile } = State) ->

%    Data  = io_lib:format("~w\n",[Temp]),
%    ok    = io:write(LogFile#sim_log_info.handle,Data),
   io:format(LogFile#sim_log_info.handle,"~w\n",[Temp]),

   {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(shutdown,State) -> 

   ?PDEBUG("[WRITER] Shutdown Requested.\n",[]),

   TLog = State#writer_state.temperature_log,
   MLog = State#writer_state.molden_log,

   close_log(TLog),
   close_log(MLog),

   ok;

terminate(Reason, _State) ->
   ?PWARNING("[WRITER] Shutdown Requested (~p).\n",[Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

open_log(#sim_log_info{ status = open , filename = Name } = Log) -> 

   ?PWARNING("[WRITER] Tried to reopen open file log: ~w\n",[Name]),

   Log;

open_log(#sim_log_info{ status = close , filename = Name } = Log) -> 

   case file:open(Name,[write]) of
   {ok,Handle} ->
      Log#sim_log_info{ status = open , handle = Handle };
   {error,Reason} ->
      ?PWARNING("[WRITER] Failed ( ~p ) while trying opening log ~s  for writing.\n",[Reason, Name]),
      Log#sim_log_info{ status = error}
   end.

close_log(#sim_log_info{ status = open , filename = Name , handle = Handle}) -> 

   ?PINFO("[WRITER] Closing open log file: ~s\n",[Name]),
   file:close(Handle);

close_log(#sim_log_info{ status = _Any , filename = Name }) -> 

   ?PWARNING("[WRITER] Tried to close a non-open log file: ~w\n",[Name]),
   error.

check_write_frequency(Opts) ->
   WriteFreq = proplists:get_value(geom_write_freq,Opts),
   case (WriteFreq < 1) or (WriteFreq > 1000) of
            true -> { error, {"Write freq must be in [1..1000]", "(dead badgers/femtosecond)"}};
            false -> Opts
    end.

option_specs() ->
    {
%%      {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
        [
             {geom_write_freq,      undefined,  "write",       {integer,20},                 "Write one every k geometries (default 20)." }
         ]
        ,[
            fun check_write_frequency/1     % Check write setting for logging geometries
        ]
    }.