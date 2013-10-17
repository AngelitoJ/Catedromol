%% Erlang Nose Hoover dynamics simulator 
%% main script component
%% @ 2012 Angel Alvarez  Initial design
%%
-module(catedromol).
-author("angel@uah.es").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% escript calls this funcion to start script, we transfer control, after args processing and app setup stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Entry point for escript(tm) execution path
% Setup main enviroment, basic getopt parsing , custom args processing 
% and user messaging

-spec main([string() | char()]) -> 'ok'.
main(Args) ->
   AppName = ?MODULE,
%    io:format("Running application: ~w\n\n\n",[AppName]),
   script_main(AppName,Args).  %  core facilities prepare a sane setup for app start-up


% Entry point for escript(tm) execution path
% Setup main enviroment, basic getopt parsing , custom args processing 
% and user messaging
-spec script_main(atom(),[string() | char()]) -> 'ok'.
script_main(AppName,Argumentos) ->
    ok                       = application:load(AppName),
    {ok,Version}             = application:get_key(AppName,vsn),
    {ok,Description}         = application:get_key(AppName,description),
    Programa                 = escript:script_name(),

    io:format("~s\nVersion: ~s\n\n",[Description,Version]),

    OptionProviders          = module_tools:list_app_modules(AppName,exports,option_specs),
    {OptSpecList,OptFunList} = getoptex:collect_option_providers(OptionProviders), 

    case getoptex:parse_args(OptSpecList, Argumentos, OptFunList) of
    {ok, {Opciones, _OtrosArgs}} -> 
        application_core:app_main(AppName,Opciones);
    {help} ->
        getopt:usage(OptSpecList, Programa);
    {version} ->
        io:format("~s (v ~s)\n~s\n", [Programa, Version, Description]);
    {error, {invalid_option_arg, Data}} ->
        io:format("Error:\n\t Invalid option: ~p~n~n", [Data]),
        getopt:usage(OptSpecList, Programa);
    {error, {Reason, Data}} ->
        io:format("Error:\n\t~s ~p~n~n", [Reason, Data]),
        getopt:usage(OptSpecList, Programa)
    end.

-spec check_help([any()]) -> [any()] | {'help'}.
check_help(Opts) ->
    case proplists:get_bool(help,Opts) of
            true -> { help };
            false -> Opts
    end.

-spec check_version([any()]) -> [any()] | {'version'}.
check_version(Opts) ->
    case proplists:get_bool(version,Opts) of
            true -> { version };
            false -> Opts
    end.

-spec check_debug([any()]) -> [any()].
check_debug(Opts) ->
    Nivel = proplists:get_value(debug,Opts,0), 
    case (Nivel < 0) or (Nivel > 3) of
        true ->  [{debug,0} | proplists:delete(debug,Opts)];
        false -> Opts
    end.

-spec check_algo([any()]) -> [any()] | { any()}.
check_algo(Opts) ->
    case proplists:get_value(algo,Opts,unknown) of
            reference -> Opts;
            serial    -> Opts;
            parallel  -> Opts;
            Other     -> { error, {"Algorithm is not available.", Other} }
    end.


option_specs() ->
    Procesos = erlang:system_info(schedulers_online) * 2,
    {    
        [
%%          {Name,     ShortOpt,  LongOpt,       ArgSpec,               HelpMsg}
            {help,        $?,        "help",        undefined,                    "Show this help."},
            {debug,       $d,        "debug",       {integer, 0},                 "Show debug info."},
            {verbose,     $v,        "verbose",     undefined,                    "Show all actions performed."},
            {version,     $V,        "version",     undefined,                    "Show software version."},
            {algo,        $a,        "algorithm",   {atom,reference},             "Use diferent algorithms."},
            {procs,       $P,        "cores",           {integer,  Procesos },        "Number of workers (default 2*core)."}
        ]
        ,[
            fun check_help/1           % comprobar si se ha solicitado la ayuda
            ,fun check_version/1       % comprobar si se ha solicitado la versión
            ,fun check_debug/1         % comprobar un nivel de depuracion correcto
            ,fun check_algo/1          % check algorithm selected is available
        ]
    }.
