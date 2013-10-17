%%
%% Erlang GetOpt Extended services
%% @ 2012 Angel Alvarez, Resmol Group U.A.H.
%%
%%
-module(getoptex).
-author("angel@uah.es").
-compile(export_all).



% Check additional conditions on cmdline arguments
parse_args(Specs,Args,Chequeos) ->
    case getopt:parse(Specs, Args) of
    {ok, {Options, NonOptArgs}} -> 
        COpts = lists:foldl(fun(Funcion,Acc) when is_function(Funcion,1), is_list(Acc) -> 
%                                         io:format("Checking ~w\n",[Funcion]),
                                        Funcion(Acc);
                                (_F,Acc) -> Acc end,
                                Options,Chequeos),
        case is_list(COpts) of
        true ->
            {ok,{COpts,NonOptArgs}};
        false ->
            COpts
        end;
    {error, {Reason, Data}} -> {error, {Reason, Data}}
    end.



collect_option_providers(ModuleList) ->
    collect_option_providers([],[],ModuleList). 

collect_option_providers(Opts,Funs,[]) ->
        {lists:reverse(Opts),lists:reverse(Funs)};

collect_option_providers(Opts,Funs,[Module|RestList]) ->
    {NewOpts,NewFuns} = Module:option_specs(),
    collect_option_providers(NewOpts ++ Opts,NewFuns ++ Funs,RestList).
