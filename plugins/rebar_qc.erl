%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Tuncer Ayaz
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_qc).

-export([qc/2, proper/2, eqc/2]).

-define(FAIL, throw({error, failed})).

-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

%% ===================================================================
%% Public API
%% ===================================================================

qc(Config, _AppFile) ->
    run_qc(Config, qc_opts(Config)).

proper(Config, _AppFile) ->
    run_qc(Config, qc_opts(Config), proper).

eqc(Config, _AppFile) ->
    run_qc(Config, qc_opts(Config), eqc).

%% ===================================================================
%% Internal functions
%% ===================================================================

-define(TEST_DIR, ".test").
-define(PROPER_MOD, proper).
-define(EQC_MOD, eqc).

qc_opts(Config) ->
    rebar_config:get(Config, qc_opts, []).

run_qc(Config, QCOpts) ->
    run_qc(Config, QCOpts, select_qc_lib(QCOpts)).

run_qc(Config, QCOpts, QC) ->
    ?DEBUG("Selected QC library: ~p~n", [QC]),
    run(Config, QC, QCOpts -- [{qc_lib, QC}]).

select_qc_lib(QCOpts) ->
    case proplists:get_value(qc_lib, QCOpts) of
        undefined ->
            detect_qc_lib();
        QC ->
            case code:ensure_loaded(QC) of
                {module, QC} ->
                    QC;
                {error, nofile} ->
                    ?ABORT("Configured QC library '~p' not available~n", [QC])
            end
    end.

detect_qc_lib() ->
    case code:ensure_loaded(?PROPER_MOD) of
        {module, ?PROPER_MOD} ->
            ?PROPER_MOD;
        {error, nofile} ->
            case code:ensure_loaded(?EQC_MOD) of
                {module, ?EQC_MOD} ->
                    ?EQC_MOD;
                {error, nofile} ->
                    ?ABORT("No QC library available~n", [])
            end
    end.

setup_codepath() ->
    CodePath = code:get_path(),
    true = code:add_patha(test_dir()),
    true = code:add_patha(ebin_dir()),
    CodePath.

run(Config, QC, QCOpts) ->
    case code:ensure_loaded(QC) of
        {module, QC} ->
            ok;
        {error, nofile} ->
            ?ABORT("Failed to load QC lib '~p'~n", [QC])
    end,
    ?DEBUG("QC Options: ~p~n", [QCOpts]),

    ok = filelib:ensure_dir(?TEST_DIR ++ "/foo"),
    CodePath = setup_codepath(),

    %% Compile erlang code to ?TEST_DIR, using a tweaked config
    %% with appropriate defines, and include all the test modules
    %% as well.
    ok = test_compile(Config),

    case lists:flatten([qc_module(QC, QCOpts, M) || M <- find_prop_mods()]) of
        [] ->
            true = code:set_path(CodePath),
            ok;
        Errors ->
            ?ABORT("One or more QC properties didn't hold true:~n~p~n",
                   [Errors])
    end.

qc_module(QC=proper, QCOpts, M) -> QC:module(M, QCOpts);
qc_module(QC=eqc, QCOpts, M) -> QC:module(QCOpts, M).

find_prop_mods() ->
    Beams = rebar_utils:find_files(?TEST_DIR, ".*\\.beam\$"),
    [M || M <- [rebar_utils:erl_to_mod(Beam) || Beam <- Beams], has_prop(M)].

has_prop(Mod) ->
    lists:any(fun({F,_A}) -> lists:prefix("prop_", atom_to_list(F)) end,
              Mod:module_info(exports)).

test_dir() ->
    filename:join(rebar_utils:get_cwd(), ?TEST_DIR).

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

test_compile(Config) ->
    %% Obtain all the test modules for inclusion in the compile stage.
    %% Notice: this could also be achieved with the following
    %% rebar.config option: {qc_compile_opts, [{src_dirs, ["test"]}]}
    TestErls = rebar_utils:find_files("test", ".*\\.erl\$"),

    %% Compile erlang code to ?QC_DIR, using a tweaked config
    %% with appropriate defines, and include all the test modules
    %% as well.
    rebar_erlc_compiler:doterl_compile(test_compile_config(Config),
                                       ?TEST_DIR, TestErls).
test_compile_config(Config) ->
    EqcOpts = eqc_opts(),
    PropErOpts = proper_opts(),

    ErlOpts = rebar_config:get_list(Config, erl_opts, []),
    EunitOpts = rebar_config:get_list(Config, eunit_compile_opts, []),
    Opts0 = [{d, 'TEST'}] ++
        ErlOpts ++ EunitOpts ++ EqcOpts ++ PropErOpts,
    Opts = [O || O <- Opts0, O =/= no_debug_info],
    Config1 = rebar_config:set(Config, erl_opts, Opts),

    FirstErls = rebar_config:get_list(Config1, eunit_first_files, []),
    rebar_config:set(Config1, erl_first_files, FirstErls).

eqc_opts() ->
    define_if('EQC', is_lib_avail(is_eqc_avail, eqc,
                                  "eqc.hrl", "QuickCheck")).

proper_opts() ->
    define_if('PROPER', is_lib_avail(is_proper_avail, proper,
                                     "proper.hrl", "PropEr")).

define_if(Def, true) -> [{d, Def}];
define_if(_Def, false) -> [].

is_lib_avail(DictKey, Mod, Hrl, Name) ->
    case erlang:get(DictKey) of
        undefined ->
            IsAvail = case code:lib_dir(Mod, include) of
                          {error, bad_name} ->
                              false;
                          Dir ->
                              filelib:is_regular(filename:join(Dir, Hrl))
                      end,
            erlang:put(DictKey, IsAvail),
            ?DEBUG("~s availability: ~p\n", [Name, IsAvail]),
            IsAvail;
        IsAvail ->
            IsAvail
    end.
