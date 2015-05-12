-module(sigsegv_compiler).
-behaviour(gen_server).
-export([
        start_link/0,
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).
-include("sigsegv.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    compile(),
    erlang:garbage_collect(),
    erlang:start_timer(0, self(), compile),
    {ok, no_state}.

handle_call(_Call, _From, _State) ->
    {reply, undefined, no_state}.

handle_cast(_Cast, _State) ->
    {noreply, no_state}.

handle_info({timeout, _TRef, compile}, _State) ->
    compile(),
    erlang:garbage_collect(),
    erlang:start_timer(0, self(), compile),
    {noreply, no_state};
handle_info(_Info, _State) ->
    {noreply, no_state}.

code_change(_OldVsn, _State, _Extra) ->
    {ok, no_state}.

terminate(_Reason, _State) ->
    ok.

compile() ->
    %% For our test module, we're just going to pre-compile a dict record
    %% and have our export fetch from that.
    Dict        = lists:foldl(fun(Key, DictAcc) -> dict:store(Key, crypto:rand_bytes(61440), DictAcc) end, dict:new(), ?KEYS),
    DictAbs     = erl_syntax:abstract(Dict),

    Forms = [
        erl_syntax:attribute(erl_syntax:atom("module"), [erl_syntax:atom(?TARGET_MODULE)]),
        erl_syntax:attribute(erl_syntax:atom("export"), [erl_syntax:list([
                        erl_syntax:arity_qualifier(erl_syntax:atom("fetch"), erl_syntax:integer(1))
                    ])]),
        erl_syntax:function(erl_syntax:atom("fetch"), [
                erl_syntax:clause(
                    [erl_syntax:variable("Key")],
                    [],
                    [
                        erl_syntax:application(erl_syntax:atom("dict"), erl_syntax:atom("fetch"), [erl_syntax:variable("Key"), DictAbs])
                    ]
                )
            ])
    ],
    Reverted = erl_syntax:revert_forms(Forms),
    {ok, Module, Binary} = compile:forms(Reverted),

    code:purge(?TARGET_MODULE),
    code:load_binary(Module, atom_to_list(Module) ++ ".erl", Binary),
    code:purge(?TARGET_MODULE).
