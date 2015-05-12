-module(sigsegv_worker).
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
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, {dict:new(), ?KEYS}, 0}.

handle_call(_Call, _From, State) ->
    {reply, undefined, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(timeout, {Counted, []}) ->
    Max = dict:fold(fun(K, C, {_OldK, M}) when C > M -> {K, C}; (_K, _C, M) -> M end, {'_', 0}, Counted),
    io:format("[~p] Max: ~p~n", [self(), Max]),
    handle_info(timeout, {dict:new(), ?KEYS});
handle_info(timeout, {_Counted, [Key|Keys]}) ->
    Value   = ?TARGET_MODULE:fetch(Key),
    Counted = lists:foldl(fun(D, Acc) -> dict:merge(fun(_K, V1, V2) -> V1 + V2 end, D, Acc) end, dict:new(), lists:map(fun(_) -> count_chars(Value, dict:new()) end, lists:seq(1,64))),
    erlang:garbage_collect(),
    {noreply, {Counted, Keys}, 0};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

count_chars(<<C, Rest/binary>>, Dict) ->
    case Dict:find(C) of
        error ->
            Dict2 = Dict:store(C, 1),
            count_chars(Rest, Dict2);
        {ok, Value} ->
            Dict2 = Dict:store(C, Value + 1),
            count_chars(Rest, Dict2)
    end;
count_chars(<<>>, Dict) ->
    Dict.
