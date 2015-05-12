-module(sigsegv_worker_sup).
-behaviour(supervisor).
-export([
        start_link/0,
        start_children/1,
        init/1
    ]).
-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_children(N) when N > 0 ->
    io:format("Starting child... (~p remain)~n", [N - 1]),
    supervisor:start_child(?MODULE, []),
    start_children(N - 1);
start_children(0) ->
    ok.

init([]) ->
    timer:apply_after(1000, ?MODULE, start_children, [64]),
    {ok, {
            {simple_one_for_one, 5, 10},
            [
                ?CHILD(sigsegv_worker, worker)
            ]
        }}.
