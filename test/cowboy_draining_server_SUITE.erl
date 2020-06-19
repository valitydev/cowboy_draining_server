-module(cowboy_draining_server_SUITE).

-export([ all/0
        , groups/0]).

-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ shutdown_test/1
        , request_interrupt_test/1
        ]).

all() -> [ { group, all_tests } ].

groups() -> [ { all_tests
              , []
              , [ shutdown_test
                , request_interrupt_test
                ]
              }
            ].

-define(NUMBER_OF_WORKERS, 10).

init_per_suite(C) ->
    genlib_app:start_application(hackney),
    C.

init_per_testcase(shutdown_test, C) ->
    genlib_app:start_application(ranch),
    dummy_sup:start_link(2000),
    unlink(whereis(dummy_sup)),
    C;
init_per_testcase(request_interrupt_test, C) ->
    genlib_app:start_application(ranch),
    dummy_sup:start_link(20000),
    unlink(whereis(dummy_sup)),
    C.

end_per_testcase(_Name, _C) ->
    application:stop(ranch),
    ok.

end_per_suite(_C) ->
    ok.

shutdown_test(_C) ->
    ok = spawn_workers(self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    dummy_sup:stop(),
    ok = receive_loop(fun(ok) -> ok end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun({error, econnrefused}) -> ok end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

request_interrupt_test(_C) ->
    ok = spawn_workers(self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    dummy_sup:stop(),
    ok = receive_loop(fun({error, timeout}) -> ok end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun({error, econnrefused}) -> ok end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

%

receive_loop(_, N, _Timeout) when N =< 0 ->
    ok;
receive_loop(MatchFun, N, Timeout) ->
    receive
        {result, Result} ->
            MatchFun(Result)
    after Timeout ->
        error(timeout)
    end,
    receive_loop(MatchFun, N - 1, Timeout).

spawn_workers(_, N) when N =< 0 ->
    ok;
spawn_workers(ParentPID, N) ->
    erlang:spawn_link(fun() -> worker(ParentPID) end),
    spawn_workers(ParentPID, N - 1).

worker(ParentPID) ->
    Result =
        case hackney:get("0.0.0.0:8080") of
            {error, _} = E -> E;
            _Success -> ok
        end,
    ParentPID ! {result, Result}.
