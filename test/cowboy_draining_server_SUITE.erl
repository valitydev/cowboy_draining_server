-module(cowboy_draining_server_SUITE).

-export([ all/0 ]).

-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ shutdown_test/1
        , request_interrupt_test/1
        ]).

-include_lib("common_test/include/ct.hrl").

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].

-spec all() ->
    [test_case_name()].
all() ->
    [ shutdown_test
    , request_interrupt_test
    ].

-define(NUMBER_OF_WORKERS, 10).

-spec init_per_suite(config()) ->
    config().
init_per_suite(C) ->
    genlib_app:start_application(ranch),
    genlib_app:start_application(hackney),
    C.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(Name, C) ->
    [{name, Name} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    ok.
end_per_testcase(_Name, _C) ->
    ok.

-spec end_per_suite(config()) ->
    ok.
end_per_suite(_C) ->
    ok = application:stop(ranch),
    ok = application:stop(hackney).

-spec shutdown_test(config()) ->
    ok.
shutdown_test(C) ->
    Name = ?config(name, C),
    Delay = 2000,
    ok = dummy_sup:start(Name, Delay),
    Address = dummy_sup:get_address(?config(name, C)),
    ok = spawn_workers(Address, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = dummy_sup:stop(),
    ok = receive_loop(fun(ok) -> ok end,
                      ?NUMBER_OF_WORKERS,
                      timer:seconds(20)),
    ok = spawn_workers(Address, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun({error, econnrefused}) -> ok end,
                      ?NUMBER_OF_WORKERS,
                      timer:seconds(20)).

-spec request_interrupt_test(config()) ->
    ok.
request_interrupt_test(C) ->
    Name = ?config(name, C),
    Delay = 20000,
    ok = dummy_sup:start(Name, Delay),
    Address = dummy_sup:get_address(Name),
    ok = spawn_workers(Address, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = dummy_sup:stop(),
    ok = receive_loop(fun({error, timeout}) -> ok end,
                      ?NUMBER_OF_WORKERS,
                      timer:seconds(20)),
    ok = spawn_workers(Address, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun({error, econnrefused}) -> ok end,
                      ?NUMBER_OF_WORKERS,
                      timer:seconds(20)).

%

receive_loop(_MatchFun, N, _Timeout) when N =< 0 ->
    ok;
receive_loop( MatchFun, N,  Timeout) ->
    receive
        {result, Result} ->
            MatchFun(Result)
    after Timeout ->
        error(timeout)
    end,
    receive_loop(MatchFun, N - 1, Timeout).

spawn_workers(_, _, N) when N =< 0 ->
    ok;
spawn_workers(Address, ParentPID, N) ->
    erlang:spawn_link(fun() -> worker(Address, ParentPID) end),
    spawn_workers(Address, ParentPID, N - 1).

worker(Address, ParentPID) ->
    Result =
        case hackney:request(Address) of
            {error, _} = E -> E;
            _Success -> ok
        end,
    ParentPID ! {result, Result}.
