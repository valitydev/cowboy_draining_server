-module(dummy_sup).

-behaviour(supervisor).

-export([start/1]).
-export([stop/0]).
-export([get_address/0]).

-export([start_link/1]).

-export([init/1]).

-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).

-spec start(Delay :: pos_integer()) ->
    ok.
start(Delay) ->
    dummy_sup:start_link(Delay),
    unlink(whereis(dummy_sup)),
    ok.

-spec stop() ->
    ok.
stop() ->
    exit(whereis(?MODULE), kill),
    ok.

-spec get_address() ->
    string().
get_address() ->
    {IP, Port} = ranch:get_addr(?MODULE),
    inet:ntoa(IP) ++ ":" ++ integer_to_list(Port).

-spec start_link(Delay :: pos_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}}.
start_link(Delay) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Delay]).

-spec init([Delay :: pos_integer()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Delay]) ->
    {ok, {{one_for_all, 0, 1}, [child_spec(Delay)]}}.

-spec child_spec(Delay :: pos_integer())
    -> supervisor:child_spec().
child_spec(Delay)
    -> {Transport, TransportOpts} = get_socket_transport(),
       Route      = [{'_', [ {"/", dummy_handler, [Delay]}]}],
       Dispatch   = cowboy_router:compile(Route),
       CowboyOpts = #{env => #{dispatch => Dispatch}},
       Protocol   = cowboy_clear,
       cowboy_draining_server:child_spec(
           ?MODULE,
           Transport,
           TransportOpts,
           Protocol,
           CowboyOpts,
           5000
       ).

-spec get_socket_transport()
    -> {module(), ranch:opts()}.
get_socket_transport() ->
    AcceptorsPool = ?DEFAULT_ACCEPTORS_POOLSIZE,
    {
        ranch_tcp,
        #{
            num_acceptors => AcceptorsPool
         }
    }.
