-module(dummy_sup).

-behaviour(supervisor).

-export([start/2]).
-export([stop/0]).
-export([get_address/1]).

-export([start_link/2]).

-export([init/1]).

-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).

-spec start(Name :: term(), Delay :: pos_integer()) ->
    ok.
start(Name, Delay) ->
    dummy_sup:start_link(Name, Delay),
    unlink(whereis(?MODULE)),
    ok.

-spec stop() ->
    ok.
stop() ->
    exit(whereis(?MODULE), kill),
    ok.

-spec get_address(Name :: term()) ->
    string().
get_address(Name) ->
    {IP, Port} = ranch:get_addr({?MODULE, Name}),
    inet:ntoa(IP) ++ ":" ++ integer_to_list(Port).

-spec start_link(Name :: term(), Delay :: pos_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}}.
start_link(Name, Delay) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Name, Delay]).

-spec init(Args :: [term()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Name, Delay]) ->
    {ok, {{one_for_all, 0, 1}, [child_spec(Name, Delay)]}}.

-spec child_spec(Name :: term(), Delay :: pos_integer())
    -> supervisor:child_spec().
child_spec(Name, Delay)
    -> {Transport, TransportOpts} = get_socket_transport(),
       Route      = [{'_', [ {"/", dummy_handler, [Delay]}]}],
       Dispatch   = cowboy_router:compile(Route),
       CowboyOpts = #{env => #{dispatch => Dispatch}},
       Protocol   = cowboy_clear,
       cowboy_draining_server:child_spec(
           {?MODULE, Name},
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
