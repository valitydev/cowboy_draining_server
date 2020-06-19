-module(dummy_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([stop/0]).

-export([init/1]).

-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-spec start_link(pos_integer()) ->
    {ok, pid()} | {error, {already_started, pid()}}.
start_link(Delay) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Delay]).

-spec stop() ->
    ok.
stop() ->
    exit(whereis(?MODULE), kill).

-spec init([pos_integer()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Delay]) ->
    {ok, {{one_for_all, 0, 1}, [child_spec(Delay)]}}.

-spec child_spec(pos_integer())
    -> supervisor:child_spec().
child_spec(Delay)
    -> {Transport, TransportOpts} = get_socket_transport(),
       Dispatch = cowboy_router:compile([{'_', [ {"/", dummy_handler, [Delay]}]}]),
       CowboyOpts = #{ env => #{ dispatch => Dispatch } },
       #{ id => ?MODULE
        , type => supervisor
        , start =>
            { genlib_adhoc_supervisor
            , start_link
            , [ #{strategy => one_for_all}
              , [ ranch:child_spec(?MODULE, Transport, TransportOpts, cowboy_clear, CowboyOpts)
                , cowboy_draining_server:child_spec(#{ranch_ref => ?MODULE, shutdown => 5000})
                ]
              ]
            }
        }.

-spec get_socket_transport()
    -> {module(), ranch:opts()}.
get_socket_transport() ->
    {ok, IP}      = inet:parse_address(?DEFAULT_IP_ADDR),
    Port          = ?DEFAULT_PORT,
    AcceptorsPool = ?DEFAULT_ACCEPTORS_POOLSIZE,
    {ranch_tcp, #{socket_opts => [{ip, IP}, {port, Port}], num_acceptors => AcceptorsPool}}.