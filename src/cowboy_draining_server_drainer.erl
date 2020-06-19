-module(cowboy_draining_server_drainer).

-behaviour(gen_server).

-type options()
    :: #{ shutdown  := timeout()
        , ranch_ref := ranch:ref()
        }.

-type shutdown()
    :: brutal_kill | pos_integer().

-export(
    [ child_spec/1
    , start_link/1
    ]).

-export(
    [ init/1
    , handle_call/3
    , handle_cast/2
    , terminate/2
    ]).

%%====================================================================
%% API functions
%%====================================================================

-spec child_spec(Opts) ->
    supervisor:child_spec() when Opts :: options().
child_spec(Opts) ->
    RanchRef = maps:get(ranch_ref, Opts),
    Shutdown = get_shutdown_param(Opts),
    #{ id       => ?MODULE
     , start    => {?MODULE, start_link, [RanchRef]}
     , shutdown => Shutdown
     }.

-spec start_link(RanchRef) ->
    genlib_gen:start_ret() when RanchRef :: ranch:ref().
start_link(RanchRef) ->
    gen_server:start_link(?MODULE, RanchRef, []).

%%====================================================================
%% Behaviour callbacks
%%====================================================================

-spec init(RanchRef) ->
    {ok, RanchRef} when RanchRef :: ranch:ref().
init(RanchRef) ->
    process_flag(trap_exit, true),
    {ok, RanchRef}.

-spec handle_call(_Msg, _From, RanchRef) ->
    {noreply, RanchRef} when RanchRef :: ranch:ref().
handle_call(_Msg, _From, RanchRef) ->
    {noreply, RanchRef}.

-spec handle_cast(_Msg, RanchRef) ->
    {noreply, RanchRef} when RanchRef :: ranch:ref().
handle_cast(_Msg, RanchRef) ->
    {noreply, RanchRef}.

-spec terminate(_Msg, RanchRef) ->
    ok when RanchRef :: ranch:ref().
terminate(shutdown, Ref) ->
    ok = ranch:suspend_listener(Ref),
    ok = ranch:wait_for_connections(Ref, '==', 0);
terminate(_Msg, _RanchRef) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_shutdown_param(Opts) ->
    shutdown() when Opts :: options().
get_shutdown_param(#{shutdown := 0}) ->
    brutal_kill;
get_shutdown_param(#{shutdown := Timeout}) ->
    Timeout.
