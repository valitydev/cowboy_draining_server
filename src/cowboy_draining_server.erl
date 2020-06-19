-module(cowboy_draining_server).

-export([child_spec/5]).

%%====================================================================
%% API functions
%%====================================================================

-spec child_spec(
    RanchRef       :: ranch:ref(),
    Transport      :: ranch:module(),
    TransportOpts  :: ranch:opts(),
    CowboyOpts     :: cowboy:opts(),
    Timeout        :: non_neg_integer()
) ->
    supervisor:child_spec().
child_spec(
    RanchRef,
    Transport,
    TransportOpts,
    CowboyOpts,
    Timeout
) ->
    SupOpts  = #{strategy => one_for_all},
    Protocol = cowboy_clear,
    Ranch    = ranch:child_spec(
        RanchRef,
        Transport,
        TransportOpts,
        Protocol,
        CowboyOpts
    ),
    Drainer  = cowboy_draining_server_drainer:child_spec(#{
        ranch_ref => RanchRef,
        shutdown  => Timeout
    }),
    #{
        id    => RanchRef ,
        type  => supervisor ,
        start => {
            genlib_adhoc_supervisor,
            start_link,
            [SupOpts, [Ranch, Drainer]]
        }
     }.
