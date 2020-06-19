-module(cowboy_draining_server).

-export([child_spec/5]).

%%====================================================================
%% API functions
%%====================================================================

-spec child_spec(RanchRef, Transport, TransportOpts, CowboyOpts, Timeout) ->
    supervisor:child_spec() when RanchRef       :: ranch:ref()
                               , Transport      :: ranch:module()
                               , TransportOpts  :: ranch:opts()
                               , CowboyOpts     :: cowboy:opts()
                               , Timeout        :: pos_integer() | brutal_kill.
child_spec(RanchRef, Transport, TransportOpts, CowboyOpts, Timeout) ->
    Ranch   = ranch:child_spec(RanchRef, Transport, TransportOpts, cowboy_clear, CowboyOpts),
    Drainer = cowboy_draining_server_drainer:child_spec(#{ranch_ref => RanchRef, shutdown => Timeout}),
    SupOpts = #{strategy => one_for_all},
    #{ id    => RanchRef
     , type  => supervisor
     , start => {genlib_adhoc_supervisor, start_link, [ SupOpts, [Ranch, Drainer] ]}
     }.

