-module(dummy_handler).

-export([init/2]).

-spec init(cowboy:req(), [Delay :: pos_integer()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Req0, [Delay] = Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    timer:sleep(Delay),
    Req = cowboy_req:reply(200, Headers, <<"Hello world!">>, Req0),
    {ok, Req, Opts}.
