-module(dummy_handler).

-export([init/2]).

init(Req0, [Delay] = Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    timer:sleep(Delay),
    Req = cowboy_req:reply(200, Headers, <<"Hello world!">>, Req0),
    {ok, Req, Opts}.
