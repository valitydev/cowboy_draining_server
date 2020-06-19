-module(dummy_handler).

-export([init/2]).

init(Req0, [Timeout] = Opts) ->
    Headers = #{<<"content-type">> => <<"text/plain">>},
    timer:sleep(Timeout),
    Req = cowboy_req:reply(200, Headers, <<"Hello world!">>, Req0),
    {ok, Req, Opts}.
