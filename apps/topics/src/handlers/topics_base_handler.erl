-module(topics_base_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Res = cowboy_req:reply(200,
                           #{<<"Content-Type">> => <<"text/plain">>},
                           <<"Hello.\n">>,
                           Req),
    {ok, Res, State}.