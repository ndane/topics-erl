%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Middlware Module for logging requests
%%====================================================================
-module(t_logger_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%%====================================================================
%% Middleware API
%%====================================================================

execute(Req, Env) ->
    log(Req, Env),
    {ok, Req, Env}.

%%====================================================================
%% Internal API
%%====================================================================

log(Req, _Env) ->
    {IP, _} = cowboy_req:peer(Req),

    lager:info("[~p] ~s\t~s\t~s:~p\t~s", [
        IP,
        cowboy_req:method(Req),
        cowboy_req:scheme(Req),
        cowboy_req:host(Req),
        cowboy_req:port(Req),
        cowboy_req:path(Req)
    ]),
    ok.