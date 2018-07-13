%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to access the topics list - /topics
%%====================================================================

-module(topics_topic_list_handler).
-behaviour(cowboy_handler).

-export([init/2,
        content_types_provided/2,
        content_types_accepted/2,
        allowed_methods/2,
        to_json/2,
        from_json/2,
        to_html/2]).

%%====================================================================
%% Initializer
%%====================================================================

init(Req, State) ->
    {cowboy_rest, Req, State}.

%%====================================================================
%% REST Handlers
%%====================================================================

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

%%====================================================================
%% Parsers
%%====================================================================

to_json(Req, State) ->
    Topics = topics_topic:all(),
    {topics_topic:to_json(Topics), Req, State}.

to_html(Req, State) ->
    {<<"Implement me">>, Req, State}.

%%====================================================================
%% REST Methods
%%====================================================================

%% Called from PUT Request
from_json(Req, State) ->
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),
    Body = io_lib:format("~s", [ReqBody]),
    Topic = topics_topic:save(topics_topic:from_json(Body)),
    Req3 = cowboy_req:reply(200, #{}, topics_topic:to_json(Topic), Req2),
    {stop, Req3, State}.