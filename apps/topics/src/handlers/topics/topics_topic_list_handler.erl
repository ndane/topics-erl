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
        create_from_json/2,
        to_html/2]).

%%====================================================================
%% Initializer
%%====================================================================

init(Req, State) ->
    {cowboy_rest, Req, State}.

%%====================================================================
%% REST Flow
%%====================================================================

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, create_from_json}], Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

%%====================================================================
%% Parsers
%%====================================================================

to_json(Req, State) ->
    Topics = topics_topic:all(),
    {topics_topic:to_json(Topics), Req, State}.

create_from_json(Req, State) ->
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),
    Body = io_lib:format("~s", [ReqBody]),
    PostedTopic = topics_topic:from_json(Body),
    topics_topic:save(PostedTopic),
    {true, Req2, State}.

to_html(Req, State) ->
    {<<"Implement me">>, Req, State}.