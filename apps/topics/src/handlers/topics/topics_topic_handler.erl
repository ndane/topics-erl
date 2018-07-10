%%====================================================================
%% REST Cowboy Module to access topics
%%====================================================================
-module(topics_topic_handler).
-behaviour(cowboy_handler).

-export([
    init/2,
    content_types_provided/2,
    to_json/2,
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


%%====================================================================
%% Parsers
%%====================================================================

to_json(Req, State) ->
    Id = extract_id(Req),
    Topic = topics_topic:get_by_id(Id),
    {topics_topic:to_json(Topic), Req, State}.

to_html(Req, State) ->
    Id = extract_id(Req),
    Topic = topics_topic:get_by_id(Id),
    HtmlBody = io_lib:format("<h1>Topic ~s</h1><br />Posted by: ~s<br /><p />~s", [
        topics_topic:title(Topic),
        topics_topic:username(Topic),
        topics_topic:body(Topic)
    ]),
    {list_to_binary(HtmlBody), Req, State}.


%%====================================================================
%% Internal API
%%====================================================================

extract_id(Req) ->
    cowboy_req:binding(id, Req).