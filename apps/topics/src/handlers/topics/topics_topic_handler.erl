%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to access topic resources - /topics/:id
%%====================================================================

-module(topics_topic_handler).
-behaviour(cowboy_handler).

-export([init/2,
        content_types_provided/2,
        allowed_methods/2,
        resource_exists/2,
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
    {[{<<"application/json">>, to_json},
      {<<"text/html">>, to_html}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

resource_exists(Req, State) ->
    Id = extract_id(Req),
    Exists = topics_topic:exists(Id),
    {Exists, Req, State}.

%%====================================================================
%% REST Methods
%%====================================================================

to_json(Req, State) ->
    Id = extract_id(Req),
    Topic = topics_topic:get_by_id(Id),
    {topics_topic:to_json(Topic), Req, State}.

from_json(Req, State) ->
    ok.

to_html(Req, State) ->
    Id = extract_id(Req),
    Topic = topics_topic:get_by_id(Id),
    Body = io_lib:format("<h1>Topic ~s</h1><br />Posted by: ~s<br /><p />~s", [
        topics_topic:title(Topic),
        topics_topic:username(Topic),
        topics_topic:body(Topic)
    ]),
    {list_to_binary(Body), Req, State}.

%%====================================================================
%% Internal API
%%====================================================================

extract_id(Req) ->
    cowboy_req:binding(id, Req).