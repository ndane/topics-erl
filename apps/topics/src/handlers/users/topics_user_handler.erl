%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to access the user resources
%%====================================================================
-module(topics_user_handler).
-behaviour(cowboy_handler).

-export([init/2,
        content_types_provided/2,
        allowed_methods/2,
        is_authorized/2,
        resource_exists/2,
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

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

is_authorized(Req, State) ->
    % TODO: Check JWT scope for /user/:id
    {true, Req, State}.

resource_exists(Req, State) ->
    % TODO: Check user with id :id exists in the DB
    {true, Req, State}.

%%====================================================================
%% Request Types
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