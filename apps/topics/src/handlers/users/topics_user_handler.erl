%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to access the user resources
%%====================================================================
-module(topics_user_handler).
-behaviour(cowboy_handler).

-export([init/2,
        content_types_provided/2,
        content_types_accepted/2,
        allowed_methods/2,
        is_authorized/2,
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
%% REST Flow
%%====================================================================
 
allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>, <<"PUT">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

is_authorized(Req, State) ->
    % TODO: Check JWT scope for /user/:id
    {true, Req, State}.

resource_exists(Req, State) ->
    % TODO: Check user with id :id exists in the DB
    {true, Req, State}.

%%====================================================================
%% REST Methods
%%====================================================================

to_json(Req, State) ->
    Id = extract_id(Req),
    Topic = topics_topic:get_by_id(Id),
    {topics_topic:to_json(Topic), Req, State}.

from_json(Req, State) ->
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),
    Body = io_lib:format("~s", [ReqBody]),
    User = topics_user:from_json(Body),
    HashedUser = topics_user:hash_password(User),
    NewUser = topics_user:save(HashedUser),
    Req3 = cowboy_req:reply(200, #{}, topics_user:to_json(NewUser), Req2),
    {stop, Req3, State}.

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