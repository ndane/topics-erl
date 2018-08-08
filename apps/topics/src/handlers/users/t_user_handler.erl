%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to access the user resources
%%====================================================================
-module(t_user_handler).
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
    {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>, <<"PUT">>, <<"POST">>], Req, State}.

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
    Username = extract_id(Req),
    case t_user:find_by_username(Username) of
        {ok, _} -> {true, Req, State};
        _ -> {false, Req, State}
    end.

%%====================================================================
%% REST Methods
%%====================================================================

to_json(Req, State) ->
    Username = extract_id(Req),
    {ok, User} = t_user:find_by_username(Username),
    {t_user:to_json(User), Req, State}.

from_json(Req, State) ->
    {ok, ReqBody, Req2} = cowboy_req:read_body(Req),
    Body = io_lib:format("~s", [ReqBody]),
    User = t_user:from_json(Body),
    HashedUser = t_user:hash_password(User),
    NewUser = t_user:save(HashedUser),
    Token = t_jwt:generate(t_user:username(NewUser), t_user:is_admin(NewUser), t_user:scopes(NewUser)),
    Res = #{
        <<"token">> => Token
    },
    Headers = #{ <<"content-type">> => <<"application/json">> },
    Req3 = cowboy_req:reply(201, Headers, jiffy:encode(Res), Req2),
    {stop, Req3, State}.

to_html(Req, State) ->
    {<<"Not Implemented">>, Req, State}.

%%====================================================================
%% Internal API
%%====================================================================

extract_id(Req) ->
    binary_to_list(cowboy_req:binding(id, Req)).