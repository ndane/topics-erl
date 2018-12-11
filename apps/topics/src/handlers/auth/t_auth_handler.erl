%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc REST Cowboy Module to handle user authentication
%%====================================================================

-module(t_auth_handler).
-behaviour(cowboy_handler).

-export([
    init/2
]).

%% REST Exports 
-export([
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2,
    from_json/2,
    unauthenticated/2
]).

%% Rest Callbacks

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"POST">>],
    {Methods, Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, unauthenticated}], Req, State}.

from_json(Req, State) ->
    case get_token(Req) of
        {ok, Token, Req2} ->
            Res = #{
                <<"token">> => Token
            },
            Req3 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Res), Req2),
            {stop, Req3, State};

        {hash_mismatch, Req2} ->
            unauthenticated(Req2, State)
    end.

unauthenticated(Req, State) ->
    %% TODO: Shall we create and error module for crafting API errors?
    Res = #{
        <<"error">> => <<"Unauthenticated">>
    },
    Req2 = cowboy_req:reply(401, #{<<"content-type">> => <<"application/json">>}, jiffy:encode(Res), Req),
    {stop, Req2, State}.

%% Internal API

get_token(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Map = jiffy:decode(Body, [return_maps]),
    #{ <<"username">> := Username, <<"password">> := Password } = Map,
    {ok, User} = t_user:find_by_username(Username),
    Hash = t_user:password(User),
    case {ok, Hash} =:= bcrypt:hashpw(Password, Hash) of
        % TODO: Start using ID's instead of usernames internally?
        true -> {ok, t_jwt:generate(Username), Req2};
        false -> {hash_mismatch, Req2}
    end.