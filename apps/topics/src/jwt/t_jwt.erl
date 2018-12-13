%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc Module for creating JWT tokens for users
%%====================================================================
-module(t_jwt).

-export([generate/1,
        generate/3,
        verify/1,
        scopes/1]).

-spec secret_key() -> binary().
secret_key() ->
    <<"A8881867-9809-44C4-A8F7-AA721326182C">>.

-spec generate(Username :: string()) -> binary().
generate(Username) ->
    {ok, User} = t_user:find_by_username(Username),
    generate(t_user:username(User), t_user:is_admin(User), t_user:scopes(User)).

-spec generate(Username :: string(), IsAdmin :: true | false, Scopes :: [atom()]) -> binary().
generate(Username, IsAdmin, Scopes) ->
    Jwt = [
        {name, list_to_binary(Username)},
        {sub, list_to_binary(Username)},
        {admin, IsAdmin},
        {scopes, Scopes}
    ],
    jwerl:sign(Jwt, hs512, secret_key()).

-spec verify(Jwt :: string()) -> {atom(), map()}.
verify(Jwt) ->
    jwerl:verify(Jwt, hs512, secret_key()).

% Verifies the JWT and returns a list of scopes
-spec scopes(Jwt :: binary()) -> {ok | invalid, [atom()]}.
scopes(Jwt) ->
    {ok, #{scopes := Scopes}} = verify(Jwt),
    {ok, [binary_to_atom(S, utf8) || S <- Scopes]}.