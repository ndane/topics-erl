%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc Module for creating JWT tokens for users
%%====================================================================
-module(t_jwt).

-export([generate/1]).

-spec secret_key() -> binary().
secret_key() ->
  <<"A8881867-9809-44C4-A8F7-AA721326182C">>.

-spec generate(Username :: string()) -> string().
generate(Username) ->
  {ok, User} = t_user:find_by_username(Username),
  Jwt = [
    {name, list_to_binary(t_user:username(User))},
    {admin, true},
    {scopes, []}
  ],
  jwerl:sign(Jwt, hs512, secret_key()).