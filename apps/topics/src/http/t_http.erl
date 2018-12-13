%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc HTTP functions
%%====================================================================
-module(t_http).

-export([atom_to_code/1,
         authorization_from_req/1]).

-spec atom_to_code(atom()) -> number().
atom_to_code(ok) -> 200;
atom_to_code(created) -> 201.

% Extract authentication bearer token from a cowboy request
-spec authorization_from_req(Req :: cowboy_req:req()) -> {ok, Token :: binary()} | {error, Reason :: string()}.
authorization_from_req(Req) ->
  {bearer, Token} = cowboy_req:parse_header(<<"authorization">>, Req),
  {ok, Token}.