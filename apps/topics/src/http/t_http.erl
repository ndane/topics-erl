%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc HTTP functions
%%====================================================================
-module(t_http).

-export([atom_to_code/1]).

-spec atom_to_code(atom()) -> number().
atom_to_code(ok) -> 200;
atom_to_code(created) -> 201.