%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc User functionality 
%%====================================================================

-module(topics_user).

-record(user, {
    id = "" :: string(),
    username :: string(),
    email :: string(),
    password = undefined :: string()
}).

-opaque user() :: #user{}.
-export_type([user/0]).

% Initializer
-export([new/3]).

% Getters
-export([username/1,
        email/1]).

% Parsers
-export([to_json/1]).

% Database Queries
-export([save/1]).

%%====================================================================
%% Initializer
%%====================================================================

-spec new(Username :: string(), Email :: string(), Password :: string()) -> user().
new(Username, Email, Password) ->
    #user{
        username = Username,
        email = Email,
        password = Password
    }.

%%====================================================================
%% Getters
%%====================================================================

-spec username(user()) -> string().
username(User) -> 
    User#user.username.

-spec email(user()) -> string().
email(User) ->
    User#user.email.

%%====================================================================
%% Parsers
%%====================================================================

-spec to_json(user()) -> iodata().
to_json(User) when is_record(User, user) ->
    Map = #{
        id => list_to_binary(User#user.id),
        username => list_to_binary(User#user.username),
        email => list_to_binary(User#user.email)
    },
    jiffy:encode(Map).

%%====================================================================
%% Internal API
%%====================================================================

%%====================================================================
%% Database Queries
%%====================================================================

-spec save(user()) -> user().
save(User) ->
    db:query("INSERT INTO USERS (username, email, password) VALUES ($1, $2, $3)", [
        User#user.username,
        User#user.email,
        User#user.password
    ]),
    User.