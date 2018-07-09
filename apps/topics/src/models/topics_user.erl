-module(topics_user).

-record(user, {
    username :: string(),
    email :: string()
}).

-opaque user() :: #user{}.
-export_type([user/0]).

% Initializer
-export([new/2]).

% Getters
-export([username/1,
        email/1]).

% Parsers
-export([to_json/1]).

%% User Initializer

-spec new(Username :: string(), Email :: string()) -> user().
new(Username, Email) ->
    #user{
        username = Username,
        email = Email
    }.

%% Getters

-spec username(user()) -> string().
username(User) -> User#user.username.

-spec email(user()) -> string().
email(User) -> User#user.email.

%% Parsers

-spec to_json(user()) -> iodata().
to_json(User) when is_record(User, user) ->
    Map = #{
        username => list_to_binary(User#user.username),
        email => list_to_binary(User#user.email)
    },
    jiffy:encode(Map).