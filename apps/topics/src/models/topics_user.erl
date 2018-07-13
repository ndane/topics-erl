%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc User functionality.
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
        email/1,
        hash_password/1]).

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

%% @doc Hash the password of a user and return a user with a hashed password
%% @param User The user whos plaintext password needs hashing
-spec hash_password(user()) -> user().
hash_password(User) when is_record(User, user) ->
    {ok, Salt} = bcrypt:gen_salt(14),
    {ok, Hash} = bcrypt:hashpw(User#user.password, Salt),
    User#user{ password = Hash }.

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

%% @doc Save a user into the users table.<br /> 
%%      <b>NOTE:</b> This does not encrypt the password!
%%      You must do that first by calling topics_user:hash_password/1
%% @param User The user to save to the database
-spec save(user()) -> user().
save(User) ->
    db:query("INSERT INTO USERS (username, email, password) VALUES ($1, $2, $3)", [
        User#user.username,
        User#user.email,
        User#user.password
    ]),
    User.