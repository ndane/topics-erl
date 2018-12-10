%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc User functionality.
%%====================================================================

-module(t_user).

-record(user, {
    id = "" :: string(),
    username :: string(),
    email :: string(),
    password = undefined :: string(),
    scopes = [readonly] :: list(atom()),
    admin = false :: atom()
}).

-opaque user() :: #user{}.
-export_type([user/0]).

% Initializer
-export([new/3]).

% Getters
-export([username/1,
        email/1,
        password/1,
        scopes/1,
        is_admin/1]).

% Parsers
-export([to_json/1,
        from_json/1]).

% Database Queries
-export([save/1, 
        find_by_username/1]).

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

-spec password(user()) -> string().
password(User) ->
    User#user.password.

-spec scopes(user()) -> list(atom()).
scopes(User) ->
    User#user.scopes.

-spec is_admin(user()) -> true | false.
is_admin(User) ->
    User#user.admin.

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
        username => list_to_binary(User#user.username),
        email => list_to_binary(User#user.email)
    },
    jiffy:encode(Map).

-spec from_json(string()) -> user().
from_json(Json) ->
    Map = jiffy:decode(Json, [return_maps]),
    #{<<"username">> := Username, <<"email">> := Email, <<"password">> := Password} = Map,
    #user{
        username = binary_to_list(Username), 
        email = binary_to_list(Email),
        password = binary_to_list(Password)
    }.

-spec from_db_row(tuple()) -> user().
from_db_row(Row) ->
    {Id, Username, Email, Password, Admin, _CreatedAt, _UpdatedAt} = Row,
    #user{
        id = binary_to_list(Id),
        username = binary_to_list(Username), 
        email = binary_to_list(Email),
        password = binary_to_list(Password),
        admin = Admin
    }.

%%====================================================================
%% Internal API
%%====================================================================

%%====================================================================
%% Database Queries
%%====================================================================

%% @doc Save a user into the users table.<br /> 
%% @param User The user to save to the database
-spec save(user()) -> user().
save(User) ->
    HashedUser = hash_password(User),
    Query = "INSERT INTO USERS (username, email, password) VALUES ($1, $2, $3) RETURNING *",
    {ok, 1, _Columns, [Row | _]} = db:query(Query, [
        HashedUser#user.username,
        HashedUser#user.email,
        HashedUser#user.password
    ]),
    from_db_row(Row).

-spec find_by_username(string()) -> {atom(), user()}.
find_by_username(Username) ->
    {ok, _Columns, [Row | _]} = db:query("SELECT * FROM USERS WHERE Username = $1", [Username]),
    {ok, from_db_row(Row)}.