%%====================================================================
%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc <b>topics_topic</b> contains topic type functions 
%%====================================================================

-module(topics_topic).

-record(topic, {
    id = "" :: string(),
    title :: string(),
    username :: string(),
    body :: string()
}).

-opaque topic() :: #topic{}.
-export_type([topic/0]).

% Initializer Export
-export([new/3]).

% Getter Exports
-export([id/1,
        title/1,
        username/1,
        body/1]).

% Parser Exports
-export([to_json/1,
        from_json/1]).

% Database Queries
-export([save/1,
        all/0,
        get_by_id/1,
        exists/1]).

%%====================================================================
%% Initializer
%%====================================================================

-spec new(Title :: string(), Username :: string(), Body :: string()) -> topic().
new(Title, Username, Body) ->
    #topic {
        title = Title,
        username = Username,
        body = Body
    }.

%%====================================================================
%% Getters
%%====================================================================

-spec id(Topic :: topic()) -> string().
id(Topic) ->
    Topic#topic.id.

-spec title(Topic :: topic()) -> string().
title(Topic) -> 
    Topic#topic.title. 

-spec username(Topic :: topic()) -> string().
username(Topic) ->
    Topic#topic.username.

-spec body(Topic :: topic()) -> string().
body(Topic) ->
    Topic#topic.body.

%%====================================================================
%% Parsers
%%====================================================================

-spec to_json(Topic :: topic()) -> iolist();
             (Topics :: [topic()]) -> iolist().
to_json(Topic) when is_record(Topic, topic) ->
    Map = to_map(Topic),
    jiffy:encode(Map);

to_json(Topics) when is_list(Topics) ->
    List = [to_map(T) || T <- Topics],
    jiffy:encode(List).

-spec from_json(string()) -> topic().
from_json(Json) ->
    Map = jiffy:decode(Json, [return_maps]),
    #{<<"title">> := Title, <<"username">> := Username, <<"body">> := Body} = Map,
    #topic{
        title = binary_to_list(Title), 
        username = binary_to_list(Username),
        body = binary_to_list(Body)
    }.

%%====================================================================
%% Internal API
%%====================================================================

-spec to_map(Topic :: topic()) -> map().
to_map(Topic) ->
    #{
        title => list_to_binary(Topic#topic.title),
        username => list_to_binary(Topic#topic.username),
        body => list_to_binary(Topic#topic.body)
    }.

-spec from_db_row(tuple()) -> topic().
from_db_row(Row) ->
    {Id, Title, Username, Body, _CreatedAt, _UpdatedAt} = Row,
    #topic{
        id = binary_to_list(Id),
        title = binary_to_list(Title), 
        username = binary_to_list(Username),
        body = binary_to_list(Body)
    }.

%%====================================================================
%% Database Queries
%%====================================================================

-spec save(topic()) -> topic().
save(Topic) ->
    Query = "INSERT INTO TOPICS (title, username, body) VALUES ($1, $2, $3) RETURNING *",
    {ok, 1, _Columns, [Row | _]} = db:query(Query, [
        Topic#topic.title,
        Topic#topic.username,
        Topic#topic.body
    ]),
    from_db_row(Row).

-spec all() -> [topic()].
all() ->
    {ok, _Columns, Rows} = db:query("SELECT * FROM TOPICS"),
    Topics = [from_db_row(R) || R <- Rows],
    Topics.

-spec get_by_id(Id :: string()) -> topic().
get_by_id(Id) ->
    {ok, _Columns, Rows} = db:query("SELECT * FROM TOPICS WHERE Id = $1 LIMIT 1", [Id]),
    Topic = from_db_row(hd(Rows)),
    Topic.

-spec exists(Id :: string()) -> atom().
exists(Id) ->
    {ok, _Columns, [Row | _]} = db:query("SELECT COUNT(1) FROM TOPICS WHERE ID = $1", [Id]),
    case Row of
        {0} -> false;
        {1} -> true
    end.