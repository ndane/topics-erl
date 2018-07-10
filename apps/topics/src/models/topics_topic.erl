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
-export([to_json/1]).

% Database Queries
-export([all/0,
        get_by_id/1]).

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

-spec all() -> [topic()].
all() ->
    {ok, _Columns, Rows} = db:query("SELECT * FROM TOPICS"),
    Topics = [from_db_row(R) || R <- Rows],
    Topics.

-spec get_by_id(Id :: string()) -> topic().
get_by_id(Id) ->
    {ok, _Columns, Rows} = db:query("SELECT * FROM TOPICS WHERE Id = $1", [Id]),
    [Topic | _] = [from_db_row(R) || R  <- Rows],
    Topic.