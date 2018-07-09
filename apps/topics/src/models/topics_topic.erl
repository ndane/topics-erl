-module(topics_topic).

-record(topic, {
    title :: string(),
    username :: string(),
    body :: string()
}).

-opaque topic() :: #topic{}.
-export_type([topic/0]).

% Initializer Export
-export([new/3]).

% Getter Exports
-export([title/1,
        username/1,
        body/1]).

% Parser Exports
-export([to_json/1]).

% Database Queries
-export([all/0]).

%% Initializer
-spec new(Title :: string(), Username :: string(), Body :: string()) -> topic().
new(Title, Username, Body) ->
    #topic {
        title = Title,
        username = Username,
        body = Body
    }.

%% Getters

-spec title(Topic :: topic()) -> string().
title(Topic) -> 
    Topic#topic.title. 

-spec username(Topic :: topic()) -> string().
username(Topic) ->
    Topic#topic.username.

-spec body(Topic :: topic()) -> string().
body(Topic) ->
    Topic#topic.body.

%% Parsers

-spec to_map(Topic :: topic()) -> map().
to_map(Topic) ->
    #{
        title => list_to_binary(Topic#topic.title),
        username => list_to_binary(Topic#topic.username),
        body => list_to_binary(Topic#topic.body)
    }.

-spec to_json(Topic :: topic()) -> iolist();
             (Topics :: [topic()]) -> iolist().
to_json(Topic) when is_record(Topic, topic) ->
    Map = to_map(Topic),
    jiffy:encode(Map);

to_json(Topics) when is_list(Topics) ->
    List = [to_map(T) || T <- Topics],
    jiffy:encode(List).

%% Database Queries

-spec all() -> [topic()].
all() ->
    Connection = db:get_connection(),
    {ok, _Columns, Rows} = epgsql:equery(Connection, "SELECT * FROM TOPICS"),
    Topics = [#topic{
        title = binary_to_list(Title), 
        username = binary_to_list(Username),
        body = binary_to_list(Body)
    } || {_, Title, Username, Body, _, _} <- Rows],
    Topics.