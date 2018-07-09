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

-spec to_json(Topic :: topic()) -> iolist().
to_json(Topic) ->
    Map = #{
        title => list_to_binary(Topic#topic.title),
        username => list_to_binary(Topic#topic.username),
        body => list_to_binary(Topic#topic.body)
    },
    jiffy:encode(Map).