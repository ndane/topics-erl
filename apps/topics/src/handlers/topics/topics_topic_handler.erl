%% RESTful Module for return singular topic records
-module(topics_topic_handler).
-behaviour(cowboy_handler).

-export([init/2,
         content_types_provided/2,
         to_json/2,
         to_html/2]).

-include("records/topic.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

to_json(Req, State) ->
    Body = io_lib:format("{~s}", [extract_id(Req)]),
    {list_to_binary(Body), Req, State}.

to_html(Req, State) ->
    Body = io_lib:format("<h1>Topic ~s</h1>", [extract_id(Req)]),
    {list_to_binary(Body), Req, State}.

% Internal API

extract_id(Req) ->
    cowboy_req:binding(id, Req).