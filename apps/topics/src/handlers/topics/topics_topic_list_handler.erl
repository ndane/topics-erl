%$ RESTful module handling topic lists
-module(topics_topic_list_handler).
-behaviour(cowboy_handler).

-export([
    init/2,
    content_types_provided/2,
    allowed_methods/2,
    to_json/2,
    to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"HEAD">>, <<"GET">>, <<"OPTIONS">>], Req, State}.

to_json(Req, State) ->
    Topics = topics_topic:all(),
    {topics_topic:to_json(Topics), Req, State}.

to_html(Req, State) ->
    {<<"Implement me">>, Req, State}.