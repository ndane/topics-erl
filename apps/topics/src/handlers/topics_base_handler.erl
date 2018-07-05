-module(topics_base_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%% REST Exports 
-export([
    allowed_methods/2,
    content_types_provided/2,
    to_json/2,
    to_html/2
]).

-include("records/user.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"HEAD">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    ContentTypes = [
        {<<"text/html">>, to_html},
        {<<"application/json">>, to_json}
    ],
    {ContentTypes, Req, State}.

to_json(Req, State) ->
    User = #user{username="Nathan", email="nathan@nathandane.co.uk"},
    {to_json(User), Req, State}.

to_html(Req, State) ->
    User = #user{username="Nathan", email="nathan@nathandane.co.uk"},
    Body = io_lib:format("<h1>~s</h1><b />~s", [User#user.username,
                                                User#user.email]),
    {list_to_binary(Body), Req, State}.