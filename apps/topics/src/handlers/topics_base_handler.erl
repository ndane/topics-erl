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
    User = t_user:new("Nathan", "nathan@nathandane.co.uk"),
    {t_user:to_json(User), Req, State}.

to_html(Req, State) ->
    User = t_user:new("Nathan", "nathan@nathandane.co.uk"),
    Body = io_lib:format("<h1>~s</h1><b />~s", [t_user:username(User),
                                                t_user:email(User)]),
    {list_to_binary(Body), Req, State}.