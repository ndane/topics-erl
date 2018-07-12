-module(topics_base_handler).
-behaviour(cowboy_handler).

-export([init/2]).

%% REST Exports 
-export([
    allowed_methods/2,
    content_types_provided/2,
    to_html/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    Methods = [<<"GET">>, <<"HEAD">>],
    {Methods, Req, State}.

content_types_provided(Req, State) ->
    ContentTypes = [
        {<<"text/html">>, to_html}
    ],
    {ContentTypes, Req, State}.

to_html(Req, State) ->
    {<<"<h1>Nothing Here.</h1>">>, Req, State}.