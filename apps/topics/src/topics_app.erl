%%%-------------------------------------------------------------------
%% @doc topics public API
%% @end
%%%-------------------------------------------------------------------

-module(topics_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Routes = [
        {'_', [
            {"/", topics_base_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(topics_http_listener, 
                                [{port, 8080}],
                                #{env => #{dispatch => Dispatch}}),
    topics_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
