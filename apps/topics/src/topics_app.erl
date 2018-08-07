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
    db:connect("localhost", "postgres", "postgres", "topics"),
    Routes = [
        {'_', [
            % Root
            {"/", topics_base_handler, []},

            % Topics
            {"/topics", topics_topic_list_handler, []}, 
            {"/topics/:id", topics_topic_handler, []},

            % Users
            {"/users", topics_user_handler, []},
            {"/users/:id", topics_user_handler, []},

            % Auth
            {"/auth", topics_auth_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(topics_http_listener, 
                                [{port, 8080}],
                                #{env => #{dispatch => Dispatch}}),
    topics_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    db:close().

%%====================================================================
%% Internal functions
%%====================================================================
