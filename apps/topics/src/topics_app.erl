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
    lager:start(),
    db:connect("localhost", "postgres", "postgres", "topics"),
    Routes = [
        {'_', [
            % Root
            {"/", topics_base_handler, []},

            % Topics
            {"/topics", t_topic_list_handler, []}, 
            {"/topics/:id", t_topic_handler, []},

            % Users
            {"/users", t_user_handler, []},
            {"/users/:id", t_user_handler, []},

            % Auth
            {"/auth", t_auth_handler, []}
        ]}
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(topics_http_listener, 
                                [{port, 8080}],
                                #{
                                    env => #{dispatch => Dispatch},
                                    middlewares => [t_logger_middleware, 
                                                    cowboy_router,
                                                    cowboy_handler]
                                }),
    topics_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    db:close().

%%====================================================================
%% Internal functions
%%====================================================================
