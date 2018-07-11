%% @author Nathan Dane <nathan@nathandane.co.uk>
%% @copyright 2018 Nathan Dane
%% @doc Start and maintain a connection to a database

-module(db).

-export([connect/4,
        close/0,
        query/1,
        query/2]).

-spec connect(Host :: string(), 
              Username :: string(), 
              Password :: string(), 
              Database :: string()) -> any().
connect(Host, Username, Password, Database) ->
    {ok, Res} = epgsql:connect(Host, Username, Password, [
        {database, Database},
        {timeout, 4000}
    ]),
    Pid = spawn(fun() -> connection(Res) end),
    link(Pid),
    register(db_connection, Pid).

connection(Conn) ->
    receive 
        {From, connection} ->
            From ! {connection, Conn}
    end,
    connection(Conn). 

get_connection() ->
    db_connection ! {self(), connection},
    receive
        {connection, Conn} ->
            Conn
    end.

close() ->
    Conn = get_connection(),
    epgsql:close(Conn).

-spec query(Query :: string()) -> any().
query(Query) ->
    Conn = get_connection(),
    epgsql:equery(Conn, Query).

-spec query(Query :: string(), Params :: [any()]) -> any().
query(Query, Params) ->
    Conn = get_connection(),
    epgsql:equery(Conn, Query, Params).