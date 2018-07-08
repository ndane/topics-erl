-module(db).

-export([
    connect/0,
    connection/1,
    get_connection/0
]).

connect() ->
    Res = epgsql:connect("localhost", "postgres", "postgres", [
        {database, "topics"},
        {timeout, 4000}
    ]).

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