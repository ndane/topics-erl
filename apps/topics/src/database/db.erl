-module(db).

-export([connect/0,
        connection/1,
        get_connection/0,
        close/0]).

connect() ->
    {ok, Res} = epgsql:connect("localhost", "postgres", "postgres", [
        {database, "topics"},
        {timeout, 4000}
    ]),
    Pid = spawn_link(?MODULE, connection, [Res]),
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