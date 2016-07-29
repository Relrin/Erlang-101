-module(handlers).

-export([accept/2, handle_connection/3]).


% Public API

accept(WorkerId, ListenSocket) ->
    io:format("Socket #~p wait for client~n", [WorkerId]),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format("Socket #~p session started~n", [WorkerId]),
            handle_connection(WorkerId, ListenSocket, Socket);
        {error, Reason} ->
            io:format("Socket #~p can't accept client request. Reason: ~p~n", [WorkerId, Reason])
    end.


handle_connection(WorkerId, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Request} ->
            Message = binary:part(Request, 0, byte_size(Request) - 2),
            Handlers_args = get_handler_args(Message),
            Response = handle(Handlers_args),
            gen_tcp:send(Socket, <<Response/binary, "\r\n">>),
            handle_connection(WorkerId, ListenSocket, Socket);
        {error, Reason} ->
            io:format("Socket #~p session has finished. Reason: ~p~n", [WorkerId, Reason]),
            accept(WorkerId, ListenSocket)
    end.


% Private API

get_handler_args(Message) ->
    case binary:split(Message, <<" ">>) of
        % [<<"GET">>, Data] -> ok;
        % [<<"GETS">>, Data] -> ok;
        % [<<"ADD">>, Data] -> ok;
        [<<"SET">>, Data] -> get_key_value(set, Data);
        % [<<"REPLACE">>, Data] -> ok;
        % [<<"APPEND">>, Data] -> ok;
        % [<<"PREPEND">>, Data] -> ok;
        % [<<"DELETE">>, Data] -> ok;
        _ -> unknown
    end.


get_key_value(Action, Data) ->
    case binary:split(Data, <<" ">>) of
        [Key, Value] -> {Action, Key, Value};
        _ -> unknown
    end.


handle({set, Key, Value}) ->
    memcache:set(Key, Value),
    <<"STORED">>;


handle(unknown) ->
    <<"UNKNOWN REQUEST">>.
