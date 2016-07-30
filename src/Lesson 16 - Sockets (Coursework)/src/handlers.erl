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
        [<<"GET">>, Key] -> {get, Key};
        [<<"GETS">>, Keys] -> {gets, binary:split(Keys, <<" ">>, [global])};
        [<<"ADD">>, Data] -> get_key_value(add, Data);
        [<<"SET">>, Data] -> get_key_value(set, Data);
        [<<"REPLACE">>, Data] -> get_key_value(replace, Data);
        [<<"APPEND">>, Data] -> get_key_value(append, Data);
        [<<"PREPEND">>, Data] -> get_key_value(prepend, Data);
        [<<"DELETE">>, Key] -> {delete, Key};
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


handle({get, Key}) ->
    case memcache:get(Key) of
        {ok, Value} -> <<"VALUE ", Key/binary, " ", Value/binary, "\r\nEND">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle({gets, Keys}) ->
    Values = memcache:gets(Keys),
    {[], Result} = lists:foldl(
        fun(Response, {[Key | Rest], Acc}) ->
                Value = case Response of
                    {ok, Val} -> Val;
                    {error, not_found} -> <<"NOT FOUND">>
                end,
                {Rest, <<Acc/binary, "VALUE ", Key/binary, " ", Value/binary, "\r\n">>}
        end,
        {Keys, <<>>}, Values
    ),
    <<Result/binary, "END">>;


handle({add, Key, Value}) ->
    case memcache:add(Key, Value) of
        ok -> <<"STORED">>;
        {error, exists} -> <<"EXISTS">>
    end;


handle({replace, Key, Value}) ->
    case memcache:replace(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle({append, Key, Value}) ->
    case memcache:append(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle({prepend, Key, Value}) ->
    case memcache:prepend(Key, Value) of
        ok -> <<"STORED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle({delete, Key}) ->
    case memcache:delete(Key) of
        ok -> <<"DELETED">>;
        {error, not_found} -> <<"NOT FOUND">>
    end;


handle(unknown) ->
    <<"UNKNOWN REQUEST">>.
