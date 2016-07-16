-module(map_reduce).

-export([start/1]).
-export([reduce/2, map/2]).


reduce(Iterations, Sender) ->
    Data = process_result(Iterations, maps:new()),
    Sender ! {ok, Data},
    ok.


process_result(Iterations, Map) when Iterations == 0 -> Map;
process_result(Iterations, Map) ->
    receive
        {ok, Data} -> process_result(Iterations - 1, merge(Data, Map));
        invalid_file -> process_result(Iterations - 1, Map)
    end.


merge(Data, Map) ->
    lists:foldl(
        fun({Word, Count}, Acc) ->
                case maps:find(Word, Acc) of
                    {ok, Current_count} ->
                        maps:put(Word, Count + Current_count, Acc);
                    error -> maps:put(Word, Count, Acc)
                end
        end,
        Map, maps:to_list(Data)
    ).


map(Filename, Result_thread_id) ->
    Data = case file:read_file(Filename) of
        {ok, Text} -> {ok, words_count(Text)};
        {error, _} -> invalid_file
    end,
    Result_thread_id ! Data,
    ok.


words_count(Text) ->
    Words = lists:map(
        fun unicode:characters_to_list/1,
        binary:split(Text, [<<" ">>, <<"\n">>, <<"\r">>],
        [global, trim, trim_all])
    ),
    lists:foldl(
        fun (Word, Map) ->
            case maps:find(Word, Map) of
                {ok, Count} -> maps:put(Word, Count + 1, Map);
                error -> maps:put(Word, 1, Map)
            end
        end,
        maps:new(), Words
    ).


start(Files) ->
    Map_threads = length(Files),
    Reduce_thread_id = spawn(map_reduce, reduce, [Map_threads, self()]),
    [spawn(map_reduce, map, [File, Reduce_thread_id]) || File <- Files],

    receive
        {ok, Data} -> Data
    after 5000 -> timeout
    end.
