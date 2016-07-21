-module(main).

-export([parse/1]).


parse(Files) ->
    Workers = lists:map(
        fun(Filename) ->
            Pid = spawn(worker, parse_file, [Filename, self()]),
            Reference = erlang:monitor(process, Pid),
            {Reference, Filename}
        end,
        Files
    ),
    process_results(Workers, maps:new(), maps:new()).


process_results([], Data, Errors) -> {Data, Errors};
process_results(Workers, Data, Errors) ->
    receive
        {'DOWN', Reference, process, _Pid, normal} ->
            Updated_workers = clear_worker_list(Reference, Workers),
            process_results(Updated_workers, Data, Errors);
        {'DOWN', Reference, process, _Pid, Reason} ->
            Updated_errors = handle_an_error(Reference, Reason, Workers, Errors),
            Updated_workers = clear_worker_list(Reference, Workers),
            process_results(Updated_workers, Data, Updated_errors);
        {ok, Products} ->
            Data2 = merge_data(Products, Data),
            process_results(Workers, Data2, Errors)
    after 3000 -> done
    end.


clear_worker_list(Reference, Workers) ->
    lists:keydelete(Reference, 1, Workers).


handle_an_error(Reference, Reason, Workers, Errors) ->
    {_, Filename} = lists:keyfind(Reference, 1, Workers),
    maps:put(Filename, Reason, Errors).


merge_data(Products, Data) ->
    lists:foldl(
        fun({Product, Quantity}, Map) ->
            case maps:find(Product, Map) of
                {ok, Value} -> maps:put(Product, Quantity + Value, Map);
                error -> maps:put(Product, Quantity, Map)
            end
        end,
        Data, Products
    ).
