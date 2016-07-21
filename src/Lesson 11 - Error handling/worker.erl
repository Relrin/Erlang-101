-module(worker).

-export([parse_file/2]).


parse_file(Filename, Result_pid) ->
    {ok, Data} = file:read_file(Filename),
    Lines = binary:split(Data, <<"\n">>, [global]),
    Products = lists:filtermap(fun parse_line/1, Lines),
    Result_pid ! {ok, Products},
    ok.


parse_line(<<>>) -> false;
parse_line(Line) ->
    [_, Item, Quantity, _] = binary:split(Line, <<",">>, [global]),
    {true, {Item, list_to_integer(binary_to_list(Quantity))}}.
