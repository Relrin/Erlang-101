-module(url_parser).

-export([parse/1]).


-spec parse(binary()) -> {ok, map()} | {error, term()}.
parse(URL) ->
    try
        Parser_functions = [
            fun get_protocol/2,
            fun get_domain/2,
            fun get_query/2,
            fun get_path/2,
            fun get_date/2
        ],
        {_, Parse_result} = lists:foldl(
            fun(Func, {Data, State}) -> Func(Data, State) end,
            {URL, maps:new()}, Parser_functions
        ),
        {ok, Parse_result}
    catch
        throw:invalid_protocol -> {error, invalid_protocol};
        throw:invalid_domain -> {error, invalid_domain}
    end.


get_protocol(URL, State) ->
    case binary:split(URL, <<"://">>) of
        [Protocol, Rest] -> {Rest, State#{protocol => Protocol}};
        _ -> throw(invalid_protocol)
    end.


get_domain(URL, State) ->
    case binary:split(URL, <<"/">>) of
        [<<>>] -> throw(invalid_domain);
        [Domain] -> {<<>>, State#{domain => Domain}};
        [Domain, Rest] -> {Rest, State#{domain => Domain}}
    end.


get_path(URL, State) ->
    Path = lists:filter(
        fun(String) -> String =/= <<>> end,
        binary:split(URL, <<"/">>, [global])
    ),
    {URL, State#{path => Path}}.


get_date(URL, State) ->
    Date = case binary:split(URL, <<"/">>, [global]) of
        [Year, Month, Day | _] -> string_to_date([Year, Month, Day]);
        _ -> undefined
    end,
    {URL, State#{date => Date}}.


string_to_date(String_date) ->
    try
        Date = lists:map(
            fun(Binary) ->
                {Number, _} = string:to_integer(binary_to_list(Binary)),
                Number
            end,
            String_date
        ),
        validate_date(Date)
    catch
        throw:invalid_date -> undefined;
        error:bad_argument -> undefined
    end.


validate_date([Y, M, D]) when 1 =< D, D =< 31, 1 =< M, M =< 12 -> {Y, M, D};
validate_date(_) -> throw(invalid_date).


get_query(URL, State) ->
    case binary:split(URL, <<"?">>) of
        [Rest, Query] -> {Rest, State#{query => Query}};
        _ -> {URL, State#{query => <<>>}}
    end.
