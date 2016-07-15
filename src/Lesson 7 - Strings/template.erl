-module(template).

-export([parse/2]).


insert_value(Template_key, Map, String) ->
    case maps:find(Template_key, Map) of
        {ok, Value} when is_integer(Value) ->
            [integer_to_binary(Value), String];
        {ok, Value} -> [Value, String];
        error -> String  % skip undefined template key
    end.


parse(Str, Data) when is_binary(Str) ->
    Splitted_string = binary:split(Str, [<<"{{">>], [global]),

    Fixed_string = lists:map(
        fun(String) ->
            case binary:split(String, [<<"}}">>]) of
                [Text] -> Text;
                [Template_Key | Rest] -> insert_value(Template_Key, Data, Rest)
            end
        end, Splitted_string),
    unicode:characters_to_binary(Fixed_string).
