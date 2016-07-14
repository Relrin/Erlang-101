-module(short_link).

-export([init/0, create_short/2, get_long/2, rand_str/1]).


init() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}),
    {dict:new(), dict:new()}.


create_short(LongLink, State) ->
    {Short_link_storage, Long_link_storage} = State,

    case dict:find(LongLink, Long_link_storage) of
        {ok, URL} -> {URL, State};
        error ->
            URL = string:concat("http://hexlet.io/", rand_str(15)),
            {URL, {dict:store(URL, LongLink, Short_link_storage),
                   dict:store(LongLink, URL, Long_link_storage)}}
    end.


get_long(ShortURL, State) ->
    {Short_link_storage, _} = State,

    case dict:find(ShortURL, Short_link_storage) of
        {ok, LongURL} -> {ok, LongURL};
        error -> {error, not_found}
    end.


%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    lists:map(fun(Char) when Char > 83 -> Char + 13;
                 (Char) when Char > 57 -> Char + 7;
                 (Char) -> Char
              end,
              [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]).
