-module(my_crypt).
-behaviour(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record (state, {
    crypto_key :: binary(),
    hash_table :: [byte()]
}).


% Public API


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec encode(binary()) -> binary().
encode(Data) ->
    Key = extend_key(byte_size(Data), get_key()),
    Data_bytes = binary_to_list(Data),
    Key_bytes = binary_to_list(Key),
    encode(Data_bytes, Key_bytes, []).


-spec get_key() -> binary().
get_key() ->
    gen_server:call(?MODULE, get_key).


-spec set_key(binary()) -> ok.
set_key(Key) ->
    gen_server:call(?MODULE, {set_key, Key}).


-spec hash(binary()) -> binary().
hash(Data) ->
    gen_server:call(?MODULE, {hash, Data}).


% gen_server API


init([]) ->
    {ok, Hash_seed} = application:get_env(my_crypt, hash_seed),
    random:seed(Hash_seed),
    Hash_table = init_hash_table(),
    {ok, Key} = application:get_env(my_crypt, crypto_key),
    {ok, #state{crypto_key = Key, hash_table = Hash_table}}.


% get_key set_key, hash
handle_call(get_key, _From, #state{crypto_key = Key} = State) ->
    {reply, Key, State};


handle_call({set_key, New_key}, _From, State) ->
    {reply, ok, State#state{crypto_key = New_key}};


handle_call({hash, Data}, _From, #state{hash_table = Hash_table} = State) ->
    {reply, pearson_hash(Data, Hash_table), State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Private API

extend_key(Size, Key) when byte_size(Key) >= Size -> Key;
extend_key(Size, Key) -> extend_key(Size, <<Key/binary, Key/binary>>).


encode([], _, Res) -> list_to_binary(lists:reverse(Res));
encode([Data_byte | Rest_data], [Key_byte | Rest_key], Res) ->
    encode(Rest_data, Rest_key, [Data_byte bxor Key_byte | Res]).


init_hash_table() ->
    Range_list = lists:seq(0, 255),
    shuffle_list(Range_list).


shuffle_list(List) ->
    Random_tuples = [{random:uniform(), Number} || Number <- List],
    [Number || {_, Number} <- lists:sort(Random_tuples)].


pearson_hash(Data, Hash_table) ->
    {ok, Hash_size} = application:get_env(my_crypt, hash_size),
    Data_bytes = binary_to_list(Data),
    Digest = lists:map(
        fun(Number) ->
            Hash = substitute_byte(Number, Data_bytes, Hash_table),
            convert_to_binary(Hash)
        end,
        lists:seq(1, Hash_size div 2)  % digest bytes from 1 to hash_size / 2
    ),
    unicode:characters_to_binary(Digest).


substitute_byte(Digest_byte, Data_bytes, Hash_table) ->
    lists:foldl(
        fun(Byte, Hash) ->
            Index = Hash bxor Byte,
            lists:nth(Index + 1, Hash_table)
        end,
        Digest_byte, Data_bytes
    ).


convert_to_binary(Number) ->
    case Number < 16 of
        true -> <<"0", (integer_to_binary(Number, 16))/binary>>;
        false -> integer_to_binary(Number, 16)
    end.
