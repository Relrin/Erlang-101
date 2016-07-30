-module(memcache).
-behavior(gen_server).

-export([start_link/0, set/2, get/1, gets/1, add/2, replace/2, append/2, prepend/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record (state, {storage :: ets:tid()}).


% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec set(binary(), binary()) -> ok.
set(Key, Value) ->
    gen_server:call(?MODULE, {set, Key, Value}).


-spec get(binary()) -> {ok, binary()} | {error, not_found}.
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


-spec gets([binary()]) -> [{ok, binary()} | {error, not_found}].
gets(Keys) ->
    lists:map(fun get/1, Keys).


-spec add(binary(), binary()) -> ok | {error, exists}.
add(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, _} -> {error, exists};
        {error, not_found} -> ?MODULE:set(Key, Value)
    end.


-spec replace(binary(), binary()) -> ok | {error, not_found}.
replace(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, _} -> ?MODULE:set(Key, Value);
        {error, not_found} -> {error, not_found}
    end.


-spec append(binary(), binary()) -> ok | {error, not_found}.
append(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, OldValue} -> ?MODULE:set(Key, <<OldValue/binary, Value/binary>>);
        {error, not_found} -> {error, not_found}
    end.


-spec prepend(binary(), binary()) -> ok | {error, not_found}.
prepend(Key, Value) ->
    case ?MODULE:get(Key) of
        {ok, OldValue} -> ?MODULE:set(Key, <<Value/binary, OldValue/binary>>);
        {error, not_found} -> {error, not_found}
    end.


-spec delete(binary()) -> ok | {error, not_found}.
delete(Key) ->
    case ?MODULE:get(Key) of
        {ok, _} -> gen_server:call(?MODULE, {delete, Key});
        {error, not_found} -> {error, not_found}
    end.


% gen_server API

init([]) ->
    Storage = ets:new(?MODULE, [set, protected, named_table]),
    {ok, #state{storage = Storage}}.


handle_call({set, Key, Value}, _From, #state{storage = Storage} = State) ->
    ets:insert(Storage, {Key, Value}),
    {reply, ok, State};


handle_call({get, Key}, _From, #state{storage = Storage} = State) ->
    Reply = case ets:lookup(Storage, Key) of
        [{Key, Value}] -> {ok, Value};
        [] -> {error, not_found}
    end,
    {reply, Reply, State};


handle_call({delete, Key}, _From, #state{storage = Storage} = State) ->
    ets:delete(Storage, Key),
    {reply, ok, State};


handle_call(Request, _From, State) ->
    io:format("[~p] Handler for `call` with Request=~p has not found ~n", [?MODULE, Request]),
    {reply, {}, State}.


handle_cast(Request, State) ->
    io:format("[~p] Handler for `cast` with Request=~p has not found ~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("[~p] Handler for `info` with Info=~p has not found ~n", [?MODULE, Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    io:format("[~p] Terminating the memcache instance: ~p ~n", [?MODULE, Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
