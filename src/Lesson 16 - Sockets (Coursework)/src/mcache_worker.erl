-module(mcache_worker).
-behavior(gen_server).

-export([start_link/0, worker_loop/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


worker_loop(ThreadId, ListenSocket) ->
    ok.


% gen_server API

init([]) ->
    {ok, Port} = application:get_env(mcache, port),
    {ok, Pool_size} = application:get_env(mcache, pool_size),
    io:format("Server listening client requests on ~p port ~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, worker_loop, [ThreadId, ListenSocket]) || ThreadId <- lists:seq(1, Pool_size)],
    {ok, ListenSocket}.


handle_call(Request, _From, State) ->
    io:format("[~p] Handler for `call` with Request=~p has not found ~n", [?MODULE, Request]),
    {noreply, State}.


handle_cast(Request, State) ->
    io:format("[~p] Handler for `cast` with Request=~p has not found ~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    io:format("[~p] Handler for `info` with Info=~p has not found ~n", [?MODULE, Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    io:format("[~p] Terminating the server: ~p ~n", [?MODULE, Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
