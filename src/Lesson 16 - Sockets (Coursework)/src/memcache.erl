-module(memcache).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% gen_server API

init([]) ->
    {ok, no_state}.


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
