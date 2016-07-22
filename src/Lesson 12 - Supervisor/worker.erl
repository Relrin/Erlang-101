-module(worker).
-behavior(gen_server).

-export([start_link/1, ping/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type (ping_result() :: {pid(), pid()}).

-record (state, {id :: pid()}).


% Public API

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).


-spec ping(pid()) -> ping_result().
ping(Pid) ->
    gen_server:call(Pid, ping).


% gen_server API

init([Id]) ->
    {ok, #state{id = Id}}.


handle_call(ping, _From, #state{id = Id} = State) ->
    {reply, {Id, self()} , State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, _Extra, State) ->
    {ok, State}.
