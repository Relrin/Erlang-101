-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-type(username() :: binary()).
-type(message() :: {username(), binary()}).

-record (state, {messages = [] :: [message()]}).


% Public API

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec add_message(pid(), username(), binary()) -> ok.
add_message(Pid, User, Message) ->
    gen_server:cast(Pid, {add_message, User, Message}),
    ok.


-spec get_messages(pid()) -> [message()].
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).


% gen_server API

init([]) ->
    {ok, #state{}}.


handle_call(get_messages, _From, #state{messages = Messages} = State) ->
    Reply = lists:reverse(Messages),
    {reply, Reply, State}.


handle_cast({add_message, User, Message}, #state{messages = Messages} = State) ->
    {noreply, State#state{messages = [{User, Message} | Messages]}}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, _State, _Extra) ->
    {ok, _State}.
