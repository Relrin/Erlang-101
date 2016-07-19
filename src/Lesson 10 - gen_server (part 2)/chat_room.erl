-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, remove_user/2, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-type(username() :: binary()).
-type(chat_user() :: {username(), pid()}).
-type(message() :: {username(), binary()}).


-record (state, {
    users = maps:new(),
    user_pids = maps:new(),
    history = [] :: [message()]
}).


% Public API

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec add_user(pid(), username(), pid()) -> ok.
add_user(Pid, User, User_pid) ->
    gen_server:cast(Pid, {add_user, User, User_pid}),
    ok.


-spec remove_user(pid(), pid()) -> ok | {error, user_not_found}.
remove_user(Pid, User_pid) ->
    gen_server:call(Pid, {remove_user, User_pid}).


-spec get_users(pid()) -> [chat_user()].
get_users(Pid) ->
    gen_server:call(Pid, get_users).


-spec add_message(pid(), username(), message()) -> ok.
add_message(Pid, Author, Message) ->
    gen_server:cast(Pid, {add_message, Author, Message}),
    ok.


-spec get_history(pid()) -> [message()].
get_history(Pid) ->
    gen_server:call(Pid, get_history).


% gen_server API

init([]) ->
    {ok, #state{}}.


handle_call({remove_user, User_pid}, _From, #state{users = Users, user_pids = User_pids} = State) ->
    case maps:find(User_pid, User_pids) of
        {ok, User} ->
            Updated_users = maps:remove(User, Users),
            Updated_user_pids = maps:remove(User_pid, User_pids),
            {reply, ok, State#state{users = Updated_users, user_pids = Updated_user_pids}};
        error -> {reply, {error, user_not_found}, State}
    end;


handle_call(get_users, _From, #state{users = Users} = State) ->
    {reply, maps:to_list(Users), State};


handle_call(get_history, _From, #state{history = History} = State) ->
    Reply = lists:reverse(History),
    {reply, Reply, State}.


handle_cast({add_user, User, User_pid}, #state{users = Users, user_pids = User_pids} = State) ->
    Updated_users = maps:put(User, User_pid, Users),
    Updated_user_pids = maps:put(User_pid, User, User_pids),
    {noreply, State#state{users = Updated_users, user_pids = Updated_user_pids}};


handle_cast({add_message, Author, Message}, #state{user_pids = User_pids, history = History} = State) ->
    Updated_history = [{Author, Message} | History],
    lists:foreach(
        fun(User_pid) -> chat_user:add_message(User_pid, Author, Message) end,
        maps:keys(User_pids)
    ),
    {noreply, State#state{history = Updated_history}}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
