-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/3, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(username() :: binary()).
-type(room_name() :: binary()).
-type(room() :: {room_name(), pid()}).
-type(message() :: {username(), binary()}).
-type(chat_user() :: {username(), pid()}).

-record (state, {rooms :: map()}).


% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec create_room(room_name()) -> room().
create_room(Room_name) ->
    {ok, Room_pid} = chat_room:start_link(),
    Room = {Room_name, Room_pid},
    gen_server:cast(?MODULE, {create_room, Room}),
    Room.


-spec get_rooms() -> [room()].
get_rooms() ->
    gen_server:call(?MODULE, get_rooms).


-spec add_user(pid(), username(), pid()) -> ok | {error, room_not_found}.
add_user(Room_pid, User, User_pid) ->
    gen_server:call(?MODULE, {add_user, Room_pid, User, User_pid}).


-spec remove_user(pid(), pid()) -> ok | {error, room_not_found} | {error, user_not_found}.
remove_user(Room_pid, User_pid) ->
    gen_server:call(?MODULE, {remove_user, Room_pid, User_pid}).


-spec get_users(pid()) -> {ok, [chat_user()]} | {error, room_not_found}.
get_users(Room_pid) ->
    gen_server:call(?MODULE, {get_users, Room_pid}).


-spec send_message(pid(), username(), binary()) -> ok | {error, room_not_found}.
send_message(Room_pid, Author, Message) ->
    gen_server:call(?MODULE, {send_message, Room_pid, Author, Message}).


-spec get_history(pid()) -> {ok, [message()]} | {error, room_not_found}.
get_history(Room_pid) ->
    gen_server:call(?MODULE, {get_history, Room_pid}).


% gen_server API

init([]) ->
    {ok, #state{rooms = maps:new()}}.


handle_call(get_rooms, _From, #state{rooms = Rooms} = State) ->
    Room_list = lists:map(
        fun({Room_pid, Room_name}) -> {Room_name, Room_pid} end,
        maps:to_list(Rooms)
    ),
    {reply, Room_list, State};


handle_call({add_user, Room_pid, User, User_pid}, _From, #state{rooms = Rooms} = State) ->
    Reply = case maps:find(Room_pid, Rooms) of
        {ok, _} ->chat_room:add_user(Room_pid, User, User_pid);
        error -> {error, room_not_found}
    end,
    {reply, Reply, State};


handle_call({remove_user, Room_pid, User_pid}, _From, #state{rooms = Rooms} = State) ->
    Reply = case maps:find(Room_pid, Rooms) of
        {ok, _} -> chat_room:remove_user(Room_pid, User_pid);
        error -> {error, room_not_found}
    end,
    {reply, Reply, State};


handle_call({get_users, Room_pid}, _From , #state{rooms = Rooms} = State) ->
    Reply = case maps:find(Room_pid, Rooms) of
        {ok, _} -> {ok, chat_room:get_users(Room_pid)};
        error -> {error, room_not_found}
    end,
    {reply, Reply, State};


handle_call({send_message, Room_pid, Author, Message}, _From, #state{rooms = Rooms} = State) ->
    Reply = case maps:find(Room_pid, Rooms) of
        {ok, _} -> chat_room:add_message(Room_pid, Author, Message);
        error -> {error, room_not_found}
    end,
    {reply, Reply, State};


handle_call({get_history, Room_pid}, _From, #state{rooms = Rooms} = State) ->
    Reply = case maps:find(Room_pid, Rooms) of
        {ok, _} -> {ok, chat_room:get_history(Room_pid)};
        error -> {error, room_not_found}
    end,
    {reply, Reply, State}.


handle_cast({create_room, {Room_name, Room_pid}}, #state{rooms = Rooms} = State) ->
    Updated_rooms = maps:put(Room_pid, Room_name, Rooms),
    {noreply, State#state{rooms = Updated_rooms}}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, _Extra, State) ->
    {ok, State}.
