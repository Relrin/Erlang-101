-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).
-export([loop/1, handle_call/2]).

-type(server() :: pid()).
-type(room_id() :: reference()).
-type(name() :: binary()).
-type(message() :: {name(), binary()}).


-record(room, {
        id :: room_id(),
        name :: name(),
        users = [] :: [name()],
        history = [] :: [message()]
}).

-record(state, {
        max_rooms = 5 :: integer(),
        rooms = maps:new() :: map()
}).


% Init gen_server

-spec start() -> server().
start() ->
    spawn(?MODULE, loop, [#state{}]).


% Create rooms

-spec create_room(server(), name()) -> {ok, room_id()} | {error, term()}.
create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


% Remove rooms

-spec remove_room(server(), room_id()) -> ok | {error, term()}.
remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


% Get list of created rooms

-spec get_rooms(server()) -> [{room_id(), name()}].
get_rooms(Server) ->
    call(Server, get_rooms).


% Add new user to the specified room

-spec add_user(server(), room_id(), name()) -> ok | {error, term()}.
add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


% Remove user from the room

-spec remove_user(server(), room_id(), name()) -> ok | {error, term()}.
remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


% Get list of users of the concrete room

-spec get_users_list(server(), room_id()) -> {ok, [name()]} | {error, term()}.
get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


% Send message to an another user

-spec send_message(server(), room_id(), name(), binary()) -> ok.
send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


% Get all messages which have been sent for the specified room

-spec get_messages_history(server(), room_id()) -> [message()].
get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).


% gen_server functionality (was taken from the theory block)

call(Server, Msg) ->
    MRef = erlang:monitor(process, Server),
    Server ! {Msg, self(), MRef},
    receive
        {reply, MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(MRef, [flush]),
            no_reply
    end.


loop(State) ->
    receive
        {Msg, From, Ref} ->
            {Reply, State2} = ?MODULE:handle_call(Msg, State),
            From ! {reply, Ref, Reply},
            ?MODULE:loop(State2);
        stop -> ok;
        _Any -> ?MODULE:loop(State)
    end.


% Handlers for each command
% Create rooms

handle_call({create_room, RoomName}, #state{rooms = Rooms, max_rooms = MaxRooms} = State) ->
    case maps:size(Rooms) of
        Limit when Limit >= MaxRooms -> {{error, room_limit}, State};
        _ ->
            New_room_id = make_ref(),
            Updated_rooms = maps:put(
                New_room_id,
                #room{id = New_room_id, name = RoomName},
                Rooms
            ),
            {{ok, New_room_id}, State#state{rooms = Updated_rooms}}
    end;


% Remove rooms

handle_call({remove_room, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, _} ->
            New_rooms = maps:remove(RoomId, Rooms),
            New_state = State#state{rooms = New_rooms},
            {ok, New_state};
        error -> {{error, room_not_found}, State}
    end;


% Get rooms

handle_call(get_rooms, #state{rooms = Rooms} = State) ->
    Room_list = lists:foldl(
        fun({RoomId, #room{name = RoomName}}, Acc) ->
            [{RoomId, RoomName} | Acc]
        end,
        [], maps:to_list(Rooms)
    ),
    {Room_list, State};


% Add new user to the specified room

handle_call({add_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users} = Room} ->
            Updated_room = Room#room{users = [UserName | Users]},
            Updated_rooms = maps:put(RoomId, Updated_room, Rooms),
            {ok, State#state{rooms = Updated_rooms}};
        error -> {{error, room_not_found}, State}
    end;


% Remove user from the room

handle_call({remove_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users} = Room} ->
            case lists:member(UserName, Users) of
                true ->
                    Updated_users = lists:delete(UserName, Users),
                    Updated_room = Room#room{users = Updated_users},
                    Updated_rooms = maps:put(RoomId, Updated_room, Rooms),
                    {ok, State#state{rooms = Updated_rooms}};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;


% Get list of users of the concrete room

handle_call({get_users_list, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users}} -> {{ok, Users}, State};
        error -> {{error, room_not_found}, State}
    end;


% Send message to an another user

handle_call({send_message, RoomId, UserName, Message}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users, history = History} = Room} ->
            case lists:member(UserName, Users) of
                true ->
                    Updated_history = [{UserName, Message} | History],
                    Updated_room = Room#room{history = Updated_history},
                    Updated_rooms = maps:put(RoomId, Updated_room, Rooms),
                    {ok, State#state{rooms = Updated_rooms}};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;


% Get all messages which have been sent for the specified room

handle_call({get_messages_history, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{history = History}} -> {{ok, History}, State};
        error -> {{error, room_not_found}, State}
    end.
