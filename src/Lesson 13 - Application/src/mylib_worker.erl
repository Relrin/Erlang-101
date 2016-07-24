-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_version() ->
    gen_server:call(?MODULE, get_version).


get_modules() ->
    gen_server:call(?MODULE, get_modules).


get_min_val() ->
    gen_server:call(?MODULE, get_min_val).


get_connection_timeout() ->
    gen_server:call(?MODULE, get_connection_timeout).


all_apps() ->
    gen_server:call(?MODULE, get_all_apps).


% gen_server API


init([]) ->
    {ok, no_state}.


handle_call(get_version, _From, State) ->
    {ok, Version} = application:get_key(mylib, vsn),
    {reply, Version, State};


handle_call(get_modules, _From, State) ->
    {ok, Modules} = application:get_key(mylib, modules),
    {reply, Modules, State};


handle_call(get_min_val, _From, State) ->
    {ok, Min_value} = application:get_env(mylib, min_val),
    {reply, Min_value, State};


handle_call(get_connection_timeout, _From, State) ->
    {ok, Connection_timeout} = application:get_env(mylib, connection_timeout),
    {reply, Connection_timeout, State};


handle_call(get_all_apps, _From, State) ->
    Running_applications = lists:foldl(
        fun({Application, Description, Version}, Map) ->
            Map#{Application => #{
                        description => Description,
                        version => Version
                    }
                }
        end,
        maps:new(), application:which_applications()
    ),
    {reply, Running_applications, State}.


handle_cast(_Request, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
