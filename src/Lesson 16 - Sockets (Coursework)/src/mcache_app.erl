-module(mcache_app).
-behaviour(application).

-include("../include/tables.hrl").

-export([install_mnesia/1]).
-export([start/0, start/2, stop/1]).


install_mnesia(Nodes) ->
    mnesia:create_schema(Nodes),
    application:start(mnesia),
    mnesia:create_table(memcache_storage,
        [{attributes, record_info(fields, memcache_storage)},
        {index, [#memcache_storage.key]},
        {disc_copies, Nodes},
        {type, bag}]
    ),
    mnesia:info(),
    io:format("Mnesia nodes succesfully started... ~n"),
    io:format("----------------------------------- ~n").


start() ->
    install_mnesia([node()]),
    application:start(mcache),
    ok.


start(_StartType, _StartArgs) ->
    mcache_sup:start_link().


stop(_State) ->
    application:stop(mnesia),
    ok.
