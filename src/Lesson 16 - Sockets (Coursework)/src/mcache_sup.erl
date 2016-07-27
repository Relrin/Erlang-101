-module(mcache_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification =  #{
      strategy => one_for_one,
      intensity => 10,
      period => 1000
    },

    ChildSpecifications = [
      % Custom memcache server will be storing key/value data
      #{id => memcache,
          start => {memcache, start_link, []},
          restart => permanent,
          shutdown => 3000,
          type => worker,
          modules => [memcache]},
      % And this server run few threads for processing client requests
      #{id => mcache_worker,
          start => {mcache_worker, start_link, []},
          restart => permanent,
          shutdown => 3000,
          type => worker,
          modules => [mcache_worker]}
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
