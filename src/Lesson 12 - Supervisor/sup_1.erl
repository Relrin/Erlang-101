-module(sup_1).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link(?MODULE, []).


init(_Args) ->
    SupervisorSpecification =  #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecifications = [
        #{id => gen_server_worker_1,
           start => {worker, start_link, [1]},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [worker]},
        #{id => gen_server_worker_2,
           start => {worker, start_link, [2]},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [worker]}
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.