-module(sup_2).

-export([start_link/0, init/1, add_worker/1, remove_worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification =  #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecifications = [
        #{id => gen_server_worker_3,
           start => {worker, start_link, [3]},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [worker]},
        #{id => gen_server_worker_4,
           start => {worker, start_link, [4]},
           restart => permanent,
           shutdown => 3000,
           type => worker,
           modules => [worker]}
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.


add_worker(WorkerId) ->
    WorkerSpec = #{
        id => WorkerId,
        start => {worker, start_link, [WorkerId]},
        restart => permanent,
        shutdown => 3000,
        type => worker,
        modules => [worker]
    },
    supervisor:start_child(?MODULE, WorkerSpec).


remove_worker(WorkerId) ->
    supervisor:terminate_child(?MODULE, WorkerId),
    supervisor:delete_child(?MODULE, WorkerId).
