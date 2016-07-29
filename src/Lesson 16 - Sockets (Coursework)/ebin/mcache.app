{application, mcache, [
    {description, "Custom memcache implementation with Telnet support"},
    {vsn, "0.1.0"},
    {modules, [mcache_app, mcache_sup, mcache_worker, handlers, memcache]},
    {registered, [mcache_sup, mcache_worker, memcache]},
    {applications, [kernel, stdlib]},
    {mod, {mcache_app, []}},
    {env, [
        {port, 1234},
        {pool_size, 10}
    ]}
]}.
