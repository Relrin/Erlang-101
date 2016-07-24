{application, my_crypt, [
    {description, "Test crypt and hash library"},
    {vsn, "0.1"},
    {modules, [my_crypt_app, my_crypt_sup, my_crypt]},
    {registered, [my_crypt_sup, my_crypt]},
    {applications, [kernel, stdlib]},
    {mod, {my_crypt_app, []}},
    {env, []}
]}.