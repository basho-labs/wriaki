{application, riak_erlang_client,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             riak_erlang_client_app,
             riak_erlang_client_sup,
             rhc,
             rec_obj
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { riak_erlang_client_app, []}},
  {env, []}
 ]}.
