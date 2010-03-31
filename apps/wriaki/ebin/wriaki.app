{application, wriaki,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             wriaki_app,
             wriaki_sup
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wriaki_app, []}},
  {env, []}
 ]}.
