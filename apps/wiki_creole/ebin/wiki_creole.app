{application, wiki_creole,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             wiki_creole_app,
             wiki_creole_sup,
             creole,
             script_manager,
             script_worker
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wiki_creole_app, []}},
  {env, []}
 ]}.
