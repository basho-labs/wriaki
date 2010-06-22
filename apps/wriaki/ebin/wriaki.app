{application, wriaki,
 [
  {description, "Wriaki, the Riak-based Wiki"},
  {vsn, "1"},
  {modules, [
             wriaki_app,
             wriaki_sup,
             wriaki,
             wriaki_auth,
             diff,

             wrc,
             wobj,

             wiki_resource,
             static_resource,
             redirect_resource,
             session_resource,
             user_resource,
             login_form_resource,

             article,
             article_history,
             session,
             wuser,

             wrq_dtl_helper,
             article_dtl_helper,
             wuser_dtl_helper,

             account_detail_form_dtl,
             action_line_dtl,
             article_diff_dtl,
             article_dtl,
             article_editor_dtl,
             article_history_dtl,
             base_dtl,
             error_404_dtl,
             login_form_dtl,
             user_404_dtl,
             user_dtl
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  riak_erlang_client,
                  webmachine,
                  erlydtl,
                  wiki_creole
                 ]},
  {mod, { wriaki_app, []}},
  {env, []}
 ]}.
