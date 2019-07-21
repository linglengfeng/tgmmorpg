% 公共头文件

-ifdef(debug).
-define(DEBUG(Str), io:format(Str)).
-define(DEBUG(Str, Args), io:format(Str, Args)).
-else.
-define(DEBUG(Str), void).
-define(DEBUG(Str, Args), void).
-endif.

% -ifdef(debug).
% -define(INFO(Msg), lager:debug(Msg)).
% -define(INFO(F, Msg), lager:debug(io_lib:format(F, Msg))).
% -else.
% -define(INFO(Msg), ok).
% -define(INFO(F, A), ok).
% -endif.


% 三目运算
-define(ITT(A, B, C), case A of true -> B; _ -> C end).

-define(ALLENV, application:get_env(outside_service, env)).

-define(ENV(K), maps:get(K, element(2, ?ALLENV), null)).

-define(SERVERID, ?ENV(server_id)).

-define(APPNAME, ?ENV(app_name)).

-define(DBMYSQL, mysql_conn_pool).