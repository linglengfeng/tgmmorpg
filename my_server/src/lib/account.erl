% 注意：此模块运行在连接进程，只有以下三种返回，Msg,Changed格式与role层一样
% {notify, Msg, Changed} -> {reply, Msg, maps:merge(State, Changed)};
% {notify, Msg} -> {reply, Msg, State};
% _ -> ok
-module(account).

-compile(export_all).

role_list(Account, State) ->
  ok.