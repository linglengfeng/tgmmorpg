-module(my_server_app).

-behaviour(application).

-export([
  start/2,
  stop/1,
  start/0,
  stop/0,
  restart/0,
  server/0,
  services/0,
  ets_init/0,
  start_child/3,
  update_env/0
]).

-include("common.hrl").

-define(APPSUP, my_server_sup).

% rebar3 shell 自动调用
start(_StartType, _StartArgs) ->
  update_env(),
  {ok, Opts} = application:get_env(outside_service, ranch),
  ranch:start_listener(my_server_app, ranch_tcp, Opts, session, []),
  {ok, Pid} = ?APPSUP:start_link(),
  services(),
  server(),
  ?DEBUG("<=== my_server_app start success ===>\n all_env:~w\n\n", [{application:get_all_env(), Pid}]),
  {ok, Pid}.

stop(_State) -> 
  ?DEBUG("<=== my_server_app stop: ===>\n ~w\n\n", [_State]),
  ok.

%% 手动调用
start() ->
  application:start(?APPNAME).

stop() ->
  % 此处还要进行写其他操作，比如踢出所有玩家
  application:stop(?APPNAME).

restart() ->
  ?DEBUG("<=== my_server_app restart: ===>\n\n"),
  stop(),
  start().

services() ->
  case application:get_env(use_db) of
    {ok, mysql} -> 
      ?DEBUG("use db: mysql\n"),
      db_mysql:init();
    {ok, mongo} -> 
      ?DEBUG("use db: mongo\n"),
      start_child(mongo_sup, 5000, supervisor);
    _ -> 
      ?DEBUG("use db: mongo\n"),
      start_child(mongo_sup, 5000, supervisor)
  end,

  ok.

server() ->
  % reloader 代码自动载入
  ?ITT({ok, true} =:= application:get_env(reloader), start_child(reloader, 5000, worker), ok),

  ?DEBUG("all child start success\n"),
  % all ets init
  ets_init(),
  ok.

ets_init() ->
  login_pool:init().

start_child(Module, Shutdown, Type) ->
  SupRef = ?APPSUP,
  ChildSpec = #{id => Module,
                  start => {Module, start_link, []},
                  restart => permanent,
                  shutdown => Shutdown,
                  type => Type,
                  modules => [Module]},
  supervisor:start_child(SupRef, ChildSpec).

% move sys.config env to app env
update_env() ->
  maps:map(fun(K, V) -> application:set_env(?APPNAME, K, V) end, element(2, ?ALLENV)).



