-module(db_role).

-compile(export_all).

-include("common.hrl").

init(DMap) ->
  maps:merge(DMap, role_data:init_data()).

-spec create(integer(), integer(), string()) -> term().
create(AccountId, Gene, Name) ->
  CreateTime = date:unixtime(),
  Status = 0,
  Info = #{},
  DMap = #{server_id => ?SERVERID, account_id => AccountId, name => Name, create_time => CreateTime, status => Status, info => Info, gene => Gene},
  Data = init(DMap),
  case application:get_env(use_db) of
    {ok, mysql} -> create_mysql(AccountId, Gene, Name, Data);
    {ok, mongo} -> create_mongo(AccountId, Gene, Name, Data);
    _ -> create_mongo(AccountId, Gene, Name, Data)
  end.

-spec load(integer()) -> term().
load(Id) ->
  case application:get_env(use_db) of
    {ok, mysql} -> load_mysql(Id);
    {ok, mongo} -> load_mongo(Id);
    _ -> load_mongo(Id)
  end.

-spec role_list(integer()) -> term().
role_list(Account) ->
  case application:get_env(use_db) of
    {ok, mysql} -> role_list_mysql(Account);
    {ok, mongo} -> role_list_mongo(Account);
    _ -> role_list_mongo(Account)
  end.

-spec save(map()) -> term().
save(Data) ->
  case application:get_env(use_db) of
    {ok, mysql} -> save_mysql(Data);
    {ok, mongo} -> save_mongo(Data);
    _ -> save_mongo(Data)
  end.

% ------ mysql ------
create_mysql(AccountId, Gene, Name, Data = #{status := Status, name := Name, gene := Gene, create_time := CreateTime, info := Info}) ->
  case repeat_name_mysql(Name) orelse 
    db_mysql:insert("insert into role (server_id, account, status, name, gene, create_time, data, info) values (?, ?, ?, ?, ?, ?, ?, ?)",
      [?SERVERID, AccountId, Status, Name, Gene, CreateTime, utils:term_to_string(Data), utils:term_to_string(Info)])
  of
    {ok, Id} -> 
      NewData = Data#{id => Id},
      case save_mysql(NewData) of
        ok -> {ok, Id, NewData};
        _Err -> {err, _Err}
      end;
    {error, Err} -> {err, Err};
    true -> {err, repeat_name};
    _Err -> {err, _Err}
  end.

load_mysql(Id) ->
  case db_mysql:get_row("select data from role where id = ?", [Id]) of
    {ok, undefined} -> {err, not_found};
    {ok, [Data]} -> {ok, element(2, utils:string_to_term(Data))};
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end.

repeat_name_mysql(Name) ->
  case db_mysql:get_one("select count(*) from role where name = ?", [Name]) of
    {ok, Num} when Num > 0 -> true;
    _ -> false
  end.

role_list_mysql(Account) ->
  case db_mysql:get_all("select id, server_id, status, name, gene from role where account = ?", [Account]) of
    {ok, L} -> [[Id, ServerId, Status, element(2, binary_to_list(Name)), Gene] || [Id, ServerId, Status, Name, Gene] <- L];
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end.

save_mysql(Data = #{id := Id, server_id := _ServerId, account_id := _Account, status := Status, name := Name, gene := Gene, create_time := _CreateTime, info := Info}) ->
  NewData = utils:map_delete(Data, info),
  case db_mysql:exec("replace into role (id, status, name, gene, data, info) values (?, ?, ?, ?, ?, ?)", 
    [Id, Status, Name, Gene, utils:term_to_string(NewData), utils:term_to_string(Info)]) 
  of
    ok -> ok;
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end;
save_mysql(_) ->
  {err, not_match}.

% ------ mongo ------
-define(mongo_collection, "role_test").
% todo: 测试新写的以下mongodb函数
create_mongo(_AccountId, _Gene, _Name, Data) ->
  Id = guid:new(role, mongo_count() + 1),
  NewData = mongo_agent:key_to_mongo_key(Data#{id => Id}),
  mongo_agent:insert_one(?mongo_collection, NewData#{<<"_id">> => Id}),
  {ok, Id, NewData}.

load_mongo(Id) ->
  {err, Id}.

role_list_mongo(Account) ->
  {err, Account}.

repeat_name_mongo(_Name) ->
  false.

save_mongo(_Data) ->
  {err, not_match}.


mongo_count() ->
  mongo_agent:count(?mongo_collection, #{}).

%% mongo_agent:find("role_test", [#{}]).

% case repeat_name_mysql(Name) orelse 
  %   db_mysql:insert("insert into role (server_id, account, status, name, gene, create_time, data, info) values (?, ?, ?, ?, ?, ?, ?, ?)",
  %     [?SERVERID, AccountId, Status, Name, Gene, CreateTime, utils:term_to_string(Data), utils:term_to_string(Info)])
  % of
  %   {ok, Id} -> 
  %     NewData = Data#{id => Id},
  %     case save_mysql(NewData) of
  %       ok -> {ok, Id, NewData};
  %       _Err -> {err, _Err}
  %     end;
  %   {error, Err} -> {err, Err};
  %   true -> {err, repeat_name};
  %   _Err -> {err, _Err}
  % end.

