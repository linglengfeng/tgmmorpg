-module(db_role).

-compile(export_all).

-include("common.hrl").

init(DMap) ->
  maps:merge(DMap, role_data:init_data()).

create(AccountId, Gene, Name) ->
  CreateTime = date:unixtime(),
  Status = 0,
  Info = #{},
  DMap = #{server_id => ?SERVERID, account_id => AccountId, name => Name, create_time => CreateTime, status => Status, info => Info, gene => Gene},
  Data = init(DMap),
  case repeat_name(Name) orelse 
    db_mysql:insert("insert into role (server_id, account, status, name, gene, create_time, data, info) values (?, ?, ?, ?, ?, ?, ?, ?)",
      [?SERVERID, AccountId, Status, Name, Gene, CreateTime, utils:term_to_string(Data), utils:term_to_string(Info)])
  of
    {ok, Id} -> 
      NewData = Data#{id => Id},
      case save(NewData) of
        ok -> {ok, Id, NewData};
        _Err -> {err, _Err}
      end;
    {error, Err} -> {err, Err};
    true -> {err, repeat_name};
    _Err -> {err, _Err}
  end.

load(Id) ->
  case db_mysql:get_row("select data from role where id = ?", [Id]) of
    {ok, undefined} -> {err, not_found};
    {ok, [Data]} -> {ok, element(2, utils:string_to_term(Data))};
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end.

repeat_name(Name) ->
  case db_mysql:get_one("select count(*) from role where name = ?", [Name]) of
    {ok, Num} when Num > 0 -> true;
    _ -> false
  end.

role_list(Account) ->
  case db_mysql:get_all("select id, server_id, status, name, gene from role where account = ?", [Account]) of
    {ok, L} -> [[Id, ServerId, Status, element(2, binary_to_list(Name)), Gene] || [Id, ServerId, Status, Name, Gene] <- L];
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end.

save(Data = #{id := Id, server_id := _ServerId, account_id := _Account, status := Status, name := Name, gene := Gene, create_time := _CreateTime, info := Info}) ->
  NewData = utils:map_delete(Data, info),
  case db_mysql:exec("replace into role (id, status, name, gene, data, info) values (?, ?, ?, ?, ?, ?)", 
    [Id, Status, Name, Gene, utils:term_to_string(NewData), utils:term_to_string(Info)]) 
  of
    ok -> ok;
    {error, Error} -> {err, Error};
    _Err -> {err, _Err}
  end;
save(_) ->
  {err, not_match}.
