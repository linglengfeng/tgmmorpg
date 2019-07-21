-module(role_data).

-compile(export_all).

-include("common.hrl").

%  load data from db
load_data(Id) ->
  case db_role:load(Id) of
    {ok, Data} -> Data;
    _Err -> {err, _Err}
  end.

% 拆分下client只需要的数据
client_data(Data) ->
  Data.

% 创建账号初始数据
init_data() ->
  #{}.
