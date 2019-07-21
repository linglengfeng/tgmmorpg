% emysql docs: http://eonblast.github.io/Emysql/
% git: https://github.com/inaka/Emysql
% mysql: https://www.runoob.com/mysql/mysql-data-types.html

-module(db_mysql).

-compile(export_all).

-include("common.hrl").
-include_lib("./_build/default/lib/emysql/include/emysql.hrl").
% -include_lib("emysql/include/emysql.hrl").% 可以这样写但vscode识别不了，编译不会报错

% -record(error_packet, { seq_num :: number()
%                       , code :: number()
%                       , status :: binary()
%                       , msg :: [byte()]
%                       }).

% -record(eof_packet, { seq_num :: number()
%                     , status :: undefined | number()
%                     , warning_count :: undefined | number()
%                     }). % extended to mySQL 4.1+ format

% -record(result_packet, { seq_num :: number()
%                        , field_list :: undefined | list()
%                        , rows :: undefined | [list()]
%                        , extra :: undefined | term()
%                        }).

init() ->
  {ok, ServerId} = application:get_env(server_id),
  {ok, Opts} = application:get_env(outside_service, db_mysql),
  % RealPoolId = ?DBMYSQL, % list_to_atom(atom_to_list(PoolId) ++ "_" ++ integer_to_list(ServerId, 10)),
  {database, Database} = lists:keyfind(database, 1, Opts),
  RealDatabase = Database ++ "_" ++ integer_to_list(ServerId, 10),
  RealOpts = lists:keyreplace(database, 1, Opts, {database, RealDatabase}),
  emysql:add_pool(?DBMYSQL, RealOpts),
  check_table(RealDatabase),
  ok.

%% 考虑这些是否写成脚本
check_table(RealDatabase) ->
  % 使用RealDatabase
  exec(list_to_binary("use " ++ RealDatabase)),
  % 创建role表
  CreateTableRole = 
    "
      CREATE TABLE if not exists role(
      id int(11) not null auto_increment,
      server_id int(6) not null default '0' comment '区号',
      account varchar(120) not null default '' comment '帐号名',
      status int(4) not null default '0' comment '账号角色0000状态均正常',
      name varchar(20) not null default '' comment '角色名',
      gene int(4) not null default '0' comment '职业性别0000',
      create_time int(11) not null default '0' comment '创建角色时间戳',
      data mediumblob not null comment '序列化后的角色数据',
      info mediumblob not null comment '序列化后的其他数据',
      primary key(id),
      unique (name)
      )engine=innodb auto_increment=1 charset=utf8 comment='角色数据'
    ",
  exec(utils:to_bitstring(CreateTableRole)),
  ok.

% db_mysql:test1().
test1() ->
  Sql = 
    "
      CREATE TABLE if not exists test_1(
      id int(11) not null auto_increment,
      server_id int(6) not null default '0' comment '区号',
      account varchar(120) not null default '' comment '帐号名',
      status int(4) not null default '0' comment '账号角色0000状态均正常',
      name varchar(20) not null default '' comment '角色名',
      gene int(4) not null default '0' comment '职业性别0000',
      create_time int(11) not null default '0' comment '创建角色时间戳',
      data mediumblob not null comment '序列化后的角色数据',
      info mediumblob not null comment '序列化后的其他数据',
      primary key(id),
      unique (name)
      )engine=innodb auto_increment=1 charset=utf8 comment='角色数据'
    ",
  Result = exec(unicode:characters_to_binary(Sql)),
  ?DEBUG("ccccccccccccccchecktable:~w\n", [Result]),
  ok.

% mysql_conn_pool
% SELECT * FROM information_schema.SCHEMATA where SCHEMA_NAME='tgmmorpg';

% 执行一条数据库语句，注意：不能用这个接口执行查询类的语句
-spec exec(binary()) -> ok | {error, term()} | {ok, term()}.
exec(Sql) ->
  case catch emysql:execute(?DBMYSQL, Sql) of
    #ok_packet{} -> ok;
    #result_packet{rows = Rows} -> {ok, Rows};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 执行一条数据库语句，带参数，注意：不能用这个接口执行查询类的语句
-spec exec(binary(), [term()]) -> ok | {error, term()} | {ok, term()}.
exec(Sql, Args) ->
  case catch emysql:execute(?DBMYSQL, Sql, Args) of
    #ok_packet{} -> ok;
    #result_packet{rows = Rows} -> {ok, Rows};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

insert(Sql, Args) ->
  case catch emysql:execute(?DBMYSQL, Sql, Args) of
    #ok_packet{insert_id = NewId} -> {ok, NewId};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 执行分页查询，返回结果中的所有行
-spec select_limit(binary(), non_neg_integer(), pos_integer()) ->
  {ok, [term()]} | {error, term()}.
select_limit(Sql, Offset, Num) ->
  S = list_to_binary([Sql, <<" limit ">>, integer_to_list(Offset), <<", ">>, integer_to_list(Num)]),
  case catch emysql:execute(?DBMYSQL, S) of
    #result_packet{rows = Rows} -> {ok, Rows};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 执行分页查询(带格式化参数)，返回结果中的所有行
-spec select_limit(binary(), [term()], non_neg_integer(), pos_integer()) ->
  {ok, [term()]} | {error, term()}.
select_limit(Sql, Args, Offset, Num) ->
  S = list_to_binary([Sql, " limit ", integer_to_list(Offset), ", ", integer_to_list(Num)]),
  case catch emysql:execute(?DBMYSQL, S, Args) of
    #result_packet{rows = Rows} -> {ok, Rows};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的第一行第一列(不带格式化参数)
% 注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
-spec get_one(binary()) -> {ok, term()} | {error, term()}.
get_one(Sql) ->
  case catch emysql:execute(?DBMYSQL, Sql) of
    #result_packet{rows = []} -> {ok, undefined};
    #result_packet{rows = [[R]]} -> {ok, R};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的第一行第一列(带有格式化参数)
% 注意：必须确保返回结果中不会多于一行一列，其它情况或未找到时返回{error, undefined}</div>
-spec get_one(binary(), [term()]) -> {ok, term()} | {error, term()}.
get_one(Sql, Args) ->
  case catch emysql:execute(?DBMYSQL, Sql, Args) of
    #result_packet{rows = [[R]]} -> {ok, R};
    #result_packet{rows = []} -> {ok, undefined};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的第一行
% 注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(binary()) -> {ok, [term()]} | {error, term()}.
get_row(Sql) ->
  case catch emysql:execute(?DBMYSQL, Sql) of
    #result_packet{rows = [R]} -> {ok, R};
    #result_packet{rows = []} -> {ok, undefined};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的第一行(带有格式化参数)
% 注意：必须确保返回结果中不会多于一行，其它情况或未找到时返回{ok, undefined}</div>
-spec get_row(binary(), [term()]) -> {ok, [term()]} | {error, term()}.
get_row(Sql, Args) ->
  case catch emysql:execute(?DBMYSQL, Sql, Args) of
    #result_packet{rows = [R]} -> {ok, R};
    #result_packet{rows = []} -> {ok, undefined};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的所有行
-spec get_all(binary()) -> {ok, term()} | {error, term()}.
get_all(Sql) ->
  case catch emysql:execute(?DBMYSQL, Sql) of
    #result_packet{rows = R} -> {ok, R};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 取出查询结果中的所有行
-spec get_all(binary(), [term()]) -> {ok, term()} | {error, term()}.
get_all(Sql, Args) ->
  case catch emysql:execute(?DBMYSQL, Sql, Args) of
    #result_packet{rows = R} -> {ok, R};
    #error_packet{msg = Msg} -> {error, Msg};
    Err -> {error, Err}
  end.

% 格式化SQL语句
-spec format_sql(Sql::bitstring(), Args::list()) -> bitstring().
format_sql(Sql, Args) ->
  case catch [emysql_util:encode(A) || A <- Args] of
    NewArgs when is_list(NewArgs) ->
      iolist_to_binary(io_lib:format(Sql, NewArgs));
    Else ->
      ?DEBUG("sql语句执行出错 ~ts， args ~w, reason: ~w", [Sql, Args, Else]),
      Else
  end.

% 创建插入语句值部分
-spec insert_vals_sql(Fields::list()) -> string().
insert_vals_sql(Fields) ->
  Sql = lists:concat(["(", string:join(lists:duplicate(length(Fields), "~s"), ","), ")"]),
  binary_to_list(format_sql(Sql, Fields)).

%  批量replace操作（返回影响行数
-spec batch_replace(Table :: atom(), Keys :: [atom()], Data :: [[term()]]) -> {ok, Affected :: integer()} | {error, bitstring()}.
batch_replace(_Table, _Keys, []) -> 0;
batch_replace(Table, Keys, Data) ->
  exec(get_replace_sql(Table, Keys, Data)).

% 生成replace sql
-spec get_replace_sql(Table :: atom(), Keys :: [atom()], Data :: [[term()]]) -> Sql :: binary().
get_replace_sql(Table, Keys, Data) ->
  <<_:1/binary, KeysBin/binary>> = list_to_binary([[",`", atom_to_list(Key), "`"] || Key <- Keys]),
  SqlHead = list_to_binary(["REPLACE INTO `", atom_to_list(Table), "`(",KeysBin, ") VALUES"]),
  SqlBody = get_batch_sql(Data, []),
  <<SqlHead/binary, SqlBody/binary>>.

% 批量SQL合并
get_batch_sql([], Res) -> list_to_binary(lists:reverse(Res));
get_batch_sql([Row], Res) ->
  SqlBody = list_to_binary(["(~s", lists:duplicate(length(Row) - 1, ",~s"), ");"]),
  get_batch_sql([], [format_sql(SqlBody, Row) | Res]);
get_batch_sql([Row | T], Res) ->
  SqlBody = list_to_binary(["(~s", lists:duplicate(length(Row) - 1, ",~s"), "),"]),
  get_batch_sql(T, [format_sql(SqlBody, Row) | Res]).
