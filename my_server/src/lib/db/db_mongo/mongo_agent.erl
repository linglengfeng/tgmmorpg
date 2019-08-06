-module(mongo_agent).

-behaviour(gen_server).

-include("common.hrl").

-compile(export_all).
% -export([stop/1, start_link/1]).
% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% -export([call/1, cast/1]).

% % 注意事项：
% % 如果 map 的 key 为数字时，存的时候必须把 key 转化为string
% % 取出来的时候，key 全部是string，得转化一次
stop(Name) ->
  gen_server:call(Name, stop).

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
  {ok, Opts} = application:get_env(outside_service, mongo),
  {ok, Conn} = mc_worker_api:connect(Opts),
  ?DEBUG("mc_worker:start_link success, opts:~w, pid:~w\n", [Opts, Conn]),
  {ok, #{conn => Conn}}.

handle_call({Func, Collection, Args}, _From, State =  #{conn := Conn}) ->
  NewArgs = [Conn, list_to_binary(Collection)] ++ Args,
  {reply, apply(mc_worker_api, Func, NewArgs), State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({Func, Collection, Args}, State =  #{conn := Conn}) ->
  NewArgs = [Conn, list_to_binary(Collection)] ++ Args,
  apply(mc_worker_api, Func, NewArgs),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ?DEBUG("mongodb agent terminate, reason is: ~w\n", [_Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

call(Request) ->
  gen_server:call(?MODULE, Request).

cast(Request) ->
  gen_server:cast(?MODULE, Request).

-spec find(string(), list()) -> term().
find(Collection, Args) ->
  case catch call({find, Collection, Args}) of
    {ok, Pid} -> 
      Result = 
        case mc_cursor:next(Pid) of
          {R} -> R;
          Other -> Other
        end,
      mc_cursor:close(Pid),
      Result;
    _Err -> 
      ?DEBUG("mongo_agent:find err: ~w\n", [_Err]),
      ok
end.

-spec find_all(string(), list()) -> list().
find_all(Collection, Args) ->
  case catch call({find, Collection, Args}) of
    {ok, Pid} -> 
      Return = find_all_loop(Pid, ok, []),
      mc_cursor:close(Pid),
      Return;
    _Err -> 
      ?DEBUG("mongo_agent:find err: ~w\n", [_Err]),
      ok
end.

find_all_loop(Pid, ok, Return) ->
  find_all_loop(Pid, mc_cursor:next(Pid), Return);
find_all_loop(Pid, {LastReture}, Return) ->
  find_all_loop(Pid, mc_cursor:next(Pid), [LastReture | Return]);
find_all_loop(_Pid, _, Return) ->
  Return.

-spec find_one(string(), list()) -> term().
find_one(Collection, Args) ->
  case catch call({find_one, Collection, Args}) of
    Result = #{} -> Result;
    _Err -> 
      ok
  end.

-spec insert_one(string(), map()) -> term().
insert_one(Collection, Args) ->
  cast({insert, Collection, [Args]}).

-spec insert_many(string(), list()) -> term().
insert_many(Collection, List) ->
  [insert_one(Collection, X) || X <- List].

% 只允许通过id更新
-spec update_one(string(), integer(), map()) -> term().
update_one(Collection, Id, Changed) ->
  Command = #{<<"$set">> => Changed},
  case catch call({update, Collection, [#{<<"_id">> => Id}, Command]}) of
    {true , _} -> ok;
    _Err ->
      ?DEBUG("mongo_agent:update err: ~w\n", [_Err]),
      ok
  end.

-spec update_many(string(), list()) -> term().
update_many(Collection, List) ->
  [update_one(Collection, Id, Changed) || {Id, Changed} <- List].

-spec count(string(), map()) -> term().
count(Collection, Condition) ->
  call({count, Collection, [Condition]}).

% note:慎用 
% 若有其他根据条件删除再加接口
-spec delete(string(), integer()) -> term().
delete(Collection, Id) ->
  cast({delete, Collection, [#{<<"_id">> => Id}]}).

% 通用接口 mc_worker_api 详情去看源码
-spec call_api(atom(), string(), term()) -> term().
call_api(Func, Collection, Args) ->
  case catch call({Func, Collection, Args}) of
    Reason -> Reason  
  end.

-spec cast_api(atom(), string(), term()) -> term().
cast_api(Func, Collection, Args) ->
  cast({Func, Collection, Args}).

% 某些不能转用到了再说吧
key_to_mongo_key(Map) when is_map(Map) ->
  maps:fold(fun(K, V, Acc) ->
    Acc#{utils:to_atom(type_deparse(K)) => key_to_mongo_key(V)}
  end, #{}, Map);
key_to_mongo_key(Map) ->
  Map.

parse_mongo_key(Map) when is_map(Map) ->
  NewMap = maps:remove(<<"_id">>, Map),
  maps:fold(fun(K, V, Acc) ->
      Acc#{parse_key_to_atom1(K) => parse_mongo_key(V)}
  end, #{}, NewMap);
parse_mongo_key(Map) ->
  Map.

type_deparse(Value) when is_integer(Value) ->
  {Value, int};
type_deparse(Value) when is_atom(Value) ->
  {Value, atom};
type_deparse(Value) when is_list(Value) ->
  {Value, list};
type_deparse(Value) ->
  Value.

type_parse({Value, int}) ->
  Value;
type_parse({Value, atom}) ->
  Value;
type_parse({Value, list}) ->
  Value;
type_parse(Value) when is_integer(Value) ->
  integer_to_list(Value);
type_parse(Value) when is_atom(Value) ->
  atom_to_list(Value);
type_parse(Value) ->
  Value.

parse_key_to_atom1(Key) when Key =:= <<"_id">> ->
  Key;
parse_key_to_atom1(Key) when is_binary(Key) ->
  case utils:string_to_term(binary_to_list(Key)) of
    {ok, Value} -> type_parse(Value);
    _Err -> 
      ?DEBUG("canot parse_key_to_atom:~w\n", [_Err]),
      Key
  end;
parse_key_to_atom1(Key) ->
  Key.

% test
% M = #{1 => 1, "1" => 2, atom => 3, "atom" => 5, [1,2,3] => 6, {#{}, [], 1, "a"} => 8}.
% M1 = mongo_agent:key_to_mongo_key(M).
% mongo_agent:insert_one("role_test", M1#{<<"_id">> => 44}).
% Find2 = mongo_agent:find_one("role_test", [#{<<"_id">> => 44}]).
% mongo_agent:parse_mongo_key(Find2).

% % note: map中的key不能太复杂，具体不能用那些用到了再备注吧
% % 确定没问题的有 atom，int，tuple
% % aaa 与 "aaa", 2 与 "2"这种做key效果是一样的
% % key不能是 [1,2,3]这种list
% int_key_to_string(Map) when is_map(Map) ->
%   maps:fold(fun(K, V, Acc) ->
%     case K of
%       K when is_integer(K) -> Acc#{utils:to_atom(K) => int_key_to_string(V)};
%       K -> Acc#{utils:to_atom(K) => int_key_to_string(V)}
%     end
%   end, #{}, Map);
% int_key_to_string(Map) ->
%   Map.

% to_atom_key(Config) when is_map(Config) ->
%   maps:fold(fun(K, V, Acc) ->
%       Acc#{parse_key_to_atom(K) => to_atom_key(V)}
%   end, #{}, Config);
% to_atom_key(Config) when is_list(Config) ->
%   maps:map(fun(_K, V) ->
%       to_atom_key(V)
%   end, Config);
% to_atom_key(Config) ->
%   Config.

% parse_key_to_atom(Key) when is_binary(Key) ->
%   case utils:string_to_term(binary_to_list(Key)) of
%     {ok, Value} -> Value;
%     _Err -> 
%       ?DEBUG("canot parse_key_to_atom:~w\n", [_Err]),
%       Key
%   end;
% parse_key_to_atom(Key) ->
%   Key.





