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

find_one(Collection, Args) ->
  case catch call({find_one, Collection, Args}) of
    Result = #{} -> Result;
    _Err -> 
      ?DEBUG("mongo_agent:find_one err: ~w\n", [_Err]),
      ok
  end.

insert_one(Collection, Args) ->
  cast({insert, Collection, [Args]}).

insert_many(Collection, List) ->
  [insert_one(Collection, X) || X <- List].

% 只允许通过id更新
update_one(Collection, Id, Changed) ->
  Command = #{<<"$set">> => Changed},
  BsonId = list_to_binary(integer_to_list(Id)),
  case catch call({update, Collection, [#{<<"id">> => BsonId}, Command]}) of
    {true , _} -> ok;
    _Err ->
      ?DEBUG("mongo_agent:update err: ~w\n", [_Err]),
      ok
  end.

update_many(Collection, List) ->
  [update_one(Collection, Id, Changed) || {Id, Changed} <- List].

count(Collection, Condition) ->
  call({count, Collection, [Condition]}).

% note:慎用 
% 若有其他根据条件删除再加接口
delete(Collection, Id) ->
  BsonId = list_to_binary(integer_to_list(Id)),
  cast({delete, Collection, [#{<<"id">> => BsonId}]}).

% 通用接口 mc_worker_api 详情去看源码
call_api(Func, Collection, Args) ->
  case catch call({Func, Collection, Args}) of
    Reason -> Reason  
  end.

cast_api(Func, Collection, Args) ->
  cast({Func, Collection, Args}).

% todo:写一个map的key value转换成bson的函数
% 例如 #{id => 1}   ===>> #{<<"id">> => <<"1">>}





