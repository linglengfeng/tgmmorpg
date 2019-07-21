-module(role).

-behaviour(gen_server).

-export([stop/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
% start(Name) ->
%    _sup:start_child(Name).
-include("common.hrl").

stop(Name) ->
   gen_server:call(Name, stop).

start_link({Id, _Session} = Args) ->
   ?DEBUG("role start_link: ~w\n", [{Id, _Session}]),
   gen_server:start_link({global, Id}, ?MODULE, Args, []);

start_link(_Err) ->
      ?DEBUG("role start_linkooook: ~w\n", [{_Err}]),
      ok.

init({Id, Session}) ->
  ?DEBUG("role init: ~w\n", [{Id, Session}]),
   case role_data:load_data(Id) of
      Data when is_map(Data) ->
         router:cast(Session, {reply, {client_data, role_data:client_data(Data)}}),
         {ok, {Id, Session, Data}};
      {err, _Reason} ->
         ?DEBUG("init load data err, reason:~w\n", [_Reason]),
         {stop, normal, stopped, {Id, Session, #{}}}
   end.

handle_call(stop, _From, State) ->
   {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({dispatch, Mod, Func, Args}, {Id, _Session, Data} = State) ->
   Apply = dispatch(Mod, Func, Args, {Id, Data}),
   handle_result(Apply, State);

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Info, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

handle_result(_Apply, State) ->
   {noreply, State}.

dispatch(Mod, Func, Args, State) ->
   case erlang:function_exported(Mod, Func, length(Args) + 1) of
      true ->
         try Mod:Func(Args, State) of
            Msg = {notify, _ClientInfo, Changed} when is_map(Changed) -> Msg;
            {notify, ClientInfo} -> {notify, ClientInfo, #{}};
            Msg = {resolve, _ClientInfo, Context} when is_map(Context) -> Msg;
            ok -> ok;
            _ -> ok
         catch
            _Err ->
               ?DEBUG("dispatch err, reason:~w, mod: ~w\n", [_Err, {Mod, Func, Args}])
         end;

      _ ->
         ?DEBUG("dispatch err, reason:~w, mod: ~w\n", [" mod.func non-existent", {Mod, Func, Args}]),
         ok
   end.

% 考虑每20秒存一次db，每2分钟检查一次socket进程
  % case process_info(ConnPid, message_queue_len) of
  %     {_, QueueLen} when QueueLen > 10000 ->
  %         ?DEBUG("连接进程堆积消息过多，断开连接"),
  %         erlang:exit(ConnPid, kill);
  %     _ -> ignore
  % end;