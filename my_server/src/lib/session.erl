-module(session).

-behaviour(gen_server).

-export([
    stop/1, start_link/4,
    init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3
  ]).

-include("common.hrl").

-define(TIMEOUT, infinity).
-define(SOCKETOPTS, [{active, 60}, binary, {packet, 0}]).

start_link(Ref, Socket, Transport, Opts) ->
  ?DEBUG("Session start_link: ~w\n", [{Ref, Socket, Transport, Opts}]),
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

stop(Name) ->
  gen_server:call(Name, stop).

init({Ref, Socket, Transport, _Opts}) ->
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, ?SOCKETOPTS),
  gen_server:enter_loop(?MODULE, [], #{socket => Socket, transport => Transport}, ?TIMEOUT).

handle_call(get_info, _From, State) ->
  {reply, State, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
  ?DEBUG("handle_call:~w, from:~w\n", [_Request, _From]),
  {reply, ok, State}.

handle_cast({login_duplicate, id}, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("handle_cast login_duplicate:~w, from:~w\n", [id, State]),
  Transport:close(Socket),
  {reply, ok, State};

handle_cast({reply, {client_data, Data}}, #{socket := Socket, transport := Transport} = State) ->
  Transport:send(Socket, encode(Data)),
  NewState = State#{client_data => Data},
  {noreply, NewState};

handle_cast({notify, Msg}, #{socket := Socket, transport := Transport} = State) ->
  Transport:send(Socket, encode(Msg)),
  {noreply, State};

handle_cast(_Msg, State) ->
  ?DEBUG("handle_cast:~w\n", [_Msg]),
  {noreply, State}.

handle_info({tcp, Socket, Packet}, #{socket := Socket, transport := Transport} = State) ->
  case decode(Packet, State) of
    ok -> 
      {noreply, State};
    err -> 
      {stop, normal, State};
    {stop, Reason, NewState} ->
      {stop, Reason, NewState};
    {noreply, NewState} -> 
      {noreply, NewState};
    {reply, Msg, NewState} -> 
      Transport:send(Socket, encode(Msg)),
      {noreply, NewState, ?TIMEOUT};
    {notify, Msg} -> 
      Transport:send(Socket, encode(Msg)),
      {noreply, State, ?TIMEOUT};
    {notify, Msg, Changed} when is_map(Changed) -> 
      Transport:send(Socket, encode(Msg)),
      {noreply, maps:merge(State, Changed), ?TIMEOUT};
    _Msg ->
      {noreply, State}
  end;

handle_info({tcp_passive, _Socket}, #{socket := Socket, transport := Transport} = State) ->
  Transport:setopts(Socket, ?SOCKETOPTS),
  {noreply, State, ?TIMEOUT};

handle_info({tcp_closed, _Socket}, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("tcp_closed:~w\n", [Socket]),
  Transport:close(Socket),
  {stop, normal, State};

handle_info({tcp_error, _, Reason}, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("tcp_error Reason:~w\n", [Reason]),
  Transport:close(Socket),
  {stop, normal, State};

handle_info({'EXIT', Pid, Reason}, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("handle_info EXIT reason:~w, pid:~w\n", [Reason, Pid]),
  Transport:close(Socket),
  {stop, normal, State};

handle_info(timeout, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("tcp_timeout:~w\n", [1]),
  Transport:close(Socket),
  {stop, normal, State};

handle_info(_Info, #{socket := Socket, transport := Transport} = State) ->
  ?DEBUG("handle_info:~w\n", [_Info]),
  Transport:close(Socket),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

decode(Packet, State) when is_binary(Packet) ->
  ?DEBUG("decode packet:~w\n", [erlang:binary_to_term(Packet)]),
  case erlang:binary_to_term(Packet) of
    [Mod, Func, Args] when is_atom(Mod) and is_atom(Func) and is_list(Args) ->
      case handle_request(Mod, Func, Args, State) of
        {noreply, NewState} ->
          {noreply, NewState};
        {reply, Msg, NewState} ->
          {reply, Msg, NewState};
        {stop, Reason, NewState} ->
          {stop, Reason, NewState};
        ok ->
          ok;
        err ->
          err;
        _ ->
          case maps:get(id, State, null) of
            Id when is_integer(Id)  ->
              ?DEBUG("router:cast msg:~w\n", [{dispatch, Mod, Func, Args}]),
              router:cast(Id, {dispatch, Mod, Func, Args});

            _ ->
              ?DEBUG("not id, state::~w\n", [State]),
              ok
          end
      end;

    _Err ->
      ?DEBUG("decode err _Err:~w\n", [_Err]),
      err
  end;

decode(_Packet, _State) ->
  ?DEBUG("decode err _Packet:~w\n", [_Packet]),
  err.

encode(Msg) ->
  erlang:term_to_binary(Msg).

handle_request(login, role, [CId], State) ->
  Id = guid:new(role, CId),
  case roles_sup:start_child(Id, self()) of
    {ok, RolePid} ->
      link(RolePid),
      process_flag(trap_exit, true),
      router:cast(Id, {login, Id}),
      login_pool:insert(Id, self()),
      NewState = maps:merge(State, session_init([#{id => Id, cid => CId}])),
      {noreply, NewState};

    {error, {already_started, RolePid}} ->
      ?DEBUG("login player already_started:~w\n", [{RolePid, self()}]),
      case login_pool:lookup(Id) of
        [{Id, SPid}] -> router:cast(SPid, {login_duplicate, Id});
        _ -> login_pool:insert(Id, self())
      end,
      link(RolePid),
      process_flag(trap_exit, true),
      NewState = maps:merge(State, session_init([#{id => Id}])),
      {noreply, NewState};

    _Err ->
      ?DEBUG("login player err:~w, sessionpis: ~w\n", [_Err, self()]),
      {stop, normal, State}
  end;

handle_request(Mod = account, Func, Args, State) when is_atom(Func) andalso is_list(Args)->
  case role:dispatch(Mod, Func, Args, State) of
    {notify, Msg, Changed} -> {reply, Msg, maps:merge(State, Changed)};
    {notify, Msg} -> {reply, Msg, State};
    _ -> ok
  end;

handle_request(_, _, _, State) ->
  {noreply, State}.

% 需要在session init 不需要传给role process的data
session_init(L) ->
  List = [] ++ L,
  lists:foldl(fun(M, Sum) -> maps:merge(M, Sum) end, #{}, List).