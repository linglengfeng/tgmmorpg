-module(router).

-export([
  call/2,
  cast/2,
  statem_call/2,
  statem_cast/2,
  info/2,
  info/3,
  register/2,
  where_is/1,
  pid_state/1,
  process_info/1,
  onlines/1,
  pid_name/1
]).

call(Pid, Request) when is_pid(Pid) ->
  gen_server:call(Pid, Request);
call(Id, Request) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, Request);
    _ -> ok
  end.

cast(Pid, Request) when is_pid(Pid) ->
  % Pid ! Request;
  gen_server:cast(Pid, Request);
cast(Id, Request) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, Request);
    _ -> ok
  end.
statem_call(Pid, Request) when is_pid(Pid) ->
    gen_server:call(Pid, Request);
statem_call(Id, Request) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> gen_server:call(Pid, Request);
    _ -> ok
  end.

statem_cast(Pid, Request) when is_pid(Pid) ->
  % Pid ! Request;
  gen_server:cast(Pid, Request);
statem_cast(Id, Request) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> gen_server:cast(Pid, Request);
    _ -> ok
  end.

info(Pid, Request) when is_pid(Pid) ->
  erlang:send_after(Pid, Request, 0);
info(Id, Request) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> erlang:send_after(Id, Request, 0);
    _ -> ok
  end.

info(Pid, Request, Delay) when is_pid(Pid) ->
  erlang:send_after(Pid, Request, max(0, Delay));
info(Id, Request, Delay) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> erlang:send_after(Pid, Request, max(0, Delay));
    _ -> ok
  end.

register(Name, Pid) ->
  global:re_register_name(Name, Pid).

where_is(Name) ->
    global:whereis_name(Name).

pid_state(Pid) when is_pid(Pid) ->
  sys:get_state(Pid);

pid_state(Id) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> sys:get_state(Pid);
    _ -> ok
  end.

process_info(Pid) when is_pid(Pid) ->
    erlang:process_info(Pid);
process_info(Id) ->
  case where_is(Id) of
    Pid when is_pid(Pid) -> erlang:process_info(Pid);
    _ -> ok
  end.

onlines(_Mod) ->
  ok.

pid_name(Pid) when is_pid(Pid) ->
  ok.