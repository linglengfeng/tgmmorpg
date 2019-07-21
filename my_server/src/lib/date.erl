-module(date).

-compile(export_all).

now() ->
  os:timestamp().

% 取得当前的unix时间戳
-spec unixtime() -> pos_integer().
unixtime() ->
  {M, S, _} = ?MODULE:now(),
  M * 1000000 + S.

% 取得当前的unix时间戳
-spec unixtime2() -> pos_integer().
unixtime2() ->
  {M, S, _} = os:timestamp(),
  M * 1000000 + S.

unixtime(ms) ->
  {S1, S2, S3} = date:now(),
  trunc(S1 * 1000000000 + S2 * 1000 + S3 / 1000);
unixtime(zero) ->
  {M, S, MS} = date:now(),
  {_, Time} = calendar:now_to_local_time({M, S, MS}),
  M * 1000000 + S - calendar:time_to_seconds(Time);
unixtime(five) ->
  unixtime(zero) + 3600 * 5;
unixtime(next_hour) ->
  Hour = hour() + 1,
  unixtime(zero) + 3600 * Hour;
unixtime({five, Ts}) ->
  unixtime({zero, Ts}) + 3600 * 5;
unixtime({five_week, Ts}) ->
  unixtime({zero_week, Ts}) + 3600 * 5;
unixtime({zero, Ts}) ->
  Base = unixtime(zero),
  case Ts > Base of
    false -> Base - util:ceil((Base - Ts) / 86400) * 86400;
    true -> (Ts - Base) div 86400 * 86400 + Base
  end;
unixtime({next_day, Ts}) ->
  unixtime({zero, Ts}) + 86400;
unixtime({next_time, DayTs}) ->
  Now = unixtime(),
  NextT = next_diff(DayTs),
  Now + NextT.

% 判断时间戳是不是今天
is_today(Unixtime) ->
  unixtime(zero) =:= unixtime({zero, Unixtime}).

% 当前的小时
hour() ->
  % {_, {Hour, _, _}} = calendar:local_time(),
  {Hour, _, _} = ?MODULE:time(),
  Hour.

% 当前时间（代替erlang:time/0)
time() ->
  {_, Time} = seconds_to_datetime(unixtime()),
  Time.

% 当前日期（代替erlang:date/0）
date() ->
  {Date, _} = seconds_to_datetime(unixtime()),
  Date.

%  将Unixtime转换成当地时间
-spec seconds_to_datetime(Unixtime) -> {{Y :: non_neg_integer(), M :: non_neg_integer(), D:: non_neg_integer()}, 
  {HH :: non_neg_integer(), MM :: non_neg_integer, SS :: non_neg_integer()}} when Unixtime :: non_neg_integer().
seconds_to_datetime(Unixtime) ->
  DT = calendar:gregorian_seconds_to_datetime(Unixtime + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})),
  erlang:universaltime_to_localtime(DT).

% 取得当前距离指定时间下次到达时相差的秒数
-spec next_diff(H, M, S) -> Seconds when
  H :: 0..23,
  M :: 0..59,
  S :: 0..59,
  Seconds :: pos_integer().
next_diff(H, M, S) ->
  Sec = H * 3600 + M * 60 + S,
  next_diff(Sec).

-spec next_diff(0..86400 | [0..86400]) -> Seconds::pos_integer().
next_diff(L = [_ | _]) ->
  lists:min([next_diff(Sec) || Sec <- L]);
next_diff({H, M, S}) ->
  next_diff(H, M, S);
next_diff(Sec) ->
  Now = unixtime(),
  next_diff(Sec, Now).
next_diff(Sec, T) ->
  Zero = unixtime({zero, T}),
  Base = Zero + Sec, %% 取当天距离X的时间为指定时间
  case Base > T of 
    true -> Base - T; %% 当前时间比指定时间小 直接返回差距
    false -> Base + 86400 - T %% 当前时间比指定时间大 加上一天时间后求差
  end.
