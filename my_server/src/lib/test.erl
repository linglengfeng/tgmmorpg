-module(test).

-compile(export_all).

-include("common.hrl").

-record(aaa, {age = 0, name = ""}).

test(A) ->
  ?DEBUG("debug here is test/1\n"),
  A.

test(A, B) ->
  ?DEBUG("here is test/2\n"),
  aaa,
   A + B.

use_time(Times) ->
  {T, _} = timer:tc(lists, foreach, [fun(_T) -> ([X || X<- [1,2,3,4,5], X>1]) end, lists:seq(1, Times)]),
  T.

use_time1(Times) ->
  {T, _} = timer:tc(lists, foreach, [fun(_T) -> (lists:filter(fun(X) -> X > 1 end, [1,2,3,4,5])) end, lists:seq(1, Times)]),
  T.

use_time2(Times) ->
  A = #aaa{},
  {T1, _} = timer:tc(lists, foreach, [fun(T) -> (A#aaa{age = T}) end, lists:seq(1, Times)]),
  {T2, _} = timer:tc(lists, foreach, [fun(_T) -> (#aaa{age = _Age} = A) end, lists:seq(1, Times)]),
  [T1, T2].

use_time3(Times) ->
  M = #{age => 0, name => ""},
  {T1, _} = timer:tc(lists, foreach, [fun(T) -> (M#{age => T}) end, lists:seq(1, Times)]),
  {T2, _} = timer:tc(lists, foreach, [fun(_T) -> (#{age := _Age} = M) end, lists:seq(1, Times)]),
  [T1, T2].

use_time4(Times) ->
  {T1, _} = timer:tc(lists, foreach, [fun(T) -> match(T) end, lists:seq(1, Times)]),
  {T2, _} = timer:tc(lists, foreach, [fun(T) -> match1(T) end, lists:seq(1, Times)]),
  [T1, T2].

use_time5(Times) ->
  {T1, _} = timer:tc(lists, foreach, [fun(T) -> match3(T) end, lists:seq(1, Times)]),
  {T2, _} = timer:tc(lists, foreach, [fun(T) -> match4(T) end, lists:seq(1, Times)]),
  [T1, T2].

match(A) -> %%%% 经过检验此方法更优
  case (A rem 3) of
    1 -> 1;
    2 -> 2;
    _ -> 0
  end.

match1(A) when (A rem 3) == 1 ->
  1;
match1(A) when (A rem 3) == 2 ->
  2;
match1(_A) ->
  0.

match3(A) ->
  case A of
    1 -> 1;
    2 -> 2;
    _ -> 0
  end.

match4(1) ->%%%% 经过检验此方法更优
  1;
match4(2) ->
  2;
match4(_A) ->
  0.

db_test(Times) ->%% 每次创建耗费大概0.15s
  {T2, _} = timer:tc(lists, foreach, [fun(_T) -> 
      db_role:create(test, 101, "test" ++ "_" ++ utils:term_to_string(utils:rand(1, 1000000))) 
    end, lists:seq(1, Times)]),
  T2.

db_test1(Times, Id) ->%% 每次大概0.07s
  {ok, Data} = db_role:load(Id),
  NewData = Data#{id => Id, test => Id, info => #{}},
  {T2, _} = timer:tc(lists, foreach, [fun(_T) -> 
      db_role:save(NewData)
    end, lists:seq(1, Times)]),
  T2.

db_test2(Times, Name) ->%% 大概每100次耗时0.1s
  {T2, _} = timer:tc(lists, foreach, [fun(_T) -> 
      db_role:repeat_name(Name)
    end, lists:seq(1, Times)]),
  T2.