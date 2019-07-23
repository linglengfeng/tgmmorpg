-module(mongo_test).

-compile(export_all).

test1() ->
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"1">>, <<"name">> => <<"name1">>}),
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"1">>, <<"name">> => <<"name1">>}),
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"111">>, <<"name">> => <<"name111">>}),
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"11">>, <<"name">> => <<"name1">>}),
  mongo_agent:insert_many("role_test", [#{<<"id">> => <<"2">>, <<"name">> => <<"name2">>}, #{<<"id">> => <<"3">>, <<"name">> => <<"name3">>}]),
  ok.

test2() ->
  R1 = mongo_agent:find("role_test", [#{}]),
  R2 = mongo_agent:find("role_test", [#{<<"name">> => <<"name1">>}]),
  R3 = mongo_agent:find_one("role_test", [#{<<"id">> => <<"2">>}]),
  R4 = mongo_agent:find_one("role_test", [#{<<"id">> => <<"11">>, <<"name">> => <<"name1">>}]),
  R5 = mongo_agent:find_one("role_test", [#{<<"id">> => <<"1">>, <<"name">> => <<"name1">>}]),
  {R1, R2, R3, R4, R5}.

% test3() ->
%   R1 = mongo_agent:find_one("role_test",  [#{<<"id">> => <<"2">>}]),
%   mongo_agent:update("role_test", #{<<"id">> => <<"2">>}, #{<<"$set">> => #{<<"name">> => "update_name1"}}),
%   R2 = mongo_agent:find_one("role_test",  [#{<<"id">> => <<"2">>}]),
%   mongo_agent:update("role_test", #{<<"name">> => <<"update_name1">>}, #{<<"$set">> => #{<<"name">> => "update111"}}),
%   R3 = mongo_agent:find_one("role_test",  [#{<<"id">> => <<"2">>}]),
%   {R1, R2, R3}.

test4() ->
  mongo_agent:delete("role_test", 1),
  R1 = mongo_agent:find_one("role_test",  [#{<<"id">> => <<"1">>}]),
  {R1}.

test5() ->
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"1">>, <<"name">> => <<"name1">>}),
  mongo_agent:insert_one("role_test", #{<<"id">> => <<"1">>, <<"name">> => <<"name1">>}),
  R1 = mongo_agent:count("role_test", #{<<"id">> => <<"1">>}),
  R2 = mongo_agent:count("role_test", #{<<"name">> => <<"name1">>}),
  R3 = mongo_agent:count("role_test", #{<<"id">> => <<"111">>}),
  {R1, R2, R3}.

test6(Id) ->
  M = #{1 => 1, "1" => 2, atom => 3, "atom" => 5, [1,2,3] => 6, {#{}, [], 1, "a"} => 8},
  M1 = mongo_agent:key_to_mongo_key(M),
  mongo_agent:insert_one("role_test", M1#{<<"_id">> => Id}),
  Find2 = mongo_agent:find_one("role_test", [#{<<"_id">> => Id}]),
  mongo_agent:parse_mongo_key(Find2).

test7() ->
  R1 = mongo_agent:find("role_test", [#{},#{}]),
  R2 = mongo_agent:find("role_test", [#{},#{projector => #{<<"id">> => 20}}]),
  {R1, R2}.


