-module(utils).

-compile(export_all).

-include("common.hrl").

to_string(X) -> lists:flatten(io_lib:format("~w", [X])).

to_atom(X) when is_atom(X) -> X;
to_atom(X) -> string_to_atom(to_list(X)).

to_list(X) when is_integer(X)     -> integer_to_list(X);
to_list(X) when is_binary(X)      -> binary_to_list(X);
to_list(X) when is_float(X)       -> float_to_list(X);
to_list(X) when is_atom(X)        -> atom_to_list(X);
to_list(X) when is_binary(X)      -> binary_to_list(X);
to_list(X) when is_bitstring(X)   -> bitstring_to_list(X);
to_list(X) when is_pid(X)         -> pid_to_list(X);
to_list(X) when is_function(X)    -> erlang:fun_to_list(X);
to_list(X) when is_port(X)        -> erlang:port_to_list(X);
to_list(X) when is_tuple(X)       -> to_string(X);
to_list(X) when is_list(X)        -> X;
to_list(X)                        -> to_string(X).

string_to_atom(Str) ->
  case catch(list_to_existing_atom(Str)) of
    {'EXIT', _} -> list_to_atom(Str);
    Atom when is_atom(Atom) -> Atom
  end.

% term序列化
term_to_string(Term) -> io_lib:format("~w", [Term]).

term_to_bitstring(Term) -> list_to_bitstring(term_to_string(Term)).

to_bitstring(Str) when is_list(Str) -> unicode:characters_to_binary(Str);
to_bitstring(Val) -> to_binary(Val).

% term反序列化，string转换为term
string_to_term(undefined) -> {ok, undefined};
string_to_term("undefined") -> {ok, undefined};
string_to_term(String) when is_bitstring(String) -> string_to_term(binary_to_list(String));
string_to_term(String) ->
  S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [{return, list}, global]),
  case erl_scan:string(S ++ ".") of
    {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
    {error, Err, _} -> {error, Err}
  end.

string_to_term2(undefined) -> {ok, undefined};
string_to_term2("undefined") -> {ok, undefined};
string_to_term2(String) when is_bitstring(String) -> string_to_term2(binary_to_list(String));
string_to_term2(String) ->
  S = re:replace(String, "<[0-9]+\\.[0-9]+\\.[0-9]+>", "undefined", [unicode, {return, list}, global]),
  case erl_scan:string(S ++ ".") of
    {ok, Tokens, _} -> erl_parse:parse_term(Tokens);
    {error, Err, _} -> {error, Err}
  end.

% 转二进制
to_binary(Val) when is_integer(Val) -> list_to_binary(integer_to_list(Val));
to_binary(Val) when is_float(Val) -> list_to_binary(float_to_list(Val));
to_binary(Val) when is_list(Val) -> list_to_binary(Val);
to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_atom(Val) -> to_binary(atom_to_list(Val));
to_binary(_Val) -> <<>>.

% maps删除keys
map_delete(Map = #{}, Keys) when is_list(Keys) ->
  AllKeys = maps:keys(Map),
  maps:with(AllKeys -- Keys, Map);
map_delete(Map = #{}, Keys)->
  map_delete(Map, [Keys]);
map_delete(Map, _) ->
  Map.

% maps删除除了keys以外的key
map_delexcept(Map = #{}, Keys) when is_list(Keys) ->
  AllKeys = maps:keys(Map),
  maps:without(AllKeys -- Keys, Map);
map_delexcept(Map = #{}, Keys) ->
  map_delexcept(Map, [Keys]);
map_delexcept(Map, _) ->
  Map.

% 随机
% 从list中随机取出一项
rand_list(List) ->
  Index = length(List),
  lists:nth(rand:uniform(Index), List).

% 随机一个在Min，Max之间的整数
rand(Min, Max) ->
  rand:uniform(Max - Min + 1) + Min - 1.