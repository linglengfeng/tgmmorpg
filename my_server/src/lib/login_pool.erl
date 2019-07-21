-module(login_pool).

-include("common.hrl").

-export([
  init/0,
  insert/2,
  lookup/0,
  lookup/1,
  update/2,
  lookup_bypid/1,
  delete/1
]).

-define(ETSNAME, ?MODULE).

init() ->
  ets:new(?ETSNAME, [set, public, named_table, {keypos, 1}]).

insert(Id, Pid) ->
  ets:insert(?ETSNAME, {Id, #{pid => Pid}}).

lookup() ->
  ets:tab2list(?ETSNAME).

lookup(Id) ->
  ets:lookup(?ETSNAME, Id).

update(Id, Pid) ->
  case ets:update_element(?ETSNAME, Id, {2, Pid}) of
    true -> true;
    _ -> 
      ?DEBUG("~w ets table no ~w, update: ~w\n", [?ETSNAME, Id, {Id, Pid}]),
      false
  end.

lookup_bypid(Pid) ->
  ets:match_object(?ETSNAME, {'_', Pid}).

delete(Id) ->
  ets:delete(?ETSNAME, Id).

