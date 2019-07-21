-module(guid).

-compile(export_all).

-include("common.hrl").

new(role, Id) ->
  (?SERVERID bsl 24) bor Id.

denew(role, Id) ->
  Id bxor (?SERVERID bsl 24).
