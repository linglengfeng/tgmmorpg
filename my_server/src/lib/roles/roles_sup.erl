-module(roles_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/2]).

-include("common.hrl").

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Id, Session) ->
  % ChildSpecs = [#{id => role,
  %   start => {role, start_link, [{Id, Session}]},
  %   restart => temporary,
  %   shutdown => infinity,
  %   type => worker,
  %   modules => [role]}
  % ],
  ?DEBUG("roles_sup start_child: ~w\n", [{Id, Session}]),
  supervisor:start_child(?MODULE, [{Id, Session}]).

% ConnSupSpec = #{id => connection_sup,
% start => {esockd_connection_sup, start_link, [Opts, MFA]},
% restart => transient,
% shutdown => infinity,
% type => supervisor,
% modules => [esockd_connection_sup]},
% {ok, ConnSup} = supervisor:start_child(Sup, ConnSupSpec),

init(_) ->
  SupFlags = #{strategy => simple_one_for_one,
                intensity => 1,
                period => 5},
  ChildSpecs = [#{id => role,
                  start => {role, start_link, []},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [role]}
                ],
  ?DEBUG("roles_sup init: ~w\n", [{1, 1}]),
  {ok, {SupFlags, ChildSpecs}}.

