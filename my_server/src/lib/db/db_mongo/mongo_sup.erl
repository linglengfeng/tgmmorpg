-module(mongo_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
  SupFlags = #{strategy => one_for_one,
                intensity => 1,
                period => 5},
  ChildSpecs = [#{id => mongo_agent,
                  start => {mongo_agent, start_link, [mongo_agent]},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [mongo_agent]}
                ],
  {ok, {SupFlags, ChildSpecs}}.