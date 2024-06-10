%% src/erl_chain_sup.erl
-module(erl_chain_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        {blockchain_manager, {blockchain_manager, start_link, []}, permanent, 5000, worker, [blockchain_manager]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
