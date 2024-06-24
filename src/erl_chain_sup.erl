-module(erl_chain_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        % Adding blockchain_manager
        #{
            id => blockchain_manager,
            start => {blockchain_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [blockchain_manager]
        },

        % Adding wallet_manager
        #{
            id => wallet_manager,
            start => {wallet_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [wallet_manager]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
