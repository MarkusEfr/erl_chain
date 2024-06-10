%%%-------------------------------------------------------------------
%% @doc erl_chain public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_chain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erl_chain_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
