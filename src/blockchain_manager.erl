%% src/blockchain_manager.erl
-module(blockchain_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([start/0, stop/0, status/0, get_chain/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    case whereis(blockchain) of
        undefined -> 
            {ok, Pid} = blockchain:start_link(),
            {started, Pid};
        Pid when is_pid(Pid) ->
            {already_running, Pid}
    end.

stop() ->
    case whereis(blockchain) of
        undefined -> {error, not_running};
        Pid when is_pid(Pid) -> 
            gen_server:stop(blockchain),
            ok
    end.

status() ->
    case whereis(blockchain) of
        undefined -> not_running;
        _Pid -> running
    end.

get_chain() ->
    case whereis(blockchain) of
        undefined -> {error, not_running};
        _Pid -> blockchain:get_chain()
    end.

%% gen_server callbacks
init([]) ->
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
