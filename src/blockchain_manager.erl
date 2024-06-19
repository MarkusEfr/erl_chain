%% src/blockchain_manager.erl

% blockchain_manager:start().
% blockchain_manager:init_chain("Genesis Block").
% blockchain_manager:add_block([transaction:new("A", "B", 10)]).
% blockchain_manager:get_chain().
% blockchain_manager:valid_chain().

-module(blockchain_manager).
-behaviour(gen_server).

-export([start_link/0]).
-export([start/0, stop/0, status/0, add_block/1, get_chain/0, init_chain/1, valid_chain/0]).
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
        undefined ->
            {error, not_running};
        Pid when is_pid(Pid) ->
            gen_server:stop(blockchain),
            ok
    end.

status() ->
    case whereis(blockchain) of
        undefined -> not_running;
        _Pid -> running
    end.

add_block(Transactions) ->
    gen_server:call(?MODULE, {add_block, Transactions}).

get_chain() ->
    gen_server:call(?MODULE, get_chain).

init_chain(GenesisData) ->
    gen_server:call(?MODULE, {init_chain, GenesisData}).

valid_chain() ->
    gen_server:call(?MODULE, valid_chain).

%% gen_server callbacks
init([]) ->
    {ok, undefined}.

handle_call({add_block, Transactions}, _From, State) ->
    case whereis(blockchain) of
        undefined -> {reply, {error, not_running}, State};
        _Pid -> {reply, blockchain:add_block(Transactions), State}
    end;
handle_call(get_chain, _From, State) ->
    case whereis(blockchain) of
        undefined -> {reply, {error, not_running}, State};
        _Pid -> {reply, blockchain:get_chain(), State}
    end;
handle_call({init_chain, GenesisData}, _From, State) ->
    case whereis(blockchain) of
        undefined -> {reply, {error, not_running}, State};
        _Pid -> {reply, blockchain:init_chain(GenesisData), State}
    end;
handle_call(valid_chain, _From, State) ->
    case whereis(blockchain) of
        undefined -> {reply, {error, not_running}, State};
        _Pid -> {reply, blockchain:valid_chain(), State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
