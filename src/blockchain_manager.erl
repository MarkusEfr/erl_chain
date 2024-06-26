-module(blockchain_manager).
-behaviour(gen_server).

-record(state, {chain = [], transaction_pool_pid = undefined}).

-export([
    start_link/0,
    start/0,
    stop/0,
    status/0,
    add_block/1,
    get_chain/0,
    init_chain/1,
    valid_chain/0,
    generate_block/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    case whereis(blockchain_manager) of
        undefined ->
            {ok, Pid} = gen_server:start_link(
                {local, blockchain_manager}, blockchain_manager, [], []
            ),
            {started, Pid};
        Pid when is_pid(Pid) ->
            {already_running, Pid}
    end.

stop() ->
    case whereis(blockchain_manager) of
        undefined ->
            {error, not_running};
        Pid when is_pid(Pid) ->
            gen_server:stop(blockchain_manager),
            ok
    end.

status() ->
    case whereis(blockchain_manager) of
        undefined -> not_running;
        _Pid -> running
    end.

add_block(Transactions) ->
    gen_server:call(blockchain_manager, {add_block, Transactions}).

get_chain() ->
    gen_server:call(blockchain_manager, get_chain).

init_chain(GenesisData) ->
    gen_server:call(blockchain_manager, {init_chain, GenesisData}).

valid_chain() ->
    gen_server:call(blockchain_manager, valid_chain).

generate_block(Transactions) ->
    gen_server:cast(blockchain_manager, {generate_block, Transactions}).

%% gen_server callbacks

init([]) ->
    %% Get transaction_pool
    TransactionPoolPid = whereis(transaction_pool),
    {ok, #state{transaction_pool_pid = TransactionPoolPid}}.

handle_call({add_block, Transactions}, _From, State) ->
    case blockchain:add_block(Transactions) of
        ok -> {reply, ok, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(get_chain, _From, State) ->
    case blockchain:get_chain() of
        {ok, Chain} -> {reply, {ok, Chain}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call({init_chain, GenesisData}, _From, State) ->
    case blockchain:init_chain(GenesisData) of
        ok -> {reply, ok, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(valid_chain, _From, State) ->
    case blockchain:valid_chain() of
        {ok, true} -> {reply, {ok, true}, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({generate_block, Transactions}, State) ->
    %% Trigger block generation
    case blockchain:add_block(Transactions) of
        ok ->
            %% Remove transactions from the pool
            transaction_pool:remove_transactions(Transactions),
            {noreply, State};
        {error, _Reason} ->
            %% Handle error
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
