%% src/blockchain.erl
-module(blockchain).
-include("block.hrl").
-behaviour(gen_server).

-export([start_link/0, init_chain/1, add_block/1, valid_chain/0, get_chain/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init_chain(GenesisData) ->
    gen_server:call(?MODULE, {init_chain, GenesisData}).

add_block(Data) ->
    gen_server:call(?MODULE, {add_block, Data}).

valid_chain() ->
    gen_server:call(?MODULE, valid_chain).

get_chain() ->
    gen_server:call(?MODULE, get_chain).

%% Server Callbacks
init([]) ->
    {ok, []}.

handle_call({init_chain, GenesisData}, _From, _State) ->
    GenesisBlock = block:new(<<>>, GenesisData),
    {reply, ok, [GenesisBlock]};

handle_call({add_block, Data}, _From, [LastBlock | _] = Chain) ->
    NewBlock = block:new(LastBlock#block.hash, Data),
    UpdatedChain = [NewBlock | Chain],
    {reply, ok, UpdatedChain};

handle_call(valid_chain, _From, Chain) ->
    {reply, is_valid_chain(Chain), Chain};

handle_call(get_chain, _From, Chain) ->
    {reply, lists:reverse(Chain), Chain};

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

is_valid_chain([_]) ->
    true;
is_valid_chain([CurrentBlock, PreviousBlock | Rest]) ->
    block:is_valid(CurrentBlock, PreviousBlock) andalso is_valid_chain([PreviousBlock | Rest]).
