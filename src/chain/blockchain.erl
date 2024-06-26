-module(blockchain).
-behaviour(gen_server).

-include("../include/block.hrl").
-include("../include/transaction.hrl").
-record(state, {chain = []}).

-export([start_link/0, add_block/1, get_chain/0, init_chain/1, valid_chain/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    {ok, #state{}}.

handle_call({add_block, Transactions}, _From, State) ->
    case State#state.chain of
        [] ->
            {reply, {error, no_genesis_block}, State};
        Chain ->
            case create_block(Chain, Transactions) of
                {ok, NewBlock} ->
                    {ok, Proof} = pzk_consensus:generate_proof(NewBlock),
                    case pzk_consensus:verify_proof(NewBlock, Proof) of
                        true ->
                            lists:foreach(fun update_balances/1, Transactions),
                            NewChain = [NewBlock | Chain],
                            {reply, ok, State#state{chain = NewChain}};
                        false ->
                            {reply, {error, invalid_proof}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;
handle_call(get_chain, _From, State) ->
    {reply, {ok, lists:reverse(State#state.chain)}, State};
handle_call({init_chain, GenesisData}, _From, State) ->
    GenesisBlock = create_genesis_block(GenesisData),
    {reply, ok, State#state{chain = [GenesisBlock]}};
handle_call(valid_chain, _From, State) ->
    Chain = lists:reverse(State#state.chain),
    {reply,
        case validate_chain(Chain) of
            true -> {ok, true};
            false -> {error, invalid_chain}
        end, State};
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

%% Internal functions

create_genesis_block(_Data) ->
    GenesisBlock = #block{
        index = 0,
        previous_hash = <<>>,
        timestamp = erlang:monotonic_time(),
        transactions = [
            {transaction, <<"genesis">>, <<"genesis">>, 0, 0, erlang:monotonic_time(), undefined}
        ],
        hash = <<>>
    },
    GenesisHash = crypto:hash(sha256, term_to_binary(GenesisBlock)),
    GenesisBlock#block{hash = GenesisHash}.

create_block([PreviousBlock | _], Transactions) ->
    {block, Index, _PreviousHash, _, _, PreviousBlockHash} = PreviousBlock,
    NewIndex = Index + 1,
    NewTimestamp = erlang:monotonic_time(),
    NewBlock = #block{
        index = NewIndex,
        previous_hash = PreviousBlockHash,
        timestamp = NewTimestamp,
        transactions = Transactions,
        hash = <<>>
    },
    NewHash = crypto:hash(
        sha256, term_to_binary({NewIndex, PreviousBlockHash, NewTimestamp, Transactions})
    ),
    {ok, NewBlock#block{hash = NewHash}}.

validate_chain([]) ->
    false;
validate_chain([_]) ->
    true;
validate_chain([GenesisBlock | Rest]) ->
    validate_blocks(GenesisBlock, Rest).

validate_blocks(_, []) ->
    true;
validate_blocks(PreviousBlock, [CurrentBlock | Rest]) ->
    case block:is_valid(CurrentBlock, PreviousBlock) of
        true -> validate_blocks(CurrentBlock, Rest);
        false -> false
    end.

update_balances(#transaction{from = Sender, to = Recipient, amount = Amount, fee = Fee}) ->
    wallet_manager:update_balance(Sender, -(Amount + Fee)),
    wallet_manager:update_balance(Recipient, Amount).
