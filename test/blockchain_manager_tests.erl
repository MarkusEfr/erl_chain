-module(blockchain_manager_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../src/chain/transaction.hrl").
-include("../src/wallet.hrl").

%% Comprehensive blockchain flow test
full_blockchain_flow_test() ->
    %% Ensure processes are started
    ensure_process_started(transaction_pool),
    ensure_process_started(blockchain_manager),
    ensure_process_started(blockchain),

    %% Create wallets
    Wallet1 = wallet:create_wallet(),
    Wallet2 = wallet:create_wallet(),
    Wallet3 = wallet:create_wallet(),
    Wallet4 = wallet:create_wallet(),
    Wallet5 = wallet:create_wallet(),
    Wallet6 = wallet:create_wallet(),
    Wallet7 = wallet:create_wallet(),
    Wallet8 = wallet:create_wallet(),
    Wallet9 = wallet:create_wallet(),
    Wallet10 = wallet:create_wallet(),
    Wallet11 = wallet:create_wallet(),

    %% Add transactions to the pool
    Transaction1 = wallet:create_transaction(Wallet1, Wallet2#wallet.public_key, 100, 1),
    ok = transaction_pool:add_transaction(Transaction1),
    Transaction2 = wallet:create_transaction(Wallet2, Wallet3#wallet.public_key, 200, 1),
    ok = transaction_pool:add_transaction(Transaction2),
    Transaction3 = wallet:create_transaction(Wallet3, Wallet4#wallet.public_key, 300, 1),
    ok = transaction_pool:add_transaction(Transaction3),
    Transaction4 = wallet:create_transaction(Wallet4, Wallet5#wallet.public_key, 400, 1),
    ok = transaction_pool:add_transaction(Transaction4),
    Transaction5 = wallet:create_transaction(Wallet5, Wallet6#wallet.public_key, 500, 1),
    ok = transaction_pool:add_transaction(Transaction5),
    Transaction6 = wallet:create_transaction(Wallet6, Wallet7#wallet.public_key, 600, 1),
    ok = transaction_pool:add_transaction(Transaction6),
    Transaction7 = wallet:create_transaction(Wallet7, Wallet8#wallet.public_key, 700, 1),
    ok = transaction_pool:add_transaction(Transaction7),
    Transaction8 = wallet:create_transaction(Wallet8, Wallet9#wallet.public_key, 800, 1),
    ok = transaction_pool:add_transaction(Transaction8),
    Transaction9 = wallet:create_transaction(Wallet9, Wallet10#wallet.public_key, 900, 1),
    ok = transaction_pool:add_transaction(Transaction9),
    Transaction10 = wallet:create_transaction(Wallet10, Wallet11#wallet.public_key, 1000, 1),
    ok = transaction_pool:add_transaction(Transaction10),

    %% Check if the transaction pool is empty after adding 10 transactions
    ?assertEqual([], transaction_pool:get_transactions()),

    %% Initialize the blockchain with a genesis block
    ok = blockchain_manager:init_chain("Genesis block data"),

    %% Retrieve the blockchain and ensure the genesis block is present
    {ok, Chain} = blockchain_manager:get_chain(),
    [_GenesisBlock | _] = Chain.

%% Ensure a process is started
ensure_process_started(Process) ->
    case whereis(Process) of
        undefined ->
            case Process:start_link() of
                {ok, _Pid} -> ok;
                {error, already_started} -> ok
            end;
        _Pid ->
            ok
    end.
