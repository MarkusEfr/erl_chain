-module(blockchain_manager_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../src/include/transaction.hrl").
-include("../src/include/wallet.hrl").

%% Comprehensive blockchain flow test
full_blockchain_flow_test() ->
    %% Ensure all necessary processes are started
    ensure_processes_started([transaction_pool, blockchain_manager, blockchain, wallet_manager]),

    %% Create wallets
    Wallets = create_wallets(11),

    %% Add transactions to the pool
    Transactions = add_transactions_to_pool(Wallets),

    %% Verify transaction pool is empty after adding transactions
    ?assertEqual([], transaction_pool:get_transactions()),

    %% Initialize the blockchain with a genesis block
    ok = blockchain_manager:init_chain("Genesis block data"),

    %% Add the transactions to the blockchain
    ok = blockchain_manager:add_block(Transactions),

    %% Retrieve the blockchain and ensure the genesis block is present
    {ok, Chain} = blockchain_manager:get_chain(),
    [_GenesisBlock | _] = Chain.

%% Ensure multiple processes are started
ensure_processes_started(Processes) ->
    lists:foreach(fun ensure_process_started/1, Processes).

%% Ensure a single process is started
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

%% Create a list of wallets
create_wallets(Count) ->
    lists:map(fun(_) -> wallet:create_wallet() end, lists:seq(1, Count)).

%% Add transactions to the pool and return the list of transactions
add_transactions_to_pool(Wallets) ->
    Transactions = [
        wallet:create_transaction(
            lists:nth(1, Wallets), (lists:nth(2, Wallets))#wallet.public_key, 100, 1
        ),
        wallet:create_transaction(
            lists:nth(2, Wallets), (lists:nth(3, Wallets))#wallet.public_key, 200, 1
        ),
        wallet:create_transaction(
            lists:nth(3, Wallets), (lists:nth(4, Wallets))#wallet.public_key, 300, 1
        ),
        wallet:create_transaction(
            lists:nth(4, Wallets), (lists:nth(5, Wallets))#wallet.public_key, 400, 1
        ),
        wallet:create_transaction(
            lists:nth(5, Wallets), (lists:nth(6, Wallets))#wallet.public_key, 500, 1
        ),
        wallet:create_transaction(
            lists:nth(6, Wallets), (lists:nth(7, Wallets))#wallet.public_key, 600, 1
        ),
        wallet:create_transaction(
            lists:nth(7, Wallets), (lists:nth(8, Wallets))#wallet.public_key, 700, 1
        ),
        wallet:create_transaction(
            lists:nth(8, Wallets), (lists:nth(9, Wallets))#wallet.public_key, 800, 1
        ),
        wallet:create_transaction(
            lists:nth(9, Wallets), (lists:nth(10, Wallets))#wallet.public_key, 900, 1
        ),
        wallet:create_transaction(
            lists:nth(10, Wallets), (lists:nth(11, Wallets))#wallet.public_key, 1000, 1
        )
    ],
    lists:foreach(fun transaction_pool:add_transaction/1, Transactions),
    Transactions.
