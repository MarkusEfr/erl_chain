-module(erl_chain_tests).

-include_lib("eunit/include/eunit.hrl").

% Include the wallet record definition
-include("../src/wallet.hrl").

-export([run_tests/0]).

% Define test functions
create_wallet_test() ->
    {ok, _WalletManagerPid} = wallet_manager:start_link(),
    {ok, Wallet1} = wallet_manager:create_wallet(),
    {ok, Wallet2} = wallet_manager:create_wallet(),
    PublicKey1 = Wallet1#wallet.public_key,
    PublicKey2 = Wallet2#wallet.public_key,
    ?assertNotEqual(undefined, PublicKey1),
    ?assertNotEqual(undefined, PublicKey2).

init_chain_test() ->
    {ok, _BlockchainPid} = blockchain:start_link(),
    ?assertEqual(ok, blockchain:init_chain("Genesis Block")).

update_balance_test() ->
    % {ok, _WalletManagerPid} = wallet_manager:start_link(),
    {ok, Wallet1} = wallet_manager:create_wallet(),
    PublicKey1 = Wallet1#wallet.public_key,
    ?assertEqual(ok, wallet_manager:update_balance(PublicKey1, 100)),
    {ok, UpdatedWallet1} = wallet_manager:get_wallet(PublicKey1),
    ?assertEqual(100, UpdatedWallet1#wallet.balance).

add_block_test() ->
    % {ok, _WalletManagerPid} = wallet_manager:start_link(),
    % {ok, _BlockchainPid} = blockchain:start_link(),
    blockchain:init_chain("Genesis Block"),
    {ok, Wallet1} = wallet_manager:create_wallet(),
    {ok, Wallet2} = wallet_manager:create_wallet(),
    PublicKey1 = Wallet1#wallet.public_key,
    PublicKey2 = Wallet2#wallet.public_key,
    wallet_manager:update_balance(PublicKey1, 100),
    Transaction = wallet:create_transaction(Wallet1, PublicKey2, 10, 1),
    ?assertEqual(ok, blockchain:add_block([Transaction])),
    ?assertEqual({ok, true}, blockchain:valid_chain()).

validate_blockchain_test() ->
    % {ok, _BlockchainPid} = blockchain:start_link(),
    blockchain:init_chain("Genesis Block"),
    {ok, Chain} = blockchain:get_chain(),
    ?assertEqual(1, length(Chain)).

% Define the test suite
all_tests() ->
    [
        create_wallet_test,
        init_chain_test,
        update_balance_test,
        add_block_test,
        validate_blockchain_test
    ].

% Run the test suite
run_tests() ->
    eunit:test(all_tests()).
