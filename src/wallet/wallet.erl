-module(wallet).
-export([create_wallet/0, update_balance/2, create_transaction/4]).

-include("../include/wallet.hrl").
-include("../include/transaction.hrl").

% Create a new wallet with ECDSA keys
create_wallet() ->
    {PublicKey, PrivateKey} = generate_keys(),
    #wallet{private_key = PrivateKey, public_key = PublicKey, balance = 0}.

% Update the wallet balance
update_balance(Wallet, Amount) ->
    Wallet#wallet{balance = Wallet#wallet.balance + Amount}.

% Create a new transaction
create_transaction(
    #wallet{public_key = SenderPublicKey, private_key = PrivateKey}, RecipientPublicKey, Amount, Fee
) ->
    _TotalAmount = Amount + Fee,
    Signature = crypto:sign(
        ecdsa, sha256, <<SenderPublicKey/binary, RecipientPublicKey/binary, Amount:32>>, [
            PrivateKey, secp256k1
        ]
    ),
    #transaction{
        from = SenderPublicKey,
        to = RecipientPublicKey,
        amount = Amount,
        fee = Fee,
        timestamp = erlang:system_time(),
        signature = Signature
    }.

% Generate ECDSA keys
generate_keys() ->
    {PublicKey, PrivateKey} = crypto:generate_key(ecdh, secp256k1),
    {PublicKey, PrivateKey}.
