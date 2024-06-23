-module(wallet).
-export([create_wallet/0, create_transaction/3]).

-record(wallet, {private_key, public_key}).

% Create a new wallet with ECDSA keys
create_wallet() ->
    {PublicKey, PrivateKey} = generate_keys(),
    #wallet{private_key = PrivateKey, public_key = PublicKey}.

% Create a new transaction
create_transaction(#wallet{private_key = PrivateKey, public_key = PublicKey}, Recipient, Amount) ->
    Message = <<Recipient/binary, Amount:32>>,
    Signature = crypto:sign(ecdsa, sha256, Message, [PrivateKey, secp256k1]),
    {transaction, PublicKey, Recipient, Amount, Signature}.

% Generate ECDSA keys
generate_keys() ->
    {PublicKey, PrivateKey} = crypto:generate_key(ecdh, secp256k1),
    {PublicKey, PrivateKey}.
