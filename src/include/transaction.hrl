%% include/transaction.hrl
-record(transaction, {
    from,
    to,
    amount,
    fee,
    timestamp,
    signature
}).

-define(TRANSACTION_FEE, 1).

-define(TRANSACTION_VALID_TIME, 5).
