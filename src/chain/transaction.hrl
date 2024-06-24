%% include/transaction.hrl
-record(transaction, {
    from,
    to,
    amount,
    fee,
    timestamp
}).
