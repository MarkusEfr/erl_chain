%% src/transaction.erl
-module(transaction).
-export([new/3, is_valid/1]).

-record(transaction, {
    from,
    to,
    amount,
    timestamp,
    signature
}).

new(From, To, Amount) ->
    Timestamp = erlang:system_time(),
    #transaction{
        from = From,
        to = To,
        amount = Amount,
        timestamp = Timestamp,
        signature = undefined
    }.

is_valid(_Transaction) ->
    %% Add more rigorous validation logic here
    true.
