%% src/transaction.erl
-module(transaction).
-export([new/3, is_valid/1]).

-record(transaction, {
    from,
    to,
    amount,
    timestamp
}).

new(From, To, Amount) ->
    Timestamp = erlang:system_time(),
    #transaction{
        from = From,
        to = To,
        amount = Amount,
        timestamp = Timestamp
    }.

is_valid(_Transaction) ->
    %% Add validation logic here
    true.
