%% include/block.hrl
-record(block, {
    index,
    previous_hash,
    timestamp,
    transactions,
    hash
}).
