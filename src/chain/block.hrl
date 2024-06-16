%% include/block.hrl
-record(block, {
    index,
    previous_hash,
    timestamp,
    data,
    hash
}).
