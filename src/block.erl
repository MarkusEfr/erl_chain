%% src/block.erl
-module(block).
-include("block.hrl").
-export([new/2, hash/1, is_valid/2]).

new(PreviousHash, Data) ->
    Timestamp = erlang:system_time(),
    %% This will be updated later
    Index = 0,
    Block = #block{
        index = Index,
        previous_hash = PreviousHash,
        timestamp = Timestamp,
        data = Data,
        hash = <<>>
    },
    Hash = hash(Block),
    Block#block{hash = Hash}.

hash(Block) ->
    %% Simple hash function for demonstration (should use SHA-256 or similar in real implementation)
    erlang:phash2({
        Block#block.index, Block#block.previous_hash, Block#block.timestamp, Block#block.data
    }).

is_valid(Block, PreviousBlock) ->
    Block#block.previous_hash == PreviousBlock#block.hash.
