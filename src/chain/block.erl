%% src/block.erl
-module(block).
-include("block.hrl").
-export([new/3, hash/1, is_valid/2]).

new(Index, PreviousHash, Data) ->
    Timestamp = erlang:system_time(),
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
    %% Strong hash function using crypto module and SHA-256
    crypto:hash(
        sha256,
        term_to_binary({
            Block#block.index,
            Block#block.previous_hash,
            Block#block.timestamp,
            Block#block.data
        })
    ).

is_valid(Block, PreviousBlock) ->
    %% Check if the block's index is incremented correctly
    Block#block.index == PreviousBlock#block.index + 1 andalso
        %% Check if the previous hash matches
        Block#block.previous_hash == PreviousBlock#block.hash andalso
        %% Verify the hash of the current block
        Block#block.hash == hash(Block).
