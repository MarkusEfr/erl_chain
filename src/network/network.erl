%% src/network.erl
-module(network).
-behaviour(gen_server).

%% API
-export([start_link/0, add_peer/1, broadcast_block/1, receive_block/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    peers = []
}).

%% API

%% Start the network process
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Add a peer to the network
add_peer(Peer) ->
    gen_server:call(?MODULE, {add_peer, Peer}).

%% Broadcast a block to all peers
broadcast_block(Block) ->
    gen_server:cast(?MODULE, {broadcast_block, Block}).

%% Receive a block from a peer
receive_block(Block) ->
    gen_server:cast(?MODULE, {receive_block, Block}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({add_peer, Peer}, _From, State) ->
    {reply, ok, State#state{peers = [Peer | State#state.peers]}}.

handle_cast({broadcast_block, Block}, State) ->
    lists:foreach(
        fun(Peer) ->
            %% Replace this with actual network communication code
            %% For example: gen_tcp:send(Peer, term_to_binary({block, Block}))
            io:format("Broadcasting block ~p to peer ~p~n", [Block, Peer])
        end,
        State#state.peers
    ),
    {noreply, State};
handle_cast({receive_block, Block}, State) ->
    %% Validate the block and add it to the blockchain if valid
    case blockchain:add_block(Block) of
        {ok, _} -> io:format("Block added successfully: ~p~n", [Block]);
        {error, Reason} -> io:format("Failed to add block: ~p~n", [Reason])
    end,
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
