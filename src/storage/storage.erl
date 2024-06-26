%% src/storage.erl
-module(storage).
-behaviour(gen_server).

%% API
-export([start_link/0, save_blockchain/1, load_blockchain/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    blockchain_file = "blockchain.dat"
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

save_blockchain(Blockchain) ->
    gen_server:call(?MODULE, {save_blockchain, Blockchain}).

load_blockchain() ->
    gen_server:call(?MODULE, load_blockchain).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({save_blockchain, Blockchain}, _From, State) ->
    {ok, File} = file:open(State#state.blockchain_file, [write]),
    file:write(File, term_to_binary(Blockchain)),
    file:close(File),
    {reply, ok, State};
handle_call(load_blockchain, _From, State) ->
    case file:read_file(State#state.blockchain_file) of
        {ok, Binary} ->
            Blockchain = binary_to_term(Binary),
            {reply, {ok, Blockchain}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
