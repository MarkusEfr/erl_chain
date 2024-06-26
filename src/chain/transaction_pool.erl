-module(transaction_pool).
-behaviour(gen_server).

%% API
-export([start_link/0, add_transaction/1, get_transactions/0, remove_transactions/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {transactions = []}).

-define(TRANSACTION_THRESHOLD, 10).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_transaction(Transaction) ->
    gen_server:call(?MODULE, {add_transaction, Transaction}).

get_transactions() ->
    gen_server:call(?MODULE, get_transactions).

remove_transactions(Transactions) ->
    gen_server:cast(?MODULE, {remove_transactions, Transactions}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({add_transaction, Transaction}, _From, State) ->
    NewTransactions = [Transaction | State#state.transactions],
    %% Check if threshold is met
    if
        length(NewTransactions) >= ?TRANSACTION_THRESHOLD ->
            %% Trigger block generation
            blockchain_manager:generate_block(NewTransactions),
            %% Update state after generating block
            {reply, ok, State#state{transactions = []}};
        true ->
            {reply, ok, State#state{transactions = NewTransactions}}
    end;
handle_call(get_transactions, _From, State) ->
    {reply, State#state.transactions, State}.

handle_cast({remove_transactions, Transactions}, State) ->
    NewTransactions = lists:subtract(State#state.transactions, Transactions),
    {noreply, State#state{transactions = NewTransactions}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
