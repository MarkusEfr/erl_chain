-module(wallet_manager).
-behaviour(gen_server).

-export([start_link/0, create_wallet/0, get_wallet/1, update_balance/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("wallet.hrl").

% Use a map to store wallets
-record(state, {wallets = #{}}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_wallet() ->
    gen_server:call(?MODULE, create_wallet).

get_wallet(PublicKey) ->
    gen_server:call(?MODULE, {get_wallet, PublicKey}).

update_balance(PublicKey, Amount) ->
    gen_server:call(?MODULE, {update_balance, PublicKey, Amount}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(create_wallet, _From, State) ->
    Wallet = wallet:create_wallet(),
    Wallets = maps:put(Wallet#wallet.public_key, Wallet, State#state.wallets),
    {reply, {ok, Wallet}, State#state{wallets = Wallets}};
handle_call({get_wallet, PublicKey}, _From, State) ->
    case maps:get(PublicKey, State#state.wallets) of
        undefined -> {reply, {error, wallet_not_found}, State};
        Wallet -> {reply, {ok, Wallet}, State}
    end;
handle_call({update_balance, PublicKey, Amount}, _From, State) ->
    case maps:get(PublicKey, State#state.wallets) of
        undefined ->
            {reply, {error, wallet_not_found}, State};
        Wallet ->
            UpdatedWallet = wallet:update_balance(Wallet, Amount),
            Wallets = maps:put(PublicKey, UpdatedWallet, State#state.wallets),
            {reply, ok, State#state{wallets = Wallets}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
