-module(wallet_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    create_wallet/0,
    get_wallet/1
]).
-export([update_balance/2, get_balance/1]).

-record(state, {wallets = #{}}).
create_wallet() ->
    gen_server:call(?MODULE, create_wallet).

get_wallet(PublicKey) ->
    gen_server:call(?MODULE, {get_wallet, PublicKey}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_balance(PublicKey, Amount) ->
    gen_server:call(?MODULE, {update_balance, PublicKey, Amount}).

get_balance(PublicKey) ->
    gen_server:call(?MODULE, {get_balance, PublicKey}).

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({update_balance, PublicKey, Amount}, _From, State) ->
    Wallets = State#state.wallets,
    case maps:get(PublicKey, Wallets, undefined) of
        undefined ->
            NewWallet = {wallet, PublicKey, PublicKey, 0},
            NewWallets = maps:put(PublicKey, NewWallet, Wallets),
            {reply, ok, State#state{wallets = NewWallets}};
        {wallet, Pub, _, Balance} ->
            NewBalance = Balance + Amount,
            NewWallet = {wallet, Pub, Pub, NewBalance},
            NewWallets = maps:put(PublicKey, NewWallet, Wallets),
            {reply, ok, State#state{wallets = NewWallets}}
    end;
handle_call({get_balance, PublicKey}, _From, State) ->
    Wallets = State#state.wallets,
    case maps:get(PublicKey, Wallets, undefined) of
        undefined -> {reply, {error, not_found}, State};
        {wallet, _, _, Balance} -> {reply, {ok, Balance}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
