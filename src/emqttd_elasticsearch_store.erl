%%%-------------------------------------------------------------------
%%% @author jfreitas <jfreitas@jfreitas.local>
%%% @copyright (C) 2015, jfreitas
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2015 by jfreitas <jfreitas@jfreitas.local>
%%%-------------------------------------------------------------------
-module(emqttd_elasticsearch_store).

-behaviour(gen_server).

%% API
-export([start_link/0,
        lookup_site/2,
        register_site/3,
        update_site/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Lookup Site related to Vendor and Node
-spec lookup_site(Vendor :: binary(), Node :: binary()) -> Site :: binary | undefined.
lookup_site(Vendor, Node) ->
    case ets:lookup(?MODULE, {Vendor, Node}) of
        [] ->
            undefined;
        [{{Vendor, Node}, Site}] ->
            Site
    end.

%% @doc Register Vendor and Node with a Site
-spec register_site(Site :: binary(), Vendor :: binary(), Node :: binary()) -> ok.
register_site(Site, Vendor, Node) ->
    gen_server:call(?MODULE, {register_site, {Site, Vendor, Node}}).

%% @doc Update a registered Site
-spec update_site(Site :: binary(), Vendor :: binary(), Node :: binary()) -> ok.
update_site(Site, Vendor, Node) ->
    gen_server:call(?MODULE, {update_site, {Site, Vendor, Node}}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register_site, {Site, Vendor, Node}}, _From, State) ->
    true = ets:insert(?MODULE, {{Vendor, Node}, Site}),
    {reply, ok, State};
handle_call({update_site, {Site, Vendor, Node}}, _From, State) ->
    true = ets:insert(?MODULE, {{Vendor, Node}, Site}),
    {reply, ok, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
