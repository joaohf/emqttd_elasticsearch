-module(emqttd_elasticsearch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [worker(emqttd_elasticsearch_store)]}
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

worker(I) ->
    {I, {I, start_link, []}, permanent, infinity, worker, [I]}.
