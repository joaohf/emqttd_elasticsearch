-module(emqttd_elasticsearch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("emqttd_elasticsearch.hrl").


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqttd_elasticsearch_sup:start_link(),
    {ok, ElasticEnv} = application:get_env(emqttd_elasticsearch, elasticsearch),
    {ok, Env} = application:get_env(emqttd_elasticsearch, decoder),
    Config = create_config(ElasticEnv, Env),
    emqttd_elasticsearch:onload(Config),
    {ok, Sup}.

stop(_State) ->
    emqttd_elasticsearch:onunload().


%% Private

create_config(ElasticEnv, Env) ->
    ElasticParams = emqttd_elasticsearch:config_params(ElasticEnv),
    
    #mqtt_elasticsearch{
       erls_params = ElasticParams,
       decoder = proplists:get_value(message_decode, Env, []),
       index = proplists:get_value(index, ElasticEnv),
       type = proplists:get_value(type, ElasticEnv)
      }.
