%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 eMQTT.IO, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emqttd elastic search plugin.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emqttd_elasticsearch).

-include_lib("erlastic_search/include/erlastic_search.hrl").
-include_lib("emqttd/include/emqttd.hrl").
-include("emqttd_elasticsearch.hrl").

-export([onload/1, onunload/0, config_params/1]).

%% Hooks functions
-export([on_message_publish/2]).

%% Called to config Elastic Search params
config_params(ElasticEnv) ->
    #erls_params{
       host = proplists:get_value(host, ElasticEnv, <<"127.0.0.1">>),
       port = proplists:get_value(port, ElasticEnv, 9200),
       http_client_options = proplists:get_value(http_client_options, ElasticEnv, [])
      }.
    
%% Called when the plugin application start
onload(Env) ->
    #mqtt_elasticsearch{decoder=Decoder} = Env,
    {Mod, _} = Decoder,
    
    DecoderData = [{model, Mod:model()}],

    NewEnv = Env#mqtt_elasticsearch{decoder_data=DecoderData},

    emqttd_broker:hook('message.publish', {?MODULE, on_message_publish},
                       {?MODULE, on_message_publish, [NewEnv]}).

    

%% transform and/or send message and return
on_message_publish(Message = #mqtt_message{topic = <<"Test/", _/binary>>}, Env) ->
    message_from(test, Message, Env),
    Message;
on_message_publish(Message, _Env) ->
    Message.

%% Called when the plugin application stop
onunload() ->
    emqttd_broker:unhook('message.publish', {?MODULE, on_message_publish}).


message_from(PayloadType, Message, Env) ->
    #mqtt_elasticsearch{erls_params=Params,
                        index=Index,
                        type=Type,
                        decoder=Decoder, 
                        decoder_data=DecoderData} = Env,
    #mqtt_message{topic=Topic, payload=Payload} = Message,

    NewDecoderData = lists:flatten([
                                    get_topic_info(Topic),
                                    DecoderData,
                                    {payload_type, PayloadType}
                                   ]),

    % Transform message (XML -> Json)
    Doc = 
    case Decoder of
        [] ->
            Payload;
        {Mod, F} ->
            Mod:F(NewDecoderData, Payload)
    end,

    % Send to ES
    {ok, _Result} =
        erlastic_search:index_doc(Params, Index, Type, Doc),

    % Return the message
    Message.

%% @doc Return Vendor, Node, Sensor from Topic
-spec get_topic_info(Topic :: binary()) ->
                            {Vendor :: binary(),
                             Node :: binary(),
                             Sensor :: binary()}.
get_topic_info(Topic) ->
    [Vendor, Node, Sensor] = get_topic_triple(Topic),
    
    [{vendor, Vendor}, {node, Node}, {sensor, Sensor}].

get_topic_triple(Topic) ->
    case get_topic_triple1(emqttd_topic:triples(Topic), []) of
        [] ->
            {undefined, undefined, undefined};
        Triple ->
            Triple
    end.

get_topic_triple1([], Acc) ->
    lists:reverse(Acc);
get_topic_triple1([{_Root, Name, _FullName} | T], Acc) ->
    get_topic_triple1(T, [Name|Acc]).
    
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_topic_triple_test() ->
    Topic = <<"Vendor/Node/Sensor">>,
    Result = get_topic_triple(Topic),

    ?assertEqual([<<"Vendor">>, <<"Node">>, <<"Sensor">>], Result).

get_topic_info_test() ->
    Topic = <<"Vendor/Node/Sensor">>,
    Result = get_topic_info(Topic),

    ?assertMatch([{vendor, <<"Vendor">>}, {node, _}, {sensor, _}], Result).

-endif.
