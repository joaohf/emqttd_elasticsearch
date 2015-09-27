%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(emqttd_elasticsearch_extend).

-export([model/0, encode/2]).

-type payload() :: term().

-record(default_message, {
          site :: binary(),
          vendor :: binary(),
          node :: binary(),
          sensor :: binary(),
          payload :: payload()
         }).

%% @doc Return the model used to encode/decode
model() ->
    cap:model().
               
%% @doc Creates a new message with payload and a common header
-spec encode(DataProps :: proplists:property(), SensorPayload :: binary()) -> binary().
encode(DataProps, SensorPayload) ->    
    % get vendor, node and sensor
    Vendor = proplists:get_value(vendor, DataProps),
    Node = proplists:get_value(node, DataProps),
    Sensor = proplists:get_value(sensor, DataProps),
    PayloadType = proplists:get_value(payload_type, DataProps),

    % lookup site
    Site = emqttd_elasticsearch_store:lookup_site(Vendor, Node),
    
    % get XML CAP Model
    Payload =
        case PayloadType of
            test ->
                Model = proplists:get_value(model, DataProps),
                cap:convert_to_term(Model, SensorPayload)
        end,
            
    Msg = create_message({Site, Vendor, Node, Sensor, Payload}),

    jsx:encode(Msg).

%% @doc Add default header
add_header(Msg, {Site, Vendor, Node, Sensor, _}) ->
    Msg#default_message{site = Site,
                        vendor = Vendor,
                        node = Node,
                        sensor = Sensor}.

%% @doc Creates a new message
create_message({_, _, _, _, Payload} = MsgInfo) ->
    Msg = #default_message{payload = Payload},
    Msg1 = add_header(Msg, MsgInfo),

    to_proplist(Msg1).

%% @doc Convert to proplist
to_proplist(#default_message{site=Site,
                             node=Node,
                             vendor=Vendor,
                             sensor=Sensor,
                             payload=Payload}) ->
    [{<<"site">>, Site},
     {<<"vendor">>, Vendor},
     {<<"node">>, Node},
     {<<"sensor">>, Sensor},
     {<<"payload">>, Payload}].
