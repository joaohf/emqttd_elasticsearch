%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-module(emqttd_elasticsearch_extend_tests).

-include_lib("eunit/include/eunit.hrl").

simple_extend_encode_message_test() ->
    CapModel = cap:model(),

    XmlPayload = sample_payload(CapModel),
    
    DataProps = [
                 {model, CapModel},
                 {vendor, <<"FakeVendor">>},
                 {node, <<"FakeNode">>},
                 {sensor, <<"FakeSensor">>}
                ],

    Json = emqttd_elasticsearch_extend:encode(DataProps, XmlPayload),

    ?assertEqual(true, jsx:is_json(Json)),

    ok.

sample_payload(Xsd) ->
    Params = [{valueName, "Dust1"},
              {value, "10"}],
    Info = [{category, "Other"},
            {event, "Sensor Test"},
            {urgency, "Immediate"},
            {severity, "Unknown"},
            {certainty, "Observed"},
            {parameter, Params}],
    
    cap:create(Xsd, [
                     {identifier, "Test"},
                     {sender, "Node1"},
                     {sent, "undefined"},
                     {info, Info}]).
