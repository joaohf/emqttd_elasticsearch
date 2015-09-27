%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et

-record(mqtt_elasticsearch, {
          erls_params,
          decoder ::  module(),
          decoder_data :: term(),
          index :: binary(),
          type :: binary()
         }).

