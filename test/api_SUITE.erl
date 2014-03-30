%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% Tests for the API of the main module eburner.erl
%%% @end
%%%
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014 Alexander Morozov
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
%%%-------------------------------------------------------------------

-module(api_SUITE).
-author("Alexander Morozov aka ~ArchimeD~").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").



%%--------------------------------------------------------------------
%% @doc
%% A "fixture" to run tests
%% @end
%%--------------------------------------------------------------------

setup_test_()->
    Suite = { foreach, local, fun setup/0, fun cleanup/1, tests() },
    Suite.



%%--------------------------------------------------------------------
%% @doc
%% Initializes tests
%% @end
%%--------------------------------------------------------------------

setup()->
    application:start(eburner),
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Cleans up the tests
%% @end
%%--------------------------------------------------------------------

cleanup(_) ->
    application:stop(eburner),
    ok.



%%--------------------------------------------------------------------
%% @doc
%% Returns the test set
%% @end
%%--------------------------------------------------------------------

tests() ->
    [
     {"Simple config", ?_test(simple_config())},
     {"Subscription", ?_test(subscription())},
     {"Subscription with failed reload", ?_test(subscription_failed_reload())},
     {"Reload of the untouched config", ?_test(unchanged_config())}
    ].



%%--------------------------------------------------------------------
%% @doc
%% Tests the simple configuration, get_config should return the expected
%% config body.
%% @end
%%--------------------------------------------------------------------

simple_config() ->
    ?assertEqual(application:ensure_started(eburner), ok),
    ConfigName = list_to_binary("simple.cfg"),
    Config =
        "[general]
        Key1 = \"value1\"
        key2 = \"value2\"
        [primary]
        primary1 = \"value1\"",
    {ok, _} = eburner:load(ConfigName, fun() -> Config end),
    {ok, ParsedConfig} = etoml:parse(Config),
    ?assertEqual({config, ParsedConfig}, eburner:get_config(ConfigName)).



%%--------------------------------------------------------------------
%% @doc
%% Tests the reload of the config.
%% @end
%%--------------------------------------------------------------------

subscription() ->
    ?assertEqual(application:ensure_started(eburner), ok),
    ConfigName = list_to_binary("subscription.cfg"),

    Config1 = "[general]
                key1 = \"value1\"
                key2 = 123",

    Config2 = "[general]
                key1 = \"value2\"
                key2 = 321",

    TabId = ets:new(reloadableConfig, []),
    ets:insert(TabId, {config, Config1}),

    Getter = fun() ->
                     [{config, Value} | _] = ets:lookup(TabId, config),
                     Value
             end,

    {ok, ConfigETOML1} = etoml:parse(Getter()),
    {ok, _} = eburner:load(ConfigName, Getter),
    ?assertEqual({config, ConfigETOML1}, eburner:subscribe(ConfigName)),

    ets:insert(TabId, {config, Config2}),
    {ok, ConfigETOML2} = etoml:parse(Getter()),
    ok = eburner:reload(ConfigName),
    receive
        Message -> ?assertEqual({config, ConfigETOML2}, Message)
    after 1000 -> ?assert(true)
    end.



%%--------------------------------------------------------------------
%% @doc
%% Tests the reload of the config, when reload shold be failed
%% @end
%%--------------------------------------------------------------------

subscription_failed_reload() ->
    ?assertEqual(application:ensure_started(eburner), ok),
    ConfigName = list_to_binary("subscription_failed_reload.cfg"),

    Config1 = "[general]
                key1 = \"value1\"
                key2 = 123",

    Config2 = "[general]\n key1 = value2\n key2 = 321",

    TabId = ets:new(reloadableConfig, []),
    ets:insert(TabId, {config, Config1}),

    Getter = fun() ->
                     [{config, Value} | _] = ets:lookup(TabId, config),
                     Value
             end,

    {ok, ConfigETOML1} = etoml:parse(Getter()),
    {ok, _} = eburner:load(ConfigName, Getter),
    ?assertEqual({config, ConfigETOML1}, eburner:subscribe(ConfigName)),

    ets:insert(TabId, {config, Config2}),
    {error, _} = etoml:parse(Getter()),
    {error, _} = eburner:reload(ConfigName),
    receive
        _ -> ?assert(true)
    after 1000 -> ?assertEqual({config, ConfigETOML1}, eburner:get_config(ConfigName))
    end.



%%--------------------------------------------------------------------
%% @doc
%% Tests the reload of the config without any changes. No notification
%% should be sent.
%% @end
%%--------------------------------------------------------------------

unchanged_config() ->
    ?assertEqual(application:ensure_started(eburner), ok),
    ConfigName = list_to_binary("subscription_failed_reload.cfg"),

    Config1 = "[general]\n key1 = \"value1\" \n key2 = 123",

    Config2 = "[general]\n key1 = \"value1\" \n key2 = 123",

    TabId = ets:new(reloadableConfig, []),
    ets:insert(TabId, {config, Config1}),

    Getter = fun() ->
                     [{config, Value} | _] = ets:lookup(TabId, config),
                     Value
             end,

    {ok, ConfigETOML1} = etoml:parse(Getter()),
    {ok, _} = eburner:load(ConfigName, Getter),
    ?assertEqual({config, ConfigETOML1}, eburner:subscribe(ConfigName)),

    ets:insert(TabId, {config, Config2}),
    {ok, _} = etoml:parse(Getter()),
    ok = eburner:reload(ConfigName),
    receive
        _ -> ?assert(true)
    after 1000 -> ?assertEqual({config, ConfigETOML1}, eburner:get_config(ConfigName))
    end.