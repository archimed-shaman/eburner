eburner
=======

Simple configuration server for etoml syntax.

Usage:

 - `load/2` Loads the new the config with the help of Getter and starts the new gen_server config holder, identified by the ConfigName.
 - `reload/1` Reloads the config, identified be ConfigName.
 - `unload/1` Unloads the config server, identified by the ConfigName.
 - `set_logger/1` Sets the logger function fun(Level, Msg).
 - `get_config/1` Just returns the current config value, identified by ConfigName.
 - `subscribe/1` Returns the current config value, identified by ConfigName and subscribes the calling pid to receive the messages, when config is reloaded.
 - `subscribe/2` Returns the current config value, identified by ConfigName and subscribes the Pid to receive the messages, when config is reloaded.
 - `unsubscribe/1` Unsubsribes the calling pid from the reload notifications of config, identified by ConfigName.
 - `unsubscribe/2` Unsubsribes the Pid from the reload notifications of config, identified by ConfigName.

Example:

```erlang
Erlang R16B03-1 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.4  (abort with ^G)
1> code:add_path("../deps/etoml/ebin").
true
2>
2> Name = "./test.cfg".
"./test.cfg"
3> Config1 = "[general]\n key1 = \"value1\" \n key2 = 123".
"[general]\n key1 = \"value1\" \n key2 = 123"
4> Config2 = "[general]\n key1 = \"value2\" \n key2 = 321".
"[general]\n key1 = \"value2\" \n key2 = 321"
5> TabId = ets:new(reloadableConfig, []).
16400
6> ets:insert(TabId, {config, Config1}).
true
6>
7> Loader = fun() -> [{config, Value} | _] = ets:lookup(TabId, config), Value end.
#Fun<erl_eval.20.80484245>
8>
8> application:start(eburner).
ok
9>
9> eburner:set_logger(fun(_, Msg) -> io:format("~s~n", [Msg]) end).
ok
10>
10> eburner:load(list_to_binary(Name), Loader).
eburner: Starting new config server (<<"./test.cfg">>)...
eburner: Config has been loaded: <<"./test.cfg">>
{ok,<0.49.0>}
11>
11> eburner:subscribe(list_to_binary(Name)).
eburner: New subscriber from <0.33.0> for <<"./test.cfg">>
{config,[{<<"general">>,
          [{<<"key2">>,123},{<<"key1">>,<<"value1">>}]}]}
12>
12> ets:insert(TabId, {config, Config2}).
true
13>
13> eburner:reload(list_to_binary(Name)).
eburner: Config <<"./test.cfg">> has been reloaded.
ok
14>
14> flush().
Shell got {config,[{<<"general">>,
                    [{<<"key2">>,321},{<<"key1">>,<<"value2">>}]}]}
ok
15>
15> eburner:unsubscribe(list_to_binary(Name)).
eburner: <0.33.0> stoped subscription for <<"./test.cfg">>
ok
16>
16> eburner:reload(list_to_binary(Name)).
eburner: No changes in <<"./test.cfg">>, nothing to reload.
ok
17>
17> flush().
ok
18>
18> spawn(fun() -> eburner:subscribe(list_to_binary(Name)), timer:sleep(3000) end).
eburner: New subscriber from <0.58.0> for <<"./test.cfg">>
<0.58.0>
19>
19> eburner: Pid <0.58.0> stopped, stopping subscription for <<"./test.cfg">>
```
