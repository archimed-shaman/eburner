%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% The common API inteface file
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

-module(eburner).
-author("Alexander Morozov aka ~ArchimeD~").

%% API
-export([load/2,
         unload/1,
         set_logger/1,
         get_config/1,
         subscribe/1,
         subscribe/2,
         unsubscribe/1,
         unsubscribe/2,
         reload/1]).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Loads the new the config with the help of Getter and starts the new
%% gen_server config holder, identified by the ConfigName
%% @end
%%--------------------------------------------------------------------

-spec load(ConfigName, Getter) -> Result when
      ConfigName :: binary(),
      Getter :: fun(() -> string()),
      Result :: supervisor:startchild_ret().


load(ConfigName, Getter) when is_binary(ConfigName), is_function(Getter) ->
    eburner_sup:start_holder(ConfigName, Getter).



%%--------------------------------------------------------------------
%% @doc
%% Unloads the config server, identified by the ConfigName
%% @end
%%--------------------------------------------------------------------

-spec unload(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: 'ok' | {'error', Error},
      Error :: 'running' |
               'restarting' |
               'not_found' |
               'simple_one_for_one'.


unload(ConfigName) when is_binary(ConfigName) ->
    eburner_sup:stop_holder(ConfigName).



%%--------------------------------------------------------------------
%% @doc
%% Sets the logger function fun(Level, Msg).
%% Level must take one of the following values:
%%  - debug
%%  - trace
%%  - info
%%  - warning
%%  - error
%% Msg is the string to be logged.
%% @end
%%--------------------------------------------------------------------

-spec set_logger(Logger) -> Result when
      Logger :: fun((Level, Msg) -> ok),
      Result :: ok,
      Level :: debug | trace | info | warning | error,
      Msg :: string().


set_logger(Logger) when is_function(Logger) ->
    eburner_logger:set_logger(Logger).



%%--------------------------------------------------------------------
%% @doc
%% Just returns the current config value, identified by ConfigName.
%% No subscription.
%% @end
%%--------------------------------------------------------------------

-spec get_config(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: {config, Name, CurrentConfig},
      Name :: binary(),
      CurrentConfig :: string().


get_config(ConfigName) when is_binary(ConfigName) ->
    eburner_holder:get_config(ConfigName).



%%--------------------------------------------------------------------
%% @doc
%% Returns the current config value, identified by ConfigName and
%% subscribes the calling pid to receive the messages, when config
%% is reloaded. The messages looks like
%%  {config, Name :: binary(), Config :: string()}
%% @end
%%--------------------------------------------------------------------

-spec subscribe(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: {config, Name, CurrentConfig},
      Name :: binary(),
      CurrentConfig :: string().


subscribe(ConfigName) when is_binary(ConfigName) ->
    eburner_holder:subscribe(ConfigName).



%%--------------------------------------------------------------------
%% @doc
%% Returns the current config value, identified by ConfigName and
%% subscribes the Pid to receive the messages, when config
%% is reloaded. The messages looks like
%%  {config, Name :: binary(), Config :: string()}
%% @end
%%--------------------------------------------------------------------

-spec subscribe(ConfigName, Pid) -> Result when
      ConfigName :: binary(),
      Pid :: pid(),
      Result :: {config, Name, CurrentConfig},
      Name :: binary(),
      CurrentConfig :: string().


subscribe(ConfigName, Pid) when is_binary(ConfigName), is_pid(Pid) ->
    eburner_holder:subscribe(ConfigName, Pid).



%%--------------------------------------------------------------------
%% @doc
%% Unsubsribes the calling pid from the reload notifications of config,
%% identified by ConfigName
%% @end
%%--------------------------------------------------------------------

-spec unsubscribe(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: ok.


unsubscribe(ConfigName) when is_binary(ConfigName) ->
    eburner_holder:unsubscribe(ConfigName).



%%--------------------------------------------------------------------
%% @doc
%% Unsubsribes the Pid from the reload notifications of config,
%% identified by ConfigName
%% @end
%%--------------------------------------------------------------------

-spec unsubscribe(ConfigName, Pid) -> Result when
      ConfigName :: binary(),
      Pid :: pid(),
      Result :: ok.


unsubscribe(ConfigName, Pid) when is_binary(ConfigName), is_pid(Pid) ->
    eburner_holder:unsubscribe(ConfigName, Pid).



%%--------------------------------------------------------------------
%% @doc
%% Reloads the config, identified be ConfigName
%% @end
%%--------------------------------------------------------------------

-spec reload(ConfgigName) -> Result when
      ConfgigName :: binary(),
      Result :: ok | {error, Error},
      Error :: term().


reload(ConfigName) when is_binary(ConfigName) ->
    eburner_holder:reload(ConfigName).

