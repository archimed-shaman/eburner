%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% The gen_server worker, responsible for the named configuration. Reloads the
%%% configuration by the command and notifies all subscribers, sending the new
%%% vesion of the configuration.
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

-module(eburner_holder).
-author("Alexander Morozov aka ~ArchimeD~").

-behaviour(gen_server).

-include("eburner.hrl").

%% API
-export([start_link/2,
         get_config/1,
         subscribe/1,
         subscribe/2,
         unsubscribe/1,
         unsubscribe/2,
         reload/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, { name = <<>>,
                 config_getter,
                 current_config = [],
                 subscribers = [] }).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------

-spec start_link(Name, ConfigGetter) -> Result when
      Name :: binary(),
      ConfigGetter :: fun(() -> string()),
      Result :: {ok, Pid} | ignore | {error, Reason},
      Pid :: pid(),
      Reason :: term().


start_link(Name, ConfigGetter) when is_binary(Name), is_function(ConfigGetter)->
    ?DEBUG(io_lib:format("eburner: Starting new config server (~p)...", [Name])),
    ServerName = ?FILE_TO_NAME(Name),
    gen_server:start_link({local, ServerName}, ?MODULE, [{config_getter, ConfigGetter}, {name, Name}], []).



%%--------------------------------------------------------------------
%% @doc
%% Just returns the current config value, identified by ConfigName.
%% No subscription.
%% @end
%%--------------------------------------------------------------------

-spec get_config(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: {config, CurrentConfig},
      CurrentConfig :: string().


get_config(ConfigName) when is_binary(ConfigName) ->
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, get_config).



%%--------------------------------------------------------------------
%% @doc
%% Returns the current config value, identified by ConfigName and
%% subscribes the calling pid to receive the messages, when config
%% is reloaded. The messages looks like
%%  {config, Config :: string()}
%% @end
%%--------------------------------------------------------------------

-spec subscribe(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: {config, CurrentConfig},
      CurrentConfig :: string().


subscribe(ConfigName) when is_binary(ConfigName) ->
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, subscribe).



%%--------------------------------------------------------------------
%% @doc
%% Returns the current config value, identified by ConfigName and
%% subscribes the Pid to receive the messages, when config
%% is reloaded. The messages looks like
%%  {config, Config :: string()}
%% @end
%%--------------------------------------------------------------------

-spec subscribe(ConfigName, Pid) -> Result when
      ConfigName :: binary(),
      Pid :: pid(),
      Result :: {config, CurrentConfig},
      CurrentConfig :: string().


subscribe(ConfigName, Pid) when is_binary(ConfigName), is_pid(Pid) ->
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, {subscribe, Pid}).



%%--------------------------------------------------------------------
%% @doc
%% Unsubsribes the calling pid from the reload notifications of config,
%% identified by ConfigName
%% @end
%%--------------------------------------------------------------------

-spec unsubscribe(ConfigName) -> Result when
      ConfigName :: binary(),
      Result :: {config, CurrentConfig},
      CurrentConfig :: string().


unsubscribe(ConfigName) when is_binary(ConfigName) ->
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, unsubscribe).



%%--------------------------------------------------------------------
%% @doc
%% Unsubsribes the Pid from the reload notifications of config,
%% identified by ConfigName
%% @end
%%--------------------------------------------------------------------

-spec unsubscribe(ConfigName, Pid) -> Result when
      ConfigName :: binary(),
      Pid :: pid(),
      Result :: {config, CurrentConfig},
      CurrentConfig :: string().


unsubscribe(ConfigName, Pid) when is_binary(ConfigName), is_pid(Pid) ->
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, {unsubscribe, Pid}).



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
    ServerName = ?FILE_TO_NAME(ConfigName),
    gen_server:call(ServerName, reload).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------

-spec init(Args) -> Result when
      Args :: [proplists:property()],
      Result :: {ok, State} |
                {stop, {error, Reason}} |
                ignore,
      State :: #state{},
      Reason :: term().


init(Args) ->
    ConfigGetter =  proplists:get_value(config_getter, Args),
    Name =  proplists:get_value(name, Args),
    case try pfile:parse(ConfigGetter()) catch _:Exception -> {error, Exception} end of
        {ok, CurrentConfig} ->
            ?INFO(io_lib:format("eburner: Config has been loaded: ~p", [Name])),
            {ok, #state{config_getter = ConfigGetter, current_config = CurrentConfig, name = Name}};
        {error, Error} ->
            ?ERROR(io_lib:format("eburner: Unable start config server for ~p: ~p", [Name, Error])),
            {stop, {error, Error}}
    end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------

-type reply() :: ok
               | {config, CurrentConfig :: string()}
               | {error, Error :: term()}.

-spec handle_call(Request, From, State) -> Result when
      Request :: term(),
      From :: {Pid, Tag},
      Pid :: pid(),
      Tag :: term(),
      State :: #state{},
      Result :: {reply, Reply, NewState} |
                {noreply, NewState},
      Reply :: reply(),
      NewState :: #state{}.
      

%% handle get_config

handle_call(get_config, _From, #state{current_config = CurrentConfig} = State) ->
    {reply, {config, CurrentConfig}, State};

%% handle subscribe

handle_call(subscribe, {From, _}, #state{current_config = CurrentConfig,
                                         subscribers = Subscribers,
                                         name = Name} = State) ->
    ?TRACE(io_lib:format("eburner: New subscriber from ~p for ~p", [From, Name])),
    MonitorRef = erlang:monitor(process, From),
    {reply, {config, CurrentConfig}, State#state{subscribers = [{From, MonitorRef} | Subscribers]}};

%% handle {subscribe, From}

handle_call({subscribe, From}, _From, #state{current_config = CurrentConfig,
                                             subscribers = Subscribers,
                                             name = Name} = State) ->
    ?TRACE(io_lib:format("eburner: New subscriber from ~p for ~p", [From, Name])),
    MonitorRef = erlang:monitor(process, From),
    {reply, {config, CurrentConfig}, State#state{subscribers = [{From, MonitorRef} | Subscribers]}};

%% handle unsubscribe

handle_call(unsubscribe, {From, _}, #state{subscribers = Subscribers, name = Name} = State) ->
    NewSubscribers = lists:foldl(fun ({Pid, Ref}, Acc) when Pid == From ->
                                         ?TRACE(io_lib:format("eburner: ~p stoped subscription for ~p", [From, Name])),
                                         erlang:demonitor(Ref, [flush]),
                                         Acc;
                                     (Element, Acc) ->
                                         [Element | Acc]
                                 end, [], Subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}};

%% handle {unsubscribe, From}

handle_call({unsubscribe, From}, _From, #state{subscribers = Subscribers, name = Name} = State) ->
    NewSubscribers = lists:foldl(fun ({Pid, Ref}, Acc) when Pid == From ->
                                         ?TRACE(io_lib:format("eburner: ~p stoped subscription for ~p", [From, Name])),
                                         erlang:demonitor(Ref, [flush]),
                                         Acc;
                                     (Element, Acc) ->
                                         [Element | Acc]
                                 end, [], Subscribers),
    {reply, ok, State#state{subscribers = NewSubscribers}};

%% handle reload

handle_call(reload, _From, #state{config_getter = ConfigGetter,
                                  subscribers = Subscribers,
                                  name = Name,
                                  current_config = CurrentConfig} = State) ->
    case try pfile:parse(ConfigGetter()) catch _:Exception -> {error, Exception} end of
        {ok, Config} ->
            case equal_config(Config, CurrentConfig) of
                false ->
                    lists:map(fun({Pid, _}) -> Pid ! {config, Config} end, Subscribers),
                    ?INFO(io_lib:format("eburner: Config ~p has been reloaded.", [Name])),
                    {reply, ok, State#state{current_config = Config}};
                _ ->
                    ?INFO(io_lib:format("eburner: No changes in ~p, nothing to reload.", [Name])),
                    {reply, ok, State}
            end;
        {error, Error} ->
            ?WARNING(io_lib:format("eburner: Unable reload config ~p: ~p, no changes have been applied.", [Name, Error])),
            {reply, {error, Error}, State}
    end;

%% handle other

handle_call(Request, From, State) ->
    ?DEBUG(io_lib:format("eburner: Unknown call message from ~p: ~p", [From, Request])),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_cast(Request, State) -> Result when
      Request :: term(),
      State :: #state{},
      Result :: {noreply, NewState},
      NewState :: #state{}.


handle_cast(Request, State) ->
    ?DEBUG(io_lib:format("eburner: Unknown cast message: ~p", [Request])),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

-spec handle_info(Info, State) -> Result when
      Info :: timeout() | term(),
      State :: #state{},
      Result :: {noreply, NewState},
      NewState :: #state{}.


%% handle 'DOWN' signal (the subscribed process stopped)

handle_info({'DOWN', MonitorRef, process, DownPid, _}, #state{subscribers = Subscribers, name = Name} = State)->
    ?TRACE(io_lib:format("eburner: Pid ~p stopped, stopping subscription for ~p", [DownPid, Name])),
    NewSubscribers = lists:filter(fun({Pid, Ref}) when Pid == DownPid, Ref == MonitorRef -> false;
                                     (_) -> true
                                  end, Subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

%% handle other signals

handle_info(Info, State) ->
    ?DEBUG(io_lib:format("eburner: Unknown info message: ~p", [Info])),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------

-spec terminate(Reason, State) -> Result when
      Reason :: normal |  shutdown | {shutdown, term()} | term(),
      State :: #state{},
      Result :: term().


terminate(_Reason, _State) ->
    ok.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------

-spec code_change(OldVersion, State, Extra) -> Result when
      OldVersion :: term() | {down, term()},
      State :: #state{},
      Extra :: term(),
      Result :: {ok, NewState} |
                {error, Reason},
      NewState :: #state{},
      Reason :: term().


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compares two lists, to determine, if there are any changes in config
%% @end
%%--------------------------------------------------------------------

-spec equal_config(Config1, Config2) -> Result when
      Config1 :: list(),
      Config2 :: list(),
      Result :: boolean().


equal_config([], [])                   -> true;
equal_config(_, [])                    -> false;
equal_config([], _)                    -> false;
equal_config([A|_], [B|_]) when A /= B -> false;
equal_config([_|A], [_|B])             -> equal_config(A, B).
