%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% The main supervisor. Starts only the logger worker by default,
%%% config servers should be started manually as children.
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

-module(eburner_sup).
-author("Alexander Morozov aka ~ArchimeD~").

-behaviour(supervisor).

-include("eburner.hrl").

%% API
-export([start_link/0,
         start_holder/2,
         stop_holder/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD_ID(Name), {eburner_holder, Name}).



%% ===================================================================
%% API functions
%% ===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%%--------------------------------------------------------------------
%% @doc
%% Starts the new config server, identified by ConfigName and puts into
%% the supervisor tree. ConfigGetter is a functionm which returns the
%% config body.
%% @end
%%--------------------------------------------------------------------

-spec(start_holder(ConfigName :: binary(), ConfigGetter :: fun(() -> string())) ->
             supervisor:startchild_ret()).

start_holder(ConfigName, ConfigGetter) when is_binary(ConfigName), is_function(ConfigGetter) ->
    HolderSpec = {?CHILD_ID(?FILE_TO_NAME(ConfigName)),
                  {eburner_holder, start_link, [ConfigName, ConfigGetter]},
                  permanent, 3000, worker, [eburner_holder]},
    supervisor:start_child(?MODULE, HolderSpec).



%%--------------------------------------------------------------------
%% @doc
%% Stops the config server attached to the supervisor tree and identified
%% by ConfigName
%% @end
%%--------------------------------------------------------------------

-spec(stop_holder(ConfigName :: binary()) ->
             ok |
             {error, Error :: running | restarting | not_found | simple_one_for_one}).

stop_holder(ConfigName) when is_binary(ConfigName) ->
    Id = ?CHILD_ID(?FILE_TO_NAME(ConfigName)),
    supervisor:terminate_child(?MODULE, Id),
    supervisor:delete_child(?MODULE, Id).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------

-spec(init(Args :: term()) -> {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                                                 MaxR :: non_neg_integer(),
                                                 MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}
                              } |
                              ignore).

init([]) ->
    LoggerSpec = {eburner_logger, {eburner_logger, start_link, []},
                  permanent, 5000, worker, [eburner_logger]},
    {ok, { {one_for_one, 5, 10}, [LoggerSpec]} }.
