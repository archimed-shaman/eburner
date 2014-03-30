%%%-------------------------------------------------------------------
%%% @author Alexander Morozov aka ~ArchimeD~
%%% @copyright 2014, Alexander Morozov
%%% @doc
%%% The gen_server worker, responsible for logging. The default logger function
%%% does nothing, you can set your owner function to log with lager, alogger, etc.
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

-module(eburner_logger).
-author("Alexander Morozov aka ~ArchimeD~").

-behaviour(gen_server).

%% API
-export([set_logger/1,
         start_link/0,
         start_link/1,
         debug/1,
         trace/1,
         info/1,
         warning/1,
         error/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { log_fun = fun(_, _) -> ok end }).



%%%===================================================================
%%% API
%%%===================================================================



%%--------------------------------------------------------------------
%% @doc
%% Starts the server without parameters
%% @end
%%--------------------------------------------------------------------

-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server with a Logger function
%% @end
%%--------------------------------------------------------------------

-type log_level() :: debug | trace | info | warning | error.

-spec(start_link(Logger :: fun((Level :: log_level(), Msg :: string()) -> ok)) ->
             {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(Logger) when is_function(Logger) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{log_fun, Logger}], []).



%%--------------------------------------------------------------------
%% @doc
%% Log with the DEBUG level
%% @end
%%--------------------------------------------------------------------

-spec (debug(Msg :: string()) -> Reply :: ok).

debug(Msg) ->
    gen_server:call(?SERVER, {debug, Msg}).



%%--------------------------------------------------------------------
%% @doc
%% Log with the TRACE level
%% @end
%%--------------------------------------------------------------------

-spec (trace(Msg :: string()) -> Reply :: ok).

trace(Msg) ->
    gen_server:call(?SERVER, {trace, Msg}).



%%--------------------------------------------------------------------
%% @doc
%% Log with the INFO level
%% @end
%%--------------------------------------------------------------------

-spec (info(Msg :: string()) -> Reply :: ok).

info(Msg) ->
    gen_server:call(?SERVER, {info, Msg}).



%%--------------------------------------------------------------------
%% @doc
%% Log with the WARNING level
%% @end
%%--------------------------------------------------------------------

-spec (warning(Msg :: string()) -> Reply :: ok).

warning(Msg) ->
    gen_server:call(?SERVER, {warning, Msg}).



%%--------------------------------------------------------------------
%% @doc
%% Log with the ERROR level
%% @end
%%--------------------------------------------------------------------

-spec (error(Msg :: string()) -> Reply :: ok).

error(Msg) ->
    gen_server:call(?SERVER, {error, Msg}).



%%--------------------------------------------------------------------
%% @doc
%% Sets the logger function
%% @end
%%--------------------------------------------------------------------

-spec (set_logger(Logger :: fun((Level :: log_level(), Msg :: string()) -> ok)) ->
              Reply :: ok).

set_logger(Logger) when is_function(Logger) ->
    gen_server:call(?SERVER, {set_logger, Logger}).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------

-spec(init(Args :: term()) -> {ok, State :: #state{}}).

init([]) ->
    {ok, #state{}};

init(Args) ->
    Logger = proplists:get_value(log_fun, Args),
    {ok, #state{log_fun = Logger}}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------

-type sync_request() :: {set_logger, Logger :: fun((Level :: log_level(), Msg :: string()) -> ok)}
                      | {Type :: log_level(), Msg :: string()}.

-spec(handle_call(Request :: sync_request(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
             {reply, ok, NewState :: #state{}}).

%% handle {set_logger, Logger}

handle_call({set_logger, Logger}, _from, #state{} = State) when is_function(Logger) ->
    {reply, ok, State#state{log_fun = Logger}};

%% handle {LogLevel, Msg}

handle_call({Type, Msg}, _From, #state{log_fun = Logger} = State) when Type == debug;
                                                                       Type == trace;
                                                                       Type == info;
                                                                       Type == warning;
                                                                       Type == error ->
    Logger(Type, Msg),
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

-spec(handle_cast(Request :: term(), State :: #state{}) ->
             {noreply, NewState :: #state{}}).

handle_cast(_Request, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
             {noreply, NewState :: #state{}}).

handle_info(_Info, State) ->
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).

terminate(_Reason, _State) ->
    ok.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
             {ok, NewState :: #state{}} | {error, Reason :: term()}).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================
