%% MIT License
%%
%% Copyright (c) 2021 Maria Scott <maria-12648430@hnc-agency.org>
%% Copyright (c) 2021 Jan Uhlig <juhlig@hnc-agency.org>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(gen_agent).
-behavior(gen_statem).

-export([start/3, start/4]).
-export([start_link/3, start_link/4]).
-export([start_monitor/3, start_monitor/4]).
-export([perform/1]).
-export([wait/1, wait/2]).
-export([call/2, call/3]).
-export([cast/2]).
-export([reply/2]).
-export([stop/1, stop/3]).

-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

-type server_ref() :: gen_statem:server_ref().
-type start_opt() :: gen_statem:start_opt().
-type start_ret() :: {ok, pid()} | ignore | {error, term()}.
-type start_mon_ret() :: {ok, {pid(), reference()}} | ignore | {error, term()}.
-type from() :: gen_statem:from().
-type agent_state() :: idle | sleeping | executing.

-callback init(Args) -> Result
	when Args :: term(),
	     Result :: {ok, Data}
		     | ignore
		     | {stop, Reason},
	     Data :: term(),
	     Reason :: term().

-callback sleep_time(Attempt, Data0) -> Result
	when Attempt :: non_neg_integer(),
	     Data0 :: term(),
	     Result :: {ok, Time}
		     | {ok, Time, Data1}
		     | stop
		     | {stop, Reason}
		     | {stop, Reason, Data1},
	     Time :: non_neg_integer(),
	     Data1 :: term(),
	     Reason :: term().

-callback handle_execute(Data0) -> Result
	when Data0 :: term(),
	     Result :: done
		     | {done, Data1}
		     | continue
		     | {continue, Data1}
		     | {continue, Data1, Timeout}
		     | retry
		     | {retry, Data1}
		     | repeat
		     | {repeat, Data1}
		     | stop
		     | {stop, Reason}
		     | {stop, Reason, Data1},
	     Data1 :: term(),
	     Timeout :: timeout(),
	     Reason :: term().

-callback handle_event(EventType, Message, State, Data0) -> Result
	when EventType :: {call, From} | cast | info | timeout,
	     From :: from(),
	     Message :: term(),
	     State :: executing,
	     Data0 :: term(),
	     Result :: done
		     | {done, Data1}
		     | continue
		     | {continue, Data1}
		     | {continue, Data1, Timeout}
		     | repeat
		     | {repeat, Data1}
		     | retry
		     | {retry, Data1}
		     | stop
		     | {stop, Reason}
		     | {stop, Reason, Data1},
	     Data1 :: term(),
	     Timeout :: timeout(),
	     Reason :: term();
    (EventType, Message, State, Data0) -> Result
	when EventType :: {call, From} | cast | info | timeout,
	     From :: from(),
	     Message :: term(),
	     State :: idle | sleeping,
	     Data0 :: term(),
	     Result::  continue
		     | {continue, Data1}
		     | {continue, Data1, Timeout}
		     | stop
		     | {stop, Reason}
		     | {stop, Reason, Data1},
	     Data1 :: term(),
	     Timeout :: timeout(),
	     Reason :: term().

-callback terminate(Reason, State, Data0) -> Ignored
	when Reason :: term(),
	     State :: agent_state(),
	     Data0 :: term(),
	     Ignored :: term().
-optional_callbacks([terminate/3]).

-callback code_change(OldVsn, State0, Data0, Extra) -> Result
	when OldVsn :: term() | {down, term()},
	     State0 :: agent_state(),
	     Data0 :: term(),
	     Extra :: term(),
	     Result :: {ok, State1, Data1} | Reason,
	     State1 :: agent_state(),
	     Data1 :: term(),
	     Reason :: term().
-optional_callbacks([code_change/4]).

-define(TAG_I(Msg), {'$gen_agent', internal, Msg}).

-record(data, {
		cb_mod :: module(),
		cb_state :: term(),
		attempt=0 :: non_neg_integer(),
		timer :: undefined | reference()
	}).

-spec start(Module, Args, Opts) -> Result
	when Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_ret().
start(Module, Args, Opts) ->
	gen_statem:start(?MODULE, {Module, Args}, Opts).

-spec start(ServerName, Module, Args, Opts) -> Result
	when ServerName :: server_ref(),
	     Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_ret().
start(ServerName, Module, Args, Opts) ->
	gen_statem:start(ServerName, ?MODULE, {Module, Args}, Opts).

-spec start_link(Module, Args, Opts) -> Result
	when Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_ret().
start_link(Module, Args, Opts) ->
	gen_statem:start_link(?MODULE, {Module, Args}, Opts).

-spec start_link(ServerName, Module, Args, Opts) -> Result
	when ServerName :: server_ref(),
	     Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_ret().
start_link(ServerName, Module, Args, Opts) ->
	gen_statem:start_link(ServerName, ?MODULE, {Module, Args}, Opts).

-spec start_monitor(Module, Args, Opts) -> Result
	when Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_mon_ret().
start_monitor(Module, Args, Opts) ->
	gen_statem:start_monitor(?MODULE, {Module, Args}, Opts).

-spec start_monitor(ServerName, Module, Args, Opts) -> Result
	when ServerName :: server_ref(),
	     Module :: module(),
	     Args :: term(),
	     Opts :: [start_opt()],
	     Result :: start_mon_ret().
start_monitor(ServerName, Module, Args, Opts) ->
	gen_statem:start_monitor(ServerName, ?MODULE, {Module, Args}, Opts).

-spec perform(ServerRef) -> Result
	when ServerRef :: server_ref(),
	     Result :: ok | {error, Reason},
	     Reason :: term().
perform(ServerRef) ->
	call(ServerRef, ?TAG_I(perform)).

-spec wait(ServerRef) -> ok
	when ServerRef :: server_ref().
wait(ServerRef) ->
	wait(ServerRef, infinity).

-spec wait(ServerRef, Timeout) -> ok
	when ServerRef :: server_ref(),
	     Timeout :: timeout().
wait(ServerRef, Timeout) ->
	call(ServerRef, ?TAG_I(wait), Timeout).

-spec call(ServerRef, Message) -> Reply
	when ServerRef :: server_ref(),
	     Message :: term(),
	     Reply :: term().
call(ServerRef, Msg) ->
	gen_statem:call(ServerRef, Msg).

-spec call(ServerRef, Message, Timeout) -> Reply
	when ServerRef :: server_ref(),
	     Message :: term(),
	     Timeout :: timeout(),
	     Reply :: term().
call(ServerRef, Msg, Timeout) ->
	gen_statem:call(ServerRef, Msg, Timeout).

-spec cast(ServerRef, Message) -> ok
	when ServerRef :: server_ref(),
	     Message :: term().
cast(ServerRef, Msg) ->
	gen_statem:cast(ServerRef, Msg).

-spec reply(From, Message) -> ok
	when From :: from(),
	     Message :: term().
reply(From, Reply) ->
	gen_statem:reply(From, Reply).

-spec stop(ServerRef) -> ok
	when ServerRef :: server_ref().
stop(ServerRef) ->
	gen_statem:stop(ServerRef).

-spec stop(ServerRef, Reason, Timeout) -> ok
	when ServerRef :: server_ref(),
	     Reason :: term(),
	     Timeout :: timeout().
stop(ServerRef, Reason, Timeout) ->
	gen_statem:stop(ServerRef, Reason, Timeout).

-spec callback_mode() -> handle_event_function.
callback_mode() ->
	handle_event_function.

-spec init({CbMod, CbArgs}) -> gen_statem:init_result(agent_state())
	when CbMod :: module(),
	     CbArgs :: term().
init({CbMod, CbArgs}) ->
	case CbMod:init(CbArgs) of
		{ok, CbState} ->
			{ok, idle, #data{cb_mod=CbMod, cb_state=CbState}};
		ignore ->
			ignore;
		Stop = {stop, _Reason} ->
			Stop
	end.

-spec handle_event(EventType, Message, State, Data) -> gen_statem:event_handler_result(agent_state())
	when EventType :: gen_statem:event_type(),
	     Message :: ?TAG_I(term()) | term(),
	     State :: agent_state(),
	     Data :: #data{}.
handle_event({call, From}, ?TAG_I(perform), idle, Data) ->
	{next_state, sleeping, Data#data{attempt=0}, [{next_event, internal, enter}, {reply, From, ok}]};
handle_event({call, From}, ?TAG_I(perform), State, _Data) ->
	{keep_state_and_data, [{reply, From, {error, State}}]};
handle_event({call, From}, ?TAG_I(wait), idle, _Data) ->
	{keep_state_and_data, [{reply, From, ok}]};
handle_event({call, _From}, ?TAG_I(wait), _State, _Data) ->
	{keep_state_and_data, [postpone]};
handle_event(internal, enter, sleeping, Data=#data{attempt=Attempt, cb_mod=CbMod, cb_state=CbState0}) ->
	case CbMod:sleep_time(Attempt, CbState0) of
		{ok, Time} ->
			{keep_state_and_data, [{state_timeout, Time, wakeup}]};
		{ok, Time, CbState1} ->
			{keep_state, Data#data{cb_state=CbState1}, [{state_timeout, Time, wakeup}]};
		stop ->
			stop;
		{stop, Reason} ->
			{stop, Reason};
		{stop, Reason, CbState1} ->
			{stop, Reason, Data#data{cb_state=CbState1}}
	end;
handle_event(state_timeout, wakeup, sleeping, Data) ->
	{next_state, executing, Data, [{next_event, internal, enter}]};
handle_event(internal, enter, executing, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_result(executing, CbMod:handle_execute(CbState0), Data);
handle_event(info, {timeout, Timer, Msg}, executing, Data=#data{cb_mod=CbMod, cb_state=CbState0, timer=Timer}) ->
	handle_result(executing, CbMod:handle_event(timeout, Msg, executing, CbState0), Data#data{timer=undefined});
handle_event(info, {timeout, Timer, Msg}, State, Data=#data{cb_mod=CbMod, cb_state=CbState0, timer=Timer}) ->
	handle_result(State, CbMod:handle_event(timeout, Msg, State, CbState0), Data#data{timer=undefined});
handle_event(Event, Msg, executing, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_result(executing, CbMod:handle_event(Event, Msg, executing, CbState0), Data);
handle_event(Event, Msg, State, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_result(State, CbMod:handle_event(Event, Msg, State, CbState0), Data);
handle_event(_Type, _Msg, _State, _Data) ->
	keep_state_and_data.

handle_result(State, Result, Data=#data{timer=Timer}) when Timer=/=undefined ->
	case erlang:cancel_timer(Timer) of
		false ->
			receive
				{timeout, Timer, _} ->
					ok
			after 0 ->
				ok
			end;
		_ ->
			ok
	end,
	handle_result(State, Result, Data#data{timer=undefined});
handle_result(_State, {continue, CbState1, {Timeout, Msg}}, Data) ->
	Timer=erlang:start_timer(Timeout, self(), Msg),
	{keep_state, Data#data{cb_state=CbState1, timer=Timer}};
handle_result(_State, continue, _Data) ->
	keep_state_and_data;
handle_result(_State, {continue, CbState1}, Data) ->
	{keep_state, Data#data{cb_state=CbState1}};
handle_result(_State, stop, _Data) ->
	stop;
handle_result(_State, {stop, Reason}, _Data) ->
	{stop, Reason};
handle_result(_State, {stop, Reason, CbState1}, Data) ->
	{stop, Reason, Data#data{cb_state=CbState1}};
handle_result(executing, done, Data) ->
	{next_state, idle, Data};
handle_result(executing, {done, CbState1}, Data) ->
	{next_state, idle, Data#data{cb_state=CbState1}};
handle_result(executing, retry, Data=#data{attempt=Attempt}) ->
	{next_state, sleeping, Data#data{attempt=Attempt+1}, [{next_event, internal, enter}]};
handle_result(executing, {retry, CbState1}, Data=#data{attempt=Attempt}) ->
	{next_state, sleeping, Data#data{cb_state=CbState1, attempt=Attempt+1}, [{next_event, internal, enter}]};
handle_result(executing, repeat, Data) ->
	{keep_state, Data, [{next_event, internal, enter}]};
handle_result(executing, {repeat, CbState1}, Data) ->
	{keep_state, Data#data{cb_state=CbState1}, [{next_event, internal, enter}]}.

-spec terminate(Reason, State, Data) -> Ignored
	when Reason :: term(),
	     State :: agent_state(),
	     Data :: #data{},
	     Ignored :: term().
terminate(Reason, State, #data{cb_mod=CbMod, cb_state=CbState}) ->
	case erlang:function_exported(CbMod, terminate, 3) of
		true ->
			CbMod:terminate(Reason, State, CbState);
		false ->
			ok
	end.

-spec code_change(OldVsn, State0, Data0, Extra) -> Result
	when OldVsn :: term() | {down, term()},
	     State0 :: agent_state(),
	     Data0 :: term(),
	     Extra :: term(),
	     Result :: {ok, State1, Data1} | Reason,
	     State1 :: agent_state(),
	     Data1 :: term(),
	     Reason :: term().
code_change(OldVsn, State, Data=#data{cb_mod=CbMod, cb_state=CbState0}, Extra) ->
	case erlang:function_exported(CbMod, code_change, 4) of
		true ->
			case CbMod:code_change(OldVsn, State, CbState0, Extra) of
				{ok, State, CbState1} ->
					{ok, State, Data#data{cb_state=CbState1}};
				Reason ->
					Reason
			end;
		false ->
			{ok, State, Data}
	end.
