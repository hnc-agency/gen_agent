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
-export([call_cmd/2, call_cmd/3]).
-export([cast_cmd/2]).
-export([call/2, call/3]).
-export([cast/2]).
-export([reply/2]).
-export([stop/1, stop/3]).

-export([callback_mode/0, init/1, handle_event/4, terminate/3, code_change/4]).

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

-callback handle_event(EventType, Message, Data0) -> Result
	when EventType :: {call, From} | cast | info | timeout,
	     From :: gen_statem:from(),
	     Message :: term(),
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
	     Reason :: term().

-callback handle_command(EventType, Message, State, Data0) -> Result
	when EventType :: {call, From} | cast | timeout,
	     From :: gen_statem:from(),
	     Message :: term(),
	     State :: agent_state(),
	     Data0 :: term(),
	     Result :: continue
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

-callback code_change(OldVsn, State0, Data0, Extra) -> Result
	when OldVsn :: term() | {down, term()},
	     State0 :: agent_state(),
	     Data0 :: term(),
	     Extra :: term(),
	     Result :: {ok, State1, Data1} | Reason,
	     State1 :: agent_state(),
	     Data1 :: term(),
	     Reason :: term().

-define(TAG_I(Msg), {'$gen_agent', internal, Msg}).
-define(TAG_C(Msg), {'$gen_agent', command, Msg}).

-record(data, {
		cb_mod :: module(),
		cb_state :: term(),
		attempt=0 :: non_neg_integer(),
		command_timer :: undefined | reference(),
		event_timer :: undefined | reference()
	}).

start(Module, Args, Opts) ->
	gen_statem:start(?MODULE, {Module, Args}, Opts).

start(ServerName, Module, Args, Opts) ->
	gen_statem:start(ServerName, ?MODULE, {Module, Args}, Opts).

start_link(Module, Args, Opts) ->
	gen_statem:start_link(?MODULE, {Module, Args}, Opts).

start_link(ServerName, Module, Args, Opts) ->
	gen_statem:start_link(ServerName, ?MODULE, {Module, Args}, Opts).

start_monitor(Module, Args, Opts) ->
	gen_statem:start_monitor(?MODULE, {Module, Args}, Opts).

start_monitor(ServerName, Module, Args, Opts) ->
	gen_statem:start_monitor(ServerName, ?MODULE, {Module, Args}, Opts).

perform(ServerRef) ->
	call(ServerRef, ?TAG_I(perform)).

wait(ServerRef) ->
	wait(ServerRef, infinity).

wait(ServerRef, Timeout) ->
	call(ServerRef, ?TAG_I(wait), Timeout).

call_cmd(ServerRef, Msg) ->
	call(ServerRef, ?TAG_C(Msg)).

call_cmd(ServerRef, Msg, Timeout) ->
	call(ServerRef, ?TAG_C(Msg), Timeout).

cast_cmd(ServerRef, Msg) ->
	cast(ServerRef, ?TAG_C(Msg)).

call(ServerRef, Msg) ->
	gen_statem:call(ServerRef, Msg).

call(ServerRef, Msg, Timeout) ->
	gen_statem:call(ServerRef, Msg, Timeout).

cast(ServerRef, Msg) ->
	gen_statem:cast(ServerRef, Msg).

reply(From, Reply) ->
	gen_statem:reply(From, Reply).

stop(ServerRef) ->
	gen_statem:stop(ServerRef).

stop(ServerRef, Reason, Timeout) ->
	gen_statem:stop(ServerRef, Reason, Timeout).

callback_mode() ->
	handle_event_function.

init({CbMod, CbArgs}) ->
	case CbMod:init(CbArgs) of
		{ok, CbState} ->
			{ok, idle, #data{cb_mod=CbMod, cb_state=CbState}};
		ignore ->
			ignore;
		Stop = {stop, _Reason} ->
			Stop
	end.

handle_event({call, From}, ?TAG_I(perform), idle, Data) ->
	{next_state, sleeping, Data#data{attempt=0}, [{next_event, internal, ?TAG_I(enter)}, {reply, From, ok}]};
handle_event({call, From}, ?TAG_I(perform), State, _Data) ->
	{keep_state_and_data, [{reply, From, {error, State}}]};
handle_event({call, From}, ?TAG_I(wait), idle, _Data) ->
	{keep_state_and_data, [{reply, From, ok}]};
handle_event({call, _From}, ?TAG_I(wait), _State, _Data) ->
	{keep_state_and_data, [postpone]};
handle_event(internal, ?TAG_I(enter), sleeping, Data=#data{attempt=Attempt, cb_mod=CbMod, cb_state=CbState0}) ->
	case CbMod:sleep_time(Attempt, CbState0) of
		{ok, Time} ->
			{keep_state_and_data, [{state_timeout, Time, ?TAG_I(wakeup)}]};
		{ok, Time, CbState1} ->
			{keep_state, Data#data{cb_state=CbState1}, [{state_timeout, Time, ?TAG_I(wakeup)}]};
		stop ->
			stop;
		{stop, Reason} ->
			{stop, Reason};
		{stop, Reason, CbState1} ->
			{stop, Reason, Data#data{cb_state=CbState1}}
	end;
handle_event(state_timeout, ?TAG_I(wakeup), sleeping, Data) ->
	{next_state, executing, Data, [{next_event, internal, ?TAG_I(enter)}]};
handle_event(internal, ?TAG_I(enter), executing, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_execute_result(CbMod:handle_execute(CbState0), Data);
handle_event(Event={call, _From}, ?TAG_C(Msg), State, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_command_result(CbMod:handle_command(Event, Msg, State, CbState0), Data);
handle_event(cast, ?TAG_C(Msg), State, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_command_result(CbMod:handle_command(cast, Msg, State, CbState0), Data);
handle_event(info, {timeout, Timer, ?TAG_C(Msg)}, State, Data=#data{cb_mod=CbMod, cb_state=CbState0, command_timer=Timer}) ->
	handle_command_result(CbMod:handle_command(timeout, Msg, State, CbState0), Data#data{command_timer=undefined});
handle_event(info, {timeout, _Timer, ?TAG_C(_Msg)}, _State, _Data) ->
	keep_state_and_data;
handle_event(info, {timeout, Timer, ?TAG_I(Msg)}, executing, Data=#data{cb_mod=CbMod, cb_state=CbState0, event_timer=Timer}) ->
	handle_execute_result(CbMod:handle_event(timeout, Msg, CbState0), Data#data{event_timer=undefined});
handle_event(info, {timeout, _Timer, ?TAG_I(_Msg)}, executing, _Data) ->
	keep_state_and_data;
handle_event(info, {timeout, _Timer, ?TAG_I(_Msg)}, _State, Data) ->
	{keep_state, Data#data{event_timer=undefined}};
handle_event(Event, Msg, executing, Data=#data{cb_mod=CbMod, cb_state=CbState0}) ->
	handle_execute_result(CbMod:handle_event(Event, Msg, CbState0), Data);
handle_event(_Type, _Msg, _State, _Data) ->
	keep_state_and_data.

handle_command_result(Result, Data=#data{command_timer=Timer}) when Timer=/=undefined ->
	cancel_timer(Timer),
	handle_command_result(Result, Data#data{command_timer=undefined});
handle_command_result({continue, CbState1, {Timeout, Msg}}, Data) ->
	Timer=erlang:start_timer(Timeout, self(), ?TAG_C(Msg)),
	{keep_state, Data#data{cb_state=CbState1, command_timer=Timer}};
handle_command_result(Result, Data) ->
	handle_common_result(Result, Data).

handle_execute_result(Result, Data=#data{event_timer=Timer}) when Timer=/=undefined ->
	cancel_timer(Timer),
	handle_execute_result(Result, Data#data{event_timer=undefined});
handle_execute_result({continue, CbState1, {Timeout, Msg}}, Data) ->
	Timer=erlang:start_timer(Timeout, self(), ?TAG_I(Msg)),
	{keep_state, Data#data{cb_state=CbState1, event_timer=Timer}};
handle_execute_result(done, Data) ->
	{next_state, idle, Data};
handle_execute_result({done, CbState1}, Data) ->
	{next_state, idle, Data#data{cb_state=CbState1}};
handle_execute_result(retry, Data=#data{attempt=Attempt}) ->
	{next_state, sleeping, Data#data{attempt=Attempt+1}, [{next_event, internal, ?TAG_I(enter)}]};
handle_execute_result({retry, CbState1}, Data=#data{attempt=Attempt}) ->
	{next_state, sleeping, Data#data{cb_state=CbState1, attempt=Attempt+1}, [{next_event, internal, ?TAG_I(enter)}]};
handle_execute_result(repeat, Data) ->
	{next_state, executing, Data, [{next_event, internal, ?TAG_I(enter)}]};
handle_execute_result({repeat, CbState1}, Data) ->
	{next_state, executing, Data#data{cb_state=CbState1}, [{next_event, internal, ?TAG_I(enter)}]};
handle_execute_result(Result, Data) ->
	handle_common_result(Result, Data).

handle_common_result(continue, _Data) ->
	keep_state_and_data;
handle_common_result({continue, CbState1}, Data) ->
	{keep_state, Data#data{cb_state=CbState1}};
handle_common_result(stop, _Data) ->
	stop;
handle_common_result({stop, Reason}, _Data) ->
	{stop, Reason};
handle_common_result({stop, Reason, CbState1}, Data) ->
	{stop, Reason, Data#data{cb_state=CbState1}}.

cancel_timer(Timer) ->
	case erlang:cancel_timer(Timer) of
		false ->
			receive {timeout, Timer, _} -> ok after 0 -> ok end;
		_ ->
			ok
	end.

terminate(Reason, State, #data{cb_mod=CbMod, cb_state=CbState}) ->
	CbMod:terminate(Reason, State, CbState).

code_change(OldVsn, State, Data=#data{cb_mod=CbMod, cb_state=CbState0}, Extra) ->
	case CbMod:code_change(OldVsn, State, CbState0, Extra) of
		{ok, State, CbState1} ->
			{ok, State, Data#data{cb_state=CbState1}};
		Reason ->
			Reason
	end.
