-module(mysql_agent).
-behavior(gen_agent).

-export([start_link/1, run/1, run/2]).
-export([init/1, sleep_time/2, handle_execute/1, handle_event/4]).

-record(data, {opts, tag, conn}).

start_link(Opts) ->
	gen_agent:start_link(?MODULE, Opts, []).

run(A) ->
	run(A, infinity).

run(A, Timeout) ->
	{ok, Tag}=gen_agent:call(A, {perform, self()}),
	ok=gen_agent:wait(A, done, Timeout),
	{ok, Conn}=gen_agent:call(A, {retrieve, Tag}),
	link(Conn),
	ok=gen_agent:call(A, {release, Tag}),
	{ok, Conn}.

init(Opts) ->
	process_flag(trap_exit, true),
	{ok, #data{opts=Opts}}.

sleep_time(N, _D) when N<5 ->
	{ok, N*1000};
sleep_time(_N, _D) ->
	{stop, max_attempts}.

handle_execute(D=#data{opts=Opts}) ->
	try
		mysql:start_link(Opts)
	of
		{ok, Conn} ->
			{done, D#data{conn=Conn}};
		_ ->
			retry
	catch
		_:_ ->
			retry
	end.

handle_event({call, From}, {perform, Pid}, idle, D) ->
	Tag=monitor(process, Pid),
	gen_agent:reply(From, {ok, Tag}),
	{perform, D#data{tag=Tag}};
handle_event({call, From}, {retrieve, Tag}, done, #data{tag=Tag, conn=Conn}) ->
	gen_agent:reply(From, {ok, Conn}),
	continue;
handle_event({call, From}, {release, Tag}, done, D=#data{tag=Tag, conn=Conn}) ->
	demonitor(Tag, [flush]),
	unlink(Conn),
	gen_agent:reply(From, ok),
	{idle, D#data{tag=undefined, conn=undefined}};
handle_event(info, {'DOWN', Tag, process, _Pid, _Reason}, done, D=#data{tag=Tag, conn=Conn}) ->
	mysql:stop(Conn),
	{idle, D=#data{tag=undefined, conn=undefined}};
handle_event(info, {'DOWN', Tag, process, _Pid, _Reason}, _State, D=#data{tag=Tag}) ->
	{idle, D=#data{tag=undefined, conn=undefined}};
handle_event({call, From}, _M, _S, _D) ->
	gen_agent:reply(From, error),
	continue;
handle_event(_E, _M, _S, _D) ->
	continue.
