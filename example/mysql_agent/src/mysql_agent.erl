-module(mysql_agent).
-behavior(gen_agent).

-export([start_link/1, run/1, run/2]).
-export([init/1, sleep_time/2, handle_execute/1, handle_event/4]).

-record(data, {opts, conn}).

start_link(Opts) ->
	gen_agent:start_link(?MODULE, Opts, []).

run(A) ->
	run(A, infinity).

run(A, Timeout) ->
	ok=gen_agent:perform(A),
	ok=gen_agent:wait(A, Timeout),
	{ok, Conn}=gen_agent:call(A, retrieve),
	link(Conn),
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

handle_event({call, From}, retrieve, idle, D=#data{conn=Conn}) when Conn=/=undefined ->
	gen_agent:reply(From, {ok, Conn}),
	unlink(Conn),
	{continue, D#data{conn=undefined}};
handle_event({call, From}, retrieve, _S, _D) ->
	gen_agent:reply(From, error),
	continue;
handle_event(_T, _M, _S, _D) ->
	continue.
