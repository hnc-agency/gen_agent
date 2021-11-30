-module(tcp_agent).
-behavior(gen_agent).

-export([start_link/0, run/3, run/4]).
-export([init/1, sleep_time/2, handle_execute/1, handle_event/4, terminate/3]).

-record(data, {host, port, sock, client}).

start_link() ->
	gen_agent:start_link(?MODULE, [], []).

run(Agent, Host, Port) ->
	run(Agent, Host, Port, infinity).

run(Agent, Host, Port, Timeout) ->
	ok=gen_agent:call(Agent, {perform, self(), Host, Port}),
	ok=gen_agent:wait(Agent, done, Timeout),
	gen_agent:call(Agent, retrieve).

init([]) ->
	process_flag(trap_exit, true),
	{ok, #data{}}.

sleep_time(N, _Data) when N<5 ->
	{ok, N*1000};
sleep_time(_N, _Data) ->
	{stop, {error, max_attempts}}.

handle_execute(Data=#data{host=Host, port=Port}) ->
	case gen_tcp:connect(Host, Port, [{active, false}], 1000) of
		{ok, Sock} ->
			{done, Data#data{sock=Sock}};
		_ ->
			retry
	end.

handle_event({call, From}, {perform, Pid, Host, Port}, idle, _Data) ->
	Mon=monitor(process, Pid),
	gen_agent:reply(From, ok),
	{perform, #data{host=Host, port=Port, client={Pid, Mon}}};
handle_event({call, From}, retrieve, done, #data{sock=Sock, client={Client, Mon}}) ->
	demonitor(Mon, [flush]),
	gen_tcp:controlling_process(Sock, Client),
	gen_agent:reply(From, {ok, Sock}),
	{idle, #data{}};
handle_event(info, {'DOWN', Mon, process, Pid, _Reason}, done, #data{sock=Sock, client={Pid, Mon}}) ->
	gen_tcp:close(Sock),
	{idle, #data{}};
handle_event(info, {'DOWN', Mon, process, Pid, _Reason}, _State, #data{client={Pid, Mon}}) ->
	{idle, #data{}};
handle_event({call, From}, _Msg, _State, _Data) ->
	gen_agent:reply(From, error),
	continue;
handle_event(_Event, _Msg, _State, _Data) ->
	continue.

terminate(Reason, State, Data=#data{sock=Sock}) when Sock=/=undefined ->
	gen_tcp:close(Sock),
	terminate(Reason, State, Data#data{sock=undefined});
terminate(_Reason, _State, _Data) ->
	ok.
