-module(smtp_agent).
-behavior(gen_agent).

-export([start_link/2, start_link/3, run/1, run/2, stop/1]).
-export([init/1, sleep_time/2, handle_execute/1, handle_event/3, handle_command/4, terminate/3]).

-record(data, {opts, mxs=undefined, host, hello, conn, caps=[], stage}).

start_link(Host, Hello) ->
	start_link(Host, Hello, #{}).

start_link(Host, Hello, Opts) ->
	Opts1=maps:merge(
		#{
			max_attempts => 3,
			delay => 0,
			backoff_time => 1000,
			backoff_exp => 2,
			jitter => 1000
		},
		Opts
	),
	gen_agent:start_link(?MODULE, {Host, Hello, Opts1}, []).

run(Agent) ->
	run(Agent, infinity).

run(Agent, Timeout) ->
	gen_agent:perform(Agent),
	gen_agent:wait(Agent, Timeout),
	gen_agent:call_cmd(Agent, {retrieve, self()}).

stop(Agent) ->
	gen_agent:call_cmd(Agent, stop).

init({Host, Hello, Opts}) ->
	{ok, #data{opts=Opts, host=Host, hello=Hello}}.

sleep_time(N, #data{opts=#{max_attempts:=Max}}) when N+1>Max ->
	{stop, {error, max_attempts}};
sleep_time(N, #data{opts=#{delay:=D, backoff_time:=BT, backoff_exp:=BE, jitter:=J}}) ->
	{ok, max(0, D+round(BT*math:pow(N, BE))+rand:uniform(J))}.

handle_execute(D=#data{mxs=undefined, host=Host}) ->
	case get_mxs(Host) of
		[] ->
			{stop, no_mxs};
		MXs ->
			handle_execute(D#data{mxs=MXs})
	end;
handle_execute(D=#data{mxs=[]}) ->
	{retry, D#data{mxs=undefined}};
handle_execute(D=#data{mxs=[{_, Mx}|Mxs]}) ->
	case gen_tcp:connect(Mx, 25, [{active, false}, binary, {packet, line}], 5000) of
		{ok, Sock} ->
			Conn={tcp, Sock},
			activate(Conn),
			{continue, D#data{mxs=Mxs, conn=Conn, stage=greeting}};
		_ ->
			{repeat, D#data{mxs=Mxs}}
	end.

handle_event(info, {SType, Sock, <<$2, _:2/bytes, $-, Cap/binary>>}, D=#data{conn=Conn={SType, Sock}, caps=Caps, stage=ehlo}) ->
	activate(Conn),
	{continue, D#data{caps=[uppercase(trim_crlf(Cap))|Caps]}};
handle_event(info, {tcp, Sock, <<$2, _:2/bytes, _, Cap/binary>>}, D=#data{conn=Conn={tcp, Sock}, hello=Hello, caps=Caps, stage=ehlo}) ->
	Caps1=tl(lists:reverse([uppercase(trim_crlf(Cap))|Caps])),
	case lists:member(<<"STARTTLS">>, Caps1) of
		true ->
			send(Conn, <<"STARTTLS", $\r, $\n>>),
			case recv(Conn) of
				{ok, <<$2, _/binary>>} ->
					application:ensure_all_started(ssl),
					case ssl:connect(Sock, [{verify, verify_none}]) of
						{ok, Sock1} ->
							Conn1={ssl, Sock1},
							send(Conn1, ["EHLO ", Hello, $\r, $\n]),
							activate(Conn1),
							{continue, D#data{conn=Conn1, caps=[]}};
						_Other ->
							close(Conn),
							{repeat, D#data{conn=undefined, caps=[], stage=undefined}}
					end;
				{ok, _Other} ->
					{done, D#data{caps=Caps1, stage=ready}};
				_Other ->
					close(Conn),
					{repeat, D#data{conn=undefined, caps=[], stage=undefined}}
			end;
		false ->
			{done, D#data{caps=Caps1, stage=ready}}
	end;
handle_event(info, {ssl, Sock, <<$2, _:2/bytes, _, Cap/binary>>}, D=#data{conn={ssl, Sock}, caps=Caps, stage=ehlo}) ->
	Caps1=tl(lists:reverse([uppercase(trim_crlf(Cap))|Caps])),
	{done, D#data{caps=Caps1, stage=ready}};
handle_event(info, {SType, Sock, <<_:3/bytes, $-, _/binary>>}, #data{conn=Conn={SType, Sock}}) ->
	activate(Conn),
	continue;
handle_event(info, {SType, Sock, <<$2, _/binary>>}, D=#data{conn=Conn={SType, Sock}, stage=Stage, hello=Hello}) ->
	case Stage of
		greeting ->
			send(Conn, ["EHLO ", Hello, $\r, $\n]),
			activate(Conn),
			{continue, D#data{stage=ehlo}};
		helo ->
			{done, D#data{stage=ready}}
	end;
handle_event(info, {SType, Sock, <<$4, _/binary>>}, D=#data{conn=Conn={SType, Sock}}) ->
	close(Conn),
	{repeat, D#data{conn=undefined, caps=[], stage=undefined}};
handle_event(info, {SType, Sock, <<$5, _/binary>>}, D=#data{conn=Conn={SType, Sock}, stage=Stage, hello=Hello}) ->
	case Stage of
		greeting ->
			close(Conn),
			{stop, greeting_5xx, D#data{conn=undefined}};
		ehlo ->
			send(Conn, ["HELO ", Hello, $\r, $\n]),
			activate(Conn),
			{continue, D#data{caps=[], stage=helo}};
		helo ->
			close(Conn),
			{repeat, D#data{conn=undefined, caps=[], stage=undefined}}
	end;
handle_event(info, {tcp_closed, Sock}, D=#data{conn={tcp, Sock}}) ->
	{repeat, D#data{conn=undefined, caps=[], stage=undefined}};
handle_event(info, {tcp_error, Sock, _Error}, D=#data{conn=Conn={tcp, Sock}}) ->
	close(Conn),
	{repeat, D#data{conn=undefined, caps=[], stage=undefined}};
handle_event(info, {ssl_closed, Sock}, D=#data{conn={ssl, Sock}}) ->
	{repeat, D#data{conn=undefined, caps=[], stage=undefined}};
handle_event(info, {ssl_error, Sock, _Error}, D=#data{conn=Conn={ssl, Sock}}) ->
	close(Conn),
	{repeat, D#data{conn=undefined, caps=[], stage=undefined}}.

handle_command({call, From}, {retrieve, _}, idle, #data{conn=undefined}) ->
	gen_agent:reply(From, error),
	continue;
handle_command({call, From}, {retrieve, CP}, idle, D=#data{conn=Conn, caps=Caps}) ->
	controlling_process(Conn, CP),
	gen_agent:reply(From, {ok, Conn, Caps}),
	{continue, D#data{conn=undefined, caps=[]}};
handle_command({call, From}, stop, _S, _D) ->
	gen_agent:reply(From, ok),
	stop;
handle_command(_T, stop, _S, _D) ->
	stop.

terminate(R, S, D=#data{conn=Conn}) when Conn=/=undefined ->
	close(Conn),
	terminate(R, S, D#data{conn=undefined});
terminate(_R, _S, _D) ->
	ok.

get_mxs(Host) ->
	lists:sort(inet_res:lookup(Host, in, mx)).

trim_crlf(Str) ->
	S=byte_size(Str),
	binary:part(Str, 0, S-2).

uppercase(Str) ->
	<< <<(if C>=$a, C=<$z -> C-$a+$A; true -> C end)>> || <<C>> <= Str>>.

send({tcp, Sock}, Msg) ->
	gen_tcp:send(Sock, Msg);
send({ssl, Sock}, Msg) ->
	ssl:send(Sock, Msg).

recv({tcp, Sock}) ->
	gen_tcp:recv(Sock, 0);
recv({ssl, Sock}) ->
	ssl:recv(Sock, 0).

activate({tcp, Sock}) ->
	inet:setopts(Sock, [{active, once}]);
activate({ssl, Sock}) ->
	ssl:setopts(Sock, [{active, once}]).

controlling_process({tcp, Sock}, Pid) ->
	gen_tcp:controlling_process(Sock, Pid);
controlling_process({ssl, Sock}, Pid) ->
	ssl:controlling_process(Sock, Pid).

close({tcp, Sock}) ->
	gen_tcp:close(Sock);
close({ssl, Sock}) ->
	ssl:close(Sock).
