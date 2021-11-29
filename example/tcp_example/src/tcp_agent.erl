-module(tcp_agent).
-behavior(gen_agent).

-export([start_link/0, run/3]).
-export([init/1, sleep_time/2, handle_execute/1, handle_event/4]).

-record(data, {host, port, client, reply_to}).

start_link() ->
	gen_agent:start_link(?MODULE, [], []).

run(Agent, Host, Port) ->
	gen_agent:call(Agent, {perform, self(), Host, Port}).

init([]) ->
	{ok, #data{}}.

sleep_time(N, _Data) when N<5 ->
	{ok, N*1000};
sleep_time(_N, _Data) ->
	{stop, {error, max_attempts}}.

handle_execute(#data{host=Host, port=Port, client=Client, reply_to=ReplyTo}) ->
	case gen_tcp:connect(Host, Port, [{active, false}], 1000) of
		{ok, Sock} ->
			gen_tcp:controlling_process(Sock, Client),
			gen_agent:reply(ReplyTo, {ok, Sock}),
			{idle, #data{}};
		_ ->
			retry
	end.

handle_event({call, From}, {perform, Client, Host, Port}, idle, _Data) ->
	{perform, #data{host=Host, port=Port, client=Client, reply_to=From}};
handle_event({call, From}, _Msg, _State, _Data) ->
	gen_agent:reply(From, error),
	continue;
handle_event(_Event, _Msg, _State, _Data) ->
	continue.
