# gen_agent

`gen_agent` is a behavior module to simplify resource fetching.

Under the hood, `gen_agent` is a rather simple state machine, with only three states:
* `idle`: Just what it says. The agent is either done with a task, or hasn't done anything yet.
* `sleeping`: The agent is in backoff, waiting before starting another attempt at resource fetching.
* `executing`: The agent is performing its task.

## Callbacks

* `init(Args)` (Mandatory): Called to initialize the agent. Must return either an `ok` tuple with the initial data, the atom `ignore`, or a `stop` tuple with the stop reason.
* `sleep_time(Attempt, Data)` (Mandatory): Called to determine the backoff time before starting the next attempt. Must return either an `ok` tuple with the backoff time and optionally updated data, or `stop` or a `stop` tuple with the stop reason and optionally updated data.
* `handle_execute(Data)` (Mandatory): Called when the agent starts performing its task. Must return an instruction or instruction tuple (see below).
* `handle_event(EventType, Event, State, Data)` (Mandatory): Called when the agent receives a message. Must return an instruction or instruction tuple (see below).
* `terminate(Reason, State, Data)` (Optional): Called when the agent is shutting down. The return value is ignored.
* `code_change(OldVsn, State, Data, Extra)` (Optional): Called when the agent code is updated by a release upgrade. Must return an `ok` tuple with the new agent state and data.

### Instructions

The `handle_execute/1` and `handle_event/4` callbacks must return an instruction or instruction tuple to tell the agent how to proceed.

#### Continue

* `continue`: The agent will continue in the same state, and with unchanged data.
* `{continue, NewData}`: The agent will update its data and continue in the same state.
* `{continue, NewData, {Timeout, TimeoutMessage}}`: The agent will update its data and continue in the same state. If the given `Timeout` expires before another event occurs, an event of type `timeout` and the given `TimeoutMessage` is passed to `handle_event/4`.

#### Stop

* `stop`: The agent will terminate with reason `normal` and unchanged data.
* `{stop, Reason}`: The agent will terminate with the given reason and unchanged data.
* `{stop, Reason, NewData}`: The agent will update its data and terminate with the given reason.

#### Done

This instruction must only be returned from `handle_execute/1`, or from `handle_event/4` when the agent is in the executing state.

* `done`: The agent will transition to the idle state, with the same data.
* `{done, NewData}`: The agent will update its data and transition to the idle state.

#### Repeat

This instruction must only be returned from `handle_execute/1`, or from `handle_event/4` when the agent is in the executing state.

* `repeat`: The agent will stay in the executing state and re-call `handle_execute/1`, with unchanged data.
* `{repeat, NewData}`: The agent will stay in the executing state, update its data, and re-call `handle_execute/1`.

#### Retry

This instruction must only be returned from `handle_execute/1`, or from `handle_event/4` when the agent is in the executing state.

* `retry`: The agent will give up the current attempt and try again later, with unchanged data.
* `{retry, NewData}`: The agent will give up the current attempt, update its data, and try again later.

## API

* `start/3,4`: Starts a standalone agent which is not linked to the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `start_link/3,4`: Starts an agent which is linked to the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `start_monitor/3,4` : Starts an agent which is monitored by the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `perform/1`: Tell the agent to start perfoming its task. Can only be used when the agent is in state `idle`. Will always return `ok` immediately.
* `wait/1,2`: Wait for the agent to complete its task within an optional timeout and then return `ok`, or fail when the timeout expires.
* `call/2,3`: Send a synchronous call to the agent and wait for a reply within an optional timeout, or fail when the timeout expires.
* `cast/2`: Send an asynchronous cast to the agent. Always returns `ok`.
* `reply/2`: Reply to a message received via a synchronous call message. Always returns `ok`.
* `stop/1,3`: Stops the agent with an optional reason and timeout. Will always return `ok`, or fail when the timeout expires.

## Usage

See the `examples` directory for examples how `gen_agent` can be implemented and used.

* The `mysql_agent` example is simple, it tries to obtain a connection to a MySQL database using MySQL-OTP.
* The `smtp_agent` example is more complex, as it not only involves connecting to a server but also some initial communication and possibly a socket upgrade to TLS.

## Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
