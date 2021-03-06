# gen_agent

`gen_agent` is a behavior module to simplify interaction with unreliable external services.

The primary use case is fetching resources, eg a connection to a remote database which may be temporarily down or overloaded: while the first connection attempt may fail, it may succeed when retried at a later time.
It is not limited to resource fetching tasks, however, in fact most use cases that need some form of retrying can be modeled, like posting a message to your favorite social network and retrying if it happens to be down.

`gen_agent` relieves users of the burden of implementing retry and backoff logic over and over again, and to focus on the actual interaction logic instead.

## Internals

Under the hood, `gen_agent` is a simplified state machine, with only four possible states:
* `idle`: The agent is ready to start its a task.
* `sleeping`: The agent is waiting in backoff before starting another attempt at doing its task.
* `executing`: The agent is performing its task.
* `done`: The agent has finished its task.

## Callbacks

* `init(Args)` (Mandatory): Called to initialize the agent. Must return either an `ok` tuple with the initial data, the atom `ignore`, or a `stop` tuple with the stop reason.
* `sleep_time(Attempt, Data)` (Mandatory): Called to determine the backoff time before starting the next attempt. Must return either an `ok` tuple with the backoff time and optionally updated data, or `stop` or a `stop` tuple with the stop reason and optionally updated data.
* `handle_execute(Data)` (Mandatory): Called when the agent starts performing its task. Must return an instruction or instruction tuple (see below).
* `handle_event(EventType, Event, State, Data)` (Mandatory): Called when the agent receives a message. Must return an instruction or instruction tuple (see below).
* `terminate(Reason, State, Data)` (Optional): Called when the agent is shutting down. The return value is ignored.
* `code_change(OldVsn, State, Data, Extra)` (Optional): Called when the agent code is updated by a release upgrade. Must return an `ok` tuple with the new agent state and data.

### Instructions

The `handle_execute/1` and `handle_event/4` callbacks must return an instruction or instruction tuple to tell the agent how to proceed.

#### Perform

This instruction is only allowed when the agent is in the `idle` state.

* `perform`: The agent will reset the attempt counter to `0`, transition to the `sleeping` state, wait for the time returned from the callback modules' `sleep_time/2` function, then transition to the `executing` state and start performing its task.
* `{perform, NewData}`: The agent will update its data, reset the attempt counter to `0`, transition to the `sleeping` state, wait for the time returned from the callback modules' `sleep_time/2` function, then transition to the `executing` state and start performing its task.

#### Idle

* `idle`: The agent will transition to the `idle` state.
* `{idle, NewData}`: The agent will update its data and transition to the `idle` state.

#### Continue

* `continue`: The agent will continue in the same state.
* `{continue, NewData}`: The agent will update its data and continue in the same state.
* `{continue, NewData, {Timeout, TimeoutMessage}}`: The agent will update its data and continue in the same state.
  If the given `Timeout` expires before another event occurs, an event of type `timeout` and the given `TimeoutMessage` is passed to `handle_event/4`.

#### Stop

* `stop`: The agent will terminate with reason `normal`.
* `{stop, Reason}`: The agent will terminate with the given reason.
* `{stop, Reason, NewData}`: The agent will update its data and terminate with the given reason.

#### Done

* `done`: The agent will transition to the `done` state.
* `{done, NewData}`: The agent will update its data and transition to the `done` state.

#### Repeat

* `repeat`: The agent will transition to the `executing` state and retry immediately.
* `{repeat, NewData}`: The agent will update its data, transition to the `executing` state, and retry immediately.

#### Retry

* `retry`: The agent will increment the attempt counter, transition to the `sleeping` state, and retry after the respective backoff time has elapsed.
* `{retry, NewData}`: The agent will update its data, increment the attempt counter, transition to the `sleeping` state, and retry after the respective backoff time has elapsed.

## API functions

* `start/3,4`: Starts a standalone agent which is not linked to the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `start_link/3,4`: Starts an agent which is linked to the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `start_monitor/3,4` : Starts an agent which is monitored by the calling process. Will return the agent pid in an `ok` tuple, or fail.
* `wait/2,3`: Wait for the agent to enter a given state within an optional timeout and return `ok`, or fail when the timeout expires.
* `call/2,3`: Send a synchronous call to the agent and wait for a reply within an optional timeout, or fail when the timeout expires.
* `cast/2`: Send an asynchronous cast to the agent. Always returns `ok`.
* `reply/2`: Reply to a message received via a synchronous call message. Always returns `ok`.
* `stop/1,3`: Stops the agent with an optional reason and timeout. Will always return `ok`, or fail when the timeout expires.

## Convenience functions

* `cooldown/5`: Calculates a backoff time from the `Attempt` number, a constant `Delay`, a `Backoff` time which exponentially grows by `Growth`, and a random `Jitter`.

## Usage

See the `examples` directory for examples how `gen_agent` can be implemented and used.

* The `tcp_agent` example is simple, it tries to establish a TCP connection to a given host and port.
* The `smtp_agent` example is more complex, as it not only involves connecting to a server but also some initial communication and possibly a socket upgrade to TLS.

## Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
