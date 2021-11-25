# gen_agent

`gen_agent` is a behavior module to simplify resource fetching.

## Callbacks

* `init/1` (Mandatory
* `sleep_time/2` (Mandatory)
* `handle_execute/1` (Mandatory)
* `handle_event/4` (Mandatory)
* `terminate/3` (Optional)
* `code_change/4`(Optional)

## API

* `start/3,4`, `start_link/3,4`, `start_monitor/3,4`
* `perform/1`
* `wait/1,2`
* `call/2,3`
* `cast/2`
* `reply/2`
* `stop/1,3`

## Usage

See the `examples` directory for examples how `gen_agent` can be implemented and used.

## Authors

* Maria Scott (Maria-12648430)
* Jan Uhlig (juhlig)
