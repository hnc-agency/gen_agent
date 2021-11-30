# `tcp_agent`

This is an example of a `gen_agent` implementation.

Given a host and port, `tcp_agent` will try to establish a connection, up to 5 times with a delay of 1s between retries.

## Example usage

Open an Erlang shell and run:
```erlang
1> {ok, A}=tcp_agent:start_link().
{ok,<0.82.0>}
2> tcp_agent:run(A, {127, 0, 0, 1}, 8888).
```
The call will hang.

In another (Unix) shell, run:
```shell
$ nc -l 8888
```

The hanging call should now return:
```erlang
{ok,#Port<0.5>}
```
