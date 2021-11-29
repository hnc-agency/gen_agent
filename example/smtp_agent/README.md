# `smtp_agent`

This is an example of a `gen_agent` implementation.

Given a host name, `smtp_agent` will try to obtain a SMTP connection to one of the MX hosts registered for the given host name, handle the greeting and capabilities, and upgrade the connection to TLS if possible. In the end, it will return a SMTP session ready for mail delivery.
If a failure occurs in the process described above, `smtp_agent` will try the next MX host in the list. If all MX hosts fail, it will back off for a few seconds before trying again.

## Example usage

### Starting a SMTP agent

```erlang
{ok, A} = smtp_agent:start_link("gmail.com", "example.org").
```
This will start an agent for obtaining a connection to one of `gmail.com`s MX hosts. The second argument is the string to be used as Hello in `EHLO`/`HELO`.

### Running the SMTP agent

```erlang
{ok, {ConnType, Sock}, Exts}=smtp_agent:run(A).
```

This will start the process described above and hopefully return an `ok` tuple containing...
* `ConnType`: the connection type , either `tcp` or `ssl`
* `Sock`: the connected socket, with the process which called `run` as the controlling process
* `Exts: the list of ESMTP extensions given by the server
