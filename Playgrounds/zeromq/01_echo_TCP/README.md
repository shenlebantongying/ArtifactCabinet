`ZMQ_REP` -> a service that can recv data from multiple clients which use `ZMQ_REQ`.

All send/recv are blocking operations.

The server binds an address
The client connects to an address