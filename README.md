# About

msgpack-rpc-client is a [Chicken 5](call-cc.org) egg that implement a client for the [MsgPack-RPC](https://github.com/msgpack-rpc/msgpack-rpc) protocol.

It aims at providing an Lispy interface to this protocol with support of asyncronous requests and possibility to respond to server-to-client requests.

# Features

- [x] Syncronous requests
- [x] Async callback based requests
- [x] Async promise based requests
- [x] Async multi-threaded requests
- [x] Async single-threaded requests
- [x] notifications
- [x] thread-safe operations
- [x] server-to-client requests and notifications
- [x] transport over TCP/IPv4
- [ ] transport throught FIFO files
- [ ] transport over other channels through an extension interface
- [ ] testing
    - [ ] transport
    - [ ] client-to-server
    - [ ] server-to-client

# Building and testing

For development building and testing is done through a `make`.
`make` build everything.
`make test` run tests.

Beware that testing is done against the [aio-msgpack-rpc](https://pypi.org/project/aio-msgpack-rpc) python module.
You must insttall it before running the test suite.
