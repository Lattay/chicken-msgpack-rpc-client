# About

msgpack-rpc-client is a [Chicken 5](call-cc.org) egg that implement a client for the [MsgPack-RPC](https://github.com/msgpack-rpc/msgpack-rpc) protocol.

It aims at providing an Lispy interface to this protocol with support of asyncronous requests and possibility to respond to server-to-client requests.

# Features

- [x] Syncronous requests
- [x] Async single-threaded callback based requests
- [x] Async single-threaded promise based requests
- [x] Async multi-threaded callback based requests
- [x] Async multi-threaded promise based requests
- [x] notifications
- [x] thread-safe operations
- [x] server-to-client requests and notifications
- [ ] transport over TCP/IPv4
- [ ] transport throught FIFO files
- [ ] transport over other channels through an extension interface
- [ ] testing
    - [ ] transport
    - [ ] client-to-server
    - [ ] server-to-client
